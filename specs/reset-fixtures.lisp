;;;; specs/reset-fixtures.lisp
;;;;
;;;; State-loss events (Phase 4C) over the real pool and the real proxy, for
;;;; the properties of specs/reset-events.lisp and the fixed cases of
;;;; tests/reset-events-test.lisp.  Needs no cl-spec, so the fixed cases run
;;;; in the default suite.
;;;;
;;;; The pool runs as in specs/pool-fixtures.lisp -- fake workers, background
;;;; work queued until an operation runs it -- and a request is the real
;;;; PROXY-TO-WORKER, with only the socket replaced: the fake RPC answers, or
;;;; meets the worker's death the way WORKER-RPC does.
;;;;
;;;; What is checked is kept apart from what is checked against, as in 4A:
;;;;
;;;; - The ACCOUNT is the harness's own record of which worker each session
;;;;   was lent, which of them it lost and how -- written from the operations
;;;;   it ran, never from the reset ledger.  A worker is lost to its session
;;;;   when it ends while bound to it; the loss is excused when the session
;;;;   is released or the pool shut down before it was told.
;;;; - What the session was TOLD is read from the responses themselves: each
;;;;   reset notice names its worker as "Worker <id>", with a phrase saying
;;;;   why it ended.  A pool-kill-worker call is read from what
;;;;   KILL-SESSION-WORKER hands its caller to tell.
;;;;
;;;; Each loss must be told exactly once, to its own session, with a cause
;;;; the harness actually applied; nothing else may be told; and once every
;;;; session has made a request, nothing is left untold.

(defpackage #:cl-mcp/specs/reset-fixtures
  (:use #:cl)
  (:import-from #:cl-mcp/src/pool
                #:kill-session-worker)
  (:import-from #:cl-mcp/src/proxy
                #:proxy-to-worker)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/worker-client
                #:worker
                #:worker-id
                #:worker-state
                #:worker-last-crash-reason)
  (:import-from #:cl-mcp/src/reset-events
                #:reset-event-worker-id
                #:reset-event-cause
                #:pending-session-resets
                #:discard-all-resets)
  (:import-from #:cl-mcp/src/object-registry
                #:register-object
                #:lookup-object
                #:clear-registry)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:text-content)
  (:import-from #:cl-mcp/specs/pool-fixtures
                #:+sessions+
                #:ledger-spawned
                #:killed-p
                #:with-fake-pool
                #:run-operation
                #:random-operation
                #:pool-snapshot
                #:ownership-violations)
  ;; Bare: the pool's lock is read through the BT nickname.
  (:import-from #:bordeaux-threads)
  (:export #:run-reset-sequence
           #:random-reset-sequence
           #:reset-violation-kinds
           #:told-in
           #:run-handle-sequence
           #:random-handle-sequence))

(in-package #:cl-mcp/specs/reset-fixtures)

;;; ------------------------------------------------------------------------
;;; A. Reading what a response told

(defparameter +cause-phrases+
  '(("stopped unexpectedly" . :crashed)
    ("did not answer within its deadline" . :timeout)
    ("retired itself" . :retired)
    ("was stopped to cancel" . :cancelled)
    ("was stopped by pool-kill-worker" . :killed)
    ("was stopped when its session was released" . :released)
    ("was stopped when the worker pool shut down" . :shutdown)
    ("was stopped by the worker pool" . :stopped))
  "How a reset notice says why its worker ended, and the cause each phrase
stands for.")

(defun told-in (text)
  "Return what TEXT told, as (WORKER-ID . CAUSE) pairs in order: one for each
\"Worker <id> <phrase>\" it contains.  CAUSE is :UNREADABLE when the phrase
is none of +CAUSE-PHRASES+."
  (let ((told '())
        (start 0))
    (loop
      (let ((at (search "Worker " text :start2 start)))
        (unless at (return))
        (let* ((digits-start (+ at (length "Worker ")))
               (digits-end (or (position-if-not
                                (lambda (c) (or (digit-char-p c) (char= c #\-)))
                                text :start digits-start)
                               (length text)))
               (id (and (< digits-start digits-end)
                        (ignore-errors
                         (parse-integer text :start digits-start :end digits-end)))))
          (when id
            (let ((phrase-start (min (length text) (1+ digits-end))))
              (push (cons id
                          (or (cdr (find-if (lambda (entry)
                                              (let ((phrase (car entry)))
                                                (and (<= (+ phrase-start (length phrase))
                                                         (length text))
                                                     (string= phrase text
                                                              :start2 phrase-start
                                                              :end2 (+ phrase-start
                                                                       (length phrase))))))
                                            +cause-phrases+))
                              :unreadable))
                    told)))
          (setf start digits-end))))
    (nreverse told)))

(defun %result-text (result)
  "Return RESULT's first text, or the empty string."
  (let ((content (and (hash-table-p result) (gethash "content" result))))
    (if (and (vectorp content) (plusp (length content)))
        (gethash "text" (aref content 0))
        "")))

;;; ------------------------------------------------------------------------
;;; B. The account

(defstruct account
  "What the harness did to workers, and what sessions were told -- written by
the harness alone."
  ;; Worker -> the session it was bound to, as the harness saw it.
  (bound (make-hash-table :test 'eq))
  ;; Worker -> the causes the harness applied to it, in order.
  (applied (make-hash-table :test 'eq))
  ;; Worker -> T once it ended while bound: a loss its session is owed.
  (lost (make-hash-table :test 'eq))
  ;; Worker -> T once its loss was excused: its session went away first.
  (excused (make-hash-table :test 'eq))
  ;; Worker id -> list of (SESSION . CAUSE) it was told as, oldest first.
  (told (make-hash-table :test 'eql))
  ;; Requests the fake RPC received, as (SESSION . REQUEST-NUMBER).
  (received '())
  (next-request 0))

(defun %apply-cause (account worker cause)
  "Record that the harness did CAUSE to WORKER."
  (when worker
    (setf (gethash worker (account-applied account))
          (append (gethash worker (account-applied account)) (list cause)))))

(defun %note-bindings (account)
  "Record every worker the pool's map holds bound, against its session."
  (loop for (session . entry) in (getf (pool-snapshot) :map)
        when (and (typep entry 'worker) (eq :bound (worker-state entry))
                  (not (gethash entry (account-bound account))))
          do (setf (gethash entry (account-bound account)) session)))

(defun %gone-p (ledger worker)
  "True when WORKER has ended: the pool ended it, its fake process died, or
an RPC marked it crashed."
  (or (killed-p ledger worker)
      (gethash worker (cl-mcp/specs/pool-fixtures::ledger-dead ledger))
      (member (worker-state worker) '(:crashed :dead :released))))

(defun %note-losses (ledger account)
  "Record as lost every bound worker that has ended."
  (maphash (lambda (worker session)
             (declare (ignore session))
             (when (and (not (gethash worker (account-lost account)))
                        (%gone-p ledger worker))
               (setf (gethash worker (account-lost account)) t)))
           (account-bound account)))

(defun %excuse (account predicate)
  "Excuse every loss not yet told whose worker and session satisfy PREDICATE:
the session it was owed to is gone.  A bound worker not yet lost is excused
too -- it is ended with its session, and owes nobody."
  (maphash (lambda (worker session)
             (when (and (funcall predicate worker session)
                        (null (gethash (worker-id worker) (account-told account))))
               (setf (gethash worker (account-excused account)) t)))
           (account-bound account)))

(defun %note-told (account session worker-id cause)
  "Record that SESSION was told WORKER-ID ended for CAUSE."
  (setf (gethash worker-id (account-told account))
        (append (gethash worker-id (account-told account))
                (list (cons session cause)))))

;;; ------------------------------------------------------------------------
;;; C. A request through the real proxy

(defun %fake-rpc (ledger account session crash)
  "Return a WORKER-RPC stand-in for SESSION's request: it keeps WORKER-RPC's
order -- a worker already ended is refused before the request is sent; the
BEFORE-SEND hook decides whether it is sent; then the worker answers, or dies
under it -- and records every request that reached the worker.  CRASH makes
it die under this request whatever its process's state."
  (lambda (worker method params &key timeout before-send after-receive
                                      preserve-json-types)
    (declare (ignore method params timeout preserve-json-types))
    ;; The pool handed this worker to SESSION's request: it is bound to it,
    ;; though it may end before any snapshot of the pool shows that.
    (when (and (eq :bound (worker-state worker))
               (not (gethash worker (account-bound account))))
      (setf (gethash worker (account-bound account)) session))
    (when (member (worker-state worker) '(:crashed :dead :released))
      (error 'cl-mcp/src/worker-client:worker-crashed
             :worker worker
             :reason (or (worker-last-crash-reason worker) "already-dead")))
    (when before-send
      (let ((verdict (funcall before-send)))
        (unless (eq :send verdict)
          (error 'cl-mcp/src/worker-client:rpc-not-sent
                 :worker worker :reason verdict))))
    (push (cons session (incf (account-next-request account)))
          (account-received account))
    (when (or crash (gethash worker (cl-mcp/specs/pool-fixtures::ledger-dead ledger)))
      ;; On the worker the request reached, which the proxy found itself.
      (when crash (%apply-cause account worker :request-crash))
      (setf (gethash worker (cl-mcp/specs/pool-fixtures::ledger-dead ledger)) t)
      (cl-mcp/src/worker-client::%mark-worker-crashed worker "eof")
      (error 'cl-mcp/src/worker-client:worker-crashed :worker worker :reason "eof"))
    (when (and after-receive (not (eq :publish (funcall after-receive))))
      (error 'cl-mcp/src/worker-client:rpc-answer-withdrawn :worker worker))
    (make-ht "content" (text-content "answered"))))

(defun %request (ledger account session &key crash)
  "Make SESSION's request through PROXY-TO-WORKER and return its result, and
the violations of what it said about itself."
  (let* ((before (length (account-received account)))
         (cl-mcp/src/proxy::%cached-worker-rpc% (progn
                                                  (cl-mcp/src/proxy::%ensure-cached-bindings)
                                                  (%fake-rpc ledger account session crash)))
         (*current-session-id* session)
         (result (proxy-to-worker (account-next-request account) "worker/eval"
                                  (make-ht)))
         (reached (> (length (account-received account)) before))
         (violations '()))
    ;; Bound for PROXY-TO-WORKER to find: the proxy's own cached binding.
    (declare (ignorable cl-mcp/src/proxy::%cached-worker-rpc%))
    (when (and (equal "not-executed" (gethash "execution_status" result)) reached)
      (push (list :kind :not-executed-but-received :session session) violations))
    (dolist (pair (told-in (%result-text result)))
      (%note-told account session (car pair) (cdr pair)))
    (values result violations)))

;;; ------------------------------------------------------------------------
;;; D. The checks

(defun %worker-with-id (ledger id)
  "Return the worker LEDGER saw with ID, or NIL."
  (find id (ledger-spawned ledger) :key #'worker-id))

(defun %telling-violations (ledger account &key final)
  "Return what was told wrongly so far, and with FINAL, what was never told.

- A worker is told at most once.
- Only a loss is told: a worker that was bound to the session it is told
  to, and has ended.
- A loss is told only while it is owed: not once excused.
- The cause told is one the harness applied, or a crash the pool found on
  its own when the process died.  Never :STOPPED, the fallback for an end
  nobody decided.
- FINAL: every loss neither excused nor told is a violation."
  (let ((violations '()))
    (flet ((add (kind &rest detail) (push (list* :kind kind detail) violations)))
      (maphash
       (lambda (id tellings)
         (let ((worker (%worker-with-id ledger id)))
           (when (> (length tellings) 1)
             (add :told-twice :worker id :tellings tellings))
           (destructuring-bind (session . cause) (first tellings)
             (let ((bound (and worker (gethash worker (account-bound account)))))
               (cond
                 ((null worker) (add :told-unknown-worker :worker id))
                 ((not (equal bound session))
                  (add :told-to-another-session :worker id :session session
                       :bound bound))
                 ((not (gethash worker (account-lost account)))
                  (add :told-before-lost :worker id))
                 ((gethash worker (account-excused account))
                  (add :told-after-excused :worker id))))
             (let ((applied (and worker (gethash worker (account-applied account)))))
               (unless (or (member cause applied)
                           (and (eq cause :crashed)
                                (or (member :died applied) (member :request-crash applied))))
                 (add :cause-unexplained :worker id :cause cause :applied applied))))))
       (account-told account))
      (when final
        (maphash (lambda (worker session)
                   (when (and (gethash worker (account-lost account))
                              (not (gethash worker (account-excused account)))
                              (null (gethash (worker-id worker) (account-told account))))
                     (add :never-told :worker (worker-id worker) :session session
                          :applied (gethash worker (account-applied account)))))
                 (account-bound account))))
    (nreverse violations)))

;;; ------------------------------------------------------------------------
;;; E. Operations

(defun %worker-at (ledger index)
  "Return the INDEXth worker LEDGER saw, modulo how many, or NIL."
  (let ((spawned (ledger-spawned ledger)))
    (when spawned (nth (mod index (length spawned)) spawned))))

(defun %bound-worker (session)
  "Return the worker the pool's map holds for SESSION, or NIL."
  (let ((entry (cdr (assoc session (getf (pool-snapshot) :map) :test #'equal))))
    (and (typep entry 'worker) entry)))

(defun %run-reset-operation (ledger model account operation)
  "Apply OPERATION and return the violations it produced.  The pool's own
operations are those of specs/pool-fixtures.lisp, with the causes they apply
recorded; two are this file's:

  (:request SESSION)            a request through the real proxy
  (:request-crash SESSION)      a request whose worker dies while running it
  (:session-rpc-crash SESSION)  an RPC to SESSION's worker times out"
  (destructuring-bind (kind &optional argument) operation
    (case kind
      (:request
       (nth-value 1 (%request ledger account argument)))
      (:request-crash
       (nth-value 1 (%request ledger account argument :crash t)))
      (:session-rpc-crash
       ;; An RPC to the worker SESSION holds times out: a death the pool
       ;; knows of at once, owed to that session, with no request to tell it.
       (let ((worker (%bound-worker argument)))
         (when (and worker (eq :bound (worker-state worker)))
           (%apply-cause account worker :timeout)
           (cl-mcp/src/worker-client::%mark-worker-crashed worker "timeout")
           (setf (gethash worker (cl-mcp/specs/pool-fixtures::model-unusable model)) t)))
       '())
      (:kill-session
       (let ((target (%bound-worker argument)))
         (when target (%apply-cause account target :killed))
         ;; What KILL-SESSION-WORKER hands its caller to tell is what the
         ;; pool-kill-worker response tells.
         (multiple-value-bind (outcome told) (kill-session-worker argument)
           (declare (ignore outcome))
           (dolist (event told)
             (%note-told account argument (reset-event-worker-id event)
                         (reset-event-cause event))))
         ;; RUN-OPERATION's own check of a kill, against the worker it ended.
         (cl-mcp/specs/pool-fixtures::%check-after-release ledger model argument
                                                            target)))
      (t
       (case kind
         (:release
          (let ((target (%bound-worker argument)))
            (when target (%apply-cause account target :released)))
          (%excuse account (lambda (worker session)
                             (declare (ignore worker))
                             (equal session argument))))
         (:shutdown
          (%excuse account (lambda (worker session)
                             (declare (ignore worker session))
                             t)))
         (:die
          (%apply-cause account (%worker-at ledger argument) :died))
         (:rpc-crash
          (%apply-cause account (%worker-at ledger argument) :timeout)))
       (nth-value 1 (run-operation ledger model operation))))))

(defun run-reset-sequence (operations &key (warmup 1) (max-size 4))
  "Run OPERATIONS against a fresh fake pool, with requests through the real
proxy, and return every violation, each tagged with the index and operation
it followed.

After each operation: the pool's ownership invariants (4A), the reset
account's (%TELLING-VIOLATIONS), and nothing told that the operation's own
result denied.  When the pool is still running afterwards, every session
makes requests until one reaches its worker, so every loss has had a
response to be told in -- a death is only known once something meets it; then
nothing may be left untold, by the account or by the ledger.  A shutdown
ends every sequence."
  ;; Whatever an earlier run left owed is not this run's to judge; it was
  ;; judged there (:LEDGER-HOLDS-AFTER-SHUTDOWN).
  (discard-all-resets)
  (let ((violations '())
        (model (cl-mcp/specs/pool-fixtures::make-model))
        (account (make-account)))
    (flet ((note (index operation found)
             (dolist (violation found)
               (push (list* :index index :operation operation violation)
                     violations))))
      (with-fake-pool (ledger :warmup warmup :max-size max-size)
        (flet ((run-step (index operation)
                 (%note-bindings account)
                 (note index operation
                       (%run-reset-operation ledger model account operation))
                 (%note-bindings account)
                 (%note-losses ledger account)
                 ;; A request binds workers too, through the real acquire.
                 (note index operation
                       (cl-mcp/specs/pool-fixtures::%check-new-bindings ledger model))
                 (note index operation
                       (ownership-violations
                        ledger
                        :stable (and cl-mcp/src/pool::*pool-running*
                                     (null (cl-mcp/specs/pool-fixtures:ledger-tasks
                                            ledger)))
                        :shut-down (not cl-mcp/src/pool::*pool-running*)))
                 (note index operation (%telling-violations ledger account))))
          (loop for operation in operations
                for index from 0
                do (run-step index operation))
          (let ((index (length operations)))
            (when cl-mcp/src/pool::*pool-running*
              ;; Every loss has had a response to be told in.  A request that
              ;; tells a reset in its own place is not sent, so it cannot find
              ;; a death nobody has discovered yet; the session asks again,
              ;; until a request reaches its worker.
              (dolist (session +sessions+)
                (loop repeat 3
                      for sent-before = (length (account-received account))
                      do (run-step index (list :request session))
                      until (> (length (account-received account)) sent-before)))
              (%note-losses ledger account)
              (note index '(:drain) (%telling-violations ledger account :final t))
              (let ((untold (loop for session in +sessions+
                                  when (pending-session-resets session)
                                    collect session)))
                (when untold
                  (note index '(:drain)
                        (list (list :kind :ledger-holds-untold :sessions untold))))))
            (run-step index '(:shutdown)))))
      ;; After the teardown too: a shutdown owes nobody anything, and a reset
      ;; left pending now would greet whoever next uses the session's id.
      (let ((left (loop for session in +sessions+
                        when (pending-session-resets session)
                          collect session)))
        (when left
          (note (length operations) '(:teardown)
                (list (list :kind :ledger-holds-after-shutdown :sessions left))))))
    (discard-all-resets)
    (remove-duplicates (nreverse violations) :test #'equal)))

(defun random-reset-sequence (&optional (length (+ 5 (random 26))))
  "Return LENGTH operations: the pool's (RANDOM-OPERATION), requests, some
of them meeting their worker's death, and deaths of the worker a session
holds."
  (loop repeat length
        collect (let ((roll (random 100))
                      (session (nth (random (length +sessions+)) +sessions+)))
                  (cond ((< roll 25) (list :request session))
                        ((< roll 32) (list :request-crash session))
                        ((< roll 42) (list :session-rpc-crash session))
                        (t (random-operation))))))

(defun reset-violation-kinds (violations)
  "Return the distinct kinds of VIOLATIONS."
  (remove-duplicates (mapcar (lambda (v) (getf v :kind)) violations)))

;;; ------------------------------------------------------------------------
;;; F. Object handles across images

(defun run-handle-sequence (operations)
  "Run OPERATIONS over two images' registries and return every lookup that
answered wrongly.

Operations:
  (:register IMAGE)      register a fresh object in IMAGE's registry
  (:clear IMAGE)         clear IMAGE's registry
  (:replace IMAGE)       IMAGE gets a new registry: its worker was replaced
  (:lookup INDEX IMAGE)  look the INDEXth handle issued so far up in IMAGE

A handle is found only in the registry that issued it, and only until that
registry is cleared; anywhere else, or after that, it is refused as stale.
It never names another object.  The model is the list of handles issued,
each with its object, its registry and whether that registry was cleared
since -- nothing about how the registry parses a handle."
  (let ((registries (vector (cl-mcp/src/object-registry::make-object-registry)
                            (cl-mcp/src/object-registry::make-object-registry)))
        (issued '())                    ; (handle object registry epoch), newest first
        (epochs (make-hash-table :test 'eq))
        (violations '()))
    (loop for operation in operations
          for index from 0
          do (destructuring-bind (kind a &optional b) operation
               (ecase kind
                 (:register
                  (let* ((registry (aref registries a))
                         (object (list :object index))
                         (handle (register-object object registry)))
                    (push (list handle object registry (gethash registry epochs 0))
                          issued)))
                 (:clear
                  (clear-registry (aref registries a))
                  (incf (gethash (aref registries a) epochs 0)))
                 (:replace
                  (setf (aref registries a)
                        (cl-mcp/src/object-registry::make-object-registry)))
                 (:lookup
                  (when issued
                    (destructuring-bind (handle object registry epoch)
                        (nth (mod a (length issued)) (reverse issued))
                      (let ((here (aref registries b)))
                        (multiple-value-bind (found found-p why)
                            (lookup-object handle here)
                          (let ((current (and (eq here registry)
                                              (= epoch (gethash registry epochs 0)))))
                            (cond
                              ((and found-p (not (eq found object)))
                               (push (list :kind :another-object :index index
                                           :handle handle)
                                     violations))
                              ((and found-p (not current))
                               (push (list :kind :stale-handle-resolved :index index
                                           :handle handle)
                                     violations))
                              ((and (not found-p) current)
                               (push (list :kind :current-handle-refused :index index
                                           :handle handle :why why)
                                     violations))
                              ((and (not found-p) (not (eq why :stale)))
                               (push (list :kind :stale-not-said :index index
                                           :handle handle :why why)
                                     violations))))))))))))
    (nreverse violations)))

(defun random-handle-sequence (&optional (length (+ 5 (random 36))))
  "Return LENGTH handle operations drawn with CL:RANDOM, mostly registrations
and lookups."
  (loop repeat length
        collect (let ((roll (random 100)) (image (random 2)))
                  (cond ((< roll 35) (list :register image))
                        ((< roll 80) (list :lookup (random 20) image))
                        ((< roll 90) (list :clear image))
                        (t (list :replace image))))))
