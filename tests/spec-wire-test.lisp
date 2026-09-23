;;;; tests/spec-wire-test.lisp
;;;;
;;;; The cl-spec tools as a client meets them: a real TCP server, a real
;;;; worker per session, the MCP handshake, and every answer read from the
;;;; bytes on the socket with false, true, null, [] and a missing key kept
;;;; apart.
;;;;
;;;; tests/spec-worker-test.lisp calls the proxy directly and reads the
;;;; hash-table it returns, so it cannot see what the parent does to a result
;;;; on its way out -- which is where a worker's false used to become the null
;;;; an inline call never sends.  These tests go through the public tools
;;;; only: load-system, spec-list, spec-symbol, spec-describe, spec-check,
;;;; inspect-object and pool-status, called over the wire in one session.
;;;;
;;;; Needs a real cl-spec and a spawnable worker, and runs in a process of its
;;;; own (CL_MCP_SPECS_MODE=integration, see docs/specs.md), where a skipped
;;;; test fails the step.  Not listed in tests.lisp: the default suite has no
;;;; cl-spec, and a suite that skipped there would be a suite nobody ran.

(defpackage #:cl-mcp/tests/spec-wire-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tcp
                #:start-tcp-server-thread
                #:stop-tcp-server-thread)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/log
                #:*log-level*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:import-from #:cl-mcp/specs/spec-response-fixtures
                #:parse-response
                #:json-at
                #:json-kind
                #:json-array-p
                #:json-object-p
                #:json-true-p
                #:json-false-p
                #:line-starting-with
                #:replay-arguments
                #:claims-p)
  (:import-from #:usocket))

(in-package #:cl-mcp/tests/spec-wire-test)

(defparameter +fixture-system+ "cl-mcp/tests/fixtures/spec-wire-fixture"
  "The declarations every session loads, by the name load-system takes.")

(defparameter +fixture-package+ "CL-MCP/TESTS/FIXTURES/SPEC-WIRE-FIXTURE"
  "The package the fixture defines.")

(defparameter +call-timeout-seconds+ 300
  "How long a client waits for one answer.  Loading cl-spec into a fresh
worker compiles nothing that is already cached, but a cold cache compiles
the whole of it.")

(defun %fixture (name)
  "Return the qualified designator of the fixture symbol NAME."
  (format nil "~A::~A" +fixture-package+ name))

;;; ------------------------------------------------------------------------
;;; The server

(defun %start-server (worker-pool)
  "Start a TCP server accepting any number of sessions and return its port.

The cl-spec group is switched on here, as a client's server would have it."
  (let ((port nil))
    (start-tcp-server-thread :port 0 :accept-once nil
                             :worker-pool worker-pool
                             :tool-groups (list :cl-spec)
                             :on-listening (lambda (actual) (setf port actual)))
    (or port (error "The TCP server did not report a port."))))

(defmacro with-wire-server ((port &key worker-pool) &body body)
  "Run BODY with PORT bound to a fresh server's port, and stop the server --
its pool, its workers and its listener -- however BODY exits.

*USE-WORKER-POOL* is put back afterwards: starting a server sets it for the
whole image, and the next test decides for itself.  The log level is raised
to warnings while the server runs, set rather than bound because the server
logs from threads of its own: at the default it writes every line it sends,
and a failure would be buried under the answers these tests read anyway."
  (let ((previous (gensym "PREVIOUS"))
        (level (gensym "LEVEL")))
    `(let ((,previous *use-worker-pool*)
           (,level *log-level*))
       (unwind-protect
            (progn
              (setf *log-level* :warn)
              (let ((,port (%start-server ,worker-pool)))
                ,@body))
         (stop-tcp-server-thread)
         (setf *use-worker-pool* ,previous
               *log-level* ,level)))))

;;; ------------------------------------------------------------------------
;;; The client

(defstruct (client (:constructor %make-client (socket stream)))
  "One MCP session: a TCP connection and the ids it has used."
  socket
  stream
  (next-id 0))

(defun %send (client object)
  "Write OBJECT to CLIENT's connection as one JSON line."
  (let ((stream (client-stream client)))
    (yason:encode object stream)
    (terpri stream)
    (finish-output stream)))

(defun %read-answer (client id)
  "Return (values DOCUMENT LINE) for the answer to request ID.

Read from the socket and parsed with the five JSON answers kept apart, so a
false here is a false the server wrote.  A line that answers something else
-- a notification -- is passed over.

An empty answer from WAIT-FOR-INPUT is not taken for the deadline -- CI saw
one 1.4 seconds into a 300-second wait: a signal
interrupts the wait too, and a server whose workers are exiting sends this
process SIGCHLD.  Only the clock says the deadline passed."
  (let ((stream (client-stream client))
        (deadline (+ (get-universal-time) +call-timeout-seconds+)))
    (loop
      ;; LISTEN first: a line already in the stream's buffer leaves nothing
      ;; for the socket to report.
      (loop until (or (listen stream)
                      (usocket:wait-for-input (client-socket client)
                                              :timeout 1 :ready-only t))
            when (> (get-universal-time) deadline)
              do (error "No answer to request ~A within ~D seconds."
                        id +call-timeout-seconds+))
      (let* ((line (or (read-line stream nil nil)
                       (error "The server closed the connection.")))
             (document (parse-response line)))
        (when (eql id (json-at document "id"))
          (return (values document line)))))))

(defun %request (client method &optional params)
  "Send METHOD with PARAMS as a request and return the answer's DOCUMENT."
  (let ((id (incf (client-next-id client))))
    (%send client (make-ht "jsonrpc" "2.0" "id" id "method" method
                           "params" (or params (make-ht))))
    (%read-answer client id)))

(defun %connect (port)
  "Open a session on PORT and complete the MCP handshake; return the client
and the initialize result."
  (let* ((socket (usocket:socket-connect "127.0.0.1" port
                                         :element-type 'character
                                         :connection-timeout 10))
         (client (%make-client socket (usocket:socket-stream socket)))
         (answer (%request client "initialize"
                           (make-ht "protocolVersion" "2025-06-18"
                                    "capabilities" (make-ht)
                                    "clientInfo" (make-ht "name" "spec-wire-test"
                                                          "version" "1")))))
    (%send client (make-ht "jsonrpc" "2.0" "method" "notifications/initialized"))
    (values client (json-at answer "result"))))

(defun %disconnect (client)
  "Close CLIENT's connection."
  (ignore-errors (usocket:socket-close (client-socket client))))

(defmacro with-session ((client port) &body body)
  "Run BODY with CLIENT connected to PORT, and close it however BODY exits."
  `(let ((,client (%connect ,port)))
     (unwind-protect (progn ,@body)
       (%disconnect ,client))))

(defun %tool (client name &rest arguments)
  "Call tool NAME with ARGUMENTS (alternating string keys and values) and
return (values RESULT TEXT): the tool result as the client read it, and the
text a client renders."
  (let* ((document (%request client "tools/call"
                             (make-ht "name" name
                                      "arguments" (apply #'make-ht arguments))))
         (result (json-at document "result"))
         (content (json-at result "content")))
    (values result
            (if (and (json-array-p content) (plusp (length content)))
                (json-at (aref content 0) "text")
                ""))))

(defun %tool-names (client)
  "Return the names tools/list gives CLIENT."
  (map 'list (lambda (tool) (json-at tool "name"))
       (json-at (%request client "tools/list") "result" "tools")))

(defun %load-fixture (client)
  "Load cl-spec and the fixture in CLIENT's session through load-system, and
return the two texts."
  (list (nth-value 1 (%tool client "load-system" "system" "cl-spec/check-it"
                            "timeout_seconds" +call-timeout-seconds+))
        (nth-value 1 (%tool client "load-system" "system" +fixture-system+
                            "timeout_seconds" +call-timeout-seconds+))))

(defun %qualified (node)
  "Return the qualified name a listing entry or a symbol object carries."
  (if (json-object-p node) (json-at node "qualified") node))

(defun %names (entries)
  "Return the qualified names of an array of listed definitions."
  (map 'list (lambda (entry) (%qualified (json-at entry "name"))) entries))

(defun %same-names-p (expected names)
  "True when NAMES are EXPECTED's fixture names, in any order."
  (null (set-exclusive-or (mapcar #'%fixture expected) names :test #'equal)))

(defun %first-result (document)
  "Return the first per-result object of a spec-check DOCUMENT."
  (let ((results (json-at document "results")))
    (when (and (json-array-p results) (plusp (length results)))
      (aref results 0))))

(defun %replay-call (client text)
  "Call spec-check with the arguments TEXT's Replay line asks for, taken from
the line as printed -- the trial budget, the one number, as a number."
  (let ((arguments (replay-arguments (line-starting-with text "Replay: spec-check "))))
    (apply #'%tool client "spec-check"
           (loop for (key value) on arguments by #'cddr
                 append (list key (if (equal "trials" key) (parse-integer value) value))))))

(defun %decimal-string-p (value)
  "True when VALUE is a non-empty string of decimal digits."
  (and (stringp value) (plusp (length value)) (every #'digit-char-p value)))

(defun %bound-worker-pids (client)
  "Return the pids of the workers pool-status reports bound to a session."
  (loop for worker across (json-at (%tool client "pool-status") "workers")
        when (equal "bound" (json-at worker "state"))
          collect (json-at worker "pid")))

(defun %all-worker-pids (client)
  "Return the pid of every worker pool-status reports."
  (map 'list (lambda (worker) (json-at worker "pid"))
       (json-at (%tool client "pool-status") "workers")))

(defun %process-gone-p (pid &key (within 15))
  "True when no process PID exists -- not even as a zombie -- within WITHIN
seconds."
  (loop repeat (* 10 within)
        unless (probe-file (format nil "/proc/~D/" pid)) return t
        do (sleep 0.1)
        finally (return nil)))

;;; ------------------------------------------------------------------------
;;; Comparing two answers

(defun %run-dependent-p (path)
  "True for a field whose value belongs to one run in one image: how long it
took, which object id the image handed out, and which registry object it
printed.  Only their JSON kind is compared -- a null where a string was is
still a difference."
  (or (search "elapsed" path)
      (search "object_id" path)
      (search "environment.registry" path)))

(defun %differences (one other &optional (path ""))
  "Return where the parsed JSON ONE and OTHER differ, as (PATH ONE OTHER)
entries, walking both together.

The nodes are compared, not a flattening of them.  An object's keys are
compared as a set, so a key one side lacks is a difference even when its
value on the other side is an empty object; an array's length is compared
before its elements; and a leaf is compared by value, so true against false
is a difference and so is false against null.  Only at a run-dependent
path (%RUN-DEPENDENT-P) is a leaf compared by its JSON kind instead, and
even there a number against a null is a difference."
  (flet ((here (a b) (list (list path a b))))
    (cond
      ((and (json-object-p one) (json-object-p other))
       (let ((keys (union (loop for key being the hash-keys of one collect key)
                          (loop for key being the hash-keys of other collect key)
                          :test #'equal)))
         (loop for key in (sort keys #'string<)
               for child-path = (format nil "~A.~A" path key)
               append (multiple-value-bind (a a-present) (gethash key one)
                        (multiple-value-bind (b b-present) (gethash key other)
                          (if (and a-present b-present)
                              (%differences a b child-path)
                              (list (list child-path
                                          (if a-present a :absent)
                                          (if b-present b :absent)))))))))
      ((and (json-array-p one) (json-array-p other))
       (if (/= (length one) (length other))
           (here (list :length (length one)) (list :length (length other)))
           (loop for a across one
                 for b across other
                 for index from 0
                 append (%differences a b (format nil "~A[~D]" path index)))))
      ((or (json-object-p one) (json-object-p other)
           (json-array-p one) (json-array-p other))
       (here one other))
      ((%run-dependent-p path)
       (unless (eq (json-kind one t) (json-kind other t))
         (here one other)))
      ((equal one other) '())
      (t (here one other)))))

;;; ------------------------------------------------------------------------
;;; Tests

(deftest the-comparison-sees-the-differences-it-claims-to
  ;; The inline-against-pool test is only as good as this function: a
  ;; comparison that reads true and false as one answer, or an empty object as
  ;; nothing at all, would pass a transport that changed either.
  (flet ((differ-p (one other)
           (and (%differences (parse-response one) (parse-response other)) t)))
    (loop for (label one other expected)
            in '(("true against false" "{\"flag\":true}" "{\"flag\":false}" t)
                 ("false against null" "{\"flag\":false}" "{\"flag\":null}" t)
                 ("[] against null" "{\"xs\":[]}" "{\"xs\":null}" t)
                 ("\"\" against []" "{\"s\":\"\"}" "{\"s\":[]}" t)
                 ("a key holding {} against no key" "{\"x\":{}}" "{}" t)
                 ("a key holding null against no key" "{\"x\":null}" "{}" t)
                 ("one {} against two" "{\"xs\":[{}]}" "{\"xs\":[{},{}]}" t)
                 ("a string against another" "{\"s\":\"a\"}" "{\"s\":\"b\"}" t)
                 ("a deep false against true"
                  "{\"a\":[{\"b\":{\"c\":false}}]}" "{\"a\":[{\"b\":{\"c\":true}}]}" t)
                 ("an object against an array" "{\"x\":{}}" "{\"x\":[]}" t)
                 ("the same document, keys in another order"
                  "{\"a\":1,\"b\":[true,null,{}]}" "{\"b\":[true,null,{}],\"a\":1}" nil)
                 ("elapsed times that differ only in value"
                  "{\"results\":[{\"elapsed\":0.5}]}" "{\"results\":[{\"elapsed\":0.25}]}" nil)
                 ("object ids that differ only in value"
                  "{\"value\":{\"object_id\":\"o-a-3\"}}" "{\"value\":{\"object_id\":\"o-b-91\"}}" nil)
                 ("an elapsed time against null"
                  "{\"results\":[{\"elapsed\":0.5}]}" "{\"results\":[{\"elapsed\":null}]}" t)
                 ("an object id against null"
                  "{\"value\":{\"object_id\":\"o-a-3\"}}" "{\"value\":{\"object_id\":null}}" t))
          do (ok (eq expected (differ-p one other))
                 (format nil "~A: ~:[the same~;a difference~]" label expected)))))

(deftest one-session-loads-discovers-checks-and-replays
  (with-wire-server (port :worker-pool t)
    (multiple-value-bind (client initialize) (%connect port)
      (unwind-protect
           (progn
             (testing "the handshake, and the cl-spec group it was started with"
               (ok (equal "2025-06-18" (json-at initialize "protocolVersion")))
               (ok (member "spec-check" (%tool-names client) :test #'equal)))
             (testing "load-system brings in cl-spec and the declarations"
               (ok (every (lambda (text) (claims-p text "loaded successfully"))
                          (%load-fixture client))))
             (testing "spec-list finds what that load registered"
               (let ((listing (%tool client "spec-list" "package" +fixture-package+)))
                 (ok (equal "ok" (json-at listing "status")))
                 (ok (%same-names-p '("WIRE-CLAMP-STAYS-INSIDE"
                                      "WIRE-CLAMP-IS-WRONG-ON-PURPOSE"
                                      "WIRE-LISTS-ARE-EMPTY")
                                    (%names (json-at listing "properties"))))))
             (testing "spec-symbol and spec-describe read it"
               (let ((symbol (%tool client "spec-symbol" "symbol" (%fixture "WIRE-CLAMP"))))
                 (ok (equal "ok" (json-at symbol "status")))
                 (ok (%same-names-p '("WIRE-CLAMP-STAYS-INSIDE"
                                      "WIRE-CLAMP-IS-WRONG-ON-PURPOSE")
                                    (%names (json-at symbol "properties")))))
               (let ((described (%tool client "spec-describe" "kind" "function-spec"
                                       "name" (%fixture "WIRE-CLAMP"))))
                 (ok (equal "ok" (json-at described "status")))
                 (ok (equal (%fixture "WIRE-CLAMP")
                            (%qualified (json-at described "name"))))))
             (multiple-value-bind (checked text)
                 (%tool client "spec-check" "property"
                        (%fixture "WIRE-CLAMP-IS-WRONG-ON-PURPOSE"))
               (let ((result (%first-result checked)))
                 (testing "a failing check arrives with its false still false"
                   (ok (json-false-p (json-at checked "verified"))
                       "verified is false on the wire, not null")
                   (ok (json-false-p (json-at checked "thread_leaked")))
                   (ok (equal "completed" (json-at checked "status")))
                   (ok (equal "safe" (json-at checked "worker_reuse")))
                   (ok (equal "failed" (json-at result "status")))
                   (ok (equal "present" (json-at result "counterexample_status")))
                   (ok (%decimal-string-p (json-at result "seed")))
                   (ok (stringp (json-at result "definition_digest")))
                   (ok (and (json-array-p (json-at checked "verification_gaps"))
                            (plusp (length (json-at checked "verification_gaps"))))))
                 (testing "and the Replay line it printed runs the same check again"
                   (let* ((replayed (%replay-call client text))
                          (again (%first-result replayed)))
                     (ok (equal "faithful" (json-at replayed "reproduction_faithful")))
                     (ok (equal "match" (json-at again "definition_match")))
                     (ok (equal (json-at result "seed") (json-at again "seed")))
                     (ok (equal (json-at result "definition_digest")
                                (json-at again "definition_digest")))
                     (ok (null (%differences (json-at result "counterexample")
                                             (json-at again "counterexample"))))))))
             (testing "a check that holds arrives as true"
               (let ((checked (%tool client "spec-check" "function" (%fixture "WIRE-CLAMP")
                                     "trials" 20)))
                 (ok (json-true-p (json-at checked "verified")))
                 (ok (equal "passed" (json-at (%first-result checked) "status"))))))
        (%disconnect client)))))

(deftest a-counterexample-object-belongs-to-the-session-that-made-it
  (with-wire-server (port :worker-pool t)
    (with-session (owner port)
      (with-session (other port)
        (%load-fixture owner)
        (let* ((checked (%tool owner "spec-check" "property" (%fixture "WIRE-LISTS-ARE-EMPTY")))
               (entry (aref (json-at (%first-result checked) "counterexample") 0))
               (id (json-at entry "value" "object_id")))
          (ok (stringp id) "a list earns an object id")
          (testing "the session that ran the check can inspect it"
            (multiple-value-bind (inspected text) (%tool owner "inspect-object" "id" id)
              (ok (not (json-true-p (json-at inspected "isError"))))
              (ok (claims-p text (json-at entry "value" "printed")))))
          (testing "another session cannot: the object lives in the other worker"
            ;; Its id names the owner's image, so the other worker refuses it
            ;; as stale rather than looking the number up in its own registry.
            (multiple-value-bind (inspected text) (%tool other "inspect-object" "id" id)
              (ok (json-true-p (json-at inspected "isError")))
              (ok (claims-p text "stale")))))))))

(deftest sessions-keep-their-own-registries-and-run-in-their-workers
  (let ((parent-had-fixture (and (find-package +fixture-package+) t)))
    (with-wire-server (port :worker-pool t)
      (with-session (owner port)
        (with-session (other port)
          (%load-fixture owner)
          (testing "the session that loaded the declarations sees them"
            (ok (equal "ok" (json-at (%tool owner "spec-symbol" "symbol"
                                            (%fixture "WIRE-CLAMP"))
                                     "status"))))
          (testing "a second session on the same server does not"
            (ok (not (equal "ok" (json-at (%tool other "spec-symbol" "symbol"
                                                 (%fixture "WIRE-CLAMP"))
                                          "status")))))
          (testing "each session has a worker of its own, and none is the server"
            (let ((pids (%bound-worker-pids owner)))
              (ok (= 2 (length pids)))
              (ok (= 2 (length (remove-duplicates pids))))
              (ok (not (member (sb-unix:unix-getpid) pids)))))
          (testing "the server's own image never loaded them"
            (ok (not parent-had-fixture)
                "this test runs before anything loads the fixture into the server")
            (ok (null (find-package +fixture-package+)))))))))

(deftest a-stopped-server-leaves-no-worker-behind
  (let ((pids '())
        (stopped-port nil))
    (with-wire-server (port :worker-pool t)
      (setf stopped-port port)
      (with-session (one port)
        (with-session (two port)
          (%tool one "repl-eval" "code" "1")
          (%tool two "repl-eval" "code" "2")
          (setf pids (%all-worker-pids one))
          (ok (<= 2 (length pids))))))
    (testing "every worker it spawned is gone, reaped rather than left a zombie"
      (ok (every #'%process-gone-p pids)))
    (testing "and nothing listens on its port"
      (ok (null (ignore-errors
                 (usocket:socket-close
                  (usocket:socket-connect "127.0.0.1" stopped-port
                                          :element-type 'character
                                          :connection-timeout 2))
                 t))))))

(deftest inline-and-pool-send-the-same-json
  ;; Last in the file: the inline half loads the declarations into this
  ;; process, which the session test above asserts has not happened.
  (flet ((answers (worker-pool)
           (with-wire-server (port :worker-pool worker-pool)
             (with-session (client port)
               (%load-fixture client)
               (list
                (cons "failing property"
                      (%tool client "spec-check" "property"
                             (%fixture "WIRE-CLAMP-IS-WRONG-ON-PURPOSE") "seed" "12345"))
                (cons "passing property"
                      (%tool client "spec-check" "property"
                             (%fixture "WIRE-CLAMP-STAYS-INSIDE") "seed" "12345"))
                (cons "contract"
                      (%tool client "spec-check" "function" (%fixture "WIRE-CLAMP")
                             "trials" 20 "seed" "12345"))
                (cons "compound counterexample"
                      (%tool client "spec-check" "property"
                             (%fixture "WIRE-LISTS-ARE-EMPTY") "seed" "12345"))
                (cons "spec-symbol"
                      (%tool client "spec-symbol" "symbol" (%fixture "WIRE-CLAMP")))
                (cons "spec-describe"
                      (%tool client "spec-describe" "kind" "function-spec"
                             "name" (%fixture "WIRE-CLAMP")))
                (cons "spec-list"
                      (%tool client "spec-list" "package" +fixture-package+)))))))
    (let ((pooled (answers t))
          (inline (answers nil)))
      (loop for (label . one) in pooled
            for (nil . other) in inline
            do (let ((differences (%differences one other)))
                 (ok (null differences)
                     (format nil "~A: the same JSON through a worker as inline~@[; ~
differs at ~{~S~^, ~}~]"
                             label
                             (subseq differences 0 (min 6 (length differences))))))))))
