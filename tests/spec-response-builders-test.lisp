;;;; tests/spec-response-builders-test.lisp
;;;;
;;;; The cl-spec tool responses, checked for the two things that actually
;;;; reach a client: the structured fields, and the content text.  An MCP
;;;; client renders only content[].text, so anything a caller must not miss --
;;;; a zero-property selection, a timeout, a definition mismatch -- has to be
;;;; in the text as well as in the payload.

(defpackage #:cl-mcp/tests/spec-response-builders-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:yason
                #:false)
  (:import-from #:cl-mcp/src/tools/spec-response-builders
                #:build-spec-list-response
                #:build-spec-symbol-response
                #:build-spec-describe-response
                #:build-spec-check-response))

(in-package #:cl-mcp/tests/spec-response-builders-test)

(deftest core-schema-is-distinct-from-mcp-schema
  (let* ((metadata '(:schema-version 1 :record-kind :definition :entity-kind :spec
                     :definition-digest "core" :definition-digest-complete t
                     :definition-digest-covers :declaration-and-registered-dependencies
                     :capabilities (:generation :available :shrinking :none
                                    :instrumentation :unavailable)))
         (response (build-spec-describe-response
                    (list :status :ok :kind "spec" :core-schema metadata)))
         (core (gethash "core_schema" response)))
    (ok (equal "1" (gethash "schema_version" response)))
    (ok (hash-table-p core))
    (when core
      (ok (eql 1 (gethash "schema_version" core)))
      (ok (equal "spec" (gethash "entity_kind" core)))
      (ok (equal "core" (gethash "definition_digest" core)))
      (ok (equal "none" (gethash "shrinking" (gethash "capabilities" core)))))))

(defun first-text (response)
  "Pull the text of the first content part out of RESPONSE, or NIL."
  (let ((content (gethash "content" response)))
    (when (and (vectorp content) (plusp (length content)))
      (gethash "text" (aref content 0)))))

(defparameter *environment*
  (list :cl-spec-loaded t :cl-spec-status :ok :cl-spec-version "0.1.0"
        :cl-spec-system-directory "/tmp/cl-spec/"
        :generator-backend "CL-SPEC/SRC/BACKENDS/CHECK-IT:CHECK-IT-BACKEND"
        :backend-available t :registry "#<HASH-TABLE-REGISTRY>"
        :missing nil :lisp "SBCL 2.4.0")
  "A healthy environment plist, shared by the cases that are not about it.")

(defun %symbol-data (package name)
  "Return the symbol plist for PACKAGE and NAME."
  (list :package package :name name
        :qualified (format nil "~A::~A" package name)))

(defun %contract-check-report (&key failure-reason failure-reason-readable
                                    rejected-overcounted (rejected 3)
                                    (effective-trials 27)
                                    (precondition-p t)
                                    (rejection-status :usable))
  "Return a completed contract check report whose single result failed."
  (list :status :completed
        :verified nil
        ;; "contract", the mode %SELECT-NAMED actually emits for a function
        ;; selection -- %SELECTION-NOUN keys on it, and "function" here sent
        ;; every test built on this fixture down the property branch.
        :selection (list :mode "contract" :kind :contract :count 1
                         :selected (list (%symbol-data "PROBE" "WIDEN"))
                         :source "explicit function argument"
                         :coverage "Only the contract named.")
        :results
        (list (list :property (%symbol-data "PROBE" "WIDEN")
                    :kind :contract
                    :status :failed
                    :trials (list :executed 30 :budget 30
                                  :budget-source "requested")
                    :contract (list :rejected rejected
                                    :rejection-status rejection-status
                                    :precondition-p precondition-p
                                    :rejected-measured t
                                    :rejected-overcounted rejected-overcounted
                                    :effective-trials effective-trials
                                    :failure-reason failure-reason
                                    :failure-reason-readable
                                    failure-reason-readable)
                    ;; No :PROFILE: a contract run has none, and production
                    ;; now leaves the key out rather than publishing the
                    ;; :NORMAL that leaks off cl-spec's synthetic property.
                    :seed "7"
                    :counterexample nil
                    :counterexample-status :present
                    :shrunk-counterexample nil
                    :shrink-status :present
                    :definition-match :not-checked))
        :counts (list :selected 1 :passed 0 :failed 1
                      :errored 0 :timed-out 0 :not-run 0)
        :environment *environment*))

(deftest not-loaded-response-says-what-to-load
  (testing "the cl-spec-not-loaded answer is actionable in the text itself"
    (let* ((response (build-spec-symbol-response
                      (list :status :cl-spec-not-loaded
                            :message "cl-spec is not loaded ... load-system ..."
                            :environment (list :cl-spec-loaded nil
                                               :cl-spec-status :not-loaded
                                               :lisp "SBCL 2.4.0"))))
           (text (first-text response)))
      (ok (string= "cl-spec-not-loaded" (gethash "status" response)))
      (ok (search "load-system" text)))))

(deftest symbol-response-lists-properties-in-text
  (testing "the property names reach the text, not only the payload"
    (let* ((response (build-spec-symbol-response
                      (list :status :ok
                            :symbol (%symbol-data "PROBE" "ADD")
                            :runtime (list :type "function" :arglist "(A B)"
                                           :documentation nil
                                           :source-file "probe.lisp"
                                           :source-line 42)
                            :registry (list :spec nil :function-spec nil
                                            :property nil
                                            :properties-about
                                            (list (%symbol-data "PROBE"
                                                                "ADD-COMMUTES")))
                            :properties
                            (list (list :name (%symbol-data "PROBE" "ADD-COMMUTES")
                                        :kind :commutativity
                                        :tags (list :math)
                                        :documentation "Addition commutes."
                                        :definition-digest "a41f9c2b7d0e5518"
                                        :body-forms 1 :body-omitted t
                                        :shrink-enabled t))
                            :nothing-registered nil
                            :notes (list (concatenate
                                          'string
                                          "properties_about lists direct "
                                          "(:about ...) registrations only"))
                            :environment *environment*)))
           (text (first-text response)))
      (ok (string= "ok" (gethash "status" response)))
      (ok (search "PROBE::ADD-COMMUTES" text))
      (ok (search "commutativity" text))
      (ok (search "a41f9c2b7d0e5518" text))
      (testing "and the omission of the body is stated"
        (ok (search "spec-describe" text)))
      (testing "the payload keeps package and name apart"
        (ok (string= "PROBE" (gethash "package" (gethash "symbol" response))))
        (ok (string= "ADD" (gethash "name" (gethash "symbol" response))))))))

(deftest symbol-response-nothing-registered
  (testing "an empty registry answer says it is not a clean bill of health"
    (let* ((response (build-spec-symbol-response
                      (list :status :ok
                            :symbol (%symbol-data "PROBE" "HELPER")
                            :runtime nil
                            :runtime-unavailable-reason "not fbound"
                            :registry (list :spec nil :function-spec nil
                                            :property nil :properties-about nil)
                            :properties nil
                            :nothing-registered t
                            :environment *environment*)))
           (text (first-text response)))
      (ok (eq t (gethash "nothing_registered" response)))
      (ok (search "Nothing is registered" text))
      (ok (search "not evidence" text)))))

(deftest check-response-zero-properties-warns-in-text
  (testing "a zero selection is loud in the text, not only in a status field"
    (let* ((response (build-spec-check-response
                      (list :status :no-properties
                            :verified nil
                            :selection (list :mode "about"
                                             :requested (list :symbol (%symbol-data
                                                                       "PROBE" "HELPER"))
                                             :selected nil :count 0
                                             :source "cl-spec:semantic-data -> :properties-about"
                                             :coverage "Direct (:about ...) registrations only.")
                            :results nil
                            :counts (list :selected 0 :passed 0 :failed 0
                                          :errored 0 :timed-out 0 :not-run 0)
                            :message (concatenate
                                      'string
                                      "0 properties selected -- this is "
                                      "NOT a successful verification.")
                            :environment *environment*)))
           (text (first-text response)))
      (ok (string= "no-properties" (gethash "status" response)))
      (ok (eq yason:false (gethash "verified" response)))
      (ok (search "NO PROPERTIES" text))
      (ok (search "NOT a successful verification" text)))))

(deftest check-response-failure-shows-both-counterexamples
  (testing "original and shrunk arguments both reach the text"
    (let* ((response (build-spec-check-response
                      (list :status :completed
                            :verified nil
                            :selection (list :mode "explicit"
                                             :selected (list (%symbol-data
                                                              "PROBE" "ADD-IS-WRONG"))
                                             :count 1
                                             :source "explicit property argument"
                                             :coverage "Only the property named.")
                            :results
                            (list (list :property (%symbol-data "PROBE" "ADD-IS-WRONG")
                                        :status :failed
                                        :trials (list :executed 1 :budget 100
                                                      :budget-source "backend-default")
                                        :seed "3963993791726803706"
                                        :profile :normal
                                        :counterexample
                                        (list (list :variable (%symbol-data "PROBE" "A")
                                                    :value (list :printed "68"
                                                                 :printed-complete t
                                                                 :omitted-chars 0
                                                                 :type "integer"
                                                                 :object-id nil)))
                                        :shrunk-counterexample
                                        (list (list :variable (%symbol-data "PROBE" "A")
                                                    :value (list :printed "0"
                                                                 :printed-complete t
                                                                 :omitted-chars 0
                                                                 :type "integer"
                                                                 :object-id nil)))
                                        :counterexample-status :present
                                        :shrink-status :present
                                        :shrink-note "Backend-searched reduction."
                                        :definition-digest "a41f9c2b7d0e5518"
                                        :definition-digest-complete t
                                        :definition-match :not-checked))
                            :counts (list :selected 1 :passed 0 :failed 1
                                          :errored 0 :timed-out 0 :not-run 0)
                            :profile :normal :timeout-seconds 60
                            :thread-leaked nil :elapsed 0.02
                            :reproduction-faithful :not-checked
                            :environment *environment*)))
           (text (first-text response)))
      (ok (search "FAILED" text))
      (ok (search "A = 68" text))
      (ok (search "A = 0" text))
      (ok (search "3963993791726803706" text))
      (testing "and the replay call is spelled out"
        (ok (search "spec-check" text))
        (ok (search "expect_definition_digest" text)))
      (testing "seed stays a string in the payload"
        (let ((result (aref (gethash "results" response) 0)))
          (ok (stringp (gethash "seed" result))))))))

(deftest check-response-timeout-is-not-a-pass
  (testing "a timeout is named in the text and never counted as passing"
    (let* ((response (build-spec-check-response
                      (list :status :incomplete
                            :verified nil
                            :selection (list :mode "explicit" :count 1
                                             :selected (list (%symbol-data
                                                              "PROBE" "SLOW"))
                                             :source "explicit property argument"
                                             :coverage "Only the property named.")
                            :results
                            (list (list :property (%symbol-data "PROBE" "SLOW")
                                        :status :timeout
                                        :timeout-seconds 0.3
                                        :thread-leaked t
                                        :counterexample-status :unavailable
                                        :counterexample-unavailable-reason
                                        "the run did not reach a verdict within its deadline"
                                        :shrink-status :unavailable
                                        :trials (list :budget 100
                                                      :budget-source "backend-default")
                                        :message "could not be stopped ... pool-kill-worker ..."))
                            :counts (list :selected 1 :passed 0 :failed 0
                                          :errored 0 :timed-out 1 :not-run 0)
                            :thread-leaked t
                            :worker-reuse :unsafe
                            :worker-reuse-message
                            "still executing ... use pool-kill-worker ..."
                            :verification-gaps (list :timeout)
                            :environment *environment*)))
           (text (first-text response)))
      (ok (string= "incomplete" (gethash "status" response)))
      (ok (eq yason:false (gethash "verified" response)))
      (ok (search "TIMEOUT" (string-upcase text)))
      (ok (search "pool-kill-worker" text))
      (testing "and the headline separates it from a falsified property"
        (ok (search "NOT VERIFIED" text))
        (ok (not (search "FAILED" text)))))))

(deftest check-response-headline-says-when-a-contract-was-not-run
  (testing "a passing :about selection that left a contract unrun says so up front"
    ;; Without this the first line of a run over a broken function reads
    ;; "✓ VERIFIED": the properties do hold, and the note saying the contract
    ;; was never executed sits several lines below a reader who has already
    ;; stopped.
    (let* ((response (build-spec-check-response
                      (list :status :completed
                            :verified t
                            :selection (list :mode "about" :count 1
                                             :selected (list (%symbol-data "PROBE" "GOOD"))
                                             :contract-not-run (%symbol-data "PROBE" "CLAMP")
                                             :source "cl-spec:semantic-data -> :properties-about"
                                             :coverage "Direct (:about ...) registrations only.")
                            :results
                            (list (list :property (%symbol-data "PROBE" "GOOD")
                                        :status :passed
                                        :trials (list :executed 100 :budget 100
                                                      :budget-source "backend-default")
                                        :seed "111" :profile :normal
                                        :definition-match :not-checked))
                            :counts (list :selected 1 :passed 1 :failed 0
                                          :errored 0 :timed-out 0 :not-run 0)
                            :environment *environment*)))
           (text (first-text response))
           (headline (subseq text 0 (or (position #\Newline text) (length text)))))
      (ok (search "VERIFIED" headline))
      (ok (search "properties only" headline))
      (ok (search "NOT run" headline))
      (testing "and a consumer reading the payload gets the name, not prose"
        (ok (string= "PROBE::CLAMP"
                     (gethash "qualified"
                              (gethash "contract_not_run"
                                       (gethash "selection" response))))))))
  (testing "a selection with no contract behind it keeps the bare headline"
    (let* ((response (build-spec-check-response
                      (list :status :completed
                            :verified t
                            :selection (list :mode "about" :count 1
                                             :selected (list (%symbol-data "PROBE" "GOOD"))
                                             :source "cl-spec:semantic-data -> :properties-about"
                                             :coverage "Direct (:about ...) registrations only.")
                            :results
                            (list (list :property (%symbol-data "PROBE" "GOOD")
                                        :status :passed
                                        :trials (list :executed 100 :budget 100
                                                      :budget-source "backend-default")
                                        :seed "111" :profile :normal
                                        :definition-match :not-checked))
                            :counts (list :selected 1 :passed 1 :failed 0
                                          :errored 0 :timed-out 0 :not-run 0)
                            :environment *environment*)))
           (text (first-text response))
           (headline (subseq text 0 (or (position #\Newline text) (length text)))))
      (ok (search "VERIFIED" headline))
      (ok (not (search "properties only" headline))))))

(deftest check-response-replays-the-failure-not-the-first-run
  (testing "the replay line names the property that did not pass"
    (let* ((response (build-spec-check-response
                      (list :status :completed
                            :verified nil
                            :selection (list :mode "about" :count 2
                                             :selected (list (%symbol-data "PROBE" "GOOD")
                                                             (%symbol-data "PROBE" "BAD"))
                                             :source "cl-spec:semantic-data -> :properties-about"
                                             :coverage "Direct (:about ...) registrations only.")
                            :results
                            (list (list :property (%symbol-data "PROBE" "GOOD")
                                        :status :passed
                                        :trials (list :executed 100 :budget 100
                                                      :budget-source "backend-default")
                                        :seed "111" :profile :normal
                                        :definition-digest "aaaaaaaaaaaaaaaa"
                                        :definition-match :not-checked)
                                  (list :property (%symbol-data "PROBE" "BAD")
                                        :status :failed
                                        :trials (list :executed 3 :budget 100
                                                      :budget-source "backend-default")
                                        :seed "222" :profile :normal
                                        :definition-digest "bbbbbbbbbbbbbbbb"
                                        :definition-match :not-checked))
                            :counts (list :selected 2 :passed 1 :failed 1
                                          :errored 0 :timed-out 0 :not-run 0)
                            :environment *environment*)))
           (text (first-text response)))
      (ok (search "property=PROBE::BAD" text))
      (ok (search "seed=222" text))
      (testing "and not the one that already holds"
        (ok (not (search "seed=111" text)))))))

(deftest check-response-distinguishes-empty-from-unavailable
  (testing "a zero-argument failure and a timeout do not read the same"
    (let* ((response (build-spec-check-response
                      (list :status :incomplete
                            :verified nil
                            :selection (list :mode "about" :count 2
                                             :selected nil
                                             :source "cl-spec:semantic-data -> :properties-about"
                                             :coverage "Direct (:about ...) registrations only.")
                            :results
                            (list (list :property (%symbol-data "PROBE" "NO-ARGS")
                                        :status :failed
                                        :trials (list :executed 1 :budget 100)
                                        :seed "7" :profile :normal
                                        :counterexample nil
                                        :counterexample-status :present
                                        :shrunk-counterexample nil
                                        :shrink-status :present
                                        :definition-match :not-checked)
                                  (list :property (%symbol-data "PROBE" "SLOW")
                                        :status :timeout
                                        :trials (list :budget 100)
                                        :counterexample nil
                                        :counterexample-status :unavailable
                                        :counterexample-unavailable-reason
                                        "the run did not reach a verdict within its deadline"
                                        :shrunk-counterexample nil
                                        :shrink-status :unavailable
                                        :definition-match :not-checked))
                            :counts (list :selected 2 :passed 0 :failed 1
                                          :errored 0 :timed-out 1 :not-run 0)
                            :worker-reuse :unknown
                            :worker-reuse-message "state unknown; replace the worker"
                            :verification-gaps (list :timeout
                                                     :rejection-counts-unmeasured)
                            :environment *environment*)))
           (text (first-text response))
           (results (gethash "results" response)))
      (testing "the empty counterexample is reported as present, not missing"
        (ok (string= "present" (gethash "counterexample_status" (aref results 0))))
        (ok (search "generates no arguments" text)))
      (testing "the timeout says why there is nothing to show"
        (ok (string= "unavailable"
                     (gethash "counterexample_status" (aref results 1))))
        (ok (search "UNAVAILABLE" text)))
      (testing "the worker is not silently assumed reusable"
        (ok (string= "unknown" (gethash "worker_reuse" response)))
        (ok (search "worker_reuse: unknown" text)))
      (testing "and the gaps are named"
        (ok (search "rejection-counts-unmeasured" text))))))

(deftest check-response-carries-a-schema-version
  (testing "every response names the version of the shape it is in"
    (let ((response (build-spec-check-response
                     (list :status :no-properties :verified nil
                           :selection (list :mode "about" :count 0
                                            :source "s" :coverage "c")
                           :results nil
                           :counts (list :selected 0 :passed 0 :failed 0
                                         :errored 0 :timed-out 0 :not-run 0)
                           :message "0 properties selected"
                           :environment *environment*))))
      (ok (stringp (gethash "schema_version" response))))))

(deftest value-response-separates-complete-from-restorable
  (testing "complete text is not the same claim as readable-back text"
    (let* ((response (build-spec-check-response
                      (list :status :completed :verified nil
                            :selection (list :mode "explicit" :count 1
                                             :source "s" :coverage "c")
                            :results
                            (list (list :property (%symbol-data "PROBE" "P")
                                        :status :failed
                                        :trials (list :executed 1 :budget 100)
                                        :counterexample-status :present
                                        :counterexample
                                        (list (list :variable (%symbol-data "PROBE" "OBJ")
                                                    :value (list :printed "#<THING {1004}>"
                                                                 :printed-complete t
                                                                 :omitted-chars 0
                                                                 :restorable nil
                                                                 :print-level 12
                                                                 :print-length 200
                                                                 :type "thing"
                                                                 :object-id 7)))
                                        :shrink-status :none
                                        :definition-match :not-checked))
                            :counts (list :selected 1 :passed 0 :failed 1
                                          :errored 0 :timed-out 0 :not-run 0)
                            :environment *environment*)))
           (value (gethash "value"
                           (aref (gethash "counterexample"
                                          (aref (gethash "results" response) 0))
                                 0))))
      (ok (eq t (gethash "printed_complete" value)))
      (ok (eq yason:false (gethash "restorable" value)))
      (ok (= 12 (gethash "print_level" value)))
      (ok (= 7 (gethash "object_id" value))))))

(deftest check-response-renders-every-status-in-the-tally
  (testing "a status with no field of its own still reaches the summary line"
    (let* ((response (build-spec-check-response
                      (list :status :incomplete :verified nil
                            :selection (list :mode "explicit" :count 1
                                             :source "s" :coverage "c")
                            :results
                            (list (list :property (%symbol-data "PROBE" "P")
                                        :status :generator-error
                                        :trials (list :budget 100)
                                        :counterexample-status :unavailable
                                        :shrink-status :unavailable
                                        :definition-match :not-checked))
                            :counts (list :selected 1 :passed 0 :failed 0
                                          :errored 0 :timed-out 0 :not-run 0
                                          :other 1
                                          :by-status '((:generator-error . 1)))
                            :environment *environment*)))
           (text (first-text response))
           (counts (gethash "counts" response)))
      (ok (= 1 (gethash "other" counts)))
      (ok (= 1 (gethash "generator-error" (gethash "by_status" counts))))
      (testing "and the text does not say zero of everything"
        (ok (search "1 generator-error" text))
        (ok (not (search "0 errored" text)))))))

(deftest check-response-not-checked-is-not-unfaithful
  (testing "an absent reproduction verdict is not-checked"
    (let ((response (build-spec-check-response
                     (list :status :completed :verified t
                           :selection (list :mode "explicit" :count 1
                                            :source "s" :coverage "c")
                           :results
                           (list (list :property (%symbol-data "PROBE" "P")
                                       :status :passed
                                       :trials (list :executed 100 :budget 100)
                                       :counterexample-status :not-applicable
                                       :shrink-status :not-applicable
                                       :definition-match :not-checked))
                           :counts (list :selected 1 :passed 1 :failed 0
                                         :errored 0 :timed-out 0 :not-run 0
                                         :other 0 :by-status '((:passed . 1)))
                           :environment *environment*))))
      (ok (string= "not-checked" (gethash "reproduction_faithful" response)))))
  (testing "an unreadable digest is unknown, and a real disagreement unfaithful"
    (flet ((faithful (value)
             (gethash "reproduction_faithful"
                      (build-spec-check-response
                       (list :status :completed :verified nil
                             :selection (list :mode "explicit" :count 1
                                              :source "s" :coverage "c")
                             :results nil
                             :counts (list :selected 0 :passed 0 :failed 0
                                           :errored 0 :timed-out 0 :not-run 0
                                           :other 0 :by-status nil)
                             :reproduction-faithful value
                             :environment *environment*)))))
      (ok (string= "unknown" (faithful :unknown)))
      (ok (string= "unfaithful" (faithful :false)))
      (ok (string= "faithful" (faithful :true))))))

(deftest value-response-carries-no-truncation-note
  (testing "the printed value is the value, not the sink's commentary"
    (let* ((response (build-spec-check-response
                      (list :status :completed :verified nil
                            :selection (list :mode "explicit" :count 1
                                             :source "s" :coverage "c")
                            :results
                            (list (list :property (%symbol-data "PROBE" "P")
                                        :status :failed
                                        :trials (list :executed 1 :budget 100)
                                        :counterexample-status :present
                                        :counterexample
                                        (list (list :variable (%symbol-data "PROBE" "V")
                                                    :value (list :printed "(1 2 3"
                                                                 :printed-complete nil
                                                                 :omitted-chars 900
                                                                 :restorable nil
                                                                 :print-level 12
                                                                 :print-length 200
                                                                 :type "cons"
                                                                 :object-id 3)))
                                        :shrink-status :none
                                        :definition-match :not-checked))
                            :counts (list :selected 1 :passed 0 :failed 1
                                          :errored 0 :timed-out 0 :not-run 0
                                          :other 0 :by-status '((:failed . 1)))
                            :environment *environment*)))
           (text (first-text response)))
      (ok (search "V = (1 2 3" text))
      (testing "the whole value sits on the counterexample line"
        (let* ((start (search "counterexample:" text))
               (end (or (position #\Newline text :start start) (length text)))
               (line (subseq text start end)))
          (ok (search "V = (1 2 3" line)))))))

(deftest describe-response-renders-the-spec-tree
  (testing "kind=spec shows the normalized IR tree the description promises"
    ;; The tree was reaching the payload and not the text, which for a client
    ;; that renders only content[].text is the same as not existing.
    (let* ((response (build-spec-describe-response
                      (list :status :ok :kind "spec"
                            :name (%symbol-data "PROBE" "SMALL-INT")
                            :spec (list :kind :and
                                        :name (%symbol-data "PROBE" "SMALL-INT")
                                        :children
                                        (list (list :kind :type :type "INTEGER")
                                              (list :kind :range :min "0"
                                                    :max "100")))
                            :source-form "(AND INTEGER (RANGE 0 100))"
                            :source-form-complete t
                            :source-form-omitted-chars 0
                            :environment *environment*)))
           (text (first-text response)))
      (ok (search "normalized IR tree" text))
      (ok (search "and" text))
      (ok (search "INTEGER" text))
      (ok (search "[0, 100]" text)))))

(deftest describe-response-shows-tags-trials-and-shrinking
  (testing "the facts the profile error message points at are actually shown"
    ;; %RESOLVE-PROFILE tells a caller to look at the property's trials table
    ;; "see spec-describe", so spec-describe has to show it rather than leave
    ;; it to whether the raw source form survived max_chars.
    (let* ((response (build-spec-describe-response
                      (list :status :ok :kind "property"
                            :name (%symbol-data "PROBE" "P")
                            :property-kind :invariant
                            :tags (list :bounds :demo)
                            :trials-table "(:NORMAL 200 :SMOKE 10)"
                            :shrink-enabled nil
                            :targets nil :arguments nil
                            :body "((= 1 1))" :body-complete t
                            :body-omitted-chars 0
                            :environment *environment*)))
           (text (first-text response)))
      (ok (search "tags: BOUNDS, DEMO" text))
      (ok (search ":NORMAL 200 :SMOKE 10" text))
      (testing "and shrinking says which of the two it is"
        (ok (search "shrinking: disabled" text))))))

(deftest describe-response-does-not-claim-a-file-that-is-nil
  (testing "a REPL definition is not reported as defined in NIL"
    (let* ((response (build-spec-describe-response
                      (list :status :ok :kind "property"
                            :name (%symbol-data "PROBE" "P")
                            :property-kind :invariant
                            :targets nil :arguments nil
                            :body "((= 1 1))" :body-complete t
                            :body-omitted-chars 0
                            :source-location (list :file nil :package "PROBE")
                            :environment *environment*)))
           (text (first-text response)))
      (ok (not (search "defined in NIL" text)))
      (ok (search "defined at a REPL, in package PROBE" text)))))

(deftest describe-response-marks-truncation
  (testing "a cut body says so in the text"
    (let* ((response (build-spec-describe-response
                      (list :status :ok :kind "property"
                            :name (%symbol-data "PROBE" "ADD-COMMUTES")
                            :property-kind :commutativity
                            :tags nil :targets nil :documentation nil
                            :arguments nil
                            :body "((= (ADD" :body-complete nil
                            :body-omitted-chars 31
                            :source-form "(DEFPROPERTY" :source-form-complete nil
                            :source-form-omitted-chars 40
                            :definition-digest "a41f9c2b7d0e5518"
                            :environment *environment*)))
           (text (first-text response)))
      (ok (eq yason:false (gethash "body_complete" response)))
      (ok (search "truncated" text))
      (ok (search "31" text)))))

(deftest describe-response-marks-a-cut-precondition
  (testing "a truncated :pre or :post says so, like the body does"
    ;; A silently cut :PRE is worse than a cut body: a reader takes the clause
    ;; for the whole condition and concludes the contract admits inputs it
    ;; refuses.
    (let* ((response (build-spec-describe-response
                      (list :status :ok :kind "function-spec"
                            :name (%symbol-data "PROBE" "TRANSFER")
                            :documentation nil :arguments nil :returns nil
                            :preconditions "(AND (PLUSP AMOUNT)"
                            :preconditions-complete nil
                            :preconditions-omitted-chars 62
                            :postconditions "(> RESULT"
                            :postconditions-complete nil
                            :postconditions-omitted-chars 17
                            :source-form "(DEFSPEC-FUNCTION" :source-form-complete t
                            :definition-digest "a41f9c2b7d0e5518"
                            :environment *environment*)))
           (text (first-text response)))
      (ok (eq yason:false (gethash "preconditions_complete" response)))
      (ok (= 62 (gethash "preconditions_omitted_chars" response)))
      (ok (= 17 (gethash "postconditions_omitted_chars" response)))
      (ok (search "62 more characters" text))
      (ok (search "17 more characters" text)))))

(deftest describe-response-renders-an-argument-spec-in-full
  (testing "an argument's own bounds, values and class reach the text"
    ;; The arguments block used to print the node's kind and recurse into its
    ;; children, so everything that says what the input actually admits was
    ;; dropped -- the half spec-describe kind=function-spec exists for.
    (let* ((response (build-spec-describe-response
                      (list :status :ok :kind "function-spec"
                            :name (%symbol-data "PROBE" "BUCKET")
                            :documentation nil
                            :arguments
                            (list (list :variable (%symbol-data "PROBE" "V")
                                        :spec (list :kind :range :min "0"
                                                    :max "100"
                                                    :base-type "INTEGER"))
                                  (list :variable (%symbol-data "PROBE" "LO")
                                        :spec (list :kind :member
                                                    :values "(1 2 3)"))
                                  (list :variable (%symbol-data "PROBE" "ACC")
                                        :spec
                                        (list :kind :class
                                              :class-name
                                              (%symbol-data "PROBE" "ACCOUNT"))))
                            :returns nil
                            :source-form "(DEFSPEC-FUNCTION" :source-form-complete t
                            :environment *environment*)))
           (text (first-text response)))
      (ok (search "V : range [0, 100]  base: INTEGER" text))
      (ok (search "LO : member  values: (1 2 3)" text))
      (testing "and a class prints its name, not SYMBOL-DATA's plist"
        (ok (search "ACC : class PROBE::ACCOUNT" text))
        (ok (not (search "QUALIFIED" text))))))
  (testing "a contract with no :pre claims nothing about its completeness"
    (let ((response (build-spec-describe-response
                     (list :status :ok :kind "function-spec"
                           :name (%symbol-data "PROBE" "WIDEN")
                           :documentation nil :arguments nil :returns nil
                           :preconditions nil
                           :preconditions-complete :not-applicable
                           :source-form "(DEFSPEC-FUNCTION"
                           :source-form-complete t
                           :environment *environment*))))
      (ok (null (gethash "preconditions" response)))
      (ok (null (gethash "preconditions_complete" response))))))

(deftest list-response-omits-a-kind-that-was-not-requested
  (testing "the header names only what was counted"
    (flet ((text-for (kind specs properties)
             (first-text
              (build-spec-list-response
               (list :status :ok :kind kind
                     :specs (when specs (list (%symbol-data "PROBE" "S")))
                     :properties nil
                     :counts (list :specs specs :properties properties)
                     :truncated nil :limit 200
                     :filters (list :tag-resolved :not-requested)
                     :coverage "coverage note"
                     :environment *environment*)))))
      (let ((properties-only (text-for "properties" nil 3)))
        (ok (search "3 properties" properties-only))
        (ok (not (search "spec" (subseq properties-only 0
                                        (position #\Newline properties-only))))))
      (let ((specs-only (text-for "specs" 2 nil)))
        (ok (search "2 specs" specs-only))
        (ok (not (search "propert" (subseq specs-only 0
                                           (position #\Newline specs-only))))))
      (testing "while a requested kind that matched nothing still says zero"
        (let ((both (text-for "both" 0 0)))
          (ok (search "0 specs" both))
          (ok (search "0 properties" both))))))
  (testing "the payload leaves an uncounted kind null rather than zero"
    (let* ((response (build-spec-list-response
                      (list :status :ok :kind "properties"
                            :specs nil :properties nil
                            :counts (list :specs nil :properties 0)
                            :truncated nil :limit 200
                            :filters (list :tag-resolved :not-requested)
                            :coverage "coverage note"
                            :environment *environment*)))
           (counts (gethash "counts" response)))
      (ok (null (gethash "specs" counts)))
      (ok (eql 0 (gethash "properties" counts))))))

(deftest list-response-says-whether-the-tag-narrowed-anything
  (flet ((header (kind &key (tag-filterable t))
           (first-text
            (build-spec-list-response
             (list :status :ok :kind kind
                   :specs (list (%symbol-data "PROBE" "SMALL-INT"))
                   :properties nil :function-specs nil
                   :specs-listable t :properties-listable t
                   :function-specs-listable t
                   :tag-filterable tag-filterable
                   :counts (list :specs 1)
                   ;; TAG-APPLIED as LIST-REPORT computes it: the kind lists
                   ;; properties, this cl-spec can enumerate them, and it can
                   ;; filter by tag.
                   :filters (list :tag "critical" :tag-resolved t
                                  :tag-applied
                                  (and (member kind '("properties" "both")
                                               :test #'equal)
                                       tag-filterable
                                       t))
                   :coverage "everything registered here"
                   :environment *environment*)))))
    (testing "a kind that lists no properties says the tag was not applied"
      ;; "1 spec  tagged critical" asserts a filter that narrowed nothing:
      ;; LIST-REPORT never offers the tag to the spec or contract listings.
      (let ((text (header "specs")))
        (ok (search "was NOT applied" text))
        (ok (not (search "tagged critical" text)))))
    (testing "a properties listing says it plainly"
      (let ((text (header "properties")))
        (ok (search "tagged critical" text))
        (ok (not (search "was NOT applied" text)))
        (ok (not (search "(properties only)" text)))))
    (testing "and a mixed listing says which half it narrowed"
      (let ((text (header "both")))
        (ok (search "tagged critical (properties only)" text))))
    (testing "while a cl-spec that cannot filter by tag says that instead"
      ;; Keyed on properties_listable, this printed "tagged critical" over a
      ;; listing the tag never touched -- read as "no property carries it".
      (let ((text (header "properties" :tag-filterable nil)))
        (ok (search "was NOT applied" text))
        (ok (search "properties-with-tag" text))
        (ok (not (search "tagged critical" text)))))))

(deftest check-response-unsupported-reaches-the-text
  (testing "a contract cl-spec cannot run says so where a client can see it"
    ;; The report carries a message and no selection, results or counts.  Read
    ;; as a report of a run it renders "Selected NIL properties via NIL" and
    ;; the one thing the caller needs -- why nothing could be executed --
    ;; never reaches content[].text.
    (let* ((response (build-spec-check-response
                      (list :status :unsupported
                            :verified nil
                            :message
                            (concatenate 'string
                                         "the cl-spec loaded here does not "
                                         "export check-function, so a "
                                         "contract cannot be executed.")
                            :environment *environment*)))
           (text (first-text response)))
      (ok (string= "unsupported" (gethash "status" response)))
      (ok (eq yason:false (gethash "verified" response)))
      (ok (search "check-function" text))
      (ok (not (search "Selected" text))))))

(deftest check-response-does-not-invent-non-determinism
  (testing "an unreadable failure reason is not a finding about the function"
    ;; NIL reaches the renderer from two opposite places: cl-spec saying the
    ;; counterexample would not reproduce, and this adapter never having had a
    ;; reader to ask.  Printing the first for both accuses the caller's code.
    (let ((text (first-text
                 (build-spec-check-response
                  (%contract-check-report :failure-reason nil
                                          :failure-reason-readable nil)))))
      (ok (search "could not be read" text))
      (ok (not (search "not deterministic" text)))))
  (testing "but cl-spec's own silence still is one"
    (let ((text (first-text
                 (build-spec-check-response
                  (%contract-check-report :failure-reason nil
                                          :failure-reason-readable t)))))
      (ok (search "not deterministic" text))))
  (testing "and a reason that was read is printed as itself"
    (let* ((response (build-spec-check-response
                      (%contract-check-report :failure-reason :return-spec
                                              :failure-reason-readable t)))
           (contract (gethash "contract" (aref (gethash "results" response) 0))))
      (ok (search "broken half: return-spec" (first-text response)))
      (ok (string= "return-spec" (gethash "failure_reason" contract)))
      (ok (eq t (gethash "failure_reason_readable" contract))))))

(deftest check-response-never-reports-a-negative-call-count
  (testing "more refusals than trials is said, not subtracted"
    ;; cl-spec stops counting refusals at the first failure it recognizes, but
    ;; a target that SIGNALS unwinds past that point with the counter running
    ;; and shrinking keeps feeding it.  Measured at 3 runs in 8 against such a
    ;; contract -- one of them 1 trial and 2 rejections, which printed as "the
    ;; function was called -1 times".
    (let* ((response (build-spec-check-response
                      (%contract-check-report :rejected 2
                                              :effective-trials nil
                                              :rejection-status :overcounted
                                              :rejected-overcounted t
                                              :failure-reason :condition
                                              :failure-reason-readable t)))
           (contract (gethash "contract" (aref (gethash "results" response) 0)))
           (text (first-text response)))
      (ok (eq t (gethash "rejected_overcounted" contract)))
      (ok (search "more refusals than trials" text))
      (ok (not (search "-1" text)))
      (ok (not (search "called 0 time" text)))
      (testing "and the JSON withholds the figure rather than saying zero"
        ;; 0 is itself the claim the text refuses to make.
        (ok (null (gethash "effective_trials" contract))))
      (testing "while the line calls a contract a contract"
        (ok (search "Selected 1 contract" text))
        (ok (not (search "Selected 1 property" text))))
      (testing "and prints no profile, which a contract run does not have"
        (ok (not (search "profile:" text))))))
  (testing "and an ordinary count still reads as one"
    (let ((text (first-text
                 (build-spec-check-response
                  (%contract-check-report :failure-reason :return-spec
                                          :failure-reason-readable t)))))
      (ok (search "called 27 times" text))
      (ok (not (search "more refusals than trials" text))))))

(deftest check-response-does-not-invent-a-precondition
  (testing "a contract with no :pre is not described as refusing inputs"
    ;; "0 of them refused by :pre" tells the reader a precondition exists.
    ;; On a contract written with :args, :returns and :post and no :pre, that
    ;; is a clause the author never wrote -- and the tool then advises raising
    ;; trials on the strength of a number that cannot mean anything.
    (let* ((response (build-spec-check-response
                      (%contract-check-report :precondition-p nil
                                              :rejection-status :no-precondition
                                              :rejected 0
                                              :effective-trials 30
                                              :failure-reason :postcondition
                                              :failure-reason-readable t)))
           (contract (gethash "contract" (aref (gethash "results" response) 0)))
           (text (first-text response)))
      (ok (eq yason:false (gethash "has_precondition" contract)))
      (ok (search "no :pre" text))
      (ok (not (search "refused by :pre" text)))))
  (testing "while one that has a :pre still reports its refusals"
    (let ((text (first-text
                 (build-spec-check-response
                  (%contract-check-report :failure-reason :return-spec
                                          :failure-reason-readable t)))))
      (ok (search "refused by :pre" text))
      (ok (not (search "no :pre" text))))))

(defun %digest-check-report (faithful &key properties-not-run (match faithful))
  "Return a passing contract report whose reproduction verdict is FAITHFUL.

PROPERTIES-NOT-RUN adds a coverage gap, so a case can ask whether the two
qualifiers displace one another.  MATCH is the result's own verdict, which is
FAITHFUL except where the two differ: a call answers :UNKNOWN both for a digest
that could not be read and for a run that never reached a comparison, and only
the result says which."
  (list :status :completed
        :verified t
        :selection (list* :mode "contract" :kind :contract :count 1
                          :selected (list (%symbol-data "PROBE" "WIDEN"))
                          :source "explicit function argument"
                          :coverage "Only the contract named."
                          :properties-not-run-read t
                          (when properties-not-run
                            (list :properties-not-run
                                  (list (%symbol-data "PROBE" "WIDEN-IS-WIDE")))))
        :results
        (list (list :property (%symbol-data "PROBE" "WIDEN")
                    :kind :contract
                    :status :passed
                    :trials (list :executed 30 :budget 30
                                  :budget-source "requested")
                    :seed "7"
                    :counterexample-status :not-applicable
                    :shrink-status :not-applicable
                    :definition-match match))
        :counts (list :selected 1 :passed 1 :failed 0
                      :errored 0 :timed-out 0 :not-run 0
                      :other 0 :by-status '((:passed . 1)))
        :reproduction-faithful faithful
        :environment *environment*))

(defun %headline (report)
  "Return the first line of the text REPORT renders to."
  (let ((text (first-text (build-spec-check-response report))))
    (subseq text 0 (or (position #\Newline text) (length text)))))

(deftest check-response-headline-says-when-the-definitions-moved
  (testing "a pass that did not reproduce the named run says so up front"
    ;; EXPECT_DEFINITION_DIGEST is an assertion by the caller: this is still
    ;; the contract I saved.  When it turns out false the run keeps its own
    ;; verdict -- but a bare "✓ VERIFIED" on the first line is read as
    ;; confirmation of the saved one, and the reproduction verdict sits
    ;; several lines under a reader who has already stopped.  The same
    ;; argument the coverage qualifiers are here for.
    (let ((headline (%headline (%digest-check-report :false))))
      (ok (search "VERIFIED" headline))
      (ok (search "definitions moved" headline))))
  (testing "a digest that could not be read is not taken for a match"
    (let ((headline (%headline (%digest-check-report :unknown))))
      (ok (search "VERIFIED" headline))
      (ok (search "could not be read" headline))))
  (testing "and a run that never compared one is not called a disagreement"
    ;; :UNKNOWN covers two different shortfalls.  A digest that could not be
    ;; read was looked at; a timeout, an exhausted budget or a signalling run
    ;; never got that far, and "could not be read" sends the reader to a
    ;; digest that was never the problem.
    (let ((headline (%headline (%digest-check-report :unknown
                                                     :match :not-checked))))
      (ok (search "VERIFIED" headline))
      (ok (search "did not get far enough" headline))
      (ok (not (search "could not be read" headline)))))
  (testing "a match, and a run with no digest to check, keep the bare headline"
    (dolist (value '(:true :not-checked))
      (let ((headline (%headline (%digest-check-report value))))
        (ok (search "VERIFIED" headline))
        (ok (not (search "definitions" headline))))))
  (testing "and it does not displace the coverage qualifier"
    ;; Two different gaps.  One says what was covered, the other says which
    ;; revision covered it, and dropping either to make room for the other
    ;; leaves the headline making a claim the run did not support.
    (let ((headline (%headline (%digest-check-report :false
                                                     :properties-not-run t))))
      (ok (search "contract only" headline))
      (ok (search "definitions moved" headline)))))

(deftest check-response-says-when-the-contract-itself-signalled
  (testing "contract-error is a finding about the contract, not the function"
    ;; CONDITION, POSTCONDITION and RETURN-SPEC all name a half of what was
    ;; claimed about the function.  CONTRACT-ERROR names the contract's own
    ;; code signalling, which is a different accusation -- and printed in the
    ;; same column as the others, the bare word reads as one more way the
    ;; function broke.
    (let ((text (first-text (build-spec-check-response
                             (%contract-check-report
                              :failure-reason :contract-error
                              :failure-reason-readable t)))))
      (ok (search "broken half: contract-error" text))
      (ok (search "contract's own code" text))
      (ok (search "NOT about the function" text))))
  (testing "and a half that really is about the function gets no such gloss"
    (let ((text (first-text (build-spec-check-response
                             (%contract-check-report
                              :failure-reason :postcondition
                              :failure-reason-readable t)))))
      (ok (search "broken half: postcondition" text))
      (ok (not (search "contract's own code" text))))))
