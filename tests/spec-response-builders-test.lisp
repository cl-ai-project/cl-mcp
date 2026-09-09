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
                #:build-spec-symbol-response
                #:build-spec-describe-response
                #:build-spec-check-response))

(in-package #:cl-mcp/tests/spec-response-builders-test)

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
                            :notes (list "properties_about lists direct (:about ...) registrations only")
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
                            :message "0 properties selected -- this is NOT a successful verification."
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
                                        :shrink-note "Backend-searched reduction."
                                        :definition-digest "a41f9c2b7d0e5518"
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
                                        :trials (list :budget 100
                                                      :budget-source "backend-default")
                                        :message "could not be stopped ... pool-kill-worker ..."))
                            :counts (list :selected 1 :passed 0 :failed 0
                                          :errored 0 :timed-out 1 :not-run 0)
                            :thread-leaked t
                            :environment *environment*)))
           (text (first-text response)))
      (ok (string= "incomplete" (gethash "status" response)))
      (ok (eq yason:false (gethash "verified" response)))
      (ok (search "TIMEOUT" (string-upcase text)))
      (ok (search "pool-kill-worker" text))
      (testing "and the headline separates it from a falsified property"
        (ok (search "NOT VERIFIED" text))
        (ok (not (search "FAILED" text)))))))

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
