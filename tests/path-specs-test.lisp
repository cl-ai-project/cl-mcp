;;;; tests/path-specs-test.lisp
;;;;
;;;; The read fixtures behind the cl-mcp/specs read-path properties, and the
;;;; read policy's key cases on fixed descriptors.  Needs no cl-spec: it runs in
;;;; the default suite, and scripts/check-specs.lisp's self-test runs it too.
;;;;
;;;; Every case builds its own scratch tree under the temporary directory,
;;;; binds *PROJECT-ROOT* for its body only, and registers at most one ASDF
;;;; system of its own, which it removes again.

(defpackage #:cl-mcp/tests/path-specs-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:allowed-read-path)
  (:import-from #:cl-mcp/specs/path-fixtures
                #:read-fixture-scratch
                #:read-fixture-system-name
                #:read-fixture-registered
                #:read-fixture-root
                #:with-read-fixture
                #:region-native
                #:place-native
                #:fixture-symlink
                #:fixture-asd-native
                #:register-dependency
                #:unregister-dependency
                #:expected-read-decision
                #:both-allow-as-p
                #:both-deny-p
                #:environment-problems
                #:read-fixture-environment-error
                #:read-fixture-cleanup-error
                #:read-fixture-cleanup-error-failures
                #:read-fixture-cleanup-warning
                #:read-fixture-cleanup-warning-failures))

(in-package #:cl-mcp/tests/path-specs-test)

;;; ------------------------------------------------------------------------
;;; Helpers

(defun place (region dirs &optional name type)
  "A place descriptor, for fixed cases."
  (list :region region :dirs dirs :name name :type type))

(defun link (region kind name target)
  "A link descriptor, for fixed cases."
  (list :region region :kind kind :name name :target target))

(defun native-directory-exists-p (native)
  "True when the directory NATIVE exists, checked without the pathname reader."
  (and (probe-file (uiop:parse-native-namestring native :ensure-directory t)) t))

(defun asdf-snapshot ()
  "Every registered ASDF system name with its system object."
  (mapcar (lambda (name) (cons name (asdf:registered-system name)))
          (asdf:registered-systems)))

(defun asdf-snapshot-kept-p (snapshot)
  "True when exactly SNAPSHOT's systems are registered, each as the same object."
  (and (= (length snapshot) (length (asdf:registered-systems)))
       (every (lambda (entry) (eq (cdr entry) (asdf:registered-system (car entry))))
              snapshot)))

(define-condition planned-failure (error) ()
  (:documentation "Signalled on purpose from a fixture body."))

;;; ------------------------------------------------------------------------
;;; The fixtures themselves

(deftest read-fixture-builds-its-tree-and-removes-it
  (let ((root-before *project-root*)
        (snapshot (asdf-snapshot))
        (scratch nil)
        (system nil)
        (target (place :project '("x[1]" "日本語") "a b" "txt")))
    (with-read-fixture (fixture :places (list target (place :outside '("src") "o"))
                                :links (list (link :project :directory "link-a"
                                                   (place :outside '("src") "o")))
                                :root-alias t
                                :register-dependency t)
      (setf scratch (read-fixture-scratch fixture)
            system (read-fixture-system-name fixture))
      (testing "the regions, places and links exist"
        (dolist (region '(:project :dependency :outside :project-other))
          (ok (native-directory-exists-p (region-native fixture region))))
        (ok (probe-file (uiop:parse-native-namestring (place-native fixture target)))))
      (testing "the project root is the alias, for the body only"
        (ok (search "project-alias" (uiop:native-namestring *project-root*)))
        (ok (equal *project-root* (read-fixture-root fixture))))
      (testing "the dependency system is registered with dependency/ as its directory"
        (ok (read-fixture-registered fixture))
        (ok (asdf:registered-system system))))
    (testing "afterwards the tree, the system and the binding are gone"
      (ng (native-directory-exists-p scratch))
      (ng (asdf:registered-system system))
      (ok (eq root-before *project-root*))
      (let ((kept (asdf-snapshot-kept-p snapshot)))
        (ok kept "every other system is still registered as itself")))))

(deftest read-fixture-cleans-up-when-its-body-signals
  (let ((root-before *project-root*)
        (snapshot (asdf-snapshot))
        (scratch nil)
        (system nil))
    (ok (handler-case
            (with-read-fixture (fixture :places (list (place :dependency '("src") "d" "lisp"))
                                        :register-dependency t)
              (setf scratch (read-fixture-scratch fixture)
                    system (read-fixture-system-name fixture))
              (error 'planned-failure))
          (planned-failure () t))
        "the body's own condition comes out")
    (ng (native-directory-exists-p scratch))
    (ng (asdf:registered-system system))
    (ok (eq root-before *project-root*))
    (let ((kept (asdf-snapshot-kept-p snapshot)))
      (ok kept "every other system is still registered as itself"))))

(deftest read-fixture-removes-a-registration-that-failed-halfway
  ;; The fixture's own .asd defines the system and then fails, so ASDF has
  ;; registered it by the time REGISTER-DEPENDENCY unwinds.  The cleanup must
  ;; still remove it: a registration left behind would point a later check in
  ;; the same image at a directory that no longer exists.
  (let ((system nil)
        (scratch nil)
        (snapshot (asdf-snapshot)))
    (unwind-protect
         (progn
           (ok (handler-case
                   (with-read-fixture (fixture)
                     (setf system (read-fixture-system-name fixture)
                           scratch (read-fixture-scratch fixture))
                     (with-open-file (out (uiop:parse-native-namestring
                                           (fixture-asd-native fixture))
                                          :direction :output :if-exists :supersede)
                       (format out "(asdf:defsystem ~S)~%~
                                    (error \"planned failure after registration\")~%"
                               system))
                     (register-dependency fixture)
                     :registered)
                 (error (condition)
                   (and (search "planned failure after registration"
                                (princ-to-string condition))
                        t)))
               "the planned failure inside the .asd comes out")
           (let ((left (and (asdf:registered-system system) t))
                 (kept (asdf-snapshot-kept-p snapshot)))
             (ng left "the half-made registration is gone")
             (ng (native-directory-exists-p scratch))
             (ok kept "every other system is still registered as itself")))
      ;; Only after the checks: keep a failure here from leaking into the
      ;; tests that follow in this image.
      (when (and system (asdf:registered-system system))
        (asdf:clear-system system)))))

(deftest read-fixture-cleanup-leaves-link-targets-alone
  (let* ((outside (format nil "~Acl-mcp-read-spec-owned-~D-~D/"
                          (uiop:native-namestring (uiop:temporary-directory))
                          (sb-posix:getpid) (get-universal-time)))
         (file (concatenate 'string outside "keep.txt")))
    (sb-posix:mkdir outside #o700)
    (unwind-protect
         (progn
           (with-open-file (out (uiop:parse-native-namestring file) :direction :output)
             (write-string "keep" out))
           (with-read-fixture (fixture)
             (fixture-symlink fixture
                              (concatenate 'string (region-native fixture :project) "link-away")
                              (string-right-trim "/" outside)))
           (ok (probe-file (uiop:parse-native-namestring file))
               "cleanup unlinked the link and left the directory it pointed to"))
      (sb-posix:unlink file)
      (sb-posix:rmdir outside))))

(deftest read-fixture-reports-a-failed-cleanup
  (let* ((stray nil)
         (failure (handler-case
                      (with-read-fixture (fixture)
                        (setf stray (concatenate 'string (region-native fixture :project)
                                                 "stray.txt"))
                        (with-open-file (out (uiop:parse-native-namestring stray)
                                             :direction :output)
                          (write-string "not recorded" out))
                        :body-returned)
                    (read-fixture-cleanup-error (condition) condition))))
    (ok (typep failure 'read-fixture-cleanup-error)
        "an object cleanup cannot remove fails the run instead of passing it")
    (ok (find :directory (read-fixture-cleanup-error-failures failure) :key #'first))
    ;; Remove what the failed cleanup had to leave: the stray file, then the two
    ;; directories it kept from being removed, one at a time.
    (let ((project (subseq stray 0 (- (length stray) (length "stray.txt")))))
      (sb-posix:unlink stray)
      (sb-posix:rmdir project)
      (sb-posix:rmdir (subseq project 0 (- (length project) (length "project/")))))))

(deftest read-fixture-warns-when-cleanup-fails-during-an-error
  ;; The body's own condition must come out, and the cleanup failure must not
  ;; be lost: it arrives as a structured warning, which a caller can keep apart
  ;; from other output.
  (let ((stray nil)
        (warnings '()))
    (ok (handler-case
            (handler-bind ((read-fixture-cleanup-warning
                             (lambda (warning)
                               (push warning warnings)
                               (muffle-warning warning))))
              (with-read-fixture (fixture)
                (setf stray (concatenate 'string (region-native fixture :project)
                                         "stray.txt"))
                (with-open-file (out (uiop:parse-native-namestring stray)
                                     :direction :output)
                  (write-string "not recorded" out))
                (error 'planned-failure)))
          (planned-failure () t))
        "the body's failure is the one that comes out")
    (ok (= 1 (length warnings)) "and the cleanup failure is reported once, as a warning")
    (let ((project (subseq stray 0 (- (length stray) (length "stray.txt")))))
      (ok (find project (read-fixture-cleanup-warning-failures (first warnings))
                :key #'second :test #'equal)
          "naming the directory it could not remove")
      (sb-posix:unlink stray)
      (sb-posix:rmdir project)
      (sb-posix:rmdir (subseq project 0 (- (length project) (length "project/")))))))

(deftest read-fixture-environment-check
  (let ((parent #p"/tmp/"))
    (testing "a registered source directory containing the scratch parent is refused"
      (ok (equal '("wide") (environment-problems parent '(("wide" . #p"/")
                                                          ("elsewhere" . #p"/opt/x/"))))))
    (testing "directories beside or below it are fine"
      (ok (null (environment-problems parent '(("below" . #p"/tmp/lib/")
                                              ("beside" . #p"/tmpx/"))))))
    (testing "the live environment passes the check, so fixtures can run here"
      (ok (with-read-fixture (fixture) (and fixture t))))
    (testing "the refusal is a condition of its own, not a verdict on the policy"
      (ok (subtypep 'read-fixture-environment-error 'error)))))

;;; ------------------------------------------------------------------------
;;; The policy on descriptors

(deftest expected-read-decision-is-the-policy-table
  (dolist (row '((:project nil :allowed) (:project t :allowed)
                 (:dependency nil :denied) (:dependency t :allowed)
                 (:outside nil :denied) (:outside t :denied)
                 (:project-other nil :denied) (:project-other t :denied)))
    (destructuring-bind (region registered expected) row
      (ok (eq expected (expected-read-decision (place region '() "f") registered))
          (format nil "~S registered=~S" region registered))))
  (testing "a link is judged by where it leads, not where it sits"
    (ok (eq :denied (expected-read-decision
                     (link :project :file "link-a" (place :outside '() "f")) t)))
    (ok (eq :allowed (expected-read-decision
                      (link :dependency :file "link-a" (place :project '() "f")) nil)))))

;;; ------------------------------------------------------------------------
;;; Fixed cases against the functions under test

(deftest project-files-are-read-as-themselves
  (let ((names (list (place :project '() "plain" "lisp")
                     (place :project '("a b") "日本語" "txt")
                     (place :project '("x[1]" "v[old] 2") "データ.d")
                     (place :project '("src"))
                     (place :project '()))))
    (with-read-fixture (fixture :places names)
      (dolist (target names)
        (ok (eq :allowed (expected-read-decision target nil)))
        (dolist (spelling '(:relative :absolute :pathname :detour))
          (ok (both-allow-as-p fixture target spelling)
              (format nil "~S ~S" spelling target)))))
    (testing "through a project root given as a symlink alias"
      (with-read-fixture (fixture :places names :root-alias t)
        (dolist (target names)
          (ok (both-allow-as-p fixture target :relative :must-exist nil)
              (format nil "alias ~S" target)))))))

(deftest dependency-reads-follow-registration
  (let ((target (place :dependency '("src") "d" "lisp"))
        (dependency-root (place :dependency '()))
        (project (place :project '() "p" "txt"))
        (outside (place :outside '() "o" "txt")))
    (with-read-fixture (fixture :places (list target project outside))
      (flet ((expect-state (registered)
               (dolist (thing (list target dependency-root project outside))
                 (dolist (spelling '(:relative :absolute))
                   (ok (if (eq :allowed (expected-read-decision thing registered))
                           (both-allow-as-p fixture thing spelling)
                           (both-deny-p fixture thing spelling))
                       (format nil "registered=~S ~S ~S" registered spelling thing))))))
        (testing "before registration" (expect-state nil))
        (register-dependency fixture)
        (testing "while registered" (expect-state t))
        (unregister-dependency fixture)
        (testing "after unregistration" (expect-state nil))))))

(deftest unlisted-regions-are-denied
  (let ((targets (list (place :outside '() "o" "txt")
                       (place :outside '("x[1]") "日本語")
                       (place :outside '())
                       (place :project-other '() "p" "lisp")
                       (place :project-other '("a b")))))
    (dolist (registered '(nil t))
      (with-read-fixture (fixture :places targets :register-dependency registered)
        (dolist (target targets)
          (dolist (spelling '(:relative :absolute :pathname :detour))
            (ok (both-deny-p fixture target spelling)
                (format nil "registered=~S ~S ~S" registered spelling target))))))
    (testing "the prefix sibling is denied through ../project-other/ as well"
      (with-read-fixture (fixture :places targets)
        (let ((sibling (place :project-other '() "p" "lisp")))
          (ok (null (allowed-read-path "../project-other/p.lisp")))
          (ok (both-deny-p fixture sibling :relative)))))))

(deftest symlinks-are-judged-by-their-target
  (let* ((project-file (place :project '("src") "t" "lisp"))
         (dependency-file (place :dependency '("lib") "d" "lisp"))
         (outside-file (place :outside '("x[1]") "secret" "txt"))
         (places (list project-file dependency-file outside-file))
         (cases (list (list (link :project :file "link-a" project-file) t)
                      (list (link :project :file "link b" dependency-file) t)
                      (list (link :project :file "link b" dependency-file) nil)
                      (list (link :project :file "link-[x]" outside-file) t)
                      (list (link :project :directory "link-リンク" outside-file) t)
                      (list (link :dependency :file "link-a" outside-file) t)
                      (list (link :dependency :directory "link-a" project-file) t))))
    (dolist (root-alias '(nil t))
      (loop for (link registered) in cases
            do (with-read-fixture (fixture :places places :links (list link)
                                           :root-alias root-alias
                                           :register-dependency registered)
                 (dolist (spelling '(:relative :absolute :pathname))
                   (ok (ecase (expected-read-decision link registered)
                         (:allowed (both-allow-as-p fixture link spelling))
                         (:denied (both-deny-p fixture link spelling)))
                       (format nil "alias=~S registered=~S ~S ~S"
                               root-alias registered spelling link))))))))
