;;;; tests/write-path-specs-test.lisp
;;;;
;;;; The write policy's key cases on fixed trees, for ENSURE-WRITE-PATH and
;;;; FS-WRITE-FILE, and the write fixtures behind the cl-mcp/specs write-path
;;;; properties.  Needs no cl-spec: it runs in the default suite, and
;;;; scripts/check-specs.lisp's self-test runs it too.
;;;;
;;;; Every case builds its own scratch tree under the temporary directory and
;;;; binds *PROJECT-ROOT* for its body only.  Everything that is written, even
;;;; by a wrong implementation, lands inside that tree: each check lists the
;;;; whole tree before and after its one call and judges from the listings, and
;;;; only then does cleanup remove what was written.

(defpackage #:cl-mcp/tests/write-path-specs-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:ensure-write-path
                #:write-path-refused)
  (:import-from #:cl-mcp/specs/path-fixtures
                #:read-fixture-scratch
                #:read-fixture-adopted
                #:call-with-read-fixture
                #:with-read-fixture
                #:region-native
                #:fixture-symlink
                #:register-dependency
                #:unregister-dependency
                #:scratch-snapshot
                #:snapshot-changes)
  (:import-from #:cl-mcp/specs/write-fixtures
                #:*write-contents*
                #:write-target
                #:write-case
                #:draw-write-case
                #:with-write-fixture
                #:expected-write-decision
                #:observe-validator
                #:observe-writer
                #:no-changes-p
                #:same-entries-p
                #:validator-agrees-p
                #:writer-agrees-p))

(in-package #:cl-mcp/tests/write-path-specs-test)

;;; ------------------------------------------------------------------------
;;; Helpers

(defun place (region dirs &optional name type)
  "A place descriptor, for fixed trees."
  (list :region region :dirs dirs :name name :type type))

(defun link (region kind name target)
  "A link descriptor, for fixed trees."
  (list :region region :kind kind :name name :target target))

(defmacro with-tree ((fixture &rest options) &body body)
  "Run BODY in a fresh scratch tree that adopts whatever BODY writes into it."
  `(with-read-fixture (,fixture :adopt-new-entries t ,@options) ,@body))

(defun validate (fixture argument)
  "ENSURE-WRITE-PATH on ARGUMENT, observed."
  (observe-validator fixture argument))

(defun write-to (fixture argument &optional (contents *write-contents*))
  "FS-WRITE-FILE of CONTENTS on ARGUMENT, observed."
  (observe-writer fixture argument contents))

(defun real-path (fixture relative)
  "The real native path of RELATIVE, a path below FIXTURE's scratch root."
  (concatenate 'string
               (uiop:native-namestring
                (truename (uiop:parse-native-namestring (read-fixture-scratch fixture)
                                                        :ensure-directory t)))
               relative))

(defun in-region (fixture region relative)
  "The native path of RELATIVE below REGION's directory in FIXTURE."
  (concatenate 'string (region-native fixture region) relative))

(defun allowed-as-p (observed native)
  "True when the observed call returned an absolute pathname naming NATIVE and
changed nothing."
  (let ((result (getf observed :returned)))
    (and (pathnamep result)
         (uiop:absolute-pathname-p result)
         (string= (uiop:native-namestring result) native)
         (no-changes-p (getf observed :changes)))))

(defun refused-p (observed reason)
  "True when the observed call signalled WRITE-PATH-REFUSED for REASON and
changed nothing."
  (and (eq (getf observed :refused) reason)
       (no-changes-p (getf observed :changes))))

(defun octets (string)
  "STRING's UTF-8 bytes."
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun wrote-exactly-p (observed &key added changed)
  "True when the observed write returned, added exactly ADDED and changed
exactly CHANGED -- snapshot entries with their new details -- and removed
nothing."
  (let ((changes (getf observed :changes)))
    (and (getf observed :wrote)
         (same-entries-p (getf changes :added) added)
         (null (getf changes :removed))
         (same-entries-p (mapcar #'second (getf changes :changed)) changed))))

(defun table-tree-options ()
  "Fixture options for the table of fixed topologies:

  project/src/a.txt, project/plain.txt, project/x[1]/, project/日本語/,
  project/a b/, outside/src/o.txt;
  project/link-a      -> project/src          (directory link, inside)
  project/link b      -> outside/src          (directory link, outside)
  project/link-リンク -> project/src/a.txt    (file link, inside)
  project/link-[x]    -> outside/src/o.txt    (file link, outside)"
  (let ((inside (place :project '("src") "a" "txt"))
        (outside (place :outside '("src") "o" "txt")))
    (list :places (list inside
                        (place :project '() "plain" "txt")
                        (place :project '("x[1]"))
                        (place :project '("日本語"))
                        (place :project '("a b"))
                        outside)
          :links (list (link :project :directory "link-a" inside)
                       (link :project :directory "link b" outside)
                       (link :project :file "link-リンク" inside)
                       (link :project :file "link-[x]" outside)))))

(defmacro with-table-tree ((fixture) &body body)
  "Run BODY in a fresh tree built from TABLE-TREE-OPTIONS, adopting whatever
BODY writes into it."
  `(apply #'call-with-read-fixture (lambda (,fixture) ,@body)
          :adopt-new-entries t (table-tree-options)))

(defun add-dangling-links (fixture)
  "Add three dangling links to FIXTURE's project/: dangling -> ../nowhere/,
dleaf -> nothing.txt, doleaf -> ../outside/nothing.txt."
  (fixture-symlink fixture (in-region fixture :project "dangling") "../nowhere/")
  (fixture-symlink fixture (in-region fixture :project "dleaf") "nothing.txt")
  (fixture-symlink fixture (in-region fixture :project "doleaf") "../outside/nothing.txt"))

(define-condition planned-failure (error) ()
  (:documentation "Signalled on purpose from a fixture body."))

;;; ------------------------------------------------------------------------
;;; The reported escape

(deftest write-refuses-the-escape-through-a-project-link
  ;; The counterexample as found: project/escape is a relative link to
  ;; ../outside/, where sentinel.txt exists.  A new file or a new directory
  ;; below escape/ used to be allowed, and fs-write-file created it in outside/.
  (with-tree (fixture :places (list (place :outside '() "sentinel" "txt")))
    (fixture-symlink fixture (in-region fixture :project "escape") "../outside/")
    (testing "the validator refuses and creates nothing"
      (ok (refused-p (validate fixture "escape/new.txt") :outside-project))
      (ok (refused-p (validate fixture "escape/new-dir/new.txt") :outside-project))
      (ok (refused-p (validate fixture "escape/sentinel.txt") :outside-project)))
    (testing "the writer refuses and leaves the whole tree as it was"
      (ok (refused-p (write-to fixture "escape/new.txt") :outside-project))
      (ok (refused-p (write-to fixture "escape/new-dir/new.txt") :outside-project))
      (ok (refused-p (write-to fixture "escape/sentinel.txt") :outside-project)))
    (testing "the refusal keeps its message"
      (ok (handler-case (progn (ensure-write-path "escape/new.txt") nil)
            (write-path-refused (condition)
              (and (search "outside project root" (princ-to-string condition)) t)))))))

;;; ------------------------------------------------------------------------
;;; The validator on fixed topologies

(deftest write-allows-project-targets-as-their-real-path
  (with-table-tree (fixture)
    (testing "an existing file, and new leaves below existing directories"
      (ok (allowed-as-p (validate fixture "src/a.txt") (real-path fixture"project/src/a.txt")))
      (ok (allowed-as-p (validate fixture "src/new.txt") (real-path fixture"project/src/new.txt")))
      (ok (allowed-as-p (validate fixture "new.txt") (real-path fixture"project/new.txt"))))
    (testing "one and two new directories, none of them created"
      (ok (allowed-as-p (validate fixture "src/n1/new.txt")
                        (real-path fixture"project/src/n1/new.txt")))
      (ok (allowed-as-p (validate fixture "n1/n2/new.txt")
                        (real-path fixture"project/n1/n2/new.txt"))))
    (testing "names with brackets, spaces and Japanese are taken literally"
      (ok (allowed-as-p (validate fixture "x[1]/v[old] 2.txt")
                        (real-path fixture"project/x[1]/v[old] 2.txt")))
      (ok (allowed-as-p (validate fixture "日本語/データ.d/新規.lisp")
                        (real-path fixture"project/日本語/データ.d/新規.lisp")))
      (ok (allowed-as-p (validate fixture "a b/c d") (real-path fixture"project/a b/c d"))))
    (testing "through a link that stays inside the project"
      (ok (allowed-as-p (validate fixture "link-a/new.txt") (real-path fixture"project/src/new.txt")))
      (ok (allowed-as-p (validate fixture "link-a/n1/new.txt")
                        (real-path fixture"project/src/n1/new.txt")))
      (ok (allowed-as-p (validate fixture "link-リンク") (real-path fixture"project/src/a.txt"))))
    (testing "other spellings of the same targets"
      (ok (allowed-as-p (validate fixture (uiop:parse-native-namestring "src/n1/new.txt"))
                        (real-path fixture"project/src/n1/new.txt")))
      (ok (allowed-as-p (validate fixture "./src/../src/new.txt")
                        (real-path fixture"project/src/new.txt")))
      (ok (allowed-as-p (validate fixture "../project/src/new.txt")
                        (real-path fixture"project/src/new.txt")))
      (ok (allowed-as-p (validate fixture "src//./new.txt") (real-path fixture"project/src/new.txt"))))))

(deftest write-refuses-targets-outside-the-project
  (with-table-tree (fixture)
    (testing "through a link that leads outside"
      (ok (refused-p (validate fixture "link b/new.txt") :outside-project))
      (ok (refused-p (validate fixture "link b/n1/new.txt") :outside-project))
      (ok (refused-p (validate fixture "link b/o.txt") :outside-project))
      (ok (refused-p (validate fixture "link-[x]") :outside-project)))
    (testing "through .."
      (ok (refused-p (validate fixture "../outside/new.txt") :outside-project))
      (ok (refused-p (validate fixture "../outside/src/o.txt") :outside-project))
      (ok (refused-p (validate fixture "../project-other/new.txt") :outside-project))
      (ok (refused-p (validate fixture "src/../../outside/new.txt") :outside-project)))
    (testing "through .. in a relative pathname, where it is kept as :UP, not collapsed"
      (ok (refused-p (validate fixture (uiop:parse-native-namestring "../outside/new.txt"))
                     :outside-project))
      (ok (refused-p (validate fixture (uiop:parse-native-namestring "../outside/n1/new.txt"))
                     :outside-project))
      (ok (refused-p (write-to fixture (uiop:parse-native-namestring "../outside/n1/new.txt"))
                     :outside-project)))
    (testing "absolute arguments, even inside the project"
      (ok (refused-p (validate fixture (real-path fixture"project/src/new.txt")) :absolute))
      (ok (refused-p (validate fixture (real-path fixture"project/src/a.txt")) :absolute))
      (ok (refused-p (validate fixture (uiop:parse-native-namestring
                                        (real-path fixture"project/new.txt")))
                     :absolute))
      (ok (refused-p (validate fixture (real-path fixture"outside/new.txt")) :absolute)))))

(deftest write-refuses-what-it-cannot-check
  (with-table-tree (fixture)
    (add-dangling-links fixture)
    (testing "an ancestor that is a file, or a link to one"
      (ok (refused-p (validate fixture "plain.txt/x.txt") :non-directory-ancestor))
      (ok (refused-p (validate fixture "link-リンク/x.txt") :non-directory-ancestor)))
    (testing "a dangling link, as an ancestor or as the target"
      (ok (refused-p (validate fixture "dangling/x.txt") :unresolvable-ancestor))
      (ok (refused-p (validate fixture "dleaf") :unresolvable-target))
      (ok (refused-p (validate fixture "doleaf") :unresolvable-target)))
    (testing ".. right after a link or after a name that does not exist"
      (ok (refused-p (validate fixture "link-a/../x.txt") :parent-after-link))
      (ok (refused-p (validate fixture "link b/../x.txt") :parent-after-link))
      (ok (refused-p (validate fixture "n1/../x.txt") :parent-after-missing))
      (ok (refused-p (validate fixture "src/n1/../../x.txt") :parent-after-missing)))
    (testing "no file name"
      (ok (refused-p (validate fixture "") :no-file-name))
      (ok (refused-p (validate fixture "src/") :no-file-name))
      (ok (refused-p (validate fixture ".") :no-file-name))
      (ok (refused-p (validate fixture "src/..") :no-file-name)))))

(deftest write-refuses-an-unresolvable-root
  (with-tree (fixture)
    (let ((*project-root* (uiop:parse-native-namestring
                           (concatenate 'string (read-fixture-scratch fixture) "missing-root/")
                           :ensure-directory t)))
      (ok (refused-p (validate fixture "new.txt") :unresolvable-root))
      (ok (refused-p (write-to fixture "n1/new.txt") :unresolvable-root)
          "nothing is created, the root included"))))

(deftest write-without-a-project-root-signals
  (let ((*project-root* nil))
    (ok (handler-case (progn (ensure-write-path "new.txt") nil)
          (error (condition)
            (and (search "Project root is not set" (princ-to-string condition)) t))))))

(deftest write-follows-a-project-root-alias
  ;; project-alias -> project.  Relative arguments are relative to the alias,
  ;; and targets come back as their real path under project/.  New targets used
  ;; to be refused here while existing ones were allowed.
  (with-tree (fixture :places (list (place :project '("src") "a" "txt")) :root-alias t)
    (ok (search "project-alias" (uiop:native-namestring *project-root*)))
    (ok (allowed-as-p (validate fixture "src/a.txt") (real-path fixture"project/src/a.txt")))
    (ok (allowed-as-p (validate fixture "brand-new.txt") (real-path fixture"project/brand-new.txt")))
    (ok (allowed-as-p (validate fixture "src/new.txt") (real-path fixture"project/src/new.txt")))
    (ok (allowed-as-p (validate fixture "n1/new.txt") (real-path fixture"project/n1/new.txt")))
    (ok (refused-p (validate fixture "../outside/new.txt") :outside-project))
    (ok (allowed-as-p (validate fixture "../project-alias/new.txt")
                      (real-path fixture"project/new.txt"))
        "out of the real directory and back in through the alias, as the OS resolves it")))

(deftest write-never-allows-a-registered-dependency
  ;; Reading follows ASDF's registry; writing never does.
  (with-tree (fixture :places (list (place :dependency '("src") "d" "lisp"))
                      :links (list (link :project :directory "link-a"
                                         (place :dependency '("src") "d" "lisp"))))
    (flet ((refused-everywhere-p ()
             (and (refused-p (validate fixture "../dependency/new.txt") :outside-project)
                  (refused-p (validate fixture "../dependency/src/d.lisp") :outside-project)
                  (refused-p (validate fixture "link-a/new.txt") :outside-project)
                  (refused-p (validate fixture "link-a/d.lisp") :outside-project))))
      (ok (refused-everywhere-p) "before registration")
      (register-dependency fixture)
      (ok (refused-everywhere-p) "while registered")
      (ok (refused-p (write-to fixture "link-a/d.lisp") :outside-project)
          "the writer leaves the registered file alone")
      (ok (refused-p (write-to fixture "../dependency/new.txt") :outside-project))
      (unregister-dependency fixture)
      (ok (refused-everywhere-p) "after registration"))))

;;; ------------------------------------------------------------------------
;;; The writer

(deftest writer-creates-exactly-the-new-directories-and-file
  (with-tree (fixture :places (list (place :project '("src") "a" "txt")))
    (ok (wrote-exactly-p (write-to fixture "src/n1/n2/new.txt")
                         :added `(("project/src/n1/" :directory nil)
                                  ("project/src/n1/n2/" :directory nil)
                                  ("project/src/n1/n2/new.txt" :file
                                                               ,(octets *write-contents*)))))
    (ok (wrote-exactly-p (write-to fixture "Makefile")
                         :added `(("project/Makefile" :file ,(octets *write-contents*))))
        "a name without a type is written as it is")
    (ok (wrote-exactly-p (write-to fixture "x[1]/v[old] 2.txt" "括弧")
                         :added `(("project/x[1]/" :directory nil)
                                  ("project/x[1]/v[old] 2.txt" :file ,(octets "括弧")))))))

(deftest writer-replaces-an-existing-file-in-place
  (with-tree (fixture :places (list (place :project '("src") "a" "txt")))
    (ok (wrote-exactly-p (write-to fixture "src/a.txt")
                         :changed `(("project/src/a.txt" :file ,(octets *write-contents*))))
        "only the file's bytes change, and no temporary file is left")))

(deftest writer-replaces-an-existing-type-less-file-in-place
  ;; RENAME-FILE merged the temporary file's type "tmp" into a target without
  ;; one: the write reported success, left the file as it was, and put the new
  ;; bytes in NAME.tmp beside it.
  (with-tree (fixture :places (list (place :project '() "Makefile")
                                    (place :project '() ".hidden")
                                    (place :project '("sub") "LICENSE")))
    (dolist (relative '("Makefile" ".hidden" "sub/LICENSE"))
      (ok (wrote-exactly-p (write-to fixture relative)
                           :changed `((,(concatenate 'string "project/" relative) :file
                                       ,(octets *write-contents*))))
          relative))))

(deftest writer-follows-links-and-aliases-that-stay-inside
  (with-table-tree (fixture)
    (ok (wrote-exactly-p (write-to fixture "link-a/n1/new.txt")
                         :added `(("project/src/n1/" :directory nil)
                                  ("project/src/n1/new.txt" :file ,(octets *write-contents*))))
        "a new file through a directory link lands in the link's target")
    (ok (wrote-exactly-p (write-to fixture "link-リンク")
                         :changed `(("project/src/a.txt" :file ,(octets *write-contents*))))
        "a file link's target changes; the link stays a link"))
  (with-tree (fixture :root-alias t)
    (ok (wrote-exactly-p (write-to fixture "n1/new.txt")
                         :added `(("project/n1/" :directory nil)
                                  ("project/n1/new.txt" :file ,(octets *write-contents*)))))))

(deftest writer-refusals-change-nothing
  (with-table-tree (fixture)
    (add-dangling-links fixture)
    (dolist (argument '("link b/new.txt" "link b/n1/new.txt" "link b/o.txt" "link-[x]"
                        "../outside/new.txt" "plain.txt/x.txt" "dangling/x.txt" "dleaf"
                        "doleaf" "link-a/../x.txt" "n1/../x.txt" "src/"))
      (let ((observed (write-to fixture argument)))
        (ok (and (getf observed :refused) (no-changes-p (getf observed :changes)))
            argument)))))

;;; ------------------------------------------------------------------------
;;; The fixtures and the oracle

(deftest write-fixture-creates-no-new-directory-before-the-call
  (let ((case (write-case (write-target :project '("src") '("n1" "n2") "new" "txt"))))
    (with-write-fixture (fixture case)
      (let ((paths (mapcar #'first (scratch-snapshot fixture))))
        (ok (member "project/src/" paths :test #'string=))
        (ng (member "project/src/n1/" paths :test #'string=))))))

(deftest write-fixture-observes-before-it-adopts
  (let ((scratch nil)
        (seen nil)
        (adopted nil))
    (with-tree (fixture)
      (setf scratch (read-fixture-scratch fixture))
      (let ((observed (write-to fixture "n1/new.txt")))
        (setf seen (mapcar #'first (getf (getf observed :changes) :added))))
      ;; Read the list after the body, from the fixture object itself.
      (setf adopted fixture))
    (ok (equal seen '("project/n1/" "project/n1/new.txt"))
        "the check saw what the write created")
    (ok (equal (mapcar #'second (read-fixture-adopted adopted))
               (list (concatenate 'string scratch "project/n1/")
                     (concatenate 'string scratch "project/n1/new.txt")))
        "cleanup adopted both entries, deepest last in the list")
    (ng (probe-file (uiop:parse-native-namestring scratch :ensure-directory t)))))

(deftest write-fixture-adopts-links-without-following-them
  (let* ((owned (format nil "~Acl-mcp-write-spec-owned-~D-~D/"
                        (uiop:native-namestring (uiop:temporary-directory))
                        (sb-posix:getpid) (get-universal-time)))
         (keep (concatenate 'string owned "keep.txt")))
    (sb-posix:mkdir owned #o700)
    (unwind-protect
         (progn
           (with-open-file (out (uiop:parse-native-namestring keep) :direction :output)
             (write-string "keep" out))
           (with-tree (fixture)
             ;; Made by the body, not the fixture, so cleanup adopts it.
             (sb-posix:symlink (string-right-trim "/" owned)
                               (in-region fixture :project "made-by-body")))
           (ok (probe-file (uiop:parse-native-namestring keep))
               "the link was unlinked and its target left alone"))
      (sb-posix:unlink keep)
      (sb-posix:rmdir owned))))

(deftest write-fixture-adopts-when-its-body-signals
  (let ((scratch nil))
    (ok (handler-case
            (with-tree (fixture)
              (setf scratch (read-fixture-scratch fixture))
              (write-to fixture "n1/new.txt")
              (error 'planned-failure))
          (planned-failure () t)))
    (ng (probe-file (uiop:parse-native-namestring scratch :ensure-directory t)))))

(deftest snapshot-changes-reports-additions-removals-and-changes
  (let ((before '(("a/" :directory nil) ("a/f" :file #(1 2)) ("l" :link "x")))
        (after '(("a/" :directory nil) ("a/f" :file #(1 3)) ("l" :link "X") ("b" :file #()))))
    (let ((changes (snapshot-changes before after)))
      (ok (equal (mapcar #'first (getf changes :added)) '("b")))
      (ok (null (getf changes :removed)))
      (ok (equal (mapcar (lambda (pair) (first (first pair))) (getf changes :changed))
                 '("a/f" "l"))
          "bytes and a link target that differs only in case both count"))
    (ok (no-changes-p (snapshot-changes before before)))
    (ok (equal (mapcar #'first (getf (snapshot-changes before (cdr before)) :removed))
               '("a/")))))

(deftest expected-write-decision-states-the-policy
  (flet ((decision (target &rest options)
           (expected-write-decision (apply #'write-case target options))))
    (let ((inside (write-target :project '("src") '() "new" "txt"))
          (new-first (write-target :project '() '("n1") "new" "txt")))
      (ok (eq (decision inside) :allowed))
      (ok (eq (decision inside :spelling :absolute) :refused))
      (ok (eq (decision inside :spelling :detour) :allowed))
      (ok (eq (decision new-first :spelling :detour) :refused) "n1/../n1 before n1 exists")
      (ok (eq (decision inside :link '(:region :project :kind :directory :name "link-a")
                               :spelling :detour)
              :refused)
          "link-a/../link-a")
      (ok (eq (decision inside :link '(:region :outside :kind :directory :name "link-a"))
              :allowed)
          "where a link sits does not matter; where it leads does"))
    (dolist (region '(:dependency :outside :project-other))
      (ok (eq (decision (write-target region '() '() "new" "txt") :dependency-registered t)
              :refused)
          ;; Rove's reporter takes the description as a string.
          (format nil "region=~S remains unwritable" region)))))

(deftest generated-write-cases-agree-on-a-fixed-sample
  ;; One draw of each shape under a fixed random state, so the default suite
  ;; also runs the property bodies' checks without cl-spec.
  (let ((*random-state* (sb-ext:seed-random-state 20260922)))
    (dotimes (i 6)
      (let ((case (draw-write-case)))
        (with-write-fixture (fixture case)
          (ok (validator-agrees-p fixture case) (prin1-to-string case))
          (ok (writer-agrees-p fixture case) (prin1-to-string case)))))))
