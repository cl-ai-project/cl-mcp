;;;; src/paredit.lisp

(defpackage #:cl-mcp/src/paredit
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan-to-strings
                #:regex-replace-all)
  (:import-from #:yason
                #:encode)
  (:import-from #:cl-mcp/src/cst
                #:cst-node
                #:make-cst-node
                #:cst-node-kind
                #:cst-node-value
                #:cst-node-children
                #:cst-node-start
                #:cst-node-end
                #:cst-node-start-line
                #:cst-node-end-line
                #:parse-top-level-forms)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file
                #:fs-resolve-read-path)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:enough-pathname
                #:native-namestring
                #:subpathp)
  (:export #:sexp-wrap
           #:sexp-unwrap
           #:sexp-raise
           #:sexp-slurp-forward
           #:sexp-slurp-backward
           #:sexp-barf-forward
           #:sexp-barf-backward
           #:sexp-kill
           #:sexp-transpose
           #:sexp-split
           #:sexp-join
           #:sexp-show-structure
           #:sexp-get-enclosing))

(in-package #:cl-mcp/src/paredit)

(defun %normalize-paths (file-path)
  "Resolve FILE-PATH and return (values absolute-path relative-namestring)."
  (let ((resolved (fs-resolve-read-path file-path))
        (root (ensure-directory-pathname *project-root*)))
    (unless (subpathp resolved root)
      (error "Path ~A is outside project root ~A" file-path root))
    (let* ((relative (enough-pathname resolved root))
           (rel-namestring (native-namestring relative)))
      (values resolved rel-namestring))))

(defun %node-list-p (node)
  (and (typep node 'cst-node)
       (eq (cst-node-kind node) :expr)
       (consp (cst-node-value node))))

(defun %node-atom-p (node)
  (and (typep node 'cst-node)
       (eq (cst-node-kind node) :expr)
       (atom (cst-node-value node))))

(defun %node-source (text node)
  "Extract the source text for NODE from TEXT."
  (subseq text (cst-node-start node) (cst-node-end node)))

(defun %expr-children (node)
  "Return only :expr CST-NODE children of NODE."
  (remove-if-not (lambda (c)
                   (and (typep c 'cst-node) (eq (cst-node-kind c) :expr)))
                 (cst-node-children node)))

(defun %node-head-name (node)
  "If NODE is a list whose CAR is a symbol, return its lowercase name."
  (when (%node-list-p node)
    (let ((children (%expr-children node)))
      (when (and children (%node-atom-p (first children))
                 (symbolp (cst-node-value (first children))))
        (string-downcase (symbol-name (cst-node-value (first children))))))))

(defun %normalize-string (thing)
  (string-downcase (princ-to-string thing)))

(defun %defmethod-candidates (form)
  (destructuring-bind (_head name &rest rest) form
    (declare (ignore _head))
    (let ((qualifiers '()) (lambda-list nil))
      (dolist (part rest)
        (when (listp part) (setf lambda-list part) (return))
        (push part qualifiers))
      (let ((name-str (%normalize-string name))
            (lambda-str (when lambda-list
                          (%normalize-string (format nil "~S" lambda-list))))
            (qual-str (when qualifiers
                        (%normalize-string (format nil "~{~S~^ ~}" (nreverse qualifiers))))))
        (remove nil (list name-str
                          (when qual-str (format nil "~A ~A" name-str qual-str))
                          (when lambda-str (format nil "~A ~A" name-str lambda-str))
                          (when (and qual-str lambda-str)
                            (format nil "~A ~A ~A" name-str qual-str lambda-str))))))))

(defun %definition-candidates (form form-type)
  (let ((name (second form)))
    (cond
      ((string= form-type "defmethod") (%defmethod-candidates form))
      ((symbolp name) (list (%normalize-string name)))
      (t (list (%normalize-string name))))))

(defun %find-top-level-form (nodes form-type form-name)
  "Find a top-level CST node matching FORM-TYPE and FORM-NAME."
  (multiple-value-bind (base-name index)
      (let ((match (nth-value 1 (scan-to-strings "^(.+?)\\[(\\d+)\\]$" form-name))))
        (if match
            (values (aref match 0) (parse-integer (aref match 1)))
            (values form-name nil)))
    (let ((target (string-downcase base-name))
          (ft (string-downcase form-type))
          (matches nil))
      (loop for node in nodes
            when (and (typep node 'cst-node) (eq (cst-node-kind node) :expr)
                      (consp (cst-node-value node)))
              do (let ((val (cst-node-value node)))
                   (when (and (string= (string-downcase (symbol-name (car val))) ft)
                              (some (lambda (c) (string= c target))
                                    (%definition-candidates val ft)))
                     (push node matches))))
      (setf matches (nreverse matches))
      (cond
        ((null matches) nil)
        ((and index (< index (length matches))) (nth index matches))
        (index (error "Index [~D] out of range, ~D match~:P found" index (length matches)))
        ((= (length matches) 1) (first matches))
        (t (error "Multiple matches for ~A ~A (~D found). Use [N] index to disambiguate."
                  form-type form-name (length matches)))))))

(defun %whitespace-normalize (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (regex-replace-all "\\s+" text " ")))

(defun %collect-all-nodes (root)
  "Depth-first collection of all :expr CST nodes under ROOT."
  (let ((result '()))
    (labels ((walk (node)
               (when (and (typep node 'cst-node) (eq (cst-node-kind node) :expr))
                 (push node result)
                 (dolist (child (cst-node-children node))
                   (walk child)))))
      (walk root))
    (nreverse result)))

(defun %resolve-by-path (root path)
  "Navigate into ROOT using PATH (list of child indices)."
  (let ((current root))
    (dolist (idx path current)
      (unless (%node-list-p current)
        (error "Cannot navigate into atom at path index ~D" idx))
      (let ((children (%expr-children current)))
        (when (>= idx (length children))
          (error "Path index ~D out of range (~D children)" idx (length children)))
        (setf current (nth idx children))))))

(defun %resolve-by-text (root target-text source-text &key line)
  "Find a descendant of ROOT whose source matches TARGET-TEXT."
  (let ((normalized (%whitespace-normalize target-text))
        (nodes (%collect-all-nodes root))
        (exact '()) (prefix '()))
    (dolist (node nodes)
      (let* ((src (%node-source source-text node))
             (nsrc (%whitespace-normalize src)))
        (cond
          ((string= nsrc normalized) (push node exact))
          ((and (> (length nsrc) (length normalized))
                (string= nsrc normalized :end1 (length normalized)))
           (push node prefix)))))
    (let ((matches (or (nreverse exact) (nreverse prefix))))
      (cond
        ((null matches)
         (error "No matching node for target: ~A" target-text))
        ((= (length matches) 1)
         (first matches))
        (line
         (first (sort matches #'< :key (lambda (n) (abs (- (cst-node-start-line n) line))))))
        (t
         (error "Multiple matches (~D) for target. Add line hint or use path to disambiguate."
                (length matches)))))))

(defun %resolve-target (nodes text &key form-type form-name target path line)
  "Unified target resolution. Returns (values target-node root-node)."
  (let ((root (if (and form-type form-name)
                  (or (%find-top-level-form nodes form-type form-name)
                      (error "Top-level form ~A ~A not found" form-type form-name))
                  (if (= 1 (length nodes))
                      (first nodes)
                      (make-cst-node :kind :expr :value nil :children nodes
                                     :start 0 :end (length text))))))
    (cond
      (path   (values (%resolve-by-path root path) root))
      (target (values (%resolve-by-text root target text :line line) root))
      (t      (values root root)))))

(defun %find-parent (root target)
  "Find the parent of TARGET within ROOT. NIL if TARGET is ROOT."
  (labels ((search-in (node)
             (dolist (child (cst-node-children node))
               (when (typep child 'cst-node)
                 (when (eq child target)
                   (return-from %find-parent node))
                 (search-in child)))))
    (search-in root)
    nil))

(defun %find-siblings (parent target)
  "Returns (values before-list target-node after-list) among PARENT's :expr children."
  (let ((children (%expr-children parent))
        (before '()) (found nil) (after '()))
    (dolist (child children)
      (cond (found (push child after))
            ((eq child target) (setf found child))
            (t (push child before))))
    (values (nreverse before) found (nreverse after))))

(defun %transform-wrap (text root target &key (count 1) wrapper head)
  (let ((parent (%find-parent root target)))
    (unless parent (error "Cannot wrap: target has no parent"))
    (multiple-value-bind (_bef _found aft) (%find-siblings parent target)
      (declare (ignore _bef _found))
      (let* ((wrap-nodes (cons target (subseq aft 0 (min (1- count) (length aft)))))
             (rstart (cst-node-start (first wrap-nodes)))
             (rend (cst-node-end (car (last wrap-nodes))))
             (region (subseq text rstart rend))
             (open (cond ((or (null wrapper) (string= wrapper "round")) "(")
                        ((string= wrapper "square") "[")
                        ((string= wrapper "curly") "{")
                        (t "(")))
             (close (cond ((or (null wrapper) (string= wrapper "round")) ")")
                          ((string= wrapper "square") "]")
                          ((string= wrapper "curly") "}")
                          (t ")"))))
        (let ((wrapped (if head
                           (format nil "~A~A ~A~A" open head region close)
                           (format nil "~A~A~A" open region close))))
          (concatenate 'string (subseq text 0 rstart) wrapped (subseq text rend)))))))

(defun %transform-unwrap (text root target &key (keep "all"))
  (unless (%node-list-p target) (error "Cannot unwrap: target is not a list"))
  (let* ((children (%expr-children target))
         (keep-children (if (string= keep "body") (rest children) children)))
    (when (null keep-children) (error "Cannot unwrap: no children to keep"))
    (let ((replacement (format nil "~{~A~^ ~}"
                               (mapcar (lambda (n) (%node-source text n)) keep-children))))
      (concatenate 'string
                   (subseq text 0 (cst-node-start target))
                   replacement
                   (subseq text (cst-node-end target))))))

(defun %transform-raise (text root target)
  (let ((parent (%find-parent root target)))
    (unless parent (error "Cannot raise: target has no parent"))
    (concatenate 'string
                 (subseq text 0 (cst-node-start parent))
                 (%node-source text target)
                 (subseq text (cst-node-end parent)))))

(defun %transform-slurp-forward (text root target &key (count 1))
  (unless (%node-list-p target) (error "Cannot slurp: target is not a list"))
  (let ((parent (%find-parent root target)))
    (unless parent (error "Cannot slurp: target has no parent"))
    (multiple-value-bind (_bef _found aft) (%find-siblings parent target)
      (declare (ignore _bef _found))
      (when (null aft) (error "Cannot slurp forward: no sibling after target"))
      (let* ((slurp-nodes (subseq aft 0 (min count (length aft))))
             (last-slurped (car (last slurp-nodes)))
             (close-pos (1- (cst-node-end target)))
             (slurp-end (cst-node-end last-slurped))
             (slurped (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (subseq text (cst-node-end target) slurp-end)))
             (close-char (char text close-pos)))
        (concatenate 'string
                     (subseq text 0 close-pos)
                     " " slurped (string close-char)
                     (subseq text slurp-end))))))

(defun %transform-slurp-backward (text root target &key (count 1))
  (unless (%node-list-p target) (error "Cannot slurp: target is not a list"))
  (let ((parent (%find-parent root target)))
    (unless parent (error "Cannot slurp: target has no parent"))
    (multiple-value-bind (bef _found _aft) (%find-siblings parent target)
      (declare (ignore _found _aft))
      (when (null bef) (error "Cannot slurp backward: no sibling before target"))
      (let* ((slurp-nodes (last bef (min count (length bef))))
             (first-slurped (first slurp-nodes))
             (slurp-start (cst-node-start first-slurped))
             (open-pos (cst-node-start target))
             (open-char (char text open-pos))
             (slurped (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (subseq text slurp-start open-pos))))
        (concatenate 'string
                     (subseq text 0 slurp-start)
                     (string open-char) slurped " "
                     (subseq text (1+ open-pos) (cst-node-end target))
                     (subseq text (cst-node-end target)))))))

(defun %transform-barf-forward (text root target &key (count 1))
  (declare (ignore root))
  (unless (%node-list-p target) (error "Cannot barf: target is not a list"))
  (let* ((children (%expr-children target))
         (n (length children)))
    (when (<= n count) (error "Cannot barf ~D children: list only has ~D" count n))
    (let* ((last-kept (nth (- n count 1) children))
           (first-barfed (nth (- n count) children))
           (close-pos (1- (cst-node-end target)))
           (close-char (char text close-pos))
           (barfed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (subseq text (cst-node-start first-barfed) close-pos))))
      (concatenate 'string
                   (subseq text 0 (cst-node-end last-kept))
                   (string close-char) " " barfed
                   (subseq text (1+ close-pos))))))

(defun %transform-barf-backward (text root target &key (count 1))
  (declare (ignore root))
  (unless (%node-list-p target) (error "Cannot barf: target is not a list"))
  (let* ((children (%expr-children target))
         (n (length children)))
    (when (<= n count) (error "Cannot barf ~D children: list only has ~D" count n))
    (let* ((first-kept (nth count children))
           (open-pos (cst-node-start target))
           (open-char (char text open-pos))
           (barfed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (subseq text (1+ open-pos) (cst-node-start first-kept)))))
      (concatenate 'string
                   (subseq text 0 open-pos)
                   barfed " "
                   (string open-char)
                   (subseq text (cst-node-start first-kept) (cst-node-end target))
                   (subseq text (cst-node-end target))))))

(defun %transform-kill (text root target &key (count 1))
  (let ((parent (%find-parent root target)))
    (unless parent (error "Cannot kill: target has no parent"))
    (multiple-value-bind (bef _found aft) (%find-siblings parent target)
      (declare (ignore _found))
      (let* ((kill-nodes (cons target (subseq aft 0 (min (1- count) (length aft)))))
             (kill-start (cst-node-start (first kill-nodes)))
             (kill-end (cst-node-end (car (last kill-nodes))))
             (remaining-aft (nthcdr (min (1- count) (length aft)) aft))
             (has-before (not (null bef)))
             (has-after (not (null remaining-aft))))
        (cond
          ((and has-before has-after)
           ;; Siblings on both sides: eat whitespace before kill region;
           ;; whitespace after kill-end provides the separator
           (let ((clean-start (loop for i from (1- kill-start) downto 0
                                    for ch = (char text i)
                                    while (member ch '(#\Space #\Tab))
                                    finally (return (1+ i)))))
             (concatenate 'string
                          (subseq text 0 clean-start) (subseq text kill-end))))
          (has-before
           ;; Last child(ren): eat whitespace before
           (let ((clean-start (loop for i from (1- kill-start) downto 0
                                    for ch = (char text i)
                                    while (member ch '(#\Space #\Tab #\Newline #\Return))
                                    finally (return (1+ i)))))
             (concatenate 'string (subseq text 0 clean-start) (subseq text kill-end))))
          (t
           ;; First child(ren): eat whitespace after
           (let* ((len (length text))
                  (clean-end (loop for i from kill-end below len
                                   for ch = (char text i)
                                   while (member ch '(#\Space #\Tab #\Newline #\Return))
                                   finally (return i))))
             (concatenate 'string (subseq text 0 kill-start) (subseq text clean-end)))))))))

(defun %transform-transpose (text root target)
  (let ((parent (%find-parent root target)))
    (unless parent (error "Cannot transpose: target has no parent"))
    (multiple-value-bind (_bef _found aft) (%find-siblings parent target)
      (declare (ignore _bef _found))
      (when (null aft) (error "Cannot transpose: no next sibling"))
      (let* ((next (first aft))
             (a-start (cst-node-start target)) (a-end (cst-node-end target))
             (b-start (cst-node-start next)) (b-end (cst-node-end next))
             (a-text (subseq text a-start a-end))
             (b-text (subseq text b-start b-end))
             (between (subseq text a-end b-start)))
        (concatenate 'string
                     (subseq text 0 a-start) b-text between a-text (subseq text b-end))))))

(defun %transform-split (text root target &key clone-head)
  (let ((parent (%find-parent root target)))
    (unless parent (error "Cannot split: target has no parent"))
    (unless (%node-list-p parent) (error "Cannot split: parent is not a list"))
    (let* ((children (%expr-children parent))
           (p-start (cst-node-start parent))
           (p-end (cst-node-end parent))
           (open-char (char text p-start))
           (close-char (char text (1- p-end))))
      (multiple-value-bind (before _found after) (%find-siblings parent target)
        (declare (ignore _found))
        (let* ((left before)
               (right (cons target after))
               (head-text (when (and clone-head left)
                            (%node-source text (first children))))
               (left-str (format nil "~{~A~^ ~}"
                                 (mapcar (lambda (n) (%node-source text n)) left)))
               (right-parts (if (and clone-head head-text)
                                (cons head-text
                                      (mapcar (lambda (n) (%node-source text n)) right))
                                (mapcar (lambda (n) (%node-source text n)) right)))
               (right-str (format nil "~{~A~^ ~}" right-parts)))
          (concatenate 'string
                       (subseq text 0 p-start)
                       (string open-char) left-str (string close-char)
                       " "
                       (string open-char) right-str (string close-char)
                       (subseq text p-end)))))))

(defun %transform-join (text root target &key drop-head)
  (unless (%node-list-p target) (error "Cannot join: target is not a list"))
  (let ((parent (%find-parent root target)))
    (unless parent (error "Cannot join: target has no parent"))
    (multiple-value-bind (_bef _found aft) (%find-siblings parent target)
      (declare (ignore _bef _found))
      (when (null aft) (error "Cannot join: no next sibling"))
      (let ((next (first aft)))
        (unless (%node-list-p next) (error "Cannot join: next sibling is not a list"))
        (let* ((close-pos (1- (cst-node-end target)))
               (close-char (char text close-pos))
               (next-children (%expr-children next))
               (merge (if drop-head (rest next-children) next-children))
               (merge-text (format nil "~{~A~^ ~}"
                                   (mapcar (lambda (n) (%node-source text n)) merge))))
          (concatenate 'string
                       (subseq text 0 close-pos)
                       (if merge " " "")
                       merge-text
                       (string close-char)
                       (subseq text (cst-node-end next))))))))

(defun %node-kind-string (node)
  (cond
    ((eq (cst-node-kind node) :skipped) "comment")
    ((%node-list-p node) "list")
    ((symbolp (cst-node-value node)) "symbol")
    ((stringp (cst-node-value node)) "string")
    ((numberp (cst-node-value node)) "number")
    (t "atom")))

(defun %truncate (text max-len)
  (if (<= (length text) max-len) text
      (concatenate 'string (subseq text 0 (- max-len 3)) "...")))

(defun %show-structure (root text &key (depth 4) (max-text 60))
  "Build structure tree as nested hash-tables for JSON output."
  (labels ((build (node path d)
             (when (and (typep node 'cst-node) (eq (cst-node-kind node) :expr))
               (let ((ht (make-ht "path" (coerce path 'vector)
                                  "kind" (%node-kind-string node)
                                  "line" (cst-node-start-line node)
                                  "text" (%truncate (%node-source text node) max-text))))
                 (when (and (%node-list-p node) (< d depth))
                   (let ((ch (loop for child in (%expr-children node)
                                   for i from 0
                                   for sub = (build child (append path (list i)) (1+ d))
                                   when sub collect sub)))
                     (when ch (setf (gethash "children" ht) (coerce ch 'vector)))))
                 ht))))
    (let ((tree (if (%node-list-p root)
                    (loop for child in (%expr-children root)
                          for i from 0
                          for sub = (build child (list i) 1)
                          when sub collect sub)
                    nil)))
      (make-ht "form" (%truncate (%node-source text root) 120)
               "tree" (coerce tree 'vector)))))

(defun %get-enclosing (root target text &key (levels 1))
  "Return info about the enclosing form of TARGET."
  (let ((current target) (enclosing nil))
    (dotimes (_ levels)
      (setf enclosing (%find-parent root current))
      (unless enclosing (return))
      (setf current enclosing))
    (unless enclosing
      (return-from %get-enclosing
        (make-ht "error" "No enclosing form at requested level")))
    (let ((children (%expr-children enclosing))
          (child-idx nil))
      (loop for ch in children for i from 0
            when (eq ch (if (= levels 1) target current))
              do (setf child-idx i) (return))
      (make-ht "enclosing_text" (%truncate (%node-source text enclosing) 200)
               "enclosing_kind" (%node-kind-string enclosing)
               "head" (or (%node-head-name enclosing) "nil")
               "child_index" (or child-idx -1)
               "sibling_count" (length children)
               "line" (cst-node-start-line enclosing)))))

(defun %apply-paredit (file-path form-type form-name target-text path line dry-run
                       operation-name transform-fn &rest transform-args)
  "Read file, parse, resolve target, apply TRANSFORM-FN, write back.
OPERATION-NAME is a string like \"sexp-wrap\" used for logging."
  (multiple-value-bind (abs rel) (%normalize-paths file-path)
    (let* ((text (fs-read-file abs))
           (nodes (parse-top-level-forms text)))
      (multiple-value-bind (target-node root-node)
          (%resolve-target nodes text
                           :form-type form-type :form-name form-name
                           :target target-text :path (when path (coerce path 'list))
                           :line line)
        (let ((new-text (apply transform-fn text root-node target-node transform-args)))
          (log-event :debug "paredit"
                     "path" (namestring abs)
                     "operation" operation-name
                     "bytes" (length new-text)
                     "dry_run" dry-run)
          (if dry-run
              (make-ht "would_change" (not (string= text new-text))
                       "original" (%node-source text target-node)
                       "preview" new-text)
              (progn
                (fs-write-file rel new-text)
                (let* ((ns (parse-top-level-forms new-text))
                       (nt (ignore-errors
                             (%resolve-target
                              ns new-text
                              :form-type form-type
                              :form-name form-name)))
                       (region-text (if nt (%node-source new-text nt) new-text)))
                  (make-ht "path" file-path
                           "bytes" (length new-text)
                           "changed_region"
                           (make-ht "start_line" (cst-node-start-line target-node)
                                    "end_line" (if nt
                                                   (cst-node-end-line nt)
                                                   (cst-node-end-line target-node))
                                    "text" (%truncate region-text 500)))))))))))

(define-tool "sexp-wrap"
  :description "Wrap one or more consecutive sibling S-expressions in new delimiters.
MOST IMPORTANT structural editing tool: adds parentheses without the LLM writing closing delimiters.
Use 'head' to specify the operator (e.g. \"progn\", \"let\", \"handler-case\").
Use 'count' to wrap multiple consecutive siblings.
Example: wrapping (db:insert order) with head=\"handler-case\" yields (handler-case (db:insert order)).
Requires at least one of 'target' or 'path' to identify the first sexp to wrap."
  :args ((file-path :type :string :required t
                    :description "Target file path (absolute or relative to project root)")
         (form-type :type :string
                    :description "Top-level form type for scoping (e.g. \"defun\", \"defmacro\")")
         (form-name :type :string
                    :description "Top-level form name for scoping")
         (target :type :string
                 :description "Source text of the first sexp to wrap (primary addressing for LLMs)")
         (path :type :array
               :description "Child index path from top-level form root, e.g. [2, 0, 1]")
         (line :type :integer
               :description "Line number hint for disambiguation (1-based)")
         (count :type :integer
                :description "Number of consecutive siblings to wrap (default: 1)")
         (wrapper :type :string
                  :description "Delimiter type: \"round\" (default), \"square\", \"curly\"")
         (head :type :string
               :description "Operator to insert as first element, e.g. \"progn\", \"let ()\", \"when\"")
         (dry-run :type :boolean
                  :description "Preview without writing to disk"))
  :body
  (let ((result-data (%apply-paredit file-path form-type form-name target path line dry-run
                                     "sexp-wrap" #'%transform-wrap
                                     :count (or count 1)
                                     :wrapper wrapper
                                     :head head)))
    (if dry-run
        (result id (make-ht "content" (text-content
                                       (format nil "Dry-run sexp-wrap (~:[no change~;would change~])"
                                               (gethash "would_change" result-data)))
                            "preview" result-data))
        (result id (make-ht "content" (text-content
                                       (format nil "Applied sexp-wrap on ~A (~D bytes)"
                                               file-path (gethash "bytes" result-data)))
                            "path" file-path
                            "operation" "sexp-wrap"
                            "bytes" (gethash "bytes" result-data)
                            "changed_region" (gethash "changed_region" result-data))))))

(define-tool "sexp-unwrap"
  :description "Remove the enclosing delimiters of a list, promoting its children into the parent (splice).
Inverse of sexp-wrap. Use keep=\"body\" to also drop the operator (first child).
Example: unwrapping (progn (a) (b)) with keep=\"body\" yields (a) (b)."
  :args ((file-path :type :string :required t
                    :description "Target file path")
         (form-type :type :string :description "Top-level form type for scoping")
         (form-name :type :string :description "Top-level form name for scoping")
         (target :type :string :description "Source text of the list to unwrap")
         (path :type :array :description "Child index path to the list")
         (line :type :integer :description "Line number hint")
         (keep :type :string
               :description "\"all\" (default) keeps all children; \"body\" drops the operator")
         (dry-run :type :boolean :description "Preview without writing"))
  :body
  (let ((result-data (%apply-paredit file-path form-type form-name target path line dry-run
                                     "sexp-unwrap" #'%transform-unwrap
                                     :keep (or keep "all"))))
    (if dry-run
        (result id (make-ht "content" (text-content "Dry-run sexp-unwrap") "preview" result-data))
        (result id (make-ht "content" (text-content
                                       (format nil "Applied sexp-unwrap on ~A" file-path))
                            "path" file-path "operation" "sexp-unwrap"
                            "bytes" (gethash "bytes" result-data)
                            "changed_region" (gethash "changed_region" result-data))))))

(define-tool "sexp-raise"
  :description "Replace the parent list with a single child S-expression.
The targeted sexp replaces its entire enclosing form.
Example: raising (do-thing) inside (if test (do-thing) nil) replaces the whole if-form with (do-thing)."
  :args ((file-path :type :string :required t :description "Target file path")
         (form-type :type :string :description "Top-level form type for scoping")
         (form-name :type :string :description "Top-level form name for scoping")
         (target :type :string :description "Source text of the child sexp to raise")
         (path :type :array :description "Child index path to the child sexp")
         (line :type :integer :description "Line number hint")
         (dry-run :type :boolean :description "Preview without writing"))
  :body
  (let ((result-data (%apply-paredit file-path form-type form-name target path line dry-run
                                     "sexp-raise" #'%transform-raise)))
    (if dry-run
        (result id (make-ht "content" (text-content "Dry-run sexp-raise") "preview" result-data))
        (result id (make-ht "content" (text-content
                                       (format nil "Applied sexp-raise on ~A" file-path))
                            "path" file-path "operation" "sexp-raise"
                            "bytes" (gethash "bytes" result-data)
                            "changed_region" (gethash "changed_region" result-data))))))

(define-tool "sexp-slurp-forward"
  :description "Expand a list by pulling the next sibling(s) into it (move closing delimiter rightward).
The target must be a list. The sibling(s) after it get absorbed as new last child(ren).
Example: (let ((x 1))) (use-x) becomes (let ((x 1)) (use-x)) when slurping on the let form."
  :args ((file-path :type :string :required t :description "Target file path")
         (form-type :type :string :description "Top-level form type for scoping")
         (form-name :type :string :description "Top-level form name for scoping")
         (target :type :string :description "Source text of the list to grow")
         (path :type :array :description "Child index path to the list")
         (line :type :integer :description "Line number hint")
         (count :type :integer :description "Number of siblings to slurp (default: 1)")
         (dry-run :type :boolean :description "Preview without writing"))
  :body
  (let ((result-data (%apply-paredit file-path form-type form-name target path line dry-run
                                     "sexp-slurp-forward" #'%transform-slurp-forward
                                     :count (or count 1))))
    (if dry-run
        (result id (make-ht "content" (text-content "Dry-run sexp-slurp-forward")
                            "preview" result-data))
        (result id (make-ht "content" (text-content
                                       (format nil "Applied sexp-slurp-forward on ~A" file-path))
                            "path" file-path "operation" "sexp-slurp-forward"
                            "bytes" (gethash "bytes" result-data)
                            "changed_region" (gethash "changed_region" result-data))))))

(define-tool "sexp-slurp-backward"
  :description "Expand a list by pulling the previous sibling(s) into it (move opening delimiter leftward).
The target must be a list. The sibling(s) before it get absorbed as new first child(ren).
Example: (compute) (list result) becomes (list (compute) result) when slurping backward on (list result)."
  :args ((file-path :type :string :required t :description "Target file path")
         (form-type :type :string :description "Top-level form type for scoping")
         (form-name :type :string :description "Top-level form name for scoping")
         (target :type :string :description "Source text of the list to grow")
         (path :type :array :description "Child index path to the list")
         (line :type :integer :description "Line number hint")
         (count :type :integer :description "Number of siblings to slurp (default: 1)")
         (dry-run :type :boolean :description "Preview without writing"))
  :body
  (let ((result-data (%apply-paredit file-path form-type form-name target path line dry-run
                                     "sexp-slurp-backward" #'%transform-slurp-backward
                                     :count (or count 1))))
    (if dry-run
        (result id (make-ht "content" (text-content "Dry-run sexp-slurp-backward")
                            "preview" result-data))
        (result id (make-ht "content" (text-content
                                       (format nil "Applied sexp-slurp-backward on ~A" file-path))
                            "path" file-path "operation" "sexp-slurp-backward"
                            "bytes" (gethash "bytes" result-data)
                            "changed_region" (gethash "changed_region" result-data))))))

(define-tool "sexp-barf-forward"
  :description "Shrink a list by pushing its last child(ren) out as following sibling(s).
Moves the closing delimiter leftward, ejecting the last N children.
Example: (let ((x 1)) (compute) (cleanup)) becomes (let ((x 1)) (compute)) (cleanup)."
  :args ((file-path :type :string :required t :description "Target file path")
         (form-type :type :string :description "Top-level form type for scoping")
         (form-name :type :string :description "Top-level form name for scoping")
         (target :type :string :description "Source text of the list to shrink")
         (path :type :array :description "Child index path to the list")
         (line :type :integer :description "Line number hint")
         (count :type :integer :description "Number of children to barf out (default: 1)")
         (dry-run :type :boolean :description "Preview without writing"))
  :body
  (let ((result-data (%apply-paredit file-path form-type form-name target path line dry-run
                                     "sexp-barf-forward" #'%transform-barf-forward
                                     :count (or count 1))))
    (if dry-run
        (result id (make-ht "content" (text-content "Dry-run sexp-barf-forward")
                            "preview" result-data))
        (result id (make-ht "content" (text-content
                                       (format nil "Applied sexp-barf-forward on ~A" file-path))
                            "path" file-path "operation" "sexp-barf-forward"
                            "bytes" (gethash "bytes" result-data)
                            "changed_region" (gethash "changed_region" result-data))))))

(define-tool "sexp-barf-backward"
  :description "Shrink a list by pushing its first child(ren) out as preceding sibling(s).
Moves the opening delimiter rightward, ejecting the first N children.
Example: (list a b c) with count=1 becomes a (list b c)."
  :args ((file-path :type :string :required t :description "Target file path")
         (form-type :type :string :description "Top-level form type for scoping")
         (form-name :type :string :description "Top-level form name for scoping")
         (target :type :string :description "Source text of the list to shrink")
         (path :type :array :description "Child index path to the list")
         (line :type :integer :description "Line number hint")
         (count :type :integer :description "Number of children to barf out (default: 1)")
         (dry-run :type :boolean :description "Preview without writing"))
  :body
  (let ((result-data (%apply-paredit file-path form-type form-name target path line dry-run
                                     "sexp-barf-backward" #'%transform-barf-backward
                                     :count (or count 1))))
    (if dry-run
        (result id (make-ht "content" (text-content "Dry-run sexp-barf-backward")
                            "preview" result-data))
        (result id (make-ht "content" (text-content
                                       (format nil "Applied sexp-barf-backward on ~A" file-path))
                            "path" file-path "operation" "sexp-barf-backward"
                            "bytes" (gethash "bytes" result-data)
                            "changed_region" (gethash "changed_region" result-data))))))

(define-tool "sexp-kill"
  :description "Delete one or more complete S-expressions. Structurally safe — always removes whole sexps.
Cleans up surrounding whitespace. Use count to kill multiple consecutive siblings."
  :args ((file-path :type :string :required t :description "Target file path")
         (form-type :type :string :description "Top-level form type for scoping")
         (form-name :type :string :description "Top-level form name for scoping")
         (target :type :string :description "Source text of the sexp to kill")
         (path :type :array :description "Child index path to the sexp")
         (line :type :integer :description "Line number hint")
         (count :type :integer :description "Number of consecutive siblings to kill (default: 1)")
         (dry-run :type :boolean :description "Preview without writing"))
  :body
  (let ((result-data (%apply-paredit file-path form-type form-name target path line dry-run
                                     "sexp-kill" #'%transform-kill
                                     :count (or count 1))))
    (if dry-run
        (result id (make-ht "content" (text-content "Dry-run sexp-kill") "preview" result-data))
        (result id (make-ht "content" (text-content
                                       (format nil "Applied sexp-kill on ~A" file-path))
                            "path" file-path "operation" "sexp-kill"
                            "bytes" (gethash "bytes" result-data)
                            "changed_region" (gethash "changed_region" result-data))))))

(define-tool "sexp-transpose"
  :description "Swap two adjacent sibling S-expressions. The target and its next sibling are swapped.
Example: (list alpha beta) with target=\"alpha\" becomes (list beta alpha)."
  :args ((file-path :type :string :required t :description "Target file path")
         (form-type :type :string :description "Top-level form type for scoping")
         (form-name :type :string :description "Top-level form name for scoping")
         (target :type :string :description "Source text of the first (left) sexp")
         (path :type :array :description "Child index path to the first sexp")
         (line :type :integer :description "Line number hint")
         (dry-run :type :boolean :description "Preview without writing"))
  :body
  (let ((result-data (%apply-paredit file-path form-type form-name target path line dry-run
                                     "sexp-transpose" #'%transform-transpose)))
    (if dry-run
        (result id (make-ht "content" (text-content "Dry-run sexp-transpose")
                            "preview" result-data))
        (result id (make-ht "content" (text-content
                                       (format nil "Applied sexp-transpose on ~A" file-path))
                            "path" file-path "operation" "sexp-transpose"
                            "bytes" (gethash "bytes" result-data)
                            "changed_region" (gethash "changed_region" result-data))))))

(define-tool "sexp-split"
  :description "Split a list into two at the targeted child. Children before the target stay in the first list;
the target and everything after go into the second list.
Use clone_head=true to copy the operator symbol into both lists.
Example: (progn (a) (b) (c)) split at (b) with clone_head becomes (progn (a)) (progn (b) (c))."
  :args ((file-path :type :string :required t :description "Target file path")
         (form-type :type :string :description "Top-level form type for scoping")
         (form-name :type :string :description "Top-level form name for scoping")
         (target :type :string :description "Source text of the child where split happens")
         (path :type :array :description "Child index path to the split point")
         (line :type :integer :description "Line number hint")
         (clone-head :type :boolean
                     :description "Copy operator symbol into both resulting lists")
         (dry-run :type :boolean :description "Preview without writing"))
  :body
  (let ((result-data (%apply-paredit file-path form-type form-name target path line dry-run
                                     "sexp-split" #'%transform-split
                                     :clone-head clone-head)))
    (if dry-run
        (result id (make-ht "content" (text-content "Dry-run sexp-split") "preview" result-data))
        (result id (make-ht "content" (text-content
                                       (format nil "Applied sexp-split on ~A" file-path))
                            "path" file-path "operation" "sexp-split"
                            "bytes" (gethash "bytes" result-data)
                            "changed_region" (gethash "changed_region" result-data))))))

(define-tool "sexp-join"
  :description "Join two adjacent sibling lists into one. The second list's children are appended to the first.
Use drop_head=true to discard the operator of the second list.
Example: (progn (a)) (progn (b)) with drop_head becomes (progn (a) (b))."
  :args ((file-path :type :string :required t :description "Target file path")
         (form-type :type :string :description "Top-level form type for scoping")
         (form-name :type :string :description "Top-level form name for scoping")
         (target :type :string :description "Source text of the first list")
         (path :type :array :description "Child index path to the first list")
         (line :type :integer :description "Line number hint")
         (drop-head :type :boolean
                    :description "Drop the operator of the second list before joining")
         (dry-run :type :boolean :description "Preview without writing"))
  :body
  (let ((result-data (%apply-paredit file-path form-type form-name target path line dry-run
                                     "sexp-join" #'%transform-join
                                     :drop-head drop-head)))
    (if dry-run
        (result id (make-ht "content" (text-content "Dry-run sexp-join") "preview" result-data))
        (result id (make-ht "content" (text-content
                                       (format nil "Applied sexp-join on ~A" file-path))
                            "path" file-path "operation" "sexp-join"
                            "bytes" (gethash "bytes" result-data)
                            "changed_region" (gethash "changed_region" result-data))))))

(define-tool "sexp-show-structure"
  :description "Show the S-expression tree structure of a form. Returns a tree with paths, text snippets,
and node kinds. USE THIS TOOL FIRST to discover the exact 'path' or 'target' text for mutation tools.
Essential for understanding code structure before performing structural edits."
  :args ((file-path :type :string :required t
                    :description "Target file path")
         (form-type :type :string
                    :description "Top-level form type for scoping (omit for full file)")
         (form-name :type :string
                    :description "Top-level form name for scoping")
         (target :type :string
                 :description "Narrow to a specific sub-form")
         (path :type :array
               :description "Child index path to narrow to")
         (line :type :integer :description "Line number hint")
         (depth :type :integer
                :description "Max tree depth to show (default: 4)"))
  :body
  (multiple-value-bind (abs _rel) (%normalize-paths file-path)
    (declare (ignore _rel))
    (let* ((text (fs-read-file abs))
           (nodes (parse-top-level-forms text)))
      (multiple-value-bind (target-node _root)
          (%resolve-target nodes text
                           :form-type form-type :form-name form-name
                           :target target
                           :path (when path (coerce path 'list))
                           :line line)
        (declare (ignore _root))
        (let ((structure (%show-structure target-node text :depth (or depth 4))))
          (result id (make-ht "content" (text-content
                                         (with-output-to-string (s)
                                           (encode structure s))))))))))

(define-tool "sexp-get-enclosing"
  :description "Return information about the enclosing form of a targeted node.
Shows the parent form's text, kind, head symbol, and the target's position among siblings.
Useful before sexp-raise or sexp-unwrap to understand what will be affected."
  :args ((file-path :type :string :required t :description "Target file path")
         (form-type :type :string :description "Top-level form type for scoping")
         (form-name :type :string :description "Top-level form name for scoping")
         (target :type :string :description "Source text of the child node")
         (path :type :array :description "Child index path to the child node")
         (line :type :integer :description "Line number hint")
         (levels :type :integer
                 :description "How many levels up to go (default: 1)"))
  :body
  (multiple-value-bind (abs _rel) (%normalize-paths file-path)
    (declare (ignore _rel))
    (let* ((text (fs-read-file abs))
           (nodes (parse-top-level-forms text)))
      (multiple-value-bind (target-node root-node)
          (%resolve-target nodes text
                           :form-type form-type :form-name form-name
                           :target target
                           :path (when path (coerce path 'list))
                           :line line)
        (let ((info (%get-enclosing root-node target-node text :levels (or levels 1))))
          (result id (make-ht "content" (text-content
                                         (with-output-to-string (s)
                                           (encode info s))))))))))
