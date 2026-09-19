# cl-spec Adapter Fidelity Implementation Plan

> **2026-09-20 追記（cl-spec PR #34 / merge `08d3ada` 以降）**: この計画は
> 旧 cl-spec の `state.capture.values = ((NAME . VALUE) ...)` を前提に書かれた
> 歴史的記録である。現在の cl-spec v1 は binding ごとの tagged availability
> record（`(:name N :availability :collected :value V)` /
> `(:name N :availability :unavailable :reason :opaque-value :type T)`）を返す
> ため、実装は `(:alist :opaque)` ではなく
> `(:array (:ref :capture-value-record))` を使い、形による opaque marker 認識
> （`%opaque-marker-node`）は削除された。以下の本文中の該当箇所は当時のまま
> 残すが、現行の正は
> `docs/superpowers/specs/2026-09-19-cl-spec-adapter-fidelity-design.md` §6.3 と
> `src/spec-core-record.lisp` を参照。

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Carry cl-spec's versioned contract and execution records through the MCP
boundary without dropping, re-deriving or flattening them, so an agent can tell a
target bug from a contract bug, an unreached case, a state-post violation, an
incomplete generation and an unavailable shrink — from the response alone.

**Architecture:** A new pure-Lisp module turns one cl-spec versioned record (a
plist) into JSON-safe report data: it validates the record, decides per-field
availability with `GET-PROPERTIES`, and projects values through a
schema-descriptor-driven recursive projector whose default is
`EXTERNALIZE-VALUE`. `spec-adapter-report.lisp` consumes it and keeps its legacy
readers as fallbacks; `spec-response-builders.lisp` renders it, with the text kept
downstream of the structured report so the two cannot disagree.

**Tech Stack:** SBCL, ASDF `package-inferred-system`, Rove, Yason. cl-spec stays
optional and late-bound — no ASDF dependency, no compile-time reference to any
cl-spec package symbol.

**Design note:** `docs/superpowers/specs/2026-09-19-cl-spec-adapter-fidelity-design.md`.
Every section reference below (§N) points there.

## Global Constraints

- Google Common Lisp Style Guide. 2-space indent, **≤100 columns**, no tabs.
- Blank line between top-level forms. Lower-case lisp-case: `my-function`,
  `*special*`, `+constant+`, `something-p`.
- **Docstrings required** on every public function and class.
- Each file starts with `(in-package ...)` after its `defpackage`.
- ASDF `package-inferred-system`: a new `src/foo.lisp` defines package
  `cl-mcp/src/foo` and is pulled in by whoever `:import-from`s it.
  **`cl-mcp.asd` needs no edit.** A new `tests/foo-test.lisp` must be added to
  the root `tests.lisp` `:import-from` list or it never runs.
- **Edit `.lisp` files with the `lisp-edit-form` / `lisp-patch-form` MCP tools,
  never a text editor.** Text edits on Lisp produce off-by-one paren errors;
  `lisp-edit-form` has parinfer auto-repair. Run `lisp-check-parens` after
  editing deeply nested forms.
- **cl-spec is optional and late-bound.** No `cl-spec:` symbol may appear in
  cl-mcp source at compile time. Everything goes through the `cl-spec-api`
  struct handles (`api-has-p`, `api-fn`).
- **No new reader or eval path.** Names from MCP arguments are resolved with
  `find-package` / `find-symbol` only — never `intern`, never `read`.
- **No Lisp value becomes a JSON number unless it is an integer in
  `[-(2^53 - 1), 2^53 - 1]`.** Seeds are always decimal strings (§6.2.3).
- Lint before every commit, with the same globs the Lint CI job runs:
  `mallet src/*.lisp src/*/*.lisp tests/*.lisp`
- Full suite: `rove cl-mcp.asd` from a **fresh process** (the live MCP server's
  image goes stale). `rove` on a single file can be green while the suite is red.

---

## File Structure

**Create**

| File | Responsibility |
|---|---|
| `src/spec-core-record.lisp` | One cl-spec versioned record → JSON-safe report data. Validation, per-field availability, the schema-descriptor recursive projector, safe-integer rule, projection-issue accumulation. Pure functions over plists; calls no cl-spec API. |
| `tests/spec-core-record-test.lisp` | Unit tests for the above. No cl-spec in the image. |

**Modify**

| File | Change |
|---|---|
| `src/spec-adapter-report.lisp` | `%describe-function-spec` (Task A), `%result-plist` (`core_result`), `%contract-facts` (schema gate), `%verified-p` / `%verification-gaps` / `+verification-gap-values+` |
| `src/tools/spec-response-builders.lisp` | `%core-result-ht` and friends, describe JSON + text, check JSON + text |
| `src/tools/spec-tools.lisp` | tool descriptions (checked against code by `spec-tools-test.lisp`) |
| `tests/spec-adapter-report-test.lisp` | stub-API cases 8–22 of §12 |
| `tests/spec-response-builders-test.lisp` | text/JSON agreement, §12 cases 17, 23, 24 |
| `tests/spec-integration-test.lisp` | §12 scenarios 1–5 against real cl-spec |
| `tests/fixtures/spec-fixture-contracts.lisp` | fixtures for those scenarios |
| `tests.lisp` | register `cl-mcp/tests/spec-core-record-test` |
| `docs/tools.md` | the `cl-spec` group section |

---

## Task 1: Leaf projection and the safe-integer rule

**Files:**
- Create: `src/spec-core-record.lisp`
- Create: `tests/spec-core-record-test.lisp`
- Modify: `tests.lisp:70` (add the new test package)

**Interfaces:**
- Consumes: `cl-mcp/src/spec-adapter-core:symbol-data`,
  `cl-mcp/src/spec-adapter-core:externalize-value`.
- Produces:
  - `(safe-json-integer-p n)` → generalized boolean.
  - `(project-value value &key (max-chars 2000))` → a **tagged node**, one of
    `(:scalar <string|number|nil>)`, `(:symbol <symbol-data plist>)` or
    `(:value <externalize-value plist>)`. Tasks 2 and 5 build trees of these;
    Task 4's builder renders them. The tag is what lets the renderer tell a
    `symbol-data` plist from an `externalize-value` plist without inspecting
    their keys.

- [ ] **Step 1: Write the failing test**

Create `tests/spec-core-record-test.lisp`:

```lisp
;;;; tests/spec-core-record-test.lisp
;;;;
;;;; The versioned-record layer, exercised with plain plists and no cl-spec in
;;;; the image.  Every function here is pure over data cl-spec would have
;;;; returned, which is what lets these cases cover the revisions this project
;;;; cannot install.

(defpackage #:cl-mcp/tests/spec-core-record-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/spec-core-record
                #:safe-json-integer-p
                #:project-value))

(in-package #:cl-mcp/tests/spec-core-record-test)

(deftest safe-json-integer-covers-the-double-mantissa
  (testing "the boundary itself is safe"
    (ok (safe-json-integer-p (1- (expt 2 53))))
    (ok (safe-json-integer-p (- (1- (expt 2 53))))))
  (testing "one past it is not, although it is still a fixnum"
    (ok (not (safe-json-integer-p (expt 2 53))))
    (ok (not (safe-json-integer-p (- (expt 2 53)))))
    ;; The case this rule exists for: a cl-spec seed is a fixnum and is far
    ;; wider than a JSON consumer holds exactly.
    (ok (not (safe-json-integer-p 4611686018427387903)))))

(deftest project-value-tags-each-type-once
  (testing "a keyword is a scalar word, not a symbol node"
    (ok (equal '(:scalar "state-post") (project-value :state-post))))
  (testing "NIL is a scalar null, not the symbol COMMON-LISP::NIL"
    (ok (equal '(:scalar nil) (project-value nil))))
  (testing "any other symbol is a symbol node and keeps its package"
    (let ((node (project-value 'cl-user::widen)))
      (ok (eq :symbol (first node)))
      (ok (equal "WIDEN" (getf (second node) :name)))
      (ok (equal "COMMON-LISP-USER" (getf (second node) :package)))))
  (testing "a string is a scalar"
    (ok (equal '(:scalar "boom") (project-value "boom"))))
  (testing "a safe integer stays a number and a wider one becomes text"
    (ok (equal '(:scalar 42) (project-value 42)))
    (ok (equal '(:scalar "4611686018427387903")
               (project-value 4611686018427387903))))
  (testing "anything else is a value node from externalize-value"
    (let ((node (project-value (make-hash-table))))
      (ok (eq :value (first node)))
      (ok (stringp (getf (second node) :printed)))
      (ok (equal "HASH-TABLE" (getf (second node) :type))))))
```

- [ ] **Step 2: Register the test package so it actually runs**

Use `lisp-edit-form` on `tests.lisp` to add one line to the `defpackage`
`:import-from` list, after `#:cl-mcp/tests/spec-adapter-core-test`:

```lisp
  (:import-from #:cl-mcp/tests/spec-core-record-test)
```

A file not listed here is never loaded and its tests silently do not run.

- [ ] **Step 3: Run the test to verify it fails**

Run: `rove tests/spec-core-record-test.lisp`
Expected: FAIL — the package `CL-MCP/SRC/SPEC-CORE-RECORD` does not exist.

- [ ] **Step 4: Create the module with the two functions**

Create `src/spec-core-record.lisp`:

```lisp
;;;; src/spec-core-record.lisp
;;;;
;;;; One cl-spec versioned record, turned into data an MCP client can read.
;;;;
;;;; Nothing here calls cl-spec.  The input is a plist cl-spec already
;;;; produced -- RESULT-DATA or FUNCTION-SPEC-DATA -- so every rule in this
;;;; file can be exercised against the revisions this project cannot install,
;;;; including ones that do not exist yet.
;;;;
;;;; Three rules shape it.  A record is validated before it is read, because a
;;;; versioned API that answers something malformed is a fault to report
;;;; rather than a reason to fall back to an older reader.  Availability is
;;;; decided with GET-PROPERTIES and never with GETF, because a key that is
;;;; absent and a key whose value is NIL are different answers and GETF gives
;;;; the same one for both.  And projection is driven by a schema descriptor
;;;; rather than by the shape of the value, because the shape does not say
;;;; what the value means: (:AT-LEAST :AT-MOST) is two case names and
;;;; (:KIND :RANGE) is a plist, and nothing in the conses tells them apart.

(defpackage #:cl-mcp/src/spec-core-record
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:symbol-data
                #:externalize-value)
  (:export #:safe-json-integer-p
           #:project-value))

(in-package #:cl-mcp/src/spec-core-record)

(defconstant +max-safe-json-integer+ (1- (expt 2 53))
  "The largest integer a JSON consumer holds exactly.

A double's mantissa is 53 bits, so a consumer reading JSON into doubles --
which is every JavaScript one -- rounds anything wider.  An SBCL fixnum reaches
2^62, and a cl-spec seed is a fixnum, which is why this adapter has always sent
seeds as text.  The rule is stated once here rather than at each site that
publishes a number.")

(defun safe-json-integer-p (value)
  "Return true when VALUE is an integer every JSON consumer holds exactly."
  (and (integerp value)
       (<= (- +max-safe-json-integer+) value +max-safe-json-integer+)))

(defun project-value (value &key (max-chars 2000))
  "Return VALUE as a tagged projection node, decided by its type.

The node is (:SCALAR x), (:SYMBOL plist) or (:VALUE plist).  The tag is for the
renderer: a SYMBOL-DATA plist and an EXTERNALIZE-VALUE plist are both plists,
and a renderer that had to tell them apart by looking for :QUALIFIED or
:PRINTED would break the first time either grew a key.

A keyword becomes its lower-case name, because cl-spec uses keywords as the
vocabulary of its statuses and kinds and a client renders them as words.  NIL
is a scalar null rather than the symbol COMMON-LISP::NIL, or every absent value
in every record would arrive as a symbol reference.  Any other symbol keeps its
package: two same-named symbols from different packages are different
definitions.

An integer inside the JSON-safe range stays a number; a wider one becomes a
decimal string rather than a number a consumer would round.  Everything else --
a CLOS instance, a structure, a hash table, a function, a value from the code
under test -- goes through EXTERNALIZE-VALUE, which prints it bounded and
offers an object id instead of pretending the text is the object."
  (cond ((keywordp value) (list :scalar (string-downcase (symbol-name value))))
        ((null value) (list :scalar nil))
        ((symbolp value) (list :symbol (symbol-data value)))
        ((stringp value) (list :scalar value))
        ((safe-json-integer-p value) (list :scalar value))
        ((integerp value) (list :scalar (format nil "~D" value)))
        (t (list :value (externalize-value value :max-chars max-chars)))))
```

- [ ] **Step 5: Run the test to verify it passes**

Run: `rove tests/spec-core-record-test.lisp`
Expected: PASS, 12 checks.

- [ ] **Step 6: Lint**

Run: `mallet src/spec-core-record.lisp tests/spec-core-record-test.lisp`
Expected: no findings.

- [ ] **Step 7: Commit**

```bash
git add src/spec-core-record.lisp tests/spec-core-record-test.lisp tests.lisp
git commit -m "feat(spec): tagged leaf projection with a JSON-safe integer rule

A keyword becomes a word, NIL a null, any other symbol keeps its package, and
an integer wider than a double's mantissa becomes text rather than a number a
consumer would round -- the rule this adapter already followed for seeds,
stated once for every number the record layer will publish.

Nodes carry their kind so the renderer never has to tell a symbol plist from an
externalized-value plist by looking for one of their keys."
```

---

## Task 2: The schema-descriptor recursive projector

Projection is driven by a descriptor, not by the shape of the value. §6.2.1 is
the whole reason: `(:AT-LEAST :AT-MOST)` is two case names, `(:KIND :RANGE ...)`
is a plist, and nothing in the conses distinguishes them — so a projector that
guesses invents key/value relations cl-spec never declared. An `explain-data`
error datum settles it: `:actual` is a value from the code under test and
`:expected` is a descriptor cl-spec built, side by side under the same keys.

**Files:**
- Modify: `src/spec-core-record.lisp`
- Modify: `tests/spec-core-record-test.lisp`

**Interfaces:**
- Consumes: `project-value` and `safe-json-integer-p` from Task 1.
- Produces:
  - `*projection-max-depth*` (12) and `*projection-max-length*` (200), specials.
  - `(project-record value descriptor &key path max-chars)` →
    `(values NODE ISSUES UNKNOWN-KEYS)`.
    - NODE is `(:scalar x)`, `(:symbol plist)`, `(:value plist)`,
      `(:object (("json_key" . NODE) ...))` or `(:array (NODE ...))`.
    - ISSUES is a list of `(:path (...) :reason :length-limit :omitted-items N)`
      or `(:path (...) :reason :depth-limit)`.
    - UNKNOWN-KEYS is a list of dotted path strings, e.g.
      `"failure.explanation.some-future-key"`.
  - Descriptor grammar, all of it:
    - `:leaf` — `project-value`
    - `:opaque` — always `externalize-value`, whatever the type
    - `:word-list` — a list of keywords becomes an array of words
    - `(:object (KEYWORD . DESCRIPTOR) ...)` — a plist; keys not listed are
      recorded in UNKNOWN-KEYS and their values are **not** projected (§5)
    - `(:array DESCRIPTOR)` — a list, every element under DESCRIPTOR
    - `(:alist DESCRIPTOR)` — an alist; each `(NAME . VALUE)` becomes
      `{"name": ..., "value": ...}` with VALUE under DESCRIPTOR
    - `(:ref NAME)` — indirection through `*record-shapes*`, for the recursion
      an error datum needs (its `:errors` holds error datums)

- [ ] **Step 1: Write the failing tests**

Append to `tests/spec-core-record-test.lisp`, and extend its `:import-from` for
`#:project-record`, `#:*projection-max-depth*` and `#:*projection-max-length*`
(the depth case below binds the first of those):

```lisp
(deftest object-descriptor-projects-only-declared-keys
  (let ((shape '(:object (:kind . :leaf) (:index . :leaf))))
    (multiple-value-bind (node issues unknown)
        (project-record '(:kind :state-postcondition :index 0 :surprise 7) shape)
      (ok (null issues))
      (testing "declared keys are projected under snake_case names"
        (ok (equal '(:scalar "state-postcondition")
                   (cdr (assoc "kind" (second node) :test #'equal))))
        (ok (equal '(:scalar 0)
                   (cdr (assoc "index" (second node) :test #'equal)))))
      (testing "an undeclared key is named and its value is not interpreted"
        (ok (equal '("surprise") unknown))
        (ok (null (assoc "surprise" (second node) :test #'equal)))))))

(deftest a-keyword-list-is-an-array-not-an-object
  ;; §6.2.1's own counterexample.  (:AT-LEAST :AT-MOST) is two case names;
  ;; read as a plist it becomes {"at-least": "at-most"}, a relation cl-spec
  ;; never declared.
  (let ((node (project-record '(:at-least :at-most) :word-list)))
    (ok (eq :array (first node)))
    (ok (equal '((:scalar "at-least") (:scalar "at-most")) (second node)))))

(deftest actual-and-expected-are-projected-differently
  ;; The same cons under two keys of one error datum: :ACTUAL is a value from
  ;; the code under test, :EXPECTED is a descriptor cl-spec built.
  (let* ((shape '(:object (:actual . :opaque) (:expected . (:object (:kind . :leaf)))))
         (node (project-record '(:actual (1 2 3) :expected (:kind :range)) shape))
         (fields (second node)))
    (testing ":actual is externalized, never structured"
      (let ((actual (cdr (assoc "actual" fields :test #'equal))))
        (ok (eq :value (first actual)))
        (ok (search "1 2 3" (getf (second actual) :printed)))))
    (testing ":expected keeps its structure"
      (let ((expected (cdr (assoc "expected" fields :test #'equal))))
        (ok (eq :object (first expected)))
        (ok (equal '(:scalar "range")
                   (cdr (assoc "kind" (second expected) :test #'equal))))))))

(deftest an-alist-of-capture-values-becomes-name-value-pairs
  ;; Measured shape: ((BALANCE-BEFORE . 30) (ID-BEFORE . 7)).  Dotted pairs are
  ;; not proper lists, so an array rule has nothing to say about them.
  (let* ((node (project-record (list (cons 'cl-user::balance-before 30))
                               '(:alist :opaque)))
         (entry (first (second node)))
         (fields (second entry)))
    (ok (eq :array (first node)))
    (ok (equal "BALANCE-BEFORE"
               (getf (second (cdr (assoc "name" fields :test #'equal))) :name)))
    (ok (eq :value (first (cdr (assoc "value" fields :test #'equal)))))))

(deftest a-length-cut-is-reported-not-hidden
  (let ((*projection-max-length* 2))
    (multiple-value-bind (node issues)
        (project-record '(:a :b :c :d :e) :word-list '("failure" "cases"))
      (ok (= 2 (length (second node))))
      (ok (= 1 (length issues)))
      (let ((issue (first issues)))
        (ok (equal '("failure" "cases") (getf issue :path)))
        (ok (eq :length-limit (getf issue :reason)))
        ;; Five errors must not arrive as two that look complete.
        (ok (eql 3 (getf issue :omitted-items)))))))

(deftest a-depth-cut-leaves-the-standard-value-node
  (let ((*projection-max-depth* 1)
        (shape '(:object (:inner . (:object (:deeper . :leaf))))))
    (multiple-value-bind (node issues)
        (project-record '(:inner (:deeper 1)) shape)
      (let ((inner (cdr (assoc "inner" (second node) :test #'equal))))
        ;; Not a marker invented for this: the externalized-value plist is what
        ;; already represents any Lisp value everywhere else in the record.
        (ok (eq :value (first inner))))
      (ok (eq :depth-limit (getf (first issues) :reason)))
      (ok (equal '("inner") (getf (first issues) :path))))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `rove tests/spec-core-record-test.lisp`
Expected: FAIL — `PROJECT-RECORD` is not defined.

- [ ] **Step 3: Implement the projector**

Add to `src/spec-core-record.lisp`, exporting `#:project-record`,
`#:*projection-max-depth*`, `#:*projection-max-length*` and `#:*record-shapes*`:

```lisp
(defparameter *projection-max-depth* 12
  "How deep a record projection descends before it externalizes the rest.

Matched to *VALUE-PRINT-LEVEL*, which bounds the printer this module hands its
leaves to: a record whose depth ran past what the printer would show anyway
costs traversal for output nobody sees.")

(defparameter *projection-max-length* 200
  "How many entries of one list or plist a record projection keeps.")

(defvar *record-shapes* '()
  "Plist of NAME to descriptor, for the descriptors that refer to themselves.

An EXPLAIN-DATA error datum holds :ERRORS, a list of error datums, so its
descriptor cannot be written as a literal without (:REF :ERROR-DATUM).")

(defun %json-key (keyword)
  "Return KEYWORD as the snake_case JSON key this project publishes under.

Keys are snake_case and values keep their hyphens -- \"budget_source\" holding
\"not-collected\" -- which is the convention every existing response follows."
  (substitute #\_ #\- (string-downcase (symbol-name keyword))))

(defun %dotted-path (path)
  "Return PATH, a list of key names and indices, as one dotted string."
  (format nil "~{~A~^.~}" path))

(defun %resolve-descriptor (descriptor)
  "Return DESCRIPTOR with a (:REF NAME) indirection followed."
  (if (and (consp descriptor) (eq :ref (first descriptor)))
      (getf *record-shapes* (second descriptor))
      descriptor))

(defun project-record (value descriptor &optional path)
  "Project VALUE under DESCRIPTOR and return (values NODE ISSUES UNKNOWN-KEYS).

DESCRIPTOR says what VALUE means; VALUE's own shape never decides.  That is the
point: (:AT-LEAST :AT-MOST) is two case names and (:KIND :RANGE) is a plist,
and no test on the conses tells them apart, so a projector that guessed would
publish a key/value relation cl-spec never declared.

NODE is one of (:SCALAR x), (:SYMBOL plist), (:VALUE plist),
(:OBJECT ((key . NODE) ...)) or (:ARRAY (NODE ...)).  ISSUES records every
place the projection was cut, as (:PATH path :REASON reason [:OMITTED-ITEMS n]),
so a consumer can tell a record with two errors from a record with ten that was
cut at two.  UNKNOWN-KEYS names the keys no descriptor covers: their existence
is reported and their meaning is deliberately not guessed.

PATH is the position reached so far, for the entries of ISSUES and
UNKNOWN-KEYS."
  (let ((issues '())
        (unknown '()))
    (labels
        ((walk (value descriptor path depth)
           (let ((descriptor (%resolve-descriptor descriptor)))
             (cond
               ((eq :leaf descriptor) (project-value value))
               ((eq :opaque descriptor) (list :value (externalize-value value)))
               ((>= depth *projection-max-depth*)
                (push (list :path (reverse path) :reason :depth-limit) issues)
                (list :value (externalize-value value)))
               ((eq :word-list descriptor)
                (list :array (walk-list value :leaf path depth)))
               ((not (consp descriptor)) (project-value value))
               ((eq :array (first descriptor))
                (list :array (walk-list value (second descriptor) path depth)))
               ((eq :alist (first descriptor))
                (list :array (walk-alist value (second descriptor) path depth)))
               ((eq :object (first descriptor))
                (walk-object value (rest descriptor) path depth))
               (t (project-value value)))))
         (bounded (items path)
           ;; Cut here rather than in each caller, so the entry that records
           ;; the cut cannot be forgotten in one of them.
           (if (<= (length items) *projection-max-length*)
               items
               (progn
                 (push (list :path (reverse path) :reason :length-limit
                             :omitted-items (- (length items)
                                               *projection-max-length*))
                       issues)
                 (subseq items 0 *projection-max-length*))))
         (walk-list (items descriptor path depth)
           (loop for item in (bounded items path)
                 for index from 0
                 collect (walk item descriptor (cons index path) (1+ depth))))
         (walk-alist (entries descriptor path depth)
           (loop for entry in (bounded entries path)
                 for index from 0
                 collect
                 (list :object
                       (list (cons "name" (project-value (car entry)))
                             (cons "value"
                                   (walk (cdr entry) descriptor
                                         (cons index path) (1+ depth)))))))
         (walk-object (plist fields path depth)
           (let ((entries '()))
             (loop for (key raw) on (bounded plist path) by #'cddr
                   for field = (assoc key fields)
                   do (if field
                          (push (cons (%json-key key)
                                      (walk raw (cdr field)
                                            (cons (%json-key key) path)
                                            (1+ depth)))
                                entries)
                          ;; Named, not interpreted.  A future key's meaning is
                          ;; cl-spec's to define, and publishing a guess at it
                          ;; is the one thing this module must not do.
                          (push (%dotted-path
                                 (reverse (cons (%json-key key) path)))
                                unknown)))
             (list :object (nreverse entries)))))
      (let ((node (walk value descriptor (reverse path) 0)))
        (values node (nreverse issues) (nreverse unknown))))))
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `rove tests/spec-core-record-test.lisp`
Expected: PASS — 12 checks from Task 1 plus 19 here.

- [ ] **Step 5: Check parens and lint**

Run the `lisp-check-parens` MCP tool on `src/spec-core-record.lisp`
(`project-record` nests six local functions), then:
`mallet src/spec-core-record.lisp tests/spec-core-record-test.lisp`

- [ ] **Step 6: Commit**

```bash
git add src/spec-core-record.lisp tests/spec-core-record-test.lisp
git commit -m "feat(spec): project records by descriptor, never by shape

A value's conses do not say what it means.  (:AT-LEAST :AT-MOST) is two case
names, (:KIND :RANGE) is a plist, and an explain-data error datum carries
:ACTUAL -- a value from the code under test -- beside :EXPECTED, a descriptor
cl-spec built.  So the descriptor decides and the default is externalization,
which leaves inventing a key/value relation impossible rather than unlikely.

Cuts are recorded with their path and the count they dropped: ten errors must
not arrive as two that look complete.  Keys no descriptor covers are named and
their values left alone -- their meaning is cl-spec's to define."
```

---

## Task 3: Record validation and per-field availability

Two rules that the rest of the adapter leans on. A versioned API that answers
something malformed is a fault to report, not a reason to quietly fall back to
an older reader (§3.5). And `GETF` cannot tell an absent key from a key whose
value is `NIL` — `:failure-phase NIL` is the legitimate reading "an ordinary
target observation with no special phase", which is not the same as a revision
that has no such key (§3.2).

**Files:**
- Modify: `src/spec-core-record.lisp`
- Modify: `tests/spec-core-record-test.lisp`

**Interfaces:**
- Produces:
  - `+v1-required-metadata+` — the seven keys `schema-info` declares required.
  - `(validate-versioned-record record &key expected-record-kind
     expected-entity-kind)` → `(values STATUS REASON)`, STATUS one of `:ok`,
    `:unsupported-schema`, `:malformed`; REASON a string for the two bad ones
    and the schema version for `:unsupported-schema`.
  - `(field-availability record key)` → `:collected`, `:not-collected` or
    `:absent`.
  - `+sentinel-fields+` — the only fields where a `:NOT-COLLECTED` value means
    availability rather than data.

- [ ] **Step 1: Write the failing tests**

Append to `tests/spec-core-record-test.lisp` (extend `:import-from` with
`#:validate-versioned-record`, `#:field-availability`):

```lisp
(defparameter *v1-metadata*
  '(:schema-version 1 :record-kind :result :entity-kind :function-spec
    :definition-digest "abc" :definition-digest-complete t
    :definition-digest-covers :declaration-and-registered-dependencies
    :capabilities (:generation :available :shrinking :none))
  "A minimal well-formed v1 result envelope, shared by the validation cases.")

(deftest a-well-formed-v1-record-validates
  (ok (eq :ok (validate-versioned-record *v1-metadata*
                                         :expected-record-kind :result))))

(deftest a-future-schema-is-unsupported-not-malformed
  (let ((record (list* :schema-version 2 (cddr *v1-metadata*))))
    (multiple-value-bind (status reason) (validate-versioned-record record)
      (ok (eq :unsupported-schema status))
      ;; The version travels so the response can name it rather than saying
      ;; only that something was wrong.
      (ok (eql 2 reason)))))

(deftest a-v1-record-missing-required-metadata-is-malformed
  ;; schema-info declares these seven required.  Continuing with
  ;; field_availability :absent would treat a broken record as an old one.
  (let ((record (remove-from-plist-once *v1-metadata* :record-kind)))
    (ok (eq :malformed (validate-versioned-record record)))))

(deftest a-record-kind-mismatch-is-malformed
  (ok (eq :malformed
          (validate-versioned-record *v1-metadata*
                                     :expected-record-kind :definition))))

(deftest nil-and-non-plists-are-malformed
  (ok (eq :malformed (validate-versioned-record nil)))
  (ok (eq :malformed (validate-versioned-record '(:schema-version))))
  (ok (eq :malformed (validate-versioned-record '("not" "a" "plist" 1)))))

(deftest present-nil-is-not-absent
  ;; The most important regression in this file.  GETF answers NIL for both.
  (let ((present (append *v1-metadata* '(:failure-phase nil)))
        (missing *v1-metadata*))
    (ok (eq :collected (field-availability present :failure-phase)))
    (ok (eq :absent (field-availability missing :failure-phase)))))

(deftest not-collected-is-a-sentinel-only-where-the-schema-says-so
  (testing "a top-level report field uses it as an availability sentinel"
    (ok (eq :not-collected
            (field-availability '(:shrink-report :not-collected) :shrink-report))))
  (testing "a field whose :NOT-COLLECTED is data keeps it"
    ;; failure.outcome :NOT-COLLECTED means the Function Spec target was never
    ;; called, and provenance records it for an item nobody collected.  Neither
    ;; is an availability marker.
    (ok (eq :collected (field-availability '(:outcome :not-collected) :outcome)))
    (ok (eq :collected
            (field-availability '(:target-revision :not-collected)
                                :target-revision)))))
```

Add this helper to the test file too — the tests need to drop one key without
disturbing the others:

```lisp
(defun remove-from-plist-once (plist key)
  "Return PLIST without KEY and its value."
  (loop for (indicator value) on plist by #'cddr
        unless (eq indicator key)
          append (list indicator value)))
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `rove tests/spec-core-record-test.lisp`
Expected: FAIL — `VALIDATE-VERSIONED-RECORD` is not defined.

- [ ] **Step 3: Implement validation and availability**

Add to `src/spec-core-record.lisp`, exporting `#:validate-versioned-record`,
`#:field-availability`, `#:+v1-required-metadata+` and `#:+sentinel-fields+`:

```lisp
(defparameter +v1-required-metadata+
  '(:schema-version :record-kind :entity-kind :definition-digest
    :definition-digest-complete :definition-digest-covers :capabilities)
  "The metadata keys cl-spec's SCHEMA-INFO declares required for version 1.

A record claiming version 1 without one of them is broken, not old.  Reporting
it as a field that happens to be absent would let a malformed answer from the
versioned API read as an older revision -- which is the one confusion this
module's availability states exist to prevent.")

(defparameter +sentinel-fields+
  '(:shrink-report :generation-report :case-report
    :digest-omissions :digest-exclusions)
  "The fields whose :NOT-COLLECTED value means availability rather than data.

:NOT-COLLECTED is not a sentinel wherever it appears.  On an observation's
:OUTCOME it means the Function Spec target was never called, and inside
provenance's :COLLECTION-STATES it names an item nobody collected -- both are
the answer, not the absence of one.  Converting either would delete a fact.")

(defparameter *plist-scan-limit* 4096
  "How far a plist is walked before it is refused as malformed.

A bound rather than a proper-list test, so a circular or improper answer from a
future revision is refused instead of hanging the worker.")

(defun %proper-plist-p (value)
  "Return true when VALUE is a bounded plist with keyword indicators."
  (loop with tail = value
        for count from 0 below *plist-scan-limit*
        do (cond ((null tail) (return t))
                 ((not (consp tail)) (return nil))
                 ((not (keywordp (car tail))) (return nil))
                 ((not (consp (cdr tail))) (return nil))
                 (t (setf tail (cddr tail))))
        finally (return nil)))

(defun field-availability (record key)
  "Return :COLLECTED, :NOT-COLLECTED or :ABSENT for KEY in RECORD.

GET-PROPERTIES rather than GETF, and that is the whole point: GETF answers NIL
both for a key that is not there and for a key whose value is NIL, and
:FAILURE-PHASE NIL is a measurement -- an ordinary target observation with no
special phase -- not an absence.

:NOT-COLLECTED counts as availability only for +SENTINEL-FIELDS+; anywhere else
it is the value cl-spec meant to give."
  (multiple-value-bind (indicator value tail) (get-properties record (list key))
    (declare (ignore indicator))
    (cond ((null tail) :absent)
          ((and (eq :not-collected value) (member key +sentinel-fields+))
           :not-collected)
          (t :collected))))

(defun validate-versioned-record (record &key expected-record-kind
                                              expected-entity-kind)
  "Return (values STATUS REASON) for one versioned record.

STATUS is :OK, :UNSUPPORTED-SCHEMA when the record declares a version this
adapter does not know, or :MALFORMED.  REASON is the declared version for
:UNSUPPORTED-SCHEMA and a sentence for :MALFORMED.

A malformed answer from a versioned API is reported rather than quietly
replaced by an older reader: falling back would hide a signature mismatch
behind a response that looked fine.  The same judgement %DESCRIBE-FUNCTION-SPEC
already makes when FUNCTION-SPEC-DATA answers NIL.

EXPECTED-RECORD-KIND and EXPECTED-ENTITY-KIND are checked when supplied --
RESULT-DATA answers :RESULT and FUNCTION-SPEC-DATA answers :DEFINITION with
:FUNCTION-SPEC -- so a record projected under the wrong reader is caught here
rather than by whatever reads it next."
  (flet ((bad (reason) (return-from validate-versioned-record
                         (values :malformed reason))))
    (unless record (bad "the versioned reader returned NIL"))
    (unless (%proper-plist-p record)
      (bad "the versioned reader returned something that is not a plist"))
    (when (eq :absent (field-availability record :schema-version))
      (bad "the record carries no :schema-version"))
    (let ((version (getf record :schema-version)))
      (unless (eql 1 version)
        (return-from validate-versioned-record
          (values :unsupported-schema version)))
      (dolist (key +v1-required-metadata+)
        (when (eq :absent (field-availability record key))
          (bad (format nil "a version 1 record is missing required metadata ~A"
                       (%json-key key)))))
      (when (and expected-record-kind
                 (not (eq expected-record-kind (getf record :record-kind))))
        (bad (format nil "expected a ~A record and got ~A"
                     (%json-key expected-record-kind)
                     (%json-key (getf record :record-kind)))))
      (when (and expected-entity-kind
                 (not (eq expected-entity-kind (getf record :entity-kind))))
        (bad (format nil "expected a ~A record and got ~A"
                     (%json-key expected-entity-kind)
                     (%json-key (getf record :entity-kind)))))
      :ok)))
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `rove tests/spec-core-record-test.lisp`
Expected: PASS.

- [ ] **Step 5: Lint and commit**

```bash
mallet src/spec-core-record.lisp tests/spec-core-record-test.lisp
git add src/spec-core-record.lisp tests/spec-core-record-test.lisp
git commit -m "feat(spec): validate a versioned record before reading it

A record claiming version 1 without one of schema-info's seven required keys
is broken rather than old, and a versioned reader that answers NIL or a
non-plist is a fault to report rather than a reason to fall back to the legacy
reader -- falling back would hide a signature mismatch behind a response that
looked fine.

Availability is decided with GET-PROPERTIES.  GETF answers NIL for a key that
is absent and for a key whose value is NIL, and :FAILURE-PHASE NIL is a
measurement: an ordinary target observation with no special phase.  And
:NOT-COLLECTED counts as availability only where the schema uses it that way,
never on an observation's :OUTCOME, where it says the target was not called."
```

---

## Task 4: The cl-spec record shapes, and one entry point

§6.2.2's table becomes data. Every shape below was read off cl-spec
`4f149e1` — the sources are named so a later revision can be diffed against
them rather than guessed at.

| Shape | cl-spec source |
|---|---|
| error datum | `src/explain.lisp:82` — `(list* :kind kind :path (reverse path) :actual value extra)` |
| explain-data root | `src/explain.lisp:632` — `(:valid B :spec S :value V :path () :errors (...))` |
| expected descriptor | `src/explain.lisp:102-170` |
| field expectation | `src/explain.lisp:145` — `(:key K :required B :expected D)` |
| observation | `src/property-runner.lisp` `OBSERVATION-DATA` |
| target outcome | `src/execution.lisp:300` `OBSERVED-OUTCOME-DATA` |
| state evidence | `src/function-spec.lisp` `CAPTURE-EVIDENCE` / `STATE-POST-EVIDENCE` |
| case report | `src/function-spec.lisp` `CASE-RUN-REPORT` |
| generation report | `src/generation-request.lisp:101` |
| shrink report | `src/backends/check-it.lisp` — `(:candidates :budget :termination)` |
| provenance | `src/property-runner.lisp` `CAPTURE-RUN-PROVENANCE` |

**Files:**
- Modify: `src/spec-core-record.lisp`
- Modify: `tests/spec-core-record-test.lisp`

**Interfaces:**
- Consumes: Tasks 1–3.
- Produces:
  - `*record-shapes*` populated with `:expected`, `:field-expectation`,
    `:error-datum`, `:explanation`, `:target-outcome`, `:observation`,
    `:state`, `:case-report`, `:generation-report`, `:shrink-report`,
    `:provenance`, `:capabilities`, `:digest-omission`, `:result-data`,
    `:function-spec-data`.
  - `(project-core-record record shape-name &key expected-record-kind
     expected-entity-kind)` → `(values REPORT STATUS REASON)`. STATUS is `:ok`,
    `:unsupported-schema` or `:malformed`. REPORT is the plist
    `(:availability :collected :schema-supported B :schema-version N
      :field-availability (KEY AVAIL ...) :unknown-keys (STRING ...)
      :projection (:complete B :issues (...)) :data NODE)`.
  - A new `:signature` descriptor clause in `walk` (Task 2's function).

- [ ] **Step 1: Write the failing tests**

Append to `tests/spec-core-record-test.lisp` (extend `:import-from` with
`#:project-core-record`):

```lisp
(defparameter *passing-result*
  (append *v1-metadata*
          '(:name cl-user::widen :status :passed :trials 2 :budget 2
            :rejected 0 :seed 4611686018427387903 :profile :normal
            :options nil :counterexample nil :shrunk-counterexample nil
            :shrunk-outcome nil :shrink-report :not-collected
            :generation-report (:scope :request :termination :completed
                                :attempts 0 :rejections 0)
            :failure-phase nil :failure-reason nil
            :case-report (:selection :exclusive :unit :normal-trials
                          :declared-cases (:success :insufficient)
                          :cases ((:name :success :documentation nil
                                   :called 2 :passed 2 :failed 0 :error 0)
                                  (:name :insufficient :documentation nil
                                   :called 0 :passed 0 :failed 0 :error 0))
                          :case-selection-errors 0 :capture-errors 0
                          :never-called (:insufficient))
            :failure nil :shrunk-failure nil :elapsed 0.005))
  "A measured v1 result whose second case was never reached.")

(defun field-of (node key)
  "Return the child NODE holds under the JSON key KEY."
  (cdr (assoc key (second node) :test #'equal)))

(deftest a-result-record-projects-its-whole-envelope
  (multiple-value-bind (report status) (project-core-record *passing-result*
                                                            :result-data)
    (ok (eq :ok status))
    (ok (eq :collected (getf report :availability)))
    (ok (getf report :schema-supported))
    (testing "the envelope keys are in data, not only in the core_schema alias"
      (let ((data (getf report :data)))
        (ok (equal '(:scalar 1) (field-of data "schema_version")))
        (ok (equal '(:scalar "result") (field-of data "record_kind")))
        (ok (equal '(:scalar "abc") (field-of data "definition_digest")))))
    (testing "a seed is text even here"
      (ok (equal '(:scalar "4611686018427387903")
                 (field-of (getf report :data) "seed"))))
    (testing "the never-called case survives as a word"
      (let* ((report-node (field-of (getf report :data) "case_report"))
             (never (field-of report-node "never_called")))
        (ok (equal '((:scalar "insufficient")) (second never)))))
    (testing "an uncollected report is availability, not a projected value"
      (ok (eq :not-collected
              (getf (getf report :field-availability) :shrink-report)))
      (ok (eq :collected
              (getf (getf report :field-availability) :generation-report))))
    (testing "a present NIL is collected"
      (ok (eq :collected
              (getf (getf report :field-availability) :failure-phase))))))

(deftest an-unsupported-schema-projects-nothing
  (multiple-value-bind (report status)
      (project-core-record (list* :schema-version 2 (cddr *passing-result*))
                           :result-data)
    (ok (eq :unsupported-schema status))
    (ok (not (getf report :schema-supported)))
    (ok (eql 2 (getf report :schema-version)))
    ;; No v1 field rules may run over a record this adapter cannot read.
    (ok (null (getf report :data)))))

(deftest a-malformed-record-is-not-projected-at-all
  (multiple-value-bind (report status reason)
      (project-core-record '(:schema-version 1 :record-kind :result) :result-data)
    (ok (null report))
    (ok (eq :malformed status))
    (ok (search "required metadata" reason))))

(deftest a-target-outcome-keeps-its-kind-and-values
  (let* ((observation '(:arguments (2 10) :status :failed :reason :missing-condition
                        :signature (:missing-condition) :explanation nil
                        :outcome (:kind :returned :values (0)) :value 0
                        :case :insufficient :condition-report nil))
         (node (project-record observation '(:ref :observation)))
         (outcome (field-of node "outcome")))
    (ok (equal '(:scalar "returned") (field-of outcome "kind")))
    (ok (eq :array (first (field-of outcome "values"))))))

(deftest a-signature-keeps-its-shapes-inside-an-array
  (let ((node (project-record '(:return-value :return-spec ((:kind :range-failed)))
                              :signature)))
    (ok (eq :array (first node)))
    (ok (equal '(:scalar "return-value") (first (second node))))
    (testing "the trailing failure shapes are objects, not flattened words"
      (let ((shapes (third (second node))))
        (ok (eq :array (first shapes)))
        (ok (equal '(:scalar "range-failed")
                   (field-of (first (second shapes)) "kind")))))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `rove tests/spec-core-record-test.lisp`
Expected: FAIL — `PROJECT-CORE-RECORD` is not defined.

- [ ] **Step 3: Add the `:signature` clause to `walk`**

`signature` is an array whose leading keyword is a tag rather than a key, so
reading it as an object would invent a relation (§6.2.5). Two of
`FAILURE-SIGNATURE`'s forms end in a list of failure shapes, and those must not
flatten. Use `lisp-patch-form` on `project-record` to add one clause to `walk`,
immediately before the `((not (consp descriptor)) ...)` clause:

```lisp
               ((eq :signature descriptor)
                ;; FAILURE-SIGNATURE builds (:RETURN-VALUE :RETURN-SPEC shapes)
                ;; and (:CONDITION-SPEC type shapes); every other form is a flat
                ;; run of tags.  Only the trailing shapes of those two are
                ;; structured, and the leading keyword stays a tag, never a key.
                (let ((shaped (and (consp value)
                                   (member (first value)
                                           '(:return-value :condition-spec))
                                   (= 3 (length value)))))
                  (list :array
                        (if shaped
                            (list (project-value (first value))
                                  (project-value (second value))
                                  (walk (third value)
                                        '(:array (:ref :error-datum))
                                        (cons 2 path) (1+ depth)))
                            (walk-list value :leaf path depth)))))
```

- [ ] **Step 4: Define the shapes**

Add to `src/spec-core-record.lisp`. `SETF` on `*record-shapes*` rather than a
literal `DEFVAR` value, because `:expected` and `:error-datum` refer to
themselves:

```lisp
(setf *record-shapes*
      (list
       ;; An EXPECTED descriptor is what cl-spec says the spec required; it is
       ;; spec-derived and stays structured.  :TYPE holds a type specifier,
       ;; which is a form and reaches EXTERNALIZE-VALUE through :LEAF.
       :expected
       '(:object (:kind . :leaf) (:type . :leaf) (:satisfies . :leaf)
                 (:closed . :leaf) (:test . :leaf) (:class . :leaf)
                 (:tag-reader . :leaf)
                 (:fields . (:array (:ref :field-expectation)))
                 (:branches . (:array (:object (:name . :leaf)
                                               (:expected . (:ref :expected))))))
       :field-expectation
       ;; :KEY is a key name out of the value under test, so it is value-derived
       ;; -- cl-spec's own *FAILURE-SHAPE-KEYS* docstring classifies it that way.
       '(:object (:key . :opaque) (:required . :leaf)
                 (:expected . (:ref :expected)))
       ;; One EXPLAIN-DATA error datum.  :ACTUAL and :ACTUAL-LENGTH come off the
       ;; value, :EXPECTED and the bounds off the spec, and the three container
       ;; keys hold more error datums -- which is cl-spec's own classification
       ;; in *FAILURE-SHAPE-KEYS* and *FAILURE-SHAPE-CONTAINERS*.
       :error-datum
       '(:object (:kind . :leaf) (:path . (:array :leaf))
                 (:tuple-path . (:array :leaf)) (:field-path . (:array :leaf))
                 (:actual . :opaque) (:actual-test . :opaque)
                 (:key . :opaque) (:expected . (:ref :expected))
                 (:violated-bound . :leaf) (:predicate . :leaf)
                 (:condition-type . :leaf) (:condition-report . :leaf)
                 (:expected-length . :leaf) (:minimum-length . :leaf)
                 (:maximum-length . :leaf) (:actual-length . :leaf)
                 (:status . :leaf) (:branch . :leaf)
                 (:branch-path . (:array :leaf)) (:known-tags . :word-list)
                 (:errors . (:array (:ref :error-datum)))
                 (:branches . (:array (:ref :error-datum)))
                 (:conjuncts . (:array (:ref :error-datum))))
       ;; The union of every explanation cl-spec builds: an EXPLAIN-DATA root
       ;; for a spec violation, and the :KIND plists a case-selection error, a
       ;; capture error and a state-post violation record.
       :explanation
       '(:object (:valid . :leaf) (:spec . :leaf) (:value . :opaque)
                 (:path . (:array :leaf))
                 (:errors . (:array (:ref :error-datum)))
                 (:kind . :leaf) (:case-error . :leaf) (:function . :leaf)
                 (:cases . :word-list) (:case . :leaf) (:index . :leaf)
                 (:form . :leaf) (:binding . :leaf) (:captured . (:alist :opaque))
                 (:condition-type . :leaf) (:condition-report . :leaf))
       :target-outcome
       '(:object (:kind . :leaf) (:values . (:array :opaque))
                 (:condition-type . :leaf) (:condition-report . :leaf))
       :capture-evidence
       '(:object (:status . :leaf) (:declared . (:array :leaf))
                 ;; Measured as ((NAME . VALUE) ...); dotted pairs are not
                 ;; proper lists, so they get their own descriptor rather than
                 ;; an array rule that has nothing to say about them.
                 (:values . (:alist :opaque))
                 (:error . (:object (:binding . :leaf) (:index . :leaf)
                                    (:condition-type . :leaf))))
       :state
       '(:object (:capture . (:ref :capture-evidence))
                 (:state-post . (:object (:status . :leaf) (:reason . :leaf)
                                         (:case . :leaf) (:index . :leaf)
                                         (:form . :leaf)
                                         (:condition-type . :leaf))))
       :observation
       '(:object (:arguments . (:array :opaque)) (:status . :leaf)
                 (:reason . :leaf) (:signature . :signature)
                 (:explanation . (:ref :explanation))
                 (:outcome . (:ref :target-outcome)) (:value . :opaque)
                 (:case . :leaf) (:condition-report . :leaf)
                 (:state . (:ref :state)))
       :case-report
       '(:object (:selection . :leaf) (:unit . :leaf)
                 (:declared-cases . :word-list)
                 (:cases . (:array (:object (:name . :leaf)
                                            (:documentation . :leaf)
                                            (:called . :leaf) (:passed . :leaf)
                                            (:failed . :leaf) (:error . :leaf))))
                 (:case-selection-errors . :leaf) (:capture-errors . :leaf)
                 (:never-called . :word-list))
       :generation-report
       '(:object (:scope . :leaf) (:unit . :leaf) (:policy . :leaf)
                 (:budget . :leaf) (:budget-source . :leaf)
                 (:default-coefficient . :leaf) (:requested-values . :leaf)
                 (:generated-values . :leaf) (:attempts . :leaf)
                 (:rejections . :leaf)
                 (:phases . (:object
                             (:generation . (:object (:attempts . :leaf)
                                                     (:rejections . :leaf)))
                             (:shrinking . (:object (:attempts . :leaf)
                                                    (:rejections . :leaf)))))
                 (:termination . :leaf) (:exhaustion-phase . :leaf)
                 (:exhausted-at . :leaf))
       :shrink-report
       '(:object (:candidates . :leaf) (:budget . :leaf) (:termination . :leaf))
       :provenance
       '(:object (:backend . :leaf) (:lisp-implementation-type . :leaf)
                 (:lisp-implementation-version . :leaf)
                 (:cl-spec-version . :leaf) (:target-revision . :leaf)
                 (:collection-states
                  . (:object (:backend . :leaf)
                             (:lisp-implementation-type . :leaf)
                             (:lisp-implementation-version . :leaf)
                             (:cl-spec-version . :leaf)
                             (:target-revision . :leaf))))
       :capabilities
       '(:object (:generation . :leaf) (:shrinking . :leaf)
                 (:instrumentation . :leaf))
       :digest-omission
       '(:object (:kind . :leaf) (:path . (:array :leaf)) (:target . :leaf)
                 (:reason . :leaf))
       :counterexample
       ;; cl-spec names the arguments before it stores them, so this is a
       ;; {variable value} plist rather than a raw argument list.
       '(:alist :opaque)
       :result-data
       '(:object (:schema-version . :leaf) (:record-kind . :leaf)
                 (:entity-kind . :leaf) (:definition-digest . :leaf)
                 (:definition-digest-complete . :leaf)
                 (:definition-digest-covers . :leaf)
                 (:digest-omissions . (:array (:ref :digest-omission)))
                 (:digest-exclusions . :word-list)
                 (:capabilities . (:ref :capabilities))
                 (:state-constraints . :leaf)
                 (:name . :leaf) (:status . :leaf) (:trials . :leaf)
                 (:budget . :leaf) (:rejected . :leaf) (:seed . :leaf)
                 (:profile . :leaf)
                 ;; cl-spec v1 does not publish the shape of caller options.
                 (:options . :opaque)
                 (:provenance . (:ref :provenance))
                 (:counterexample . (:ref :counterexample))
                 (:shrunk-counterexample . (:ref :counterexample))
                 (:shrunk-outcome . :leaf)
                 (:shrink-report . (:ref :shrink-report))
                 (:generation-report . (:ref :generation-report))
                 (:failure-phase . :leaf) (:failure-reason . :leaf)
                 (:case-report . (:ref :case-report))
                 (:failure . (:ref :observation))
                 (:shrunk-failure . (:ref :observation))
                 (:elapsed . :leaf))
       :function-spec-data
       '(:object (:schema-version . :leaf) (:record-kind . :leaf)
                 (:entity-kind . :leaf) (:definition-digest . :leaf)
                 (:definition-digest-complete . :leaf)
                 (:definition-digest-covers . :leaf)
                 (:digest-omissions . (:array (:ref :digest-omission)))
                 (:digest-exclusions . :word-list)
                 (:capabilities . (:ref :capabilities))
                 (:state-constraints . :leaf)
                 (:name . :leaf) (:kind . :leaf) (:documentation . :leaf)
                 (:argument-generator . :leaf)
                 (:preconditions . (:array :leaf))
                 (:postconditions . (:array :leaf))
                 (:post-value-variables . (:array :leaf))
                 (:capture . (:array (:object (:name . :leaf) (:form . :leaf))))
                 (:state-post . (:array :leaf))
                 (:case-selection . :leaf)
                 (:source-form . :leaf)
                 (:metadata . :opaque))))
```

`:arguments`, `:returns`, `:signals`, `:argument-schema` and `:cases` are left
out of `:function-spec-data` on purpose: Task 5 renders those through the
existing `%SPEC-TREE`, which already projects an IR node and is what
`spec-describe` has always published.

- [ ] **Step 5: Implement the entry point**

```lisp
(defun project-core-record (record shape-name &key expected-record-kind
                                                   expected-entity-kind)
  "Return (values REPORT STATUS REASON) for one versioned cl-spec RECORD.

STATUS is :OK, :UNSUPPORTED-SCHEMA or :MALFORMED.  A malformed record yields no
REPORT: the caller reports it as an adapter-visible fault rather than falling
back to a legacy reader, which would hide it.

REPORT separates what cl-mcp knows about the transport from what cl-spec said.
:DATA is the record and carries no key cl-mcp added; :AVAILABILITY,
:SCHEMA-SUPPORTED, :FIELD-AVAILABILITY, :UNKNOWN-KEYS and :PROJECTION are the
transport metadata, and they live outside it.  A truncated projection is
visible there rather than silently shorter inside :DATA."
  (multiple-value-bind (status reason)
      (validate-versioned-record record
                                 :expected-record-kind expected-record-kind
                                 :expected-entity-kind expected-entity-kind)
    (case status
      (:malformed (values nil :malformed reason))
      (:unsupported-schema
       (values (list :availability :collected
                     :schema-supported nil
                     :schema-version reason
                     :field-availability nil
                     :unknown-keys nil
                     :source nil
                     :projection (list :complete t :issues nil)
                     :data nil)
               :unsupported-schema
               reason))
      (t
       (multiple-value-bind (node issues unknown)
           (project-record record (list :ref shape-name))
         (values (list :availability :collected
                       :schema-supported t
                       :schema-version 1
                       ;; The record as cl-spec gave it, kept for the adapter's
                       ;; own reading.  The verdict logic asks questions like
                       ;; "was any declared case never reached", and answering
                       ;; them off :DATA would mean re-parsing projected JSON
                       ;; nodes to recover keywords this already has.  Never
                       ;; rendered: :DATA is what reaches the client.
                       :source record
                       :field-availability
                       (loop for (key . nil) in (rest (%resolve-descriptor
                                                       (list :ref shape-name)))
                             append (list key (field-availability record key)))
                       :unknown-keys unknown
                       :projection (list :complete (null issues) :issues issues)
                       :data node)
                 :ok
                 nil))))))
```

- [ ] **Step 6: Run the tests to verify they pass**

Run: `rove tests/spec-core-record-test.lisp`
Expected: PASS.

- [ ] **Step 7: Check parens, lint, commit**

```bash
mallet src/spec-core-record.lisp tests/spec-core-record-test.lisp
git add src/spec-core-record.lisp tests/spec-core-record-test.lisp
git commit -m "feat(spec): the cl-spec record shapes, as data

Every descriptor here was read off cl-spec 4f149e1 and the sources are named
in the plan, so a later revision can be diffed against them rather than guessed
at.  The classification of which error-datum keys come from the spec and which
from the value is cl-spec's own, out of *FAILURE-SHAPE-KEYS*.

project-core-record keeps the two halves apart: data is the record, and
availability, schema support, unknown keys and the projection's own losses sit
outside it.  A record this adapter cannot read projects nothing rather than
having version 1's rules applied to it, and a malformed one yields no report at
all."
```

---

## Task 5: `spec-describe kind=function-spec` keeps the whole contract

Eleven things `FUNCTION-SPEC-DATA` carries never cross the boundary today.
`%DESCRIBE-FUNCTION-SPEC` projects the arguments' specs, `:returns`, `:pre` and
`:post`, and drops the argument kinds, the supplied-p and keyword names, the
argument generator and schema, `:signals`, `:post-value-variables`, `:capture`,
`:state-post`, `:case-selection` and the ordered `:cases`.

**Files:**
- Modify: `src/spec-adapter-report.lisp` — `%spec-tree` (~line 798),
  `%describe-function-spec` (~line 847), `describe-report` (~line 982)
- Modify: `tests/spec-adapter-report-test.lisp`

**Interfaces:**
- Consumes: `project-core-record`, `validate-versioned-record` (Tasks 3–4).
- Produces: `%describe-function-spec` returns, additionally:
  `:arguments` entries gain `:kind`, `:supplied-p`, `:keyword`;
  `:argument-generator`, `:argument-schema`, `:signals`,
  `:post-value-variables`, `:capture`, `:state-post`, `:case-selection`,
  `:cases`, and `:core-record` (the Task 4 report for the same plist).

- [ ] **Step 1: Write the failing tests**

Add to `tests/spec-adapter-report-test.lisp`. The stub API returns a contract
plist shaped like the measured one (§4):

```lisp
(defparameter *cases-contract*
  '(:schema-version 1 :record-kind :definition :entity-kind :function-spec
    :definition-digest "fnv1a64-v1:0cbb" :definition-digest-complete t
    :definition-digest-covers :declaration-and-registered-dependencies
    :digest-omissions nil :digest-exclusions (:target-implementation)
    :capabilities (:generation :available :shrinking :none
                   :instrumentation :unavailable)
    :case-selection :exclusive
    :cases ((:name :sufficient-funds
             :documentation "The amount fits."
             :when (<= amount balance) :outcome :returns
             :returns (:kind :range :base-type integer :min 0 :max :unbounded)
             :signals nil :postconditions ((= result (- balance amount))))
            (:name :insufficient-funds
             :documentation "The amount does not fit."
             :when (> amount balance) :outcome :signals :returns nil
             :signals (:kind :type :type insufficient-funds)
             :postconditions nil))
    :capture ((:name balance-before :form (account-balance account)))
    :state-post ((= (account-balance account) (- balance-before amount)))
    :name cl-mcp-spec-report-fixture::add :kind :function-spec
    :documentation "Withdraw."
    :arguments ((:variable cl-mcp-spec-report-fixture::a
                 :spec (:kind :range :base-type integer :min 0 :max 1000))
                (:variable cl-mcp-spec-report-fixture::b
                 :spec (:kind :type :type integer)
                 :kind :key :supplied-p cl-mcp-spec-report-fixture::b-p
                 :keyword :b))
    :argument-generator cl-mcp-spec-report-fixture::scripted
    :argument-schema (:kind :tuple :generator cl-mcp-spec-report-fixture::scripted
                      :children ((:kind :type :type integer)))
    :preconditions nil :returns nil :signals nil :postconditions nil
    :post-value-variables (cl-mcp-spec-report-fixture::result)
    :source-form (defspec-function add)
    :source-location (:file "/tmp/a.lisp" :package "CL-MCP-SPEC-REPORT-FIXTURE")
    :metadata nil)
  "A contract carrying every feature the describe path used to drop.")

(deftest describe-keeps-argument-kinds-and-generators
  (let* ((api (%stub-api :function-spec-data
                         (lambda (name &key registry)
                           (declare (ignore name registry))
                           *cases-contract*)))
         (report (describe-report api :ok "function-spec" "ADD"
                                  :package "CL-MCP-SPEC-REPORT-FIXTURE"))
         (arguments (getf report :arguments)))
    (ok (eq :ok (getf report :status)))
    (testing "a required argument's absent :kind means required, not unknown"
      (ok (eq :required (getf (first arguments) :kind)))
      (ok (null (getf (first arguments) :supplied-p))))
    (testing "a keyword argument keeps its kind, supplied-p and keyword"
      (ok (eq :key (getf (second arguments) :kind)))
      (ok (equal "B-P" (getf (getf (second arguments) :supplied-p) :name)))
      (ok (eq :b (getf (second arguments) :keyword))))
    (testing "the argument generator and schema survive"
      (ok (equal "SCRIPTED" (getf (getf report :argument-generator) :name)))
      (ok (getf report :argument-schema))
      ;; %SPEC-TREE used to drop :GENERATOR, which is where a custom
      ;; whole-argument generator is recorded.
      (ok (equal "SCRIPTED"
                 (getf (getf (getf report :argument-schema) :generator) :name))))))

(deftest describe-keeps-cases-capture-and-state-post
  (let* ((api (%stub-api :function-spec-data
                         (lambda (name &key registry)
                           (declare (ignore name registry))
                           *cases-contract*)))
         (report (describe-report api :ok "function-spec" "ADD"
                                  :package "CL-MCP-SPEC-REPORT-FIXTURE"))
         (cases (getf report :cases)))
    (ok (eq :exclusive (getf report :case-selection)))
    (ok (= 2 (length cases)))
    (testing "cases keep their order, guard, outcome and postconditions"
      (ok (eq :sufficient-funds (getf (first cases) :name)))
      (ok (search "<=" (getf (first cases) :guard)))
      (ok (eq :returns (getf (first cases) :outcome)))
      (ok (getf (first cases) :returns))
      (ok (search "=" (getf (first cases) :postconditions))))
    (testing "the signalling case keeps its condition spec"
      (ok (eq :signals (getf (second cases) :outcome)))
      (ok (getf (second cases) :signals)))
    (testing "capture and state-post are declarations, printed not run"
      (ok (equal "BALANCE-BEFORE"
                 (getf (getf (first (getf report :capture)) :name) :name)))
      (ok (search "account-balance" (getf (first (getf report :capture)) :form)))
      (ok (search "account-balance" (getf report :state-post))))
    (testing "post-value variables survive"
      (ok (equal "RESULT"
                 (getf (first (getf report :post-value-variables)) :name))))))

(deftest describe-refuses-a-contract-schema-it-cannot-read
  (let* ((api (%stub-api :function-spec-data
                         (lambda (name &key registry)
                           (declare (ignore name registry))
                           (list* :schema-version 2 (cddr *cases-contract*)))))
         (report (describe-report api :ok "function-spec" "ADD"
                                  :package "CL-MCP-SPEC-REPORT-FIXTURE")))
    (ok (eq :unsupported (getf report :status)))
    (ok (search "schema version 2" (getf report :message)))
    ;; No v1 rule may run over it -- including "an absent :kind means required".
    (ok (null (getf report :arguments)))))
```

- [ ] **Step 2: Run to verify they fail**

Run: `rove tests/spec-adapter-report-test.lisp`
Expected: FAIL — `:kind`, `:cases`, `:capture` are absent from the report.

- [ ] **Step 3: Teach `%spec-tree` about `:generator`**

Use `lisp-patch-form` on `%spec-tree` in `src/spec-adapter-report.lisp`,
inserting after the `:name` entry:

```lisp
          ;; Where a custom whole-argument generator is recorded.  Measured:
          ;; an :ARGUMENT-SCHEMA tuple node comes back as
          ;; (:KIND :TUPLE :GENERATOR SCRIPTED-ARGUMENTS ...), and dropping
          ;; this key lost the only statement that the inputs are scripted
          ;; rather than drawn.
          :generator (let ((generator (getf spec-plist :generator)))
                       (when generator (symbol-data generator)))
```

- [ ] **Step 4: Rewrite `%describe-function-spec`**

Replace the function with `lisp-edit-form`. Keep the existing `clause`,
`%print-bounded-form` and digest handling; add the schema gate and the new
fields. The full replacement:

```lisp
(defun %describe-function-spec (api name registry max-chars)
  "Return the detail plist for the contract registered for NAME.

Built from cl-spec's own FUNCTION-SPEC-DATA.  The two halves a caller asks
about before editing a function -- which inputs are accepted, which output is
required -- are cl-spec's answer to give, not this adapter's to assemble.

Nothing here runs contract code.  FUNCTION-SPEC-DATA projects no compiled
guard, capture form, post form or state-post form, so describing a contract
cannot call the target, its :PRE, its captures, its case guards or any of its
post forms.

A record whose schema version this adapter does not know is refused rather than
read under version 1's rules -- an absent :KIND means :REQUIRED in version 1
and may mean anything in a later one."
  (let ((data (funcall (api-fn api :function-spec-data) name :registry registry)))
    (multiple-value-bind (status reason)
        (validate-versioned-record data :expected-record-kind :definition
                                        :expected-entity-kind :function-spec)
      (when (eq :unsupported-schema status)
        (return-from %describe-function-spec
          (list :status :unsupported :kind "function-spec" :name (symbol-data name)
                :message (format nil "cl-spec returned this contract under ~
schema version ~A, which this cl-mcp does not know. Its fields are not read ~
under version 1's rules, because an absent key means different things between ~
versions." reason))))
      (when (eq :malformed status)
        (return-from %describe-function-spec
          (list :status :unsupported :kind "function-spec" :name (symbol-data name)
                :message (format nil "cl-spec returned a contract projection ~
this adapter cannot read: ~A. An empty description would read as a contract ~
with no arguments and no :returns." reason)))))
    (flet ((clause (forms)
             (when forms
               (multiple-value-bind (text complete omitted)
                   (%print-bounded-form (if (null (rest forms))
                                            (first forms)
                                            (cons 'and forms))
                                        max-chars)
                 (list text complete omitted))))
           (form-text (form)
             (when form (%print-bounded-form form max-chars))))
      (let ((pre (clause (getf data :preconditions)))
            (post (clause (getf data :postconditions))))
        (multiple-value-bind (source source-complete source-omitted)
            (%print-bounded-form (getf data :source-form) max-chars)
          (multiple-value-bind (digest complete)
              (definition-digest api name registry :property data)
            (list :core-schema (core-schema-data data)
                  :core-record (project-core-record
                                data :function-spec-data
                                :expected-record-kind :definition
                                :expected-entity-kind :function-spec)
                  :status :ok
                  :kind "function-spec"
                  :name (symbol-data name)
                  :documentation (getf data :documentation)
                  :arguments (%contract-arguments (getf data :arguments))
                  :argument-generator
                  (let ((generator (getf data :argument-generator)))
                    (when generator (symbol-data generator)))
                  :argument-schema (%spec-tree (getf data :argument-schema))
                  :returns (%spec-tree (getf data :returns))
                  :signals (%spec-tree (getf data :signals))
                  :post-value-variables
                  (mapcar #'symbol-data (getf data :post-value-variables))
                  :capture (%contract-capture (getf data :capture) max-chars)
                  :state-post (form-text (getf data :state-post))
                  :case-selection (getf data :case-selection)
                  :cases (%contract-cases (getf data :cases) max-chars)
                  :preconditions (first pre)
                  :preconditions-complete (if pre (second pre) :not-applicable)
                  :preconditions-omitted-chars (third pre)
                  :postconditions (first post)
                  :postconditions-complete (if post (second post) :not-applicable)
                  :postconditions-omitted-chars (third post)
                  :source-form source
                  :source-form-complete source-complete
                  :source-form-omitted-chars source-omitted
                  :source-location (getf data :source-location)
                  :definition-digest digest
                  :definition-digest-complete complete
                  :definition-digest-covers :contract)))))))
```

- [ ] **Step 5: Add the three helpers**

Insert before `%describe-function-spec` with `lisp-edit-form`:

```lisp
(defun %contract-arguments (arguments)
  "Return one contract argument list as report plists.

An absent :KIND means :REQUIRED and is normalized to it rather than published
as null.  FUNCTION-SPEC-DATA omits the key for a required argument -- measured,
and its source reads (unless (eq :required (argument-binding-kind binding)))
-- so null here would leave a reader unable to tell a required argument from a
revision that does not report kinds at all.  This normalization is a statement
about version 1 only; the caller refuses any other version before reaching it."
  (loop for argument in arguments
        collect (list :variable (symbol-data (getf argument :variable))
                      :spec (%spec-tree (getf argument :spec))
                      :kind (or (getf argument :kind) :required)
                      :supplied-p (let ((name (getf argument :supplied-p)))
                                    (when name (symbol-data name)))
                      :keyword (getf argument :keyword))))

(defun %contract-capture (bindings max-chars)
  "Return a contract's :CAPTURE declarations, bounded.

These are the forms as written, never the values a run observed: projecting
this list runs no capture form."
  (loop for binding in bindings
        collect (multiple-value-bind (text complete omitted)
                    (%print-bounded-form (getf binding :form) max-chars)
                  (list :name (symbol-data (getf binding :name))
                        :form text
                        :form-complete complete
                        :form-omitted-chars omitted))))

(defun %contract-cases (cases max-chars)
  "Return a contract's ordered named cases as report plists.

Order is the contract's, because case selection is exclusive and the author
wrote them in the order they are tried.  Guards and postconditions are the
source forms; their compiled counterparts are not projected and are not run."
  (loop for case in cases
        collect
        (multiple-value-bind (guard guard-complete guard-omitted)
            (%print-bounded-form (getf case :when) max-chars)
          (let ((post (getf case :postconditions)))
            (multiple-value-bind (post-text post-complete post-omitted)
                (if post
                    (%print-bounded-form (if (null (rest post))
                                             (first post)
                                             (cons 'and post))
                                         max-chars)
                    (values nil :not-applicable nil))
              (list :name (getf case :name)
                    :documentation (getf case :documentation)
                    :guard guard
                    :guard-complete guard-complete
                    :guard-omitted-chars guard-omitted
                    :outcome (getf case :outcome)
                    :returns (%spec-tree (getf case :returns))
                    :signals (%spec-tree (getf case :signals))
                    :postconditions post-text
                    :postconditions-complete post-complete
                    :postconditions-omitted-chars post-omitted
                    :post-value-variables
                    (mapcar #'symbol-data (getf case :post-value-variables))
                    :state-post
                    (let ((forms (getf case :state-post)))
                      (when forms (%print-bounded-form forms max-chars)))))))))
```

Add `#:project-core-record` and `#:validate-versioned-record` to
`spec-adapter-report`'s `:import-from` for `#:cl-mcp/src/spec-core-record`.

- [ ] **Step 6: Run tests, check parens, lint, commit**

```bash
rove tests/spec-adapter-report-test.lisp
mallet src/spec-adapter-report.lisp tests/spec-adapter-report-test.lisp
git add src/spec-adapter-report.lisp tests/spec-adapter-report-test.lisp
git commit -m "feat(spec): describe the whole Function Spec, not half of it

Eleven things the contract record carries were never reaching the caller: the
argument kinds, the supplied-p and keyword names, the argument generator and
schema, :signals, :post-value-variables, :capture, :state-post, the case
selection and the ordered cases.  A caller deciding what to edit was reading a
contract with its branches removed.

%spec-tree was also dropping :generator, which is where a custom whole-argument
generator is recorded -- so a scripted-input contract described itself as one
that draws.

An absent argument :kind is normalized to required, which is what version 1
means by omitting it, and a record under any other schema version is refused
rather than read under version 1's rules."
```

---

## Task 6: Render the contract — JSON first, then text

The JSON/object representation is primary; the text summarizes it (§4). A field
that reaches the payload alone has not reached a client, which renders only
`content[].text`.

**Files:**
- Modify: `src/tools/spec-response-builders.lisp` —
  `%spec-tree-ht` (~line 304), `%format-spec-node` (~line 321),
  `%format-describe-text` (~line 405), `build-spec-describe-response` (~line 505)
- Modify: `tests/spec-response-builders-test.lisp`

**Interfaces:**
- Consumes: Task 5's report keys.
- Produces:
  - `(%projected-ht node)` — renders a Task 2 tagged node as hash-tables,
    vectors and scalars. Used again by Task 7.
  - `(%core-record-ht report)` — renders a Task 4 report. Used again by Task 7.
  - `spec-describe` response gains `argument_generator`, `argument_schema`,
    `signals`, `post_value_variables`, `capture`, `state_post`,
    `case_selection`, `cases`, `core_record`; `arguments[]` gain `kind`,
    `supplied_p`, `keyword`.

- [ ] **Step 1: Write the failing tests**

Add to `tests/spec-response-builders-test.lisp`:

```lisp
(deftest describe-response-carries-cases-in-json-and-text
  (let* ((report
           (list :status :ok :kind "function-spec"
                 :name (%symbol-data "PROBE" "WITHDRAW")
                 :arguments (list (list :variable (%symbol-data "PROBE" "AMOUNT")
                                        :spec (list :kind :type :type "INTEGER")
                                        :kind :key
                                        :supplied-p (%symbol-data "PROBE" "AMOUNT-P")
                                        :keyword :amount))
                 :case-selection :exclusive
                 :cases (list (list :name :sufficient-funds
                                    :documentation "It fits."
                                    :guard "(<= AMOUNT BALANCE)"
                                    :guard-complete t
                                    :outcome :returns
                                    :returns (list :kind :type :type "INTEGER")
                                    :postconditions "(= RESULT 1)"
                                    :postconditions-complete t)
                              (list :name :insufficient-funds
                                    :guard "(> AMOUNT BALANCE)"
                                    :guard-complete t
                                    :outcome :signals
                                    :signals (list :kind :type
                                                   :type "INSUFFICIENT-FUNDS")
                                    :postconditions-complete :not-applicable))
                 :capture (list (list :name (%symbol-data "PROBE" "BALANCE-BEFORE")
                                      :form "(ACCOUNT-BALANCE ACCOUNT)"
                                      :form-complete t))
                 :state-post "(= (ACCOUNT-BALANCE ACCOUNT) 0)"
                 :environment *environment*))
         (response (build-spec-describe-response report))
         (text (first-text response)))
    (testing "the JSON carries the ordered cases"
      (let ((cases (gethash "cases" response)))
        (ok (= 2 (length cases)))
        (ok (equal "sufficient-funds" (gethash "name" (aref cases 0))))
        (ok (equal "returns" (gethash "outcome" (aref cases 0))))
        (ok (equal "signals" (gethash "outcome" (aref cases 1))))))
    (testing "the argument keeps its kind and keyword in JSON"
      (let ((argument (aref (gethash "arguments" response) 0)))
        (ok (equal "key" (gethash "kind" argument)))
        (ok (equal "amount" (gethash "keyword" argument)))
        (ok (equal "AMOUNT-P" (gethash "name" (gethash "supplied_p" argument))))))
    (testing "the text shows them too -- a client renders only this"
      (ok (search "cases (exclusive selection):" text))
      (ok (search "sufficient-funds" text))
      (ok (search "insufficient-funds" text))
      (ok (search "capture:" text))
      (ok (search "BALANCE-BEFORE" text))
      (ok (search "state-post:" text))
      (ok (search "&key" text)))))
```

- [ ] **Step 2: Run to verify it fails**

Run: `rove tests/spec-response-builders-test.lisp`
Expected: FAIL — `cases` is not in the response.

- [ ] **Step 3: Add the two shared renderers**

Insert into `src/tools/spec-response-builders.lisp` after `%core-schema-ht`:

```lisp
(defun %projected-ht (node)
  "Render one projection node from the record layer as JSON-ready data.

The node carries its own kind, so this never has to tell a symbol plist from an
externalized-value plist by looking for one of their keys."
  (when node
    (ecase (first node)
      (:scalar (second node))
      (:symbol (%symbol-ht (second node)))
      (:value (%value-ht (second node)))
      (:array (coerce (mapcar #'%projected-ht (second node)) 'vector))
      (:object (let ((table (make-hash-table :test #'equal)))
                 (loop for (key . child) in (second node)
                       do (setf (gethash key table) (%projected-ht child)))
                 table)))))

(defun %projection-issue-ht (issue)
  "Render one projection loss: where it happened, why, and how much was dropped."
  (make-ht "path" (coerce (mapcar #'princ-to-string (getf issue :path)) 'vector)
           "reason" (%keyword-string (getf issue :reason))
           "omitted_items" (getf issue :omitted-items)))

(defun %core-record-ht (report)
  "Render a versioned cl-spec record and what cl-mcp knows about carrying it.

DATA is the record; everything beside it is transport metadata.  Keeping them
apart is what lets a reader ask whether a short list is short or was cut."
  (when report
    (make-ht "availability" (%keyword-string (getf report :availability))
             "schema_supported" (json-bool (getf report :schema-supported))
             "schema_version" (getf report :schema-version)
             "field_availability"
             (let ((table (make-hash-table :test #'equal)))
               (loop for (key availability) on (getf report :field-availability)
                       by #'cddr
                     do (setf (gethash (substitute #\_ #\- (string-downcase
                                                            (symbol-name key)))
                                       table)
                              (%keyword-string availability)))
               table)
             "unknown_keys" (coerce (getf report :unknown-keys) 'vector)
             "projection"
             (let ((projection (getf report :projection)))
               (make-ht "complete" (json-bool (getf projection :complete))
                        "issues" (coerce (mapcar #'%projection-issue-ht
                                                 (getf projection :issues))
                                         'vector)))
             "data" (%projected-ht (getf report :data)))))
```

- [ ] **Step 4: Extend `%spec-tree-ht` and the describe response**

Add `"generator" (%symbol-ht (getf data :generator))` to `%spec-tree-ht`, and
add these keys to `build-spec-describe-response`'s `make-ht`:

```lisp
              "argument_generator" (%symbol-ht (getf report :argument-generator))
              "argument_schema" (%spec-tree-ht (getf report :argument-schema))
              "signals" (%spec-tree-ht (getf report :signals))
              "post_value_variables" (%symbol-hts
                                      (getf report :post-value-variables))
              "capture"
              (coerce (mapcar (lambda (binding)
                                (make-ht "name" (%symbol-ht (getf binding :name))
                                         "form" (sanitize-for-json
                                                 (getf binding :form))
                                         "form_complete" (%optional-bool
                                                          binding :form-complete)
                                         "form_omitted_chars"
                                         (getf binding :form-omitted-chars)))
                              (getf report :capture))
                      'vector)
              "state_post" (sanitize-for-json (getf report :state-post))
              "case_selection" (%keyword-string (getf report :case-selection))
              "cases" (coerce (mapcar #'%case-ht (getf report :cases)) 'vector)
              "core_record" (%core-record-ht (getf report :core-record))
```

and change the `arguments` entry to carry the three new fields:

```lisp
              "arguments"
              (coerce (mapcar (lambda (argument)
                                (make-ht "variable"
                                         (%symbol-ht (getf argument :variable))
                                         "spec"
                                         (%spec-tree-ht (getf argument :spec))
                                         "kind" (%keyword-string
                                                 (getf argument :kind))
                                         "supplied_p"
                                         (%symbol-ht (getf argument :supplied-p))
                                         "keyword" (%keyword-string
                                                    (getf argument :keyword))))
                              (getf report :arguments))
                      'vector)
```

with the case renderer:

```lisp
(defun %case-ht (case)
  "Render one named case of a Function Spec."
  (make-ht "name" (%keyword-string (getf case :name))
           "documentation" (sanitize-for-json (getf case :documentation))
           "guard" (sanitize-for-json (getf case :guard))
           "guard_complete" (%optional-bool case :guard-complete)
           "guard_omitted_chars" (getf case :guard-omitted-chars)
           "outcome" (%keyword-string (getf case :outcome))
           "returns" (%spec-tree-ht (getf case :returns))
           "signals" (%spec-tree-ht (getf case :signals))
           "postconditions" (sanitize-for-json (getf case :postconditions))
           "postconditions_complete" (%optional-bool case :postconditions-complete)
           "postconditions_omitted_chars" (getf case :postconditions-omitted-chars)
           "post_value_variables" (%symbol-hts (getf case :post-value-variables))
           "state_post" (sanitize-for-json (getf case :state-post))))
```

- [ ] **Step 5: Extend the describe text**

Insert into `%format-describe-text`, after the `:post` block and before the
normalized IR tree. The argument line gains its kind, so a reader sees the
lambda list the contract actually declares:

```lisp
    ;; The lambda list as declared.  An argument rendered without its kind
    ;; reads as positional, and a caller building a call from this description
    ;; would pass a keyword argument by position.
    (let ((signals (getf report :signals)))
      (when signals
        (format stream "~&~%signals:")
        (%format-spec-node stream signals 0)))
    (when (getf report :capture)
      (format stream "~&~%capture:")
      (dolist (binding (getf report :capture))
        (format stream "~&  ~A = ~A"
                (getf (getf binding :name) :name) (getf binding :form))))
    (when (getf report :state-post)
      (format stream "~&~%state-post: ~A" (getf report :state-post)))
    (when (getf report :cases)
      (format stream "~&~%cases (~A selection):"
              (string-downcase (princ-to-string (getf report :case-selection))))
      (dolist (case (getf report :cases))
        (format stream "~&  ~A~@[ -- ~A~]"
                (string-downcase (princ-to-string (getf case :name)))
                (getf case :documentation))
        (format stream "~&    when:  ~A" (getf case :guard))
        (let ((outcome (getf case :outcome)))
          (if (eq :signals outcome)
              (progn (format stream "~&    signals:")
                     (%format-spec-node stream (getf case :signals) 2))
              (progn (format stream "~&    returns:")
                     (%format-spec-node stream (getf case :returns) 2))))
        (when (getf case :postconditions)
          (format stream "~&    :post  ~A" (getf case :postconditions)))
        (when (getf case :state-post)
          (format stream "~&    state-post: ~A" (getf case :state-post)))))
```

and in the arguments loop, print the kind before the name:

```lisp
      (dolist (argument (getf report :arguments))
        (let* ((spec (getf argument :spec))
               (kind (getf argument :kind))
               (name (format nil "~@[~A ~]~A~@[ [supplied-p ~A]~]"
                             (case kind
                               (:optional "&optional")
                               (:key "&key")
                               (:rest "&rest")
                               (t nil))
                             (getf (getf argument :variable) :name)
                             (getf (getf argument :supplied-p) :name))))
          (if spec
              (%format-spec-node stream spec 0 name)
              (format stream "~&  ~A" name))))
```

- [ ] **Step 6: Keep cl-spec's "could not freeze this" marker intact**

When a capture value is a CLOS instance, a structure or a hash table, cl-spec's
`PROJECT-CAPTURE-VALUE` does not publish it. It publishes

```lisp
(:UNAVAILABLE :REASON :OPAQUE-VALUE :TYPE <type>)
```

which is cl-spec stating that the value could not be frozen as evidence.
Handing that marker to `EXTERNALIZE-VALUE` registers an object id **for the
marker list**, and the response then offers `inspect-object` on it — which
reads as evidence cl-mcp obtained, about an object cl-spec just said it could
not keep.

Write the test first:

```lisp
(deftest an-unfreezable-capture-value-is-not-given-an-object-id
  (let* ((report (%state-post-check-report-with-opaque-capture))
         (response (build-spec-check-response report))
         (result (aref (gethash "results" response) 0))
         (capture (gethash "capture"
                           (gethash "state"
                                    (gethash "failure"
                                             (gethash "data"
                                                      (gethash "core_result"
                                                               result))))))
         (entry (aref (gethash "values" capture) 0))
         (value (gethash "value" entry)))
    (ok (equal "opaque-value" (gethash "reason" value)))
    (ok (equal "HASH-TABLE" (gethash "type" value)))
    ;; cl-spec said it could not freeze this.  An id here would offer the
    ;; caller an inspection cl-spec declined to support.
    (ok (null (gethash "object_id" value)))))
```

The fixture's capture value is
`(:unavailable :reason :opaque-value :type hash-table)`.

Then add the marker case to `project-value` in `src/spec-core-record.lisp`,
above its `externalize-value` fall-through, and export nothing new:

```lisp
        ;; cl-spec's own statement that a value could not be frozen as
        ;; evidence.  Passed through as it is: externalizing it would register
        ;; an object id for the marker and publish an inspection cl-spec had
        ;; just declined to support.
        ((and (consp value) (eq :unavailable (first value))
              (eq :opaque-value (getf (rest value) :reason)))
         (list :object
               (list (cons "unavailable" (list :scalar t))
                     (cons "reason" (list :scalar "opaque-value"))
                     (cons "type" (project-value (getf (rest value) :type))))))
```

Run `rove tests/spec-core-record-test.lisp` as well — `project-value` is that
file's subject, so its own case for the marker belongs there too.

- [ ] **Step 7: Run tests, lint, commit**

```bash
rove tests/spec-response-builders-test.lisp tests/spec-core-record-test.lisp
mallet src/spec-core-record.lisp src/tools/spec-response-builders.lisp \
       tests/spec-response-builders-test.lisp
git add src/tools/spec-response-builders.lisp tests/spec-response-builders-test.lisp
git commit -m "feat(spec): render the contract's cases, capture and state-post

The structured fields are primary and the text summarizes them, because a
client renders content[].text and nothing else -- a case that reached the
payload alone had not reached the caller.

Arguments now print their lambda-list kind.  A &key argument rendered as a bare
name reads as positional, and a caller building a call from the description
would pass it by position."
```

---

## Task 7: `spec-check` carries the result record

Twelve fields of `RESULT-DATA` have never crossed the boundary: `:budget`,
`:rejected`, `:options`, `:provenance`, `:shrunk-outcome`, `:shrink-report`,
`:generation-report`, `:failure-phase`, `:failure-reason`, `:case-report`,
`:failure`, `:shrunk-failure`. `%RESULT-PLIST` calls `RESULT-DATA` today and
uses it for two things: the core schema and the digest.

The existing top-level fields stay where they are and become **compatibility
aliases built from the same parsed record**, so the two cannot disagree (§2).
The legacy readers stay as the fallback for a cl-spec with no `RESULT-DATA`.

One reader is kept deliberately: `PROPERTY-RESULT-CONDITION`. `RESULT-DATA`
carries `:condition-report`, which is text, and has no `:condition` key at all —
measured. Dropping the reader would lose `condition.object_id` and with it the
`inspect-object` path onto the live condition. It supplies information the
record does not have and never reclassifies a fact the record does.

**Files:**
- Modify: `src/spec-adapter-report.lisp` — `%result-plist` (~line 1797),
  `%recorded-budget` (~line 1785), `%contract-plist` (~line 1651)
- Modify: `src/tools/spec-response-builders.lisp` — `%result-ht` (~line 643)
- Modify: `tests/spec-adapter-report-test.lisp`

**Interfaces:**
- Consumes: `project-core-record` (Task 4), `%core-record-ht` (Task 6).
- Produces: `%result-plist` returns `:core-record`; `%result-ht` publishes
  `core_result`. `(%core-fact record key legacy-thunk)` resolves one fact by
  the precedence rule.

- [ ] **Step 1: Write the failing tests**

Add to `tests/spec-adapter-report-test.lisp`:

```lisp
(deftest result-record-crosses-the-boundary-whole
  (let* ((api (%stub-api :result-data (constantly *passing-result*)
                         :result-status (constantly :passed)
                         :result-trials (constantly 2)
                         :result-seed (constantly 4611686018427387903)
                         :result-profile (constantly :normal)
                         :result-counterexample (constantly nil)
                         :result-shrunk-counterexample (constantly nil)
                         :result-condition (constantly nil)
                         :result-elapsed (constantly 0.005)))
         (report (cl-mcp/src/spec-adapter-report::%result-plist
                  api nil (%sym "ADD") :contract '(:executed 2)
                  '(:value "abc" :complete t :covers :contract) nil 2000 nil))
         (record (getf report :core-record))
         (data (getf record :data)))
    (ok (eq :collected (getf record :availability)))
    (testing "the twelve fields that never crossed now do"
      (dolist (key '("budget" "rejected" "options" "provenance" "shrunk_outcome"
                     "shrink_report" "generation_report" "failure_phase"
                     "failure_reason" "case_report" "failure" "shrunk_failure"))
        (ok (assoc key (second data) :test #'equal))))
    (testing "the aliases agree with the record because they come from it"
      (ok (eq :passed (getf report :status)))
      ;; A seed is text on both sides; a JSON consumer would round the number.
      (ok (equal "4611686018427387903" (getf report :seed))))))

(deftest an-old-cl-spec-still-answers-through-the-legacy-readers
  ;; §12 case 6.  No RESULT-DATA handle at all.
  (let* ((api (%stub-api :result-status (constantly :failed)
                         :result-trials (constantly 30)
                         :result-seed (constantly 7)
                         :result-profile (constantly :normal)
                         :result-counterexample (constantly '(a 1))
                         :result-shrunk-counterexample (constantly nil)
                         :result-condition (constantly nil)
                         :result-elapsed (constantly 0.1)))
         (report (cl-mcp/src/spec-adapter-report::%result-plist
                  api nil (%sym "ADD") :property '(:executed 30)
                  '(:value nil :complete nil :covers :property) nil 2000
                  '(:argument-count 1 :shrink-enabled t :known t)))
         (record (getf report :core-record)))
    (ok (eq :failed (getf report :status)))
    (ok (eq :unavailable (getf record :availability)))
    (ok (null (getf record :data)))
    ;; Not reported as measured zeros for counters nothing kept.
    (ok (null (getf record :field-availability)))))

(deftest a-result-data-that-signals-is-an-adapter-fault
  ;; §12 case 21.  The name resolved and the call broke: falling back to the
  ;; legacy readers would hide a signature mismatch behind a healthy response.
  (let* ((api (%stub-api :result-data (lambda (result)
                                        (declare (ignore result))
                                        (error "boom"))
                         :result-status (constantly :passed)
                         :result-trials (constantly 2)
                         :result-seed (constantly 7)
                         :result-profile (constantly :normal)
                         :result-counterexample (constantly nil)
                         :result-shrunk-counterexample (constantly nil)
                         :result-condition (constantly nil)
                         :result-elapsed (constantly 0)))
         (report (cl-mcp/src/spec-adapter-report::%result-plist
                  api nil (%sym "ADD") :property '(:executed 2)
                  '(:value nil :complete nil :covers :property) nil 2000 nil)))
    (ok (eq :internal-error (getf report :status)))
    (ok (search "result-data" (string-downcase (getf report :message))))))

(deftest a-live-condition-keeps-its-object-id
  ;; §12 case 10.  RESULT-DATA has no :condition key -- measured -- so this is
  ;; the auxiliary reader the core rule allows.
  (let* ((condition (make-condition 'simple-error
                                    :format-control "gone" :format-arguments nil))
         (api (%stub-api :result-data (constantly *passing-result*)
                         :result-status (constantly :error)
                         :result-trials (constantly 1)
                         :result-seed (constantly 7)
                         :result-profile (constantly :normal)
                         :result-counterexample (constantly nil)
                         :result-shrunk-counterexample (constantly nil)
                         :result-condition (constantly condition)
                         :result-elapsed (constantly 0)))
         (report (cl-mcp/src/spec-adapter-report::%result-plist
                  api nil (%sym "ADD") :property '(:executed 1)
                  '(:value nil :complete nil :covers :property) nil 2000 nil)))
    (ok (integerp (getf (getf report :condition) :object-id)))))
```

- [ ] **Step 2: Run to verify they fail**

Run: `rove tests/spec-adapter-report-test.lisp`
Expected: FAIL — `:core-record` is absent from the report.

- [ ] **Step 3: Add the precedence helper**

Insert before `%result-plist`:

```lisp
(defun %core-fact (record key legacy)
  "Return one fact by the precedence rule: the record first, then the reader.

RECORD is the parsed core record's :DATA source plist, or NIL when this cl-spec
has none.  LEGACY is a thunk calling the public reader.

The rule is about which reader is CALLED, not about comparing two answers.
When the record is there it is the only thing read, so the two can never
disagree; when it is not, the reader is all there is.  That is also what makes
the existing top-level fields safe as compatibility aliases -- they are built
from this, not beside it."
  (if record
      (getf record key)
      (funcall legacy)))
```

- [ ] **Step 4: Rewrite `%result-plist`'s head**

Replace the `let*` head and the `:core-schema` / `:status` / `:trials` / `:seed`
/ `:profile` / `:counterexample` / `:shrunk-counterexample` / `:elapsed`
bindings. The new head, with everything downstream unchanged except as noted:

```lisp
  (let* ((raw (when (api-has-p api :result-data)
                (handler-case (list :ok (funcall (api-fn api :result-data) result))
                  (error (condition) (list :failed condition)))))
         (core-status (first raw))
         (core-data (when (eq :ok core-status) (second raw))))
    ;; A versioned reader that exists and then breaks is a fault to report.
    ;; Falling back here would publish a healthy-looking response built from the
    ;; older readers while the new API was broken -- which is the failure this
    ;; adapter's status vocabulary exists to keep visible.
    (when (eq :failed core-status)
      (return-from %result-plist
        (list :property (symbol-data name) :kind kind :status :internal-error
              :trials trials :counterexample-status :unavailable
              :shrink-status :unavailable
              :message (format nil "cl-spec's result-data signalled while this ~
adapter read the result: ~A. The legacy readers are not used in its place, ~
because that would hide a broken versioned API behind a response that looked ~
complete." (princ-to-string (second raw))))))
    (multiple-value-bind (core-record core-record-status core-record-reason)
        (if core-data
            (project-core-record core-data :result-data
                                 :expected-record-kind :result)
            (values (list :availability :unavailable :schema-supported nil
                          :schema-version nil :field-availability nil
                          :unknown-keys nil
                          :projection (list :complete t :issues nil)
                          :data nil)
                    :unavailable nil))
      (when (eq :malformed core-record-status)
        (return-from %result-plist
          (list :property (symbol-data name) :kind kind :status :internal-error
                :trials trials :counterexample-status :unavailable
                :shrink-status :unavailable
                :message (format nil "cl-spec's result-data returned a record ~
this adapter cannot read: ~A." core-record-reason))))
      (let* ((source (when (eq :ok core-record-status) core-data))
             (status (%core-fact source :status
                                 (lambda () (funcall (api-fn api :result-status)
                                                     result))))
             (executed (%core-fact source :trials
                                   (lambda () (funcall (api-fn api :result-trials)
                                                       result))))
             (seed (%core-fact source :seed
                               (lambda () (funcall (api-fn api :result-seed)
                                                   result))))
             (counterexample
               (%core-fact source :counterexample
                           (lambda () (funcall (api-fn api :result-counterexample)
                                               result))))
             (shrunk (%core-fact source :shrunk-counterexample
                                 (lambda ()
                                   (funcall (api-fn api :result-shrunk-counterexample)
                                            result))))
             (elapsed (%core-fact source :elapsed
                                  (lambda () (funcall (api-fn api :result-elapsed)
                                                      result))))
             ;; Kept as a reader on purpose.  result-data carries
             ;; :CONDITION-REPORT, which is text, and no :CONDITION key at all,
             ;; so this is the only route to the object inspect-object drills
             ;; into.  It adds what the record does not carry and reclassifies
             ;; nothing the record does.
             (condition (funcall (api-fn api :result-condition) result))
             ...)
```

Keep the remaining bindings and the returned plist as they are, with three
changes: `:core-record core-record` is added; `:trials`'s recorded budget now
reads `(%core-fact source :budget (lambda () (%recorded-budget api result)))`;
and the `:profile` entry reads `(%core-fact source :profile ...)`.

- [ ] **Step 5: Publish it**

In `src/tools/spec-response-builders.lisp`, add to `%result-ht`:

```lisp
           "core_result" (%core-record-ht (getf result :core-record))
```

- [ ] **Step 6: Run tests, check parens, lint, commit**

```bash
rove tests/spec-adapter-report-test.lisp
mallet src/spec-adapter-report.lisp src/tools/spec-response-builders.lisp
git add src/spec-adapter-report.lisp src/tools/spec-response-builders.lisp \
        tests/spec-adapter-report-test.lisp
git commit -m "feat(spec): carry the whole result record, not its digest

result-data was already being called and was being used for two things: the
core schema and the digest.  Twelve of its fields -- the budget, the refusal
count, the options, the provenance, both shrink fields, the generation report,
the failure phase and reason, the case report and both failure observations --
were read by nobody.

The existing fields become compatibility aliases built from the same parsed
record rather than from a second reader, so they cannot disagree with it.  The
legacy readers stay as the fallback for a cl-spec without result-data, and one
of them stays unconditionally: property-result-condition is the only route to
the live condition object, which the record has no key for.

A result-data that exists and then signals is an internal error.  Falling back
to the older readers there would publish a complete-looking response built
while the versioned API was broken."
```

---

## Task 8: What the run could not establish

`verified` is cl-mcp's own question — is this result enough evidence — and it
already requires more than a passing status (`%EVALUATED-P` demands an evaluated
trial). Three more shortfalls join it, and cl-spec's `:passed` is not
reinterpreted: `results[].status` keeps the value cl-spec gave.

**Files:**
- Modify: `src/spec-adapter-report.lisp` — `+verification-gap-values+`
  (~line 2025), `%contract-facts` (~line 1498), `%verified-p` (~line 2173),
  `%verification-gaps` (~line 2102)
- Modify: `tests/spec-adapter-report-test.lisp`

**Interfaces:**
- Consumes: Task 7's `:core-record` on each result; Task 3's
  `validate-versioned-record`.
- Produces: four new `verification_gaps` values —
  `cases-never-called`, `case-coverage-unknown`, `generation-incomplete`,
  `core-schema-unsupported`, `contract-schema-unsupported`.

- [ ] **Step 1: Write the failing tests**

```lisp
(deftest an-unreached-case-is-not-a-verified-contract
  ;; §12 scenario 1, at the report layer.  cl-spec says :PASSED and that stays;
  ;; what changes is whether cl-mcp calls it evidence.
  (let ((results (list (list :status :passed :kind :contract
                             :contract '(:effective-trials 2 :rejected-usable t)
                             :trials '(:executed 2)
                             :core-record
                             (list :availability :collected :schema-supported t
                                   :field-availability '(:case-report :collected)
                                   :source '(:case-report
                                             (:never-called (:insufficient))))))))
    (ok (not (cl-mcp/src/spec-adapter-report::%verified-p results)))
    (ok (member :cases-never-called
                (cl-mcp/src/spec-adapter-report::%verification-gaps results)))))

(deftest an-unreadable-case-report-is-unknown-only-when-cases-are-declared
  (testing "cases are declared and the report did not come back"
    (let ((results (list (list :status :passed :kind :contract
                               :declares-cases t
                               :contract '(:effective-trials 2 :rejected-usable t)
                               :trials '(:executed 2)
                               :core-record
                               '(:availability :collected :schema-supported t
                                 :field-availability (:case-report :not-collected)
                                 :source nil)))))
      (ok (member :case-coverage-unknown
                  (cl-mcp/src/spec-adapter-report::%verification-gaps results)))))
  (testing "whether cases exist could not be read -- do not guess that they do"
    (let ((results (list (list :status :passed :kind :contract
                               :declares-cases :unknown
                               :contract '(:effective-trials 2 :rejected-usable t)
                               :trials '(:executed 2)
                               :core-record
                               '(:availability :collected :schema-supported t
                                 :field-availability (:case-report :not-collected)
                                 :source nil)))))
      (ok (not (member :case-coverage-unknown
                       (cl-mcp/src/spec-adapter-report::%verification-gaps
                        results)))))))

(deftest generation-and-shrinking-incompleteness-are-different
  (testing "the run stopped in generation -- verification did not complete"
    (let ((results (list (list :status :error :kind :contract
                               :core-record
                               '(:availability :collected :schema-supported t
                                 :field-availability (:failure-phase :collected)
                                 :source (:failure-phase :generation))))))
      (ok (member :generation-incomplete
                  (cl-mcp/src/spec-adapter-report::%verification-gaps results)))))
  (testing "the budget ran out while shrinking -- the failure still stands"
    (let ((results (list (list :status :failed :kind :contract
                               :core-record
                               '(:availability :collected :schema-supported t
                                 :field-availability (:failure-phase :collected)
                                 :source (:failure-phase nil
                                          :generation-report
                                          (:termination :budget-exhausted
                                           :exhaustion-phase :shrinking)))))))
      (ok (not (member :generation-incomplete
                       (cl-mcp/src/spec-adapter-report::%verification-gaps
                        results)))))))

(deftest a-schema-this-adapter-cannot-read-is-not-a-pass
  (let ((results (list (list :status :passed :kind :contract
                             :contract '(:effective-trials 2 :rejected-usable t)
                             :trials '(:executed 2)
                             :core-record
                             '(:availability :collected :schema-supported nil
                               :schema-version 2 :field-availability nil
                               :source nil)))))
    (ok (not (cl-mcp/src/spec-adapter-report::%verified-p results)))
    (ok (member :core-schema-unsupported
                (cl-mcp/src/spec-adapter-report::%verification-gaps results)))))
```

The tests read `:source` off the core record — the raw cl-spec plist
`project-core-record` keeps beside `:data` (Task 4), so the verdict logic reads
keywords instead of re-parsing projected JSON nodes. It is adapter-internal and
`%core-record-ht` does not render it.

- [ ] **Step 2: Run to verify they fail**

Run: `rove tests/spec-adapter-report-test.lisp`

- [ ] **Step 3: Gate `%contract-facts` on the schema**

The contract record is read here for `:preconditions`; it must not be read
under version 1's rules when it is not version 1. Patch `%contract-facts` so
the `handler-case` body begins:

```lisp
        (unless data (error 'unreadable-projection))
        (let ((schema (validate-versioned-record
                       data :expected-record-kind :definition
                            :expected-entity-kind :function-spec)))
          ;; A v2 contract record must not have v1's keys read off it: an
          ;; absent :KIND means :REQUIRED in v1 and may mean anything later,
          ;; and the same holds for :CASES and :PRECONDITIONS.
          (unless (eq :ok schema) (error 'unreadable-projection)))
```

and add to the returned plist, on both branches:

```lisp
              ;; Three answers, not two.  "This contract declares no cases" and
              ;; "whether it declares any could not be read" lead to different
              ;; verdicts, and %VERIFICATION-GAPS must not guess that cases
              ;; exist in order to report their coverage unknown.
              :declares-cases (if (getf data :case-selection) t nil)
```
with `:declares-cases :unknown` in the `error` branch.

**Then carry it onto the result.** `%CONTRACT-FACTS` returns the facts plist,
and the gap predicates read the *result*. `%RESULT-PLIST` already receives
`facts`, so add one entry to the plist it returns, beside `:kind`:

```lisp
          ;; From the contract facts, because the verdict is about this run of
          ;; that contract.  A property run has no cases and answers NIL.
          :declares-cases (getf facts :declares-cases)
```

Without this the two predicates below read NIL for every result and neither
`case-coverage-unknown` nor `contract-schema-unsupported` can ever fire.

- [ ] **Step 4: Extend the gap vocabulary and the two predicates**

Add to `+verification-gap-values+`:

```lisp
    :cases-never-called :case-coverage-unknown :generation-incomplete
    :core-schema-unsupported :contract-schema-unsupported
```

Add these helpers and use them from `%verification-gaps` and `%verified-p`:

```lisp
(defun %core-source (result)
  "Return the raw cl-spec record behind RESULT, or NIL."
  (getf (getf result :core-record) :source))

(defun %never-called-cases (result)
  "Return the declared cases this run never reached, or NIL.

Only a measured case report answers.  A report that did not come back says
nothing about coverage, which is a different shortfall and has its own gap."
  (when (eq :collected (getf (getf (getf result :core-record) :field-availability)
                             :case-report))
    (getf (getf (%core-source result) :case-report) :never-called)))

(defun %case-coverage-unknown-p (result)
  "Return true when cases are declared and their run report is missing.

Keyed on :DECLARES-CASES being exactly T.  :UNKNOWN means the contract record
could not be read, and claiming a coverage gap there would be this adapter
asserting that cases exist -- which is what it could not find out."
  (and (eq t (getf result :declares-cases))
       (not (eq :collected
                (getf (getf (getf result :core-record) :field-availability)
                      :case-report)))))

(defun %schema-unsupported-p (result)
  "Return true when the result record declares a schema version this adapter
does not know.  Its fields are then unread, so nothing in it is evidence."
  (let ((record (getf result :core-record)))
    (and (eq :collected (getf record :availability))
         (not (getf record :schema-supported)))))

(defun %generation-incomplete-p (result)
  "Return true when the run stopped in generation rather than on the target.

cl-spec's own :FAILURE-PHASE decides, not the generation report's termination.
An exhaustion in the shrinking phase leaves the failure established and only
the reduction unfinished, so it is not a verification shortfall."
  (eq :generation (getf (%core-source result) :failure-phase)))
```

In `%verification-gaps`, add inside the `dolist`:

```lisp
      (when (%never-called-cases result) (pushnew :cases-never-called gaps))
      (when (%case-coverage-unknown-p result) (pushnew :case-coverage-unknown gaps))
      (when (%generation-incomplete-p result) (pushnew :generation-incomplete gaps))
      (when (%schema-unsupported-p result) (pushnew :core-schema-unsupported gaps))
      (when (eq :unknown (getf result :declares-cases))
        (pushnew :contract-schema-unsupported gaps))
```

In `%verified-p`, extend the per-result predicate:

```lisp
       (every (lambda (result)
                (and (eq :passed (getf result :status))
                     (%evaluated-p result)
                     ;; A declared case nobody reached is a branch of the
                     ;; contract this run says nothing about.  cl-spec's
                     ;; :PASSED is untouched; what is refused is calling it
                     ;; evidence about the whole contract.
                     (null (%never-called-cases result))
                     (not (%case-coverage-unknown-p result))
                     ;; Reading the contract is a precondition for judging its
                     ;; coverage.  VERIFIED over a declaration this adapter
                     ;; could not parse would be a verdict about nothing.
                     (not (eq :unknown (getf result :declares-cases)))
                     (not (%schema-unsupported-p result))))
              results)
```

- [ ] **Step 5: Document the five new gaps in the tool description**

`tests/spec-tools-test.lisp:297` asserts that **every** value in
`+verification-gap-values+` appears in the `spec-check` tool description. Adding
a gap value without describing it turns that suite red, so the description is
part of this task, not of Task 11.

Add to the `verification_gaps` block in `src/tools/spec-tools.lisp`, in the same
style as the entries already there:

```text
  cases-never-called          a Function Spec declares named cases and at
                              least one was never reached. status stays
                              passed -- cl-spec judged the trials that ran --
                              but the unreached branch was not verified.
  case-coverage-unknown       cases are declared and no case report came back.
  generation-incomplete       the run stopped in generation and never reached
                              a verdict. NOT a finding about the code.
  core-schema-unsupported     the result record declares a schema version this
                              cl-mcp cannot read, so nothing in it is evidence.
  contract-schema-unsupported the same, for the Function Spec declaration.
```

Run `rove tests/spec-tools-test.lisp` and confirm it is green before committing.

- [ ] **Step 6: Run tests, lint, commit**

```bash
rove tests/spec-adapter-report-test.lisp
rove tests/spec-tools-test.lisp
mallet src/spec-adapter-report.lisp src/tools/spec-tools.lisp \
       tests/spec-adapter-report-test.lisp
git add src/spec-adapter-report.lisp src/tools/spec-tools.lisp \
        tests/spec-adapter-report-test.lisp
git commit -m "feat(spec): a declared case nobody reached is not a verification

cl-spec's :PASSED keeps its meaning and its place in results[].status.  What
changes is cl-mcp's own question -- is this enough evidence -- which already
required an evaluated trial and now also requires that every declared case was
reached, that the case report came back when cases exist, and that both the
result and the contract declare a schema this adapter can read.

Generation and shrinking incompleteness stay apart.  The gap is keyed on
cl-spec's failure_phase, so a budget exhausted while shrinking leaves the
failure established and raises nothing: what is unfinished there is the
reduction, not the verification.

case-coverage-unknown needs cases to be known to exist.  A contract record this
adapter could not read answers :UNKNOWN, and reporting a coverage gap on it
would be asserting the cases it just failed to find."
```

---

## Task 9: The text and the JSON must agree

A client renders `content[].text` and nothing else, so a verification gap that
reaches only the payload has not reached the caller. Three renderings are now
forbidden outright (§7):

```
text "VERIFIED"                         json never_called = ["insufficient-funds"]
text "no smaller counterexample exists" json shrink_report.termination
                                             = "state-restoration-unavailable"
text "target failed"                    json failure_phase = "generation"
```

**Files:**
- Modify: `src/tools/spec-response-builders.lisp` —
  `%format-one-result` (~line 823), `%format-counterexample` (~line 690),
  `%check-headline` (~line 934)
- Modify: `tests/spec-response-builders-test.lisp`

**Interfaces:**
- Consumes: `:core-record` on each result (Task 7), the gaps (Task 8).
- Produces: `(%format-core-evidence stream result)`, called from
  `%format-one-result` after the contract block.

- [ ] **Step 1: Write the failing tests**

```lisp
(deftest the-headline-names-a-case-nobody-reached
  (let* ((report (%contract-check-report-with-cases))
         (text (first-text (build-spec-check-response report))))
    (ok (search "NOT VERIFIED" text))
    (ok (search "insufficient-funds" text))
    (ok (search "NEVER CALLED" text))
    ;; The forbidden rendering: a clean verdict beside an unreached branch.
    (ok (not (search "✓ VERIFIED" text)))))

(deftest shrinking-that-could-not-run-does-not-read-as-shrinking-that-found-nothing
  (let* ((report (%state-post-check-report))
         (text (first-text (build-spec-check-response report))))
    (ok (search "state-restoration-unavailable" text))
    (ok (not (search "returned no smaller input" text)))
    (ok (not (search "no smaller counterexample" text)))))

(deftest a-generation-failure-does-not-read-as-a-target-failure
  (let* ((report (%generation-exhausted-check-report))
         (text (first-text (build-spec-check-response report))))
    (ok (search "generation" text))
    (ok (search "did NOT complete" text))
    ;; Nothing was learned about the function, and the text has to say so.
    (ok (not (search "the function" text)))))

(deftest a-state-post-violation-says-the-target-returned
  (let* ((report (%state-post-check-report))
         (text (first-text (build-spec-check-response report))))
    (ok (search "failure phase: state-post" text))
    (ok (search "target WAS called" text))
    (ok (search "captured:" text))
    (ok (search "BALANCE-BEFORE" text))
    (ok (search "state-post: violation" text))))

(deftest an-ordinary-property-is-never-told-its-body-was-not-called
  ;; §12 case 14.  A property's observation records no target outcome, so
  ;; :NOT-COLLECTED there means "no target evidence", not "nothing ran".
  (let* ((report (%property-check-report-with-not-collected-outcome))
         (text (first-text (build-spec-check-response report))))
    (ok (not (search "target was not called" text)))
    (ok (not (search "target WAS called" text)))))
```

Write the four fixture builders beside the existing `%contract-check-report`,
each returning a completed report with one result carrying a `:core-record`
whose `:source` holds the measured record shape from §12's scenarios.

- [ ] **Step 2: Run to verify they fail**

Run: `rove tests/spec-response-builders-test.lisp`

- [ ] **Step 3: Add the evidence renderer**

```lisp
(defparameter +shrink-terminations+
  '((:state-restoration-unavailable
     . "not attempted -- this contract observes state, which nothing restores, ~
so no candidate may call the target again")
    (:not-a-target-failure
     . "not attempted -- the failure happened before the target was called")
    (:disabled . "not attempted -- shrinking is off for this definition")
    (:no-shrinker . "not attempted -- this generator has no shrinker")
    (:mutation . "stopped -- the target changed its arguments")
    (:generation-budget-exhausted
     . "stopped -- the generation budget ran out while shrinking. The failure ~
above still stands; only the reduction is unfinished")
    (:budget-exhausted . "stopped -- the shrink budget ran out")
    (:shrinker-error . "stopped -- the shrinker signalled")
    (:exhausted . "ran to exhaustion -- no smaller failing input was found"))
  "How to word each shrink termination cl-spec is known to record.

Not a closed enumeration: cl-spec publishes none, and there is no :COMPLETED at
all -- :EXHAUSTED is the successful search.  A value absent from this table is
printed as itself rather than sorted into complete or incomplete, because
sorting it would be this adapter deciding a meaning cl-spec has not stated.")

(defun %format-core-evidence (stream result)
  "Write the evidence cl-spec recorded for RESULT to STREAM.

Only what the record actually carries.  Every line here is keyed on a field of
the versioned record, so the text cannot claim something the JSON beside it
does not say."
  (let* ((record (getf result :core-record))
         (source (getf record :source))
         (contract-p (eq :contract (getf result :kind))))
    (when source
      (let ((cases (getf (getf source :case-report) :cases))
            (never (getf (getf source :case-report) :never-called)))
        (when cases
          (format stream "~&    cases: ~{~A~^ | ~}"
                  (mapcar (lambda (entry)
                            (if (zerop (getf entry :called))
                                (format nil "~(~A~) NEVER CALLED"
                                        (getf entry :name))
                                (format nil "~(~A~) ~D called (~D passed)"
                                        (getf entry :name) (getf entry :called)
                                        (getf entry :passed))))
                          cases)))
        (when never
          (format stream "~&      ~D declared case~:P ~:*~[~;was~:;were~] never ~
reached, so this run says nothing about ~:*~[~;it~:;them~]." (length never))))
      (let ((phase (getf source :failure-phase)))
        (when phase
          (format stream "~&    failure phase: ~(~A~) -- ~A"
                  phase
                  (case phase
                    (:state-post "the target WAS called and returned; the \
contract's state-post clause is what failed")
                    (:case-selection "the target was NOT called; choosing which \
case applies is what failed")
                    (:capture "the target was NOT called; a :capture form \
signalled before it")
                    (:generation "the run stopped in generation and never \
reached a verdict -- verification did NOT complete, and this is not a finding \
about the code under test")
                    (t "see failure_phase in the payload")))))
      ;; Only for a contract.  A property's observation records no target
      ;; outcome at all, so :NOT-COLLECTED there means "no target evidence was
      ;; kept", not "the body never ran" -- and saying the latter would be a
      ;; false statement about code that executed.
      (when contract-p
        (let ((outcome (getf (getf source :failure) :outcome)))
          (cond ((eq :not-collected outcome)
                 (format stream "~&    target: not called"))
                ((eq :returned (getf outcome :kind))
                 (format stream "~&    target: returned ~{~A~^, ~}"
                         (mapcar #'princ-to-string (getf outcome :values))))
                ((eq :signaled (getf outcome :kind))
                 (format stream "~&    target: signalled ~A"
                         (getf outcome :condition-type))))))
      (let ((state (getf (getf source :failure) :state)))
        (let ((capture (getf state :capture)))
          (when (getf capture :values)
            (format stream "~&    captured: ~{~A~^, ~}"
                    (mapcar (lambda (entry)
                              (format nil "~A = ~A" (car entry) (cdr entry)))
                            (getf capture :values)))))
        (let ((post (getf state :state-post)))
          (when (member (getf post :status) '(:violation :error))
            (format stream "~&    state-post: ~(~A~) at form ~A~@[ -- ~A~]"
                    (getf post :status) (getf post :index)
                    (when (getf post :form)
                      (princ-to-string (getf post :form)))))))
      (let ((generation (getf source :generation-report)))
        (when (and generation (not (eq :completed (getf generation :termination))))
          (format stream "~&    generation: ~(~A~)~@[ in the ~(~A~) phase~] ~
(~D of ~D candidates)~@[ -- ~A~]"
                  (getf generation :termination)
                  (getf generation :exhaustion-phase)
                  (getf generation :attempts) (getf generation :budget)
                  (when (eq :shrinking (getf generation :exhaustion-phase))
                    "the failure above still stands; only the reduction is \
unfinished"))))
      (let ((shrink (getf source :shrink-report)))
        (when (and shrink (not (eq :not-collected shrink)))
          (let ((wording (cdr (assoc (getf shrink :termination)
                                     +shrink-terminations+))))
            (format stream "~&    shrinking: ~A"
                    (or wording
                        ;; Unknown to this adapter: print it, do not classify it.
                        (format nil "~(~A~) (this cl-mcp does not know that ~
termination; it is reported as cl-spec gave it)"
                                (getf shrink :termination))))))))))
```

- [ ] **Step 4: Call it, and stop the old shrink line contradicting it**

In `%format-one-result`, insert `(%format-core-evidence stream result)` between
`(%format-contract stream result)` and `(%format-counterexample stream result)`.

In `%format-counterexample`, guard the `:none` branch so it does not speak when
the record already explained the shrink:

```lisp
      (:none
       ;; Silent when a shrink report exists: "shrinking was enabled but
       ;; returned no smaller input" is a claim about a search, and
       ;; state-restoration-unavailable means no search happened.
       (unless (getf (getf (getf result :core-record) :source) :shrink-report)
         (format stream "~&    shrunk counterexample: shrinking was enabled but ~
returned no smaller input")))
```

- [ ] **Step 5: Put the coverage shortfall in the headline**

`%check-headline` already binds `contract` and `properties` in its `let*` and
tests them in the `coverage` `cond`. Bind a third the same way, and give it the
first clause: the headline is where a reader stops, and a declared case nobody
reached is exactly the gap a bare verdict hides.

Add to the `let*`, after the `properties` binding:

```lisp
         (never-called
           (remove-duplicates
            (loop for result in (getf report :results)
                  append (getf (getf (getf (getf result :core-record) :source)
                                     :case-report)
                               :never-called))))
```

and make this the first clause of the `coverage` `cond`, above `(contract ...)`:

```lisp
             (never-called
              (format nil "~D declared case~:P never reached: ~{~(~A~)~^, ~}"
                      (length never-called) never-called))

- [ ] **Step 6: Run tests, lint, commit**

```bash
rove tests/spec-response-builders-test.lisp
mallet src/tools/spec-response-builders.lisp tests/spec-response-builders-test.lisp
git add src/spec-core-record.lisp src/tools/spec-response-builders.lisp \
        tests/spec-response-builders-test.lisp tests/spec-core-record-test.lisp
git commit -m "feat(spec): say in the text what the record says in the JSON

A client renders content[].text and nothing else, so a case nobody reached, a
shrink that could not run and a run that stopped in generation had to reach the
text or they had not reached the caller at all.

Each line is keyed on a field of the versioned record, so the two cannot
diverge.  The shrink wording comes from a table of the terminations cl-spec is
known to record; anything else is printed as given rather than sorted into
complete or incomplete, because there is no :COMPLETED termination to contrast
with and :EXHAUSTED is the successful search.

The target line is written only for a contract.  A property's observation keeps
no target outcome, so :NOT-COLLECTED there means no target evidence was
recorded -- rendering it as \"the target was not called\" would be a false
statement about a body that ran.

A capture value cl-spec marked unfreezable keeps that marker and gets no object
id.  Externalizing it would have registered one for the marker list itself and
offered an inspection cl-spec had just declined to support."
```

---

## Task 10: Five scenarios against the real cl-spec

Stub tests cover the branches a healthy cl-spec cannot reach. These cover the
opposite: that the shapes this adapter projects are the shapes cl-spec actually
produces. The DSL below is copied from cl-spec's own runnable examples
(`examples/function-spec-cases.lisp`, `examples/stateful-withdraw.lisp`) rather
than written from the specification.

**Files:**
- Modify: `tests/fixtures/spec-fixture.lisp` (exports and helpers)
- Modify: `tests/fixtures/spec-fixture-contracts.lisp` (the contracts)
- Modify: `tests/spec-integration-test.lisp`

- [ ] **Step 1: Add the fixture targets and generator**

Append to `tests/fixtures/spec-fixture.lisp`, and add each new name to its
`defpackage` `:export` list:

```lisp
(defstruct (purse (:constructor make-purse (balance id)))
  "A tiny mutable object, so a contract has some state to observe."
  balance
  id)

(defvar *scripted-arguments* nil
  "Argument lists a scripted generator hands out, front to back.

A special rather than a closure, so a test states its inputs in one place and
the contract names one registered generator.  Copied from cl-spec's own
examples, where the same device keeps a demo from depending on what a seed
happens to draw.")

(define-condition insufficient-funds (error)
  ((balance :initarg :balance :reader insufficient-funds-balance)
   (amount :initarg :amount :reader insufficient-funds-amount))
  (:report (lambda (condition stream)
             (format stream "Cannot withdraw ~D from ~D."
                     (insufficient-funds-amount condition)
                     (insufficient-funds-balance condition))))
  (:documentation "The expected error of the withdrawal fixtures."))

(defun remaining-balance (balance amount)
  "Return what is left, or signal INSUFFICIENT-FUNDS.  Pure."
  (if (<= amount balance)
      (- balance amount)
      (error 'insufficient-funds :balance balance :amount amount)))

(defun overlapping-balance (balance amount)
  "A target whose contract's two guards both hold when the amounts are equal."
  (- balance amount))

(defun withdraw-without-recording! (purse amount)
  "Return the new balance and forget to store it.

The target returns correctly and leaves the object wrong, which is the one
failure a return-value contract cannot see and a :state-post can."
  (declare (ignore amount))
  (purse-balance purse))

(defun never-satisfied-p (value)
  "Return NIL for every value, so a filtered generator can exhaust its budget."
  (declare (ignore value))
  nil)
```

- [ ] **Step 2: Add the contracts**

Append to `tests/fixtures/spec-fixture-contracts.lisp`:

```lisp
(cl-spec:defgenerator scripted-arguments ()
  "Return the next scripted argument list, so a test controls its inputs."
  (pop *scripted-arguments*))

(cl-spec:defspec-function remaining-balance
  "Require the remainder when the balance suffices, and the named error when
it does not."
  (:args (balance (range integer 0 1000)) (amount (range integer 1 1000)))
  (:args-generator scripted-arguments)
  (:cases
    (:sufficient-funds
      "The amount fits: return the remaining balance."
      (:when (<= amount balance))
      (:returns (range integer 0 *))
      (:post (= result (- balance amount))))
    (:insufficient-funds
      "The amount does not fit: signal the named error."
      (:when (> amount balance))
      (:signals (type insufficient-funds)))))

(cl-spec:defspec-function overlapping-balance
  "Two guards that both hold when the amounts are equal."
  (:args (balance (range integer 0 1000)) (amount (range integer 1 1000)))
  (:args-generator scripted-arguments)
  (:cases
    (:at-least (:when (>= balance amount)) (:returns (range integer 0 *)))
    (:at-most (:when (<= balance amount)) (:returns (range integer 0 *)))))

(cl-spec:defspec-function withdraw-without-recording!
  "A successful call must reduce the stored balance, which this target does not."
  (:args (purse (satisfies purse-p)) (amount (range integer 1 100)))
  (:args-generator scripted-arguments)
  (:capture
    (balance-before (purse-balance purse))
    (id-before (purse-id purse)))
  (:cases
    (:sufficient-funds
      (:when (<= amount balance-before))
      (:returns (type integer))
      (:state-post (= (purse-balance purse) (- balance-before amount))
                   (eql (purse-id purse) id-before)))))

(cl-spec:defspec impossible-int (and (range integer 0 100)
                                     (satisfies never-satisfied-p)))

(cl-spec:defspec-function magnitude-of-impossible
  "A contract whose argument spec no generated candidate satisfies."
  (:args (value impossible-int))
  (:returns (range integer 0 *)))
```

Add `(defun magnitude-of-impossible (value) (abs value))` to the main fixture
file and export it.

- [ ] **Step 3: Confirm the fixtures produce the states before asserting on them**

Before writing assertions, run each contract once in a fresh process and print
`(cl-spec:result-data ...)`. A fixture that does not reach the state a test is
written for makes the test green for the wrong reason.

```bash
ros run --load /tmp/probe-fixtures.lisp > /tmp/probe-fixtures.out 2>&1 < /dev/null
```

with a script that loads `cl-spec/check-it`, loads the fixture into a fresh
registry, and prints `result-data` for each of the five. Record in the test
file which state each one reached. If `magnitude-of-impossible` does not
exhaust the generation budget, adjust its spec until it does — the assertion
depends on the generator actually giving up, not on the spec looking impossible.

- [ ] **Step 4: Write the five scenarios**

Add to `tests/spec-integration-test.lisp`, each guarded by
`(%contracts-available-p)` and `with-fixture-registry` like the existing cases:

```lisp
(deftest real-named-cases-report-the-one-never-reached
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (with-fixture-registry
        (setf (symbol-value (find-symbol "*SCRIPTED-ARGUMENTS*"
                                         "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
              (list '(10 2) '(20 5)))
        (let* ((response (spec-check-response
                          (make-ht "function" (%fixture-name "REMAINING-BALANCE")
                                   "trials" 2 "seed" "1")))
               (result (%first-result response))
               (record (gethash "data" (gethash "core_result" result)))
               (cases (gethash "case_report" record))
               (text (%text response)))
          (ok (equal "passed" (gethash "status" result)))
          (testing "the report distinguishes reached from declared"
            (ok (= 2 (length (gethash "declared_cases" cases))))
            (ok (equal #("insufficient-funds") (gethash "never_called" cases))))
          (testing "and the verdict does not read as full coverage"
            (ok (eq (yason:false) (gethash "verified" response)))
            (ok (find "cases-never-called"
                      (gethash "verification_gaps" response) :test #'equal))
            (ok (search "NEVER CALLED" text)))))))

(deftest real-state-post-failure-keeps-the-target-outcome
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (with-fixture-registry
        (let ((purse (funcall (find-symbol "MAKE-PURSE"
                                           "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE")
                              100 7)))
          (setf (symbol-value (find-symbol "*SCRIPTED-ARGUMENTS*"
                                           "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
                (list (list purse 30)))
          (let* ((response (spec-check-response
                            (make-ht "function"
                                     (%fixture-name "WITHDRAW-WITHOUT-RECORDING!")
                                     "trials" 1 "seed" "1")))
                 (result (%first-result response))
                 (record (gethash "data" (gethash "core_result" result)))
                 (failure (gethash "failure" record))
                 (text (%text response)))
            (ok (equal "state-post" (gethash "failure_phase" record)))
            (ok (equal "state-postcondition" (gethash "failure_reason" record)))
            (testing "the target returned normally and that is visible"
              (ok (equal "returned"
                         (gethash "kind" (gethash "outcome" failure)))))
            (testing "the captured pre-state survives"
              (ok (plusp (length (gethash "values"
                                          (gethash "capture"
                                                   (gethash "state" failure)))))))
            (testing "and state-post is reported as violated, not as a target bug"
              (ok (equal "violation"
                         (gethash "status" (gethash "state_post"
                                                    (gethash "state" failure)))))
              (ok (search "target WAS called" text))
              (ok (search "state-post: violation" text))))))))

(deftest real-case-selection-error-does-not-blame-the-target
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (with-fixture-registry
        (setf (symbol-value (find-symbol "*SCRIPTED-ARGUMENTS*"
                                         "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
              (list '(5 5)))
        (let* ((response (spec-check-response
                          (make-ht "function" (%fixture-name "OVERLAPPING-BALANCE")
                                   "trials" 1 "seed" "1")))
               (result (%first-result response))
               (record (gethash "data" (gethash "core_result" result)))
               (text (%text response)))
          (ok (equal "case-selection" (gethash "failure_phase" record)))
          (testing "the target was never called"
            (ok (equal "not-collected"
                       (gethash "kind" (gethash "outcome"
                                                (gethash "failure" record))))))
          (testing "the structured selection evidence is preserved"
            (ok (eql 1 (gethash "case_selection_errors"
                                (gethash "case_report" record)))))
          (testing "and the text says which half broke"
            (ok (search "the target was NOT called" text))
            ;; A counterexample exists for this run, and it must not read as
            ;; an input the function failed on.
            (ok (not (search "the function failed" text))))))))

(deftest real-generation-exhaustion-is-not-a-target-failure
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (with-fixture-registry
        (let* ((response (spec-check-response
                          (make-ht "function"
                                   (%fixture-name "MAGNITUDE-OF-IMPOSSIBLE")
                                   "trials" 1 "seed" "1")))
               (result (%first-result response))
               (record (gethash "data" (gethash "core_result" result)))
               (generation (gethash "generation_report" record))
               (text (%text response)))
          (ok (gethash "termination" generation))
          (ok (equal "generation" (gethash "failure_phase" record)))
          (ok (find "generation-incomplete"
                    (gethash "verification_gaps" response) :test #'equal))
          (ok (search "did NOT complete" text)))))))

(deftest real-state-contract-says-why-it-was-not-shrunk
  (if (not (%contracts-available-p))
      (skip +no-contracts-reason+)
      (with-fixture-registry
        (let ((purse (funcall (find-symbol "MAKE-PURSE"
                                           "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE")
                              100 7)))
          (setf (symbol-value (find-symbol "*SCRIPTED-ARGUMENTS*"
                                           "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
                (list (list purse 30)))
          (let* ((response (spec-check-response
                            (make-ht "function"
                                     (%fixture-name "WITHDRAW-WITHOUT-RECORDING!")
                                     "trials" 1 "seed" "1")))
                 (result (%first-result response))
                 (record (gethash "data" (gethash "core_result" result)))
                 (shrink (gethash "shrink_report" record))
                 (text (%text response)))
            (ok (equal "state-restoration-unavailable"
                       (gethash "termination" shrink)))
            (testing "the original evidence is still there"
              (ok (gethash "failure" record)))
            (testing "and nothing claims a minimal counterexample"
              (ok (not (search "no smaller" text)))
              (ok (search "nothing restores" text))))))))
```

- [ ] **Step 5: Run and commit**

```bash
rove tests/spec-integration-test.lisp
mallet tests/spec-integration-test.lisp tests/fixtures/spec-fixture.lisp \
       tests/fixtures/spec-fixture-contracts.lisp
git add tests/
git commit -m "test(spec): the five states this projection exists to preserve

Against the real cl-spec, not a stub: a named case nobody reached, a target
that returned correctly and left the object wrong, guards that both matched, a
generator that gave up, and a contract nothing could shrink.  Each asserts on
both the record and the text, because the text is all a client renders.

The contracts are copied from cl-spec's own runnable examples rather than
written from the specification, and each fixture was run once and its
result-data read before any assertion was written against it -- a fixture that
does not reach the state under test makes its test green for the wrong reason."
```

---

## Task 11: Documentation, and the whole suite

**Files:**
- Modify: `docs/tools.md` — the `cl-spec` group section (~line 1046)
- Modify: `src/tools/spec-tools.lisp` — the `spec-check` and `spec-describe`
  descriptions. `tests/spec-tools-test.lisp` checks the description against
  `+verification-gap-values+`, so the five new values must be documented or
  that test fails.
- Modify: `docs/cl-spec-adapter-feedback.md` — a closing note

The five new `verification_gaps` values were documented in Task 8, because
`tests/spec-tools-test.lisp` pins the description to `+verification-gap-values+`
and a gap added without a description turns that suite red in the task that adds
it. What remains here is the prose that no test pins.

- [ ] **Step 1: Document `core_result` in `docs/tools.md`**

Add under the `spec-check` bullets:

```markdown
  - `results[].core_result` carries cl-spec's own versioned result record.
    `data` is that record and holds no field cl-mcp added; `availability`,
    `schema_supported`, `field_availability`, `unknown_keys` and `projection`
    are cl-mcp's transport metadata and sit beside it. `field_availability`
    distinguishes a key that is absent from one whose value is `null` — which
    matters most for `failure_phase`, where `null` means an ordinary target
    observation. `projection.issues` names every place a long or deep value was
    cut, so a short list can be told from a truncated one.
  - `data.failure` and `data.shrunk_failure` are the observations. Their
    `outcome.kind` is `returned`, `signaled` or `not-collected`; for a
    **contract** `not-collected` means the target was never called, and for a
    **property** it only means no target outcome was recorded — a property body
    runs without one. `state.capture` and `state.state_post` keep their own
    statuses, so "the target returned and state-post failed" is distinguishable
    from "the target failed".
  - `data.shrink_report.termination` is reported as cl-spec gave it and is never
    used to decide whether the run verified anything. There is no `completed`
    value; `exhausted` is the successful search, and
    `state-restoration-unavailable` means no search happened at all.
  - `data.generation_report` says whether generation finished.
    `exhaustion_phase: "shrinking"` leaves the failure established and only the
    reduction unfinished, so it is not a verification gap.
  - `data.provenance` describes the environment the run happened in, recorded
    before it started. `environment` describes this image now. They answer
    different questions and neither overwrites the other.
```

And under `spec-describe`:

```markdown
  - `function-spec` projects the contract whole: each argument's spec, `kind`
    (`required` / `optional` / `key` / `rest`), `supplied_p` and `keyword`, the
    `argument_generator` and `argument_schema`, `returns`, `signals`, `:pre`,
    `:post`, `post_value_variables`, `capture`, `state_post`, `case_selection`
    and the ordered `cases`. Describing a contract runs none of it — no target,
    no `:pre`, no capture form, no case guard, no post form.
```

- [ ] **Step 2: Run every check**

```bash
# Cold compile, to catch warnings from all changed files
ros run --eval '(asdf:compile-system :cl-mcp :force :all)' --eval '(sb-ext:quit)'

# The full suite, from a fresh process -- the live MCP server's image is stale
rove cl-mcp.asd

# Lint, the globs the Lint CI job uses
mallet src/*.lisp src/*/*.lisp tests/*.lisp
```

For `rove cl-mcp.asd`, count the `;; testing '` lines against the number of test
packages: the final Summary covers only the last package, and a load failure
still exits 0. Two `×` are the expected baseline for this repo.

- [ ] **Step 3: Close the feedback note**

Append a dated section to `docs/cl-spec-adapter-feedback.md` recording which of
its 2026-03 P1 items current cl-spec has since answered — 1.1 (`:budget` and
the `:skipped` rule), 1.2 (`:failure` distinguishes an empty counterexample
from a missing one), 1.3 (`:shrunk-outcome` and `:shrink-report`) — and which
remain. Name what still cannot be exposed: `call-outcome`'s readers are not
exported, so the target outcome is readable only through `observation-data`'s
plist, and `check-call` is deferred with its reasons in §11 of the design note.

- [ ] **Step 4: Commit**

```bash
git add docs/ src/tools/spec-tools.lisp
git commit -m "docs(spec): describe the record the tools now carry

The tool description is the only documentation a model ever sees, and
spec-tools-test checks it against the code's gap list, so the five new values
had to be written there rather than only here.

Two distinctions are spelled out because getting them wrong is the failure this
change was made to prevent: outcome.kind not-collected means the target
was not called for a contract and only that no target outcome was recorded for
a property, and a shrink termination is reported as given rather than sorted
into complete or incomplete -- there is no completed value to contrast with."
```

---

## Self-Review

**Spec coverage.** Task A → Tasks 5–6. Task B → Task 7. Task C → Tasks 7–9.
Task D → Tasks 4, 7, 9. Task E → Tasks 4, 8, 9. Task F → Tasks 4, 9. Task G →
Tasks 4, 7, 11. Task H → Tasks 3, 7. Task I → Task 7 (`%core-fact`) and Task 11
(documented). Task J → Task 9. §12's twenty-three tests: 1–5 in Task 10, 6–7 in
Task 7, 8 in Task 3, 9 and 21 in Task 7, 10 in Task 7, 11 in Task 3, 12 in
Tasks 4–5, 13 in Task 4, 14 in Task 9, 15–16 in Task 2, 17 in Task 6, 18–19 in
Tasks 2–3, 20 in Task 1, 22 in Task 9 step 6, 23–24 in Task 9.

**Type consistency.** `project-value` (Task 1) → `project-record` (Task 2) →
`project-core-record` (Task 4) → `%core-record-ht` (Task 6) → `%result-ht`
(Task 7). Node tags `:scalar` / `:symbol` / `:value` / `:object` / `:array` are
produced only in Task 2 and consumed only in Task 6. `field-availability`
(Task 3) is consumed in Tasks 4 and 8 under the same three keywords.
`:declares-cases` is produced in Task 8's `%contract-facts` and consumed in the
same task's two predicates.

---

## Execution Handoff

Two execution options:

1. **Subagent-Driven (recommended)** — a fresh subagent per task, reviewed
   between tasks.
2. **Inline Execution** — tasks executed in this session with checkpoints.
