# cl-spec 仕様取得・Property 検証アダプタ 実装計画

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** cl-spec に登録された Spec / Property を cl-mcp の tool として発見・取得・実行し、構造化された反例と再現情報を LLM へ返せるようにする。

**Architecture:** cl-spec への依存は追加せず、`find-package` + `find-symbol` による遅延解決で公開 API に束縛する。registry は worker image に載るため、発見・取得・実行のすべてを worker で行い、`with-proxy-dispatch` で routing する。時間上限は cl-spec ではなく cl-mcp 既存の `call-with-deadline-thread` が担う。

**Tech Stack:** SBCL / ASDF package-inferred-system / rove / yason / 既存 cl-mcp モジュール(`define-tool`, `proxy`, `utils/deadline`, `object-registry`, `utils/bounded-stream`, `code-core`)

**設計書:** `docs/superpowers/specs/2026-09-09-cl-spec-mcp-adapter-design.md`

## Global Constraints

- Google Common Lisp Style Guide。2-space indent、100 桁以内、トップレベルフォーム間に空行 1 行。
- 各 `*.lisp` は `;;;; <path>` で始め、次に `defpackage`、次に `(in-package ...)`。
- `(:use #:cl)` のみ。他は全て `:import-from` で明示的に取り込む。
- 公開関数・マクロ・クラスに docstring 必須。
- lower-case lisp-case、`*special*`、`+constant+`、`something-p`。
- **`cl-mcp.asd` を編集しない。** package-inferred-system なので新規ファイルは自動で解決される。登録先は `src/tools/all.lisp`(load 副作用)、`main.lisp`(export)、`tests.lisp`(テストスイート)。
- **`cl-spec` を `cl-mcp.asd` の `:depends-on` に追加しない。** cl-spec を使わないプロジェクトで既存 tool が壊れてはならない。
- **cl-spec core に cl-mcp や check-it への依存を追加しない。** 本計画は cl-spec リポジトリを一切変更しない。
- **tool 入力の symbol 解決に reader 評価・`intern` を使わない。** `find-package` と `find-symbol` のみ。
- **seed は 10 進文字列でのみ受け渡す。** cl-spec の seed は最大 `(expt 2 62)` 未満で JSON の安全整数 2^53 を超える。JSON number を受け付けると、既に丸められた seed を受理してしまう。
- **Lisp の値を JSON number として返さない。** すべて printed 文字列 + `printed_complete` フラグ。
- ゼロ件・skip・timeout・generator error・backend error を Property 成功にまとめない。
- 既存 tool の descriptor・応答形状を変更しない。
- SBCL 専用で構わない(cl-mcp は SBCL 専用)。
- Lint: `mallet src/*.lisp src/*/*.lisp tests/*.lisp`
- コミットは imperative で scoped (`module: action`)。各 Task の最後にコミットする。

## File Structure

| ファイル | 責務 | Task |
|---|---|---|
| `src/spec-adapter-core.lisp` | cl-spec API の遅延解決、symbol 解決、値の外部表現、digest。cl-spec を知らない純粋な部品 | 1, 2 |
| `src/spec-adapter-report.lisp` | 3 つの操作(発見・取得・実行)を plist で組み立てる。deadline と予算配分もここ | 3, 4 |
| `src/tools/spec-response-builders.lisp` | plist → hash-table と content text | 5 |
| `src/tools/spec-tools.lisp` | `define-tool` ×3 + `with-proxy-dispatch` | 6 |
| `src/worker/handlers.lisp` (修正) | `worker/spec-symbol` / `worker/spec-describe` / `worker/spec-check` | 6 |
| `src/tools/all.lisp` (修正) | 読み込み副作用のための import | 6 |
| `main.lisp` (修正) | 公開 symbol の再 export | 6 |
| `tests.lisp` (修正) | テストスイート登録 | 1, 5, 6, 7 |
| `tests/spec-adapter-core-test.lisp` | symbol 解決 / API 解決 / 値の外部表現 / digest | 1, 2 |
| `tests/spec-adapter-report-test.lisp` | 3 操作の全分岐(stub API で cl-spec 非依存) | 3, 4 |
| `tests/spec-response-builders-test.lisp` | 応答形状と content text | 5 |
| `tests/spec-tools-test.lisp` | tool 経由(`process-json-line`)。cl-spec があれば実物、無ければ skip | 6, 7 |

**設計書 §3.3 からの変更:** `src/spec-adapter-core.lisp` 1 本の予定だったものを、部品層(`-core`)と操作層(`-report`)に分割した。1 ファイルが 600 行超になり、レビューと編集の単位として大きすぎるため。

---

### Task 1: cl-spec API の遅延解決と symbol 解決

**Files:**
- Create: `src/spec-adapter-core.lisp`
- Create: `tests/spec-adapter-core-test.lisp`
- Modify: `tests.lisp`

**Interfaces:**
- Consumes: なし(最初の Task)
- Produces:
  - `(defstruct cl-spec-api functions classes version system-directory missing)` — `functions` と `classes` は plist。`make-cl-spec-api` はキーワード引数コンストラクタ
  - `(api-fn api key)` → function。無ければ `error`
  - `(api-has-p api key)` → boolean
  - `(api-class api key)` → symbol or NIL
  - `(resolve-cl-spec-api)` → `(values api-or-nil status)`、status は `:ok` / `:not-loaded` / `:incomplete`
  - `(api-backend-available-p api)` → boolean
  - `(resolve-symbol-designator designator &key package)` → `(values symbol nil)` または `(values nil reason-plist)`
  - `(symbol-data symbol)` → `(:package "P" :name "N" :qualified "P::N")`

- [ ] **Step 1: 失敗するテストを書く**

`tests/spec-adapter-core-test.lisp` を新規作成する。

```lisp
;;;; tests/spec-adapter-core-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/spec-adapter-core.  These must not require
;;;; cl-spec to be loadable: the adapter's whole point is that cl-mcp works
;;;; with and without it, and a suite that needs cl-spec cannot check the
;;;; "not loaded" branch at all.

(defpackage #:cl-mcp/tests/spec-adapter-core-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:resolve-symbol-designator
                #:symbol-data))

(in-package #:cl-mcp/tests/spec-adapter-core-test)

(defun %ensure-fixture-packages ()
  "Create two packages that both export a symbol named FOO.
Same name, different home package: the pair a resolver must not confuse."
  (dolist (name '("CL-MCP-SPEC-FIXTURE-A" "CL-MCP-SPEC-FIXTURE-B"))
    (let ((package (or (find-package name) (make-package name :use '()))))
      (let ((symbol (intern "FOO" package)))
        (export symbol package))
      (intern "HIDDEN" package))))

(deftest resolve-symbol-designator-distinguishes-packages
  (testing "the same symbol name in two packages resolves to two symbols"
    (%ensure-fixture-packages)
    (let ((a (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-A:FOO"))
          (b (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-B:FOO")))
      (ok (and a b))
      (ok (not (eq a b)))
      (ok (string= "CL-MCP-SPEC-FIXTURE-A" (package-name (symbol-package a))))
      (ok (string= "CL-MCP-SPEC-FIXTURE-B" (package-name (symbol-package b)))))))

(deftest resolve-symbol-designator-uses-package-argument
  (testing "an unqualified name resolves inside the package argument"
    (%ensure-fixture-packages)
    (let ((a (resolve-symbol-designator "foo" :package "cl-mcp-spec-fixture-a")))
      (ok a)
      (ok (string= "CL-MCP-SPEC-FIXTURE-A" (package-name (symbol-package a)))))))

(deftest resolve-symbol-designator-single-colon-requires-external
  (testing "PKG:SYM refuses an internal symbol and says so"
    (%ensure-fixture-packages)
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-A:HIDDEN")
      (ok (null symbol))
      (ok (eq :not-external (getf reason :reason))))
    (testing "PKG::SYM accepts it"
      (ok (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-A::HIDDEN")))))

(deftest resolve-symbol-designator-never-interns
  (testing "an unknown name is reported, not created"
    (%ensure-fixture-packages)
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-A::NO-SUCH-SYMBOL-HERE")
      (ok (null symbol))
      (ok (eq :symbol-not-found (getf reason :reason))))
    ;; The point of the test: the image must be unchanged afterwards.
    (ok (null (find-symbol "NO-SUCH-SYMBOL-HERE" "CL-MCP-SPEC-FIXTURE-A")))))

(deftest resolve-symbol-designator-missing-package
  (testing "an unknown package is reported by name"
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator "NO-SUCH-PACKAGE-CL-MCP:FOO")
      (ok (null symbol))
      (ok (eq :package-not-found (getf reason :reason)))
      (ok (string= "NO-SUCH-PACKAGE-CL-MCP" (getf reason :package))))))

(deftest resolve-symbol-designator-rejects-escapes
  (testing "escaped names are refused rather than mis-parsed"
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator "|weird name|")
      (ok (null symbol))
      (ok (eq :malformed (getf reason :reason))))))

(deftest symbol-data-shape
  (testing "a symbol externalizes as package, name and qualified name"
    (%ensure-fixture-packages)
    (let ((data (symbol-data (resolve-symbol-designator
                              "CL-MCP-SPEC-FIXTURE-A:FOO"))))
      (ok (string= "CL-MCP-SPEC-FIXTURE-A" (getf data :package)))
      (ok (string= "FOO" (getf data :name)))
      (ok (string= "CL-MCP-SPEC-FIXTURE-A::FOO" (getf data :qualified))))))
```

`tests.lisp` の `(:import-from #:cl-mcp/tests/lisp-macroexpand-test)` の直後に
1 行足す。

```lisp
  (:import-from #:cl-mcp/tests/spec-adapter-core-test)
```

- [ ] **Step 2: 失敗を確認する**

Run: `rove tests/spec-adapter-core-test.lisp`
Expected: FAIL。`Component "cl-mcp/src/spec-adapter-core" not found`

- [ ] **Step 3: 最小の実装を書く**

`src/spec-adapter-core.lisp` を新規作成する。

```lisp
;;;; src/spec-adapter-core.lisp
;;;;
;;;; Late-bound access to cl-spec, and the pieces that turn its Lisp values
;;;; into something an MCP client can read.
;;;;
;;;; cl-mcp does not depend on cl-spec.  A project that has never heard of
;;;; cl-spec must keep working, so the API is resolved through FIND-PACKAGE
;;;; and FIND-SYMBOL at call time, the way SRC/PROXY.LISP reaches the pool and
;;;; SRC/CODE-CORE.LISP reaches SB-INTROSPECT.  Resolving fixed literal names
;;;; is not the same thing as interning a name that arrived from outside: this
;;;; file never interns anything.

(defpackage #:cl-mcp/src/spec-adapter-core
  (:use #:cl)
  (:export #:cl-spec-api
           #:cl-spec-api-p
           #:make-cl-spec-api
           #:cl-spec-api-functions
           #:cl-spec-api-classes
           #:cl-spec-api-version
           #:cl-spec-api-system-directory
           #:cl-spec-api-missing
           #:api-fn
           #:api-has-p
           #:api-class
           #:api-backend-available-p
           #:resolve-cl-spec-api
           #:resolve-symbol-designator
           #:symbol-data))

(in-package #:cl-mcp/src/spec-adapter-core)

;;; ---------------------------------------------------------------------------
;;; cl-spec API handles
;;; ---------------------------------------------------------------------------

(defstruct cl-spec-api
  "Handles on the cl-spec public API, resolved once per call.

FUNCTIONS and CLASSES are plists keyed by the adapter's own keywords rather
than by cl-spec's symbols, so a test can build one out of lambdas and exercise
every branch of the report layer in an image where cl-spec was never loaded."
  (functions nil :type list)
  (classes nil :type list)
  (version nil)
  (system-directory nil)
  (missing nil :type list))

(defparameter +required-functions+
  '((:semantic-data . "SEMANTIC-DATA")
    (:spec-data . "SPEC-DATA")
    (:property-data . "PROPERTY-DATA")
    (:properties-for . "PROPERTIES-FOR")
    (:run-property . "RUN-PROPERTY")
    (:backend-default-trials . "BACKEND-DEFAULT-TRIALS")
    (:result-status . "PROPERTY-RESULT-STATUS")
    (:result-trials . "PROPERTY-RESULT-TRIALS")
    (:result-seed . "PROPERTY-RESULT-SEED")
    (:result-profile . "PROPERTY-RESULT-PROFILE")
    (:result-counterexample . "PROPERTY-RESULT-COUNTEREXAMPLE")
    (:result-shrunk-counterexample . "PROPERTY-RESULT-SHRUNK-COUNTEREXAMPLE")
    (:result-condition . "PROPERTY-RESULT-CONDITION")
    (:result-elapsed . "PROPERTY-RESULT-ELAPSED"))
  "Adapter key to cl-spec function name.  Every one of these must be present
and fbound for the adapter to report itself usable.")

(defparameter +required-specials+
  '((:registry . "*REGISTRY*")
    (:generator-backend . "*GENERATOR-BACKEND*"))
  "Adapter key to cl-spec special name.  Each becomes a zero-argument reader
in the FUNCTIONS plist, so the report layer reads a special the same way it
calls a function and a stub can supply either.")

(defparameter +condition-classes+
  '((:cl-spec-error . "CL-SPEC-ERROR")
    (:no-generator-backend . "NO-GENERATOR-BACKEND")
    (:generator-unavailable . "GENERATOR-UNAVAILABLE")
    (:unknown-spec . "UNKNOWN-SPEC")
    (:unknown-property . "UNKNOWN-PROPERTY")
    (:not-implemented . "NOT-IMPLEMENTED"))
  "Adapter key to cl-spec condition name.  Absence is tolerated: a missing
condition class only costs a coarser classification, never an error.")

(defun %cl-spec-package ()
  "Return the CL-SPEC package, or NIL when cl-spec is not loaded."
  (find-package "CL-SPEC"))

(defun %system-version-and-directory ()
  "Return (values VERSION DIRECTORY) for the loaded cl-spec ASDF system.

Both are NIL when ASDF does not know the system.  Neither is guessed: a
version reported here has to have come from the system definition."
  (handler-case
      (let ((system (asdf:find-system "cl-spec" nil)))
        (if system
            (values (asdf:component-version system)
                    (ignore-errors
                     (namestring (asdf:system-source-directory system))))
            (values nil nil)))
    (error () (values nil nil))))

(defun resolve-cl-spec-api ()
  "Return (values API STATUS) for the cl-spec loaded in this image.

STATUS is :NOT-LOADED when the CL-SPEC package is absent, :INCOMPLETE when it
is present but some name this adapter needs is missing or unbound, and :OK
otherwise.  The three are kept apart because they call for different advice:
load the system, report a version mismatch, or proceed."
  (let ((package (%cl-spec-package)))
    (unless package
      (return-from resolve-cl-spec-api (values nil :not-loaded)))
    (let ((functions '())
          (classes '())
          (missing '()))
      (loop for (key . name) in +required-functions+
            for symbol = (find-symbol name package)
            do (if (and symbol (fboundp symbol))
                   (setf functions
                         (list* key (fdefinition symbol) functions))
                   (push name missing)))
      (loop for (key . name) in +required-specials+
            for symbol = (find-symbol name package)
            do (if (and symbol (boundp symbol))
                   (setf functions
                         (list* key
                                (let ((s symbol)) (lambda () (symbol-value s)))
                                functions))
                   (push name missing)))
      (loop for (key . name) in +condition-classes+
            for symbol = (find-symbol name package)
            do (when (and symbol (find-class symbol nil))
                 (setf classes (list* key symbol classes))))
      (multiple-value-bind (version directory) (%system-version-and-directory)
        (let ((api (make-cl-spec-api :functions functions
                                     :classes classes
                                     :version version
                                     :system-directory directory
                                     :missing (sort missing #'string<))))
          (values api (if missing :incomplete :ok)))))))

(defun api-has-p (api key)
  "Return true when API carries a handle for KEY."
  (and api (getf (cl-spec-api-functions api) key) t))

(defun api-fn (api key)
  "Return API's handle for KEY, signalling when it is absent.

Absence is a programming error in this adapter rather than a user-facing
condition: the report layer checks the API's status before it calls anything."
  (or (and api (getf (cl-spec-api-functions api) key))
      (error "cl-spec API handle ~S is not available." key)))

(defun api-class (api key)
  "Return the condition class symbol API carries for KEY, or NIL."
  (and api (getf (cl-spec-api-classes api) key)))

(defun api-backend-available-p (api)
  "Return true when a cl-spec generator backend is installed.

An absent backend is not an error: introspection works without one and only
execution needs it, so the two are reported separately."
  (and (api-has-p api :generator-backend)
       (handler-case (and (funcall (api-fn api :generator-backend)) t)
         (error () nil))))

;;; ---------------------------------------------------------------------------
;;; Symbol resolution
;;; ---------------------------------------------------------------------------
;;;
;;; The reader is not used here, and neither is INTERN.  A tool argument is
;;; text from outside the image: reading it would run reader macros, and
;;; interning it would let a caller grow the image by asking about names that
;;; do not exist.  FIND-PACKAGE and FIND-SYMBOL answer the only question that
;;; matters -- does this name already denote something -- and answer it
;;; without side effects.

(defun %find-package-named (name)
  "Return the package NAME denotes, trying NAME before its upcased form.

Exact first, because a package genuinely created with a lower-case name must
not be shadowed by an upper-case one that happens to exist."
  (or (find-package name)
      (find-package (string-upcase name))))

(defun %find-symbol-named (name package)
  "Return (values SYMBOL STATUS) for NAME in PACKAGE, exact form first."
  (multiple-value-bind (symbol status) (find-symbol name package)
    (if status
        (values symbol status)
        (find-symbol (string-upcase name) package))))

(defun %split-designator (designator)
  "Split DESIGNATOR into (values PACKAGE-PART NAME-PART DOUBLE-COLON-P).

PACKAGE-PART is NIL when DESIGNATOR carries no package marker.  A leading
colon names a keyword.  Returns NIL for a shape this parser will not accept."
  (let ((colon (position #\: designator)))
    (cond
      ((null colon) (values nil designator nil))
      ((zerop colon)
       ;; :FOO and ::FOO both name a keyword.
       (let ((start (if (and (> (length designator) 1)
                             (char= #\: (char designator 1)))
                        2
                        1)))
         (values "KEYWORD" (subseq designator start) t)))
      (t
       (let* ((double (and (< (1+ colon) (length designator))
                           (char= #\: (char designator (1+ colon)))))
              (start (+ colon (if double 2 1))))
         (values (subseq designator 0 colon)
                 (subseq designator start)
                 double))))))

(defun resolve-symbol-designator (designator &key package)
  "Return (values SYMBOL NIL) for DESIGNATOR, or (values NIL REASON).

DESIGNATOR is \"SYM\", \"PKG:SYM\" or \"PKG::SYM\".  PACKAGE is consulted only
for the unqualified form and defaults to COMMON-LISP-USER.  A single colon
accepts only an external symbol, as the reader would; a double colon accepts
an internal one.

REASON is a plist headed by :REASON, one of :MALFORMED, :PACKAGE-NOT-FOUND,
:SYMBOL-NOT-FOUND or :NOT-EXTERNAL, carrying the names involved so the caller
can say which package and which name it looked in."
  (unless (and (stringp designator) (plusp (length designator)))
    (return-from resolve-symbol-designator
      (values nil (list :reason :malformed :input designator
                        :detail "symbol must be a non-empty string"))))
  (when (or (find #\| designator) (find #\\ designator))
    (return-from resolve-symbol-designator
      (values nil (list :reason :malformed :input designator
                        :detail "escaped symbol names are not supported"))))
  (multiple-value-bind (package-part name-part) (%split-designator designator)
    (when (or (zerop (length name-part)) (find #\: name-part))
      (return-from resolve-symbol-designator
        (values nil (list :reason :malformed :input designator
                          :detail "expected SYM, PKG:SYM or PKG::SYM"))))
    (let* ((double-colon (and package-part
                              (search "::" designator)
                              (= (search "::" designator)
                                 (length package-part))))
           (package-name (or package-part
                             (and (stringp package) (plusp (length package))
                                  package)
                             "COMMON-LISP-USER"))
           (found-package (%find-package-named package-name)))
      (unless found-package
        (return-from resolve-symbol-designator
          (values nil (list :reason :package-not-found
                            :package package-name :input designator))))
      (multiple-value-bind (symbol status)
          (%find-symbol-named name-part found-package)
        (cond
          ((null status)
           (values nil (list :reason :symbol-not-found
                             :package (package-name found-package)
                             :name (string-upcase name-part)
                             :input designator)))
          ;; A qualified name written with one colon must name an external
          ;; symbol, exactly as the reader requires.  Accepting an internal
          ;; one here would make PKG:SYM and PKG::SYM interchangeable, and
          ;; the distinction is the only thing telling a caller that it is
          ;; reaching past a package's own boundary.
          ((and package-part (not double-colon) (not (eq status :external)))
           (values nil (list :reason :not-external
                             :package (package-name found-package)
                             :name (symbol-name symbol)
                             :input designator)))
          (t (values symbol nil)))))))

(defun symbol-data (symbol)
  "Return SYMBOL as the plist every response uses for a symbol.

  (:package <string-or-nil> :name <string> :qualified <string>)

Package and name are carried separately because two symbols with the same name
in different packages are different contracts, and a single printed string
would let a consumer conflate them (cl-spec specification 72.6)."
  (let ((package (symbol-package symbol)))
    (list :package (when package (package-name package))
          :name (symbol-name symbol)
          :qualified (if package
                         (format nil "~A::~A"
                                 (package-name package) (symbol-name symbol))
                         (format nil "#:~A" (symbol-name symbol))))))
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `rove tests/spec-adapter-core-test.lisp`
Expected: PASS。7 テストすべて緑。

- [ ] **Step 5: API 解決のテストを足す**

`tests/spec-adapter-core-test.lisp` の `:import-from` に
`#:resolve-cl-spec-api #:make-cl-spec-api #:api-has-p #:api-fn #:api-class
#:api-backend-available-p` を追加し、末尾に以下を足す。

```lisp
(deftest resolve-cl-spec-api-reports-absence
  (testing "a stub API built from lambdas answers api-has-p"
    (let ((api (make-cl-spec-api
                :functions (list :run-property (lambda (&rest ignored)
                                                 (declare (ignore ignored))
                                                 :called)
                                 :generator-backend (lambda () nil)))))
      (ok (api-has-p api :run-property))
      (ok (not (api-has-p api :semantic-data)))
      (ok (eq :called (funcall (api-fn api :run-property) 'x)))
      (ok (null (api-class api :unknown-property)))
      (testing "a NIL backend reads as unavailable, not as an error"
        (ok (not (api-backend-available-p api)))))))

(deftest resolve-cl-spec-api-live-image
  (testing "resolution against this image reports one of three states"
    (multiple-value-bind (api status) (resolve-cl-spec-api)
      (ok (member status '(:ok :not-loaded :incomplete)))
      (if (eq status :not-loaded)
          (ok (null api))
          (progn
            (ok api)
            ;; :INCOMPLETE must say which names were missing; a bare status
            ;; would leave a version mismatch undiagnosable.
            (ok (or (eq status :ok)
                    (plusp (length (cl-mcp/src/spec-adapter-core:cl-spec-api-missing
                                    api))))))))))
```

- [ ] **Step 6: テストが通ることを確認する**

Run: `rove tests/spec-adapter-core-test.lisp`
Expected: PASS。9 テスト。

- [ ] **Step 7: lint とコミット**

```bash
mallet src/spec-adapter-core.lisp tests/spec-adapter-core-test.lisp
git add src/spec-adapter-core.lisp tests/spec-adapter-core-test.lisp tests.lisp
git commit -m "spec-adapter: reach cl-spec by name, and resolve symbols without interning them"
```

---

### Task 2: 値の外部表現と定義 digest

**Files:**
- Modify: `src/spec-adapter-core.lisp`
- Modify: `tests/spec-adapter-core-test.lisp`

**Interfaces:**
- Consumes: Task 1 の `cl-spec-api`、`api-fn`
- Produces:
  - `(externalize-value value &key max-chars)` → `(:printed s :printed-complete bool :omitted-chars n :type "integer" :object-id n-or-nil)`
  - `(digest-string string)` → 16 桁 hex 文字列
  - `(printed-for-digest form)` → 決定的な printed 表現
  - `(definition-digest api property-name registry)` → 16 桁 hex 文字列 or NIL

- [ ] **Step 1: 失敗するテストを書く**

`tests/spec-adapter-core-test.lisp` の `:import-from` に
`#:externalize-value #:digest-string #:printed-for-digest #:definition-digest`
を追加し、末尾に足す。

```lisp
(deftest externalize-value-keeps-integers-exact
  (testing "a bignum beyond JSON's safe integer survives as text"
    (let* ((seed 3963993791726803706)
           (data (externalize-value seed)))
      (ok (string= "3963993791726803706" (getf data :printed)))
      (ok (getf data :printed-complete))
      (ok (string= "integer" (getf data :type)))
      ;; A primitive is not registered for inspection: there is nothing to
      ;; drill into and an id would only invite a pointless round trip.
      (ok (null (getf data :object-id))))))

(deftest externalize-value-marks-truncation
  (testing "a value past max-chars is marked incomplete and counted"
    (let* ((big (make-list 400 :initial-element :aaaaaaaa))
           (data (externalize-value big :max-chars 40)))
      (ok (not (getf data :printed-complete)))
      (ok (plusp (getf data :omitted-chars)))
      (ok (string= "cons" (getf data :type)))
      (testing "a compound value gets an object id for inspect-object"
        (ok (integerp (getf data :object-id)))))))

(deftest externalize-value-handles-circularity
  (testing "a circular structure prints rather than hanging"
    (let ((cycle (list 1 2 3)))
      (setf (cdr (last cycle)) cycle)
      (let ((data (externalize-value cycle :max-chars 200)))
        (ok (stringp (getf data :printed)))
        (ok (search "#1=" (getf data :printed)))))))

(deftest digest-string-is-stable-and-sensitive
  (testing "the same text digests the same, different text does not"
    (ok (string= (digest-string "abc") (digest-string "abc")))
    (ok (not (string= (digest-string "abc") (digest-string "abd"))))
    (ok (= 16 (length (digest-string "abc"))))
    (ok (every (lambda (c) (find c "0123456789abcdef")) (digest-string "abc")))))

(deftest printed-for-digest-qualifies-symbols
  (testing "symbols print with their package regardless of *package*"
    (%ensure-fixture-packages)
    (let* ((symbol (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-A:FOO"))
           (in-cl-user (let ((*package* (find-package "COMMON-LISP-USER")))
                         (printed-for-digest (list symbol))))
           (in-fixture (let ((*package* (find-package "CL-MCP-SPEC-FIXTURE-A")))
                         (printed-for-digest (list symbol)))))
      (ok (string= in-cl-user in-fixture))
      (ok (search "CL-MCP-SPEC-FIXTURE-A" in-cl-user)))))

(deftest definition-digest-follows-spec-references
  (testing "a change in a referenced spec changes the property's digest"
    (let* ((property-plist
             (list :name 'prop :kind :invariant :targets '(add)
                   :arguments (list (list :variable 'a
                                          :spec (list :name nil :kind :reference
                                                      :target 'small-int)))
                   :body '((= a a))))
           (spec-v1 (list :name 'small-int :kind :range :min 0 :max 100))
           (spec-v2 (list :name 'small-int :kind :range :min 0 :max 999))
           (api-for (lambda (spec-plist)
                      (make-cl-spec-api
                       :functions
                       (list :property-data
                             (lambda (name &key registry)
                               (declare (ignore name registry))
                               property-plist)
                             :spec-data
                             (lambda (name &key registry)
                               (declare (ignore name registry))
                               spec-plist))))))
      (let ((d1 (definition-digest (funcall api-for spec-v1) 'prop nil))
            (d2 (definition-digest (funcall api-for spec-v2) 'prop nil)))
        (ok (stringp d1))
        (ok (not (string= d1 d2)))
        (testing "and the same inputs digest the same twice"
          (ok (string= d1 (definition-digest (funcall api-for spec-v1)
                                             'prop nil))))))))

(deftest definition-digest-tolerates-unresolved-reference
  (testing "a reference to a spec that is not registered does not signal"
    (let ((api (make-cl-spec-api
                :functions
                (list :property-data
                      (lambda (name &key registry)
                        (declare (ignore name registry))
                        (list :name 'prop
                              :arguments (list (list :variable 'a
                                                     :spec (list :kind :reference
                                                                 :target 'gone)))))
                      :spec-data
                      (lambda (name &key registry)
                        (declare (ignore registry))
                        (error "No spec named ~S is registered." name))))))
      (ok (stringp (definition-digest api 'prop nil))))))
```

- [ ] **Step 2: 失敗を確認する**

Run: `rove tests/spec-adapter-core-test.lisp`
Expected: FAIL。`EXTERNALIZE-VALUE` などが未定義。

- [ ] **Step 3: 実装を書く**

`src/spec-adapter-core.lisp` の `defpackage` に import と export を足す。

```lisp
  (:import-from #:cl-mcp/src/object-registry
                #:inspectable-p
                #:register-object)
  (:import-from #:cl-mcp/src/utils/bounded-stream
                #:make-bounded-output-stream
                #:bounded-output-string
                #:bounded-output-dropped)
```

export に追加する。

```lisp
           #:externalize-value
           #:digest-string
           #:printed-for-digest
           #:definition-digest
```

ファイル末尾に以下を足す。

```lisp
;;; ---------------------------------------------------------------------------
;;; Value externalization
;;; ---------------------------------------------------------------------------

(defun %value-type-name (value)
  "Return a short, stable type name for VALUE.

TYPE-OF is not used directly because on SBCL it answers a fixnum with its
whole range -- (INTEGER 0 4611686018427387903) -- which tells a reader
nothing it wanted to know.  The common shapes are named outright and the
rest fall back to the head of TYPE-OF, which is the class name for a CLOS
instance or a structure."
  (typecase value
    (null "null")
    (integer "integer")
    (ratio "ratio")
    (float "float")
    (complex "complex")
    (string "string")
    (character "character")
    (symbol "symbol")
    (cons "cons")
    (hash-table "hash-table")
    (function "function")
    (package "package")
    (pathname "pathname")
    (vector "vector")
    (array "array")
    (t (let ((name (type-of value)))
         (string-downcase (princ-to-string (if (consp name) (first name) name)))))))

(defun %print-bounded (value max-chars)
  "Return (values TEXT DROPPED) for VALUE, retaining at most MAX-CHARS.

The bound is on what is *retained*, not on what is produced: printing an
unbounded structure into a string and cutting it afterwards costs the whole
structure in heap first, and a generated counterexample can be arbitrarily
large.  *PRINT-CIRCLE* is on so a shared or circular value prints as #n=
notation instead of running forever."
  (let ((stream (make-bounded-output-stream (max 1 max-chars))))
    (handler-case
        (let ((*print-circle* t)
              (*print-readably* nil)
              (*print-pretty* nil)
              (*print-level* nil)
              (*print-length* nil))
          (prin1 value stream)
          (let ((dropped (bounded-output-dropped stream)))
            (values (bounded-output-string stream) dropped)))
      (serious-condition (condition)
        (values (format nil "#<error printing a ~A: ~A>"
                        (%value-type-name value) (type-of condition))
                0)))))

(defun externalize-value (value &key (max-chars 2000))
  "Return VALUE as the plist every response uses for a generated value.

  (:printed <string> :printed-complete <boolean> :omitted-chars <integer>
   :type <string> :object-id <integer-or-nil>)

No Lisp value is emitted as a JSON number: an integer seed or a rational can
exceed what a JSON consumer holds exactly, and a rounded number that looks
like a value is worse than text that admits to being text.

:PRINTED-COLETE NIL means the text was cut at MAX-CHARS.  Such text is a
display preview and NOT a value that can be read back -- the distinction
cl-spec specification 72.6 asks for.  :OBJECT-ID, when non-NIL, is the
object-registry id the existing inspect-object tool drills into."
  (multiple-value-bind (printed dropped) (%print-bounded value max-chars)
    (list :printed printed
          :printed-complete (zerop dropped)
          :omitted-chars dropped
          :type (%value-type-name value)
          :object-id (when (inspectable-p value)
                       (ignore-errors (register-object value))))))

;;; ---------------------------------------------------------------------------
;;; Definition digest
;;; ---------------------------------------------------------------------------

(defconstant +fnv-offset-basis+ 14695981039346656037
  "FNV-1a 64-bit offset basis.")

(defconstant +fnv-prime+ 1099511628211
  "FNV-1a 64-bit prime.")

(defconstant +fnv-mask+ #xFFFFFFFFFFFFFFFF
  "Mask keeping the FNV-1a accumulator at 64 bits.")

(defun digest-string (string)
  "Return the FNV-1a 64-bit digest of STRING as 16 lower-case hex digits.

FNV-1a rather than a real hash because this identifies a definition for a
human and an agent to compare, not for anything to trust: a 64-bit
non-cryptographic digest is enough to notice that a property changed, and
adding a crypto dependency to cl-mcp for it would not be."
  (let ((hash +fnv-offset-basis+))
    (loop for byte across (sb-ext:string-to-octets string :external-format :utf-8)
          do (setf hash (logand (* (logxor hash byte) +fnv-prime+) +fnv-mask+)))
    (format nil "~(~16,'0x~)" hash)))

(defun printed-for-digest (form)
  "Return FORM printed the same way regardless of the caller's environment.

*PACKAGE* is bound to KEYWORD so every symbol prints with its home package:
the same form read in two packages must not digest differently, and a symbol
printed without its package would let two same-named symbols collide."
  (handler-case
      (let ((*package* (find-package "KEYWORD"))
            (*print-circle* t)
            (*print-pretty* nil)
            (*print-readably* nil)
            (*print-level* nil)
            (*print-length* nil)
            (*print-base* 10)
            (*print-radix* nil)
            (*print-case* :upcase)
            (*read-default-float-format* 'double-float))
        (prin1-to-string form))
    (serious-condition (condition)
      (format nil "#<unprintable: ~A>" (type-of condition)))))

(defun %collect-spec-references (spec-plist accumulator)
  "Push every :REFERENCE target reachable from SPEC-PLIST onto ACCUMULATOR.

Returns the accumulator.  Walks :CHILDREN, which is how SPEC-DATA nests an
AND or a LIST-OF node."
  (when (listp spec-plist)
    (when (eq :reference (getf spec-plist :kind))
      (let ((target (getf spec-plist :target)))
        (when target (pushnew target accumulator))))
    (dolist (child (getf spec-plist :children))
      (setf accumulator (%collect-spec-references child accumulator))))
  accumulator)

(defun definition-digest (api property-name registry)
  "Return a digest of PROPERTY-NAME's definition, or NIL when it cannot be read.

The digest covers the property's own data and the data of every named spec
reachable from its arguments, transitively.  Covering only the property would
miss the case that matters most in practice: the property text is untouched
but the spec it generates from was widened, so the same seed now explores a
different input domain and the run is not a reproduction of the earlier one.

A reference to a spec that is not registered is recorded as unresolved rather
than skipped, so the digest still changes if it is defined later."
  (handler-case
      (let* ((property (funcall (api-fn api :property-data)
                                property-name :registry registry))
             (pending '())
             (seen (make-hash-table :test #'eq))
             (specs '()))
        (dolist (argument (getf property :arguments))
          (setf pending (%collect-spec-references (getf argument :spec) pending)))
        (loop while pending
              for name = (pop pending)
              unless (gethash name seen)
                do (setf (gethash name seen) t)
                   (let ((data (handler-case
                                   (funcall (api-fn api :spec-data)
                                            name :registry registry)
                                 (error () (list :unresolved-reference name)))))
                     (push (cons name data) specs)
                     (setf pending (%collect-spec-references data pending))))
        (digest-string
         (printed-for-digest
          (list :property property
                :specs (sort specs #'string<
                             :key (lambda (entry)
                                    (princ-to-string (car entry))))))))
    (error () nil)))
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `rove tests/spec-adapter-core-test.lisp`
Expected: PASS。16 テスト。

- [ ] **Step 5: docstring の typo を直す**

`externalize-value` の docstring 中 `:PRINTED-COLETE` を `:PRINTED-COMPLETE`
に修正する。

- [ ] **Step 6: lint とコミット**

```bash
mallet src/spec-adapter-core.lisp tests/spec-adapter-core-test.lisp
git add src/spec-adapter-core.lisp tests/spec-adapter-core-test.lisp
git commit -m "spec-adapter: externalize values without losing precision, and digest what a replay depends on"
```

---

### Task 3: 発見と取得の操作層

**Files:**
- Create: `src/spec-adapter-report.lisp`
- Create: `tests/spec-adapter-report-test.lisp`
- Modify: `tests.lisp`

**Interfaces:**
- Consumes: Task 1-2 の `cl-spec-api` / `api-fn` / `api-has-p` / `api-class` / `api-backend-available-p` / `resolve-symbol-designator` / `symbol-data` / `externalize-value` / `definition-digest`
- Produces:
  - `(environment-data api api-status)` → plist
  - `(unavailable-report api-status environment)` → plist
  - `(symbol-report api api-status designator &key package include-runtime)` → plist
  - `(describe-report api api-status kind name &key package max-chars)` → plist
  - すべての plist は `:status` を必ず持つ。`:status` は
    `:ok` / `:cl-spec-not-loaded` / `:cl-spec-incomplete` / `:unresolved-symbol` /
    `:not-registered` / `:unsupported` / `:invalid-arguments` のいずれか

- [ ] **Step 1: 失敗するテストを書く**

`tests/spec-adapter-report-test.lisp` を新規作成する。

```lisp
;;;; tests/spec-adapter-report-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/spec-adapter-report.  Every cl-spec call goes
;;;; through the CL-SPEC-API struct, so the whole layer is exercised here with
;;;; lambdas and no cl-spec in the image.  That is deliberate: the branches
;;;; that matter most -- cl-spec absent, backend absent, zero properties,
;;;; timeout -- are exactly the ones a suite depending on a healthy cl-spec
;;;; could never reach.

(defpackage #:cl-mcp/tests/spec-adapter-report-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:make-cl-spec-api)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:environment-data
                #:symbol-report
                #:describe-report))

(in-package #:cl-mcp/tests/spec-adapter-report-test)

(defun %fixture-package ()
  "Return a package holding the symbols these tests talk about."
  (let ((package (or (find-package "CL-MCP-SPEC-REPORT-FIXTURE")
                     (make-package "CL-MCP-SPEC-REPORT-FIXTURE" :use '()))))
    (dolist (name '("ADD" "SMALL-INT" "ADD-COMMUTES" "LONELY"))
      (export (intern name package) package))
    package))

(defun %sym (name)
  "Return the fixture package's symbol named NAME."
  (find-symbol name (%fixture-package)))

(defun %stub-api (&rest overrides)
  "Return a CL-SPEC-API whose handles answer a small fixed registry."
  (make-cl-spec-api
   :version "0.1.0"
   :system-directory "/tmp/cl-spec/"
   :functions
   (append
    overrides
    (list
     :registry (lambda () :stub-registry)
     :generator-backend (lambda () :stub-backend)
     :backend-default-trials (lambda (backend) (declare (ignore backend)) 100)
     :semantic-data
     (lambda (symbol &key registry)
       (declare (ignore registry))
       (list :symbol symbol
             :package (package-name (symbol-package symbol))
             :spec nil :function-spec nil :property nil
             :properties-about (if (eq symbol (%sym "ADD"))
                                   (list (%sym "ADD-COMMUTES"))
                                   nil)))
     :properties-for
     (lambda (symbol &optional registry)
       (declare (ignore registry))
       (if (eq symbol (%sym "ADD")) (list (%sym "ADD-COMMUTES")) nil))
     :property-data
     (lambda (name &key registry)
       (declare (ignore registry))
       (unless (eq name (%sym "ADD-COMMUTES"))
         (error "No property named ~S is registered." name))
       (list :name (%sym "ADD-COMMUTES")
             :kind :commutativity
             :targets (list (%sym "ADD"))
             :tags (list :math)
             :documentation "Addition commutes."
             :trials (list :normal 100)
             :arguments (list (list :variable (intern "A" (%fixture-package))
                                    :spec (list :name nil :kind :reference
                                                :target (%sym "SMALL-INT")
                                                :source-form (%sym "SMALL-INT")
                                                :source-location nil)))
             :body '((= (add a a) (add a a)))
             :source-form '(defproperty add-commutes ((a small-int)) (= 1 1))
             :source-location (list :file "fixture.lisp" :package "FIXTURE")
             :metadata (list :shrink t)))
     :spec-data
     (lambda (name &key registry)
       (declare (ignore registry))
       (unless (eq name (%sym "SMALL-INT"))
         (error "No spec named ~S is registered." name))
       (list :name (%sym "SMALL-INT") :kind :range :base-type nil
             :min 0 :max 100
             :source-form '(range 0 100) :source-location nil))))))

(deftest environment-data-separates-not-loaded-from-incomplete
  (testing "cl-spec absent is reported as such, with no invented version"
    (let ((data (environment-data nil :not-loaded)))
      (ok (not (getf data :cl-spec-loaded)))
      (ok (null (getf data :cl-spec-version)))
      (ok (stringp (getf data :lisp)))))
  (testing "an incomplete API names what was missing"
    (let* ((api (make-cl-spec-api :missing (list "RUN-PROPERTY")))
           (data (environment-data api :incomplete)))
      (ok (not (getf data :cl-spec-loaded)))
      (ok (equal (list "RUN-PROPERTY") (getf data :missing))))))

(deftest symbol-report-without-cl-spec
  (testing "an absent cl-spec is not reported as an absent contract"
    (let ((report (symbol-report nil :not-loaded "cl:car")))
      (ok (eq :cl-spec-not-loaded (getf report :status)))
      (ok (search "load-system" (getf report :message)))
      (ok (search "not" (string-downcase (getf report :message)))))))

(deftest symbol-report-unresolved-symbol
  (testing "a name that denotes nothing is diagnosed, not guessed"
    (let ((report (symbol-report (%stub-api) :ok "NO-SUCH-PACKAGE-XYZ:FOO")))
      (ok (eq :unresolved-symbol (getf report :status)))
      (ok (eq :package-not-found (getf (getf report :reason) :reason))))))

(deftest symbol-report-lists-related-properties
  (testing "a symbol with an :about property reports it by package and name"
    (let ((report (symbol-report (%stub-api) :ok
                                 "CL-MCP-SPEC-REPORT-FIXTURE:ADD"
                                 :include-runtime nil)))
      (ok (eq :ok (getf report :status)))
      (ok (string= "ADD" (getf (getf report :symbol) :name)))
      (let ((properties (getf report :properties)))
        (ok (= 1 (length properties)))
        (let ((first-property (first properties)))
          (ok (string= "ADD-COMMUTES" (getf (getf first-property :name) :name)))
          (ok (eq :commutativity (getf first-property :kind)))
          (ok (getf first-property :shrink-enabled))
          (testing "the body is summarized, not inlined, and says so"
            (ok (getf first-property :body-omitted))
            (ok (= 1 (getf first-property :body-forms)))
            (ok (stringp (getf first-property :definition-digest)))))))))

(deftest symbol-report-zero-properties-is-not-a-clean-bill
  (testing "no registration is reported as no registration"
    (let ((report (symbol-report (%stub-api) :ok
                                 "CL-MCP-SPEC-REPORT-FIXTURE:LONELY"
                                 :include-runtime nil)))
      (ok (eq :ok (getf report :status)))
      (ok (null (getf report :properties)))
      (ok (getf report :nothing-registered)))))

(deftest describe-report-property-includes-body
  (testing "detail returns the body the summary omitted"
    (let ((report (describe-report (%stub-api) :ok "property"
                                   "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES")))
      (ok (eq :ok (getf report :status)))
      (ok (search "ADD" (getf report :body)))
      (ok (getf report :body-complete))
      (ok (search "DEFPROPERTY" (string-upcase (getf report :source-form)))))))

(deftest describe-report-truncates-loudly
  (testing "a body past max-chars is cut and the cut is reported"
    (let ((report (describe-report (%stub-api) :ok "property"
                                   "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES"
                                   :max-chars 5)))
      (ok (not (getf report :body-complete)))
      (ok (plusp (getf report :body-omitted-chars))))))

(deftest describe-report-unknown-name
  (testing "an unregistered name is not-registered, not an error"
    (let ((report (describe-report (%stub-api) :ok "spec"
                                   "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :not-registered (getf report :status))))))

(deftest describe-report-function-spec-is-unsupported
  (testing "function specs name the cl-spec API that is missing"
    (let ((report (describe-report (%stub-api) :ok "function-spec"
                                   "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :unsupported (getf report :status)))
      (ok (search "function-spec-data" (getf report :message))))))

(deftest describe-report-rejects-unknown-kind
  (testing "an unrecognized kind is an argument error"
    (let ((report (describe-report (%stub-api) :ok "generator"
                                   "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :invalid-arguments (getf report :status))))))
```

`tests.lisp` に 1 行足す。

```lisp
  (:import-from #:cl-mcp/tests/spec-adapter-report-test)
```

- [ ] **Step 2: 失敗を確認する**

Run: `rove tests/spec-adapter-report-test.lisp`
Expected: FAIL。`Component "cl-mcp/src/spec-adapter-report" not found`

- [ ] **Step 3: 実装を書く**

`src/spec-adapter-report.lisp` を新規作成する。

```lisp
;;;; src/spec-adapter-report.lisp
;;;;
;;;; The three operations the cl-spec tools expose, as plists.
;;;;
;;;; Everything here answers with a plist headed by :STATUS rather than by
;;;; signalling.  cl-spec signals UNKNOWN-PROPERTY for a name it does not
;;;; know, and an agent asking "is there a contract for this symbol?" must be
;;;; told "no" rather than handed a condition -- "not registered", "cl-spec is
;;;; not loaded" and "this cl-spec cannot do that" are three different answers
;;;; and none of them is an error in the caller.

(defpackage #:cl-mcp/src/spec-adapter-report
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:cl-spec-api-version
                #:cl-spec-api-system-directory
                #:cl-spec-api-missing
                #:api-fn
                #:api-has-p
                #:api-class
                #:api-backend-available-p
                #:resolve-symbol-designator
                #:symbol-data
                #:externalize-value
                #:definition-digest
                #:printed-for-digest)
  (:import-from #:cl-mcp/src/code-core
                #:code-describe-symbol)
  (:export #:environment-data
           #:unavailable-report
           #:symbol-report
           #:describe-report))

(in-package #:cl-mcp/src/spec-adapter-report)

;;; ---------------------------------------------------------------------------
;;; Environment
;;; ---------------------------------------------------------------------------

(defun %backend-name (api)
  "Return the installed generator backend's class name, or NIL."
  (handler-case
      (let ((backend (and (api-has-p api :generator-backend)
                          (funcall (api-fn api :generator-backend)))))
        (when backend
          (if (symbolp backend)
              (princ-to-string backend)
              (let ((name (class-name (class-of backend))))
                (format nil "~A:~A"
                        (package-name (symbol-package name))
                        (symbol-name name))))))
    (error () nil)))

(defun %registry-description (api)
  "Return a short printed description of the registry in effect, or NIL."
  (handler-case
      (let ((registry (and (api-has-p api :registry)
                           (funcall (api-fn api :registry)))))
        (when registry
          (let ((*print-level* 1) (*print-length* 2) (*print-readably* nil))
            (princ-to-string registry))))
    (error () nil)))

(defun environment-data (api api-status)
  "Return what this image can say about the cl-spec it is talking to.

Attached to every response.  An agent that cannot tell a missing system from a
missing registration will read an empty answer as a clean bill of health, and
that is the single most expensive mistake available here."
  (list :cl-spec-loaded (eq api-status :ok)
        :cl-spec-status api-status
        :cl-spec-version (and api (cl-spec-api-version api))
        :cl-spec-system-directory (and api (cl-spec-api-system-directory api))
        :generator-backend (and api (%backend-name api))
        :backend-available (and api (api-backend-available-p api))
        :registry (and api (%registry-description api))
        :missing (and api (cl-spec-api-missing api))
        :lisp (format nil "~A ~A"
                      (lisp-implementation-type)
                      (lisp-implementation-version))))

(defparameter +not-loaded-message+
  "cl-spec is not loaded in this session's worker. Run load-system with system \
\"cl-spec/check-it\" to get introspection and property execution, or \
\"cl-spec\" for introspection only. Until then this tool cannot tell whether \
anything is registered: an empty answer here is NOT evidence that the symbol \
has no contract."
  "Said when the CL-SPEC package is absent.")

(defparameter +incomplete-message+
  "cl-spec is loaded but does not provide every name this adapter needs; see \
the missing list in environment. This is a version mismatch between cl-mcp and \
cl-spec, not a statement about the symbol."
  "Said when cl-spec is present but a required name is missing.")

(defun unavailable-report (api-status environment)
  "Return the plist answering a call cl-spec cannot serve at all."
  (list :status (if (eq api-status :not-loaded)
                    :cl-spec-not-loaded
                    :cl-spec-incomplete)
        :message (if (eq api-status :not-loaded)
                     +not-loaded-message+
                     +incomplete-message+)
        :environment environment))

;;; ---------------------------------------------------------------------------
;;; Shared helpers
;;; ---------------------------------------------------------------------------

(defun %print-bounded-form (form max-chars)
  "Return (values TEXT COMPLETE-P OMITTED-CHARS) for FORM at MAX-CHARS.

Uses the digest printer so a body reads the same way it is hashed, which is
what lets a caller compare the two by eye."
  (let ((text (printed-for-digest form)))
    (if (<= (length text) max-chars)
        (values text t 0)
        (values (subseq text 0 max-chars) nil (- (length text) max-chars)))))

(defun %runtime-data (symbol)
  "Return (values RUNTIME REASON) for SYMBOL from cl-mcp's own introspection.

This is the half of specification 28's describe_symbol that cl-spec's registry
does not hold: the signature, the docstring and where the definition lives.
A symbol with no binding at all is not an error here -- a property may be
registered about a symbol that is not yet defined -- so the reason is
reported and the caller carries on."
  (handler-case
      (multiple-value-bind (name type arglist documentation path line)
          (code-describe-symbol (format nil "~A::~A"
                                        (package-name (symbol-package symbol))
                                        (symbol-name symbol)))
        (declare (ignore name))
        (values (list :type type
                      :arglist arglist
                      :documentation documentation
                      :source-file path
                      :source-line line)
                nil))
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun %spec-summary (spec-plist)
  "Return the one-line summary of an argument's spec used in a listing.

The whole spec tree belongs in spec-describe; here a caller needs only enough
to see which spec generates the argument."
  (list :kind (getf spec-plist :kind)
        :name (let ((name (getf spec-plist :name)))
                (when name (symbol-data name)))
        :target (let ((target (getf spec-plist :target)))
                  (when target (symbol-data target)))))

(defun %property-summary (api name registry)
  "Return the listing entry for property NAME, with its body omitted.

The body and the source form are deliberately left out: PROPERTY-DATA carries
the author's whole DEFPROPERTY form, and inlining that for every property a
symbol has would make one response's size depend on how much its author
wrote.  BODY-OMITTED says so rather than leaving a reader to infer it."
  (handler-case
      (let ((data (funcall (api-fn api :property-data) name :registry registry)))
        (list :name (symbol-data name)
              :kind (getf data :kind)
              :tags (getf data :tags)
              :targets (mapcar #'symbol-data (getf data :targets))
              :documentation (getf data :documentation)
              :arguments (loop for argument in (getf data :arguments)
                               collect (list :variable
                                             (symbol-data (getf argument :variable))
                                             :spec (%spec-summary
                                                    (getf argument :spec))))
              :trials-table (let ((table (getf data :trials)))
                              (when table (printed-for-digest table)))
              :shrink-enabled (getf (getf data :metadata) :shrink)
              :source-location (getf data :source-location)
              :definition-digest (definition-digest api name registry)
              :body-forms (length (getf data :body))
              :body-omitted t
              :detail-via "spec-describe kind=property"))
    (error (condition)
      (list :name (symbol-data name)
            :unavailable-reason (princ-to-string condition)))))

;;; ---------------------------------------------------------------------------
;;; spec-symbol
;;; ---------------------------------------------------------------------------

(defun symbol-report (api api-status designator &key package (include-runtime t))
  "Return the plist behind the spec-symbol tool.

Joins what cl-spec's registry knows about DESIGNATOR with what this image
knows about it (specification 27-28: describe_symbol is a join, and the half
that is a signature and a source location belongs to cl-mcp)."
  (let ((environment (environment-data api api-status)))
    (unless (eq api-status :ok)
      (return-from symbol-report (unavailable-report api-status environment)))
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator designator :package package)
      (unless symbol
        (return-from symbol-report
          (list :status :unresolved-symbol
                :reason reason
                :input designator
                :environment environment)))
      (let* ((registry (funcall (api-fn api :registry)))
             (routing (funcall (api-fn api :semantic-data) symbol
                               :registry registry))
             (about (getf routing :properties-about)))
        (multiple-value-bind (runtime runtime-reason)
            (if include-runtime (%runtime-data symbol) (values nil nil))
          (list :status :ok
                :symbol (symbol-data symbol)
                :runtime runtime
                :runtime-unavailable-reason
                (cond (runtime nil)
                      ((not include-runtime) "include_runtime was false")
                      (t runtime-reason))
                :registry
                (list :spec (let ((s (getf routing :spec)))
                              (when s (symbol-data s)))
                      :function-spec (let ((s (getf routing :function-spec)))
                                       (when s (symbol-data s)))
                      :property (let ((s (getf routing :property)))
                                  (when s (symbol-data s)))
                      :properties-about (mapcar #'symbol-data about))
                :properties (loop for name in about
                                  collect (%property-summary api name registry))
                :nothing-registered (not (or about
                                             (getf routing :spec)
                                             (getf routing :function-spec)
                                             (getf routing :property)))
                :notes (append
                        (list "properties_about lists direct (:about ...) registrations only")
                        (when (getf routing :property)
                          (list "this symbol is itself a registered property; run it with property=")))
                :environment environment))))))

;;; ---------------------------------------------------------------------------
;;; spec-describe
;;; ---------------------------------------------------------------------------

(defparameter +function-spec-unsupported-message+
  "cl-spec provides no function-spec-data projection in this revision, and \
defspec-function is still a stub, so there is nothing to describe. cl-mcp will \
not assemble one out of the individual readers: that would duplicate cl-spec's \
introspection responsibility on this side of the boundary."
  "Said when spec-describe is asked for a function spec.")

(defun %describe-property (api name registry max-chars)
  "Return the detail plist for property NAME."
  (let ((data (funcall (api-fn api :property-data) name :registry registry)))
    (multiple-value-bind (body complete omitted)
        (%print-bounded-form (getf data :body) max-chars)
      (multiple-value-bind (source source-complete source-omitted)
          (%print-bounded-form (getf data :source-form) max-chars)
        (list :status :ok
              :kind "property"
              :name (symbol-data name)
              :property-kind (getf data :kind)
              :tags (getf data :tags)
              :targets (mapcar #'symbol-data (getf data :targets))
              :documentation (getf data :documentation)
              :trials-table (let ((table (getf data :trials)))
                              (when table (printed-for-digest table)))
              :shrink-enabled (getf (getf data :metadata) :shrink)
              :arguments (loop for argument in (getf data :arguments)
                               collect (list :variable
                                             (symbol-data (getf argument :variable))
                                             :spec (%spec-tree (getf argument :spec))))
              :body body
              :body-complete complete
              :body-omitted-chars omitted
              :source-form source
              :source-form-complete source-complete
              :source-form-omitted-chars source-omitted
              :source-location (getf data :source-location)
              :definition-digest (definition-digest api name registry))))))

(defun %spec-tree (spec-plist)
  "Return SPEC-PLIST with its symbols externalized, children included.

Kept as a projection of cl-spec's own SPEC-DATA rather than a re-derivation:
every key here comes straight across, and nothing is computed on this side."
  (when (listp spec-plist)
    (list :kind (getf spec-plist :kind)
          :name (let ((name (getf spec-plist :name)))
                  (when name (symbol-data name)))
          :target (let ((target (getf spec-plist :target)))
                    (when target (symbol-data target)))
          :type (let ((type (getf spec-plist :type)))
                  (when type (printed-for-digest type)))
          :predicate (let ((predicate (getf spec-plist :predicate)))
                       (when predicate (printed-for-digest predicate)))
          :values (let ((values (getf spec-plist :values)))
                    (when values (printed-for-digest values)))
          :base-type (let ((base (getf spec-plist :base-type)))
                       (when base (printed-for-digest base)))
          :min (let ((min (getf spec-plist :min)))
                 (when min (printed-for-digest min)))
          :max (let ((max (getf spec-plist :max)))
                 (when max (printed-for-digest max)))
          :class-name (let ((name (getf spec-plist :class-name)))
                        (when name (symbol-data name)))
          :source-form (printed-for-digest (getf spec-plist :source-form))
          :source-location (getf spec-plist :source-location)
          :children (mapcar #'%spec-tree (getf spec-plist :children)))))

(defun %describe-spec (api name registry max-chars)
  "Return the detail plist for spec NAME."
  (let ((data (funcall (api-fn api :spec-data) name :registry registry)))
    (multiple-value-bind (source complete omitted)
        (%print-bounded-form (getf data :source-form) max-chars)
      (list :status :ok
            :kind "spec"
            :name (symbol-data name)
            :spec (%spec-tree data)
            :source-form source
            :source-form-complete complete
            :source-form-omitted-chars omitted
            :source-location (getf data :source-location)))))

(defun describe-report (api api-status kind name &key package (max-chars 8000))
  "Return the plist behind the spec-describe tool.

KIND is \"property\", \"spec\" or \"function-spec\"."
  (let ((environment (environment-data api api-status)))
    (unless (eq api-status :ok)
      (return-from describe-report (unavailable-report api-status environment)))
    (unless (member kind '("property" "spec" "function-spec") :test #'string=)
      (return-from describe-report
        (list :status :invalid-arguments
              :message (format nil "kind must be one of property, spec or ~
                                    function-spec; got ~S" kind)
              :environment environment)))
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator name :package package)
      (unless symbol
        (return-from describe-report
          (list :status :unresolved-symbol :reason reason :input name
                :environment environment)))
      (when (string= kind "function-spec")
        (return-from describe-report
          (list :status :unsupported
                :name (symbol-data symbol)
                :message +function-spec-unsupported-message+
                :environment environment)))
      (let ((registry (funcall (api-fn api :registry))))
        (handler-case
            (append (if (string= kind "property")
                        (%describe-property api symbol registry max-chars)
                        (%describe-spec api symbol registry max-chars))
                    (list :environment environment))
          (error (condition)
            (list :status :not-registered
                  :kind kind
                  :name (symbol-data symbol)
                  :message (princ-to-string condition)
                  :environment environment)))))))
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `rove tests/spec-adapter-report-test.lisp`
Expected: PASS。10 テスト。

- [ ] **Step 5: lint とコミット**

```bash
mallet src/spec-adapter-report.lisp tests/spec-adapter-report-test.lisp
git add src/spec-adapter-report.lisp tests/spec-adapter-report-test.lisp tests.lisp
git commit -m "spec-adapter: answer discovery and detail as plists, never as a signalled condition"
```

---

### Task 4: 実行の操作層(予算配分・時間上限・status 語彙)

**Files:**
- Modify: `src/spec-adapter-report.lisp`
- Modify: `tests/spec-adapter-report-test.lisp`

**Interfaces:**
- Consumes: Task 3 の `environment-data` / `unavailable-report` / `%property-summary` 相当のヘルパ、Task 1-2 の core
- Produces:
  - `(check-report api api-status &key property symbol package profile seed expect-definition-digest timeout-seconds max-value-chars)` → plist
  - 返す plist の主キー: `:status` `:verified` `:selection` `:results` `:counts` `:reproduction-faithful` `:thread-leaked` `:elapsed` `:environment`
  - `:results` の各要素は `:property` `:status` `:trials` `:seed` `:profile` `:counterexample` `:shrunk-counterexample` `:shrink-enabled` `:condition` `:elapsed` `:definition-digest` `:definition-match` を持つ
  - 個別 status: `:passed` `:failed` `:error` `:skipped` `:pending` `:timeout` `:generator-error` `:backend-error` `:internal-error` `:not-run`
  - 全体 status: `:no-properties` `:completed` `:incomplete` `:not-registered` `:unresolved-symbol` `:invalid-arguments` `:backend-not-loaded`

- [ ] **Step 1: 失敗するテストを書く**

`tests/spec-adapter-report-test.lisp` の `:import-from` に `#:check-report` を、
rove から `#:deftest #:testing #:ok` に加えて何も足さず、
`cl-mcp/src/utils/deadline` から `#:forget-leaked-threads` を import する。
末尾に以下を足す。

```lisp
(defun %result-stub (&key (status :passed) (trials 100) (seed 42)
                          counterexample shrunk condition (elapsed 0.01))
  "Return a stand-in for a cl-spec PROPERTY-RESULT as a plist.

The API struct reads a result through eight reader handles, so a plist plus
GETF readers is a complete substitute and no cl-spec class is needed."
  (list :status status :trials trials :seed seed :profile :normal
        :counterexample counterexample :shrunk-counterexample shrunk
        :condition condition :elapsed elapsed))

(defun %api-with-run (run-property &rest overrides)
  "Return a stub API whose RUN-PROPERTY is RUN-PROPERTY."
  (apply #'%stub-api
         (append
          (list :run-property run-property
                :result-status (lambda (r) (getf r :status))
                :result-trials (lambda (r) (getf r :trials))
                :result-seed (lambda (r) (getf r :seed))
                :result-profile (lambda (r) (getf r :profile))
                :result-counterexample (lambda (r) (getf r :counterexample))
                :result-shrunk-counterexample (lambda (r) (getf r :shrunk-counterexample))
                :result-condition (lambda (r) (getf r :condition))
                :result-elapsed (lambda (r) (getf r :elapsed)))
          overrides)))

(deftest check-report-zero-properties-is-never-success
  (testing "a symbol with no :about property reports no-properties"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (error "must not run")))
                   :ok
                   :symbol "CL-MCP-SPEC-REPORT-FIXTURE:LONELY")))
      (ok (eq :no-properties (getf report :status)))
      (ok (not (getf report :verified)))
      (ok (null (getf report :results)))
      (ok (zerop (getf (getf report :selection) :count))))))

(deftest check-report-passing-property
  (testing "a passing property is verified and carries its seed as text"
    (let ((report (check-report
                   (%api-with-run (lambda (name &key profile seed registry)
                                    (declare (ignore name profile seed registry))
                                    (%result-stub :seed 3963993791726803706)))
                   :ok
                   :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :completed (getf report :status)))
      (ok (getf report :verified))
      (let ((result (first (getf report :results))))
        (ok (eq :passed (getf result :status)))
        (ok (string= "3963993791726803706" (getf result :seed)))
        (ok (null (getf result :counterexample)))
        (ok (= 100 (getf (getf result :trials) :budget)))
        (ok (= 100 (getf (getf result :trials) :executed)))))))

(deftest check-report-failing-property-externalizes-the-counterexample
  (testing "a failure carries named, printed arguments and is not verified"
    (let* ((a (intern "A" (%fixture-package)))
           (report (check-report
                    (%api-with-run
                     (lambda (name &key profile seed registry)
                       (declare (ignore name profile seed registry))
                       (%result-stub :status :failed :trials 1
                                     :counterexample (list a 68)
                                     :shrunk (list a 0))))
                    :ok
                    :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :completed (getf report :status)))
      (ok (not (getf report :verified)))
      (let* ((result (first (getf report :results)))
             (counterexample (first (getf result :counterexample))))
        (ok (eq :failed (getf result :status)))
        (ok (string= "A" (getf (getf counterexample :variable) :name)))
        (ok (string= "68" (getf (getf counterexample :value) :printed)))
        (ok (string= "0" (getf (getf (first (getf result :shrunk-counterexample))
                                     :value)
                               :printed)))))))

(deftest check-report-generator-error-is-not-a-pass
  (testing "a backend condition is classified, not counted as success"
    ;; No cl-spec condition classes are on this stub API, which is the
    ;; interesting case: classification has to fall back to the message and
    ;; must still keep the result out of the pass column.
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (error "No generator backend is installed.")))
                   :ok
                   :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD")))
      (ok (eq :incomplete (getf report :status)))
      (ok (not (getf report :verified)))
      (let ((result (first (getf report :results))))
        (ok (member (getf result :status)
                    '(:generator-error :backend-error :internal-error)))
        (ok (search "generator" (string-downcase (getf (getf result :condition)
                                                       :message))))))))

(deftest check-report-timeout-and-budget
  (testing "a run past the budget is timeout, and the next one never starts"
    (unwind-protect
         (let ((report (check-report
                        (%api-with-run (lambda (&rest ignored)
                                         (declare (ignore ignored))
                                         (sleep 5)
                                         (%result-stub))
                                       :properties-for
                                       (lambda (symbol &optional registry)
                                         (declare (ignore symbol registry))
                                         (list (%sym "ADD-COMMUTES")
                                               (%sym "ADD-COMMUTES"))))
                        :ok
                        :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD"
                        :timeout-seconds 0.3)))
           (ok (eq :incomplete (getf report :status)))
           (ok (not (getf report :verified)))
           (let ((results (getf report :results)))
             (ok (= 2 (length results)))
             (ok (eq :timeout (getf (first results) :status)))
             (ok (eq :not-run (getf (second results) :status)))
             (ok (eq :budget-exhausted (getf (second results) :reason)))))
      (forget-leaked-threads))))

(deftest check-report-seed-requires-a-single-property
  (testing "a seed across several properties is refused rather than reused"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub))
                                  :properties-for
                                  (lambda (symbol &optional registry)
                                    (declare (ignore symbol registry))
                                    (list (%sym "ADD-COMMUTES")
                                          (%sym "ADD-COMMUTES"))))
                   :ok
                   :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD"
                   :seed 42)))
      (ok (eq :invalid-arguments (getf report :status)))
      (ok (search "single" (string-downcase (getf report :message)))))))

(deftest check-report-digest-mismatch-is-loud
  (testing "an unexpected definition is reported as an unfaithful replay"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub)))
                   :ok
                   :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES"
                   :expect-definition-digest "0000000000000000")))
      (ok (not (getf report :reproduction-faithful)))
      (let ((result (first (getf report :results))))
        (ok (eq :false (getf result :definition-match)))))))

(deftest check-report-without-backend
  (testing "an absent generator backend is its own answer"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub))
                                  :generator-backend (lambda () nil))
                   :ok
                   :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES")))
      (ok (eq :backend-not-loaded (getf report :status)))
      (ok (not (getf report :verified)))
      (ok (search "cl-spec/check-it" (getf report :message))))))

(deftest check-report-unknown-property
  (testing "an unregistered property name is not-registered"
    (let ((report (check-report
                   (%api-with-run (lambda (&rest ignored)
                                    (declare (ignore ignored))
                                    (%result-stub)))
                   :ok
                   :property "CL-MCP-SPEC-REPORT-FIXTURE:LONELY")))
      (ok (eq :not-registered (getf report :status)))
      (ok (not (getf report :verified))))))

(deftest check-report-requires-exactly-one-target
  (testing "neither or both of property and symbol is an argument error"
    (let ((api (%api-with-run (lambda (&rest ignored)
                                (declare (ignore ignored))
                                (%result-stub)))))
      (ok (eq :invalid-arguments (getf (check-report api :ok) :status)))
      (ok (eq :invalid-arguments
              (getf (check-report api :ok
                                  :property "CL-MCP-SPEC-REPORT-FIXTURE:ADD-COMMUTES"
                                  :symbol "CL-MCP-SPEC-REPORT-FIXTURE:ADD")
                    :status))))))
```

- [ ] **Step 2: 失敗を確認する**

Run: `rove tests/spec-adapter-report-test.lisp`
Expected: FAIL。`CHECK-REPORT` が未定義。

- [ ] **Step 3: 実装を書く**

`src/spec-adapter-report.lisp` の `defpackage` に足す。

```lisp
  (:import-from #:cl-mcp/src/utils/deadline
                #:call-with-deadline-thread)
```

export に `#:check-report` と `#:*default-check-timeout-seconds*` を足す。

ファイル末尾に以下を足す。

```lisp
;;; ---------------------------------------------------------------------------
;;; spec-check
;;; ---------------------------------------------------------------------------

(defvar *default-check-timeout-seconds* 60
  "Whole-call budget a spec-check uses when the caller names none.

Whole-call rather than per-property because the proxy reads the same
timeout_seconds to decide how long to wait for the worker's answer
(SRC/PROXY.LISP, %EFFECTIVE-RPC-TIMEOUT).  A per-property figure would let a
selection of five properties outlive the proxy's patience, and a proxy timeout
is not a timeout report: it kills the worker and resets the session.")

(defconstant +minimum-run-budget-seconds+ 0.05
  "Budget below which a property is reported as not run rather than started.

Starting a run with a few milliseconds left produces a timeout that says
nothing about the property, and costs a thread to say it.")

(defparameter +backend-missing-message+
  "No cl-spec generator backend is installed, so no property can be executed. \
Run load-system with system \"cl-spec/check-it\". Nothing was executed: this \
is NOT a successful verification."
  "Said when execution is requested with *GENERATOR-BACKEND* unset.")

(defparameter +about-coverage-note+
  "Direct (:about ...) registrations only. Callers, generic-function methods, \
macro users and shared mutable state are NOT analysed. This is not a change \
impact analysis (cl-spec specification 31 and 72.5)."
  "The limit of what the :about reverse index can be said to cover.")

(defparameter +shrink-note+
  "Backend-searched reduction. NOT a guaranteed global minimum, and the \
backend does not report whether shrinking completed, exhausted its budget or \
was interrupted (cl-spec specification 16 and 72.4)."
  "What a shrunk counterexample does and does not mean.")

(defparameter +reproduce-scope-note+
  "Regenerates the trial sequence from this seed under the same definitions, \
backend, profile and image. It does NOT reproduce the code revision, external \
I/O, the clock, or shared mutable state. This is regeneration, not replay of a \
saved counterexample against a fixed implementation (cl-spec specification 15 \
and 72.3)."
  "What a seed does and does not fix.")

(defun %resolve-profile (profile)
  "Return (values KEYWORD NIL) for the profile named by PROFILE.

PROFILE arrives as text from outside the image, so the keyword is looked up
rather than interned.  A profile no loaded code mentions cannot be interned
either, which means no property's :TRIALS table can name it -- running under
it would silently fall back to the backend default while the response claimed
the requested profile.  Refusing is the honest answer."
  (let* ((name (string-upcase (or profile "normal")))
         (keyword (find-symbol name "KEYWORD")))
    (if keyword
        (values keyword nil)
        (values nil
                (format nil
                        "profile ~S names no keyword present in this image, so \
no registered property can select a trial count for it. Use a profile that \
appears in the property's trials table (see spec-describe)."
                        profile)))))

(defun %select-properties (api property symbol package registry)
  "Return (values NAMES SELECTION ERROR) for the requested selection.

Exactly one of PROPERTY and SYMBOL must be supplied.  A symbol that is itself
a registered property is deliberately not added to a :ABOUT selection: that is
a different relationship, and quietly widening what gets executed would make
the reported coverage wrong."
  (cond
    ((and property symbol)
     (values nil nil
             (list :status :invalid-arguments
                   :message "give either property or symbol, not both")))
    ((not (or property symbol))
     (values nil nil
             (list :status :invalid-arguments
                   :message "give one of property or symbol")))
    (property
     (multiple-value-bind (name reason)
         (resolve-symbol-designator property :package package)
       (cond
         ((null name)
          (values nil nil (list :status :unresolved-symbol :reason reason
                                :input property)))
         ((handler-case
              (progn (funcall (api-fn api :property-data) name :registry registry)
                     nil)
            (error (condition) condition))
          (values nil nil (list :status :not-registered
                                :name (symbol-data name)
                                :message (format nil "No property named ~A is ~
registered in this registry." (getf (symbol-data name) :qualified)))))
         (t (values (list name)
                    (list :mode "explicit"
                          :requested (list :property (symbol-data name))
                          :selected (list (symbol-data name))
                          :count 1
                          :source "explicit property argument"
                          :coverage "Only the property named. Nothing else was selected.")
                    nil)))))
    (t
     (multiple-value-bind (name reason)
         (resolve-symbol-designator symbol :package package)
       (if (null name)
           (values nil nil (list :status :unresolved-symbol :reason reason
                                 :input symbol))
           (let* ((routing (funcall (api-fn api :semantic-data) name
                                    :registry registry))
                  (about (getf routing :properties-about)))
             (values about
                     (list :mode "about"
                           :requested (list :symbol (symbol-data name))
                           :selected (mapcar #'symbol-data about)
                           :count (length about)
                           :source "cl-spec:semantic-data -> :properties-about (registry :about reverse index)"
                           :coverage +about-coverage-note+
                           :notes (when (getf routing :property)
                                    (list "this symbol is itself a registered property; it was NOT selected -- run it with property=")))
                     nil)))))))

(defun %trials-budget (api name registry profile)
  "Return the trial budget plist for property NAME under PROFILE.

cl-spec resolves this internally in RESOLVE-TRIALS and neither exports that
function nor records the figure on a PROPERTY-RESULT, so it is derived here
from the two exported readers.  BUDGET-DERIVATION says so: a number a consumer
cannot trace back is worse than one it can question."
  (let* ((data (handler-case
                   (funcall (api-fn api :property-data) name :registry registry)
                 (error () nil)))
         (table (getf data :trials))
         (from-profile (and table (getf table profile)))
         (backend (handler-case (funcall (api-fn api :generator-backend))
                    (error () nil)))
         (default (and backend
                       (handler-case
                           (funcall (api-fn api :backend-default-trials) backend)
                         (error () nil)))))
    (list :budget (or from-profile default)
          :budget-source (cond (from-profile "property-profile")
                               (default "backend-default")
                               (t "unknown"))
          :property-trials (when table (printed-for-digest table))
          :backend-default default
          :budget-derivation "derived by cl-mcp from cl-spec:property-trials and cl-spec:backend-default-trials; cl-spec does not expose the resolved budget")))

(defun %classify-condition (api condition)
  "Return the adapter status keyword for CONDITION.

The condition classes are looked up on the API rather than named here, so an
image whose cl-spec predates one of them degrades to a coarser status instead
of failing to load."
  (flet ((is-a (key)
           (let ((class (api-class api key)))
             (and class (typep condition class)))))
    (cond
      ((or (is-a :no-generator-backend) (is-a :generator-unavailable))
       :generator-error)
      ((is-a :unknown-property) :not-registered)
      ((is-a :cl-spec-error) :backend-error)
      ;; A cl-spec that predates the condition hierarchy, or a condition from
      ;; somewhere else entirely.  The message is the only evidence available,
      ;; and it is reported rather than interpreted -- except for the one
      ;; phrase the backend-missing condition is guaranteed to carry.
      ((search "generator backend" (princ-to-string condition)) :generator-error)
      (t :internal-error))))

(defun %condition-data (condition &key (max-chars 2000))
  "Return CONDITION as the plist a response carries for it."
  (list :type (princ-to-string (type-of condition))
        :message (let ((text (handler-case (princ-to-string condition)
                               (error () "#<unprintable condition>"))))
                   (if (<= (length text) max-chars)
                       text
                       (subseq text 0 max-chars)))
        :object-id (getf (externalize-value condition :max-chars 1) :object-id)))

(defun %named-values (plist max-value-chars)
  "Return cl-spec's {variable value} counterexample PLIST as a list of plists."
  (loop for (variable value) on plist by #'cddr
        collect (list :variable (symbol-data variable)
                      :value (externalize-value value
                                                :max-chars max-value-chars))))

(defun %result-plist (api result name trials digest expected-digest max-value-chars)
  "Return the per-property plist for a cl-spec RESULT."
  (let ((status (funcall (api-fn api :result-status) result))
        (counterexample (funcall (api-fn api :result-counterexample) result))
        (shrunk (funcall (api-fn api :result-shrunk-counterexample) result))
        (condition (funcall (api-fn api :result-condition) result))
        (seed (funcall (api-fn api :result-seed) result)))
    (list :property (symbol-data name)
          :status status
          :trials (list* :executed (funcall (api-fn api :result-trials) result)
                         trials)
          ;; Text, not a number: a cl-spec seed reaches 2^62 and a JSON
          ;; consumer holding it as a number would round it, which turns a
          ;; reproducible failure into one that cannot be reproduced.
          :seed (when seed (format nil "~D" seed))
          :profile (funcall (api-fn api :result-profile) result)
          :counterexample (%named-values counterexample max-value-chars)
          :shrunk-counterexample (%named-values shrunk max-value-chars)
          :shrink-note (when shrunk +shrink-note+)
          :condition (when condition (%condition-data condition))
          :elapsed (funcall (api-fn api :result-elapsed) result)
          :definition-digest digest
          :definition-match (cond ((null expected-digest) :not-checked)
                                  ((and digest (string-equal digest expected-digest))
                                   :true)
                                  (t :false)))))

(defun %elapsed-since (start)
  "Return the seconds elapsed since internal real time START."
  (/ (float (- (get-internal-real-time) start))
     internal-time-units-per-second))

(defun %run-one (api name registry profile seed trials digest expected-digest
                 remaining max-value-chars)
  "Run property NAME within REMAINING seconds and return its result plist."
  (if (< remaining +minimum-run-budget-seconds+)
      (list :property (symbol-data name)
            :status :not-run
            :reason :budget-exhausted
            :trials trials
            :definition-digest digest
            :message "the whole-call timeout_seconds budget was spent before \
this property started; nothing about it was checked")
      (multiple-value-bind (value status leaked)
          (call-with-deadline-thread
           (lambda ()
             (funcall (api-fn api :run-property) name
                      :profile profile :seed seed :registry registry))
           remaining
           :name "mcp-spec-check")
        (ecase status
          (:ok (%result-plist api (first value) name trials digest
                              expected-digest max-value-chars))
          (:timeout
           (list :property (symbol-data name)
                 :status :timeout
                 :timeout-seconds value
                 :thread-leaked leaked
                 :trials trials
                 :definition-digest digest
                 :message
                 (if leaked
                     "the property run exceeded its deadline and could not be \
stopped: it is still executing in this worker and may hold locks. Use \
pool-kill-worker to get a fresh worker before retrying."
                     "the property run exceeded its deadline and its run thread \
was stopped. Nothing was proved or disproved; retry with a larger \
timeout_seconds if the property legitimately needs longer.")))
          (:error
           (list :property (symbol-data name)
                 :status (%classify-condition api value)
                 :trials trials
                 :definition-digest digest
                 :condition (%condition-data value)))))))

(defun %terminal-status-p (status)
  "Return true when STATUS is a verdict about the property rather than about
the run's own machinery."
  (member status '(:passed :failed :error :skipped :pending)))

(defun check-report (api api-status &key property symbol package profile seed
                                         expect-definition-digest
                                         timeout-seconds
                                         (max-value-chars 2000))
  "Return the plist behind the spec-check tool.

Runs either the property named by PROPERTY or every property registered
:ABOUT the symbol named by SYMBOL.  TIMEOUT-SECONDS is the budget for the
whole call, spent across the selection in order.

:VERIFIED is true only when at least one property was selected and every one
of them passed.  A selection of zero, a timeout, a generator failure and a
skipped run are each reported as themselves: none of them is evidence that
anything holds."
  (let ((environment (environment-data api api-status)))
    (unless (eq api-status :ok)
      (return-from check-report (unavailable-report api-status environment)))
    (unless (api-backend-available-p api)
      (return-from check-report
        (list :status :backend-not-loaded
              :verified nil
              :message +backend-missing-message+
              :environment environment)))
    (multiple-value-bind (profile-keyword profile-error) (%resolve-profile profile)
      (when profile-error
        (return-from check-report
          (list :status :invalid-arguments :verified nil
                :message profile-error :environment environment)))
      (let ((registry (funcall (api-fn api :registry))))
        (multiple-value-bind (names selection selection-error)
            (%select-properties api property symbol package registry)
          (when selection-error
            (return-from check-report
              (append selection-error (list :verified nil
                                            :environment environment))))
          (when (null names)
            (return-from check-report
              (list :status :no-properties
                    :verified nil
                    :selection selection
                    :results nil
                    :message "0 properties selected -- this is NOT a successful \
verification. Nothing was executed, and a registry with no property registered \
about this symbol says nothing about whether it is correct."
                    :environment environment)))
          (when (and seed (rest names))
            (return-from check-report
              (list :status :invalid-arguments
                    :verified nil
                    :selection selection
                    :message "seed reproduces a single property run; the \
selection holds more than one property. Name one with property= instead."
                    :environment environment)))
          (let* ((budget (or timeout-seconds *default-check-timeout-seconds*))
                 (start (get-internal-real-time))
                 (results '())
                 (thread-leaked nil))
            (dolist (name names)
              (let* ((trials (%trials-budget api name registry profile-keyword))
                     (digest (definition-digest api name registry))
                     (remaining (- budget (%elapsed-since start)))
                     (result (%run-one api name registry profile-keyword seed
                                       trials digest expect-definition-digest
                                       remaining max-value-chars)))
                (when (getf result :thread-leaked) (setf thread-leaked t))
                (push result results)))
            (let* ((results (nreverse results))
                   (counts (list :selected (length results)
                                 :passed (count :passed results :key (lambda (r) (getf r :status)))
                                 :failed (count :failed results :key (lambda (r) (getf r :status)))
                                 :errored (count :error results :key (lambda (r) (getf r :status)))
                                 :timed-out (count :timeout results :key (lambda (r) (getf r :status)))
                                 :not-run (count :not-run results :key (lambda (r) (getf r :status)))))
                   (all-terminal (every (lambda (r) (%terminal-status-p (getf r :status)))
                                        results)))
              (list :status (if all-terminal :completed :incomplete)
                    :verified (and results
                                   (every (lambda (r) (eq :passed (getf r :status)))
                                          results))
                    :selection selection
                    :results results
                    :counts counts
                    :profile profile-keyword
                    :timeout-seconds budget
                    :thread-leaked thread-leaked
                    :elapsed (%elapsed-since start)
                    :options nil
                    :options-note "cl-mcp passes no backend options, so a \
re-run under the same seed and profile cannot silently differ in them."
                    :reproduce-scope +reproduce-scope-note+
                    ;; Kept apart from :VERIFIED on purpose.  A run whose
                    ;; definitions moved can still pass; what it cannot claim
                    ;; is to have reproduced the earlier run.
                    :reproduction-faithful
                    (cond ((null expect-definition-digest) :not-checked)
                          ((every (lambda (r) (eq :true (getf r :definition-match)))
                                  results)
                           t)
                          (t nil))
                    :environment environment))))))))
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `rove tests/spec-adapter-report-test.lisp`
Expected: PASS。20 テスト。

- [ ] **Step 5: lint とコミット**

```bash
mallet src/spec-adapter-report.lisp tests/spec-adapter-report-test.lisp
git add src/spec-adapter-report.lisp tests/spec-adapter-report-test.lisp
git commit -m "spec-adapter: spend one budget across a selection, and keep every non-verdict out of the pass column"
```

---

### Task 5: 応答ビルダーと content text

**Files:**
- Create: `src/tools/spec-response-builders.lisp`
- Create: `tests/spec-response-builders-test.lisp`
- Modify: `tests.lisp`

**Interfaces:**
- Consumes: Task 3-4 の report plist
- Produces:
  - `(build-spec-symbol-response report)` → hash-table
  - `(build-spec-describe-response report)` → hash-table
  - `(build-spec-check-response report)` → hash-table
  - どれも `"content"` に `text-content` ベクタを持ち、失敗系では `"isError"` は立てない(tool として正常に答えているため)

**設計上の注意:** MCP クライアントは `content[].text` しか描画しない。
判断に必要な情報は必ず text に出す。兄弟 JSON フィールドだけに置いた値は
人間にもモデルにも見えない。

- [ ] **Step 1: 失敗するテストを書く**

`tests/spec-response-builders-test.lisp` を新規作成する。

```lisp
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
        :missing nil :lisp "SBCL 2.4.0"))

(defun %symbol-data (package name)
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
                                            (list (%symbol-data "PROBE" "ADD-COMMUTES")))
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
      (ok (search "not evidence" (string-downcase text))))))

(deftest check-response-zero-properties-warns-in-text
  (testing "a zero selection is loud in the text, not only in a status field"
    (let* ((response (build-spec-check-response
                      (list :status :no-properties
                            :verified nil
                            :selection (list :mode "about"
                                             :requested (list :symbol (%symbol-data "PROBE" "HELPER"))
                                             :selected nil :count 0
                                             :source "cl-spec:semantic-data -> :properties-about"
                                             :coverage "Direct (:about ...) registrations only.")
                            :results nil
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
                                             :selected (list (%symbol-data "PROBE" "ADD-IS-WRONG"))
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
                                             :selected (list (%symbol-data "PROBE" "SLOW"))
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
      (ok (search "pool-kill-worker" text)))))

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
      (ok (search "truncated" (string-downcase text)))
      (ok (search "31" text)))))
```

`tests/spec-response-builders-test.lisp` の `defpackage` は `yason:false` を
参照するので `(:import-from #:yason #:false)` は不要(パッケージ修飾で足りる)。

`tests.lisp` に 1 行足す。

```lisp
  (:import-from #:cl-mcp/tests/spec-response-builders-test)
```

- [ ] **Step 2: 失敗を確認する**

Run: `rove tests/spec-response-builders-test.lisp`
Expected: FAIL。`Component "cl-mcp/src/tools/spec-response-builders" not found`

- [ ] **Step 3: 実装を書く**

`src/tools/spec-response-builders.lisp` を新規作成する。

```lisp
;;;; src/tools/spec-response-builders.lisp
;;;;
;;;; Report plists into MCP tool responses.
;;;;
;;;; Two rules shape every builder here.  An MCP client renders only
;;;; content[].text, so anything a caller must not miss goes into the text as
;;;; well as into the payload -- a zero-property selection reported only in a
;;;; sibling field is a zero-property selection nobody sees.  And no Lisp
;;;; value becomes a JSON number: cl-spec seeds reach 2^62 and trial counts
;;;; are the only numbers here small enough to be safe.

(defpackage #:cl-mcp/src/tools/spec-response-builders
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:text-content #:json-bool)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:export #:build-spec-symbol-response
           #:build-spec-describe-response
           #:build-spec-check-response))

(in-package #:cl-mcp/src/tools/spec-response-builders)

;;; ---------------------------------------------------------------------------
;;; Shared conversions
;;; ---------------------------------------------------------------------------

(defun %keyword-string (value)
  "Return VALUE as a lower-case string, or NIL.

Status keywords cross the boundary as text rather than as JSON identifiers:
:NOT-RUN reads as \"not-run\", which is what the tool documentation names."
  (when value
    (string-downcase (princ-to-string value))))

(defun %symbol-ht (data)
  "Return a symbol plist as a hash-table, or NIL when there is no symbol."
  (when data
    (make-ht "package" (getf data :package)
             "name" (getf data :name)
             "qualified" (getf data :qualified))))

(defun %symbol-hts (list)
  "Return a list of symbol plists as a vector of hash-tables."
  (coerce (mapcar #'%symbol-ht list) 'vector))

(defun %value-ht (data)
  "Return an externalized value plist as a hash-table."
  (make-ht "printed" (sanitize-for-json (getf data :printed))
           "printed_complete" (json-bool (getf data :printed-complete))
           "omitted_chars" (getf data :omitted-chars)
           "type" (getf data :type)
           "object_id" (getf data :object-id)))

(defun %named-value-hts (list)
  "Return a counterexample as a vector of {variable, value} hash-tables."
  (coerce (mapcar (lambda (entry)
                    (make-ht "variable" (%symbol-ht (getf entry :variable))
                             "value" (%value-ht (getf entry :value))))
                  list)
          'vector))

(defun %environment-ht (data)
  "Return the environment plist as a hash-table."
  (make-ht "cl_spec_loaded" (json-bool (getf data :cl-spec-loaded))
           "cl_spec_status" (%keyword-string (getf data :cl-spec-status))
           "cl_spec_version" (getf data :cl-spec-version)
           "cl_spec_system_directory" (getf data :cl-spec-system-directory)
           "generator_backend" (getf data :generator-backend)
           "backend_available" (json-bool (getf data :backend-available))
           "registry" (getf data :registry)
           "missing" (coerce (getf data :missing) 'vector)
           "lisp" (getf data :lisp)))

(defun %strings (list)
  "Return LIST as a vector of printed strings."
  (coerce (mapcar (lambda (item)
                    (if (stringp item) item (princ-to-string item)))
                  list)
          'vector))

(defun %unavailable-response (report)
  "Return the response for a status that carries only a message.

Not an isError: the tool answered correctly.  \"cl-spec is not loaded\" is a
fact about the image, and flagging it as a tool failure would push a caller
towards retrying rather than towards loading the system."
  (make-ht "status" (%keyword-string (getf report :status))
           "message" (getf report :message)
           "environment" (%environment-ht (getf report :environment))
           "content" (text-content
                      (format nil "~A~%~%~A"
                              (string-upcase (%keyword-string (getf report :status)))
                              (getf report :message)))))

(defun %reason-line (reason)
  "Return a one-line rendering of a symbol resolution failure."
  (case (getf reason :reason)
    (:package-not-found
     (format nil "No package named ~A exists in this image."
             (getf reason :package)))
    (:symbol-not-found
     (format nil "Package ~A has no symbol named ~A. It was looked up, not ~
created: this tool never interns a name it was given."
             (getf reason :package) (getf reason :name)))
    (:not-external
     (format nil "~A is internal to package ~A. Write ~A::~A to reach it."
             (getf reason :name) (getf reason :package)
             (getf reason :package) (getf reason :name)))
    (t (format nil "~A is not a symbol name this tool accepts~@[: ~A~]"
               (getf reason :input) (getf reason :detail)))))

(defun %unresolved-response (report)
  "Return the response for a symbol designator that resolves to nothing."
  (let ((line (%reason-line (getf report :reason))))
    (make-ht "status" "unresolved-symbol"
             "input" (getf report :input)
             "reason" (%keyword-string (getf (getf report :reason) :reason))
             "message" line
             "environment" (%environment-ht (getf report :environment))
             "content" (text-content
                        (format nil "UNRESOLVED SYMBOL~%~A" line)))))

(defun %simple-status-response (report &optional extra-text)
  "Return the response for a status carrying a name and a message."
  (make-ht "status" (%keyword-string (getf report :status))
           "name" (%symbol-ht (getf report :name))
           "message" (getf report :message)
           "environment" (%environment-ht (getf report :environment))
           "content" (text-content
                      (format nil "~A~@[ ~A~]~%~%~A~@[~%~%~A~]"
                              (string-upcase (%keyword-string (getf report :status)))
                              (getf (getf report :name) :qualified)
                              (or (getf report :message) "")
                              extra-text))))

;;; ---------------------------------------------------------------------------
;;; spec-symbol
;;; ---------------------------------------------------------------------------

(defun %property-summary-ht (data)
  "Return one property listing entry as a hash-table."
  (make-ht "name" (%symbol-ht (getf data :name))
           "kind" (%keyword-string (getf data :kind))
           "tags" (%strings (getf data :tags))
           "targets" (%symbol-hts (getf data :targets))
           "documentation" (getf data :documentation)
           "arguments"
           (coerce (mapcar (lambda (argument)
                             (make-ht "variable" (%symbol-ht (getf argument :variable))
                                      "spec"
                                      (let ((spec (getf argument :spec)))
                                        (make-ht "kind" (%keyword-string (getf spec :kind))
                                                 "name" (%symbol-ht (getf spec :name))
                                                 "target" (%symbol-ht (getf spec :target))))))
                           (getf data :arguments))
                   'vector)
           "trials_table" (getf data :trials-table)
           "shrink_enabled" (json-bool (getf data :shrink-enabled))
           "source_location" (let ((location (getf data :source-location)))
                               (when location
                                 (make-ht "file" (getf location :file)
                                          "package" (getf location :package))))
           "definition_digest" (getf data :definition-digest)
           "body_forms" (getf data :body-forms)
           "body_omitted" (json-bool (getf data :body-omitted))
           "detail_via" (getf data :detail-via)
           "unavailable_reason" (getf data :unavailable-reason)))

(defun %format-symbol-text (report)
  "Render the spec-symbol report as the text an MCP client will show."
  (with-output-to-string (stream)
    (let ((symbol (getf report :symbol))
          (runtime (getf report :runtime))
          (properties (getf report :properties)))
      (format stream "~A" (getf symbol :qualified))
      (when runtime
        (format stream "  ~A~@[ ~A~]" (getf runtime :type) (getf runtime :arglist))
        (when (getf runtime :source-file)
          (format stream "~&  defined at ~A~@[:~D~]"
                  (getf runtime :source-file) (getf runtime :source-line)))
        (when (getf runtime :documentation)
          (format stream "~&  ~A" (getf runtime :documentation))))
      (unless runtime
        (format stream "~&  runtime information unavailable~@[: ~A~]"
                (getf report :runtime-unavailable-reason)))
      (if (getf report :nothing-registered)
          (format stream "~&~%Nothing is registered about this symbol in the ~
cl-spec registry: no spec, no function spec, and no property. An empty ~
registry answer is not evidence that the symbol needs no contract -- the ~
system defining them may simply not be loaded.")
          (progn
            (format stream "~&~%Registered:")
            (let ((registry (getf report :registry)))
              (format stream "~&  spec:          ~A"
                      (or (getf (getf registry :spec) :qualified) "none"))
              (format stream "~&  function spec: ~A"
                      (or (getf (getf registry :function-spec) :qualified) "none"))
              (format stream "~&  property:      ~A"
                      (or (getf (getf registry :property) :qualified) "none")))
            (format stream "~&~%Properties about this symbol (~D):" (length properties))
            (dolist (property properties)
              (format stream "~&  ~A~@[  [~A]~]"
                      (getf (getf property :name) :qualified)
                      (%keyword-string (getf property :kind)))
              (when (getf property :documentation)
                (format stream "~&      ~A" (getf property :documentation)))
              (when (getf property :tags)
                (format stream "~&      tags: ~{~A~^, ~}"
                        (mapcar #'%keyword-string (getf property :tags))))
              (format stream "~&      digest: ~A~@[  trials: ~A~]"
                      (or (getf property :definition-digest) "unavailable")
                      (getf property :trials-table))
              (when (getf property :body-omitted)
                (format stream "~&      body omitted (~D form~:P) -- fetch it with ~A"
                        (getf property :body-forms)
                        (getf property :detail-via))))))
      (dolist (note (getf report :notes))
        (format stream "~&~%note: ~A" note)))))

(defun build-spec-symbol-response (report)
  "Return the MCP response for a SYMBOL-REPORT plist."
  (case (getf report :status)
    ((:cl-spec-not-loaded :cl-spec-incomplete) (%unavailable-response report))
    (:unresolved-symbol (%unresolved-response report))
    (t
     (make-ht "status" "ok"
              "symbol" (%symbol-ht (getf report :symbol))
              "runtime" (let ((runtime (getf report :runtime)))
                          (when runtime
                            (make-ht "type" (getf runtime :type)
                                     "arglist" (getf runtime :arglist)
                                     "documentation" (getf runtime :documentation)
                                     "source_file" (getf runtime :source-file)
                                     "source_line" (getf runtime :source-line))))
              "runtime_unavailable_reason" (getf report :runtime-unavailable-reason)
              "registry"
              (let ((registry (getf report :registry)))
                (make-ht "spec" (%symbol-ht (getf registry :spec))
                         "function_spec" (%symbol-ht (getf registry :function-spec))
                         "property" (%symbol-ht (getf registry :property))
                         "properties_about" (%symbol-hts (getf registry :properties-about))))
              "properties" (coerce (mapcar #'%property-summary-ht
                                           (getf report :properties))
                                   'vector)
              "nothing_registered" (json-bool (getf report :nothing-registered))
              "notes" (%strings (getf report :notes))
              "environment" (%environment-ht (getf report :environment))
              "content" (text-content (%format-symbol-text report))))))

;;; ---------------------------------------------------------------------------
;;; spec-describe
;;; ---------------------------------------------------------------------------

(defun %spec-tree-ht (data)
  "Return a spec-data tree as nested hash-tables."
  (when data
    (make-ht "kind" (%keyword-string (getf data :kind))
             "name" (%symbol-ht (getf data :name))
             "target" (%symbol-ht (getf data :target))
             "type" (getf data :type)
             "predicate" (getf data :predicate)
             "values" (getf data :values)
             "base_type" (getf data :base-type)
             "min" (getf data :min)
             "max" (getf data :max)
             "class_name" (%symbol-ht (getf data :class-name))
             "source_form" (getf data :source-form)
             "children" (coerce (mapcar #'%spec-tree-ht (getf data :children))
                                'vector))))

(defun %format-describe-text (report)
  "Render the spec-describe report as text."
  (with-output-to-string (stream)
    (format stream "~A ~A" (string-upcase (getf report :kind))
            (getf (getf report :name) :qualified))
    (when (getf report :property-kind)
      (format stream "  [~A]" (%keyword-string (getf report :property-kind))))
    (when (getf report :documentation)
      (format stream "~&~A" (getf report :documentation)))
    (when (getf report :targets)
      (format stream "~&about: ~{~A~^, ~}"
              (mapcar (lambda (s) (getf s :qualified)) (getf report :targets))))
    (when (getf report :arguments)
      (format stream "~&~%arguments:")
      (dolist (argument (getf report :arguments))
        (format stream "~&  ~A : ~A~@[ -> ~A~]"
                (getf (getf argument :variable) :name)
                (%keyword-string (getf (getf argument :spec) :kind))
                (getf (getf (getf argument :spec) :target) :qualified))))
    (when (getf report :definition-digest)
      (format stream "~&~%definition_digest: ~A" (getf report :definition-digest)))
    (when (getf report :body)
      (format stream "~&~%body:~%~A" (getf report :body))
      (unless (getf report :body-complete)
        (format stream "~&... truncated, ~D more character~:P. Raise max_chars ~
to see the rest; the text above is a preview, not a form that can be read back."
                (getf report :body-omitted-chars))))
    (when (getf report :source-form)
      (format stream "~&~%source form:~%~A" (getf report :source-form))
      (unless (getf report :source-form-complete)
        (format stream "~&... truncated, ~D more character~:P."
                (getf report :source-form-omitted-chars))))
    (let ((location (getf report :source-location)))
      (when location
        (format stream "~&~%defined in ~A~@[ (package ~A)~]"
                (getf location :file) (getf location :package))))))

(defun build-spec-describe-response (report)
  "Return the MCP response for a DESCRIBE-REPORT plist."
  (case (getf report :status)
    ((:cl-spec-not-loaded :cl-spec-incomplete) (%unavailable-response report))
    (:unresolved-symbol (%unresolved-response report))
    ((:not-registered :unsupported :invalid-arguments)
     (%simple-status-response report))
    (t
     (make-ht "status" "ok"
              "kind" (getf report :kind)
              "name" (%symbol-ht (getf report :name))
              "property_kind" (%keyword-string (getf report :property-kind))
              "tags" (%strings (getf report :tags))
              "targets" (%symbol-hts (getf report :targets))
              "documentation" (getf report :documentation)
              "trials_table" (getf report :trials-table)
              "shrink_enabled" (json-bool (getf report :shrink-enabled))
              "arguments"
              (coerce (mapcar (lambda (argument)
                                (make-ht "variable" (%symbol-ht (getf argument :variable))
                                         "spec" (%spec-tree-ht (getf argument :spec))))
                              (getf report :arguments))
                      'vector)
              "spec" (%spec-tree-ht (getf report :spec))
              "body" (sanitize-for-json (getf report :body))
              "body_complete" (json-bool (getf report :body-complete))
              "body_omitted_chars" (getf report :body-omitted-chars)
              "source_form" (sanitize-for-json (getf report :source-form))
              "source_form_complete" (json-bool (getf report :source-form-complete))
              "source_form_omitted_chars" (getf report :source-form-omitted-chars)
              "source_location" (let ((location (getf report :source-location)))
                                  (when location
                                    (make-ht "file" (getf location :file)
                                             "package" (getf location :package))))
              "definition_digest" (getf report :definition-digest)
              "environment" (%environment-ht (getf report :environment))
              "content" (text-content (%format-describe-text report))))))

;;; ---------------------------------------------------------------------------
;;; spec-check
;;; ---------------------------------------------------------------------------

(defun %match-string (value)
  "Return a definition-match keyword as the word the tool documents."
  (case value
    (:true "match")
    (:false "mismatch")
    (t "not-checked")))

(defun %faithful-string (value)
  "Return the reproduction-faithful value as a documented word."
  (cond ((eq value :not-checked) "not-checked")
        (value "faithful")
        (t "unfaithful")))

(defun %result-ht (result)
  "Return one per-property result as a hash-table."
  (let ((trials (getf result :trials)))
    (make-ht "property" (%symbol-ht (getf result :property))
             "status" (%keyword-string (getf result :status))
             "reason" (%keyword-string (getf result :reason))
             "trials" (make-ht "executed" (getf trials :executed)
                               "budget" (getf trials :budget)
                               "budget_source" (getf trials :budget-source)
                               "property_trials" (getf trials :property-trials)
                               "backend_default" (getf trials :backend-default)
                               "budget_derivation" (getf trials :budget-derivation))
             "seed" (getf result :seed)
             "profile" (%keyword-string (getf result :profile))
             "counterexample" (%named-value-hts (getf result :counterexample))
             "shrunk_counterexample" (%named-value-hts (getf result :shrunk-counterexample))
             "shrink_note" (getf result :shrink-note)
             "condition" (let ((condition (getf result :condition)))
                           (when condition
                             (make-ht "type" (getf condition :type)
                                      "message" (sanitize-for-json
                                                 (getf condition :message))
                                      "object_id" (getf condition :object-id))))
             "elapsed" (getf result :elapsed)
             "timeout_seconds" (getf result :timeout-seconds)
             "thread_leaked" (json-bool (getf result :thread-leaked))
             "definition_digest" (getf result :definition-digest)
             "definition_match" (%match-string (getf result :definition-match))
             "message" (getf result :message))))

(defun %format-values (entries)
  "Return \"A = 68, B = 85\" for a counterexample, or NIL when there is none."
  (when entries
    (format nil "~{~A~^, ~}"
            (mapcar (lambda (entry)
                      (format nil "~A = ~A"
                              (getf (getf entry :variable) :name)
                              (getf (getf entry :value) :printed)))
                    entries))))

(defun %format-check-text (report)
  "Render the spec-check report as the text an MCP client will show."
  (with-output-to-string (stream)
    (let* ((selection (getf report :selection))
           (results (getf report :results))
           (counts (getf report :counts)))
      (cond
        ((eq :no-properties (getf report :status))
         (format stream "NO PROPERTIES  ~A~&~A~&~%~A~&verified: false"
                 (or (getf (getf (getf selection :requested) :symbol) :qualified)
                     "")
                 (format nil "Selected 0 properties via ~A." (getf selection :source))
                 (getf report :message)))
        (t
         (format stream "~A" (if (getf report :verified) "PASSED" "NOT VERIFIED"))
         (format stream "~&Selected ~D propert~:@P via ~A."
                 (getf selection :count) (getf selection :source))
         (format stream "~&  ~A" (getf selection :coverage))
         (loop for result in results
               for index from 1
               do (format stream "~&~%[~D] ~A  ~A"
                          index
                          (getf (getf result :property) :qualified)
                          (%keyword-string (getf result :status)))
                  (let ((trials (getf result :trials)))
                    (format stream "~&    trials: ~A executed of ~A budget (~A)"
                            (or (getf trials :executed) "none")
                            (or (getf trials :budget) "unknown")
                            (or (getf trials :budget-source) "unknown")))
                  (let ((original (%format-values (getf result :counterexample)))
                        (shrunk (%format-values (getf result :shrunk-counterexample))))
                    (when original
                      (format stream "~&    counterexample:        ~A" original))
                    (when shrunk
                      (format stream "~&    shrunk counterexample: ~A" shrunk)
                      (format stream "~&      ~A" (getf result :shrink-note))))
                  (let ((condition (getf result :condition)))
                    (when condition
                      (format stream "~&    condition: [~A] ~A"
                              (getf condition :type) (getf condition :message))))
                  (when (getf result :seed)
                    (format stream "~&    seed: ~A   profile: ~A"
                            (getf result :seed)
                            (%keyword-string (getf result :profile))))
                  (when (getf result :definition-digest)
                    (format stream "~&    definition_digest: ~A~@[  (~A)~]"
                            (getf result :definition-digest)
                            (unless (eq :not-checked (getf result :definition-match))
                              (%match-string (getf result :definition-match)))))
                  (when (getf result :message)
                    (format stream "~&    ~A" (getf result :message))))
         (format stream "~&~%verified: ~A   ~D passed, ~D failed, ~D errored, ~
~D timed out, ~D not run"
                 (if (getf report :verified) "true" "false")
                 (getf counts :passed) (getf counts :failed)
                 (getf counts :errored) (getf counts :timed-out)
                 (getf counts :not-run))
         (when (getf report :thread-leaked)
           (format stream "~&A run thread could not be stopped. Use ~
pool-kill-worker to get a fresh worker before trusting later results."))
         (unless (eq :not-checked (getf report :reproduction-faithful))
           (format stream "~&reproduction: ~A"
                   (%faithful-string (getf report :reproduction-faithful))))
         (let ((first-result (first results)))
           (when (and first-result (getf first-result :seed))
             (format stream "~&~%Replay: spec-check property=~A seed=~A profile=~A~@[ ~
expect_definition_digest=~A~]"
                     (getf (getf first-result :property) :qualified)
                     (getf first-result :seed)
                     (%keyword-string (getf first-result :profile))
                     (getf first-result :definition-digest))))
         (when (getf report :reproduce-scope)
           (format stream "~&~A" (getf report :reproduce-scope))))))))

(defun build-spec-check-response (report)
  "Return the MCP response for a CHECK-REPORT plist."
  (case (getf report :status)
    ((:cl-spec-not-loaded :cl-spec-incomplete) (%unavailable-response report))
    (:unresolved-symbol (%unresolved-response report))
    ((:not-registered :invalid-arguments :backend-not-loaded)
     (let ((response (%simple-status-response report)))
       (setf (gethash "verified" response) (json-bool nil))
       response))
    (t
     (let ((selection (getf report :selection)))
       (make-ht "status" (%keyword-string (getf report :status))
                "verified" (json-bool (getf report :verified))
                "selection"
                (make-ht "mode" (getf selection :mode)
                         "requested"
                         (let ((requested (getf selection :requested)))
                           (make-ht "property" (%symbol-ht (getf requested :property))
                                    "symbol" (%symbol-ht (getf requested :symbol))))
                         "selected" (%symbol-hts (getf selection :selected))
                         "count" (getf selection :count)
                         "source" (getf selection :source)
                         "coverage" (getf selection :coverage)
                         "notes" (%strings (getf selection :notes)))
                "results" (coerce (mapcar #'%result-ht (getf report :results))
                                  'vector)
                "counts" (let ((counts (getf report :counts)))
                           (make-ht "selected" (getf counts :selected)
                                    "passed" (getf counts :passed)
                                    "failed" (getf counts :failed)
                                    "errored" (getf counts :errored)
                                    "timed_out" (getf counts :timed-out)
                                    "not_run" (getf counts :not-run)))
                "profile" (%keyword-string (getf report :profile))
                "timeout_seconds" (getf report :timeout-seconds)
                "thread_leaked" (json-bool (getf report :thread-leaked))
                "elapsed" (getf report :elapsed)
                "options" nil
                "options_note" (getf report :options-note)
                "reproduce_scope" (getf report :reproduce-scope)
                "reproduction_faithful" (%faithful-string
                                         (getf report :reproduction-faithful))
                "message" (getf report :message)
                "environment" (%environment-ht (getf report :environment))
                "content" (text-content (%format-check-text report)))))))
```

- [ ] **Step 4: テストが通ることを確認する**

Run: `rove tests/spec-response-builders-test.lisp`
Expected: PASS。7 テスト。

- [ ] **Step 5: lint とコミット**

```bash
mallet src/tools/spec-response-builders.lisp tests/spec-response-builders-test.lisp
git add src/tools/spec-response-builders.lisp tests/spec-response-builders-test.lisp tests.lisp
git commit -m "spec-adapter: put what a caller must not miss in the text a client actually renders"
```

---

### Task 6: tool 定義・worker handler・登録

**Files:**
- Create: `src/tools/spec-entry.lisp`
- Create: `src/tools/spec-tools.lisp`
- Modify: `src/worker/handlers.lisp`
- Modify: `src/tools/all.lisp`
- Modify: `main.lisp`
- Create: `tests/spec-tools-test.lisp`
- Modify: `tests.lisp`

**Interfaces:**
- Consumes: Task 1-5 のすべて
- Produces:
  - `(spec-symbol-response params)` / `(spec-describe-response params)` / `(spec-check-response params)` — `params` は文字列キーの hash-table。API 解決 → report → hash-table を一括で行う
  - `(parse-seed-string text)` → `(values integer nil)` または `(values nil message)`
  - MCP tool `spec-symbol` / `spec-describe` / `spec-check`
  - worker method `worker/spec-symbol` / `worker/spec-describe` / `worker/spec-check`

**なぜ `spec-entry` を別ファイルにするか:** 「API を解決して report を組み立てて
hash-table にする」3 行の並びを、tool の inline path と worker handler の両方が
必要とする。`spec-tools.lisp` に置くと worker が `proxy` と tool registry まで
読み込むことになるので、依存を持たない小さな入口を分ける。

- [ ] **Step 1: 失敗するテストを書く**

`tests/spec-tools-test.lisp` を新規作成する。

```lisp
;;;; tests/spec-tools-test.lisp
;;;;
;;;; The cl-spec tools driven the way a client drives them: a JSON-RPC line in,
;;;; a JSON-RPC line out, with the worker pool disabled so the call runs in
;;;; this image.  These checks are about the tool surface -- schema, argument
;;;; validation, and the shape of an answer when cl-spec is absent -- not about
;;;; cl-spec itself, which tests/spec-adapter-report-test.lisp covers with
;;;; stubs and tests/spec-integration-test.lisp covers with the real thing.

(defpackage #:cl-mcp/tests/spec-tools-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/protocol #:process-json-line)
  (:import-from #:cl-mcp/src/proxy #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/tools/spec-entry #:parse-seed-string)
  (:import-from #:yason #:parse))

(in-package #:cl-mcp/tests/spec-tools-test)

(defvar *tools-loaded* nil
  "True once cl-mcp/main has been loaded into this image.")

(defun %ensure-tools ()
  "Load the tool definitions so process-json-line can dispatch to them."
  (unless *tools-loaded*
    (asdf:load-system "cl-mcp/main")
    (setf *tools-loaded* t)))

(defun %call (name arguments-json)
  "Call tool NAME with ARGUMENTS-JSON and return the parsed result object."
  (%ensure-tools)
  (let* ((*use-worker-pool* nil)
         (line (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",~
\"params\":{\"name\":\"~A\",\"arguments\":~A}}" name arguments-json))
         (response (process-json-line line)))
    (gethash "result" (parse response))))

(defun %text (result)
  "Return the first content text of RESULT."
  (let ((content (gethash "content" result)))
    (when (and (vectorp content) (plusp (length content)))
      (gethash "text" (aref content 0)))))

(deftest spec-tools-are-registered
  (testing "all three tools appear in tools/list with an inputSchema"
    (%ensure-tools)
    (let* ((*use-worker-pool* nil)
           (response (process-json-line
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"))
           (tools (gethash "tools" (gethash "result" (parse response))))
           (names (map 'list (lambda (tool) (gethash "name" tool)) tools)))
      (dolist (name '("spec-symbol" "spec-describe" "spec-check"))
        (ok (member name names :test #'string=))))))

(deftest spec-symbol-requires-a-symbol
  (testing "a missing symbol argument is a validation error, not a crash"
    (let ((result (%call "spec-symbol" "{}")))
      (ok (or (gethash "isError" result)
              (search "symbol" (or (%text result) "")))))))

(deftest spec-describe-rejects-an-unknown-kind
  (testing "kind is constrained and the message says what is allowed"
    (let ((result (%call "spec-describe"
                         "{\"kind\":\"generator\",\"name\":\"cl:car\"}")))
      (ok (search "property" (%text result)))
      (ok (search "spec" (%text result))))))

(deftest spec-check-refuses-both-targets
  (testing "property and symbol together is refused with a usable message"
    (let ((result (%call "spec-check"
                         "{\"property\":\"cl:car\",\"symbol\":\"cl:cdr\"}")))
      (ok (search "both" (string-downcase (%text result)))))))

(deftest spec-check-refuses-a-non-numeric-seed
  (testing "a seed that is not decimal digits is rejected before any run"
    (let ((result (%call "spec-check"
                         "{\"property\":\"cl:car\",\"seed\":\"not-a-number\"}")))
      (ok (search "seed" (string-downcase (%text result)))))))

(deftest parse-seed-string-round-trips-a-big-seed
  (testing "a seed beyond JSON's safe integer parses exactly"
    (multiple-value-bind (value message)
        (parse-seed-string "3963993791726803706")
      (ok (null message))
      (ok (= 3963993791726803706 value))))
  (testing "and anything else is refused rather than coerced"
    (dolist (bad '("-1" "1.5" "" "12a" "0x10"))
      (multiple-value-bind (value message) (parse-seed-string bad)
        (ok (null value))
        (ok (stringp message))))))
```

`tests.lisp` に 1 行足す。

```lisp
  (:import-from #:cl-mcp/tests/spec-tools-test)
```

- [ ] **Step 2: 失敗を確認する**

Run: `rove tests/spec-tools-test.lisp`
Expected: FAIL。`Component "cl-mcp/src/tools/spec-entry" not found`

- [ ] **Step 3: 入口モジュールを書く**

`src/tools/spec-entry.lisp` を新規作成する。

```lisp
;;;; src/tools/spec-entry.lisp
;;;;
;;;; One place where a request becomes a response: resolve the cl-spec API,
;;;; build the report, project it into a hash-table.
;;;;
;;;; Separate from SRC/TOOLS/SPEC-TOOLS.LISP because the worker handlers need
;;;; the same three steps and must not drag the proxy and the tool registry
;;;; into the worker image to get them.

(defpackage #:cl-mcp/src/tools/spec-entry
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:resolve-cl-spec-api)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:symbol-report
                #:describe-report
                #:check-report)
  (:import-from #:cl-mcp/src/tools/spec-response-builders
                #:build-spec-symbol-response
                #:build-spec-describe-response
                #:build-spec-check-response)
  (:export #:spec-symbol-response
           #:spec-describe-response
           #:spec-check-response
           #:parse-seed-string))

(in-package #:cl-mcp/src/tools/spec-entry)

(defun parse-seed-string (text)
  "Return (values SEED NIL) for a decimal seed TEXT, or (values NIL MESSAGE).

A seed arrives as text and never as a JSON number.  cl-spec draws seeds below
2 to the 62nd, and a JSON number that large has already lost digits by the
time it reaches here -- accepting one would mean accepting a seed that cannot
reproduce anything and reporting it as if it could.

PARSE-INTEGER rather than the reader: this is a tool argument from outside the
image, and nothing about a seed calls for reader macros."
  (cond
    ((null text) (values nil nil))
    ((not (stringp text))
     (values nil "seed must be a string of decimal digits"))
    ((zerop (length text))
     (values nil "seed must not be empty"))
    ((notevery #'digit-char-p text)
     (values nil (format nil "seed must be decimal digits only, got ~S. A cl-spec ~
seed can exceed what JSON holds exactly as a number, so it travels as text."
                         text)))
    (t (handler-case (values (parse-integer text) nil)
         (error () (values nil (format nil "seed ~S is not an integer" text)))))))

(defun %string-arg (params name)
  "Return the string argument NAME from PARAMS, or NIL when absent or empty."
  (let ((value (and params (gethash name params))))
    (when (and (stringp value) (plusp (length value))) value)))

(defun spec-symbol-response (params)
  "Return the spec-symbol response hash-table for PARAMS."
  (multiple-value-bind (api status) (resolve-cl-spec-api)
    (build-spec-symbol-response
     (symbol-report api status (gethash "symbol" params)
                    :package (%string-arg params "package")
                    :include-runtime (multiple-value-bind (value present)
                                         (gethash "include_runtime" params)
                                       (if present value t))))))

(defun spec-describe-response (params)
  "Return the spec-describe response hash-table for PARAMS."
  (multiple-value-bind (api status) (resolve-cl-spec-api)
    (build-spec-describe-response
     (describe-report api status
                      (gethash "kind" params)
                      (gethash "name" params)
                      :package (%string-arg params "package")
                      :max-chars (or (gethash "max_chars" params) 8000)))))

(defun spec-check-response (params)
  "Return the spec-check response hash-table for PARAMS.

An unusable seed is answered here rather than passed on: a run started with a
seed the caller did not mean is a run whose result means nothing."
  (multiple-value-bind (seed seed-error)
      (parse-seed-string (%string-arg params "seed"))
    (if seed-error
        (build-spec-check-response
         (list :status :invalid-arguments :verified nil :message seed-error
               :environment (list :cl-spec-loaded nil :cl-spec-status :unknown
                                  :lisp (format nil "~A ~A"
                                                (lisp-implementation-type)
                                                (lisp-implementation-version)))))
        (multiple-value-bind (api status) (resolve-cl-spec-api)
          (build-spec-check-response
           (check-report api status
                         :property (%string-arg params "property")
                         :symbol (%string-arg params "symbol")
                         :package (%string-arg params "package")
                         :profile (%string-arg params "profile")
                         :seed seed
                         :expect-definition-digest
                         (%string-arg params "expect_definition_digest")
                         :timeout-seconds (gethash "timeout_seconds" params)
                         :max-value-chars (or (gethash "max_value_chars" params)
                                              2000)))))))
```

- [ ] **Step 4: tool を書く**

`src/tools/spec-tools.lisp` を新規作成する。

```lisp
;;;; src/tools/spec-tools.lisp
;;;;
;;;; MCP tools over cl-spec: find the contracts registered about a symbol,
;;;; read one, and run it.
;;;;
;;;; All three run in the worker.  The cl-spec registry is populated by
;;;; loading the system that defines the properties, and that load happens in
;;;; the session's worker image -- so discovery, detail and execution have to
;;;; happen there too, or a tool would answer from a registry nobody wrote to.
;;;; It is also what makes "edit, load-system, re-check" work: the definition
;;;; that runs is the one the last load put in this worker.

(defpackage #:cl-mcp/src/tools/spec-tools
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:spec-symbol-response
                #:spec-describe-response
                #:spec-check-response)
  (:import-from #:cl-mcp/src/proxy
                #:with-proxy-dispatch)
  (:export #:spec-symbol
           #:spec-describe
           #:spec-check))

(in-package #:cl-mcp/src/tools/spec-tools)

(define-tool "spec-symbol"
  :description
  "Find the cl-spec contracts registered about a symbol, joined with this
image's own knowledge of it (signature, docstring, source location).

Use this FIRST when you are about to change a function: it tells you which
properties claim something about it, without running anything.

PREREQUISITE: cl-spec must be loaded in this session's worker, and so must the
system that defines the specs and properties.  Load them with 'load-system'
(system 'cl-spec/check-it' for execution as well as introspection).  A status
of cl-spec-not-loaded is NOT evidence that the symbol has no contract.

Property bodies are NOT included here: each entry reports how many forms the
body has and points at 'spec-describe' for the text.  This keeps one response's
size from depending on how much the property's author wrote.

properties_about lists ONLY properties registered with (:about <symbol>).
Callers, generic-function methods, macro users and shared state are not
analysed; this is not a change-impact analysis.

Examples:
  symbol='my-app::transfer'
  symbol='transfer', package='my-app'"
  :args
  ((symbol :type :string :required t
    :description "Symbol to look up: 'SYM', 'PKG:SYM' or 'PKG::SYM'. Resolved with find-symbol; never interned.")
   (package :type :string
    :description "Package for an unqualified symbol (default: COMMON-LISP-USER)")
   (include-runtime :type :boolean :json-name "include_runtime" :default t
    :description "Join this image's signature/docstring/source location (default: true)"))
  :body
  (with-proxy-dispatch (id "worker/spec-symbol"
                           (make-ht "symbol" symbol
                                    "package" package
                                    "include_runtime" include-runtime))
    (result id (spec-symbol-response
                (make-ht "symbol" symbol
                         "package" package
                         "include_runtime" include-runtime)))))

(define-tool "spec-describe"
  :description
  "Read one registered cl-spec definition in full: a property's body and
source form, or a spec's normalized tree.

Use this after 'spec-symbol' has told you which names exist.  A property body
is a specification you can read: it says what relation must hold, over which
generated inputs.  It does not say the relation holds for every input -- only
that this relation is checked over that domain.

kind='property'      the property's arguments, body, source form and digest
kind='spec'          the spec's normalized IR tree
kind='function-spec' NOT SUPPORTED by this cl-spec revision; the tool says so
                     rather than inventing a projection

Long bodies are cut at max_chars and the cut is reported. Truncated text is a
preview for reading, NOT a form that can be read back.

Examples:
  kind='property', name='my-app::transfer-preserves-total'
  kind='spec', name='my-app::account'"
  :args
  ((kind :type :string :required t
    :enum ("property" "spec" "function-spec")
    :description "What to describe")
   (name :type :string :required t
    :description "Registered name: 'SYM', 'PKG:SYM' or 'PKG::SYM'")
   (package :type :string
    :description "Package for an unqualified name (default: COMMON-LISP-USER)")
   (max-chars :type :integer :json-name "max_chars"
    :description "Maximum characters of body and source form (default: 8000)"))
  :body
  (with-proxy-dispatch (id "worker/spec-describe"
                           (make-ht "kind" kind
                                    "name" name
                                    "package" package
                                    "max_chars" max-chars))
    (result id (spec-describe-response
                (make-ht "kind" kind
                         "name" name
                         "package" package
                         "max_chars" max-chars)))))

(define-tool "spec-check"
  :description
  "Run cl-spec properties and return structured results and counterexamples.

Give EITHER property (one named property) OR symbol (every property registered
with (:about <symbol>)).  Not both.

WHAT A RESULT MEANS
  passed          not falsified over the trials that were generated. This is
                  evidence about the inputs that were tried, not a proof.
  failed          a counterexample was found; both the original and the shrunk
                  arguments are reported.
  error           the property body signalled.
  timeout         the deadline expired. NOTHING was proved or disproved.
  generator-error no value could be generated. Nothing was checked.
  not-run         the whole-call budget was spent before this property started.
  no-properties   ZERO properties were selected. This is NOT a successful
                  verification: nothing ran.

verified is true only when at least one property was selected and every one of
them passed.

REPRODUCING A RUN
Every result carries seed (decimal TEXT, because a cl-spec seed can exceed
what JSON holds exactly as a number), profile, and definition_digest.  Re-run
with the same property, seed and profile to regenerate the same trial
sequence.  Pass expect_definition_digest to be told when the definitions moved
underneath you: definition_match then reports match or mismatch.

This regenerates the trial sequence.  It does NOT reproduce the code revision,
external I/O, the clock or shared mutable state, and it is NOT replay of a
saved counterexample against a fixed implementation.

TIMEOUT
timeout_seconds is the budget for the WHOLE call, spent across the selection
in order, enforced by cl-mcp rather than by cl-spec. A property that exhausts
it is reported as timeout and the rest as not-run.  If a run thread cannot be
stopped the response says so; use 'pool-kill-worker' before trusting later
results in that session.

PREREQUISITE: load 'cl-spec/check-it' and the system defining the properties
with 'load-system' first.  After editing a definition, load-system again: this
tool runs what is in the worker image, and it is the same worker your
load-system call wrote to.

Examples:
  symbol='my-app::transfer'
  property='my-app::transfer-preserves-total'
  property='my-app::transfer-preserves-total', seed='3963993791726803706',
    profile='normal', expect_definition_digest='a41f9c2b7d0e5518'"
  :args
  ((property :type :string
    :description "One registered property to run. Exclusive with symbol.")
   (symbol :type :string
    :description "Run every property registered (:about <symbol>). Exclusive with property.")
   (package :type :string
    :description "Package for an unqualified name (default: COMMON-LISP-USER)")
   (profile :type :string
    :description "Trial-count profile, e.g. 'normal' or 'smoke' (default: normal)")
   (seed :type :string
    :description "Decimal digits AS A STRING. A JSON number would already have lost digits.")
   (expect-definition-digest :type :string
    :json-name "expect_definition_digest"
    :description "Digest from an earlier run; a mismatch is reported rather than ignored")
   (timeout-seconds :type :number :json-name "timeout_seconds"
    :description "Budget for the whole call in seconds (default: 60)")
   (max-value-chars :type :integer :json-name "max_value_chars"
    :description "Maximum printed characters per counterexample value (default: 2000)"))
  :body
  (let ((params (make-ht "property" property
                         "symbol" symbol
                         "package" package
                         "profile" profile
                         "seed" seed
                         "expect_definition_digest" expect-definition-digest
                         "timeout_seconds" timeout-seconds
                         "max_value_chars" max-value-chars)))
    (with-proxy-dispatch (id "worker/spec-check" params)
      (result id (spec-check-response params)))))
```

- [ ] **Step 5: worker handler を足す**

`src/worker/handlers.lisp` の `defpackage` に足す。

```lisp
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:spec-symbol-response
                #:spec-describe-response
                #:spec-check-response)
```

`register-all-handlers` の直前に足す。

```lisp
;;; ---------------------------------------------------------------------------
;;; worker/spec-symbol, worker/spec-describe, worker/spec-check
;;; ---------------------------------------------------------------------------
;;;
;;; These run in the worker because the cl-spec registry lives here: the
;;; load-system that defined the properties ran in this image, so this is the
;;; only process that can see them.  Running them in the parent would answer
;;; from an empty registry and report "nothing registered" for every symbol.

(defun %handle-spec-symbol (params)
  "Find the cl-spec contracts registered about a symbol."
  (unless (gethash "symbol" params)
    (error "symbol is required"))
  (spec-symbol-response params))

(defun %handle-spec-describe (params)
  "Read one registered cl-spec definition in full."
  (unless (and (gethash "kind" params) (gethash "name" params))
    (error "kind and name are required"))
  (spec-describe-response params))

(defun %handle-spec-check (params)
  "Run cl-spec properties and return structured results."
  (spec-check-response params))
```

`register-all-handlers` の `(register-method server "worker/macroexpand" ...)`
の直後に 3 行足す。

```lisp
  (register-method server "worker/spec-symbol" #'%handle-spec-symbol)
  (register-method server "worker/spec-describe" #'%handle-spec-describe)
  (register-method server "worker/spec-check" #'%handle-spec-check)
```

- [ ] **Step 6: 登録する**

`src/tools/all.lisp` の `defpackage` の最後の `:import-from` の後に足す。

```lisp
  (:import-from #:cl-mcp/src/tools/spec-tools
                #:spec-symbol
                #:spec-describe
                #:spec-check)
```

`main.lisp` の `(:import-from #:cl-mcp/src/lisp-macroexpand ...)` の後に足す。

```lisp
  (:import-from #:cl-mcp/src/tools/spec-tools
                #:spec-symbol
                #:spec-describe
                #:spec-check)
```

そして `main.lisp` の `:export` 節に足す。

```lisp
           #:spec-symbol
           #:spec-describe
           #:spec-check
```

- [ ] **Step 7: テストが通ることを確認する**

Run: `rove tests/spec-tools-test.lisp`
Expected: PASS。6 テスト。

- [ ] **Step 8: 既存スイートが壊れていないことを確認する**

Run: `rove tests/tools-test.lisp tests/protocol-test.lisp tests/define-tool-test.lisp`
Expected: PASS。tool 登録数を数えるテストがあれば期待値を +3 する。

- [ ] **Step 9: lint とコミット**

```bash
mallet src/tools/spec-entry.lisp src/tools/spec-tools.lisp src/worker/handlers.lisp tests/spec-tools-test.lisp
git add src/tools/spec-entry.lisp src/tools/spec-tools.lisp src/worker/handlers.lisp \
        src/tools/all.lisp main.lisp tests/spec-tools-test.lisp tests.lisp
git commit -m "spec-adapter: expose the three tools, and run them where the registry actually lives"
```

---

### Task 7: cl-spec 実物との統合テスト

**Files:**
- Create: `tests/fixtures/spec-fixture.lisp`
- Create: `tests/spec-integration-test.lisp`
- Modify: `tests.lisp`

**Interfaces:**
- Consumes: Task 6 の `spec-symbol-response` / `spec-describe-response` / `spec-check-response`
- Produces: なし(テストのみ)

**なぜ fixture を別ファイルに置くか:** `defproperty` はマクロ展開時に本体を
関数へコンパイルするので、テスト内で組み立てて `eval` するのは避けたい。
`tests/fixtures/spec-fixture.lisp` は `tests.lisp` にも `.asd` にも登録しない。
ASDF は誰も依存していないファイルを触らないので、通常のロードでは読み込まれない。
テストが明示的に `load` し、**専用の registry へ**登録し、終了時に package を
削除して registry を元へ戻す。意図的に失敗する定義が通常の利用環境に残らない。

- [ ] **Step 1: fixture を書く**

`tests/fixtures/spec-fixture.lisp` を新規作成する。

```lisp
;;;; tests/fixtures/spec-fixture.lisp
;;;;
;;;; Fixture definitions for tests/spec-integration-test.lisp.
;;;;
;;;; Deliberately NOT listed in tests.lisp or cl-mcp.asd: nothing depends on
;;;; this file, so ASDF never loads it and a normal image never sees it.  The
;;;; integration test LOADs it into a registry of its own and deletes the
;;;; package afterwards, because one of the properties below is written to
;;;; fail and a failing property must not be left lying in a working image.
;;;;
;;;; CL-SPEC symbols are written package-qualified so this file has no
;;;; :IMPORT-FROM on a system that may not be loaded.

(defpackage #:cl-mcp/tests/fixtures/spec-fixture
  (:use #:cl)
  (:export #:clamp
           #:small-int
           #:clamp-is-within-bounds
           #:clamp-is-idempotent
           #:clamp-is-wrong-on-purpose))

(in-package #:cl-mcp/tests/fixtures/spec-fixture)

(defun clamp (value low high)
  "Return VALUE confined to the closed interval [LOW, HIGH].

No I/O and no shared state: the smallest thing worth checking a property
against, which is what the first demonstration calls for."
  (cond ((< value low) low)
        ((> value high) high)
        (t value)))

(cl-spec:defspec small-int (and integer (range 0 100)))

(cl-spec:defproperty clamp-is-within-bounds
    ((value small-int) (low small-int))
  "CLAMP never returns a value below LOW."
  (:about clamp)
  (:kind :invariant)
  (:tags :bounds)
  (:trials (:normal 100))
  (>= (clamp value low 100) low))

(cl-spec:defproperty clamp-is-idempotent
    ((value small-int))
  "Clamping twice is the same as clamping once."
  (:about clamp)
  (:kind :idempotence)
  (= (clamp (clamp value 10 90) 10 90)
     (clamp value 10 90)))

(cl-spec:defproperty clamp-is-wrong-on-purpose
    ((value small-int))
  "A property that is false, so a test can see a real counterexample.

Not a claim about CLAMP: it asserts that clamping to [10, 90] leaves every
value unchanged, which is false for anything outside that interval."
  (:about clamp)
  (:kind :invariant)
  (= (clamp value 10 90) value))
```

- [ ] **Step 2: 統合テストを書く**

`tests/spec-integration-test.lisp` を新規作成する。

```lisp
;;;; tests/spec-integration-test.lisp
;;;;
;;;; The adapter against a real cl-spec: fetch the contract, take a real
;;;; counterexample, re-run from the seed, and see an edit reflected.
;;;;
;;;; Skipped, loudly, when cl-spec cannot be resolved.  cl-mcp does not depend
;;;; on cl-spec and its suite must stay green without it -- but a silent skip
;;;; would let this file rot unnoticed, so the skip says why.

(defpackage #:cl-mcp/tests/spec-integration-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:spec-symbol-response
                #:spec-describe-response
                #:spec-check-response)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht))

(in-package #:cl-mcp/tests/spec-integration-test)

(defvar *fixture-loaded* nil
  "True once the fixture file has been loaded into *FIXTURE-REGISTRY*.")

(defvar *fixture-registry* nil
  "The registry the fixture definitions were registered in.")

(defun %cl-spec-available-p ()
  "Return true when cl-spec/check-it can be loaded into this image."
  (handler-case
      (progn
        (unless (find-package "CL-SPEC")
          (asdf:load-system "cl-spec/check-it"))
        (and (find-package "CL-SPEC")
             (symbol-value (find-symbol "*GENERATOR-BACKEND*" "CL-SPEC"))
             t))
    (error () nil)))

(defun %registry-symbol ()
  "Return the CL-SPEC:*REGISTRY* symbol."
  (find-symbol "*REGISTRY*" "CL-SPEC"))

(defun %ensure-fixture ()
  "Load the fixture into a registry of its own, once.

The global registry is swapped rather than rebound: the run thread a deadline
spawns does not inherit dynamic bindings, and the adapter reads the registry
on the calling thread precisely so it can hand it across.  Swapping keeps the
two paths agreeing whichever one a test exercises."
  (unless *fixture-loaded*
    (let* ((registry-symbol (%registry-symbol))
           (make (find-symbol "MAKE-HASH-TABLE-REGISTRY" "CL-SPEC"))
           (previous (symbol-value registry-symbol))
           (fresh (funcall make)))
      (setf (symbol-value registry-symbol) fresh)
      (unwind-protect
           (load (merge-pathnames "tests/fixtures/spec-fixture.lisp"
                                  (asdf:system-source-directory "cl-mcp")))
        (setf (symbol-value registry-symbol) previous))
      (setf *fixture-registry* fresh
            *fixture-loaded* t))))

(defmacro with-fixture-registry (&body body)
  "Run BODY with the fixture's registry installed as CL-SPEC:*REGISTRY*."
  `(let* ((symbol (%registry-symbol))
          (previous (symbol-value symbol)))
     (setf (symbol-value symbol) *fixture-registry*)
     (unwind-protect (progn ,@body)
       (setf (symbol-value symbol) previous))))

(defun %fixture-name (name)
  "Return the qualified designator for a fixture symbol named NAME."
  (format nil "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE::~A" name))

(deftest cl-spec-adapter-end-to-end
  (if (not (%cl-spec-available-p))
      (skip "cl-spec/check-it could not be loaded in this image; the adapter's
cl-spec-facing behaviour is covered by tests/spec-adapter-report-test.lisp
with stubs, and this file needs the real system.")
      (progn
        (%ensure-fixture)
        (with-fixture-registry
          (testing "discovery finds the properties registered about CLAMP"
            (let* ((response (spec-symbol-response
                              (make-ht "symbol" (%fixture-name "CLAMP"))))
                   (properties (gethash "properties" response)))
              (ok (string= "ok" (gethash "status" response)))
              (ok (= 3 (length properties)))
              (ok (string= "CLAMP" (gethash "name" (gethash "symbol" response))))
              (testing "and the runtime join carries the signature"
                (ok (search "VALUE" (gethash "arglist" (gethash "runtime" response)))))
              (testing "and each body is omitted with a pointer to the detail"
                (ok (every (lambda (property) (eq t (gethash "body_omitted" property)))
                           properties)))))

          (testing "detail returns the body the listing omitted"
            (let ((response (spec-describe-response
                             (make-ht "kind" "property"
                                      "name" (%fixture-name "CLAMP-IS-IDEMPOTENT")))))
              (ok (string= "ok" (gethash "status" response)))
              (ok (search "CLAMP" (gethash "body" response)))
              (ok (eq t (gethash "body_complete" response)))))

          (testing "a true property passes and is verified"
            (let ((response (spec-check-response
                             (make-ht "property"
                                      (%fixture-name "CLAMP-IS-WITHIN-BOUNDS")))))
              (ok (string= "completed" (gethash "status" response)))
              (ok (eq t (gethash "verified" response)))
              (let ((result (aref (gethash "results" response) 0)))
                (ok (string= "passed" (gethash "status" result)))
                (ok (= 100 (gethash "budget" (gethash "trials" result)))))))

          (let (seed digest)
            (testing "a false property yields a real counterexample"
              (let* ((response (spec-check-response
                                (make-ht "property"
                                         (%fixture-name "CLAMP-IS-WRONG-ON-PURPOSE"))))
                     (result (aref (gethash "results" response) 0)))
                (ok (string= "completed" (gethash "status" response)))
                (ok (eq yason:false (gethash "verified" response)))
                (ok (string= "failed" (gethash "status" result)))
                (ok (plusp (length (gethash "counterexample" result))))
                (setf seed (gethash "seed" result)
                      digest (gethash "definition_digest" result))
                (testing "the seed is text, and long enough to have needed to be"
                  (ok (stringp seed))
                  (ok (every #'digit-char-p seed)))))

            (testing "the same seed reproduces the same counterexample"
              (let* ((response (spec-check-response
                                (make-ht "property"
                                         (%fixture-name "CLAMP-IS-WRONG-ON-PURPOSE")
                                         "seed" seed
                                         "profile" "normal"
                                         "expect_definition_digest" digest)))
                     (result (aref (gethash "results" response) 0)))
                (ok (string= "failed" (gethash "status" result)))
                (ok (string= seed (gethash "seed" result)))
                (ok (string= "match" (gethash "definition_match" result)))
                (ok (string= "faithful" (gethash "reproduction_faithful" response)))))

            (testing "a digest from a different definition is reported, not ignored"
              (let ((response (spec-check-response
                               (make-ht "property"
                                        (%fixture-name "CLAMP-IS-WRONG-ON-PURPOSE")
                                        "seed" seed
                                        "profile" "normal"
                                        "expect_definition_digest" "0000000000000000"))))
                (ok (string= "unfaithful" (gethash "reproduction_faithful" response)))
                (ok (string= "mismatch"
                             (gethash "definition_match"
                                      (aref (gethash "results" response) 0)))))))

          (testing "selecting by symbol runs all three and is not verified"
            (let ((response (spec-check-response
                             (make-ht "symbol" (%fixture-name "CLAMP")))))
              (ok (= 3 (gethash "count" (gethash "selection" response))))
              (ok (eq yason:false (gethash "verified" response)))
              (ok (= 1 (gethash "failed" (gethash "counts" response))))
              (ok (= 2 (gethash "passed" (gethash "counts" response))))))

          (testing "a symbol with nothing registered is not a clean bill"
            (let ((response (spec-check-response (make-ht "symbol" "cl:car"))))
              (ok (string= "no-properties" (gethash "status" response)))
              (ok (eq yason:false (gethash "verified" response)))))))))

(deftest cl-spec-adapter-sees-a-reload
  (if (not (%cl-spec-available-p))
      (skip "cl-spec/check-it could not be loaded in this image.")
      (progn
        (%ensure-fixture)
        (with-fixture-registry
          (testing "re-registering a property changes its digest and its verdict"
            (let* ((before (spec-check-response
                            (make-ht "property"
                                     (%fixture-name "CLAMP-IS-WRONG-ON-PURPOSE"))))
                   (before-result (aref (gethash "results" before) 0))
                   (before-digest (gethash "definition_digest" before-result)))
              (ok (string= "failed" (gethash "status" before-result)))
              ;; Re-register the property as a statement that holds, exactly
              ;; as editing the file and loading it again would.
              (funcall (find-symbol "REGISTER-CORRECTED-PROPERTY"
                                    "CL-MCP/TESTS/FIXTURES/SPEC-FIXTURE"))
              (let* ((after (spec-check-response
                             (make-ht "property"
                                      (%fixture-name "CLAMP-IS-WRONG-ON-PURPOSE"))))
                     (after-result (aref (gethash "results" after) 0)))
                (ok (string= "passed" (gethash "status" after-result)))
                (ok (eq t (gethash "verified" after)))
                (testing "and the digest moved, so an old seed is not faithful"
                  (ok (not (string= before-digest
                                    (gethash "definition_digest" after-result))))))))))))
```

`tests/fixtures/spec-fixture.lisp` の `:export` に `#:register-corrected-property`
を足し、ファイル末尾に次を足す。

```lisp
(defun register-corrected-property ()
  "Re-register CLAMP-IS-WRONG-ON-PURPOSE as a statement that holds.

Stands in for an edit followed by a reload: DEFPROPERTY expands into a
REGISTER-PROPERTY call, so evaluating this replaces the registered definition
with one carrying a fresh source form, metadata and compiled function --
exactly what loading an edited file produces."
  (cl-spec:defproperty clamp-is-wrong-on-purpose
      ((value small-int))
    "Corrected: clamping to [10, 90] is idempotent."
    (:about clamp)
    (:kind :invariant)
    (= (clamp (clamp value 10 90) 10 90)
       (clamp value 10 90))))
```

`tests.lisp` に 1 行足す。

```lisp
  (:import-from #:cl-mcp/tests/spec-integration-test)
```

- [ ] **Step 3: cl-spec を ASDF から見えるようにする**

cl-spec は roswell の local-projects にある。テスト実行前に一度だけ登録する。

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd "/home/wiz/.roswell/local-projects/cl-spec/cl-spec.asd")' \
  --eval '(print (asdf:find-system "cl-spec/check-it" nil))'
```
Expected: システムが見つかること。見つからない場合は
`~/.config/common-lisp/source-registry.conf.d/` に local-projects を足すか、
テストの `%cl-spec-available-p` が false を返して skip になる(それも正しい挙動)。

- [ ] **Step 4: テストを実行する**

Run: `rove tests/spec-integration-test.lisp`
Expected: PASS(cl-spec が解決できる場合)、または skip 理由が出力されること。
どちらでも赤にならないこと。

- [ ] **Step 5: lint とコミット**

```bash
mallet tests/spec-integration-test.lisp tests/fixtures/spec-fixture.lisp
git add tests/fixtures/spec-fixture.lisp tests/spec-integration-test.lisp tests.lisp
git commit -m "spec-adapter: take a real counterexample, replay it, and watch a redefinition move the digest"
```

---

### Task 8: 実 tool 経由の実証と全体検証

**Files:**
- Create: `<scratchpad>/spec-demo/clamp.lisp`(使い捨て。リポジトリには入れない)
- Create: `<scratchpad>/spec-demo/drive.lisp`(使い捨て)
- Modify: `docs/superpowers/plans/2026-09-09-cl-spec-mcp-adapter.md`(実行結果の記録)

**Interfaces:**
- Consumes: Task 6 の tool 群
- Produces: なし(検証のみ)

**目的:** 単体・統合テストは `process-json-line` を pool 無効で叩く。ここでは
**worker pool を有効にした実プロセス**で、契約取得 → 失敗取得 → 実装修正 →
保存 → 再ロード → 同一 seed で再検証、を一周させる。
定義のロードと Property 実行が同じ worker で起きること、再ロードした変更が
検査へ反映されること、seed による再現が実装修正の前後で成立することを、
tool の外側から確認する。

fixture は scratchpad にのみ置く。cl-mcp / cl-spec のソースツリーにも、
稼働中の MCP worker にも、意図的に失敗するコードを残さない。

- [ ] **Step 1: 実証用 fixture を書く**

`<scratchpad>/spec-demo/clamp.lisp` を作る。実装にバグがあり、Property は正しい。
外部 I/O も共有可変状態も持たない。

```lisp
(defpackage #:spec-demo
  (:use #:cl)
  (:export #:clamp))

(in-package #:spec-demo)

(defun clamp (value low high)
  "Return VALUE confined to [LOW, HIGH]."
  ;; Bug on purpose: the upper bound is never applied.
  (cond ((< value low) low)
        (t value)))

(cl-spec:defspec small-int (and integer (range 0 100)))

(cl-spec:defproperty clamp-respects-high ((value small-int))
  "CLAMP never returns more than HIGH."
  (:about clamp)
  (:kind :invariant)
  (:trials (:normal 100))
  (<= (clamp value 0 50) 50))

(cl-spec:defproperty clamp-respects-low ((value small-int))
  "CLAMP never returns less than LOW."
  (:about clamp)
  (:kind :invariant)
  (>= (clamp value 10 100) 10))
```

- [ ] **Step 2: ドライバを書く**

`<scratchpad>/spec-demo/drive.lisp` を作る。worker pool を有効にしたまま
`process-json-line` を呼ぶので、呼び出しは実際に worker へ routing される。

```lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(push #p"/home/wiz/.roswell/local-projects/cl-ai-project/cl-mcp/" asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning))
  (ql:quickload "cl-mcp" :silent t))

(defvar *demo-dir* "<scratchpad>/spec-demo/")

(defun call-tool (name arguments-json)
  "Send one tools/call line and print the content text of the answer."
  (let* ((line (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",~
\"params\":{\"name\":\"~A\",\"arguments\":~A}}" name arguments-json))
         (response (cl-mcp/src/protocol:process-json-line line))
         (result (gethash "result" (yason:parse response)))
         (content (and result (gethash "content" result))))
    (format t "~&~%========== ~A ==========~%" name)
    (if (and content (plusp (length content)))
        (format t "~A~%" (gethash "text" (aref content 0)))
        (format t "~A~%" response))
    result))

(setf (symbol-value (find-symbol "*CURRENT-SESSION-ID*" "CL-MCP/SRC/PROXY"))
      "spec-demo-session")

;; 1. Make cl-spec findable inside the worker, then load it there.
(call-tool "repl-eval"
           "{\"code\":\"(asdf:load-asd \\\"/home/wiz/.roswell/local-projects/cl-spec/cl-spec.asd\\\")\"}")
(call-tool "load-system" "{\"system\":\"cl-spec/check-it\",\"force\":false}")

;; 2. Load the buggy fixture into the same worker.
(call-tool "repl-eval"
           (format nil "{\"code\":\"(load \\\"~Aclamp.lisp\\\")\"}" *demo-dir*))

;; 3. Fetch the contract, then one property in full.
(call-tool "spec-symbol" "{\"symbol\":\"spec-demo::clamp\"}")
(call-tool "spec-describe"
           "{\"kind\":\"property\",\"name\":\"spec-demo::clamp-respects-high\"}")

;; 4. Run the related properties and take the counterexample.
(defvar *failure*
  (call-tool "spec-check" "{\"symbol\":\"spec-demo::clamp\"}"))

(defvar *failed-result*
  (find-if (lambda (r) (string= "failed" (gethash "status" r)))
           (gethash "results" *failure*)))

(defvar *seed* (gethash "seed" *failed-result*))
(defvar *digest* (gethash "definition_digest" *failed-result*))
(format t "~&~%seed=~A digest=~A~%" *seed* *digest*)

;; 5. Fix the implementation on disk, leaving the contract untouched.
(call-tool "lisp-patch-form"
           (format nil "{\"path\":\"~Aclamp.lisp\",\"form_type\":\"defun\",~
\"form_name\":\"clamp\",\"old_text\":\"  ;; Bug on purpose: the upper bound is never applied.~
\\n  (cond ((< value low) low)\\n        (t value))\",~
\"new_text\":\"  (cond ((< value low) low)\\n        ((> value high) high)\\n        (t value))\"}"
                   *demo-dir*))

;; 6. Reload into the same worker and re-check from the same seed.
(call-tool "repl-eval"
           (format nil "{\"code\":\"(load \\\"~Aclamp.lisp\\\")\"}" *demo-dir*))
(call-tool "spec-check"
           (format nil "{\"property\":\"spec-demo::clamp-respects-high\",~
\"seed\":\"~A\",\"profile\":\"normal\",\"expect_definition_digest\":\"~A\"}"
                   *seed* *digest*))

;; 7. Prove the negative cases are not dressed up as success.
(call-tool "spec-check" "{\"symbol\":\"cl:car\"}")
(call-tool "spec-check" "{\"property\":\"spec-demo::no-such-property\"}")

(sb-ext:quit)
```

- [ ] **Step 3: 実証を走らせ、出力を確認する**

Run: `sbcl --script <scratchpad>/spec-demo/drive.lisp 2>&1 | tee <scratchpad>/spec-demo/run.log`

確認すること(すべて出力テキストで見えること):

1. `spec-symbol` が 2 件の property を列挙し、body を省略したと述べている
2. `spec-describe` が body を返している
3. 最初の `spec-check` が `NOT VERIFIED`、`clamp-respects-high` が `failed`、
   反例と shrunk 反例が両方出ている、`verified: false`
4. seed が 10 進の文字列、digest が 16 桁 hex
5. 修正後の `spec-check` が `PASSED`、`definition_match` が `match`、
   `reproduction: faithful`
6. `cl:car` に対して `NO PROPERTIES` と
   `0 properties selected -- this is NOT a successful verification`
7. 未登録 property が `not-registered`

- [ ] **Step 4: 結果を計画書へ記録する**

`docs/superpowers/plans/2026-09-09-cl-spec-mcp-adapter.md` の末尾に
「## 実証結果」節を追加し、実際の出力から次を貼る。
seed、digest、反例、修正前後の status。
掲載する値は実行結果そのものにする(手で書き換えない)。

- [ ] **Step 5: cl-spec を使わない環境での非破壊を確認する**

cl-spec を一切ロードしない新規プロセスで、既存 tool と新 tool の双方を叩く。

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --load ~/quicklisp/setup.lisp \
  --eval '(push #p"/home/wiz/.roswell/local-projects/cl-ai-project/cl-mcp/" asdf:*central-registry*)' \
  --eval '(handler-bind ((warning (function muffle-warning))) (ql:quickload "cl-mcp" :silent t))' \
  --eval '(setf (symbol-value (find-symbol "*USE-WORKER-POOL*" "CL-MCP/SRC/PROXY")) nil)' \
  --eval '(princ (cl-mcp/src/protocol:process-json-line "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"spec-symbol\",\"arguments\":{\"symbol\":\"cl:car\"}}}"))' \
  --eval '(terpri)' \
  --eval '(princ (cl-mcp/src/protocol:process-json-line "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\",\"params\":{\"name\":\"repl-eval\",\"arguments\":{\"code\":\"(+ 1 2)\"}}}"))'
```
Expected: 1 本目が `cl-spec-not-loaded` と load-system の案内を返し、
2 本目が `3` を返すこと。cl-spec が無くても既存 tool が壊れていないこと。

- [ ] **Step 6: 冷えた fasl でコンパイル警告を確認する**

開発中の REPL ではなく新規プロセスで走らせる。既にロード済みの image で
コンパイルすると全 generic function が再定義され、その警告に本物が埋もれる。

Run:
```bash
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --load ~/quicklisp/setup.lisp \
  --eval '(push #p"/home/wiz/.roswell/local-projects/cl-ai-project/cl-mcp/" asdf:*central-registry*)' \
  --eval '(asdf:compile-system :cl-mcp :force :all)' 2>&1 | tail -40
```
Expected: 新しい警告が出ないこと。UIOP 由来の既知の警告(約 427 件)は既存のノイズ。

- [ ] **Step 7: 全スイートを走らせる**

Run: `rove cl-mcp.asd 2>&1 | tee <scratchpad>/full-suite.log`

**exit code を根拠にしない。** ロードに失敗しても 0 が返る。
次を数えて確認する。

```bash
grep -c "^;; testing '" <scratchpad>/full-suite.log   # 実行されたスイート数
grep -c "✓" <scratchpad>/full-suite.log               # 成功アサーション数
grep -n "✗\|failed\|FAIL" <scratchpad>/full-suite.log | head -40
```
Expected: 変更前のベースラインと比べてスイート数が +5(新規テストファイル 5 本)、
失敗が増えていないこと。ベースラインは実装開始前に同じコマンドで取っておく。

- [ ] **Step 8: lint**

Run: `mallet src/*.lisp src/*/*.lisp tests/*.lisp`
Expected: 新規ファイル由来の警告が無いこと。

- [ ] **Step 9: fixture を片付ける**

Run: `rm -rf <scratchpad>/spec-demo`(ログは報告に転記した後で削除する)
Expected: 意図的に失敗する定義がどこにも残っていないこと。
`git status` に `spec-demo` が現れないこと。

- [ ] **Step 10: コミット**

```bash
git add docs/superpowers/plans/2026-09-09-cl-spec-mcp-adapter.md
git commit -m "docs: record what the tools actually returned end to end"
```

---

## Self-Review 記録

**Spec coverage**

| 設計書の節 | 実装する Task |
|---|---|
| §3 アーキテクチャ / 遅延解決 | 1 |
| §3.2 worker 実行 | 6 |
| §4 4 つの状態 | 1(判定)、3(報告)、5(text) |
| §5 symbol 解決 | 1 |
| §6 spec-symbol | 3(report)、5(response)、6(tool) |
| §7 spec-describe | 3、5、6 |
| §8.1-8.2 引数と seed | 6(`parse-seed-string`) |
| §8.3 選択根拠 | 4(`%select-properties`) |
| §8.4 status 語彙 | 4 |
| §8.5 時間上限 | 4(`%run-one`、全体予算) |
| §8.6 trial 予算 | 4(`%trials-budget`) |
| §8.7 反例の外部表現 | 2(`externalize-value`)、5(`%named-value-hts`) |
| §8.8 再現性と digest | 2(`definition-digest`)、4(`definition-match`) |
| §9 content text | 5 |
| §10.1 単体 | 1-4 |
| §10.2 統合 | 7 |
| §10.3 実 tool 経由 | 8 |
| §10.4 非破壊 | 8 |
| §11 §72 到達範囲 | 最終報告 |
| §12 cl-spec 側必要変更 | 最終報告 |

**型の一貫性**

- `cl-spec-api` の生成は全 Task で `make-cl-spec-api`、参照は `api-fn` / `api-has-p` / `api-class`。
- report plist のキーは kebab keyword、応答 hash-table のキーは snake_case 文字列。変換は Task 5 のビルダーのみが行う。
- `definition-match` は report 層で `:true` / `:false` / `:not-checked`、応答で `"match"` / `"mismatch"` / `"not-checked"`。
- `reproduction-faithful` は report 層で `t` / `nil` / `:not-checked`、応答で `"faithful"` / `"unfaithful"` / `"not-checked"`。
- seed は report 層以降つねに文字列。整数は `check-report` の引数と cl-spec 呼び出しの内側だけ。
