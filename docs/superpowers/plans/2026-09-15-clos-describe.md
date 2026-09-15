# clos-describe Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 新ツール `clos-describe` で、総称関数のメソッド（修飾子・特化子・行・`lisp-edit-form` 用 form_name）とクラスの階層・直接/実効スロット・特化メソッドを、クラスを finalize せず initform を評価せずに返す。前提として、クラス・メソッドの行解決と `lisp-edit-form` の defmethod 照合を直す。

**Architecture:** worker（`src/clos-core.lisp`）が sb-mop で実行時情報を読み、各定義の abs_path / line を `src/code-core.lisp` の `definition-source-location` で解決した report を返す。親（`src/clos.lisp`）は `src/tools/clos-response-builders.lisp` で、report の各定義に `code-refs-scan:top-level-forms-at` でソースから form_type / form_name を付け、本文テキストを組み立てる。

**Tech Stack:** SBCL（sb-mop, sb-introspect, sb-c 内部）, eclector（親のみ）, yason, cl-ppcre, Rove

**Spec:** `docs/superpowers/specs/2026-09-15-clos-describe-design.md`

**Prototype:** 本計画のコードはすべて worker 上で試作・実測済み（行解決の一致、往復テスト 37 件、既存 `lisp-edit-form-test` 309 件・`code-refs-scan-test` 123 件の成功、in-process と worker JSON 往復で本文が一致）。

## Global Constraints

- SBCL 専用（`project_sbcl_only`）。他処理系の可搬性は考えない
- Google Common Lisp Style Guide: 2 スペースインデント、1 行 100 桁以内、トップレベルフォーム間に空行、公開関数には docstring
- **mallet は `::` を禁止する（src も tests も）**。SBCL 内部関数は `find-symbol` 経由で呼ぶ（`%sbcl-function`）。テストで sb-introspect / sb-posix を呼ぶときは `uiop:symbol-call`
- **worker は eclector に依存しない**: `src/clos-core.lisp` は `cl-mcp/src/cst`・`code-refs-scan`・`lisp-edit-form-core`・`tools/clos-response-builders` を import してはならない
- 本番コードで `intern` / `eval` / `read-from-string` によるシンボル解決をしない（`resolve-target` を使う）
- **読み取り専用**: clos-core は `finalize-inheritance`・`make-instance`・`class-slots`（未 finalize のクラス）・initfunction の呼び出しをしない
- JSON 配列は組み立て時に vector、真偽値は `json-bool`（in-process では `yason:false`）。読む側は `sequence->list` と `%true-p` を通し、worker 経由（配列は list、false/null は NIL）でも同じ結果にする
- Rove で condition を検査するときは `signals` ではなく `handler-case` で包む
- package-inferred-system: `cl-mcp.asd` は編集しない。新しい src は `src/tools/all.lisp` と `main.lisp`、新しいテストはルートの `tests.lisp` に登録する
- Lisp ソースの編集は cl-mcp の `lisp-edit-form` / `lisp-patch-form`、新規ファイルは `fs-write-file`。**src に export を足すと、稼働中の MCP 親イメージはそのシンボルを参照するファイルを解析できなくなる**。そのときに限り Edit/Write を使い、直後に `lisp-check-parens` と `mallet` で確認する
- コミットは変更したファイルをパス指定で `git add` する。`coverage/` と `src/specs/` は無関係なので絶対に add しない
- コミットメッセージの末尾に次の 2 行を付ける:
  ```
  Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01FwJGaMhQXB93nGDWsR7eSS
  ```

### テストの実行方法（全タスク共通）

worker / コアのコードを変えるので、稼働中のイメージではなく新しいプロセスで実行する（`<name>` はテストファイル名から `.lisp` を除いたもの、例 `code-test`）:

```bash
ros run -e '(ql:quickload :rove :silent t)' \
        -e '(asdf:load-asd (truename "cl-mcp.asd"))' \
        -e '(handler-bind ((warning (function muffle-warning))) (asdf:load-system "cl-mcp/tests/<name>"))' \
        -e '(uiop:quit (if (rove:run :cl-mcp/tests/<name>) 0 1))' 2>&1 | tail -40
```

判定は終了コードではなく出力の `Summary:` と `✗` の有無で行う。ロードエラーはデバッガのメッセージとして出る。

### Lint（コミット前に必ず）

```bash
mallet <変更した .lisp ファイル>
```

---

## File Structure

| ファイル | 側 | 責務 |
|---|---|---|
| `src/lisp-edit-form-core.lisp`（変更） | 親 | defmethod の候補を名前だけ・1 行で印字、利用者の form_name を正規化、完全一致の優先 |
| `src/code-core.lisp`（変更） | 両方 | `definition-source-line` / `definition-source-location` / `with-definition-source-cache` / `%read-form-starts` / `generic-function-method-count`。`code-find-definition` がクラスの行を返す |
| `src/code-refs-core.lisp`（変更） | 両方 | `%status-string` と `*note-stale*` を export |
| `src/code-refs-scan.lisp`（変更） | 親 | `top-level-forms-at` |
| `src/clos-core.lisp`（新規） | worker | `clos-describe-report`: 総称関数・メソッド・クラス・スロットの report |
| `src/tools/clos-response-builders.lisp`（新規） | 親 | `annotate-report-forms`、`build-clos-describe-response` |
| `src/clos.lisp`（新規） | 親 | `define-tool "clos-describe"` |
| `src/worker/handlers.lisp`（変更） | worker | `worker/clos-describe`、code-describe のメソッド数 |
| `src/tools/response-builders.lisp`（変更） | 両方 | `build-code-describe-response` の案内行 |
| `src/inspect.lisp`（変更） | 両方 | `inspect-object` の `hint` |
| `src/code.lisp`（変更） | 親 | code-describe にメソッド数を渡す |
| `src/tools/all.lisp`、`main.lisp`、`tests.lisp`（変更） | - | 登録 |
| `tests/fixtures/clos-fixture.lisp`（新規） | - | CLOS 定義のフィクスチャ |
| `tests/clos-core-test.lisp`、`tests/clos-response-builders-test.lisp`（新規） | - | テスト |
| `docs/tools.md`、`prompts/repl-driven-development.md`、`CLAUDE.md`（変更） | - | 文書 |

---

### Task 1: `lisp-edit-form` の defmethod 照合を直す

**Files:**
- Modify: `src/lisp-edit-form-core.lisp`（`%defmethod-candidates` と `%find-target` を置き換え、補助関数 3 つを追加）
- Test: `tests/lisp-edit-form-test.lisp`（テスト 4 本とフィクスチャ用パッケージ）、`tests/code-refs-scan-test.lisp`（テスト 1 本）

**Interfaces:**
- Consumes: 既存の `%normalize-string`、`%strip-hash-colon`、`%strip-name-prefix`、`%whitespace-char-p`、`%definition-candidates`
- Produces:
  - `%defmethod-candidates (form) → list of string`（戻り値の形は同じ。修飾子とラムダリストはパッケージ接頭辞なし・改行なし）
  - `%find-target (nodes form-type form-name)`（シグネチャ同じ。form_name の接頭辞・改行を無視し、`[N]` なしなら署名全体の完全一致を優先）
  - 内部: `%names-only (tree)`、`%signature-text (object) → string`、`%normalize-form-name-text (s) → string`

- [ ] **Step 1: 失敗するテストを書く**

`tests/lisp-edit-form-test.lisp` の `(setf cl-mcp/src/project-root:*project-root* ...)` フォームの直後に、テスト用パッケージを足す:

```lisp
;;; A package the CST reader reads the method fixtures below into.  Printing
;;; their symbols from any other package qualified them, which is what made
;;; defmethod form_names fail to match.
(defpackage #:cl-mcp-edit-form-method-fixture
  (:use #:cl))
```

`lisp-edit-form-defmethod-preserves-hash-colon-in-strings` の直後に追加:

```lisp
(deftest lisp-edit-form-defmethod-in-another-package
  (testing "a defmethod read in a package of its own matches name + lambda list"
    (with-temp-file "tests/tmp/edit-form-method-package.lisp"
        (format nil "(in-package #:cl-mcp-edit-form-method-fixture)~%~%~
(defmethod paint ((w widget) stream)~%  (list :widget w stream))~%~%~
(defmethod paint ((g gadget) stream)~%  (list :gadget g stream))~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defmethod"
                        :form-name "paint ((g gadget) stream)"
                        :operation "replace"
                        :content (format nil "(defmethod paint ((g gadget) stream)~%  ~
(list :gadget-replaced g stream))"))
        (let ((updated (fs-read-file path)))
          (ok (search ":gadget-replaced" updated))
          (ok (search "(list :widget w stream)" updated)))))))

(deftest lisp-edit-form-defmethod-ignores-prefixes-and-line-breaks
  (testing "package prefixes and line breaks in form_name do not matter"
    (with-temp-file "tests/tmp/edit-form-method-prefix.lisp"
        (format nil "(in-package #:cl-mcp-edit-form-method-fixture)~%~%~
(defmethod paint ((w widget) stream)~%  (list :widget w stream))~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defmethod"
                        :form-name (format nil "cl-mcp-edit-form-method-fixture::paint ~
((w cl-mcp-edit-form-method-fixture::widget)~%    stream)")
                        :operation "replace"
                        :content (format nil "(defmethod paint ((w widget) stream)~%  ~
(list :widget-replaced w stream))"))
        (ok (search ":widget-replaced" (fs-read-file path)))))))

(deftest lisp-edit-form-defmethod-long-lambda-list
  (testing "a lambda list longer than a printed line still matches on one line"
    (with-temp-file "tests/tmp/edit-form-method-long.lisp"
        (format nil "(defmethod write-out ((stream sink) string &optional (start 0) end ~
(fill-pointer-output nil) (element-type 'character))~%  ~
(list stream string start end fill-pointer-output element-type))~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defmethod"
                        :form-name "write-out ((stream sink) string &optional (start 0) end (fill-pointer-output nil) (element-type 'character))"
                        :operation "replace"
                        :content (format nil "(defmethod write-out ((stream sink) string ~
&optional (start 0) end (fill-pointer-output nil) (element-type 'character))~%  :long-replaced)"))
        (ok (search ":long-replaced" (fs-read-file path)))))))

(deftest lisp-edit-form-defmethod-prefers-the-exact-signature
  (testing "a primary method's full signature does not also pick the :around method"
    (with-temp-file "tests/tmp/edit-form-method-around.lisp"
        (format nil "(defmethod area ((s circle))~%  :primary)~%~%~
(defmethod area :around ((s circle))~%  (call-next-method))~%")
      (lambda (path)
        (lisp-edit-form :file-path path
                        :form-type "defmethod"
                        :form-name "area ((s circle))"
                        :operation "replace"
                        :content (format nil "(defmethod area ((s circle))~%  :primary-replaced)"))
        (let ((updated (fs-read-file path)))
          (ok (search ":primary-replaced" updated))
          (ok (search "(call-next-method)" updated)))))))
```

長い `:form-name` 文字列の行は 100 桁を超える。mallet が桁数を警告したら、その文字列を `(concatenate 'string "write-out ((stream sink) string &optional (start 0) end " "(fill-pointer-output nil) (element-type 'character))")` に分ける（文字列リテラルの行末 `\` は改行を埋め込むので使わない）。

`tests/code-refs-scan-test.lisp` の `scan-text-records-enclosing-form` の直後に追加:

```lisp
(deftest scan-text-form-name-is-one-line
  (testing "a defmethod's form_name has no line break however long its lambda list"
    (let* ((text (format nil "(defmethod write-out ((stream sink) string &optional (start 0) end ~
(fill-pointer-output nil) (element-type 'character))~%  (target stream))"))
           (form (first (scan-text text "TARGET"))))
      (ok (equal (concatenate 'string
                              "write-out ((stream sink) string &optional (start 0) end "
                              "(fill-pointer-output nil) (element-type 'character))")
                 (gethash "form_name" form))))))
```

- [ ] **Step 2: テストが失敗することを確認する**

テストの実行方法の `<name>` を `lisp-edit-form-test`、次に `code-refs-scan-test` にして実行。
Expected: 新しい 5 本が失敗する（試作で確認済みの失敗: `Form defmethod paint ((g gadget) stream) not found` ×2 系統、`write-out ...` not found、`Multiple matches for defmethod area ((s circle))`、form_name に改行）。

- [ ] **Step 3: 補助関数を追加する**

`lisp-edit-form` の `insert_after`、`form_type` `defun`、`form_name` `%normalize-string` で、次の 2 フォームを追加:

```lisp
(defun %names-only (tree)
  "Return TREE with each symbol outside COMMON-LISP and KEYWORD replaced by an
uninterned symbol of the same name, so %SIGNATURE-TEXT prints it bare.
COMMON-LISP symbols are kept so the pretty printer still writes (QUOTE X) as
'X; they print without a prefix from COMMON-LISP-USER anyway."
  (let ((cl (find-package "COMMON-LISP"))
        (keyword (find-package "KEYWORD")))
    (labels ((walk (node)
               (cond
                 ((consp node) (cons (walk (car node)) (walk (cdr node))))
                 ((and (symbolp node)
                       (not (member (symbol-package node) (list cl keyword))))
                  (make-symbol (symbol-name node)))
                 (t node))))
      (walk tree))))

(defun %signature-text (object)
  "Return OBJECT, part of a definition's signature, printed as form names are
compared: lower case, on one line, and with no package prefix on any symbol.

The package a form was read in decides how PRIN1 qualifies its symbols, so
printing them as read made a method's lambda list come out as
\"((stream cl-mcp/src/utils/bounded-stream:bounded-output-stream) character)\"
in one process and unqualified in another, and a long lambda list gained line
breaks.  Neither matched what a caller writes."
  (let ((*package* (find-package "COMMON-LISP-USER"))
        (*print-gensym* nil)
        (*print-pretty* t)
        (*print-right-margin* most-positive-fixnum)
        (*print-readably* nil))
    (string-downcase (prin1-to-string (%names-only object)))))
```

`insert_after`、`form_name` `%strip-hash-colon` で次を追加:

```lisp
(defun %normalize-form-name-text (s)
  "Return S, a form_name a caller wrote, as the candidates are written.
Outside string literals, each run of whitespace becomes one space and a
package prefix -- 'pkg:' or 'pkg::' at the start of a token -- is dropped, so
\"sb-gray:stream-write-char ((stream\\n  bounded-output-stream) character)\"
reads as the candidate does.  A token starting with a colon is a keyword and
is kept."
  (with-output-to-string (out)
    (let ((len (length s))
          (in-string nil)
          (pending-space nil)
          (i 0))
      (flet ((token-start-p ()
               ;; I begins a token when nothing, whitespace or an opening
               ;; delimiter precedes it.
               (or (zerop i)
                   (find (char s (1- i)) '(#\( #\' #\` #\, #\Space #\Tab
                                           #\Newline #\Return #\Page)))))
        (loop while (< i len) do
          (let ((c (char s i)))
            (cond
              ((and in-string (char= c #\\) (< (1+ i) len))
               (write-char c out)
               (write-char (char s (1+ i)) out)
               (incf i 2))
              (in-string
               (when (char= c #\") (setf in-string nil))
               (write-char c out)
               (incf i))
              ((%whitespace-char-p c)
               (setf pending-space t)
               (incf i))
              (t
               (when pending-space
                 (write-char #\Space out)
                 (setf pending-space nil))
               (if (and (token-start-p) (not (find c "():\"'`,#")))
                   ;; Copy the token from just past its last colon.
                   (let* ((end (or (position-if (lambda (ch)
                                                  (or (%whitespace-char-p ch)
                                                      (find ch "()\"'`,")))
                                                s :start i)
                                   len))
                          (colon (position #\: s :start i :end end :from-end t)))
                     (write-string s out :start (if colon (1+ colon) i) :end end)
                     (setf i end))
                   (progn
                     (when (char= c #\") (setf in-string t))
                     (write-char c out)
                     (incf i)))))))))))
```

- [ ] **Step 4: `%defmethod-candidates` と `%find-target` を置き換える**

`lisp-edit-form` `replace`、`form_type` `defun`、`form_name` `%defmethod-candidates`:

```lisp
(defun %defmethod-candidates (form)
  "Return candidate signature strings for a DEFMETHOD FORM.
Candidates are generated in order of specificity:
1. name only: \"resize\"
2. name + qualifier: \"resize :after\"
3. name + lambda-list: \"resize ((s shape) factor)\"
4. name + qualifier + lambda-list: \"resize :after ((s shape) factor)\"

Qualifiers and the lambda list are printed by %SIGNATURE-TEXT, so symbols
carry no package prefix and '#:' never appears, whichever package the form
was read in."
  (destructuring-bind
      (_ name &rest rest)
      form
    (declare (ignore _))
    (let ((qualifiers 'nil) (lambda-list nil))
      (dolist (part rest)
        (when (listp part) (setf lambda-list part) (return))
        (push part qualifiers))
      (let ((name-str (%normalize-string name))
            (lambda-str (and lambda-list (%signature-text lambda-list)))
            (qual-str
             (and qualifiers
                  (format nil "~{~A~^ ~}"
                          (mapcar #'%signature-text (nreverse qualifiers))))))
        (remove nil
                (list name-str
                      (and qual-str (format nil "~A ~A" name-str qual-str))
                      (and lambda-str (format nil "~A ~A" name-str lambda-str))
                      (and (and qual-str lambda-str)
                           (format nil "~A ~A ~A" name-str qual-str
                                   lambda-str))))))))
```

`lisp-edit-form` `replace`、`form_type` `defun`、`form_name` `%find-target`:

```lisp
(defun %find-target (nodes form-type form-name)
  "Find a target node matching FORM-TYPE and FORM-NAME.
If FORM-NAME ends with [N] (e.g., 'resize[1]'), select the Nth match (0-indexed).
If multiple matches exist without an index, signals an error with candidate info.
FORM-NAME is compared after %NORMALIZE-FORM-NAME-TEXT, so package prefixes and
line breaks in it do not matter."
  (multiple-value-bind (base-name index)
      (let ((match (nth-value 1 (scan-to-strings "^(.+?)\\[(\\d+)\\]$" form-name))))
        (if match
            (values (aref match 0) (parse-integer (aref match 1)))
            (values form-name nil)))
    (let ((target (%normalize-form-name-text
                   (%strip-hash-colon
                    (string-downcase (%strip-name-prefix base-name)))))
          (matches nil))
      (when (zerop (length target))
        (error "form_name resolved to empty string after prefix stripping; ~
provide a non-empty name (e.g. \"my-pkg\" instead of \"#:\" alone)"))
      (loop for node in nodes
            when (and (typep node 'cst-node)
                      (eq (cst-node-kind node) :expr))
              do (let ((value (cst-node-value node)))
                   (when (and (consp value)
                              (string= (string-downcase (symbol-name (car value))) form-type)
                              (some (lambda (cand) (string= cand target))
                                    (%definition-candidates value form-type)))
                     (push (cons node value) matches))))
      (setf matches (nreverse matches))
      ;; A method's candidates include its lambda list without its qualifiers,
      ;; so "area ((s circle))" names both the primary method and the :around
      ;; one.  When no index was given, a form whose full signature is exactly
      ;; FORM-NAME wins over forms it only abbreviates.
      (unless index
        (let ((exact (remove-if-not
                      (lambda (match)
                        (string= target
                                 (car (last (%definition-candidates (cdr match) form-type)))))
                      matches)))
          (when exact
            (setf matches exact))))
      (cond
        ((null matches)
         nil)
        ((and index (< index (length matches)))
         (car (nth index matches)))
        (index
         (error "Index [~D] out of range, only ~D match~:P found for ~A"
                index (length matches) form-name))
        ((= (length matches) 1)
         (car (first matches)))
        (t
         ;; Multiple matches without index - provide helpful error
         (let ((descriptions
                 (loop for (node . form) in matches
                       for i from 0
                       collect (format nil "[~D] ~A"
                                       i
                                       (let ((candidates (%definition-candidates form form-type)))
                                         (or (car (last candidates)) (first candidates)))))))
           (error "Multiple matches for ~A ~A. Specify an index:~%~{  ~A~%~}"
                  form-type form-name descriptions)))))))
```

`lisp-check-parens` で `src/lisp-edit-form-core.lisp` を確認する。

- [ ] **Step 5: テストが通ることを確認する**

`<name>` を `lisp-edit-form-test`、`code-refs-scan-test`、`lisp-patch-form-test`、`code-test` にして順に実行。
Expected: すべて `✗` なし（`code-test` の既存 form_name `shape-area ((shape integer))` も変わらない）。

- [ ] **Step 6: Lint とコミット**

```bash
mallet src/lisp-edit-form-core.lisp tests/lisp-edit-form-test.lisp tests/code-refs-scan-test.lisp
git add src/lisp-edit-form-core.lisp tests/lisp-edit-form-test.lisp tests/code-refs-scan-test.lisp
git commit -m "fix(lisp-edit-form): match a defmethod's form_name outside CL-USER

The candidates printed a method's lambda list relative to the parent's
*package*, so a method read in its own package only matched a
package-qualified name, and a long lambda list gained a line break.  Print
qualifiers and lambda lists with bare names on one line, strip prefixes
and line breaks from the caller's form_name, and let a form whose whole
signature equals form_name win over a qualified method it abbreviates.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FwJGaMhQXB93nGDWsR7eSS"
```

---
### Task 2: クラス・メソッドの行を解決する（`code-core`）

**Files:**
- Create: `tests/fixtures/clos-fixture.lisp`
- Modify: `src/code-core.lisp`（export 追加、`%offset->line` の直後に新関数、`code-find-definition` の行計算）
- Test: `tests/code-test.lisp`（import、フィクスチャ補助、テスト 5 本）

**Interfaces:**
- Consumes: 既存の `%offset->line`、`%ensure-sb-introspect`、`%truename-string`、`%source-stale-p`、`normalize-path-for-display`
- Produces（すべて `cl-mcp/src/code-core` から export）:
  - `definition-source-line (source) → (or null integer)`
  - `definition-source-location (source) → (values abs-path path line stale)`（abs-path は絶対パスの truename 文字列か NIL、stale は T/NIL）
  - `with-definition-source-cache (&body body)`（マクロ）
  - `%sbcl-function (package name) → (or null function)`
  - `%read-form-starts (pathname) → (or null vector)`
  - `code-find-definition` の LINE がクラス・condition・struct でも入る

- [ ] **Step 1: フィクスチャを作る**

`fs-write-file` で `tests/fixtures/clos-fixture.lisp`:

```lisp
;;;; tests/fixtures/clos-fixture.lisp
;;;;
;;;; Compiled and loaded by the clos-describe and code-find tests so that they
;;;; see real CLOS definitions with source locations.
;;;; 日本語のコメント: SBCL の位置情報はバイト単位なので、この行の後ろでも
;;;; 行番号がずれないことを確かめる。
;;;; The tests find each definition by the text of its first line, so keep those
;;;; lines unique.  Never MAKE-INSTANCE a class defined here: the tests need
;;;; CIRCLE and SQUARE unfinalized.  (Loading this file finalizes SHAPE: PCL
;;;; finalizes the class whose slot a method's SLOT-VALUE names.)

(defpackage #:cl-mcp-clos-fixture
  (:use #:cl)
  (:export #:shape #:circle #:square #:area #:label #:radius #:side
           #:shape-name #:combine #:describe-shape #:probe-error
           #:probe-error-code #:point #:pending))

(in-package #:cl-mcp-clos-fixture)

(defclass shape ()
  ((name :initarg :name :reader shape-name :initform "anon"
         :documentation "A label for the shape.")
   (registry :allocation :class :initform (make-hash-table)))
  (:documentation "The base of every shape."))

(defclass circle (shape)
  ((radius :initarg :radius :accessor radius :type real :initform (random 10))))

(defclass square (shape)
  ((side :initarg :side :accessor side))
  (:default-initargs :name "square"))

(defgeneric area (shape)
  (:documentation "Return the area of SHAPE.")
  (:method ((shape (eql :unit)))
    1))

(defmethod area ((shape circle))
  (* pi (radius shape) (radius shape)))

(defmethod area ((shape square))
  (* (side shape) (side shape)))

(defmethod area :around ((shape circle))
  (call-next-method))

(defgeneric (setf label) (value shape))

(defmethod (setf label) (value (shape shape))
  (setf (slot-value shape 'name) value))

(defgeneric combine (a b)
  (:method-combination +))

(defmethod combine + ((a integer) b)
  a)

(defmethod combine + ((a number) (b number))
  b)

(defmethod describe-shape ((shape shape) &optional (stream *standard-output*) verbose)
  (format stream "~A~@[ (verbose)~]" (shape-name shape) verbose))

(defmethod print-object ((shape square) stream)
  (print-unreadable-object (shape stream :type t :identity t)))

(define-condition probe-error (error)
  ((code :initarg :code :reader probe-error-code)))

(defstruct point x (y 0))

(defclass pending (not-yet-defined) ())
```

- [ ] **Step 2: 失敗するテストを書く**

`tests/code-test.lisp` の defpackage の `(:import-from #:cl-mcp/src/code-core #:code-find-references-report)` を次に置き換える（`lisp-patch-form`、form_type `defpackage`、form_name `cl-mcp/tests/code-test`）:

```lisp
  (:import-from #:cl-mcp/src/code-core
                #:code-find-references-report
                #:definition-source-line
                #:definition-source-location
                #:%read-form-starts
                #:%offset->line)
```

ファイル末尾に追加:

```lisp
(defparameter *clos-fixture*
  (asdf/system:system-relative-pathname :cl-mcp "tests/fixtures/clos-fixture.lisp")
  "CLOS definitions compiled so that SBCL records their source locations.")

(defun %compile-and-load-under-own-name (file)
  "Compile and load FILE with its truename as the source namestring.
repl-eval wraps evaluation in a compilation unit that names every file it
compiles \"repl-eval\"; overriding the unit keeps FILE's own path when the
tests run from there."
  (let ((truename (truename file)))
    (uiop:with-temporary-file (:pathname fasl :type "fasl")
      (with-compilation-unit (:override t :source-namestring (namestring truename))
        (handler-bind ((warning #'muffle-warning))
          (load (compile-file truename :output-file fasl :verbose nil :print nil)))))))

(defun %find-definition-sources (package-name name kind)
  "Return SB-INTROSPECT's definition sources of kind KIND for PACKAGE-NAME::NAME."
  (uiop:symbol-call :sb-introspect :find-definition-sources-by-name
                    (find-symbol name package-name) kind))

(deftest code-find-definition-returns-lines-of-classes
  (testing "defclass, define-condition and defstruct get the line of their form"
    (%compile-and-load-under-own-name *clos-fixture*)
    (dolist (case '(("cl-mcp-clos-fixture:circle" "(defclass circle")
                    ("cl-mcp-clos-fixture:probe-error" "(define-condition probe-error")
                    ("cl-mcp-clos-fixture:point" "(defstruct point")))
      (destructuring-bind (designator needle) case
        (multiple-value-bind (path line) (code-find-definition designator)
          (ok (search "tests/fixtures/clos-fixture.lisp" path) designator)
          (ok (eql (%fixture-line needle *clos-fixture*) line) designator))))))

(deftest definition-source-line-resolves-methods-and-accessors
  (testing "a method and a slot accessor resolve to their own top-level form"
    (%compile-and-load-under-own-name *clos-fixture*)
    (flet ((method-lines (name)
             (sort (mapcar (lambda (method)
                             (definition-source-line
                              (uiop:symbol-call :sb-introspect :find-definition-source method)))
                           (sb-mop:generic-function-methods (fdefinition name)))
                   #'<)))
      (ok (equal (sort (list (%fixture-line "(defgeneric area" *clos-fixture*)
                             (%fixture-line "(defmethod area ((shape circle" *clos-fixture*)
                             (%fixture-line "(defmethod area ((shape square" *clos-fixture*)
                             (%fixture-line "(defmethod area :around" *clos-fixture*))
                       #'<)
                 (method-lines (find-symbol "AREA" "CL-MCP-CLOS-FIXTURE"))))
      (ok (equal (list (%fixture-line "(defclass circle" *clos-fixture*))
                 (method-lines (find-symbol "RADIUS" "CL-MCP-CLOS-FIXTURE")))))))

(deftest definition-source-location-reports-path-line-and-staleness
  (testing "absolute truename, display path and line for a file definition"
    (%compile-and-load-under-own-name *clos-fixture*)
    (multiple-value-bind (abs-path path line stale)
        (definition-source-location
         (first (%find-definition-sources "CL-MCP-CLOS-FIXTURE" "SQUARE" :class)))
      (ok (equal (namestring (truename *clos-fixture*)) abs-path))
      (ok (search "tests/fixtures/clos-fixture.lisp" path))
      (ok (eql (%fixture-line "(defclass square" *clos-fixture*) line))
      (ok (null stale))))
  (testing "no source means no location"
    (ok (equal '(nil nil nil nil) (multiple-value-list (definition-source-location nil)))))
  (testing "a file written after it was compiled is stale"
    (let ((file (asdf/system:system-relative-pathname
                 :cl-mcp "tests/tmp/clos-stale-fixture.lisp")))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede)
        (format out "(defpackage #:cl-mcp-clos-stale-fixture (:use #:cl))~%~
(in-package #:cl-mcp-clos-stale-fixture)~%~
(defclass stale-probe () ())~%~
(defun stale-probe-function () 1)~%"))
      (unwind-protect
           (progn
             (%compile-and-load-under-own-name file)
             ;; utimes takes Unix time; FILE-WRITE-DATE is universal time.
             (let ((later (+ (- (file-write-date file) 2208988800) 100)))
               (uiop:symbol-call :sb-posix :utimes (namestring (truename file)) later later))
             (ok (nth-value 3 (definition-source-location
                               (first (%find-definition-sources "CL-MCP-CLOS-STALE-FIXTURE"
                                                                "STALE-PROBE" :class))))))
        (ignore-errors (delete-file file))))))

(deftest read-form-starts-counts-forms-as-the-compiler-does
  (testing "a form a reader conditional excludes leaves no position"
    (let ((file (asdf/system:system-relative-pathname :cl-mcp "tests/tmp/read-form-starts.lisp")))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
        (format out ";;; 日本語のコメント~%(defun one () 1)~%#+(or) (defun never () 0)~%~
#-sbcl (defun not-sbcl () 0)~%#+sbcl~%(defun two () 2)~%#| block~%comment |#~%~
(defparameter *three* #.(+ 1 2))~%#+(or) #+sbcl (defun stacked () 0)~%~
(defun four () (list #\\) \"str)ing\" '|a b|))~%"))
      (unwind-protect
           (let ((starts (%read-form-starts file)))
             (ok (= 4 (length starts)))
             (ok (equal '(2 6 9 11)
                        (map 'list (lambda (start) (%offset->line file start)) starts))))
        (ignore-errors (delete-file file))))))

(deftest definition-source-line-survives-collected-code
  (testing "a file holding only a class still yields a line after a full GC"
    (let ((file (asdf/system:system-relative-pathname :cl-mcp "tests/tmp/clos-classes-only.lisp")))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede)
        (format out "(defpackage #:cl-mcp-clos-classes-only (:use #:cl))~%~
(in-package #:cl-mcp-clos-classes-only)~%~%~
(defclass only-probe ()~%  ((a :initarg :a :accessor only-a)))~%"))
      (unwind-protect
           (progn
             (%compile-and-load-under-own-name file)
             ;; Once collected, the file's debug source and its recorded form
             ;; positions are gone, and the line has to come from reading it.
             (uiop:symbol-call :sb-ext :gc :full t)
             (ok (eql 4 (definition-source-line
                         (first (%find-definition-sources "CL-MCP-CLOS-CLASSES-ONLY"
                                                          "ONLY-PROBE" :class))))))
        (ignore-errors (delete-file file))))))
```

- [ ] **Step 3: テストが失敗することを確認する**

`<name>` を `code-test` にして実行。
Expected: ロード時に `DEFINITION-SOURCE-LINE` などが `cl-mcp/src/code-core` に無いというパッケージエラー（未実装）。

- [ ] **Step 4: export を足す**

`lisp-patch-form`、form_type `defpackage`、form_name `cl-mcp/src/code-core`:
- old_text: `           #:%ensure-sb-introspect))`
- new_text:

```lisp
           #:%ensure-sb-introspect
           #:%sbcl-function
           #:%read-form-starts
           #:definition-source-line
           #:definition-source-location
           #:with-definition-source-cache))
```

- [ ] **Step 5: 行解決の関数を足す**

`lisp-edit-form` `insert_after`、form_type `defun`、form_name `%offset->line` で、次のフォームを 1 つずつ順に追加する（1 回に 1 フォーム。2 つ目以降は直前に足したフォームの後ろに `insert_after`。`defvar` の form_type は `defvar`、`defmacro` は `defmacro`）:

```lisp
(defvar *debug-sources* nil
  "Source namestring -> newest debug source, or :UNBUILT, inside
WITH-DEFINITION-SOURCE-CACHE.  NIL outside it, where every lookup walks the
heap afresh.")
```

```lisp
(defvar *read-form-starts* nil
  "Source namestring -> %READ-FORM-STARTS' vector (or :NONE) inside
WITH-DEFINITION-SOURCE-CACHE, so each file is read at most once.  NIL outside
it.")
```

```lisp
(defmacro with-definition-source-cache (&body body)
  "Run BODY so that DEFINITION-SOURCE-LINE walks the heap for debug sources,
and reads each source file for its form positions, at most once however many
definitions BODY resolves.  Nothing is kept past BODY: a reload in between
would leave the tables describing files as they were."
  `(let ((*debug-sources* (or *debug-sources* :unbuilt))
         (*read-form-starts* (or *read-form-starts* (make-hash-table :test #'equal))))
     ,@body))
```

```lisp
(defun %sbcl-function (package name)
  "Return the function NAME in SBCL's PACKAGE, or NIL when this SBCL lacks it."
  (let ((symbol (and (find-package package) (find-symbol name package))))
    (and symbol (fboundp symbol) (fdefinition symbol))))
```

```lisp
(defun %debug-sources-by-namestring ()
  "Return a table from source namestring to the newest debug source recording
the start positions of that file's top-level forms.

It walks every code object in the heap (about 60ms for 28,000 objects).  Of
several debug sources for one file, the one with the latest
DEBUG-SOURCE-CREATED wins (NIL counts as 0), and of those created in the same
second, the one recording the most forms.  Loading a file more than once leaves
one set per load; compiling a file that starts with DEFPACKAGE also leaves a
second debug source, created in the same second, that records only the forms
read before the package existed."
  (let ((table (make-hash-table :test #'equal))
        (list-objects (%sbcl-function "SB-VM" "LIST-ALLOCATED-OBJECTS"))
        (code-widetag (let ((symbol (find-symbol "CODE-HEADER-WIDETAG" "SB-VM")))
                        (and symbol (boundp symbol) (symbol-value symbol))))
        (debug-info-fn (%sbcl-function "SB-KERNEL" "%CODE-DEBUG-INFO"))
        (info-type (find-symbol "COMPILED-DEBUG-INFO" "SB-C"))
        (info-source-fn (%sbcl-function "SB-C" "COMPILED-DEBUG-INFO-SOURCE"))
        (source-type (find-symbol "DEBUG-SOURCE" "SB-C"))
        (namestring-fn (%sbcl-function "SB-C" "DEBUG-SOURCE-NAMESTRING"))
        (positions-fn (%sbcl-function "SB-C" "DEBUG-SOURCE-START-POSITIONS"))
        (created-fn (%sbcl-function "SB-C" "DEBUG-SOURCE-CREATED")))
    (when (and list-objects code-widetag debug-info-fn info-type info-source-fn
               source-type namestring-fn positions-fn created-fn)
      (dolist (code (funcall list-objects :all :type code-widetag))
        (let ((info (funcall debug-info-fn code)))
          (when (typep info info-type)
            (let ((source (funcall info-source-fn info)))
              (when (and (typep source source-type)
                         (stringp (funcall namestring-fn source))
                         (funcall positions-fn source))
                (let* ((name (funcall namestring-fn source))
                       (old (gethash name table)))
                  (when (or (null old)
                            (let ((created (or (funcall created-fn source) 0))
                                  (old-created (or (funcall created-fn old) 0)))
                              (or (> created old-created)
                                  (and (= created old-created)
                                       (> (length (funcall positions-fn source))
                                          (length (funcall positions-fn old)))))))
                    (setf (gethash name table) source))))))))
      table)))
```

```lisp
(defun %debug-source-for (pathname)
  "Return the newest debug source compiled from PATHNAME, or NIL."
  (let ((table (cond
                 ((hash-table-p *debug-sources*) *debug-sources*)
                 ((eq *debug-sources* :unbuilt)
                  (setf *debug-sources* (%debug-sources-by-namestring)))
                 (t (%debug-sources-by-namestring)))))
    (and table pathname (gethash (namestring pathname) table))))
```

```lisp
(defun %read-form-starts (pathname)
  "Return a vector of the file positions at which PATHNAME's top-level forms
start, numbered as COMPILE-FILE numbers them, or NIL when the file cannot be
read that way.

The file is read with the standard readtable, *READ-SUPPRESS* true and
READ-PRESERVING-WHITESPACE, so nothing is evaluated or interned (a feature
expression's keywords aside) and a form a reader conditional excludes counts
for nothing -- which is how the compiler counts.  On cl-mcp's own sources the
positions equal the ones the compiler records.  A file using a custom reader
macro may fail to read, giving NIL."
  (handler-case
      (with-open-file (in (translate-logical-pathname pathname)
                          :external-format '(:utf-8 :replacement #\?))
        (let ((*read-suppress* t)
              (*read-eval* nil)
              (*package* (find-package "COMMON-LISP-USER"))
              (*readtable* (copy-readtable nil))
              (eof (list :eof))
              (starts '()))
          (loop
            (let ((position (file-position in)))
              (when (eq (read-preserving-whitespace in nil eof) eof)
                (return (coerce (nreverse starts) 'vector)))
              (push position starts)))))
    (error () nil)))
```

```lisp
(defun %cached-read-form-starts (pathname)
  "Return %READ-FORM-STARTS for PATHNAME, reading the file at most once inside
WITH-DEFINITION-SOURCE-CACHE."
  (let ((key (namestring pathname)))
    (if (hash-table-p *read-form-starts*)
        (let ((cached (gethash key *read-form-starts*)))
          (cond
            ((eq cached :none) nil)
            (cached cached)
            (t (let ((starts (%read-form-starts pathname)))
                 (setf (gethash key *read-form-starts*) (or starts :none))
                 starts))))
        (%read-form-starts pathname))))
```

```lisp
(defun %form-start-offset (pathname form-number)
  "Return the file position where top-level form FORM-NUMBER of PATHNAME
starts, or NIL.

The positions PATHNAME's newest debug source recorded are used when they reach
FORM-NUMBER; they also cover files that use custom reader syntax.  Otherwise
the file is read (%CACHED-READ-FORM-STARTS): the debug source is gone once the
garbage collector has freed every function compiled from the file -- a file
holding only DEFCLASS forms keeps no code after it is loaded -- or the one
left may record only the forms read before a DEFPACKAGE took effect."
  (flet ((position-in (positions)
           (and (vectorp positions)
                (integerp form-number)
                (< -1 form-number (length positions))
                (aref positions form-number))))
    (let ((source (%debug-source-for pathname)))
      (or (and source
               (position-in
                (funcall (%sbcl-function "SB-C" "DEBUG-SOURCE-START-POSITIONS") source)))
          (position-in (%cached-read-form-starts pathname))))))
```

```lisp
(defun definition-source-line (source)
  "Return the 1-based line an SB-INTROSPECT definition SOURCE starts on, or NIL.

A character offset, recorded for functions, is used when present.  Classes,
conditions, structures and methods carry only a form path, whose first element
numbers the top-level form; that form's start comes from %FORM-START-OFFSET.
Both are octet positions that %OFFSET->LINE converts."
  (let* ((pkg (%ensure-sb-introspect))
         (path-fn (and pkg (find-symbol "DEFINITION-SOURCE-PATHNAME" pkg)))
         (offset-fn (and pkg (find-symbol "DEFINITION-SOURCE-CHARACTER-OFFSET" pkg)))
         (form-path-fn (and pkg (find-symbol "DEFINITION-SOURCE-FORM-PATH" pkg)))
         (pathname (and source path-fn (ignore-errors (funcall path-fn source))))
         (offset (and source offset-fn (ignore-errors (funcall offset-fn source))))
         (form-path (and source form-path-fn (ignore-errors (funcall form-path-fn source)))))
    (when pathname
      (let ((position (or offset
                          (and (consp form-path)
                               (ignore-errors
                                (%form-start-offset pathname (first form-path)))))))
        (and position (%offset->line pathname position))))))
```

```lisp
(defun %debug-source-created (pathname)
  "Return the source write date recorded in PATHNAME's newest debug source, or NIL."
  (let ((source (%debug-source-for pathname)))
    (and source
         (funcall (%sbcl-function "SB-C" "DEBUG-SOURCE-CREATED") source))))
```

```lisp
(defun definition-source-location (source)
  "Return (values ABS-PATH PATH LINE STALE) for an SB-INTROSPECT definition SOURCE.

ABS-PATH is the source file's truename namestring, or NIL when SOURCE has no
file or names none that is absolute (a definition typed into repl-eval records
the path \"repl-eval\").  PATH is the display path (NORMALIZE-PATH-FOR-DISPLAY)
and LINE comes from DEFINITION-SOURCE-LINE; either may be NIL.  STALE is true
when the file was written after the date recorded for SOURCE -- its own
FILE-WRITE-DATE, or, for definitions that keep none, the date in the file's
debug source.  Without either date STALE is false."
  (let* ((pkg (%ensure-sb-introspect))
         (path-fn (and pkg (find-symbol "DEFINITION-SOURCE-PATHNAME" pkg)))
         (write-date-fn (and pkg (find-symbol "DEFINITION-SOURCE-FILE-WRITE-DATE" pkg)))
         (pathname (and source path-fn (ignore-errors (funcall path-fn source)))))
    (if (null pathname)
        (values nil nil nil nil)
        (let ((truename (%truename-string pathname))
              (recorded (or (and write-date-fn (ignore-errors (funcall write-date-fn source)))
                            (ignore-errors (%debug-source-created pathname)))))
          (values (and truename (uiop:absolute-pathname-p truename) truename)
                  (normalize-path-for-display pathname)
                  (definition-source-line source)
                  (and (%source-stale-p pathname recorded) t))))))
```

- [ ] **Step 6: `code-find-definition` の行計算を置き換える**

`lisp-patch-form`、form_type `defun`、form_name `code-find-definition` で 2 回:

1. old_text（改行を含む 1 行）:
```
           (offset (and pkg (find-symbol "DEFINITION-SOURCE-CHARACTER-OFFSET" pkg)))
```
   new_text: 空文字列（行ごと削除。`lisp-patch-form` が空の new_text を受け付けなければ、直前の `(path-fn ...)` 行と合わせて old_text にし、`(path-fn ...)` 行だけを new_text にする）

2. old_text:
```
               (char-offset (and offset (funcall offset source)))
               (line (%offset->line pathname char-offset))
```
   new_text:
```
               (line (definition-source-line source))
```

`lisp-check-parens` で `src/code-core.lisp` を確認する。

- [ ] **Step 7: テストが通ることを確認する**

`<name>` を `code-test` にして実行。
Expected: 新しい 5 本を含めて `✗` なし。

- [ ] **Step 8: Lint とコミット**

```bash
mallet src/code-core.lisp tests/code-test.lisp tests/fixtures/clos-fixture.lisp
git add src/code-core.lisp tests/code-test.lisp tests/fixtures/clos-fixture.lisp
git commit -m "fix(code-core): resolve the lines of classes and methods

SBCL records a class, condition, structure or method by top-level form
number, not character offset, so code-find returned no line for them.
Map the form number to the file position the compiler recorded for it, or,
once that record has been collected with the file's code, to the position
found by reading the file with *read-suppress*, which counts forms the way
the compiler does.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FwJGaMhQXB93nGDWsR7eSS"
```

---
### Task 3: 行で始まるフォームを引く（`code-refs-scan`）

**Files:**
- Modify: `src/code-refs-scan.lisp`（export、`scan-project` の直後に `top-level-forms-at`）
- Test: `tests/code-refs-scan-test.lisp`（import、補助関数、テスト 3 本）

**Interfaces:**
- Consumes: 既存の `%readable-path`、`fs-read-source-text`、`parse-top-level-forms`、`%unwrap`、`%form-metadata`、`%in-package-form-p`、`%first-line`
- Produces: `cl-mcp/src/code-refs-scan:top-level-forms-at (abs-path lines) → (values table failure)`
  - `table`: `eql` hash-table、行 → `(form-type . form-name)`
  - `failure`: NIL / `:denied` / 1 行の文字列

- [ ] **Step 1: 失敗するテストを書く**

`tests/code-refs-scan-test.lisp` の defpackage の `(:import-from #:cl-mcp/src/code-refs-scan ...)` に `#:top-level-forms-at` を足す（`lisp-patch-form`、old_text `                #:scan-project))`、new_text `                #:scan-project\n                #:top-level-forms-at))`）。

ファイル末尾に追加:

```lisp
(defun %write-tmp (name text)
  "Write TEXT to tests/tmp/NAME in the cl-mcp source tree; return its truename namestring."
  (let ((file (asdf/system:system-relative-pathname :cl-mcp (format nil "tests/tmp/~A" name))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text out))
    (namestring (truename file))))

(deftest top-level-forms-at-describes-forms-starting-on-lines
  (testing "form_type and form_name of the forms starting on the given lines"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (path (%write-tmp "top-level-forms-at.lisp"
                            (format nil "(in-package #:cl-user)~%~%(defclass widget ()~%  ())~%~%~
(defmethod paint ((w widget) stream)~%  (list w stream))~%~%#+sbcl~%(defun gated () 1)~%"))))
      (unwind-protect
           (multiple-value-bind (table failure) (top-level-forms-at path '(3 6 9 10 4))
             (ok (null failure))
             (ok (equal '("defclass" . "widget") (gethash 3 table)))
             (ok (equal '("defmethod" . "paint ((w widget) stream)") (gethash 6 table)))
             (ok (equal '("defun" . "gated") (gethash 9 table)) "the #+sbcl line")
             (ok (equal '("defun" . "gated") (gethash 10 table)) "the wrapped form's line")
             (ok (null (gethash 4 table)) "a line inside a form"))
        (ignore-errors (delete-file path))))))

(deftest top-level-forms-at-reports-why-it-found-nothing
  (testing "a file that does not parse gives its reader error"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (path (%write-tmp "top-level-forms-at-bad.lisp"
                            (format nil "(in-package #:cl-user)~%~%(defparameter *x* #.(+ 1 2))~%"))))
      (unwind-protect
           (multiple-value-bind (table failure) (top-level-forms-at path '(3))
             (ok (zerop (hash-table-count table)))
             (ok (and (stringp failure) (search "*READ-EVAL*" failure)) failure))
        (ignore-errors (delete-file path)))))
  (testing "a file outside the readable paths is not opened"
    (let* ((*project-root* (asdf:system-source-directory :cl-mcp))
           (outside (merge-pathnames "cl-mcp-top-level-forms-at-outside.lisp"
                                     (uiop:temporary-directory))))
      (with-open-file (out outside :direction :output :if-exists :supersede)
        (write-string "(defun outside () 1)" out))
      (unwind-protect
           (ok (eq :denied (nth-value 1 (top-level-forms-at (namestring outside) '(1)))))
        (ignore-errors (delete-file outside))))))

(deftest top-level-forms-at-without-lines-reads-nothing
  (testing "no lines asked for is no failure and an empty table"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp))
          (path (%write-tmp "top-level-forms-at-empty.lisp" "(defun one () 1)")))
      (unwind-protect
           (multiple-value-bind (table failure) (top-level-forms-at path '())
             (ok (zerop (hash-table-count table)))
             (ok (null failure)))
        (ignore-errors (delete-file path))))))
```

- [ ] **Step 2: テストが失敗することを確認する**

`<name>` を `code-refs-scan-test` にして実行。
Expected: `TOP-LEVEL-FORMS-AT` が export されていないというパッケージエラー。

- [ ] **Step 3: 実装する**

`lisp-patch-form`、form_type `defpackage`、form_name `cl-mcp/src/code-refs-scan`:
- old_text: `           #:scan-project))`
- new_text: `           #:scan-project\n           #:top-level-forms-at))`（`\n` は実際の改行）

`lisp-edit-form` `insert_after`、form_type `defun`、form_name `scan-project`:

```lisp
(defun top-level-forms-at (abs-path lines)
  "Describe the top-level forms of the file at ABS-PATH that start on LINES.

Returns (values TABLE FAILURE).  TABLE maps each line in LINES on which a
top-level form starts to (FORM-TYPE . FORM-NAME), as %FORM-METADATA gives them
to code-find-references, with the package from the file's IN-PACKAGE forms; a
line no form starts on is absent.  A form wrapped in #+feature or #-feature is
found both on its own line and on the line of the form it wraps (%UNWRAP).

FAILURE is NIL when the file was read and parsed.  It is :DENIED when the read
policy refuses ABS-PATH (%READABLE-PATH) -- the file is then not opened -- and
a one-line string when the file cannot be read or does not parse; TABLE is
empty in both cases."
  (let ((table (make-hash-table))
        (wanted (remove-duplicates (remove-if-not #'integerp lines)))
        (readable (and abs-path (%readable-path abs-path))))
    (cond
      ((null readable) (values table :denied))
      ((null wanted) (values table nil))
      (t
       (multiple-value-bind (text read-condition)
           (ignore-errors (fs-read-source-text readable))
         (if (null text)
             (values table (%first-line (princ-to-string read-condition)))
             (handler-case
                 (let ((in-package nil))
                   (dolist (node (parse-top-level-forms text :source-path (pathname abs-path)))
                     (when (eq (cst-node-kind node) :expr)
                       (let ((value (cst-node-value node)))
                         (dolist (line (list (cst-node-start-line node)
                                             (cst-node-start-line (%unwrap node))))
                           (when (and (member line wanted) (not (gethash line table)))
                             (multiple-value-bind (form-type form-name)
                                 (%form-metadata value in-package)
                               (setf (gethash line table) (cons form-type form-name)))))
                         (let ((designator (%in-package-form-p value)))
                           (when designator
                             (setf in-package designator))))))
                   (values table nil))
               (error (e)
                 (values table (%first-line (princ-to-string e)))))))))))
```

- [ ] **Step 4: テストが通ることを確認する**

`<name>` を `code-refs-scan-test` にして実行。
Expected: `✗` なし。

- [ ] **Step 5: Lint とコミット**

```bash
mallet src/code-refs-scan.lisp tests/code-refs-scan-test.lisp
git add src/code-refs-scan.lisp tests/code-refs-scan-test.lisp
git commit -m "feat(code-refs-scan): describe the top-level forms starting on given lines

clos-describe learns a definition's file and line from the worker and needs
the form_type and form_name lisp-edit-form takes for it.  Read the file once
under the read policy and describe the forms starting on those lines the
way code-find-references does.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FwJGaMhQXB93nGDWsR7eSS"
```

---
### Task 4: `clos-core` — 総称関数とメソッド

**Files:**
- Modify: `src/code-refs-core.lisp`（`%status-string` を export）
- Create: `src/clos-core.lisp`
- Create: `tests/clos-core-test.lisp`
- Modify: `tests.lisp`（登録）

**Interfaces:**
- Consumes: Task 2 の `definition-source-location`、`with-definition-source-cache`、`%sbcl-function`、既存の `%ensure-sb-introspect`、`resolve-target`、`qualified-symbol-name`、`symbol-kind`、`%status-string`、`json-bool`、`make-ht`、`path-inside-p`
- Produces: `cl-mcp/src/clos-core:clos-describe-report (symbol-name &key package (limit 50)) → hash-table`
  - キー: `symbol` `symbol_status` `resolved_symbol` `symbol_kind` `lookup_package` `lookup_name` `generic_functions`（vector）`class`（この Task では常に NIL）`limit` `notes`（vector）
  - 総称関数: `abs_path` `path` `line` `stale` `form_type`(nil) `form_name`(nil) `note`(nil) `name` `lambda_list` `documentation` `method_combination` `method_count` `truncated` `methods`
  - メソッド: 位置の 7 キー + `generic_function` `qualifiers` `specializers` `kind` `slot`、クラス経由なら `via`
  - `cl-mcp/src/clos-core:*note-not-finalized*`（Task 5 が使う文字列）

- [ ] **Step 1: 失敗するテストを書く**

`fs-write-file` で `tests/clos-core-test.lisp`:

```lisp
;;;; tests/clos-core-test.lisp
;;;;
;;;; Tests for cl-mcp/src/clos-core: the clos-describe report built from the
;;;; CLOS definitions in tests/fixtures/clos-fixture.lisp.

(defpackage #:cl-mcp/tests/clos-core-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/clos-core
                #:clos-describe-report
                #:*note-not-finalized*)
  (:import-from #:cl-mcp/src/code-refs-core
                #:sequence->list)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*))

(in-package #:cl-mcp/tests/clos-core-test)

(defparameter *fixture*
  (asdf/system:system-relative-pathname :cl-mcp "tests/fixtures/clos-fixture.lisp")
  "CLOS definitions compiled so that SBCL records their source locations.")

(defun %load-fixture ()
  "Compile and load the fixture with its truename as the source namestring.
repl-eval's compilation unit would otherwise name the file \"repl-eval\"."
  (let ((truename (truename *fixture*)))
    (uiop:with-temporary-file (:pathname fasl :type "fasl")
      (with-compilation-unit (:override t :source-namestring (namestring truename))
        (handler-bind ((warning #'muffle-warning))
          (load (compile-file truename :output-file fasl :verbose nil :print nil)))))))

(defun %line (needle)
  "Return the 1-based line of the fixture on which NEEDLE starts."
  (let ((text (uiop:read-file-string *fixture*)))
    (1+ (count #\Newline text :end (search needle text)))))

(defun %report (designator &key (limit 50))
  "Load the fixture and return DESIGNATOR's report with the project root bound."
  (%load-fixture)
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (clos-describe-report designator :limit limit)))

(defun %gfs (report)
  "Return REPORT's generic function entries as a list."
  (sequence->list (gethash "generic_functions" report)))

(defun %methods (entry)
  "Return ENTRY's method objects as a list."
  (sequence->list (gethash "methods" entry)))

(defun %summary (method)
  "Return (QUALIFIERS SPECIALIZERS LINE) for METHOD."
  (list (sequence->list (gethash "qualifiers" method))
        (sequence->list (gethash "specializers" method))
        (gethash "line" method)))

(deftest generic-function-report-lists-methods-by-role
  (testing "a generic function's methods: :around first, then primaries in file order"
    (let* ((report (%report "cl-mcp-clos-fixture:area"))
           (gf (first (%gfs report))))
      (ok (equal "found" (gethash "symbol_status" report)))
      (ok (= 1 (length (%gfs report))))
      (ok (null (gethash "class" report)))
      (ok (equal "CL-MCP-CLOS-FIXTURE:AREA" (gethash "name" gf)))
      (ok (equal "(SHAPE)" (gethash "lambda_list" gf)))
      (ok (equal "Return the area of SHAPE." (gethash "documentation" gf)))
      (ok (equal "STANDARD" (gethash "method_combination" gf)))
      (ok (= 4 (gethash "method_count" gf)))
      (ok (eq yason:false (gethash "truncated" gf)))
      (ok (equal (namestring (truename *fixture*)) (gethash "abs_path" gf)))
      (ok (search "tests/fixtures/clos-fixture.lisp" (gethash "path" gf)))
      (ok (eql (%line "(defgeneric area") (gethash "line" gf)))
      (ok (equal (list (list '(":AROUND") '("CL-MCP-CLOS-FIXTURE:CIRCLE")
                             (%line "(defmethod area :around"))
                       (list '() '("(EQL :UNIT)") (%line "(defgeneric area"))
                       (list '() '("CL-MCP-CLOS-FIXTURE:CIRCLE")
                             (%line "(defmethod area ((shape circle"))
                       (list '() '("CL-MCP-CLOS-FIXTURE:SQUARE")
                             (%line "(defmethod area ((shape square")))
                 (mapcar #'%summary (%methods gf))))
      (dolist (method (%methods gf))
        (ok (equal "method" (gethash "kind" method)))
        (ok (equal "CL-MCP-CLOS-FIXTURE:AREA" (gethash "generic_function" method)))
        (ok (nth-value 1 (gethash "form_name" method)) "form_name is left for the parent")))))

(deftest generic-function-report-covers-setf-functions-and-accessors
  (testing "a symbol naming only a SETF generic function"
    (let* ((report (%report "cl-mcp-clos-fixture:label"))
           (gf (first (%gfs report))))
      (ok (equal "unbound" (gethash "symbol_kind" report)))
      (ok (equal "(SETF CL-MCP-CLOS-FIXTURE:LABEL)" (gethash "name" gf)))
      (ok (equal (list (list '() '("COMMON-LISP:T" "CL-MCP-CLOS-FIXTURE:SHAPE")
                             (%line "(defmethod (setf label)")))
                 (mapcar #'%summary (%methods gf))))))
  (testing "an accessor: the reader and its SETF writer, located at the defclass"
    (let* ((report (%report "cl-mcp-clos-fixture:radius"))
           (gfs (%gfs report)))
      (ok (equal '("CL-MCP-CLOS-FIXTURE:RADIUS" "(SETF CL-MCP-CLOS-FIXTURE:RADIUS)")
                 (mapcar (lambda (gf) (gethash "name" gf)) gfs)))
      (ok (null (gethash "path" (first gfs))) "no defgeneric")
      (ok (equal '(("reader" "CL-MCP-CLOS-FIXTURE:RADIUS") ("writer" "CL-MCP-CLOS-FIXTURE:RADIUS"))
                 (mapcar (lambda (gf)
                           (let ((method (first (%methods gf))))
                             (list (gethash "kind" method) (gethash "slot" method))))
                         gfs)))
      (ok (every (lambda (gf)
                   (eql (%line "(defclass circle") (gethash "line" (first (%methods gf)))))
                 gfs)))))

(deftest generic-function-report-shows-other-method-combinations
  (testing "a + combination keeps its qualifier and lists methods in file order"
    (let ((gf (first (%gfs (%report "cl-mcp-clos-fixture:combine")))))
      (ok (equal "+ :MOST-SPECIFIC-FIRST" (gethash "method_combination" gf)))
      (ok (equal (list (list '("+") '("COMMON-LISP:INTEGER" "COMMON-LISP:T")
                             (%line "(defmethod combine + ((a integer)"))
                       (list '("+") '("COMMON-LISP:NUMBER" "COMMON-LISP:NUMBER")
                             (%line "(defmethod combine + ((a number)")))
                 (mapcar #'%summary (%methods gf)))))))

(deftest generic-function-report-honours-limit
  (testing "at most LIMIT methods are listed and the total is kept"
    (let ((gf (first (%gfs (%report "cl:print-object" :limit 2)))))
      (ok (> (gethash "method_count" gf) 2))
      (ok (= 2 (length (%methods gf))))
      (ok (eq t (gethash "truncated" gf))))))

(deftest clos-describe-report-resolves-without-interning
  (testing "a missing symbol is reported and not interned"
    (%load-fixture)
    (ok (null (find-symbol "NO-SUCH-CLOS-NAME" "CL-MCP-CLOS-FIXTURE")))
    (let ((report (%report "cl-mcp-clos-fixture::no-such-clos-name")))
      (ok (equal "not_found" (gethash "symbol_status" report)))
      (ok (null (gethash "resolved_symbol" report))))
    (ok (null (find-symbol "NO-SUCH-CLOS-NAME" "CL-MCP-CLOS-FIXTURE"))))
  (testing "a missing package"
    (ok (equal "package_not_found"
               (gethash "symbol_status" (%report "no-such-clos-package:thing")))))
  (testing "a plain function has no generic function or class"
    (let ((report (%report "cl:car")))
      (ok (equal "function" (gethash "symbol_kind" report)))
      (ok (null (%gfs report)))
      (ok (null (gethash "class" report))))))
```

`tests.lisp` の `(:import-from #:cl-mcp/tests/code-refs-scan-test)` の直後に `(:import-from #:cl-mcp/tests/clos-core-test)` を追加（`lisp-patch-form`、form_type `defpackage`、form_name `cl-mcp/tests`）。

- [ ] **Step 2: テストが失敗することを確認する**

`<name>` を `clos-core-test` にして実行。
Expected: `cl-mcp/src/clos-core` が見つからないロードエラー。

- [ ] **Step 3: `%status-string` を export する**

`lisp-patch-form`、form_type `defpackage`、form_name `cl-mcp/src/code-refs-core`:
- old_text: `           #:build-references-report))`
- new_text: `           #:build-references-report\n           #:%status-string))`（`\n` は実際の改行）

- [ ] **Step 4: `src/clos-core.lisp` を作る**

`fs-write-file` で、まず defpackage と `in-package` と `*note-not-finalized*` だけのファイルを作り、残りのフォームを `lisp-edit-form` の `insert_after` で順に足す（親イメージが古くて解析できなければ Write で全体を書き、`lisp-check-parens` と `mallet`）。完成形:

```lisp
;;;; src/clos-core.lisp
;;;;
;;;; CLOS introspection for clos-describe: a generic function's methods and a
;;;; class's hierarchy, slots and specialized methods, read from the running
;;;; image.  Runs in the worker; loads no eclector.  Reads only: it never
;;;; finalizes a class, calls an initfunction or interns a symbol.

(defpackage #:cl-mcp/src/clos-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/code-core
                #:definition-source-location
                #:with-definition-source-cache
                #:%ensure-sb-introspect
                #:%sbcl-function)
  (:import-from #:cl-mcp/src/code-refs-core
                #:resolve-target
                #:qualified-symbol-name
                #:symbol-kind
                #:%status-string)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:path-inside-p)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:json-bool)
  (:export #:clos-describe-report
           #:*note-not-finalized*))

(in-package #:cl-mcp/src/clos-core)

(defparameter *note-not-finalized*
  "not finalized; precedence list and slots were computed without finalizing the class"
  "Note added when a class's precedence list and slots were computed rather
than read, because the class has not been finalized.")

(defun %introspect (name)
  "Return SB-INTROSPECT's function NAME, or NIL when it is unavailable."
  (let ((pkg (%ensure-sb-introspect)))
    (and pkg (%sbcl-function (package-name pkg) name))))

(defun %home-package (name)
  "Return the package whose reader prints NAME -- a symbol or (SETF symbol) --
without a prefix: its symbol's home package, or COMMON-LISP-USER."
  (let ((symbol (if (consp name) (second name) name)))
    (or (and (symbolp symbol) (symbol-package symbol))
        (find-package "COMMON-LISP-USER"))))

(defun %form-text (form package)
  "Return FORM, unevaluated code from a definition, printed on one line as
PACKAGE's reader would read it, or a placeholder when it cannot be printed."
  (let ((*package* (or package (find-package "COMMON-LISP-USER")))
        (*print-pretty* t)
        (*print-right-margin* most-positive-fixnum)
        (*print-length* 10)
        (*print-level* 4)
        (*print-circle* t)
        (*print-readably* nil))
    (handler-case (prin1-to-string form)
      (error () "#<unprintable>"))))

(defun %datum-text (object)
  "Return OBJECT printed with every symbol fully qualified, keywords as :NAME."
  (%form-text object (find-package "KEYWORD")))

(defun %name-string (name)
  "Return a function or class NAME fully qualified: PKG::NAME or (SETF PKG::NAME)."
  (cond
    ((and (consp name) (eq (first name) 'setf) (symbolp (second name)))
     (format nil "(SETF ~A)" (qualified-symbol-name (second name))))
    ((symbolp name) (qualified-symbol-name name))
    (t (%datum-text name))))

(defun %qualifier-string (qualifier)
  "Return a method QUALIFIER as written: :AROUND for a keyword, the bare name
of any other symbol (+ for the + method combination)."
  (cond
    ((keywordp qualifier) (format nil ":~A" (symbol-name qualifier)))
    ((symbolp qualifier) (symbol-name qualifier))
    (t (%datum-text qualifier))))

(defun %proper-class-name (class)
  "Return CLASS's name when it is a symbol that names CLASS, else NIL."
  (let ((name (ignore-errors (class-name class))))
    (and name (symbolp name) (eq (find-class name nil) class) name)))

(defun %specializer-string (specializer)
  "Return SPECIALIZER as text: a class's qualified name or (EQL object)."
  (cond
    ((typep specializer 'sb-mop:eql-specializer)
     (format nil "(EQL ~A)" (%datum-text (sb-mop:eql-specializer-object specializer))))
    ((and (typep specializer 'class) (%proper-class-name specializer))
     (qualified-symbol-name (%proper-class-name specializer)))
    (t (%datum-text specializer))))

(defun %standard-combination-p (gf)
  "True when GF uses the STANDARD method combination."
  (let ((name-fn (%sbcl-function "SB-PCL" "METHOD-COMBINATION-TYPE-NAME")))
    (and name-fn
         (eq (ignore-errors
              (funcall name-fn (sb-mop:generic-function-method-combination gf)))
             'standard))))

(defun %method-combination-string (gf)
  "Return GF's method combination as its name and options: STANDARD,
+ :MOST-SPECIFIC-FIRST.  NIL when this SBCL does not say."
  (let* ((combination (ignore-errors (sb-mop:generic-function-method-combination gf)))
         (name-fn (%sbcl-function "SB-PCL" "METHOD-COMBINATION-TYPE-NAME"))
         (options-fn (%sbcl-function "SB-PCL" "METHOD-COMBINATION-OPTIONS"))
         (name (and combination name-fn (ignore-errors (funcall name-fn combination))))
         (options (and combination options-fn
                       (ignore-errors (funcall options-fn combination)))))
    (and (symbolp name) name
         (format nil "~A~{ ~A~}" (symbol-name name) (mapcar #'%datum-text options)))))

(defun %role-rank (method)
  "Rank METHOD by its role in the standard method combination:
:AROUND, :BEFORE, primary, :AFTER, anything else."
  (let ((qualifiers (method-qualifiers method)))
    (cond
      ((equal qualifiers '(:around)) 0)
      ((equal qualifiers '(:before)) 1)
      ((null qualifiers) 2)
      ((equal qualifiers '(:after)) 3)
      (t 4))))

(defun %method-source (method)
  "Return METHOD's SB-INTROSPECT definition source, or NIL."
  (let ((find (%introspect "FIND-DEFINITION-SOURCE")))
    (and find (ignore-errors (funcall find method)))))

(defun %source-sort-key (source)
  "Return (PLACE NAMESTRING FORM-NUMBER) ordering SOURCE without reading its
file: PLACE is 0 inside the project root, 1 elsewhere, 2 without a file."
  (let* ((path-fn (%introspect "DEFINITION-SOURCE-PATHNAME"))
         (form-path-fn (%introspect "DEFINITION-SOURCE-FORM-PATH"))
         (pathname (and source path-fn (ignore-errors (funcall path-fn source))))
         (form-path (and source form-path-fn (ignore-errors (funcall form-path-fn source))))
         (absolute (and pathname (ignore-errors (uiop:absolute-pathname-p pathname)))))
    (list (cond ((null absolute) 2)
                ((and *project-root*
                      (ignore-errors (path-inside-p pathname *project-root*)))
                 0)
                (t 1))
          (if pathname (namestring pathname) "")
          (if (and (consp form-path) (integerp (first form-path)))
              (first form-path)
              0))))

(defun %key< (a b)
  "Compare two lists of numbers and strings element by element."
  (loop for x in a
        for y in b
        do (cond ((and (numberp x) (numberp y))
                  (cond ((< x y) (return t)) ((> x y) (return nil))))
                 ((string< x y) (return t))
                 ((string> x y) (return nil)))
        finally (return nil)))

(defun %set-location (ht source)
  "Store SOURCE's location in HT -- abs_path, path, line and stale -- with null
form_type, form_name and note for the parent to fill from the source file."
  (multiple-value-bind (abs-path path line stale)
      (if source (definition-source-location source) (values nil nil nil nil))
    (setf (gethash "abs_path" ht) abs-path
          (gethash "path" ht) path
          (gethash "line" ht) line
          (gethash "stale" ht) (json-bool stale)
          (gethash "form_type" ht) nil
          (gethash "form_name" ht) nil
          (gethash "note" ht) nil)
    ht))

(defun %first-line (condition)
  "Return CONDITION's report, cut at its first line."
  (let ((text (handler-case (princ-to-string condition)
                (error () "unprintable error"))))
    (subseq text 0 (or (position #\Newline text) (length text)))))

(defun %method-entry (method &key via)
  "Return the JSON object for METHOD.  VIA, a class, is recorded when METHOD
was found through that class's specialized methods."
  (let ((ht (%set-location (make-ht) (%method-source method))))
    (setf (gethash "generic_function" ht) nil
          (gethash "qualifiers" ht) (vector)
          (gethash "specializers" ht) (vector)
          (gethash "kind" ht) "method"
          (gethash "slot" ht) nil)
    (when via
      (setf (gethash "via" ht) (%name-string (%proper-class-name via))))
    (handler-case
        (let ((gf (sb-mop:method-generic-function method)))
          (setf (gethash "generic_function" ht)
                (and gf (%name-string (sb-mop:generic-function-name gf)))
                (gethash "qualifiers" ht)
                (map 'vector #'%qualifier-string (method-qualifiers method))
                (gethash "specializers" ht)
                (map 'vector #'%specializer-string (sb-mop:method-specializers method)))
          (when (typep method 'sb-mop:standard-accessor-method)
            (setf (gethash "kind" ht)
                  (if (typep method 'sb-mop:standard-reader-method) "reader" "writer")
                  (gethash "slot" ht)
                  (qualified-symbol-name
                   (sb-mop:slot-definition-name
                    (sb-mop:accessor-method-slot-definition method))))))
      (error (e)
        (setf (gethash "note" ht)
              (format nil "could not read this method: ~A" (%first-line e)))))
    ht))

(defun %sorted-methods (methods standard-p)
  "Return METHODS ordered for display: by role when STANDARD-P, then project
files, other files and no file, then by file and top-level form."
  (mapcar #'car
          (stable-sort (mapcar (lambda (method)
                                 (cons method
                                       (cons (if standard-p (%role-rank method) 0)
                                             (%source-sort-key (%method-source method)))))
                               methods)
                       #'%key< :key #'cdr)))

(defun %generic-function-entry (gf limit)
  "Return the JSON object for GF with at most LIMIT of its methods.  Methods are
sorted before any is located, so only the listed ones read their files."
  (let* ((name (sb-mop:generic-function-name gf))
         (package (%home-package name))
         (methods (sb-mop:generic-function-methods gf))
         (sorted (%sorted-methods methods (%standard-combination-p gf)))
         (by-name (%introspect "FIND-DEFINITION-SOURCES-BY-NAME"))
         (source (and by-name
                      (first (ignore-errors (funcall by-name name :generic-function)))))
         (ht (%set-location (make-ht) source)))
    (setf (gethash "name" ht) (%name-string name)
          (gethash "lambda_list" ht)
          (%form-text (ignore-errors (sb-mop:generic-function-lambda-list gf)) package)
          (gethash "documentation" ht) (ignore-errors (documentation gf t))
          (gethash "method_combination" ht) (%method-combination-string gf)
          (gethash "method_count" ht) (length methods)
          (gethash "truncated" ht) (json-bool (> (length methods) limit))
          (gethash "methods" ht)
          (map 'vector #'%method-entry (subseq sorted 0 (min limit (length sorted)))))
    ht))

(defun clos-describe-report (symbol-name &key package (limit 50))
  "Return the clos-describe payload for SYMBOL-NAME, everything but its content
text and the form_type, form_name and note fields the parent fills in.

SYMBOL-NAME is resolved like code-find-references' symbol (RESOLVE-TARGET):
nothing is interned.  The report holds a generic_functions entry for the
function SYMBOL-NAME names and one for its SETF function, when either is a
generic function.  At most LIMIT methods are listed per entry.  docs/tools.md
describes every field."
  (multiple-value-bind (symbol status lookup-package lookup-name)
      (resolve-target symbol-name :package package)
    (let ((report (make-ht "symbol" symbol-name
                           "symbol_status" (%status-string status)
                           "resolved_symbol" (and symbol (qualified-symbol-name symbol))
                           "symbol_kind" (and (eq status :found) (symbol-kind symbol))
                           "lookup_package" lookup-package
                           "lookup_name" lookup-name
                           "generic_functions" (vector)
                           "class" nil
                           "limit" limit
                           "notes" (vector))))
      (when (eq status :found)
        (with-definition-source-cache
          (setf (gethash "generic_functions" report)
                (coerce (loop for name in (list symbol (list 'setf symbol))
                              for function = (ignore-errors
                                              (and (fboundp name) (fdefinition name)))
                              when (typep function 'generic-function)
                                collect (%generic-function-entry function limit))
                        'vector))))
      report)))
```

- [ ] **Step 5: テストが通ることを確認する**

`<name>` を `clos-core-test` にして実行。
Expected: `✗` なし。

- [ ] **Step 6: Lint とコミット**

```bash
mallet src/clos-core.lisp src/code-refs-core.lisp tests/clos-core-test.lisp
git add src/clos-core.lisp src/code-refs-core.lisp tests/clos-core-test.lisp tests.lisp
git commit -m "feat(clos-core): report a generic function's methods

List each method's qualifiers, specializers, accessor slot and source line,
ordered by role for the standard method combination and by file otherwise,
for the function a symbol names and its SETF function.  Methods are sorted
on their recorded file and form number before any is located, so a generic
function with hundreds of methods reads only the files of those listed.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FwJGaMhQXB93nGDWsR7eSS"
```

---
### Task 5: `clos-core` — クラス

**Files:**
- Modify: `src/clos-core.lisp`（`%generic-function-entry` の後ろにクラスの関数を追加、`clos-describe-report` を置き換え）
- Test: `tests/clos-core-test.lisp`（テスト 6 本）

**Interfaces:**
- Consumes: Task 4 の `%set-location`、`%name-string`、`%form-text`、`%datum-text`、`%proper-class-name`、`%home-package`、`%role-rank`、`%key<`、`%method-entry`、`*note-not-finalized*`
- Produces: report の `class` と `notes`
  - クラス: 位置の 7 キー + `name` `metaclass` `documentation` `finalized` `direct_superclasses` `direct_subclasses` `precedence_list`（CPL が取れなければ NIL）`undefined_superclasses` `direct_slots` `effective_slots`（CPL が取れなければ NIL）`default_initargs`（同）`method_count` `truncated` `methods` `omitted_classes`
  - スロット: `name` `from`（実効のみ）`initargs` `initform` `type` `allocation` `readers` `writers` `documentation`
  - default initarg: `initarg` `form` `from`

- [ ] **Step 1: 失敗するテストを書く**

`tests/clos-core-test.lisp` の末尾に追加:

```lisp
(defun %class (designator)
  "Return DESIGNATOR's class entry."
  (gethash "class" (%report designator)))

(defun %slot-named (class name)
  "Return CLASS's effective slot whose name is NAME."
  (find name (sequence->list (gethash "effective_slots" class))
        :key (lambda (slot) (gethash "name" slot)) :test #'equal))

(defun %strings (entry key)
  "Return ENTRY's KEY as a list of strings."
  (sequence->list (gethash key entry)))

(deftest class-report-leaves-an-unfinalized-class-unfinalized
  (testing "precedence list and slots of a class nobody instantiated"
    (%load-fixture)
    (let ((circle (find-class (find-symbol "CIRCLE" "CL-MCP-CLOS-FIXTURE"))))
      (ok (not (sb-mop:class-finalized-p circle)) "precondition: CIRCLE is unfinalized")
      (let* ((report (%report "cl-mcp-clos-fixture:circle"))
             (class (gethash "class" report))
             (cpl (%strings class "precedence_list")))
        (ok (equal "CL-MCP-CLOS-FIXTURE:CIRCLE" (gethash "name" class)))
        (ok (equal "COMMON-LISP:STANDARD-CLASS" (gethash "metaclass" class)))
        (ok (eq yason:false (gethash "finalized" class)))
        (ok (eql (%line "(defclass circle") (gethash "line" class)))
        (ok (equal '("CL-MCP-CLOS-FIXTURE:SHAPE") (%strings class "direct_superclasses")))
        (ok (equal '("CL-MCP-CLOS-FIXTURE:CIRCLE" "CL-MCP-CLOS-FIXTURE:SHAPE")
                   (subseq cpl 0 2)))
        (ok (equal "COMMON-LISP:T" (car (last cpl))))
        (ok (member *note-not-finalized* (%strings report "notes") :test #'equal)))
      (ok (not (sb-mop:class-finalized-p circle)) "still unfinalized after the report"))))

(deftest class-report-merges-inherited-slots
  (testing "effective slots name the class that defines them"
    (let ((class (%class "cl-mcp-clos-fixture:circle")))
      (ok (equal '("CL-MCP-CLOS-FIXTURE::NAME" "CL-MCP-CLOS-FIXTURE::REGISTRY"
                   "CL-MCP-CLOS-FIXTURE:RADIUS")
                 (mapcar (lambda (slot) (gethash "name" slot))
                         (sequence->list (gethash "effective_slots" class)))))
      (let ((radius (%slot-named class "CL-MCP-CLOS-FIXTURE:RADIUS"))
            (name (%slot-named class "CL-MCP-CLOS-FIXTURE::NAME"))
            (registry (%slot-named class "CL-MCP-CLOS-FIXTURE::REGISTRY")))
        (ok (equal "CL-MCP-CLOS-FIXTURE:CIRCLE" (gethash "from" radius)))
        (ok (equal '(":RADIUS") (%strings radius "initargs")))
        (ok (equal "(RANDOM 10)" (gethash "initform" radius)) "the code, not a value")
        (ok (equal "REAL" (gethash "type" radius)))
        (ok (equal '("CL-MCP-CLOS-FIXTURE:RADIUS") (%strings radius "readers")))
        (ok (equal '("(SETF CL-MCP-CLOS-FIXTURE:RADIUS)") (%strings radius "writers")))
        (ok (equal "CL-MCP-CLOS-FIXTURE:SHAPE" (gethash "from" name)))
        (ok (equal "\"anon\"" (gethash "initform" name)))
        (ok (equal "A label for the shape." (gethash "documentation" name)))
        (ok (equal '("CL-MCP-CLOS-FIXTURE:SHAPE-NAME") (%strings name "readers")))
        (ok (equal "class" (gethash "allocation" registry)))))))

(deftest class-report-reads-a-finalized-class-and-its-initargs
  (testing "a finalized class reports as such and lists its subclasses"
    (%load-fixture)
    (let* ((shape (find-class (find-symbol "SHAPE" "CL-MCP-CLOS-FIXTURE")))
           (class (%class "cl-mcp-clos-fixture:shape")))
      (ok (eq (sb-mop:class-finalized-p shape) (eq t (gethash "finalized" class))))
      (ok (null (set-exclusive-or '("CL-MCP-CLOS-FIXTURE:CIRCLE" "CL-MCP-CLOS-FIXTURE:SQUARE")
                                  (%strings class "direct_subclasses")
                                  :test #'equal)))
      (ok (equal "The base of every shape." (gethash "documentation" class)))))
  (testing "default initargs with the class that supplies them"
    (let ((initargs (sequence->list (gethash "default_initargs"
                                             (%class "cl-mcp-clos-fixture:square")))))
      (ok (equal '((":NAME" "\"square\"" "CL-MCP-CLOS-FIXTURE:SQUARE"))
                 (mapcar (lambda (entry)
                           (list (gethash "initarg" entry) (gethash "form" entry)
                                 (gethash "from" entry)))
                         initargs))))))

(deftest class-report-lists-specialized-methods
  (testing "own methods first, then inherited ones, the standard protocol left out"
    (let ((class (%class "cl-mcp-clos-fixture:circle")))
      (ok (= 7 (gethash "method_count" class)))
      (ok (equal '(("CL-MCP-CLOS-FIXTURE:AREA" (":AROUND") "CL-MCP-CLOS-FIXTURE:CIRCLE" "method")
                   ("CL-MCP-CLOS-FIXTURE:AREA" () "CL-MCP-CLOS-FIXTURE:CIRCLE" "method")
                   ("CL-MCP-CLOS-FIXTURE:RADIUS" () "CL-MCP-CLOS-FIXTURE:CIRCLE" "reader")
                   ("(SETF CL-MCP-CLOS-FIXTURE:RADIUS)" () "CL-MCP-CLOS-FIXTURE:CIRCLE" "writer")
                   ("CL-MCP-CLOS-FIXTURE:DESCRIBE-SHAPE" () "CL-MCP-CLOS-FIXTURE:SHAPE" "method")
                   ("(SETF CL-MCP-CLOS-FIXTURE:LABEL)" () "CL-MCP-CLOS-FIXTURE:SHAPE" "method")
                   ("CL-MCP-CLOS-FIXTURE:SHAPE-NAME" () "CL-MCP-CLOS-FIXTURE:SHAPE" "reader"))
                 (mapcar (lambda (method)
                           (list (gethash "generic_function" method)
                                 (%strings method "qualifiers")
                                 (gethash "via" method)
                                 (gethash "kind" method)))
                         (%methods class))))
      (ok (member "COMMON-LISP:STANDARD-OBJECT" (%strings class "omitted_classes")
                  :test #'equal))
      (ok (member "COMMON-LISP:T" (%strings class "omitted_classes") :test #'equal))))
  (testing "LIMIT applies to a class's methods too"
    (let ((class (gethash "class" (%report "cl-mcp-clos-fixture:circle" :limit 3))))
      (ok (= 7 (gethash "method_count" class)))
      (ok (= 3 (length (%methods class))))
      (ok (eq t (gethash "truncated" class))))))

(deftest class-report-handles-an-undefined-superclass
  (testing "no precedence list or effective slots, and the missing class named"
    (let* ((report (%report "cl-mcp-clos-fixture:pending"))
           (class (gethash "class" report)))
      (ok (null (gethash "precedence_list" class)))
      (ok (null (gethash "effective_slots" class)))
      (ok (equal '("CL-MCP-CLOS-FIXTURE::NOT-YET-DEFINED")
                 (%strings class "undefined_superclasses")))
      (ok (some (lambda (note) (search "undefined superclass" note))
                (%strings report "notes"))))))

(deftest class-report-covers-conditions-and-structures
  (testing "a condition's slot reader and its location"
    (let ((class (%class "cl-mcp-clos-fixture:probe-error")))
      (ok (equal "SB-PCL::CONDITION-CLASS" (gethash "metaclass" class)))
      (ok (eql (%line "(define-condition probe-error") (gethash "line" class)))
      (ok (equal '("CL-MCP-CLOS-FIXTURE:PROBE-ERROR-CODE")
                 (%strings (first (sequence->list (gethash "direct_slots" class))) "readers")))
      (ok (member "COMMON-LISP:CONDITION" (%strings class "omitted_classes") :test #'equal))))
  (testing "a structure's slots and initforms"
    (let ((class (%class "cl-mcp-clos-fixture:point")))
      (ok (equal "COMMON-LISP:STRUCTURE-CLASS" (gethash "metaclass" class)))
      (ok (eql (%line "(defstruct point") (gethash "line" class)))
      (ok (equal "0" (gethash "initform" (%slot-named class "CL-MCP-CLOS-FIXTURE::Y"))))))
  (testing "describing them signals no warning (SBCL warns on slot DOCUMENTATION)"
    (let ((warned nil))
      (handler-bind ((warning (lambda (w)
                                (setf warned (princ-to-string w))
                                (muffle-warning w))))
        (%report "cl-mcp-clos-fixture:probe-error")
        (%report "cl-mcp-clos-fixture:point"))
      (ok (null warned) warned))))
```

- [ ] **Step 2: テストが失敗することを確認する**

`<name>` を `clos-core-test` にして実行。
Expected: 新しい 6 本が `class` が NIL のため失敗し、Task 4 の 5 本は通る。

- [ ] **Step 3: クラスの関数を足す**

`lisp-edit-form` `insert_after`、form_type `defun`、form_name `%generic-function-entry` から順に（1 回に 1 フォーム）:

```lisp
(defun %language-class-p (class)
  "True when CLASS belongs to the language or the implementation: its name's
package is COMMON-LISP or an SB- package.  Their methods are the standard
protocol every class inherits."
  (let* ((name (%proper-class-name class))
         (package (and name (symbol-package name)))
         (package-name (and package (package-name package))))
    (and package-name
         (or (string= package-name "COMMON-LISP")
             (uiop:string-prefix-p "SB-" package-name)))))
```

```lisp
(defun %undefined-ancestors (class)
  "Return the forward-referenced classes among CLASS's ancestors."
  (let ((seen '())
        (undefined '()))
    (labels ((walk (c)
               (unless (member c seen)
                 (push c seen)
                 (if (typep c 'sb-mop:forward-referenced-class)
                     (pushnew c undefined)
                     (mapc #'walk (ignore-errors (sb-mop:class-direct-superclasses c)))))))
      (walk class))
    (nreverse undefined)))
```

```lisp
(defun %precedence-list (class)
  "Return CLASS's precedence list without finalizing it, or NIL when an
ancestor is undefined."
  (if (sb-mop:class-finalized-p class)
      (sb-mop:class-precedence-list class)
      (ignore-errors (sb-mop:compute-class-precedence-list class))))
```

```lisp
(defun %direct-slots-named (cpl name)
  "Return (CLASS . DIRECT-SLOT) for each class in CPL defining a slot NAME."
  (loop for class in cpl
        for slot = (find name (ignore-errors (sb-mop:class-direct-slots class))
                         :key #'sb-mop:slot-definition-name)
        when slot collect (cons class slot)))
```

```lisp
(defun %names (names)
  "Return a vector of NAMES, function or class names, fully qualified."
  (map 'vector #'%name-string names))
```

```lisp
(defun %slot-type-text (types package)
  "Return the type of a slot declared TYPES along the precedence list: T, the
one declared type, or the conjunction of several."
  (let ((declared (remove-duplicates (remove t types) :test #'equal :from-end t)))
    (%form-text (cond ((null declared) t)
                      ((null (rest declared)) (first declared))
                      (t (cons 'and declared)))
                package)))
```

```lisp
(defun %allocation-text (allocation)
  "Return a slot ALLOCATION as lower-case text: instance or class."
  (if (symbolp allocation)
      (string-downcase (symbol-name allocation))
      "class"))
```

```lisp
(defun %slot-documentation (slot)
  "Return SLOT's documentation, or NIL.  Only standard slot definitions are
asked: SBCL warns \"unsupported DOCUMENTATION\" for condition and structure
slots, and that warning would reach the caller's stderr."
  (and (typep slot 'sb-mop:standard-slot-definition)
       (ignore-errors (documentation slot t))))
```

```lisp
(defun %direct-slot-entry (slot package)
  "Return the JSON object for SLOT, a direct slot definition."
  (make-ht "name" (qualified-symbol-name (sb-mop:slot-definition-name slot))
           "initargs" (map 'vector #'%datum-text (sb-mop:slot-definition-initargs slot))
           "initform" (and (sb-mop:slot-definition-initfunction slot)
                           (%form-text (sb-mop:slot-definition-initform slot) package))
           "type" (%form-text (sb-mop:slot-definition-type slot) package)
           "allocation" (%allocation-text (sb-mop:slot-definition-allocation slot))
           "readers" (%names (ignore-errors (sb-mop:slot-definition-readers slot)))
           "writers" (%names (ignore-errors (sb-mop:slot-definition-writers slot)))
           "documentation" (%slot-documentation slot)))
```

```lisp
(defun %effective-slot-entry (name cpl package &optional effective)
  "Return the JSON object for the slot NAME as CPL's classes define it.

EFFECTIVE, the finalized class's effective slot definition, supplies initargs,
initform, type and allocation when given.  Otherwise they are merged from the
direct slots the standard way (CLHS 7.5.3): allocation and documentation from
the most specific, the initform from the most specific that has one, the
initargs from all of them, the type as the conjunction of their types.
Readers and writers are those of every direct slot NAME."
  (let* ((pairs (%direct-slots-named cpl name))
         (slots (mapcar #'cdr pairs))
         (with-initform (find-if #'sb-mop:slot-definition-initfunction slots)))
    (flet ((all (reader)
             (remove-duplicates (mapcan (lambda (slot)
                                          (copy-list (ignore-errors (funcall reader slot))))
                                        slots)
                                :test #'equal :from-end t)))
      (make-ht "name" (qualified-symbol-name name)
               "from" (and pairs (%name-string (%proper-class-name (car (first pairs)))))
               "initargs" (map 'vector #'%datum-text
                               (if effective
                                   (sb-mop:slot-definition-initargs effective)
                                   (all #'sb-mop:slot-definition-initargs)))
               "initform" (let ((source (or effective with-initform)))
                            (and source (sb-mop:slot-definition-initfunction source)
                                 (%form-text (sb-mop:slot-definition-initform source) package)))
               "type" (if effective
                          (%form-text (sb-mop:slot-definition-type effective) package)
                          (%slot-type-text (mapcar #'sb-mop:slot-definition-type slots)
                                           package))
               "allocation" (%allocation-text
                             (sb-mop:slot-definition-allocation (or effective (first slots))))
               "readers" (%names (all #'sb-mop:slot-definition-readers))
               "writers" (%names (all #'sb-mop:slot-definition-writers))
               "documentation" (some #'%slot-documentation slots)))))
```

```lisp
(defun %effective-slots (class cpl package)
  "Return the JSON objects for CLASS's effective slots, or NIL without CPL.
A finalized class's own effective slots are used, so a metaclass that
computes them differently is respected; an unfinalized class's are merged
from the direct slots, the most general class's first."
  (cond
    ((null cpl) nil)
    ((sb-mop:class-finalized-p class)
     (map 'vector
          (lambda (effective)
            (%effective-slot-entry (sb-mop:slot-definition-name effective) cpl package effective))
          (sb-mop:class-slots class)))
    (t
     (let ((names (remove-duplicates
                   (loop for c in (reverse cpl)
                         append (mapcar #'sb-mop:slot-definition-name
                                        (ignore-errors (sb-mop:class-direct-slots c))))
                   :from-end t)))
       (map 'vector (lambda (name) (%effective-slot-entry name cpl package)) names)))))
```

```lisp
(defun %default-initargs (cpl package)
  "Return the JSON objects for the default initargs of the class whose
precedence list is CPL, each with the class that supplies it, or NIL without
CPL."
  (when cpl
    (let ((seen '())
          (entries '()))
      (dolist (c cpl)
        (dolist (initarg (ignore-errors (sb-mop:class-direct-default-initargs c)))
          (unless (member (first initarg) seen)
            (push (first initarg) seen)
            (push (make-ht "initarg" (%datum-text (first initarg))
                           "form" (%form-text (second initarg) package)
                           "from" (%name-string (%proper-class-name c)))
                  entries))))
      (coerce (nreverse entries) 'vector))))
```

```lisp
(defun %class-methods (class cpl)
  "Return (values PAIRS OMITTED) for the methods specialized on CLASS or its
superclasses.  PAIRS are (METHOD . CLASS-SPECIALIZED), ordered by that class's
place in CPL, then by generic function name -- X before (SETF X) -- and role.
OMITTED lists the language-level superclasses (%LANGUAGE-CLASS-P) whose
methods were left out; CLASS itself is never left out."
  (let ((seen (make-hash-table :test #'eq))
        (entries '())
        (omitted '()))
    (loop for c in (or cpl (list class))
          for rank from 0
          for methods = (ignore-errors (sb-mop:specializer-direct-methods c))
          do (if (and (not (eq c class)) (%language-class-p c))
                 (when methods (push c omitted))
                 (dolist (method methods)
                   (unless (gethash method seen)
                     (setf (gethash method seen) t)
                     (push (list method c rank) entries)))))
    (flet ((key (entry)
             (destructuring-bind (method c rank) entry
               (declare (ignore c))
               (let* ((gf (ignore-errors (sb-mop:method-generic-function method)))
                      (name (and gf (sb-mop:generic-function-name gf)))
                      (setf-p (consp name))
                      (base (if setf-p (second name) name)))
                 (list rank
                       (if (symbolp base) (symbol-name base) "")
                       (if setf-p 1 0)
                       (%role-rank method))))))
      (values (mapcar (lambda (entry) (cons (first entry) (second entry)))
                      (stable-sort (nreverse entries) #'%key< :key #'key))
              (nreverse omitted)))))
```

```lisp
(defun %class-entry (class limit)
  "Return (values ENTRY NOTES): the JSON object for CLASS, with at most LIMIT
methods, and the notes the report should carry about it."
  (let* ((name (%proper-class-name class))
         (package (%home-package name))
         (finalized (sb-mop:class-finalized-p class))
         (cpl (%precedence-list class))
         (by-name (%introspect "FIND-DEFINITION-SOURCES-BY-NAME"))
         (source (and by-name name
                      (loop for kind in '(:class :condition :structure)
                            thereis (first (ignore-errors (funcall by-name name kind))))))
         (ht (%set-location (make-ht) source))
         (notes '()))
    (multiple-value-bind (pairs omitted) (%class-methods class cpl)
      (setf (gethash "name" ht) (%name-string name)
            (gethash "metaclass" ht) (%name-string (class-name (class-of class)))
            (gethash "documentation" ht) (ignore-errors (documentation class t))
            (gethash "finalized" ht) (json-bool finalized)
            (gethash "direct_superclasses" ht)
            (%names (mapcar #'class-name (sb-mop:class-direct-superclasses class)))
            (gethash "direct_subclasses" ht)
            (%names (remove nil (mapcar #'%proper-class-name
                                        (sb-mop:class-direct-subclasses class))))
            (gethash "precedence_list" ht) (and cpl (%names (mapcar #'class-name cpl)))
            (gethash "undefined_superclasses" ht)
            (if cpl
                (vector)
                (%names (mapcar #'class-name (%undefined-ancestors class))))
            (gethash "direct_slots" ht)
            (map 'vector (lambda (slot) (%direct-slot-entry slot package))
                 (ignore-errors (sb-mop:class-direct-slots class)))
            (gethash "effective_slots" ht) (%effective-slots class cpl package)
            (gethash "default_initargs" ht) (%default-initargs cpl package)
            (gethash "method_count" ht) (length pairs)
            (gethash "truncated" ht) (json-bool (> (length pairs) limit))
            (gethash "methods" ht)
            (map 'vector (lambda (pair) (%method-entry (car pair) :via (cdr pair)))
                 (subseq pairs 0 (min limit (length pairs))))
            (gethash "omitted_classes" ht) (%names (mapcar #'class-name omitted)))
      (cond
        ((null cpl)
         (push (format nil "precedence list and effective slots unavailable: ~
undefined superclass ~{~A~^, ~}"
                       (coerce (gethash "undefined_superclasses" ht) 'list))
               notes))
        ((not finalized)
         (push *note-not-finalized* notes))))
    (values ht (nreverse notes))))
```

- [ ] **Step 4: `clos-describe-report` をクラス対応に置き換える**

`lisp-edit-form` `replace`、form_type `defun`、form_name `clos-describe-report`:

```lisp
(defun clos-describe-report (symbol-name &key package (limit 50))
  "Return the clos-describe payload for SYMBOL-NAME, everything but its content
text and the form_type, form_name and note fields the parent fills in.

SYMBOL-NAME is resolved like code-find-references' symbol (RESOLVE-TARGET):
nothing is interned.  The report holds a generic_functions entry for the
function SYMBOL-NAME names and one for its SETF function, when either is a
generic function, and a class entry when it names a class.  At most LIMIT
methods are listed per entry.  docs/tools.md describes every field."
  (multiple-value-bind (symbol status lookup-package lookup-name)
      (resolve-target symbol-name :package package)
    (let ((report (make-ht "symbol" symbol-name
                           "symbol_status" (%status-string status)
                           "resolved_symbol" (and symbol (qualified-symbol-name symbol))
                           "symbol_kind" (and (eq status :found) (symbol-kind symbol))
                           "lookup_package" lookup-package
                           "lookup_name" lookup-name
                           "generic_functions" (vector)
                           "class" nil
                           "limit" limit
                           "notes" (vector))))
      (when (eq status :found)
        (with-definition-source-cache
          (setf (gethash "generic_functions" report)
                (coerce (loop for name in (list symbol (list 'setf symbol))
                              for function = (ignore-errors
                                              (and (fboundp name) (fdefinition name)))
                              when (typep function 'generic-function)
                                collect (%generic-function-entry function limit))
                        'vector))
          (let ((class (find-class symbol nil)))
            (when class
              (multiple-value-bind (entry notes) (%class-entry class limit)
                (setf (gethash "class" report) entry
                      (gethash "notes" report) (coerce notes 'vector)))))))
      report)))
```

- [ ] **Step 5: テストが通ることを確認する**

`<name>` を `clos-core-test` にして実行。
Expected: 11 本すべて `✗` なし。

- [ ] **Step 6: Lint とコミット**

```bash
mallet src/clos-core.lisp tests/clos-core-test.lisp
git add src/clos-core.lisp tests/clos-core-test.lisp
git commit -m "feat(clos-core): report a class's hierarchy, slots and methods

Return the superclasses, subclasses, precedence list, direct and effective
slots with the class each comes from, default initargs and the methods
specialized on the class and its user-level superclasses.  An unfinalized
class is not finalized: its precedence list is computed and its effective
slots merged from the direct slots, with a note, and initforms stay code.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FwJGaMhQXB93nGDWsR7eSS"
```

---
### Task 6: 注釈と本文テキスト（`clos-response-builders`）

**Files:**
- Modify: `src/code-refs-core.lisp`（`*note-stale*` を export）
- Create: `src/tools/clos-response-builders.lisp`
- Create: `tests/clos-response-builders-test.lisp`
- Modify: `tests.lisp`（登録）

**Interfaces:**
- Consumes: Task 3 の `top-level-forms-at`、Task 4/5 の report、既存の `sequence->list`、`text-content`、`*note-stale*`
- Produces（`cl-mcp/src/tools/clos-response-builders` から export）:
  - `build-clos-describe-response (report) → hash-table`: report なら注釈して `content` を付けて返す。report でなければ（isError の結果）そのまま返す
  - `annotate-report-forms (report) → report`: 各定義に `form_type` / `form_name` / `note` を付け、`abs_path` を消す
  - `clos-report-p (object) → boolean`
  - `*note-no-form-at-line*`、`*note-unparseable*`

- [ ] **Step 1: 失敗するテストを書く**

`fs-write-file` で `tests/clos-response-builders-test.lisp`:

```lisp
;;;; tests/clos-response-builders-test.lisp
;;;;
;;;; Tests for cl-mcp/src/tools/clos-response-builders: annotating a
;;;; clos-describe report from its source files and rendering its text.

(defpackage #:cl-mcp/tests/clos-response-builders-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/clos-response-builders
                #:build-clos-describe-response
                #:annotate-report-forms
                #:clos-report-p
                #:*note-no-form-at-line*
                #:*note-unparseable*)
  (:import-from #:cl-mcp/src/clos-core
                #:clos-describe-report)
  (:import-from #:cl-mcp/src/code-refs-core
                #:sequence->list
                #:*note-stale*)
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:text-content)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*))

(in-package #:cl-mcp/tests/clos-response-builders-test)

(defparameter *fixture*
  (asdf/system:system-relative-pathname :cl-mcp "tests/fixtures/clos-fixture.lisp")
  "CLOS definitions compiled so that SBCL records their source locations.")

(defun %load-fixture ()
  "Compile and load the fixture with its truename as the source namestring.
repl-eval's compilation unit would otherwise name the file \"repl-eval\"."
  (let ((truename (truename *fixture*)))
    (uiop:with-temporary-file (:pathname fasl :type "fasl")
      (with-compilation-unit (:override t :source-namestring (namestring truename))
        (handler-bind ((warning #'muffle-warning))
          (load (compile-file truename :output-file fasl :verbose nil :print nil)))))))

(defun %text (response)
  "Return the text of RESPONSE's first content part."
  (gethash "text" (elt (gethash "content" response) 0)))

(defun %method (&key (gf "PKG::AREA") qualifiers specializers (kind "method") slot via
                  path line form-type form-name note)
  "Return a synthetic method object."
  (make-ht "generic_function" gf
           "qualifiers" (coerce qualifiers 'vector)
           "specializers" (coerce specializers 'vector)
           "kind" kind "slot" slot "via" via
           "path" path "line" line "stale" yason:false
           "form_type" form-type "form_name" form-name "note" note))

(defun %slot (&key name from initargs initform (type "T") (allocation "instance")
                readers writers)
  "Return a synthetic slot object."
  (make-ht "name" name "from" from "initargs" (coerce initargs 'vector)
           "initform" initform "type" type "allocation" allocation
           "readers" (coerce readers 'vector) "writers" (coerce writers 'vector)
           "documentation" nil))

(defun %gf-report ()
  "Return a synthetic report on PKG::AREA, with 2 of its 3 methods."
  (make-ht "symbol" "pkg::area" "symbol_status" "found" "resolved_symbol" "PKG::AREA"
           "symbol_kind" "generic-function" "lookup_package" "PKG" "lookup_name" "AREA"
           "generic_functions"
           (vector (make-ht "name" "PKG::AREA" "lambda_list" "(SHAPE)" "documentation" "Area."
                            "method_combination" "STANDARD"
                            "path" "src/shapes.lisp" "line" 3 "stale" yason:false
                            "form_type" "defgeneric" "form_name" "area" "note" nil
                            "method_count" 3 "truncated" t
                            "methods"
                            (vector (%method :qualifiers '(":AROUND") :specializers '("PKG::CIRCLE")
                                             :path "src/shapes.lisp" :line 9
                                             :form-type "defmethod"
                                             :form-name "area :around ((s circle))")
                                    (%method :specializers '("PKG::SQUARE")
                                             :note "could not read this method: boom"))))
           "class" nil "limit" 2 "notes" (vector)))

(defun %class-report ()
  "Return a synthetic report on the unfinalized class PKG:CIRCLE."
  (make-ht "symbol" "pkg:circle" "symbol_status" "found" "resolved_symbol" "PKG:CIRCLE"
           "symbol_kind" "unbound" "lookup_package" "PKG" "lookup_name" "CIRCLE"
           "generic_functions" (vector)
           "class"
           (make-ht "name" "PKG:CIRCLE" "metaclass" "COMMON-LISP:STANDARD-CLASS"
                    "documentation" nil "finalized" yason:false
                    "path" "src/shapes.lisp" "line" 20 "stale" yason:false
                    "form_type" "defclass" "form_name" "circle" "note" nil
                    "direct_superclasses" (vector "PKG::SHAPE") "direct_subclasses" (vector)
                    "precedence_list" (vector "PKG:CIRCLE" "PKG::SHAPE"
                                              "COMMON-LISP:STANDARD-OBJECT" "COMMON-LISP:T")
                    "undefined_superclasses" (vector)
                    "direct_slots" (vector)
                    "effective_slots"
                    (vector (%slot :name "PKG::NAME" :from "PKG::SHAPE" :initargs '(":NAME")
                                   :initform "\"anon\"" :readers '("PKG:SHAPE-NAME"))
                            (%slot :name "PKG:RADIUS" :from "PKG:CIRCLE" :initargs '(":RADIUS")
                                   :initform "(RANDOM 10)" :type "REAL"
                                   :readers '("PKG:RADIUS") :writers '("(SETF PKG:RADIUS)"))
                            (%slot :name "PKG::REGISTRY" :from "PKG::SHAPE" :allocation "class"))
                    "default_initargs" (vector (make-ht "initarg" ":NAME" "form" "\"round\""
                                                        "from" "PKG::SHAPE"))
                    "method_count" 2 "truncated" yason:false
                    "methods"
                    (vector (%method :gf "PKG:RADIUS" :specializers '("PKG:CIRCLE") :kind "reader"
                                     :slot "PKG:RADIUS" :via "PKG:CIRCLE"
                                     :path "src/shapes.lisp" :line 20
                                     :form-type "defclass" :form-name "circle")
                            (%method :gf "(SETF PKG::LABEL)"
                                     :specializers '("COMMON-LISP:T" "PKG::SHAPE")
                                     :via "PKG::SHAPE" :path "src/shapes.lisp" :line 30
                                     :form-type "defmethod"
                                     :form-name "(setf label) (value (s shape))"))
                    "omitted_classes" (vector "COMMON-LISP:STANDARD-OBJECT" "COMMON-LISP:T"))
           "limit" 50
           "notes" (vector (concatenate 'string "not finalized; precedence list and slots "
                                        "were computed without finalizing the class"))))

(defun %round-trip (report)
  "Return REPORT as it arrives from the worker: JSON-encoded and parsed back,
so arrays are lists and false is NIL."
  (yason:parse (with-output-to-string (out) (yason:encode report out))))

(defun %lines (&rest lines)
  "Return LINES joined, each ended by a newline."
  (format nil "~{~A~%~}" lines))

(deftest clos-text-for-a-generic-function
  (testing "header, documentation, definition, aligned methods and the rest counted"
    (ok (equal (%lines
                "Generic function PKG::AREA (SHAPE) — standard combination, 3 methods"
                "Area."
                "Defined at src/shapes.lisp:3 (defgeneric area)"
                "  :AROUND (CIRCLE)  src/shapes.lisp:9 (defmethod area :around ((s circle)))"
                "  (SQUARE)          (no source)  [could not read this method: boom]"
                "  … and 1 more (raise limit to see them)")
               (%text (build-clos-describe-response (%gf-report)))))))

(deftest clos-text-for-a-class
  (testing "hierarchy, aligned slots, initargs and methods with their origin"
    (ok (equal (%lines
                (concatenate 'string "Class PKG:CIRCLE (standard-class, not finalized) — "
                             "src/shapes.lisp:20 (defclass circle)")
                "Superclasses: SHAPE"
                "Subclasses: (none)"
                "Precedence: CIRCLE SHAPE STANDARD-OBJECT T"
                "Slots (3):"
                "  NAME      from SHAPE :initarg :NAME :initform \"anon\"  reader SHAPE-NAME"
                (concatenate 'string "  RADIUS    direct     :initarg :RADIUS "
                             ":initform (RANDOM 10) :type REAL  accessor RADIUS")
                "  REGISTRY  from SHAPE :allocation :class"
                "Default initargs:"
                "  :NAME \"round\" from SHAPE"
                "Methods (2; standard protocol on STANDARD-OBJECT, T omitted):"
                "  RADIUS (CIRCLE) [reader]          src/shapes.lisp:20 (defclass circle)"
                (concatenate 'string "  (SETF LABEL) (T SHAPE) via SHAPE  src/shapes.lisp:30 "
                             "(defmethod (setf label) (value (s shape)))")
                (concatenate 'string "Note: not finalized; precedence list and slots were computed "
                             "without finalizing the class"))
               (%text (build-clos-describe-response (%class-report)))))))

(deftest clos-text-is-the-same-after-the-worker-round-trip
  (testing "lists for vectors and NIL for false render the same text"
    (dolist (make (list #'%gf-report #'%class-report))
      (ok (equal (%text (build-clos-describe-response (funcall make)))
                 (%text (build-clos-describe-response (%round-trip (funcall make)))))))))

(deftest clos-text-explains-an-empty-answer
  (flet ((text-for (&rest pairs)
           (%text (build-clos-describe-response
                   (apply #'make-ht "symbol" "x" "generic_functions" (vector) "class" nil
                          "notes" (vector) pairs)))))
    (testing "missing symbol and missing package"
      (ok (search "Symbol \"FOO\" not found in PKG (nothing was interned)"
                  (text-for "symbol_status" "not_found" "lookup_name" "FOO"
                            "lookup_package" "PKG")))
      (ok (search "Package \"NOPE\" not found (nothing was interned)"
                  (text-for "symbol_status" "package_not_found" "lookup_package" "NOPE"))))
    (testing "a symbol naming neither a class nor a generic function"
      (ok (search (concatenate 'string "COMMON-LISP:CAR names a function, not a generic function "
                               "or class; code-describe describes it.")
                  (text-for "symbol_status" "found" "resolved_symbol" "COMMON-LISP:CAR"
                            "symbol_kind" "function")))
      (ok (search "PKG::X names nothing in this image. Is the system loaded?"
                  (text-for "symbol_status" "found" "resolved_symbol" "PKG::X"
                            "symbol_kind" "unbound"))))))

(deftest build-clos-describe-response-passes-errors-through
  (testing "a crash notice or worker error is returned untouched"
    (let ((error-result (make-ht "isError" t "content" (text-content "Worker error: boom"))))
      (ok (not (clos-report-p error-result)))
      (ok (eq error-result (build-clos-describe-response error-result))))))

(deftest annotate-report-forms-reads-the-fixture
  (testing "each definition gets the form starting on its line, and abs_path goes"
    (%load-fixture)
    (let* ((*project-root* (asdf:system-source-directory :cl-mcp))
           (report (annotate-report-forms (clos-describe-report "cl-mcp-clos-fixture:circle")))
           (class (gethash "class" report)))
      (ok (equal '("defclass" "circle")
                 (list (gethash "form_type" class) (gethash "form_name" class))))
      (ok (not (nth-value 1 (gethash "abs_path" class))))
      (ok (equal `(("defmethod" "area :around ((shape circle))")
                   ("defmethod" "area ((shape circle))")
                   ("defclass" "circle")
                   ("defclass" "circle")
                   ("defmethod" ,(concatenate 'string "describe-shape ((shape shape) "
                                              "&optional (stream *standard-output*) verbose)"))
                   ("defmethod" "(setf label) (value (shape shape))")
                   ("defclass" "shape"))
                 (mapcar (lambda (method)
                           (list (gethash "form_type" method) (gethash "form_name" method)))
                         (sequence->list (gethash "methods" class))))))))

(deftest annotate-report-forms-explains-a-missing-form
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (flet ((annotated (abs-path line &key stale)
             (let ((entry (make-ht "abs_path" abs-path "path" "x.lisp" "line" line
                                   "stale" (if stale t yason:false)
                                   "form_type" nil "form_name" nil "note" nil)))
               (annotate-report-forms
                (make-ht "symbol_status" "found" "generic_functions" (vector)
                         "class" (make-ht "abs_path" abs-path "path" "x.lisp" "line" line
                                          "stale" (if stale t yason:false)
                                          "form_type" nil "form_name" nil "note" nil
                                          "methods" (vector entry))))
               entry)))
      (testing "a line that starts no form"
        (let ((entry (annotated (namestring (truename *fixture*)) 2)))
          (ok (null (gethash "form_name" entry)))
          (ok (equal *note-no-form-at-line* (gethash "note" entry)))))
      (testing "a stale file"
        (ok (equal *note-stale*
                   (gethash "note" (annotated (namestring (truename *fixture*)) 2 :stale t)))))
      (testing "a file that does not parse"
        (let ((file (asdf/system:system-relative-pathname
                     :cl-mcp "tests/tmp/clos-unparseable.lisp")))
          (ensure-directories-exist file)
          (with-open-file (out file :direction :output :if-exists :supersede)
            (format out "(defparameter *x* #.(+ 1 2))~%"))
          (unwind-protect
               (let ((note (gethash "note" (annotated (namestring (truename file)) 1))))
                 (ok (and note (eql 0 (search *note-unparseable* note))) note))
            (ignore-errors (delete-file file)))))
      (testing "a file outside the readable paths gets no note"
        (ok (null (gethash "note" (annotated "/nonexistent-cl-mcp-dir/x.lisp" 1))))))))

(deftest clos-describe-form-names-work-in-lisp-edit-form
  (testing "every form_name the fixture's reports carry finds that very form"
    (%load-fixture)
    (let* ((*project-root* (asdf:system-source-directory :cl-mcp))
           (lines (uiop:read-file-lines *fixture*))
           (checked 0))
      (dolist (designator '("cl-mcp-clos-fixture:area" "cl-mcp-clos-fixture:circle"
                            "cl-mcp-clos-fixture:square" "cl-mcp-clos-fixture:label"
                            "cl-mcp-clos-fixture:combine" "cl-mcp-clos-fixture:describe-shape"
                            "cl-mcp-clos-fixture:probe-error" "cl-mcp-clos-fixture:point"
                            "cl-mcp-clos-fixture:radius"))
        (let* ((report (build-clos-describe-response (clos-describe-report designator)))
               (class (gethash "class" report))
               (entries (append (loop for gf
                                        in (sequence->list (gethash "generic_functions" report))
                                      collect gf
                                      append (sequence->list (gethash "methods" gf)))
                                (and (hash-table-p class)
                                     (cons class (sequence->list (gethash "methods" class)))))))
          (dolist (entry entries)
            (let ((path (gethash "path" entry)))
              (when (and path (search "tests/fixtures/clos-fixture.lisp" path))
                (incf checked)
                (let* ((result (lisp-edit-form
                                :file-path (namestring (truename *fixture*))
                                :form-type (gethash "form_type" entry)
                                :form-name (gethash "form_name" entry)
                                :operation "delete"
                                :dry-run t))
                       (original (gethash "original" result)))
                  (ok (equal (nth (1- (gethash "line" entry)) lines)
                             (subseq original 0 (or (position #\Newline original)
                                                    (length original))))
                      (format nil "~A ~A" (gethash "form_type" entry)
                              (gethash "form_name" entry)))))))))
      (ok (> checked 25) (format nil "~D definitions checked" checked)))))
```

`tests.lisp` の `(:import-from #:cl-mcp/tests/clos-core-test)` の直後に `(:import-from #:cl-mcp/tests/clos-response-builders-test)` を追加。

- [ ] **Step 2: テストが失敗することを確認する**

`<name>` を `clos-response-builders-test` にして実行。
Expected: `cl-mcp/src/tools/clos-response-builders` が見つからないロードエラー。

- [ ] **Step 3: `*note-stale*` を export する**

`lisp-patch-form`、form_type `defpackage`、form_name `cl-mcp/src/code-refs-core`:
- old_text: `           #:%status-string))`
- new_text: `           #:%status-string\n           #:*note-stale*))`（`\n` は実際の改行）

- [ ] **Step 4: `src/tools/clos-response-builders.lisp` を作る**

`fs-write-file` で次の内容のファイルを作る（親イメージが古くて後続の編集ができない場合も、新規ファイルなので全体を一度に書いてよい。書いたら `lisp-check-parens`）:

```lisp
;;;; src/tools/clos-response-builders.lisp
;;;;
;;;; Parent-side half of clos-describe: annotate the worker's report with the
;;;; form_type and form_name of each definition, read from its source file, and
;;;; render the content text.  Loads eclector (through code-refs-scan), so the
;;;; worker must not import this file.

(defpackage #:cl-mcp/src/tools/clos-response-builders
  (:use #:cl)
  (:import-from #:cl-mcp/src/code-refs-scan
                #:top-level-forms-at)
  (:import-from #:cl-mcp/src/code-refs-core
                #:sequence->list
                #:*note-stale*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:text-content)
  (:import-from #:cl-ppcre
                #:regex-replace-all
                #:quote-meta-chars)
  (:export #:clos-report-p
           #:annotate-report-forms
           #:build-clos-describe-response
           #:*note-no-form-at-line*
           #:*note-unparseable*))

(in-package #:cl-mcp/src/tools/clos-response-builders)

(defparameter *note-no-form-at-line* "no top-level form starts at this line"
  "Note on a definition whose recorded line starts no top-level form in its file.")

(defparameter *note-unparseable* "file could not be parsed"
  "Note on a definition whose source file does not parse.")

(defun %true-p (value)
  "True when VALUE, a JSON boolean, is true.  False arrives as YASON:FALSE
in-process and as NIL after the worker's JSON round trip."
  (and value (not (eq value yason:false))))

(defun clos-report-p (object)
  "True when OBJECT is a clos-describe report rather than an error result.
PROXY-TO-WORKER returns a crash notice or a worker error as a hash-table with
isError and content, which must reach the caller untouched."
  (and (hash-table-p object)
       (nth-value 1 (gethash "symbol_status" object))
       (not (%true-p (gethash "isError" object)))))

(defun %located-entries (report)
  "Return every object in REPORT that carries a source location: each generic
function and its methods, the class and its methods."
  (let ((entries '()))
    (dolist (gf (sequence->list (gethash "generic_functions" report)))
      (push gf entries)
      (dolist (method (sequence->list (gethash "methods" gf)))
        (push method entries)))
    (let ((class (gethash "class" report)))
      (when (hash-table-p class)
        (push class entries)
        (dolist (method (sequence->list (gethash "methods" class)))
          (push method entries))))
    (nreverse entries)))

(defun %add-note (entry note)
  "Append NOTE to ENTRY's note, separated by '; '."
  (let ((old (gethash "note" entry)))
    (setf (gethash "note" entry)
          (if (and (stringp old) (plusp (length old)))
              (format nil "~A; ~A" old note)
              note))))

(defun annotate-report-forms (report)
  "Fill in the form_type, form_name and note of every located object in
REPORT from its source file, then remove abs_path from each; return REPORT.

Each file is read once (TOP-LEVEL-FORMS-AT).  An object gets the form that
starts on its line.  When none does it gets a note instead: the file does not
parse, or it changed since it was loaded (stale), or neither, in which case the
recorded line simply starts no form.  A file the read policy refuses gets
neither form nor note -- the text still gives path:line."
  (let ((by-file (make-hash-table :test #'equal)))
    (dolist (entry (%located-entries report))
      (let ((abs-path (gethash "abs_path" entry)))
        (when (and (stringp abs-path) (integerp (gethash "line" entry)))
          (push entry (gethash abs-path by-file)))))
    (maphash (lambda (abs-path entries)
               (multiple-value-bind (table failure)
                   (top-level-forms-at abs-path
                                       (mapcar (lambda (entry) (gethash "line" entry))
                                               entries))
                 (dolist (entry entries)
                   (let ((form (gethash (gethash "line" entry) table)))
                     (cond
                       (form
                        (setf (gethash "form_type" entry) (car form)
                              (gethash "form_name" entry) (cdr form)))
                       ((eq failure :denied))
                       (failure
                        (%add-note entry (format nil "~A: ~A" *note-unparseable* failure)))
                       ((%true-p (gethash "stale" entry)) (%add-note entry *note-stale*))
                       (t (%add-note entry *note-no-form-at-line*)))))))
             by-file)
    (dolist (entry (%located-entries report))
      (remhash "abs_path" entry))
    report))

(defun %home-package-name (report)
  "Return the package name of REPORT's resolved symbol, or NIL."
  (let* ((resolved (gethash "resolved_symbol" report))
         (colon (and (stringp resolved) (position #\: resolved))))
    (and colon (plusp colon) (subseq resolved 0 colon))))

(defun %short (text home)
  "Return TEXT, names printed fully qualified, without the prefixes of HOME and
COMMON-LISP, as a reader in HOME would write them."
  (let ((result (or text "")))
    (dolist (package (remove nil (list home "COMMON-LISP")) result)
      (setf result (regex-replace-all
                    (format nil "(?<![^\\s(])~A::?" (quote-meta-chars package))
                    result "")))))

(defun %location-text (entry)
  "Return where ENTRY is defined: PATH:LINE (FORM-TYPE FORM-NAME), with the
note in brackets, or (no source)."
  (let ((path (gethash "path" entry))
        (line (gethash "line" entry))
        (form-type (gethash "form_type" entry))
        (form-name (gethash "form_name" entry))
        (note (gethash "note" entry)))
    (concatenate 'string
                 (if path
                     (format nil "~A~@[:~D~]~@[ (~A)~]"
                             path line
                             (and form-type (format nil "~A~@[ ~A~]" form-type form-name)))
                     "(no source)")
                 (if note (format nil "  [~A]" note) ""))))

(defun %method-signature (method home &key with-name class-name)
  "Return METHOD's signature: [NAME] QUALIFIERS (SPECIALIZERS) [kind], plus
'via CLASS' when it was found through a superclass of CLASS-NAME."
  (let ((qualifiers (sequence->list (gethash "qualifiers" method)))
        (specializers (sequence->list (gethash "specializers" method)))
        (kind (gethash "kind" method))
        (via (gethash "via" method)))
    (%short (format nil "~@[~A ~]~{~A ~}(~{~A~^ ~})~:[ [~A]~;~*~]~@[ via ~A~]"
                    (and with-name (gethash "generic_function" method))
                    qualifiers specializers
                    (equal kind "method") kind
                    (and via (not (equal via class-name)) via))
            home)))

(defun %write-methods (stream methods home &key with-name class-name)
  "Write one aligned line per method in METHODS to STREAM."
  (let* ((signatures (mapcar (lambda (method)
                               (%method-signature method home
                                                  :with-name with-name
                                                  :class-name class-name))
                             methods))
         (width (reduce #'max signatures :key #'length :initial-value 0)))
    (loop for method in methods
          for signature in signatures
          do (format stream "  ~vA  ~A~%" width signature (%location-text method)))))

(defun %write-more (stream entry)
  "Write how many of ENTRY's methods were left out, when any were."
  (let ((count (or (gethash "method_count" entry) 0))
        (shown (length (sequence->list (gethash "methods" entry)))))
    (when (> count shown)
      (format stream "  … and ~D more (raise limit to see them)~%" (- count shown)))))

(defun %write-generic-function (stream gf home)
  "Write the text for GF, one generic_functions entry, to STREAM."
  (let ((methods (sequence->list (gethash "methods" gf))))
    (format stream "Generic function ~A ~A — ~@[~(~A~) combination, ~]~D method~:P~%"
            (gethash "name" gf)
            (%short (gethash "lambda_list" gf) home)
            (gethash "method_combination" gf)
            (or (gethash "method_count" gf) 0))
    (when (gethash "documentation" gf)
      (format stream "~A~%" (gethash "documentation" gf)))
    (if (gethash "path" gf)
        (format stream "Defined at ~A~%" (%location-text gf))
        (format stream "No defgeneric: created by its first defmethod.~%"))
    (%write-methods stream methods home)
    (%write-more stream gf)))

(defun %accessor-words (slot home)
  "Return the reader, writer and accessor words for SLOT: accessor X when X
reads it and (SETF X) writes it."
  (let* ((readers (mapcar (lambda (name) (%short name home))
                          (sequence->list (gethash "readers" slot))))
         (writers (mapcar (lambda (name) (%short name home))
                          (sequence->list (gethash "writers" slot))))
         (words '()))
    (dolist (reader readers)
      (let ((writer (format nil "(SETF ~A)" reader)))
        (if (member writer writers :test #'string=)
            (progn (push (format nil "accessor ~A" reader) words)
                   (setf writers (remove writer writers :test #'string=)))
            (push (format nil "reader ~A" reader) words))))
    (dolist (writer writers)
      (push (format nil "writer ~A" writer) words))
    (nreverse words)))

(defun %slot-parts (slot class-name home)
  "Return (NAME ORIGIN ATTRIBUTES), the three columns of SLOT's text line:
its name, 'direct' or 'from CLASS' when a superclass of CLASS-NAME defines it,
and its initargs, initform, type, class allocation and accessors."
  (let ((from (gethash "from" slot))
        (type (gethash "type" slot)))
    (list (%short (gethash "name" slot) home)
          (if (and from (not (equal from class-name)))
              (format nil "from ~A" (%short from home))
              "direct")
          (format nil "~{ :initarg ~A~}~@[ :initform ~A~]~@[ :type ~A~]~
~:[~; :allocation :class~]~{  ~A~}"
                  (sequence->list (gethash "initargs" slot))
                  (gethash "initform" slot)
                  (and type (not (equal type "T")) type)
                  (equal (gethash "allocation" slot) "class")
                  (%accessor-words slot home)))))

(defun %write-class (stream class home)
  "Write the text for CLASS, the report's class entry, to STREAM."
  (let* ((name (gethash "name" class))
         (cpl (gethash "precedence_list" class))
         ;; Effective slots exist exactly when the precedence list does.  Test
         ;; that, not the slots: after the worker's JSON round trip an empty
         ;; array and null are both NIL.
         (slots (sequence->list (gethash (if cpl "effective_slots" "direct_slots") class)))
         (initargs (sequence->list (gethash "default_initargs" class)))
         (omitted (sequence->list (gethash "omitted_classes" class))))
    (format stream "Class ~A (~(~A~)~:[, not finalized~;~]) — ~A~%"
            name
            (%short (gethash "metaclass" class) home)
            (%true-p (gethash "finalized" class))
            (%location-text class))
    (when (gethash "documentation" class)
      (format stream "~A~%" (gethash "documentation" class)))
    (format stream "Superclasses: ~:[(none)~;~:*~{~A~^ ~}~]~%"
            (mapcar (lambda (c) (%short c home))
                    (sequence->list (gethash "direct_superclasses" class))))
    (format stream "Subclasses: ~:[(none)~;~:*~{~A~^ ~}~]~%"
            (mapcar (lambda (c) (%short c home))
                    (sequence->list (gethash "direct_subclasses" class))))
    (if cpl
        (format stream "Precedence: ~{~A~^ ~}~%"
                (mapcar (lambda (c) (%short c home)) (sequence->list cpl)))
        (format stream "Precedence: unavailable (undefined superclass ~{~A~^, ~})~%"
                (mapcar (lambda (c) (%short c home))
                        (sequence->list (gethash "undefined_superclasses" class)))))
    (format stream "~:[Direct slots~;Slots~] (~D):~%" cpl (length slots))
    (let* ((parts (mapcar (lambda (slot) (%slot-parts slot name home)) slots))
           (name-width (reduce #'max parts :key (lambda (p) (length (first p)))
                                           :initial-value 0))
           (origin-width (reduce #'max parts :key (lambda (p) (length (second p)))
                                             :initial-value 0)))
      (dolist (part parts)
        (format stream "~A~%"
                (string-right-trim " " (format nil "  ~vA  ~vA~A"
                                               name-width (first part)
                                               origin-width (second part)
                                               (third part))))))
    (format stream "Default initargs:~:[ (none)~;~]~%" initargs)
    (dolist (initarg initargs)
      (format stream "  ~A ~A~:[~; from ~A~]~%"
              (gethash "initarg" initarg)
              (%short (gethash "form" initarg) home)
              (not (equal (gethash "from" initarg) name))
              (%short (gethash "from" initarg) home)))
    (format stream "Methods (~D~@[; standard protocol on ~{~A~^, ~} omitted~]):~%"
            (or (gethash "method_count" class) 0)
            (and omitted (mapcar (lambda (c) (%short c home)) omitted)))
    (%write-methods stream (sequence->list (gethash "methods" class)) home
                    :with-name t :class-name name)
    (%write-more stream class)))

(defun %format-clos-report (report)
  "Return the content text for REPORT, an annotated clos-describe payload."
  (let ((status (gethash "symbol_status" report))
        (home (%home-package-name report))
        (gfs (sequence->list (gethash "generic_functions" report)))
        (class (gethash "class" report)))
    (with-output-to-string (s)
      (cond
        ((equal status "package_not_found")
         (format s "Package ~S not found (nothing was interned). ~
                    Load the system that defines it with load-system.~%"
                 (gethash "lookup_package" report)))
        ((equal status "not_found")
         (format s "Symbol ~S not found in ~A (nothing was interned). ~
                    Is the system loaded? Run load-system first.~%"
                 (gethash "lookup_name" report) (gethash "lookup_package" report)))
        ((and (null gfs) (not (hash-table-p class)))
         (if (equal (gethash "symbol_kind" report) "unbound")
             (format s "~A names nothing in this image. Is the system loaded?~%"
                     (gethash "resolved_symbol" report))
             (format s "~A names a ~A, not a generic function or class; ~
                        code-describe describes it.~%"
                     (gethash "resolved_symbol" report)
                     (gethash "symbol_kind" report))))
        (t
         (loop for (gf . more) on gfs
               do (%write-generic-function s gf home)
                  (when (or more (hash-table-p class))
                    (terpri s)))
         (when (hash-table-p class)
           (%write-class s class home))))
      (dolist (note (sequence->list (gethash "notes" report)))
        (format s "Note: ~A~%" note)))))

(defun build-clos-describe-response (report)
  "Return REPORT, a clos-describe payload, annotated and with its content text.

A result that is not a report (CLOS-REPORT-P), such as the error PROXY-TO-WORKER
returns when the worker crashed, is returned unchanged."
  (if (clos-report-p report)
      (progn
        (annotate-report-forms report)
        (setf (gethash "content" report)
              (text-content (%format-clos-report report)))
        report)
      report))
```

- [ ] **Step 5: テストが通ることを確認する**

`<name>` を `clos-response-builders-test` にして実行。
Expected: `✗` なし。往復テストの `definitions checked` は 26 以上。

- [ ] **Step 6: Lint とコミット**

```bash
mallet src/tools/clos-response-builders.lisp src/code-refs-core.lisp tests/clos-response-builders-test.lisp
git add src/tools/clos-response-builders.lisp src/code-refs-core.lisp tests/clos-response-builders-test.lisp tests.lisp
git commit -m "feat(clos-describe): annotate reports from source and render their text

Give every generic function, class and method in a report the form_type
and form_name of the form starting on its line, read once per file, or a
note saying why there is none, and render the text an MCP client shows:
aligned methods and slots with the names shortened as their own package
writes them.  Worker results that are errors pass through untouched.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FwJGaMhQXB93nGDWsR7eSS"
```

---
### Task 7: ツールと worker ハンドラの接続、文書

**Files:**
- Create: `src/clos.lisp`
- Modify: `src/worker/handlers.lisp`（import、`%handle-clos-describe`、登録表）
- Modify: `src/tools/all.lisp`、`main.lisp`（登録と export）
- Modify: `tests/tools-test.lisp`、`tests/worker-test.lisp`（テスト追加）
- Modify: `docs/tools.md`、`prompts/repl-driven-development.md`、`CLAUDE.md`

**Interfaces:**
- Consumes: Task 4/5 `clos-describe-report`、Task 6 `build-clos-describe-response`、既存の `proxy-to-worker`、`*use-worker-pool*`、`define-tool`、`arg-validation-error`
- Produces: MCP ツール `clos-describe`（引数 `symbol` / `package` / `limit`）。worker メソッド `worker/clos-describe`（params `symbol` `package` `limit`、結果は content を持たない report）

- [ ] **Step 1: 失敗するテストを書く**

`tests/tools-test.lisp` の `tools-call-code-find-references` の直前に追加:

```lisp
(deftest tools-call-clos-describe
  (testing "tools/list describes clos-describe"
    (multiple-value-bind (obj result tools) (%tools-list)
      (declare (ignore obj result))
      (let* ((desc (%find-tool-descriptor tools "clos-describe"))
             (schema (and desc (gethash "inputSchema" desc)))
             (props (and schema (gethash "properties" schema))))
        (ok desc)
        (ok (find "symbol" (gethash "required" schema) :test #'string=))
        (ok (equal "integer" (gethash "type" (gethash "limit" props)))))))
  (testing "tools/call clos-describe lists a generic function's methods in its text"
    (let* ((req (concatenate 'string
                  "{\"jsonrpc\":\"2.0\",\"id\":41,\"method\":\"tools/call\","
                  "\"params\":{\"name\":\"clos-describe\","
                  "\"arguments\":{\"symbol\":\"cl:print-object\",\"limit\":1}}}"))
           (result (gethash "result" (parse (%pjl req))))
           (text (gethash "text" (elt (gethash "content" result) 0))))
      (ok (equal "found" (gethash "symbol_status" result)))
      (ok (search "Generic function COMMON-LISP:PRINT-OBJECT" text))
      (ok (search "more (raise limit to see them)" text))))
  (testing "a missing symbol is explained, not an error"
    (let* ((req (concatenate 'string
                  "{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"tools/call\","
                  "\"params\":{\"name\":\"clos-describe\","
                  "\"arguments\":{\"symbol\":\"cl-user::no-such-clos-describe-name\"}}}"))
           (result (gethash "result" (parse (%pjl req)))))
      (ok (search "nothing was interned" (gethash "text" (elt (gethash "content" result) 0))))
      (ok (null (find-symbol "NO-SUCH-CLOS-DESCRIBE-NAME" "COMMON-LISP-USER")))))
  (testing "a non-positive limit is an argument error"
    (let* ((req (concatenate 'string
                  "{\"jsonrpc\":\"2.0\",\"id\":43,\"method\":\"tools/call\","
                  "\"params\":{\"name\":\"clos-describe\","
                  "\"arguments\":{\"symbol\":\"cl:print-object\",\"limit\":0}}}"))
           (obj (parse (%pjl req)))
           (result (gethash "result" obj)))
      (ok (or (gethash "error" obj) (and result (gethash "isError" result)))))))
```

`tests/worker-test.lisp` の `worker-code-find-references-returns-result` の直後に追加:

```lisp
(deftest worker-clos-describe-returns-the-report
  (testing "worker/clos-describe returns the report, which the parent renders"
    (with-handler-server (stream)
      (let ((params (make-hash-table :test 'equal)))
        (setf (gethash "symbol" params) "cl:print-object"
              (gethash "limit" params) 1)
        (let* ((response (%send-and-receive stream 420 "worker/clos-describe" params))
               (result (%result-of response))
               (gfs (gethash "generic_functions" result)))
          (ok (equal "found" (gethash "symbol_status" result)))
          (ok (= 1 (length gfs)))
          (ok (= 1 (length (gethash "methods" (first gfs)))))
          (ok (not (nth-value 1 (gethash "content" result))) "no content text yet")))))
  (testing "worker/clos-describe needs a symbol"
    (with-handler-server (stream)
      (let ((response (%send-and-receive stream 421 "worker/clos-describe"
                                         (make-hash-table :test 'equal))))
        (ok (gethash "error" response))))))
```

- [ ] **Step 2: テストが失敗することを確認する**

`<name>` を `tools-test`、次に `worker-test` にして実行。
Expected: `clos-describe` の descriptor が無い／`worker/clos-describe` が未登録（Method not found）で失敗。

- [ ] **Step 3: `src/clos.lisp` を作る**

`fs-write-file`:

```lisp
;;;; src/clos.lisp
;;;;
;;;; The clos-describe tool: the worker reads a class or generic function from
;;;; its image (clos-core), and the parent annotates that report from the source
;;;; files and renders its text (clos-response-builders).

(defpackage #:cl-mcp/src/clos
  (:use #:cl)
  (:import-from #:cl-mcp/src/clos-core
                #:clos-describe-report)
  (:import-from #:cl-mcp/src/tools/clos-response-builders
                #:build-clos-describe-response)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:arg-validation-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*
                #:proxy-to-worker)
  (:export #:clos-describe-report))

(in-package #:cl-mcp/src/clos)

(define-tool "clos-describe"
  :description "Describe a CLOS class or generic function from the running image -
the structure a source search cannot see:
- a generic function: its methods with their qualifiers (:around, :before,
  :after), specializers (classes, (eql ...)), method combination and source line
- a class: superclasses, subclasses, precedence list, direct and effective slots
  (initargs, initform as code, type, accessors, and the class each comes from),
  default initargs, and the methods specialized on it or its superclasses
A symbol naming a class and a generic function, or a SETF generic function,
gets every section.  Each definition's form_type and form_name can be passed
straight to lisp-edit-form.

Reads only: a class is never finalized, no initform is evaluated, and nothing
is interned.

PREREQUISITE: load the defining system first (load-system).

Use inspect-object for one instance's slot values, code-describe for a plain
function, macro or variable, and code-find-references for who calls a generic
function."
  :args ((symbol :type :string :required t
                 :description "Symbol name like \"my-pkg:shape\" (package-qualified preferred)")
         (package :type :string
                  :description "Optional package used when SYMBOL is unqualified")
         (limit :type :integer
                :description
                "Maximum methods listed per generic function and per class (default 50);
the total is always reported"))
  :body
  (progn
    ;; Checked before any worker call, so a bad value gets the same argument
    ;; error with and without the worker pool.
    (when (and limit (not (and (integerp limit) (plusp limit))))
      (error 'arg-validation-error
             :arg-name "limit"
             :message "limit must be a positive integer"))
    (let ((limit (or limit 50)))
      ;; Not WITH-PROXY-DISPATCH: the worker returns only the report, and both
      ;; paths finish it here, in the process that can parse source files.
      (result id
              (build-clos-describe-response
               (if *use-worker-pool*
                   (proxy-to-worker id "worker/clos-describe"
                                    (make-ht "symbol" symbol
                                             "package" package
                                             "limit" limit))
                   (clos-describe-report symbol :package package :limit limit)))))))
```

- [ ] **Step 4: worker ハンドラを足す**

`lisp-patch-form`、form_type `defpackage`、form_name `cl-mcp/src/worker/handlers`:
- old_text: `  (:import-from #:cl-mcp/src/system-loader-core`
- new_text:

```lisp
  (:import-from #:cl-mcp/src/clos-core
                #:clos-describe-report)
  (:import-from #:cl-mcp/src/system-loader-core
```

`lisp-edit-form` `insert_after`、form_type `defun`、form_name `%handle-code-find-references`:

```lisp
(defun %handle-clos-describe (params)
  "Describe a class or generic function.  Returns the clos-describe report
without content text: the parent annotates it from the source files, which
this image cannot parse, and renders the text."
  (let ((symbol (gethash "symbol" params))
        (package (gethash "package" params))
        (limit (or (gethash "limit" params) 50)))
    (unless symbol
      (error "symbol is required"))
    (clos-describe-report symbol :package package :limit limit)))
```

`lisp-patch-form`、form_type `defun`、form_name `register-all-handlers`:
- old_text: `                   (cons "worker/code-find-references" #'%handle-code-find-references)`
- new_text:

```lisp
                   (cons "worker/code-find-references" #'%handle-code-find-references)
                   (cons "worker/clos-describe" #'%handle-clos-describe)
```

- [ ] **Step 5: 登録する**

`src/tools/all.lisp` の defpackage で、`(:import-from #:cl-mcp/src/code ...)` の直後に追加:

```lisp
  (:import-from #:cl-mcp/src/clos
                #:clos-describe-report)
```

`main.lisp` の defpackage で、`(:import-from #:cl-mcp/src/code ...)` の直後に同じ import を追加し、export の `#:code-find-references` の直後に `#:clos-describe-report` を追加する。

- [ ] **Step 6: テストが通ることを確認する**

`<name>` を `tools-test`、`worker-test` にして実行。
Expected: `✗` なし。

- [ ] **Step 7: 文書を更新する**

`docs/tools.md`: `## \`clhs-lookup\`` の直前に次の節を足す:

````markdown
## `clos-describe`
Describe a CLOS class or generic function from the running image — the structure a source
search cannot see: a generic function's methods with their qualifiers, specializers and
source lines, and a class's superclasses, subclasses, precedence list, direct and effective
slots, default initargs and specialized methods.

Input:
- `symbol` (string, required): `pkg:name`, `pkg::name` or `name`; a single colon also finds internal symbols
- `package` (string, optional): package used when `symbol` is unqualified
- `limit` (integer, default `50`): most methods listed per generic function and per class; `method_count` always gives the total

Output (the content text carries everything that matters; names in it drop the symbol's own package and `COMMON-LISP:`):
- `symbol_status`: `found`, `not_found` or `package_not_found`; nothing is interned either way
- `resolved_symbol`, `symbol_kind`, `lookup_package`, `lookup_name`, `limit`, `notes`
- `generic_functions` (array, up to 2): the function `symbol` names and its `(setf symbol)` function, when generic
  - `name`, `lambda_list`, `documentation`, `method_combination` (`STANDARD`, `+ :MOST-SPECIFIC-FIRST`, ...)
  - `path`, `line`, `stale`, `form_type`, `form_name`, `note`: the `defgeneric`; `path` is null when only `defmethod` created the generic function
  - `method_count`, `truncated`, `methods`
- `class` (object or null):
  - `name`, `metaclass`, `documentation`, `finalized`, `path`, `line`, `stale`, `form_type`, `form_name`, `note`
  - `direct_superclasses`, `direct_subclasses`, `precedence_list` (null when a superclass is undefined), `undefined_superclasses`
  - `direct_slots`, `effective_slots` (null without a precedence list): `name`, `from` (effective slots: the most specific class defining it), `initargs`, `initform` (the code, never evaluated; null when there is none), `type`, `allocation` (`instance`, `class`), `readers`, `writers`, `documentation`
  - `default_initargs`: `initarg`, `form`, `from`
  - `method_count`, `truncated`, `methods`, `omitted_classes`: the methods specialized on the class and its superclasses, except superclasses in `COMMON-LISP` or an `SB-` package (the standard protocol), which `omitted_classes` names
- Method objects: `generic_function`, `qualifiers`, `specializers` (`PKG::CLASS`, `COMMON-LISP:T`, `(EQL :KEY)`), `kind` (`method`, `reader`, `writer`), `slot` (accessors), `via` (class methods: the class specialized), `path`, `line`, `stale`, `form_type`, `form_name`, `note`

`form_type` / `form_name` are read from the source file, so they can be passed straight to
`lisp-edit-form`: a `defmethod` gets its qualifiers and specializers, a method written inside
`defgeneric` gets that `defgeneric`, and a slot accessor gets its `defclass`. When no form starts
on the recorded line, `note` says why (the file changed since it was loaded, or does not parse).

Order: a generic function's methods run `:around`, `:before`, primary, `:after` for the standard
method combination, project files before other files; a class's methods follow its precedence
list, then the generic function's name.

Reads only: a class is never finalized — an unfinalized class's precedence list is computed and
its effective slots merged from the direct slots the standard way, with a note — no initform is
evaluated, and nothing is interned.

Limits: structure accessors are not MOP readers, so a `defstruct` slot lists none, and every
structure slot shows an initform (`NIL` when none was written). A metaclass that customizes
`compute-slots` may finalize with other effective slots than an unfinalized class shows. Lines come
from SBCL's record of each file's top-level forms, or from reading the file once that record has
been garbage collected with the file's code; a file using a custom reader macro whose record is
gone gets no line, and without the record `stale` is not known.

Use `inspect-object` for one instance's slot values, `code-describe` for a plain function, macro or
variable, and `code-find-references` for who calls a generic function.
````

`prompts/repl-driven-development.md`:
- Tool Cheat Sheet の `| Callers / impact | ... |` 行の直後に `| Classes / generic functions | \`clos-describe\` | \`symbol\` (load-system first) |` を追加
- Worker process の `- \`code-find\`, \`code-describe\`, \`code-find-references\`` を `- \`code-find\`, \`code-describe\`, \`code-find-references\`, \`clos-describe\`` に変更
- `lisp-macroexpand` splits across both の段落の直後に次の段落を追加:
  ```
  `clos-describe` splits the other way: the worker reads the classes and methods from its image,
  then the parent reads their source files for each definition's `form_type`/`form_name`.
  ```
- Tool Selection の `- Find callers/references, ...` 行の直後に `  - Class hierarchy and slots, or a generic function's methods -> \`clos-describe\` (loaded)` を追加

`CLAUDE.md` のアーキテクチャ表で `| Code Intel | ... |` 行の直後に追加:

```
| CLOS | `src/clos.lisp`, `src/clos-core.lisp`, `src/tools/clos-response-builders.lisp` | Generic function methods, class hierarchy, slots and specialized methods (clos-describe; worker reads the image, parent reads the source) |
```

- [ ] **Step 8: Lint とコミット**

```bash
mallet src/clos.lisp src/worker/handlers.lisp src/tools/all.lisp main.lisp tests/tools-test.lisp tests/worker-test.lisp
git add src/clos.lisp src/worker/handlers.lisp src/tools/all.lisp main.lisp tests/tools-test.lisp tests/worker-test.lisp docs/tools.md prompts/repl-driven-development.md CLAUDE.md
git commit -m "feat(clos-describe): expose CLOS introspection as a tool

Add the clos-describe tool and its worker method: the worker builds the
report from its image and the parent, with or without the worker pool,
annotates it from the source files and renders its text.  Document the
tool and point the prompt's tool selection at it for class hierarchies,
slots and a generic function's methods.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FwJGaMhQXB93nGDWsR7eSS"
```

---
### Task 8: `code-describe` と `inspect-object` から `clos-describe` へ案内する

**Files:**
- Modify: `src/code-core.lisp`（`generic-function-method-count` と export）
- Modify: `src/tools/response-builders.lisp`（`build-code-describe-response` と補助関数）
- Modify: `src/code.lisp`、`src/worker/handlers.lisp`（メソッド数を渡す）
- Modify: `src/inspect.lisp`（import、`%clos-hint`、`inspect-object-by-id`、`format-inspect-elements`）
- Test: `tests/response-builders-test.lisp`、`tests/code-test.lisp`、`tests/inspect-test.lisp`
- Modify: `docs/tools.md`（`code-describe` と `inspect-object` の節）

**Interfaces:**
- Consumes: Task 2 のフィクスチャ補助（`tests/code-test.lisp`）、既存の `resolve-target`、`qualified-symbol-name`
- Produces:
  - `cl-mcp/src/code-core:generic-function-method-count (symbol-name &key package) → (or null integer)`
  - `build-code-describe-response (name type arglist doc path line &key method-count)`
  - `inspect-object-by-id` の結果に `hint`（名前付きのクラス・総称関数のときだけ）

- [ ] **Step 1: 失敗するテストを書く**

`tests/response-builders-test.lisp` の `build-code-describe-response-without-doc` の直後に追加:

```lisp
(deftest build-code-describe-response-points-at-clos-describe
  (testing "a generic function's text counts its methods and names clos-describe"
    (ok (search "4 methods; clos-describe lists them with their specializers and source lines."
                (first-text (build-code-describe-response "AREA" "generic-function" "(SHAPE)"
                                                          nil nil nil :method-count 4)))))
  (testing "a class, condition or structure points at clos-describe for its structure"
    (dolist (type '("class" "condition" "structure"))
      (ok (search "clos-describe shows its slots, superclasses, subclasses and methods."
                  (first-text (build-code-describe-response "SHAPE" type "(name)" nil nil nil)))
          type)))
  (testing "a plain function does not mention clos-describe"
    (ok (not (search "clos-describe"
                     (first-text (build-code-describe-response "FOO" "function" "(X)"
                                                               nil nil nil)))))))
```

`tests/code-test.lisp` の defpackage の code-core import に `#:generic-function-method-count` を足し、ファイル末尾に追加:

```lisp
(deftest generic-function-method-count-counts-methods
  (testing "a generic function's method count, and NIL for anything else"
    (%compile-and-load-under-own-name *clos-fixture*)
    (ok (eql 4 (generic-function-method-count "cl-mcp-clos-fixture:area")))
    (ok (null (generic-function-method-count "cl-mcp-clos-fixture:circle")))
    (ok (null (generic-function-method-count "cl:car")))
    (ok (null (generic-function-method-count "cl-mcp-clos-fixture::no-such-counted-name")))
    (ok (null (find-symbol "NO-SUCH-COUNTED-NAME" "CL-MCP-CLOS-FIXTURE")))))
```

`tests/inspect-test.lisp` の defpackage の `(:import-from #:cl-mcp/src/inspect #:inspect-object-by-id)` を `#:inspect-object-by-id #:generate-result-preview` にし、`(defstruct test-point x y)` の直後に追加:

```lisp
;;; A generic function and a SETF generic function for the clos-describe hint
(defgeneric inspect-test-describe (object)
  (:method ((object test-person)) (person-name object)))

(defgeneric (setf inspect-test-label) (value object))
```

ファイル末尾に追加:

```lisp
(defun %hint-of (object)
  "Inspect OBJECT in a fresh registry and return its hint, or NIL."
  (let ((*object-registry* (make-object-registry)))
    (ht-get (inspect-object-by-id (register-object object)) "hint")))

(deftest inspect-hints-clos-describe-for-classes-and-generic-functions
  (testing "a class its symbol names"
    (let ((hint (%hint-of (find-class 'test-person))))
      (ok (search "This is the class CL-MCP/TESTS/INSPECT-TEST::TEST-PERSON" hint))
      (ok (search "clos-describe CL-MCP/TESTS/INSPECT-TEST::TEST-PERSON shows its slots" hint))))
  (testing "a generic function, and a SETF one named by its symbol"
    (ok (search "clos-describe CL-MCP/TESTS/INSPECT-TEST::INSPECT-TEST-DESCRIBE lists its methods"
                (%hint-of #'inspect-test-describe)))
    (let ((hint (%hint-of (fdefinition '(setf inspect-test-label)))))
      (ok (search "(SETF CL-MCP/TESTS/INSPECT-TEST::INSPECT-TEST-LABEL)" hint))
      (ok (search "clos-describe CL-MCP/TESTS/INSPECT-TEST::INSPECT-TEST-LABEL " hint))))
  (testing "the hint is in the content text"
    (with-fresh-registry
     (lambda ()
       (let ((response (build-inspect-response
                        (inspect-object-by-id (register-object (find-class 'test-person))))))
         (ok (search "Hint: This is the class"
                     (gethash "text" (aref (ht-get response "content") 0)))))))))

(deftest inspect-gives-no-clos-hint-for-other-objects
  (testing "instances, anonymous classes and plain functions get none"
    (ok (null (%hint-of (make-instance 'test-person :name "Ann" :age 3))))
    (ok (null (%hint-of (make-instance 'standard-class))))
    (ok (null (%hint-of #'car))))
  (testing "nor does a list holding a class, or its elements"
    (with-fresh-registry
     (lambda ()
       (let ((result (inspect-object-by-id (register-object (list (find-class 'test-person))))))
         (ok (not (nth-value 1 (gethash "hint" result))))
         (ok (notany (lambda (element)
                       (and (hash-table-p element) (nth-value 1 (gethash "hint" element))))
                     (coerce (ht-get result "elements") 'list)))))))
  (testing "nor a repl-eval result preview"
    (ok (not (nth-value 1 (gethash "hint" (generate-result-preview (find-class 'test-person))))))))
```

- [ ] **Step 2: テストが失敗することを確認する**

`<name>` を `response-builders-test`、`code-test`、`inspect-test` にして実行。
Expected: `:method-count` が不正なキーワード引数、`GENERIC-FUNCTION-METHOD-COUNT` が無い、`hint` が無い、でそれぞれ失敗。

- [ ] **Step 3: `generic-function-method-count` を足す**

`lisp-patch-form`、form_type `defpackage`、form_name `cl-mcp/src/code-core`:
- old_text: `           #:with-definition-source-cache))`
- new_text: `           #:with-definition-source-cache\n           #:generic-function-method-count))`（`\n` は実際の改行）

`lisp-edit-form` `insert_after`、form_type `defun`、form_name `code-describe-symbol`:

```lisp
(defun generic-function-method-count (symbol-name &key package)
  "Return how many methods the generic function SYMBOL-NAME names has, or NIL
when it names no generic function.  Resolved with RESOLVE-TARGET, so nothing
is interned; code-describe uses it to point at clos-describe."
  (let ((symbol (resolve-target symbol-name :package package)))
    (and symbol
         (fboundp symbol)
         (typep (fdefinition symbol) 'generic-function)
         (length (sb-mop:generic-function-methods (fdefinition symbol))))))
```

- [ ] **Step 4: `build-code-describe-response` に案内行を足す**

`lisp-edit-form` `replace`、form_type `defun`、form_name `build-code-describe-response`:

```lisp
(defun build-code-describe-response (name type arglist doc path line &key method-count)
  "Build the standard code-describe response hash-table.
The text ends with %CLOS-DESCRIBE-HINT's line for a generic function (with
METHOD-COUNT, its number of methods) or a class, since code-describe shows
neither methods nor more than a class's direct slot names."
  (let ((text (format nil "~A :: ~A~@[ ~A~]~%~@[~A~]~@[~%Defined at ~A~@[:~D~]~]"
                      name type arglist doc path line))
        (hint (%clos-describe-hint type method-count)))
    (make-ht "name" name
             "type" type
             "arglist" arglist
             "documentation" doc
             "path" path
             "line" line
             "content" (text-content (format nil "~A~@[~%~A~]" text hint)))))
```

`lisp-edit-form` `insert_before`、form_type `defun`、form_name `build-code-describe-response`:

```lisp
(defun %clos-describe-hint (type method-count)
  "Return the line pointing code-describe's reader at clos-describe for a
symbol of TYPE, or NIL.  METHOD-COUNT is a generic function's method count."
  (cond
    ((equal type "generic-function")
     (format nil "~@[~D method~:P; ~]clos-describe lists ~:[its methods~;them~] ~
with their specializers and source lines."
             method-count method-count))
    ((member type '("class" "condition" "structure") :test #'equal)
     "clos-describe shows its slots, superclasses, subclasses and methods.")))
```

（`~@[...~]` と `~@[:~D~]` を 1 つの FORMAT に続けると、PATH が NIL のとき LINE が次の指令に食われる。本文と案内行を別々に作ってから連結するのはそのため。）

- [ ] **Step 5: 呼び出し側でメソッド数を渡す**

`src/code.lisp`: defpackage の code-core import に `#:generic-function-method-count` を足す。`lisp-patch-form`、form_type `define-tool`、form_name `code-describe`:
- old_text: `      (result id (build-code-describe-response name type arglist doc path line)))))`
- new_text:

```lisp
      (result id (build-code-describe-response
                  name type arglist doc path line
                  :method-count (generic-function-method-count symbol :package package))))))
```

`src/worker/handlers.lisp`: defpackage の code-core import に `#:generic-function-method-count` を足す。`lisp-patch-form`、form_type `defun`、form_name `%handle-code-describe`:
- old_text: `      (build-code-describe-response name type arglist doc path line))))`
- new_text:

```lisp
      (build-code-describe-response
       name type arglist doc path line
       :method-count (generic-function-method-count symbol :package package)))))
```

- [ ] **Step 6: `inspect-object` に hint を足す**

`src/inspect.lisp` の defpackage に import を足す（`lisp-patch-form`、form_type `defpackage`、form_name `cl-mcp/src/inspect`、old_text `  (:import-from #:cl-mcp/src/utils/printing`、new_text は次の 3 行＋old_text）:

```lisp
  (:import-from #:cl-mcp/src/code-refs-core
                #:qualified-symbol-name)
  (:import-from #:cl-mcp/src/utils/printing
```

`lisp-edit-form` `insert_before`、form_type `defun`、form_name `inspect-object-by-id`:

```lisp
(defun %clos-hint (object)
  "Return the line pointing at clos-describe for OBJECT, or NIL.
OBJECT qualifies when it is the class its symbol names, or the generic function
its name -- a symbol or (SETF symbol) -- names.  inspect-object shows such an
object's internal representation; clos-describe describes the class or generic
function itself.  Anything else, an anonymous or replaced class included, gets
no hint, and so does an object whose check signals."
  (ignore-errors
   (cond
     ((typep object 'class)
      (let ((name (class-name object)))
        (when (and name (symbolp name) (eq (find-class name nil) object))
          (let ((qualified (qualified-symbol-name name)))
            (format nil "This is the class ~A; clos-describe ~A shows its slots, ~
superclasses, subclasses and methods with source lines."
                    qualified qualified)))))
     ((typep object 'generic-function)
      (let* ((name (sb-mop:generic-function-name object))
             (setf-p (and (consp name) (eq (first name) 'setf)))
             (base (if setf-p (second name) name)))
        (when (and base (symbolp base) (fboundp name) (eq (fdefinition name) object))
          (format nil "This is the generic function ~:[~A~;(SETF ~A)~]; clos-describe ~A ~
lists its methods with their specializers and source lines."
                  setf-p (qualified-symbol-name base) (qualified-symbol-name base))))))))
```

`lisp-patch-form`、form_type `defun`、form_name `inspect-object-by-id`:
- old_text:
```
              (setf (gethash "id" result) id)
              result)
```
- new_text:
```
              (setf (gethash "id" result) id)
              ;; Only the object asked about gets a hint, never its elements.
              (let ((hint (%clos-hint object)))
                (when hint
                  (setf (gethash "hint" result) hint)))
              result)
```

`lisp-patch-form`、form_type `defun`、form_name `format-inspect-elements`:
- old_text:
```
      (when (gethash "id" inspection-result)
        (format s "~&[object-id: ~A]" (gethash "id" inspection-result)))
```
- new_text:
```
      (when (gethash "id" inspection-result)
        (format s "~&[object-id: ~A]" (gethash "id" inspection-result)))
      (when (gethash "hint" inspection-result)
        (format s "~&Hint: ~A" (gethash "hint" inspection-result)))
```

- [ ] **Step 7: テストが通ることを確認する**

`<name>` を `response-builders-test`、`code-test`、`inspect-test`、`repl-inspect-integration-test`、`worker-test`、`spec-adapter-report-test` にして実行（最後の 1 つは `code-describe-symbol` の別の呼び出し元）。
Expected: すべて `✗` なし。

- [ ] **Step 8: 文書を更新する**

`docs/tools.md` の `## \`code-describe\`` 節の Output を次に置き換える:

```markdown
Output:
- `type` (`function`, `generic-function`, `macro`, `variable`, `class`, `condition`, `structure`)
- `arglist` (string; for a class, its direct slot names)
- `documentation` (string|null)
- `path`, `line`: where it is defined; a class, condition or structure gets its line too

The text ends with a pointer to `clos-describe` for a generic function (with its method count) or a class.
```

`## \`inspect-object\`` 節の `- \`meta\`: ...` 行の直後に追加:

```markdown
- `hint` (string, only when the object is the class its symbol names or a named generic function): points at `clos-describe`, which describes the class or generic function itself; `inspect-object` shows its internal representation. The text shows it as `Hint:`
```

- [ ] **Step 9: Lint とコミット**

```bash
mallet src/code-core.lisp src/tools/response-builders.lisp src/code.lisp src/worker/handlers.lisp src/inspect.lisp tests/response-builders-test.lisp tests/code-test.lisp tests/inspect-test.lisp
git add src/code-core.lisp src/tools/response-builders.lisp src/code.lisp src/worker/handlers.lisp src/inspect.lisp tests/response-builders-test.lisp tests/code-test.lisp tests/inspect-test.lisp docs/tools.md
git commit -m "feat(clos-describe): point code-describe and inspect-object at it

code-describe shows a generic function's lambda list and a class's direct
slot names, and inspect-object dumps a class object's internals; neither
answers what the methods or slots are.  End their text with a line naming
clos-describe for a generic function (with its method count) or a class.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01FwJGaMhQXB93nGDWsR7eSS"
```

---
### Task 9: 全体検証と実測

**Files:** 変更なし（問題が見つかった場合のみ該当ファイル）

- [ ] **Step 1: Lint（CI と同じ範囲）**

```bash
mallet src/*.lisp src/*/*.lisp tests/*.lisp
```
Expected: エラーなし。

- [ ] **Step 2: 全再コンパイルで新しい警告がないことを確認**

```bash
ros run -e '(asdf:load-asd (truename "cl-mcp.asd"))' \
        -e '(handler-bind ((warning (lambda (w) (format t "~&WARNING: ~A~%" w) (muffle-warning w)))) (asdf:compile-system :cl-mcp :force :all))' \
        -e '(uiop:quit 0)' 2>&1 | grep -i -B2 -A3 "clos\|code-core\|code-refs\|lisp-edit-form-core\|inspect\|response-builders\|handlers" | head -60
```
Expected: 今回触ったファイルに起因する WARNING / STYLE-WARNING がない（UIOP 由来の大量の redefinition 警告は既存のノイズ）。

- [ ] **Step 3: 全テストスイート**

```bash
LOG=/tmp/claude-1000/-home-wiz--roswell-local-projects-cl-ai-project-cl-mcp/b507e2ae-1ff8-4308-aa9c-f90845dc87fc/scratchpad/rove-full.log
rove cl-mcp.asd 2>&1 | tee "$LOG" | tail -30
grep -c ";; testing '" "$LOG"
grep -n "✗" "$LOG" | head -20
```
Expected: `clos-core-test` と `clos-response-builders-test` が `;; testing` に含まれる。`✗` があれば名前を列挙し、今回の変更に関係するもの（clos / code / inspect / lisp-edit-form / code-refs / response-builders / tools / worker）はすべて直す。既存の無関係な失敗（通常 2 本）は名前を報告に記録する。終了コードは根拠にしない。

- [ ] **Step 4: 実物で計測する（dogfood）**

```bash
ros run -e '(asdf:load-asd (truename "cl-mcp.asd"))' \
        -e '(handler-bind ((warning (function muffle-warning))) (asdf:load-system :cl-mcp))' \
        -e '(setf cl-mcp/src/project-root:*project-root* (truename "./"))' \
        -e '(let ((cl-mcp/src/proxy:*use-worker-pool* nil))
              (dolist (sym (list "hunchentoot:acceptor"
                                 "hunchentoot:acceptor-dispatch-request"
                                 "cl:print-object"
                                 "cl-mcp/src/utils/bounded-stream:bounded-output-stream"))
                (let* ((start (get-internal-real-time))
                       (resp (cl-mcp/src/protocol:process-json-line
                              (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"clos-describe\",\"arguments\":{\"symbol\":\"~A\"}}}" sym)))
                       (ms (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second))
                       (text (gethash "text" (elt (gethash "content" (gethash "result" (yason:parse resp))) 0))))
                  (format t "~&===== ~A: ~Dms, response ~D chars, text ~D chars~%~A~%"
                          sym ms (length resp) (length text) (subseq text 0 (min 2500 (length text)))))))' \
        -e '(uiop:quit 0)' 2>&1 | tail -150
```

Expected / 確認すること:
- 計画時の worker 実測（`print-object` 271ms、`hunchentoot:acceptor` 324ms）と同程度の所要時間
- `cl-mcp/src/http::mcp-acceptor` が `hunchentoot:acceptor` の Subclasses に出る
- `acceptor-dispatch-request` の cl-mcp 側メソッドが `src/http.lisp:449 (defmethod acceptor-dispatch-request ((acceptor mcp-acceptor) request))` の形で先頭に出る（project ファイルが先）
- `bounded-output-stream` の `code-find`（`src/utils/bounded-stream.lisp:21`）と同じ行、`sb-gray` のメソッドが `stream-write-char ((stream bounded-output-stream) character)` の form_name 付きで出る
- hunchentoot の `acceptor.lisp` は `#.` を含むので `[file could not be parsed: ...]` になる（既知。path:line は出る）
- 結果（時間、サイズ、気付いた問題）を報告に書く。応答が極端に大きい・遅い場合は問題として報告する（その場で設計を変えない）

- [ ] **Step 5: 報告**

実行したコマンドと結果（Lint、警告、テストの ✓ 数と `;; testing` 数と ✗ の名前、dogfood の計測値）をまとめる。コミットは不要（問題を直した場合のみ、直したファイルをパス指定でコミット）。
