# code-find-references 影響範囲分析 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `code-find-references` が、呼び出し元フォームごとの正確な呼び出し行、xref に記録されないトップレベル使用、テスト名を返し、存在しないシンボルを intern せずに理由付きで報告するようにする。

**Architecture:** 親プロセス（`src/code-refs-scan.lisp`）がプロジェクトの Lisp ファイルを CST で走査して名前が一致する位置を集め、worker（`src/code-refs-core.lisp`）がそれを `find-symbol` で解決して SBCL xref と (truename, トップレベルフォーム番号) で突き合わせる。結果は `build-code-find-references-response` が本文テキストに整形する。

**Tech Stack:** SBCL, sb-introspect, eclector（親のみ）, yason, Rove

**Spec:** `docs/superpowers/specs/2026-09-14-code-find-references-impact-design.md`

## Global Constraints

- SBCL 専用（`project_sbcl_only`）。他処理系の可搬性は考えない
- Google Common Lisp Style Guide: 2 スペースインデント、1 行 100 桁以内、トップレベルフォーム間に空行、公開関数には docstring
- **worker は eclector に依存しない**: `src/code-refs-core.lisp` は `cl-mcp/src/cst` / `lisp-edit-form-core` / `utils/clgrep` を import してはならない
- 本番コードで `intern` / `eval` / `read-from-string` によるシンボル解決をしない（`find-package` と `find-symbol` のみ）
- JSON 配列は組み立て時に vector で作る。読む側は `sequence->list` を通して list と vector の両方を受け付ける（worker 経由の JSON は list になる）
- 走査結果（scan）のペイロードには JSON の真偽値を入れない（in-process では `yason:false`、worker 経由では `NIL` になって食い違うため）。null か数値か文字列で表す
- Rove で condition を検査するときは `signals` ではなく `handler-case` で包む
- package-inferred-system: `cl-mcp.asd` は編集しない。新しいテストファイルはルートの `tests.lisp` の `defpackage` に `(:import-from #:cl-mcp/tests/<name>)` を足して登録する
- Lisp ソースの編集は cl-mcp の `lisp-edit-form` / `lisp-patch-form`、新規ファイルは `fs-write-file` を使う。稼働中の MCP サーバーの親イメージが古くて対象ファイルを解析できない場合に限り Edit/Write を使い、直後に `lisp-check-parens` と `mallet` で確認する
- コミットは変更したファイルをパス指定で `git add` する。`src/specs/random-spec*.lisp` と `coverage/` は無関係なので絶対に add しない
- コミットメッセージの末尾に次の 2 行を付ける:
  ```
  Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_016jJhPXZ53FRPgqhA3aT8m9
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
| `src/code-core.lisp`（変更） | 両方 | `%offset->line` のバイト位置修正。xref エントリの収集、`code-find-references-report`、互換ラッパー `code-find-references` |
| `src/code-refs-core.lisp`（新規） | worker | intern しないシンボル解析・解決、走査位置の判定、xref とのマージ、ペイロード組み立て。ファイルを読まない純粋ロジック |
| `src/code-refs-scan.lisp`（新規） | 親 | プロジェクトファイルの列挙、CST 走査、位置の分類、囲むフォームの情報 |
| `src/tools/response-builders.lisp`（変更） | 両方 | `build-code-find-references-response` を report を受け取る形に変え、本文テキストを整形 |
| `src/code.lisp`（変更） | 親 | tool 定義: `limit` 引数、走査の実行、description |
| `src/worker/handlers.lisp`（変更） | worker | `%handle-code-find-references` が scan と limit を渡す |
| `tests/fixtures/xref-fixture.lisp`（新規） | テスト | 実際の xref を記録させるフィクスチャ |
| `tests/code-refs-core-test.lisp`（新規） | テスト | code-refs-core の単体テスト |
| `tests/code-refs-scan-test.lisp`（新規） | テスト | code-refs-scan の単体テスト |
| `tests/code-test.lisp` / `response-builders-test.lisp` / `tools-test.lisp` / `worker-test.lisp`（変更） | テスト | 統合・整形・ツール・worker ハンドラ |
| `tests.lisp`（変更） | テスト | 新テストファイルの登録 |
| `docs/tools.md` / `prompts/repl-driven-development.md` / `CLAUDE.md`（変更） | 文書 | ツールの説明 |

依存の向き: `code.lisp → code-refs-scan → code-refs-core`、`code.lisp → code-core → code-refs-core`、`handlers → code-core`。循環はない。

---

### Task 1: `%offset->line` がバイト位置を正しく読む

SBCL の `definition-source-character-offset`（とデバッグソースの start positions）は UTF-8 のバイト位置で、`%offset->line` はそれを文字位置として読んでいる。日本語コメントより後ろの定義で `code-find` / `code-describe` / `code-find-references` / フレームの行番号がずれる。

**Files:**
- Modify: `src/code-core.lisp`（`%offset->line` の直前に関数を追加し、`%offset->line` の `start` 計算と docstring 先頭を変更）
- Test: `tests/code-test.lisp`（`code-offset-to-line-skips-reader-conditionals` の直後に 2 テスト追加）

**Interfaces:**
- Consumes: なし
- Produces: `cl-mcp/src/code-core::%byte-offset->char-offset (pathname byte-offset) → integer`。`%offset->line` のシグネチャは不変（OFFSET はバイト位置として解釈される）

- [ ] **Step 1: 失敗するテストを書く**

`tests/code-test.lisp` の `code-offset-to-line-skips-reader-conditionals` の直後に `lisp-edit-form`（`insert_after`, `form_type: "deftest"`, `form_name: "code-offset-to-line-skips-reader-conditionals"`）で追加する:

```lisp
(deftest code-offset-to-line-counts-octets
  (testing "%offset->line reads SBCL's octet offset after a multibyte comment"
    (let* ((tmp (uiop:merge-pathnames*
                 (format nil "cl-mcp-offset-octets-~A.lisp" (get-universal-time))
                 (uiop:temporary-directory)))
           (path (namestring tmp))
           (text (format nil "(in-package :cl-user)~%~
                              ;; 日本語のコメントでバイト数と文字数がずれる~%~
                              (defun before-mb () :ok)~%~
                              ~%~
                              (defun after-mb () :ok)~%~
                              (defun filler-1 () :ok)~%~
                              (defun filler-2 () :ok)~%~
                              (defun filler-3 () :ok)~%")))
      (unwind-protect
           (progn
             (with-open-file (s path :direction :output :if-exists :supersede
                                     :external-format :utf-8)
               (write-string text s))
             (let* ((char-pos (search "(defun after-mb" text))
                    ;; SBCL records the octet position just past the previous
                    ;; form, i.e. at the whitespace before this one.
                    (octet-pos (length (sb-ext:string-to-octets
                                        text :end (1- char-pos)
                                        :external-format :utf-8))))
               (ok (= (1+ (count #\Newline text :end char-pos))
                      (cl-mcp/src/code-core::%offset->line path octet-pos))
                   "the defun after the comment is reported on its own line")))
        (ignore-errors (delete-file path))))))

(deftest code-find-definition-line-after-multibyte-comment
  (testing "code-find-definition reports the defun's own line in a UTF-8 file"
    (let* ((dir (uiop:ensure-directory-pathname
                 (uiop:merge-pathnames* (format nil "cl-mcp-octets-~D/" (random 1000000))
                                        (uiop:temporary-directory))))
           (src (merge-pathnames "octets.lisp" dir))
           (fasl (merge-pathnames "octets.fasl" dir))
           (text (format nil "(defpackage #:cl-mcp-octets-fixture (:use #:cl))~%~
                              (in-package #:cl-mcp-octets-fixture)~%~
                              ;; 日本語のコメントでバイト数と文字数がずれる~%~
                              (defun one () 1)~%~
                              (defun two () 2)~%~
                              (defun three () 3)~%~
                              (defun four () 4)~%")))
      (ensure-directories-exist dir)
      (unwind-protect
           (progn
             (with-open-file (s src :direction :output :if-exists :supersede
                                    :external-format :utf-8)
               (write-string text s))
             (handler-bind ((warning #'muffle-warning))
               (load (compile-file src :output-file fasl :verbose nil :print nil)))
             (ok (eql (1+ (count #\Newline text :end (search "(defun three" text)))
                      (nth-value 1 (code-find-definition "cl-mcp-octets-fixture::three")))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))
```

- [ ] **Step 2: 失敗を確認する**

共通コマンドを `<name>` = `code-test` で実行。
Expected: 上の 2 テストが `✗`（行番号が 1 行以上ずれる）。既存テストは `✓`。

- [ ] **Step 3: 実装する**

`src/code-core.lisp` の `%offset->line` の直前に `lisp-edit-form`（`insert_before`, `form_type: "defun"`, `form_name: "%offset->line"`）で追加:

```lisp
(defun %byte-offset->char-offset (pathname byte-offset)
  "Return the character offset in PATHNAME that BYTE-OFFSET corresponds to.

SBCL records source positions (DEFINITION-SOURCE-CHARACTER-OFFSET, a debug
source's start positions) as FILE-POSITION values, and FILE-POSITION on a
UTF-8 character stream counts octets.  Where a multibyte character -- a
Japanese comment, say -- precedes a definition, the recorded value exceeds the
character offset, and reading it as one lands lines too far down.  The prefix
is decoded as UTF-8, the external format sources are compiled with by default;
a malformed prefix decodes with replacement characters, and any failure
returns BYTE-OFFSET unchanged, which is exact for ASCII text."
  (handler-case
      (with-open-file (in pathname :element-type '(unsigned-byte 8))
        (let* ((count (min (max byte-offset 0) (file-length in)))
               (octets (make-array count :element-type '(unsigned-byte 8))))
          (read-sequence octets in)
          (length (sb-ext:octets-to-string
                   octets :external-format '(:utf-8 :replacement #\?)))))
    (error () byte-offset)))
```

`lisp-patch-form`（`form_type: "defun"`, `form_name: "%offset->line"`）で 2 か所置換:

- old_text: `"Convert character OFFSET within PATHNAME to a 1-based line number.`
  new_text: `"Convert SBCL's source OFFSET within PATHNAME to a 1-based line number.\nOFFSET is an octet position; see %BYTE-OFFSET->CHAR-OFFSET.`（`\n` は実際の改行）
- old_text: `(start (min (max offset 0) len))`
  new_text: `(start (min (max (%byte-offset->char-offset physical offset) 0) len))`

- [ ] **Step 4: 成功を確認する**

`code-test` を再実行。Expected: 追加の 2 テストを含め `code-test` 全体が `✓`（既存の `code-offset-to-line-skips-reader-conditionals` は ASCII なので不変）。

- [ ] **Step 5: Lint とコミット**

```bash
mallet src/code-core.lisp tests/code-test.lisp
git add src/code-core.lisp tests/code-test.lisp
git commit -m "fix(code): read SBCL source offsets as octets

A definition after a multibyte comment was reported lines too far down by
code-find, code-describe and code-find-references.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016jJhPXZ53FRPgqhA3aT8m9"
```

---

### Task 2: `code-refs-core` — intern しないシンボル解析と解決

**Files:**
- Create: `src/code-refs-core.lisp`
- Create: `tests/code-refs-core-test.lisp`
- Modify: `tests.lisp`（`(:import-from #:cl-mcp/tests/code-test)` の直後に登録）

**Interfaces:**
- Consumes: `cl-mcp/src/tools/helpers`: `make-ht`, `json-bool`, `arg-validation-error`
- Produces（パッケージ `cl-mcp/src/code-refs-core` から export）:
  - `(sequence->list sequence) → list`
  - `(parse-symbol-text text) → (values name package-part problem)`: name/package-part は文字列、`:foo` の package-part は `"KEYWORD"`、不正時は name と package-part が NIL で problem が文章
  - `(parse-target-designator text) → (values name package-part)`: 不正またはキーワードなら `arg-validation-error`（`:arg-name "symbol"`）
  - `(find-package-named name) → package or nil`
  - `(resolve-target text &key package) → (values symbol status package-name name)`: status は `:found` / `:not-found` / `:package-not-found`
  - `(qualified-symbol-name symbol) → string`: `PKG:NAME` / `PKG::NAME` / `:NAME` / `#:NAME`
  - `(symbol-kind symbol) → string`: `"special-operator"` `"macro"` `"generic-function"` `"function"` `"constant"` `"variable"` `"unbound"`
  - `(resolve-site-token token in-package target) → :match | :other | (values :unresolved missing-package-name)`

- [ ] **Step 1: 失敗するテストを書く**

`fs-write-file` で `tests/code-refs-core-test.lisp` を作る:

```lisp
;;;; tests/code-refs-core-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/code-refs-core: reading a symbol as written
;;;; without interning it, and deciding which scan sites name a symbol.

(defpackage #:cl-mcp/tests/code-refs-core-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/helpers
                #:arg-validation-error)
  (:import-from #:cl-mcp/src/code-refs-core
                #:parse-symbol-text
                #:parse-target-designator
                #:resolve-target
                #:qualified-symbol-name
                #:symbol-kind
                #:resolve-site-token))

(in-package #:cl-mcp/tests/code-refs-core-test)

;;; Packages with known contents, so resolution is checked against them
;;; rather than against whatever the image happens to hold.

(defpackage #:cl-mcp-refs-core-a
  (:use #:cl)
  (:export #:shared)
  (:intern #:inner))

(defpackage #:cl-mcp-refs-core-b
  (:use #:cl)
  (:intern #:shared))

(defpackage #:cl-mcp-refs-core-c
  (:use #:cl)
  (:local-nicknames (#:aa #:cl-mcp-refs-core-a)))

(defpackage #:cl-mcp-refs-core-d
  (:use #:cl #:cl-mcp-refs-core-a))

(defun %validation-error-p (thunk)
  "True when calling THUNK signals ARG-VALIDATION-ERROR."
  (handler-case (progn (funcall thunk) nil)
    (arg-validation-error () t)))

(deftest parse-symbol-text-splits-and-upcases
  (testing "unqualified, single and double colon, escapes and keywords"
    (flet ((parts (text) (multiple-value-list (parse-symbol-text text))))
      (ok (equal '("FOO" nil nil) (parts "foo")))
      (ok (equal '("FOO" "PKG" nil) (parts "pkg:foo")))
      (ok (equal '("FOO" "PKG" nil) (parts "pkg::foo")))
      (ok (equal '("Foo" nil nil) (parts "|Foo|")))
      (ok (equal '("aB" "P" nil) (parts "p::\\a|B|")))
      (ok (equal '("FOO" "KEYWORD" nil) (parts ":foo")))
      (ok (equal '("CL-MCP/SRC/X" nil nil) (parts "  cl-mcp/src/x "))))))

(deftest parse-symbol-text-reports-malformed-input
  (testing "text that is not a symbol name comes back with a reason"
    (flet ((problem (text) (nth-value 2 (parse-symbol-text text))))
      (ok (stringp (problem "")))
      (ok (stringp (problem "   ")))
      (ok (stringp (problem "pkg:")))
      (ok (stringp (problem "a:b:c")))
      (ok (stringp (problem "a:::b")))
      (ok (stringp (problem "#:foo")))
      (ok (stringp (problem "|open")))
      (ok (stringp (problem "foo\\")))
      (ok (null (nth-value 0 (parse-symbol-text "a:b:c")))))))

(deftest parse-target-designator-signals-validation-errors
  (testing "keywords and malformed text are argument errors"
    (ok (%validation-error-p (lambda () (parse-target-designator ":foo"))))
    (ok (%validation-error-p (lambda () (parse-target-designator ""))))
    (ok (equal '("FOO" "PKG") (multiple-value-list (parse-target-designator "pkg::foo"))))))

(deftest resolve-target-finds-without-interning
  (testing "found, internal through one colon, not found, missing package"
    (multiple-value-bind (symbol status package-name name)
        (resolve-target "cl-mcp-refs-core-a:shared")
      (ok (eq :found status))
      (ok (eq symbol (find-symbol "SHARED" "CL-MCP-REFS-CORE-A")))
      (ok (equal "CL-MCP-REFS-CORE-A" package-name))
      (ok (equal "SHARED" name)))
    (ok (eq :found (nth-value 1 (resolve-target "cl-mcp-refs-core-a:inner")))
        "a single colon reaches an internal symbol")
    (ok (eq :found (nth-value 1 (resolve-target "shared" :package "cl-mcp-refs-core-a"))))
    (ok (eq :found (nth-value 1 (resolve-target "aa:shared" :package "cl-mcp-refs-core-c")))
        "the package argument's local nicknames apply")
    (multiple-value-bind (symbol status package-name name)
        (resolve-target "cl-mcp-refs-core-a::never-interned-xyz")
      (ok (null symbol))
      (ok (eq :not-found status))
      (ok (equal "CL-MCP-REFS-CORE-A" package-name))
      (ok (equal "NEVER-INTERNED-XYZ" name)))
    (ok (null (nth-value 1 (find-symbol "NEVER-INTERNED-XYZ" "CL-MCP-REFS-CORE-A")))
        "looking a symbol up must not intern it")
    (ok (eq :package-not-found (nth-value 1 (resolve-target "no-such-package-xyz::foo"))))
    (ok (eq :package-not-found
            (nth-value 1 (resolve-target "foo" :package "no-such-package-xyz"))))
    (ok (null (find-package "NO-SUCH-PACKAGE-XYZ")))))

(deftest qualified-symbol-name-follows-the-reader
  (testing "external, internal, keyword and uninterned"
    (ok (equal "CL-MCP-REFS-CORE-A:SHARED"
               (qualified-symbol-name (find-symbol "SHARED" "CL-MCP-REFS-CORE-A"))))
    (ok (equal "CL-MCP-REFS-CORE-A::INNER"
               (qualified-symbol-name (find-symbol "INNER" "CL-MCP-REFS-CORE-A"))))
    (ok (equal ":TEST" (qualified-symbol-name :test)))
    (ok (equal "#:LOOSE" (qualified-symbol-name (make-symbol "LOOSE"))))))

(deftest symbol-kind-names-what-a-symbol-denotes
  (testing "most specific kind first"
    (ok (equal "special-operator" (symbol-kind 'if)))
    (ok (equal "macro" (symbol-kind 'deftest)))
    (ok (equal "generic-function" (symbol-kind 'print-object)))
    (ok (equal "function" (symbol-kind 'parse-symbol-text)))
    (ok (equal "constant" (symbol-kind 'most-positive-fixnum)))
    (ok (equal "variable" (symbol-kind '*package*)))
    (ok (equal "unbound" (symbol-kind (find-symbol "INNER" "CL-MCP-REFS-CORE-A"))))))

(deftest resolve-site-token-judges-each-spelling
  (testing "inherited, nicknamed, other symbol, and missing packages"
    (let ((shared (find-symbol "SHARED" "CL-MCP-REFS-CORE-A")))
      (ok (eq :match (resolve-site-token "shared" "CL-MCP-REFS-CORE-A" shared)))
      (ok (eq :match (resolve-site-token "shared" "cl-mcp-refs-core-d" shared))
          "inherited through :use, designator written in lower case")
      (ok (eq :match (resolve-site-token "aa:shared" "CL-MCP-REFS-CORE-C" shared))
          "package-local nickname of the site's package")
      (ok (eq :other (resolve-site-token "shared" "CL-MCP-REFS-CORE-B" shared))
          "same name, different symbol")
      (ok (eq :other (resolve-site-token "shared" "CL-MCP-REFS-CORE-C" shared))
          "absent from the site's package")
      (multiple-value-bind (verdict missing)
          (resolve-site-token "shared" "NOT-LOADED-PKG-XYZ" shared)
        (ok (eq :unresolved verdict))
        (ok (equal "NOT-LOADED-PKG-XYZ" missing)))
      (multiple-value-bind (verdict missing)
          (resolve-site-token "nope-xyz:shared" "CL-MCP-REFS-CORE-A" shared)
        (ok (eq :unresolved verdict))
        (ok (equal "NOPE-XYZ" missing)))
      (ok (null (nth-value 1 (find-symbol "SHARED" "CL-MCP-REFS-CORE-C")))
          "resolution must not intern into the site's package"))))
```

`tests.lisp` の `defpackage` で `(:import-from #:cl-mcp/tests/code-test)` の直後に `(:import-from #:cl-mcp/tests/code-refs-core-test)` を追加する（`lisp-patch-form`, `form_type: "defpackage"`, `form_name: "cl-mcp/tests"`）。

- [ ] **Step 2: 失敗を確認する**

`<name>` = `code-refs-core-test`。Expected: `cl-mcp/src/code-refs-core` が存在しないのでロードエラー。

- [ ] **Step 3: 実装する**

`fs-write-file` で `src/code-refs-core.lisp` を作る:

```lisp
;;;; src/code-refs-core.lisp
;;;;
;;;; Worker-side half of code-find-references' impact analysis.  It turns a
;;;; symbol as written into a symbol without interning anything, decides
;;;; which of the parent's source-scan sites really name that symbol in this
;;;; image, and merges those sites with SBCL's xref entries.  It reads no
;;;; files and does not depend on eclector, so the worker image stays free of
;;;; the parent's parsing stack.

(defpackage #:cl-mcp/src/code-refs-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:json-bool
                #:arg-validation-error)
  (:export #:sequence->list
           #:parse-symbol-text
           #:parse-target-designator
           #:find-package-named
           #:resolve-target
           #:qualified-symbol-name
           #:symbol-kind
           #:resolve-site-token))

(in-package #:cl-mcp/src/code-refs-core)

(defun sequence->list (sequence)
  "Return SEQUENCE as a list.
A JSON array is a vector when the parent's scan is used in-process and a list
after it crosses the worker's JSON parser; everything that walks scan data
goes through this so the two paths cannot diverge."
  (coerce (or sequence '()) 'list))

(defun parse-symbol-text (text)
  "Split TEXT, a symbol as it would be written in source, without reading it.

Returns (values NAME PACKAGE-PART PROBLEM).  NAME and PACKAGE-PART follow the
standard reader: unescaped characters are upcased, characters inside |...| or
after a backslash are kept as written, and one or two colons separate the
package from the name.  PACKAGE-PART is NIL for an unqualified symbol and
\"KEYWORD\" for :NAME.  When TEXT is not a symbol name, NAME and PACKAGE-PART
are NIL and PROBLEM is a sentence saying why."
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))
         (length (length text))
         (buffer (make-string-output-stream))
         (package-part nil)
         (first-marker nil)
         (marker-end nil)
         (in-bars nil)
         (i 0))
    (flet ((fail (control &rest args)
             (return-from parse-symbol-text
               (values nil nil (apply #'format nil control args)))))
      (when (zerop length)
        (fail "symbol must be a non-empty string"))
      (when (and (> length 1) (string= "#:" (subseq text 0 2)))
        (fail "~A is an uninterned symbol; it has no references to find" text))
      (loop while (< i length)
            do (let ((ch (char text i)))
                 (cond
                   ((char= ch #\|)
                    (setf in-bars (not in-bars)))
                   (in-bars
                    (write-char ch buffer))
                   ((char= ch #\\)
                    (incf i)
                    (when (>= i length)
                      (fail "~A ends with an escaping backslash" text))
                    (write-char (char text i) buffer))
                   ((char= ch #\:)
                    (cond
                      ((null package-part)
                       (setf package-part (get-output-stream-string buffer)
                             first-marker i
                             marker-end (1+ i)))
                      ((and (= i marker-end) (= i (1+ first-marker)))
                       (setf marker-end (1+ i)))
                      (t
                       (fail "~A has more than one package marker" text))))
                   (t
                    (write-char (char-upcase ch) buffer))))
               (incf i))
      (when in-bars
        (fail "~A has an unterminated |" text))
      (let ((name (get-output-stream-string buffer)))
        (when (zerop (length name))
          (fail "~A has no symbol name" text))
        (values name
                (cond ((null package-part) nil)
                      ((zerop (length package-part)) "KEYWORD")
                      (t package-part))
                nil)))))

(defun parse-target-designator (text)
  "Return (values NAME PACKAGE-PART) for TEXT, the symbol a caller asked about.
Signals ARG-VALIDATION-ERROR naming the \"symbol\" argument when TEXT is not a
symbol name, or is a keyword, which no code references in the xref sense."
  (multiple-value-bind (name package-part problem) (parse-symbol-text text)
    (cond
      (problem
       (error 'arg-validation-error :arg-name "symbol" :message problem))
      ((equal package-part "KEYWORD")
       (error 'arg-validation-error
              :arg-name "symbol"
              :message (format nil "~A is a keyword; keywords have no references to find"
                               text)))
      (t (values name package-part)))))

(defun find-package-named (name)
  "Return the package NAME designates, trying NAME as given and then upcased.
FIND-PACKAGE consults the package-local nicknames of *PACKAGE*, so bind
*PACKAGE* to the package the name was written in before calling this."
  (and (stringp name)
       (plusp (length name))
       (or (find-package name)
           (find-package (string-upcase name)))))

(defun resolve-target (text &key package)
  "Resolve TEXT to a symbol using FIND-PACKAGE and FIND-SYMBOL only.

PACKAGE (a name) is used for an unqualified TEXT and defaults to
COMMON-LISP-USER; its package-local nicknames apply to a qualified one.  A
single colon is accepted for an internal symbol: the question is where a
symbol is used, not whether it is exported.

Returns (values SYMBOL STATUS PACKAGE-NAME NAME), STATUS being :FOUND,
:NOT-FOUND or :PACKAGE-NOT-FOUND.  PACKAGE-NAME and NAME say where the lookup
happened, for the message shown when it fails.  Nothing is interned."
  (multiple-value-bind (name package-part) (parse-target-designator text)
    (let* ((given (and (stringp package) (plusp (length package)) package))
           (context (or (and given (find-package-named given))
                        (find-package "COMMON-LISP-USER")))
           (home (cond (package-part
                        (let ((*package* context))
                          (find-package-named package-part)))
                       (given (find-package-named given))
                       (t context))))
      (if (null home)
          (values nil :package-not-found (or package-part given) name)
          (multiple-value-bind (symbol status) (find-symbol name home)
            (if status
                (values symbol :found (package-name home) name)
                (values nil :not-found (package-name home) name)))))))

(defun qualified-symbol-name (symbol)
  "Return SYMBOL's name qualified the way a reader outside its package needs:
PKG:NAME when external, PKG::NAME when internal, :NAME for a keyword and
#:NAME for an uninterned symbol.  The package's primary name is used."
  (let ((package (symbol-package symbol))
        (name (symbol-name symbol)))
    (cond
      ((null package) (format nil "#:~A" name))
      ((eq package (find-package "KEYWORD")) (format nil ":~A" name))
      (t (format nil "~A~A~A"
                 (package-name package)
                 (if (eq (nth-value 1 (find-symbol name package)) :external) ":" "::")
                 name)))))

(defun symbol-kind (symbol)
  "Return a word for what SYMBOL names in this image, most specific first:
special-operator, macro, generic-function, function, constant, variable or
unbound."
  (cond
    ((special-operator-p symbol) "special-operator")
    ((macro-function symbol) "macro")
    ((and (fboundp symbol) (typep (fdefinition symbol) 'generic-function))
     "generic-function")
    ((fboundp symbol) "function")
    ((constantp symbol) "constant")
    ((or (boundp symbol)
         (eq (sb-int:info :variable :kind symbol) :special))
     "variable")
    (t "unbound")))

(defun resolve-site-token (token in-package target)
  "Decide whether TOKEN, written where IN-PACKAGE was current, names TARGET.

IN-PACKAGE is the designator of the IN-PACKAGE in effect at the site; NIL means
COMMON-LISP-USER.  Package-local nicknames of that package apply to a
qualified TOKEN.  Returns :MATCH, :OTHER (TOKEN names another symbol, or none),
or :UNRESOLVED with the missing package's name as a second value when the site
cannot be judged because a package does not exist in this image.  Nothing is
interned."
  (multiple-value-bind (name package-part problem) (parse-symbol-text token)
    (if problem
        :other
        (let* ((home-name (or in-package "COMMON-LISP-USER"))
               (home (find-package-named home-name)))
          (if (null home)
              (values :unresolved home-name)
              (let ((package (if package-part
                                 (let ((*package* home))
                                   (find-package-named package-part))
                                 home)))
                (if (null package)
                    (values :unresolved package-part)
                    (multiple-value-bind (symbol status) (find-symbol name package)
                      (if (and status (eq symbol target)) :match :other)))))))))
```

注: `make-ht` と `json-bool` は Task 4 で使う。この時点で未使用 import の警告が出ても Task 4 で解消する（mallet が未使用 import をエラーにする場合は、この 2 つを Task 4 で import に追加する形にしてよい）。

- [ ] **Step 4: 成功を確認する**

`code-refs-core-test` を実行。Expected: 全テスト `✓`。

- [ ] **Step 5: Lint とコミット**

```bash
mallet src/code-refs-core.lisp tests/code-refs-core-test.lisp
git add src/code-refs-core.lisp tests/code-refs-core-test.lisp tests.lisp
git commit -m "feat(code-refs): resolve symbols as written without interning

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016jJhPXZ53FRPgqhA3aT8m9"
```

---

### Task 3: `code-refs-scan` — 親側の CST 走査

**Files:**
- Create: `src/code-refs-scan.lisp`
- Create: `tests/code-refs-scan-test.lisp`
- Modify: `tests.lisp`（`code-refs-core-test` の直後に `(:import-from #:cl-mcp/tests/code-refs-scan-test)`）

**Interfaces:**
- Consumes: `cl-mcp/src/code-refs-core`: `parse-target-designator`, `find-package-named`。`cl-mcp/src/cst`: `cst-node-kind` `cst-node-value` `cst-node-children` `cst-node-start` `cst-node-end` `cst-node-start-line` `cst-node-end-line` `parse-top-level-forms` `%in-package-form-p`。`cl-mcp/src/lisp-edit-form-core:%definition-candidates`。`cl-mcp/src/utils/clgrep:collect-target-files`。`cl-mcp/src/utils/paths:normalize-path-for-display`。`cl-mcp/src/project-root:*project-root*`
- Produces（`cl-mcp/src/code-refs-scan` から export）:
  - `*max-scan-sites*`（既定 5000）
  - `(target-name-from-designator designator) → string`（不正なら `arg-validation-error`）
  - `(scan-text text target-name &key path abs-path max-sites) → (values forms site-count truncated-p)`: forms は hash-table の list。キー: `"path" "abs_path" "index" "start_line" "end_line" "form_type" "form_name" "test_name" "test_framework" "in_package" "context" "sites"`。`"sites"` は vector で、各要素のキー: `"line" "column" "kind" "token" "context" "shadowed_by"`
  - `(scan-project designator &key root max-sites) → hash-table`。キー: `"target_name" "root" "files_scanned" "files_matched" "forms"(vector) "parse_failures"(vector of {"path" "abs_path" "error"}) "truncated_at" "skipped_reason"`

- [ ] **Step 1: 失敗するテストを書く**

`fs-write-file` で `tests/code-refs-scan-test.lisp`:

```lisp
;;;; tests/code-refs-scan-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/code-refs-scan: finding where a symbol name is
;;;; written, classifying the position, and describing the enclosing form.

(defpackage #:cl-mcp/tests/code-refs-scan-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/helpers
                #:arg-validation-error)
  (:import-from #:cl-mcp/src/code-refs-scan
                #:target-name-from-designator
                #:scan-text
                #:scan-project))

(in-package #:cl-mcp/tests/code-refs-scan-test)

(defun %sites (text name)
  "Return (kind line shadowed-by token) for every site of NAME in TEXT."
  (loop for form in (scan-text text name)
        append (loop for site across (gethash "sites" form)
                     collect (list (gethash "kind" site)
                                   (gethash "line" site)
                                   (gethash "shadowed_by" site)
                                   (gethash "token" site)))))

(defun %kinds (text name)
  "Return the kind of every site of NAME in TEXT, in source order."
  (mapcar #'first (%sites text name)))

(deftest scan-text-classifies-positions
  (testing "call, function, quoted, reference, bind and set"
    (ok (equal '("call") (%kinds "(defun a () (foo 1))" "FOO")))
    (ok (equal '("function") (%kinds "(defun a () (mapcar #'foo xs))" "FOO")))
    (ok (equal '("function") (%kinds "(defun a () (mapcar (function foo) xs))" "FOO")))
    (ok (equal '("quoted") (%kinds "(defun a () (funcall 'foo))" "FOO")))
    (ok (equal '("quoted" "quoted") (%kinds "(defun a () '(foo (foo)))" "FOO")))
    (ok (equal '("reference") (%kinds "(defun a () (list foo))" "FOO")))
    (ok (equal '("bind" "reference") (%kinds "(defun a () (let ((foo 1)) foo))" "FOO")))
    (ok (equal '("bind") (%kinds "(defun a (foo) nil)" "FOO")))
    (ok (equal '("set" "set") (%kinds "(defun a () (setf foo 1) (setq foo 2))" "FOO")))
    (ok (equal '("call") (%kinds "(defun a () (setf (foo x) 1))" "FOO")))
    (ok (equal '("call") (%kinds "(defun a () (funcall #'(lambda (x) (foo x)) 1))" "FOO")))
    (ok (equal '("reference") (%kinds "(defclass a (foo) ())" "FOO")))))

(deftest scan-text-labels-backquote-templates
  (testing "a template is labelled, an unquoted island is ordinary code"
    (ok (equal '("template" "call") (%kinds "(defmacro m (x) `(foo ,(foo x)))" "FOO")))))

(deftest scan-text-excludes-definitions-and-packages
  (testing "definition names, package forms, keywords, strings and comments"
    (ok (null (%kinds "(defun foo () 1)" "FOO")))
    (ok (null (%kinds "(defvar foo 1)" "FOO")))
    (ok (null (%kinds "(defpackage #:p (:export #:foo foo))" "FOO")))
    (ok (null (%kinds "(defun a () :foo)" "FOO")) "keyword")
    (ok (null (%kinds "(defun a () (list '#:foo))" "FOO")) "uninterned")
    (ok (null (%kinds (format nil "(defun a () \"foo\") ; foo~%") "FOO")) "string and comment")
    (ok (equal '("call") (%kinds "(defun foo (n) (foo (1- n)))" "FOO"))
        "a recursive call is still a call")
    (ok (equal '("method") (%kinds "(defmethod foo ((x integer)) x)" "FOO")))))

(deftest scan-text-marks-flet-shadowing
  (testing "sites below a flet binding the name carry the operator"
    (let ((sites (%sites "(defun a () (flet ((foo (x) (foo x))) (foo 1)))" "FOO")))
      (ok (equal '("call" "call") (mapcar #'first sites))
          "the binding name itself is not a site")
      (ok (every (lambda (site) (equal "flet" (third site))) sites))))
  (testing "an flet of another name shadows nothing"
    (ok (equal '(nil) (mapcar #'third (%sites "(defun a () (flet ((bar () 1)) (foo)))" "FOO"))))))

(deftest scan-text-records-enclosing-form
  (testing "index, lines, type, name, test and in-package of each form"
    (let* ((text (format nil "(in-package #:p1)~%~
                              (defun a ()~%~
                              ~2@T(foo))~%~
                              #+sbcl~%~
                              (defmethod b ((x integer))~%~
                              ~2@T(foo))~%~
                              (in-package :p2)~%~
                              (deftest c-test~%~
                              ~2@T(foo))~%~
                              (eval-when (:execute) (foo))~%"))
           (forms (scan-text text "FOO")))
      (flet ((field (i key) (gethash key (nth i forms))))
        (ok (= 4 (length forms)))
        (ok (equal '(1 2 4 5) (mapcar (lambda (form) (gethash "index" form)) forms))
            "indexes count every top-level expression, in-package forms included")
        (ok (equal "defun" (field 0 "form_type")))
        (ok (equal "a" (field 0 "form_name")))
        (ok (equal "P1" (field 0 "in_package")))
        (ok (null (field 0 "test_name")))
        (ok (equal "b ((x integer))" (field 1 "form_name")))
        (ok (= 4 (field 1 "start_line")) "a reader conditional starts the form")
        (ok (= 6 (field 1 "end_line")))
        (ok (equal "c-test" (field 2 "test_name")))
        (ok (equal "rove" (field 2 "test_framework")))
        (ok (equal "P2" (field 2 "in_package")))
        (ok (equal "eval-when" (field 3 "form_type")))
        (ok (null (field 3 "form_name")))))))

(deftest scan-text-keeps-token-column-and-context
  (testing "the token as written, its 1-based column and its line"
    (let* ((forms (scan-text (format nil "(defun a ()~%  (fx:foo 1))") "FOO"))
           (site (aref (gethash "sites" (first forms)) 0)))
      (ok (equal "fx:foo" (gethash "token" site)))
      (ok (= 2 (gethash "line" site)))
      (ok (= 4 (gethash "column" site)))
      (ok (equal "(fx:foo 1))" (gethash "context" site))))))

(deftest scan-text-stops-at-max-sites
  (testing "collection stops and says so"
    (multiple-value-bind (forms count truncated)
        (scan-text "(defun a () (foo) (foo) (foo))" "FOO" :max-sites 2)
      (ok (= 2 count))
      (ok truncated)
      (ok (= 2 (length (gethash "sites" (first forms))))))))

(deftest scan-project-reports-files-and-failures
  (testing "files scanned, files matched, forms and parse failures"
    (let ((dir (uiop:ensure-directory-pathname
                (uiop:merge-pathnames* (format nil "cl-mcp-refs-scan-~D/" (random 1000000))
                                       (uiop:temporary-directory)))))
      (ensure-directories-exist dir)
      (unwind-protect
           (flet ((put (name text)
                    (with-open-file (s (merge-pathnames name dir)
                                       :direction :output :if-exists :supersede
                                       :external-format :utf-8)
                      (write-string text s))))
             (put "uses.lisp" "(defun a () (foo))")
             (put "silent.lisp" "(defun b () (bar))")
             (put "broken.lisp" "(defun c () (foo")
             (let ((scan (scan-project "foo" :root dir)))
               (ok (equal "FOO" (gethash "target_name" scan)))
               (ok (stringp (gethash "root" scan)))
               (ok (= 3 (gethash "files_scanned" scan)))
               (ok (= 2 (gethash "files_matched" scan)))
               (ok (= 1 (length (gethash "forms" scan))))
               (ok (= 1 (length (gethash "parse_failures" scan))))
               (ok (search "broken.lisp"
                           (gethash "abs_path" (aref (gethash "parse_failures" scan) 0))))
               (ok (null (gethash "truncated_at" scan)))
               (ok (null (gethash "skipped_reason" scan)))))
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(deftest scan-project-without-root-is-skipped
  (testing "no root means no scan, with a reason"
    (let ((scan (scan-project "foo" :root nil)))
      (ok (stringp (gethash "skipped_reason" scan)))
      (ok (zerop (length (gethash "forms" scan)))))))

(deftest target-name-from-designator-validates
  (testing "the name as read, or an argument error"
    (ok (equal "FOO" (target-name-from-designator "pkg::foo")))
    (ok (handler-case (progn (target-name-from-designator ":foo") nil)
          (arg-validation-error () t)))))
```

`tests.lisp` に `(:import-from #:cl-mcp/tests/code-refs-scan-test)` を追加。

- [ ] **Step 2: 失敗を確認する**

`<name>` = `code-refs-scan-test`。Expected: `cl-mcp/src/code-refs-scan` が無くロードエラー。

- [ ] **Step 3: 実装する**

`fs-write-file` で `src/code-refs-scan.lisp`:

```lisp
;;;; src/code-refs-scan.lisp
;;;;
;;;; Parent-side half of code-find-references' impact analysis.  It finds
;;;; every place a symbol name is written in the project's Lisp files,
;;;; classifies the position (call, quoted, bound, ...), and records the
;;;; enclosing top-level form.  It does not decide which symbol a site names:
;;;; the parent image does not have the user's packages, so the worker does
;;;; that (CL-MCP/SRC/CODE-REFS-CORE:RESOLVE-SCAN-FORMS).

(defpackage #:cl-mcp/src/code-refs-scan
  (:use #:cl)
  (:import-from #:cl-mcp/src/cst
                #:cst-node-kind
                #:cst-node-value
                #:cst-node-children
                #:cst-node-start
                #:cst-node-end
                #:cst-node-start-line
                #:cst-node-end-line
                #:parse-top-level-forms
                #:%in-package-form-p)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%definition-candidates)
  (:import-from #:cl-mcp/src/utils/clgrep
                #:collect-target-files)
  (:import-from #:cl-mcp/src/utils/paths
                #:normalize-path-for-display)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:import-from #:cl-mcp/src/code-refs-core
                #:parse-target-designator
                #:find-package-named)
  (:export #:*max-scan-sites*
           #:target-name-from-designator
           #:scan-text
           #:scan-project))

(in-package #:cl-mcp/src/code-refs-scan)

(defparameter *max-scan-sites* 5000
  "Most sites one scan collects.  The rest are dropped and the scan says so,
which keeps the request the parent sends the worker bounded.")

(defparameter *context-width* 160
  "Characters of a source line kept as a site's or a form's context.")

(defparameter *test-frameworks*
  '(("DEFTEST" . "rove") ("TEST" . "fiveam") ("DEF-TEST" . "fiveam")
    ("DEFINE-TEST" . "parachute"))
  "Head names of test-defining forms, with the framework each usually means.")

(defparameter *lambda-list-positions*
  '(("DEFUN" . 1) ("DEFMACRO" . 1) ("DEFGENERIC" . 1)
    ("DEFINE-COMPILER-MACRO" . 1) ("LAMBDA" . 0)
    ("DESTRUCTURING-BIND" . 0) ("MULTIPLE-VALUE-BIND" . 0))
  "Operators whose argument at the given position is a lambda list, counting
the operator's arguments from 0.  For the DEF forms argument 0 is the name.")

(defparameter *shadowing-operators* '("FLET" "LABELS" "MACROLET")
  "Operators whose bindings shadow a global function or macro of the same name.")

(defun target-name-from-designator (designator)
  "Return the symbol name DESIGNATOR spells, as the reader would read it.
Signals ARG-VALIDATION-ERROR for text that is not a symbol name or is a
keyword, before any file is read."
  (values (parse-target-designator designator)))

(defun %expr-children (node)
  "Return NODE's children that are expressions, skipping comments."
  (remove-if-not (lambda (child) (eq (cst-node-kind child) :expr))
                 (cst-node-children node)))

(defun %definer-name-p (name)
  "True when NAME, a head's symbol name, names a DEF... form."
  (and (> (length name) 3) (string= "DEF" (subseq name 0 3))))

(defun %eclector-marker-p (symbol name)
  "True when SYMBOL is eclector's own backquote marker NAME.
Eclector reads `x as (ECLECTOR.READER:QUASIQUOTE x) and ,x as
(ECLECTOR.READER:UNQUOTE x); comparing the package keeps a user's own UNQUOTE
function from being taken for one."
  (and (symbolp symbol)
       (string= (symbol-name symbol) name)
       (let ((package (symbol-package symbol)))
         (and package (string= (package-name package) "ECLECTOR.READER")))))

(defun %line-context (text start)
  "Return the source line containing START, trimmed and cut to *CONTEXT-WIDTH*."
  (let* ((newline (position #\Newline text :end start :from-end t))
         (line-start (if newline (1+ newline) 0))
         (line-end (or (position #\Newline text :start start) (length text)))
         (line (string-trim '(#\Space #\Tab #\Return)
                            (subseq text line-start line-end))))
    (if (> (length line) *context-width*)
        (subseq line 0 *context-width*)
        line)))

(defun %column (text start)
  "Return the 1-based column of character offset START in TEXT."
  (- start (or (position #\Newline text :end start :from-end t) -1)))

(defun %collect-sites (top text target-name)
  "Return plists (:node :kind :token :shadowed-by) for TARGET-NAME inside the
top-level node TOP, in source order.

Classification is positional, not a code walker.  The head of a list in an
evaluated position is \"call\" and any other evaluated position \"reference\";
QUOTE data is \"quoted\"; #'name is \"function\"; lambda lists and LET bindings
are \"bind\"; SETF and SETQ places are \"set\"; a DEFMETHOD's name is
\"method\"; and anything inside a backquote template, outside its unquotes, is
\"template\".  The name position of any other DEF... form is the definition
itself and is skipped, as are DEFPACKAGE forms, keywords and #:symbols.  Below
a FLET, LABELS or MACROLET that binds the name every site carries that operator
in :SHADOWED-BY, however far down it is."
  (let ((sites '()))
    (labels
        ((emit (node kind template shadowed-by)
           (let ((value (cst-node-value node)))
             (when (and (eq (cst-node-kind node) :expr)
                        (symbolp value)
                        (string= (symbol-name value) target-name))
               (let ((token (subseq text (cst-node-start node) (cst-node-end node))))
                 (unless (or (zerop (length token))
                             (char= (char token 0) #\:)
                             (and (> (length token) 1) (string= "#:" (subseq token 0 2))))
                   (push (list :node node
                               :kind (if template "template" kind)
                               :token token
                               :shadowed-by shadowed-by)
                         sites))))))
         (walk (node template shadowed-by)
           (when (eq (cst-node-kind node) :expr)
             (if (consp (cst-node-value node))
                 (walk-list node template shadowed-by)
                 (emit node "reference" template shadowed-by))))
         (walk-quoted (node shadowed-by)
           (when (eq (cst-node-kind node) :expr)
             (if (consp (cst-node-value node))
                 (dolist (child (%expr-children node))
                   (walk-quoted child shadowed-by))
                 (emit node "quoted" nil shadowed-by))))
         (walk-lambda-list (node template shadowed-by)
           ;; A variable is a binding.  A (var init) or ((key var) init) entry
           ;; also holds evaluated forms, and a DEFMETHOD's (var class) holds a
           ;; class name, which then reads as a reference.
           (when (eq (cst-node-kind node) :expr)
             (if (consp (cst-node-value node))
                 (dolist (entry (%expr-children node))
                   (if (consp (cst-node-value entry))
                       (let ((parts (%expr-children entry)))
                         (when parts
                           (if (consp (cst-node-value (first parts)))
                               (dolist (key-or-var (%expr-children (first parts)))
                                 (emit key-or-var "bind" template shadowed-by))
                               (emit (first parts) "bind" template shadowed-by))
                           (dolist (part (rest parts))
                             (walk part template shadowed-by))))
                       (emit entry "bind" template shadowed-by)))
                 (emit node "bind" template shadowed-by))))
         (walk-bindings (node shadowed-by)
           (when (and (eq (cst-node-kind node) :expr) (consp (cst-node-value node)))
             (dolist (binding (%expr-children node))
               (if (consp (cst-node-value binding))
                   (let ((parts (%expr-children binding)))
                     (when parts
                       (emit (first parts) "bind" nil shadowed-by)
                       (dolist (part (rest parts))
                         (walk part nil shadowed-by))))
                   (emit binding "bind" nil shadowed-by)))))
         (binds-target-p (bindings)
           (and bindings
                (some (lambda (binding)
                        (let* ((parts (%expr-children binding))
                               (name (and parts (cst-node-value (first parts)))))
                          (and name
                               (symbolp name)
                               (string= (symbol-name name) target-name))))
                      (%expr-children bindings))))
         (walk-list (node template shadowed-by)
           (let* ((value (cst-node-value node))
                  (children (%expr-children node))
                  (wrapped (find value children :key #'cst-node-value :test #'eq)))
             ;; #+feature form reads as a node whose value is the form and whose
             ;; children are the feature expression and the form itself.
             (when wrapped
               (return-from walk-list (walk wrapped template shadowed-by)))
             (let* ((head (car value))
                    (explicit (and children
                                   (symbolp head)
                                   (eq (cst-node-value (first children)) head)))
                    (head-node (and explicit (first children)))
                    (args (if explicit (rest children) children))
                    (name (and (symbolp head) (symbol-name head))))
               (flet ((emit-head (kind)
                        (when head-node
                          (emit head-node kind template shadowed-by)))
                      (walk-args (&optional (from args) (in-template template)
                                   (shadow shadowed-by))
                        (dolist (arg from)
                          (walk arg in-template shadow))))
                 (cond
                   ((null name)
                    (dolist (child children)
                      (walk child template shadowed-by)))
                   ((%eclector-marker-p head "QUASIQUOTE")
                    (walk-args args t))
                   ((or (%eclector-marker-p head "UNQUOTE")
                        (%eclector-marker-p head "UNQUOTE-SPLICING"))
                    (walk-args args nil))
                   (template
                    (emit-head "template")
                    (walk-args))
                   ((string= name "QUOTE")
                    (emit-head "call")
                    (dolist (arg args)
                      (walk-quoted arg shadowed-by)))
                   ((string= name "FUNCTION")
                    (emit-head "call")
                    (dolist (arg args)
                      (if (consp (cst-node-value arg))
                          (walk arg nil shadowed-by)
                          (emit arg "function" nil shadowed-by))))
                   ((member name '("DEFPACKAGE" "DEFINE-PACKAGE") :test #'string=)
                    nil)
                   ((string= name "DEFMETHOD")
                    (emit-head "call")
                    (when args
                      (emit (first args) "method" nil shadowed-by)
                      (let ((lambda-list-seen nil))
                        (dolist (arg (rest args))
                          (cond
                            (lambda-list-seen
                             (walk arg nil shadowed-by))
                            ((listp (cst-node-value arg))
                             (setf lambda-list-seen t)
                             (walk-lambda-list arg nil shadowed-by)))))))
                   ((assoc name *lambda-list-positions* :test #'string=)
                    (emit-head "call")
                    (let ((position (cdr (assoc name *lambda-list-positions*
                                                :test #'string=))))
                      (loop for arg in args
                            for i from 0
                            do (cond
                                 ((and (= i 0) (= position 1)) nil)
                                 ((= i position) (walk-lambda-list arg nil shadowed-by))
                                 (t (walk arg nil shadowed-by))))))
                   ((member name '("LET" "LET*") :test #'string=)
                    (emit-head "call")
                    (when args
                      (walk-bindings (first args) shadowed-by)
                      (walk-args (rest args))))
                   ((member name '("SETF" "SETQ" "PSETF" "PSETQ") :test #'string=)
                    (emit-head "call")
                    (loop for arg in args
                          for i from 0
                          do (if (and (evenp i) (not (consp (cst-node-value arg))))
                                 (emit arg "set" nil shadowed-by)
                                 (walk arg nil shadowed-by))))
                   ((member name *shadowing-operators* :test #'string=)
                    (emit-head "call")
                    (let* ((bindings (first args))
                           (inner (if (binds-target-p bindings)
                                      (string-downcase name)
                                      shadowed-by)))
                      (when bindings
                        (dolist (binding (%expr-children bindings))
                          (let ((parts (%expr-children binding)))
                            (when (second parts)
                              (walk-lambda-list (second parts) nil inner))
                            (dolist (part (cddr parts))
                              (walk part nil inner)))))
                      (walk-args (rest args) nil inner)))
                   ((member name '("DEFCLASS" "DEFINE-CONDITION") :test #'string=)
                    (emit-head "call")
                    (when (second args)
                      (dolist (super (%expr-children (second args)))
                        (emit super "reference" nil shadowed-by)))
                    (walk-args (cddr args)))
                   ((%definer-name-p name)
                    ;; Any other definer: argument 0 is the name being defined.
                    (emit-head "call")
                    (walk-args (rest args)))
                   (t
                    (emit-head "call")
                    (walk-args))))))))
      (walk top nil nil))
    (nreverse sites)))

(defun %form-metadata (value in-package)
  "Return (values FORM-TYPE FORM-NAME TEST-NAME TEST-FRAMEWORK) for VALUE, a
top-level form read where IN-PACKAGE (a designator or NIL) was current.

FORM-NAME is what lisp-edit-form's form_name matches, a method's specializers
included, and is given only for DEF... and test forms.  FORM-TYPE is the head's
lower-case name whenever the head is a symbol."
  (let ((head (and (consp value) (symbolp (car value)) (car value))))
    (if (null head)
        (values nil nil nil nil)
        (let* ((name (symbol-name head))
               (form-type (string-downcase name))
               (framework (cdr (assoc name *test-frameworks* :test #'string=)))
               (form-name
                 (and (or framework (%definer-name-p name))
                      (consp (cdr value))
                      ;; Print as the parser read it, so the name's symbols
                      ;; come out without package prefixes.
                      (let ((*package* (or (find-package-named in-package)
                                           (find-package "COMMON-LISP-USER"))))
                        (car (last (ignore-errors
                                    (%definition-candidates value form-type))))))))
          (values form-type form-name (and framework form-name) framework)))))

(defun %site->ht (site text)
  "Return the JSON object for SITE, a %COLLECT-SITES plist, in TEXT."
  (let ((node (getf site :node)))
    (make-ht "line" (cst-node-start-line node)
             "column" (%column text (cst-node-start node))
             "kind" (getf site :kind)
             "token" (getf site :token)
             "context" (%line-context text (cst-node-start node))
             "shadowed_by" (getf site :shadowed-by))))

(defun scan-text (text target-name &key path abs-path (max-sites *max-scan-sites*))
  "Scan TEXT, one file's contents, for sites of TARGET-NAME.

TARGET-NAME is a symbol name as read (see TARGET-NAME-FROM-DESIGNATOR).  PATH
is the display path and ABS-PATH the truename namestring the worker matches
xref entries on; ABS-PATH also gives the parser the file's package context.

Returns (values FORMS SITE-COUNT TRUNCATED-P).  FORMS lists one JSON-ready
hash-table per top-level form holding a site, with path, abs_path, index,
start_line, end_line, form_type, form_name, test_name, test_framework,
in_package, context and sites.  INDEX counts the file's top-level expressions
from 0, the number SBCL's DEFINITION-SOURCE-FORM-PATH starts with.  Each site
has line, column, kind, token, context and shadowed_by.  Collection stops after
MAX-SITES sites.  Signals the parser's error when TEXT does not parse."
  (let ((nodes (if abs-path
                   (parse-top-level-forms text :source-path (pathname abs-path))
                   (let ((*package* (find-package "COMMON-LISP-USER")))
                     (parse-top-level-forms text))))
        (forms '())
        (count 0)
        (truncated nil)
        (index -1)
        (in-package nil))
    (dolist (node nodes)
      (when (eq (cst-node-kind node) :expr)
        (incf index)
        (let ((sites (and (not truncated)
                          (%collect-sites node text target-name))))
          (when (> (+ count (length sites)) max-sites)
            (setf sites (subseq sites 0 (- max-sites count))
                  truncated t))
          (when sites
            (incf count (length sites))
            (multiple-value-bind (form-type form-name test-name framework)
                (%form-metadata (cst-node-value node) in-package)
              (push (make-ht "path" path
                             "abs_path" abs-path
                             "index" index
                             "start_line" (cst-node-start-line node)
                             "end_line" (cst-node-end-line node)
                             "form_type" form-type
                             "form_name" form-name
                             "test_name" test-name
                             "test_framework" framework
                             "in_package" in-package
                             "context" (%line-context text (cst-node-start node))
                             "sites" (map 'vector
                                          (lambda (site) (%site->ht site text))
                                          sites))
                    forms))))
        (let ((designator (%in-package-form-p (cst-node-value node))))
          (when designator
            (setf in-package designator)))))
    (values (nreverse forms) count truncated)))

(defun %first-line (text)
  "Return TEXT's first line, for a one-line error summary."
  (subseq text 0 (or (position #\Newline text) (length text))))

(defun scan-project (designator &key (root *project-root*) (max-sites *max-scan-sites*))
  "Scan every Lisp file under ROOT for sites of the symbol DESIGNATOR spells.

Validates DESIGNATOR first (see TARGET-NAME-FROM-DESIGNATOR).  The files are
those clgrep-search reads (.lisp, .asd and .ros, honouring .gitignore); only
those whose text contains the name, ignoring case, are parsed.  Returns a
JSON-ready hash-table:
  target_name     the name matched
  root            ROOT's truename namestring, or null
  files_scanned   files considered
  files_matched   files whose text contains the name
  forms           SCAN-TEXT's forms for every file, concatenated
  parse_failures  path, abs_path and error of each file that did not parse
  truncated_at    MAX-SITES when collection stopped there, else null
  skipped_reason  why nothing was scanned, else null"
  (let ((name (target-name-from-designator designator))
        (root-truename (and root (ignore-errors (namestring (truename root)))))
        (forms '())
        (failures '())
        (scanned 0)
        (matched 0)
        (count 0)
        (truncated nil))
    (flet ((report (&optional skipped)
             (make-ht "target_name" name
                      "root" root-truename
                      "files_scanned" scanned
                      "files_matched" matched
                      "forms" (coerce forms 'vector)
                      "parse_failures" (coerce (reverse failures) 'vector)
                      "truncated_at" (and truncated max-sites)
                      "skipped_reason" skipped)))
      (unless root-truename
        (return-from scan-project
          (report (if root "project root is not readable" "project root is not set"))))
      (dolist (file (collect-target-files root-truename))
        (incf scanned)
        (let ((text (and (not truncated) (ignore-errors (uiop:read-file-string file)))))
          (when (and text (search name text :test #'char-equal))
            (incf matched)
            (let ((abs-path (namestring file)))
              (handler-case
                  (multiple-value-bind (file-forms file-count file-truncated)
                      (scan-text text name
                                 :path (normalize-path-for-display file)
                                 :abs-path abs-path
                                 :max-sites (- max-sites count))
                    (setf forms (append forms file-forms))
                    (incf count file-count)
                    (when file-truncated
                      (setf truncated t)))
                (error (e)
                  (push (make-ht "path" (normalize-path-for-display file)
                                 "abs_path" abs-path
                                 "error" (%first-line (princ-to-string e)))
                        failures)))))))
      (report))))
```

- [ ] **Step 4: 成功を確認する**

`code-refs-scan-test` を実行。Expected: 全テスト `✓`。失敗したら、該当する入力テキストを `repl-eval` で `scan-text` に直接渡し、CST の形（ノードの value と children）を確かめてから直す。テストの期待値は仕様（spec 6.4）なので、期待値ではなく実装を直す。

- [ ] **Step 5: Lint とコミット**

```bash
mallet src/code-refs-scan.lisp tests/code-refs-scan-test.lisp
git add src/code-refs-scan.lisp tests/code-refs-scan-test.lisp tests.lisp
git commit -m "feat(code-refs): scan project sources for where a symbol is written

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016jJhPXZ53FRPgqhA3aT8m9"
```

---

### Task 4: `code-refs-core` — 走査位置の解決、xref とのマージ、ペイロード

**Files:**
- Modify: `src/code-refs-core.lisp`（末尾に追加、export 追加）
- Modify: `tests/code-refs-core-test.lisp`（末尾に追加、import 追加）

**Interfaces:**
- Consumes: Task 2 の関数。Task 3 の form hash-table 形式（キーは Task 3 の Produces を参照）
- Produces（export 追加）:
  - `(resolve-scan-forms forms target &key macro-p) → (values resolved unresolved)`
    - resolved: plist の list `(:truename :path :index :start-line :end-line :form-type :form-name :test-name :test-framework :context :sites)`、`:sites` は plist `(:line :column :kind :context :shadowed-by)`
    - unresolved: plist の list `(:path :package :count :tests)`
  - `(merge-references xref-entries forms) → list of hash-table`
    - xref-entries: plist `(:type :caller :caller-symbol :truename :path :line :context :form-index :stale :scan-status)`、`:scan-status` は `:scanned` / `:parse-failed` / `:not-scanned`
    - 各 hash-table のキー: `"path" "line" "type" "types" "caller" "caller_symbol" "context" "form_type" "form_name" "origin" "call_sites" "test" "stale" "note"`
  - `(build-references-report &key symbol resolved-symbol status kind lookup-package lookup-name project-only limit refs unresolved notes xref-count files-scanned name-matches scan-skipped) → hash-table`
    - キー: `"symbol" "resolved_symbol" "symbol_status" "symbol_kind" "lookup_package" "lookup_name" "count" "file_count" "limit" "truncated" "project_only" "refs" "tests" "unresolved" "notes" "xref_count" "files_scanned" "name_matches" "scan_skipped"`

- [ ] **Step 1: 失敗するテストを書く**

`tests/code-refs-core-test.lisp` の `(:import-from #:cl-mcp/src/code-refs-core ...)` に `#:resolve-scan-forms #:merge-references #:build-references-report` を追加し、`(:import-from #:cl-mcp/src/tools/helpers #:arg-validation-error)` に `#:make-ht` を追加する。ファイル末尾に追加:

```lisp
;;; Scan sites, merge and report

(defun %site (token &key (line 1) (kind "call") shadowed-by)
  "Return a scan site object as CL-MCP/SRC/CODE-REFS-SCAN:SCAN-TEXT builds it."
  (make-ht "line" line "column" 1 "kind" kind "token" token
           "context" token "shadowed_by" shadowed-by))

(defun %form (path index in-package sites &key test-name form-name)
  "Return a scan form object for PATH holding SITES."
  (make-ht "path" path "abs_path" (concatenate 'string "/abs/" path)
           "index" index "start_line" (* 10 index) "end_line" (+ 5 (* 10 index))
           "form_type" "defun" "form_name" form-name
           "test_name" test-name "test_framework" (and test-name "rove")
           "in_package" in-package "context" "(defun ..."
           "sites" (coerce sites 'vector)))

(defun %xref (truename index &key (type "call") (caller "c") caller-symbol (line 5)
                                  stale (scan-status :scanned))
  "Return an xref entry plist as CL-MCP/SRC/CODE-CORE collects it."
  (list :type type :caller caller :caller-symbol caller-symbol
        :truename truename :path (subseq truename 5) :line line :context "ctx"
        :form-index index :stale stale :scan-status scan-status))

(defun %resolved (truename index &key (start-line 10) test-name form-name
                                      (sites (list (list :line 11 :column 3 :kind "call"
                                                         :context "(shared)"
                                                         :shadowed-by nil))))
  "Return a resolved form plist as RESOLVE-SCAN-FORMS returns it."
  (list :truename truename :path (subseq truename 5) :index index
        :start-line start-line :end-line (+ start-line 5)
        :form-type "defun" :form-name form-name
        :test-name test-name :test-framework (and test-name "rove")
        :context "(defun x" :sites sites))

(defun %with-origin (refs origin)
  "Return the references among REFS whose origin is ORIGIN."
  (remove-if-not (lambda (ref) (equal origin (gethash "origin" ref))) refs))

(deftest resolve-scan-forms-keeps-only-the-target
  (testing "matches kept, other symbols dropped, missing packages counted"
    (let* ((shared (find-symbol "SHARED" "CL-MCP-REFS-CORE-A"))
           (forms (list (%form "a.lisp" 1 "CL-MCP-REFS-CORE-A"
                               (list (%site "shared" :line 11)
                                     (%site "shared" :line 12 :kind "quoted")))
                        (%form "b.lisp" 2 "CL-MCP-REFS-CORE-B"
                               (list (%site "shared" :line 21)))
                        (%form "c.lisp" 3 "NOT-LOADED-PKG-XYZ"
                               (list (%site "shared") (%site "shared"))
                               :test-name "c-test"))))
      (multiple-value-bind (resolved unresolved)
          (resolve-scan-forms forms shared :macro-p t)
        (ok (= 1 (length resolved)))
        (ok (equal '("macro" "quoted")
                   (mapcar (lambda (site) (getf site :kind)) (getf (first resolved) :sites)))
            "a call becomes macro when the target is a macro")
        (ok (equal "/abs/a.lisp" (getf (first resolved) :truename)))
        (ok (= 1 (length unresolved)))
        (ok (= 2 (getf (first unresolved) :count)))
        (ok (equal '("c-test") (getf (first unresolved) :tests)))
        (ok (equal "NOT-LOADED-PKG-XYZ" (getf (first unresolved) :package)))))))

(deftest resolve-scan-forms-accepts-lists-from-json
  (testing "sites parsed back from JSON arrive as a list"
    (let ((form (%form "a.lisp" 1 "CL-MCP-REFS-CORE-A" '())))
      (setf (gethash "sites" form) (list (%site "shared")))
      (ok (= 1 (length (resolve-scan-forms (list form)
                                           (find-symbol "SHARED" "CL-MCP-REFS-CORE-A"))))))))

(deftest merge-references-three-outcomes
  (testing "xref and source together, xref alone, source alone"
    (let ((refs (merge-references
                 (list (%xref "/abs/a.lisp" 1 :caller "a" :caller-symbol "P::A")
                       (%xref "/abs/a.lisp" 2 :caller "hidden")
                       (%xref "/abs/z.lisp" nil :line 40 :scan-status :not-scanned))
                 (list (%resolved "/abs/a.lisp" 1 :form-name "a")
                       (%resolved "/abs/a.lisp" 3 :form-name "*top*")))))
      (ok (= 4 (length refs)))
      (let ((both (first (%with-origin refs "xref+source"))))
        (ok (equal "P::A" (gethash "caller_symbol" both)))
        (ok (= 1 (length (gethash "call_sites" both))))
        (ok (= 10 (gethash "line" both)) "line is the enclosing form's start")
        (ok (equal "a" (gethash "form_name" both)))
        (ok (null (gethash "note" both))))
      (let ((xref-only (%with-origin refs "xref")))
        (ok (= 2 (length xref-only)))
        (ok (find-if (lambda (ref) (search "macro expansion" (gethash "note" ref))) xref-only))
        (ok (find-if (lambda (ref) (search "not scanned" (gethash "note" ref))) xref-only))
        (ok (every (lambda (ref) (zerop (length (gethash "call_sites" ref)))) xref-only)))
      (let ((source-only (first (%with-origin refs "source"))))
        (ok (equal "*top*" (gethash "caller" source-only)))
        (ok (null (gethash "caller_symbol" source-only)))
        (ok (equal '("call") (coerce (gethash "types" source-only) 'list)))
        (ok (search "not in xref" (gethash "note" source-only)))))))

(deftest merge-references-groups-types-and-prefers-named-callers
  (testing "one form, two xref kinds, a lambda and a named caller"
    (let* ((refs (merge-references
                  (list (%xref "/abs/a.lisp" 1 :type "call" :caller "(lambda)")
                        (%xref "/abs/a.lisp" 1 :type "reference" :caller "named"
                                               :caller-symbol "P::NAMED" :stale t))
                  (list (%resolved "/abs/a.lisp" 1 :test-name "a-test"))))
           (ref (first refs)))
      (ok (= 1 (length refs)))
      (ok (equal '("call" "reference") (coerce (gethash "types" ref) 'list)))
      (ok (equal "call" (gethash "type" ref)))
      (ok (equal "named" (gethash "caller" ref)))
      (ok (eq t (gethash "stale" ref)))
      (ok (search "reload" (gethash "note" ref)))
      (ok (equal "a-test" (gethash "name" (gethash "test" ref)))))))

(deftest build-references-report-sorts-limits-and-lists-tests
  (testing "sorted by path and line, cut to LIMIT, counted in full"
    (let* ((refs (merge-references
                  '()
                  (list (%resolved "/abs/b.lisp" 1 :test-name "b-test")
                        (%resolved "/abs/a.lisp" 2 :start-line 20)
                        (%resolved "/abs/a.lisp" 1 :start-line 10))))
           (report (build-references-report :symbol "p::x" :resolved-symbol "P::X"
                                            :status :found :kind "function"
                                            :project-only t :limit 2 :refs refs)))
      (ok (= 3 (gethash "count" report)))
      (ok (= 2 (gethash "file_count" report)))
      (ok (= 2 (length (gethash "refs" report))))
      (ok (eq t (gethash "truncated" report)))
      (ok (equal '("a.lisp" "a.lisp")
                 (map 'list (lambda (ref) (gethash "path" ref)) (gethash "refs" report))))
      (ok (equal '(10 20)
                 (map 'list (lambda (ref) (gethash "line" ref)) (gethash "refs" report))))
      (ok (equal '("b-test")
                 (map 'list (lambda (test) (gethash "name" test)) (gethash "tests" report)))
          "tests come from every reference, not only the ones shown")
      (ok (equal "found" (gethash "symbol_status" report)))))
  (testing "status spellings and an untruncated report"
    (ok (equal "not_found"
               (gethash "symbol_status" (build-references-report :symbol "x" :status :not-found))))
    (ok (equal "package_not_found"
               (gethash "symbol_status"
                        (build-references-report :symbol "x" :status :package-not-found))))
    (ok (eq yason:false (gethash "truncated" (build-references-report :symbol "x"))))))
```

- [ ] **Step 2: 失敗を確認する**

`code-refs-core-test` を実行。Expected: 新しい関数が未定義でロードエラー（または未定義関数エラー）。

- [ ] **Step 3: 実装する**

`src/code-refs-core.lisp` の `:export` に `#:resolve-scan-forms #:merge-references #:build-references-report` を追加し（`lisp-patch-form`, `form_type: "defpackage"`, `form_name: "cl-mcp/src/code-refs-core"`）、`resolve-site-token` の後ろに `lisp-edit-form`（`insert_after`）で順に追加:

```lisp
(defparameter *note-stale* "file changed since load; reload for accurate results"
  "Note for a reference whose file was written after its code was compiled.")

(defparameter *note-macro-expansion*
  "call not visible in source (produced by a macro expansion)"
  "Note for an xref entry whose form in a scanned file holds no matching site.")

(defparameter *note-parse-failed* "file could not be parsed; call sites unavailable"
  "Note for an xref entry in a file the source scan could not parse.")

(defparameter *note-not-scanned* "source not scanned; call sites unavailable"
  "Note for an xref entry in a file the source scan did not cover.")

(defparameter *note-not-in-xref*
  "not in xref (top-level form, or not compiled since it was written)"
  "Note for a form only the source scan found.")

(defun resolve-scan-forms (forms target &key macro-p)
  "Keep the scan sites in FORMS that name TARGET in this image.

FORMS are the parent's form objects (CL-MCP/SRC/CODE-REFS-SCAN:SCAN-TEXT).
Returns (values RESOLVED UNRESOLVED).

RESOLVED lists, in input order, one plist per form that kept a site:
  (:truename :path :index :start-line :end-line :form-type :form-name
   :test-name :test-framework :context :sites)
with :SITES plists (:line :column :kind :context :shadowed-by).  When MACRO-P a
\"call\" site becomes \"macro\", the type WHO-MACROEXPANDS entries carry.

UNRESOLVED lists plists (:path :package :count :tests), one per file and
missing package, counting the sites that could not be judged."
  (let ((resolved '())
        (unresolved '()))
    (dolist (form (sequence->list forms))
      (let ((kept '()))
        (dolist (site (sequence->list (gethash "sites" form)))
          (multiple-value-bind (verdict missing)
              (resolve-site-token (gethash "token" site) (gethash "in_package" form) target)
            (case verdict
              (:match
               (push (list :line (gethash "line" site)
                           :column (gethash "column" site)
                           :kind (let ((kind (gethash "kind" site)))
                                   (if (and macro-p (equal kind "call")) "macro" kind))
                           :context (gethash "context" site)
                           :shadowed-by (gethash "shadowed_by" site))
                     kept))
              (:unresolved
               (let ((entry (find-if (lambda (entry)
                                       (and (equal (getf entry :path) (gethash "path" form))
                                            (equal (getf entry :package) missing)))
                                     unresolved))
                     (test (gethash "test_name" form)))
                 (unless entry
                   (setf entry (list :path (gethash "path" form) :package missing
                                     :count 0 :tests '()))
                   (push entry unresolved))
                 (incf (getf entry :count))
                 (when (and test (not (member test (getf entry :tests) :test #'equal)))
                   (setf (getf entry :tests) (append (getf entry :tests) (list test)))))))))
        (when kept
          (push (list :truename (gethash "abs_path" form)
                      :path (gethash "path" form)
                      :index (gethash "index" form)
                      :start-line (gethash "start_line" form)
                      :end-line (gethash "end_line" form)
                      :form-type (gethash "form_type" form)
                      :form-name (gethash "form_name" form)
                      :test-name (gethash "test_name" form)
                      :test-framework (gethash "test_framework" form)
                      :context (gethash "context" form)
                      :sites (nreverse kept))
                resolved))))
    (values (nreverse resolved) (nreverse unresolved))))

(defun %distinct (strings)
  "Return STRINGS without duplicates, keeping the first occurrence of each."
  (let ((seen '()))
    (dolist (string strings (nreverse seen))
      (unless (member string seen :test #'equal)
        (push string seen)))))

(defun %site->ht (site)
  "Return the JSON object for a resolved SITE plist."
  (make-ht "line" (getf site :line)
           "column" (getf site :column)
           "kind" (getf site :kind)
           "context" (getf site :context)
           "shadowed_by" (getf site :shadowed-by)))

(defun %reference-note (form xrefs primary stale)
  "Return the note explaining a reference built from FORM and XREFS, or NIL.
PRIMARY is the xref entry the reference takes its caller from."
  (cond
    (stale *note-stale*)
    ((null form)
     (case (getf primary :scan-status)
       (:scanned *note-macro-expansion*)
       (:parse-failed *note-parse-failed*)
       (t *note-not-scanned*)))
    ((null xrefs) *note-not-in-xref*)
    (t nil)))

(defun %reference (form xrefs)
  "Return the reference object for one top-level form.
FORM is a resolved scan form or NIL; XREFS are the xref entries in that form,
in finder order, possibly none.  A named caller is preferred over a lambda
when several xref entries share the form."
  (let* ((primary (or (find-if (lambda (entry) (getf entry :caller-symbol)) xrefs)
                      (first xrefs)))
         (sites (and form (getf form :sites)))
         (types (%distinct (if xrefs
                               (mapcar (lambda (entry) (getf entry :type)) xrefs)
                               (mapcar (lambda (site) (getf site :kind)) sites))))
         (stale (some (lambda (entry) (getf entry :stale)) xrefs)))
    (make-ht "path" (if form (getf form :path) (getf primary :path))
             "line" (if form (getf form :start-line) (getf primary :line))
             "type" (first types)
             "types" (coerce types 'vector)
             "caller" (if primary
                          (getf primary :caller)
                          (or (getf form :form-name) ""))
             "caller_symbol" (and primary (getf primary :caller-symbol))
             "context" (if form (getf form :context) (getf primary :context))
             "form_type" (and form (getf form :form-type))
             "form_name" (and form (getf form :form-name))
             "origin" (cond ((and form xrefs) "xref+source")
                            (xrefs "xref")
                            (t "source"))
             "call_sites" (map 'vector #'%site->ht sites)
             "test" (and form
                         (getf form :test-name)
                         (make-ht "name" (getf form :test-name)
                                  "framework" (getf form :test-framework)))
             "stale" (json-bool stale)
             "note" (%reference-note form xrefs primary stale))))

(defun merge-references (xref-entries forms)
  "Merge XREF-ENTRIES with resolved scan FORMS into reference objects.

XREF-ENTRIES are plists (:type :caller :caller-symbol :truename :path :line
:context :form-index :stale :scan-status), :SCAN-STATUS being :SCANNED,
:PARSE-FAILED or :NOT-SCANNED.  FORMS are RESOLVE-SCAN-FORMS' first value.

Entries and forms meet on (truename, top-level form index).  SBCL's
DEFINITION-SOURCE-FORM-PATH starts with the index of the top-level form among
those the reader returned, which is the form's position among the file's :EXPR
nodes -- reader conditionals, EVAL-WHEN and PROGN included.  The character
offset is no key: it is an octet position, just past the PREVIOUS form.  An
entry without a form path groups on its line instead.

Returns JSON-ready hash-tables, one per top-level form, in first-seen order."
  (let ((seen (make-hash-table :test #'equal))
        (form-by-key (make-hash-table :test #'equal))
        (xrefs-by-key (make-hash-table :test #'equal))
        (order '()))
    (flet ((remember (key)
             (unless (gethash key seen)
               (setf (gethash key seen) t)
               (push key order))))
      (dolist (form forms)
        (let ((key (list (getf form :truename) (getf form :index))))
          (remember key)
          (setf (gethash key form-by-key) form)))
      (dolist (entry xref-entries)
        (let ((key (if (getf entry :form-index)
                       (list (getf entry :truename) (getf entry :form-index))
                       (list (getf entry :truename) :line (getf entry :line)))))
          (remember key)
          (push entry (gethash key xrefs-by-key)))))
    (mapcar (lambda (key)
              (%reference (gethash key form-by-key)
                          (reverse (gethash key xrefs-by-key))))
            (reverse order))))

(defun %reference< (a b)
  "Order references by path, then by line."
  (let ((path-a (gethash "path" a))
        (path-b (gethash "path" b)))
    (if (string= path-a path-b)
        (< (or (gethash "line" a) 0) (or (gethash "line" b) 0))
        (string< path-a path-b))))

(defun %tests-of (refs)
  "Return one JSON object (name, path, line) per distinct test among REFS."
  (let ((tests '()))
    (dolist (ref refs (nreverse tests))
      (let ((test (gethash "test" ref)))
        (when (and test
                   (not (find-if (lambda (seen)
                                   (and (equal (gethash "name" seen) (gethash "name" test))
                                        (equal (gethash "path" seen) (gethash "path" ref))))
                                 tests)))
          (push (make-ht "name" (gethash "name" test)
                         "path" (gethash "path" ref)
                         "line" (gethash "line" ref))
                tests))))))

(defun %status-string (status)
  "Return the JSON spelling of a RESOLVE-TARGET status keyword."
  (ecase status
    (:found "found")
    (:not-found "not_found")
    (:package-not-found "package_not_found")))

(defun build-references-report (&key symbol resolved-symbol (status :found) kind
                                  lookup-package lookup-name project-only (limit 50)
                                  refs unresolved notes (xref-count 0)
                                  (files-scanned 0) (name-matches 0) scan-skipped)
  "Return the code-find-references payload, everything but its content text.

REFS are MERGE-REFERENCES' objects.  They are sorted by path and line and at
most LIMIT are kept, while count, file_count and tests describe all of them.
UNRESOLVED is RESOLVE-SCAN-FORMS' second value and NOTES are plain sentences.
docs/tools.md describes every field."
  (let* ((sorted (sort (copy-list refs) #'%reference<))
         (count (length sorted)))
    (make-ht "symbol" symbol
             "resolved_symbol" resolved-symbol
             "symbol_status" (%status-string status)
             "symbol_kind" kind
             "lookup_package" lookup-package
             "lookup_name" lookup-name
             "count" count
             "file_count" (length (%distinct (mapcar (lambda (ref) (gethash "path" ref))
                                                     sorted)))
             "limit" limit
             "truncated" (json-bool (> count limit))
             "project_only" (json-bool project-only)
             "refs" (coerce (subseq sorted 0 (min count limit)) 'vector)
             "tests" (coerce (%tests-of sorted) 'vector)
             "unresolved" (map 'vector
                               (lambda (entry)
                                 (make-ht "path" (getf entry :path)
                                          "package" (getf entry :package)
                                          "count" (getf entry :count)
                                          "tests" (coerce (getf entry :tests) 'vector)))
                               unresolved)
             "notes" (coerce notes 'vector)
             "xref_count" xref-count
             "files_scanned" files-scanned
             "name_matches" name-matches
             "scan_skipped" scan-skipped)))
```

- [ ] **Step 4: 成功を確認する**

`code-refs-core-test` を実行。Expected: Task 2 のテストを含め全 `✓`。

- [ ] **Step 5: Lint とコミット**

```bash
mallet src/code-refs-core.lisp tests/code-refs-core-test.lisp
git add src/code-refs-core.lisp tests/code-refs-core-test.lisp
git commit -m "feat(code-refs): merge scan sites with xref entries per top-level form

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016jJhPXZ53FRPgqhA3aT8m9"
```

---

### Task 5: `code-core` の組み込みと実 xref による統合テスト

**Files:**
- Modify: `src/code-core.lisp`（import/export 追加、`code-find-references` を置き換え、補助関数追加）
- Create: `tests/fixtures/xref-fixture.lisp`
- Modify: `tests/code-test.lisp`（import 追加、統合テスト 3 本追加）

**Interfaces:**
- Consumes: Task 2〜4 の `resolve-target` `qualified-symbol-name` `symbol-kind` `resolve-scan-forms` `merge-references` `build-references-report` `sequence->list`。Task 3 の `scan-project`（テストのみ）
- Produces:
  - `cl-mcp/src/code-core:code-find-references-report (symbol-name &key package (project-only t) (limit 50) scan) → hash-table`（Task 4 の `build-references-report` のキー）
  - `cl-mcp/src/code-core:code-find-references (symbol-name &key package (project-only t)) → (values refs-vector count)`（互換。scan なし、limit なし）

- [ ] **Step 1: フィクスチャを作る**

`fs-write-file` で `tests/fixtures/xref-fixture.lisp`（テストはテキスト検索で行を求めるので、呼び出しの綴り `(target N)` は一意に保つ）:

```lisp
;;;; tests/fixtures/xref-fixture.lisp
;;;;
;;;; Compiled and loaded by tests/code-test.lisp so that SBCL records real
;;;; cross references for code-find-references to merge with its source scan.
;;;; 日本語のコメント: SBCL の位置情報はバイト単位なので、この行の後ろでも
;;;; 行番号がずれないことを確かめる。
;;;; The tests find each call site by its text, so keep every (target N) unique.

(defpackage #:cl-mcp-xref-fixture
  (:use #:cl)
  (:import-from #:rove #:deftest #:ok)
  (:export #:target #:with-target))

(defpackage #:cl-mcp-xref-fixture-other
  (:use #:cl)
  (:export #:target))

(defpackage #:cl-mcp-xref-fixture-nick
  (:use #:cl)
  (:local-nicknames (#:fx #:cl-mcp-xref-fixture)))

(in-package #:cl-mcp-xref-fixture)

(defun target (x)
  "The function every other form refers to."
  (1+ x))

(defun plain-caller ()
  (target 1))

#+sbcl
(defun feature-caller ()
  (target 2))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun eval-when-caller ()
    (target 3)))

(defparameter *top-level-use* (target 4))

(defmacro with-target (&body body)
  `(progn (target 5) ,@body))

(defun macro-hidden-caller ()
  (with-target :done))

(defun shadowing-caller ()
  (flet ((target (x) (* x 10)))
    (target 6)))

(defgeneric shape-area (shape))

(defmethod shape-area ((shape integer))
  (target shape))

(deftest target-is-called-from-a-test
  (ok (= 8 (target 7))))

(in-package #:cl-mcp-xref-fixture-other)

(defun target (x)
  "Same name, different symbol: never a reference to the fixture's TARGET."
  x)

(defun other-caller ()
  (target 8))

(in-package #:cl-mcp-xref-fixture-nick)

(defun nickname-caller ()
  (fx:target 9))
```

- [ ] **Step 2: 失敗するテストを書く**

`tests/code-test.lisp` の `defpackage` に追加（`lisp-patch-form`）:

```lisp
  (:import-from #:cl-mcp/src/code-core
                #:code-find-references-report)
  (:import-from #:cl-mcp/src/code-refs-scan
                #:scan-project)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
```

ファイル末尾に追加:

```lisp
;;; code-find-references-report against real xref data

(defparameter *xref-fixture*
  (asdf:system-relative-pathname :cl-mcp "tests/fixtures/xref-fixture.lisp")
  "Fixture compiled so that SBCL records cross references for it.")

(defun %load-xref-fixture ()
  "Compile and load the xref fixture; xref needs COMPILE-FILE, not LOAD of source."
  (uiop:with-temporary-file (:pathname fasl :type "fasl")
    (handler-bind ((warning #'muffle-warning))
      (load (compile-file *xref-fixture* :output-file fasl :verbose nil :print nil)))))

(defun %xref-fixture-report ()
  "Return the report for the fixture's TARGET, scanning the fixture directory."
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (%load-xref-fixture)
    (code-find-references-report
     "cl-mcp-xref-fixture:target"
     :limit 1000
     :scan (scan-project "cl-mcp-xref-fixture:target"
                         :root (uiop:pathname-directory-pathname *xref-fixture*)))))

(defun %fixture-line (needle)
  "Return the 1-based line of the fixture on which NEEDLE starts."
  (let ((text (uiop:read-file-string *xref-fixture*)))
    (1+ (count #\Newline text :end (search needle text)))))

(defun %ref-named (report form-name)
  "Return the reference in REPORT whose form_name is FORM-NAME."
  (find form-name (gethash "refs" report)
        :key (lambda (ref) (gethash "form_name" ref)) :test #'equal))

(defun %site-lines (ref)
  "Return the call-site lines of REF."
  (map 'list (lambda (site) (gethash "line" site)) (gethash "call_sites" ref)))

(deftest code-find-references-report-exact-call-sites
  (if (uiop:os-macosx-p)
      (skip "XREF tests are unstable on macOS")
      (let ((report (%xref-fixture-report)))
        (testing "the symbol resolves and is described"
          (ok (equal "found" (gethash "symbol_status" report)))
          (ok (equal "CL-MCP-XREF-FIXTURE:TARGET" (gethash "resolved_symbol" report)))
          (ok (equal "function" (gethash "symbol_kind" report))))
        (testing "a plain caller carries its exact call line and qualified name"
          (let ((ref (%ref-named report "plain-caller")))
            (ok ref)
            (when ref
              (ok (equal "xref+source" (gethash "origin" ref)))
              (ok (equal (list (%fixture-line "(target 1)")) (%site-lines ref)))
              (ok (equal "CL-MCP-XREF-FIXTURE::PLAIN-CALLER" (gethash "caller_symbol" ref)))
              (ok (= (%fixture-line "(defun plain-caller") (gethash "line" ref))))))
        (testing "#+sbcl and eval-when wrappers still meet their xref entries"
          (let ((feature (%ref-named report "feature-caller"))
                (eval-when (find (%fixture-line "(target 3)") (gethash "refs" report)
                                 :key (lambda (ref) (first (%site-lines ref))))))
            (ok (and feature (equal "xref+source" (gethash "origin" feature))))
            (ok (and feature (equal (list (%fixture-line "(target 2)")) (%site-lines feature))))
            (ok (and eval-when (equal "xref+source" (gethash "origin" eval-when))))
            (ok (and eval-when (equal "eval-when" (gethash "form_type" eval-when))))))
        (testing "a method is named the way lisp-edit-form addresses it"
          (let ((ref (%ref-named report "shape-area ((shape integer))")))
            (ok ref)
            (ok (and ref (equal "xref+source" (gethash "origin" ref))))
            (ok (and ref (equal "CL-MCP-XREF-FIXTURE::SHAPE-AREA"
                                (gethash "caller_symbol" ref))))))
        (testing "a package-local nickname resolves; a same-named symbol does not"
          (ok (%ref-named report "nickname-caller"))
          (ok (null (%ref-named report "other-caller")))))))

(deftest code-find-references-report-finds-what-xref-cannot
  (if (uiop:os-macosx-p)
      (skip "XREF tests are unstable on macOS")
      (let ((report (%xref-fixture-report)))
        (testing "a top-level use comes from the source scan alone"
          (let ((ref (%ref-named report "*top-level-use*")))
            (ok ref)
            (ok (and ref (equal "source" (gethash "origin" ref))))
            (ok (and ref (equal "defparameter" (gethash "form_type" ref))))
            (ok (and ref (search "not in xref" (gethash "note" ref))))))
        (testing "a macro template is labelled; the call it expands into comes from xref"
          (let ((template (%ref-named report "with-target"))
                (hidden (find-if (lambda (ref)
                                   (search "macro-hidden-caller" (gethash "caller" ref)))
                                 (gethash "refs" report))))
            (ok (and template
                     (equal '("template")
                            (map 'list (lambda (site) (gethash "kind" site))
                                 (gethash "call_sites" template)))))
            (ok hidden)
            (ok (and hidden (equal "xref" (gethash "origin" hidden))))
            (ok (and hidden (zerop (length (gethash "call_sites" hidden)))))
            (ok (and hidden (search "macro expansion" (gethash "note" hidden))))))
        (testing "a flet of the same name is flagged as shadowing"
          (let ((ref (%ref-named report "shadowing-caller")))
            (ok ref)
            (ok (and ref
                     (plusp (length (gethash "call_sites" ref)))
                     (every (lambda (site) (equal "flet" (gethash "shadowed_by" site)))
                            (gethash "call_sites" ref))))))
        (testing "a call inside a deftest is attributed to the test"
          (let ((ref (%ref-named report "target-is-called-from-a-test")))
            (ok ref)
            (ok (and ref (equal "xref+source" (gethash "origin" ref))))
            (ok (and ref (equal "target-is-called-from-a-test"
                                (gethash "name" (gethash "test" ref)))))
            (ok (find "target-is-called-from-a-test" (gethash "tests" report)
                      :key (lambda (test) (gethash "name" test)) :test #'equal)))))))

(deftest code-find-references-report-never-interns
  (testing "a missing symbol is reported and left uninterned"
    (let ((report (code-find-references-report
                   "cl-mcp/src/code-core::%no-such-function-xyz")))
      (ok (equal "not_found" (gethash "symbol_status" report)))
      (ok (null (nth-value 1 (find-symbol "%NO-SUCH-FUNCTION-XYZ" "CL-MCP/SRC/CODE-CORE"))))))
  (testing "a single colon reaches an internal symbol"
    (ok (equal "found"
               (gethash "symbol_status"
                        (code-find-references-report "cl-mcp/src/code-core:%parse-symbol"))))))
```

- [ ] **Step 3: 失敗を確認する**

`code-test` を実行。Expected: `code-find-references-report` が未 export でロードエラー。

- [ ] **Step 4: 実装する**

`src/code-core.lisp` の `defpackage` に追加（`lisp-patch-form`, `form_name: "cl-mcp/src/code-core"`）:

```lisp
  (:import-from #:cl-mcp/src/code-refs-core
                #:sequence->list
                #:resolve-target
                #:qualified-symbol-name
                #:symbol-kind
                #:resolve-scan-forms
                #:merge-references
                #:build-references-report)
```

`:export` に `#:code-find-references-report` を追加。

`%finder->type` の直後に `lisp-edit-form`（`insert_after`, `form_name: "%finder->type"`）で追加:

```lisp
(defun %xref-caller-symbol (name)
  "Return the symbol naming the definition an xref caller NAME sits in, or NIL.
NAME is a symbol for a plain function; (SB-PCL::FAST-METHOD GF ...) and its
relatives are a method of GF; (FLET INNER :IN OUTER) and (LABELS ...) sit in
OUTER.  Lambdas and other shapes have no such symbol."
  (cond
    ((and name (symbolp name)) name)
    ((not (consp name)) nil)
    ((and (symbolp (car name))
          (member (symbol-name (car name)) '("FAST-METHOD" "SLOW-METHOD" "METHOD")
                  :test #'string=)
          (second name)
          (symbolp (second name)))
     (second name))
    ((and (symbolp (car name))
          (member (symbol-name (car name)) '("FLET" "LABELS") :test #'string=))
     (let ((outer (second (member :in name))))
       (and outer (symbolp outer) outer)))
    (t nil)))

(defun %truename-string (pathname)
  "Return PATHNAME's truename as a namestring, or its namestring when it has none."
  (and pathname
       (handler-case (namestring (truename pathname))
         (error () (namestring pathname)))))

(defun %source-stale-p (pathname recorded-write-date)
  "True when PATHNAME was written after RECORDED-WRITE-DATE, the date SBCL kept."
  (and pathname
       recorded-write-date
       (let ((current (ignore-errors (file-write-date pathname))))
         (and current (> current recorded-write-date)))))

(defun %collect-xref-entries (symbol &key project-only)
  "Return SBCL's xref entries for SYMBOL as plists, deduplicated, in finder order.

Each plist carries :TYPE :CALLER :CALLER-SYMBOL :TRUENAME :PATH :LINE :CONTEXT
:FORM-INDEX and :STALE.  LINE points at the start of the enclosing definition
and FORM-INDEX is the first element of its DEFINITION-SOURCE-FORM-PATH, the
index of its top-level form in the file."
  (let* ((pkg (%ensure-sb-introspect))
         (path-fn (and pkg (find-symbol "DEFINITION-SOURCE-PATHNAME" pkg)))
         (offset-fn (and pkg (find-symbol "DEFINITION-SOURCE-CHARACTER-OFFSET" pkg)))
         (form-path-fn (and pkg (find-symbol "DEFINITION-SOURCE-FORM-PATH" pkg)))
         (write-date-fn (and pkg (find-symbol "DEFINITION-SOURCE-FILE-WRITE-DATE" pkg)))
         (seen (make-hash-table :test #'equal))
         (entries '()))
    (dolist (finder '("WHO-CALLS" "WHO-MACROEXPANDS" "WHO-BINDS" "WHO-REFERENCES" "WHO-SETS")
                    (nreverse entries))
      (let ((fn (and pkg (find-symbol finder pkg))))
        (when fn
          (dolist (source (ignore-errors (funcall fn symbol)))
            (let ((caller-name (and (consp source) (car source)))
                  (definition (if (consp source) (cdr source) source)))
              (multiple-value-bind (pathname path line)
                  (%definition->path/line definition path-fn offset-fn)
                (when (and path line
                           (or (not project-only) (%path-inside-project-p pathname)))
                  (let* ((type (%finder->type finder))
                         (caller (or (%format-xref-caller caller-name) ""))
                         (key (format nil "~A:~A:~A:~A" path line type caller)))
                    (unless (gethash key seen)
                      (setf (gethash key seen) t)
                      (let ((form-path (and form-path-fn
                                            (ignore-errors (funcall form-path-fn definition))))
                            (caller-symbol (%xref-caller-symbol caller-name)))
                        (push (list :type type
                                    :caller caller
                                    :caller-symbol (and caller-symbol
                                                        (qualified-symbol-name caller-symbol))
                                    :truename (%truename-string pathname)
                                    :path path
                                    :line line
                                    :context (or (%line-snippet pathname line) "")
                                    :form-index (and (consp form-path)
                                                     (integerp (first form-path))
                                                     (first form-path))
                                    :stale (%source-stale-p
                                            pathname
                                            (and write-date-fn
                                                 (ignore-errors
                                                  (funcall write-date-fn definition)))))
                              entries)))))))))))))

(defun %scan-status (entry scan)
  "Say whether SCAN, the parent's source scan, covered ENTRY's file.
Returns :SCANNED, :PARSE-FAILED or :NOT-SCANNED.  A truncated scan covers no
file for certain, so nothing is claimed about any."
  (let ((truename (getf entry :truename))
        (root (and scan (gethash "root" scan))))
    (cond
      ((or (null scan) (null root) (null truename)
           (gethash "skipped_reason" scan) (gethash "truncated_at" scan))
       :not-scanned)
      ((find truename (sequence->list (gethash "parse_failures" scan))
             :key (lambda (failure) (gethash "abs_path" failure))
             :test #'equal)
       :parse-failed)
      ((uiop:string-prefix-p root truename) :scanned)
      (t :not-scanned))))

(defun %scan-notes (scan)
  "Return the sentences saying what SCAN, the parent's source scan, missed."
  (if (null scan)
      (list "source scan not performed; call sites and top-level uses are unavailable")
      (let ((notes '())
            (failures (sequence->list (gethash "parse_failures" scan)))
            (skipped (gethash "skipped_reason" scan))
            (truncated (gethash "truncated_at" scan)))
        (when skipped
          (push (format nil "source scan skipped: ~A" skipped) notes))
        (when failures
          (push (format nil "~D file~:P could not be parsed and ~:[were~;was~] not scanned: ~
                             ~{~A~^, ~}~:[~;, ...~]"
                        (length failures)
                        (= 1 (length failures))
                        (mapcar (lambda (failure) (gethash "path" failure))
                                (subseq failures 0 (min 3 (length failures))))
                        (> (length failures) 3))
                notes))
        (when truncated
          (push (format nil "source scan stopped after ~D sites; results may be incomplete"
                        truncated)
                notes))
        (nreverse notes))))

(defun code-find-references-report (symbol-name &key package (project-only t) (limit 50)
                                                   scan)
  "Return the code-find-references payload for SYMBOL-NAME, without content text.

SCAN is the parent's source scan (CL-MCP/SRC/CODE-REFS-SCAN:SCAN-PROJECT),
built in-process or parsed back from JSON, or NIL.  The symbol is looked up
with FIND-SYMBOL only, so asking about a name that does not exist leaves no
trace.  Xref entries and scan sites are merged by
CL-MCP/SRC/CODE-REFS-CORE:MERGE-REFERENCES; the fields are those of
CL-MCP/SRC/CODE-REFS-CORE:BUILD-REFERENCES-REPORT."
  (multiple-value-bind (symbol status lookup-package lookup-name)
      (resolve-target symbol-name :package package)
    (let* ((scan-forms (and scan (sequence->list (gethash "forms" scan))))
           (common (list :symbol symbol-name
                         :lookup-package lookup-package
                         :lookup-name lookup-name
                         :project-only project-only
                         :limit limit
                         :files-scanned (or (and scan (gethash "files_scanned" scan)) 0)
                         :name-matches (loop for form in scan-forms
                                             sum (length (sequence->list
                                                          (gethash "sites" form))))
                         :scan-skipped (and scan (gethash "skipped_reason" scan)))))
      (if (not (eq status :found))
          (apply #'build-references-report :status status common)
          (let* ((kind (symbol-kind symbol))
                 (entries (mapcar (lambda (entry)
                                    (append entry
                                            (list :scan-status (%scan-status entry scan))))
                                  (%collect-xref-entries symbol :project-only project-only))))
            (multiple-value-bind (forms unresolved)
                (resolve-scan-forms scan-forms symbol :macro-p (equal kind "macro"))
              (apply #'build-references-report
                     :status :found
                     :resolved-symbol (qualified-symbol-name symbol)
                     :kind kind
                     :refs (merge-references entries forms)
                     :unresolved unresolved
                     :xref-count (length entries)
                     :notes (%scan-notes scan)
                     common)))))))
```

既存の `code-find-references` を `lisp-edit-form`（`replace`, `form_name: "code-find-references"`）で置き換える。直前の `(declaim ...)`（ftype 宣言）がこの関数用なら、ラムダリストが変わらないので残してよい:

```lisp
(defun code-find-references (symbol-name &key package (project-only t))
  "Return (values REFS COUNT) for SYMBOL-NAME: the reference objects of
CODE-FIND-REFERENCES-REPORT, computed without a source scan and without a
limit, and their number.  Each reference's LINE points at the start of the
enclosing definition and CALLER names it; see CODE-FIND-REFERENCES-REPORT for
call sites and the other fields."
  (let ((report (code-find-references-report symbol-name
                                             :package package
                                             :project-only project-only
                                             :limit most-positive-fixnum)))
    (values (gethash "refs" report) (gethash "count" report))))
```

- [ ] **Step 5: 成功を確認する**

`code-test` を実行。Expected: 追加 3 本と既存テストがすべて `✓`。

失敗したら、`repl-eval` でフィクスチャを `%load-xref-fixture` と同じ手順でロードし、`(sb-introspect:who-calls 'cl-mcp-xref-fixture:target)` の `definition-source-form-path` と `scan-project` の `"index"` を並べて確かめる。計画時の実測（spec 5.2）では両者は一致する。

- [ ] **Step 6: 既存テストの回帰を確認する**

`<name>` = `response-builders-test` と `worker-test` を実行。Expected: `build-code-find-references-response` はまだ旧シグネチャなので、`code.lisp` と `handlers.lisp` が `code-find-references`（互換ラッパー）経由で動き、既存テストは通る。

- [ ] **Step 7: Lint とコミット**

```bash
mallet src/code-core.lisp tests/code-test.lisp tests/fixtures/xref-fixture.lisp
git add src/code-core.lisp tests/code-test.lisp tests/fixtures/xref-fixture.lisp
git commit -m "feat(code): report references with call sites, top-level uses and tests

code-find-references-report merges SBCL xref with the source scan on the
top-level form index, and resolves the symbol without interning it.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016jJhPXZ53FRPgqhA3aT8m9"
```

---

### Task 6: 本文テキストの整形

**Files:**
- Modify: `src/tools/response-builders.lisp`（`build-code-find-references-response` を置き換え、補助関数を直前に追加）
- Modify: `tests/response-builders-test.lisp`（旧テスト 3 本を削除して新テストに置き換え、import 追加）

**Interfaces:**
- Consumes: Task 4 の report hash-table（キーは Task 4 の Produces）
- Produces: `(build-code-find-references-response report) → report`（同じ hash-table に `"content"` を設定して返す）。**シグネチャが変わる**ので、Task 7 で `code.lisp` と `handlers.lisp` を追従させるまで、この 2 ファイルは古い呼び出しのまま壊れる。Task 6 と Task 7 は連続して実施する

- [ ] **Step 1: 失敗するテストを書く**

`tests/response-builders-test.lisp` の `defpackage` に追加:

```lisp
  (:import-from #:cl-mcp/src/code-refs-core
                #:build-references-report)
```

`build-code-find-references-response-uses-caller`、`build-code-find-references-response-empty`、`build-code-find-references-response-lambda-caller-falls-back` の 3 本を削除し（`lisp-edit-form` で各 deftest を空文字に `replace` できない場合は、1 本目を下のコードで `replace` し、残り 2 本の `replace` の content を後続テストにする）、次を入れる:

```lisp
(defun %ref (&key (path "src/a.lisp") (line 10) (type "call") (caller "a") form-type
                  form-name (origin "xref+source") sites test note)
  "Return a reference object shaped like MERGE-REFERENCES' output."
  (make-ht "path" path "line" line "type" type "types" (vector type)
           "caller" caller "caller_symbol" nil "context" "(defun a"
           "form_type" form-type "form_name" form-name "origin" origin
           "call_sites" (coerce sites 'vector) "test" test "stale" nil "note" note))

(defun %site (line &key (kind "call") (context "(foo 1)") shadowed-by)
  "Return a call-site object shaped like MERGE-REFERENCES' output."
  (make-ht "line" line "column" 3 "kind" kind "context" context
           "shadowed_by" shadowed-by))

(defun %report (refs &rest overrides)
  "Return a found-symbol report for REFS; OVERRIDES win over the defaults."
  (apply #'build-references-report
         (append overrides
                 (list :symbol "p::foo" :resolved-symbol "P::FOO" :status :found
                       :kind "function" :project-only t :refs refs))))

(deftest build-code-find-references-response-shows-forms-sites-and-tests
  (testing "header, one line per form, its call sites, and the tests"
    (let* ((r (build-code-find-references-response
               (%report (list (%ref :form-type "defun" :form-name "a"
                                    :sites (list (%site 12)
                                                 (%site 13 :kind "quoted" :context "'foo")))
                              (%ref :path "tests/a-test.lisp" :line 5 :form-type "deftest"
                                    :form-name "a-test" :caller "(lambda)"
                                    :sites (list (%site 6))
                                    :test (make-ht "name" "a-test" "framework" "rove"))))))
           (text (first-text r)))
      (ok (search "P::FOO (function) — 2 forms in 2 files, 1 test" text))
      (ok (search "src/a.lisp:10 (defun a) [call]" text))
      (ok (search "  12: (foo 1)" text))
      (ok (search "  13 (quoted): 'foo" text))
      (ok (search "tests/a-test.lisp:5 (deftest a-test) [call] TEST" text))
      (ok (search "Tests: a-test" text))
      (ok (= 2 (gethash "count" r))))))

(deftest build-code-find-references-response-notes-and-caps
  (testing "notes follow the form line; call sites beyond five are counted"
    (let ((text (first-text
                 (build-code-find-references-response
                  (%report (list (%ref :form-type "defun" :form-name "a"
                                       :sites (loop for i from 1 to 7 collect (%site i)))
                                 (%ref :path "src/b.lisp" :origin "xref" :caller "hidden"
                                       :note "call not visible in source (produced by a macro expansion)")
                                 (%ref :path "src/c.lisp" :form-type "defun" :form-name "c"
                                       :sites (list (%site 3 :shadowed-by "flet")))))))))
      (ok (search "  +2 more" text))
      (ok (search "src/b.lisp:10 (used in hidden) [call] — call not visible in source" text))
      (ok (search "[shadowed by flet]" text)))))

(deftest build-code-find-references-response-hides-lambda-callers
  (testing "a lambda caller without a form falls back to path:line"
    (let ((text (first-text (build-code-find-references-response
                             (%report (list (%ref :caller "(lambda)" :origin "xref")))))))
      (ok (search "src/a.lisp:10 [call]" text))
      (ok (not (search "(lambda)" text))))))

(deftest build-code-find-references-response-truncates
  (testing "forms beyond the limit are counted, not listed"
    (let ((text (first-text (build-code-find-references-response
                             (%report (list (%ref :path "a.lisp") (%ref :path "b.lisp")
                                            (%ref :path "c.lisp"))
                                      :limit 2)))))
      (ok (search "— 3 forms in 3 files" text))
      (ok (search "… 1 more form (raise limit to see them)" text))
      (ok (not (search "c.lisp" text))))))

(deftest build-code-find-references-response-empty-and-missing
  (testing "no references says so and shows what was searched"
    (let ((text (first-text (build-code-find-references-response
                             (%report '() :files-scanned 147 :name-matches 0)))))
      (ok (search "P::FOO (function) — no references." text))
      (ok (search "xref: 0 entries   source scan: 147 files, 0 matches" text))))
  (testing "a missing symbol"
    (let ((text (first-text (build-code-find-references-response
                             (%report '() :status :not-found :resolved-symbol nil :kind nil
                                          :lookup-package "P" :lookup-name "NOPE"
                                          :name-matches 7)))))
      (ok (search "Symbol \"NOPE\" not found in P (nothing was interned)" text))
      (ok (search "7 textual matches" text))))
  (testing "a missing package"
    (let ((text (first-text (build-code-find-references-response
                             (%report '() :status :package-not-found
                                          :lookup-package "NOPE-PKG")))))
      (ok (search "Package \"NOPE-PKG\" not found" text))))
  (testing "unresolved matches and notes are listed"
    (let ((text (first-text (build-code-find-references-response
                             (%report (list (%ref))
                                      :unresolved (list (list :path "tests/x-test.lisp"
                                                              :package "X-TEST" :count 3
                                                              :tests '("x-test")))
                                      :notes '("source scan skipped: project root is not set"))))))
      (ok (search "+ 3 possible matches in files whose package is not loaded:" text))
      (ok (search "tests/x-test.lisp (X-TEST; tests: x-test)" text))
      (ok (search "Note: source scan skipped: project root is not set" text)))))
```

- [ ] **Step 2: 失敗を確認する**

`response-builders-test` を実行。Expected: `build-code-find-references-response` の引数の数が合わずエラー（`✗`）。

- [ ] **Step 3: 実装する**

`src/tools/response-builders.lisp` の `build-code-find-references-response` を `lisp-edit-form`（`replace`）で次の関数群に置き換える（補助関数を含めて一つの content にしてよい。`lisp-edit-form` が複数フォームを受け付けない場合は、`replace` で本体を置き換えてから `insert_before` で補助関数を足す）:

```lisp
(defparameter *references-sites-shown* 5
  "Call sites listed per form in code-find-references' text; the rest are counted.")

(defparameter *references-context-width* 120
  "Characters of source shown per call site in code-find-references' text.")

(defun %clip-context (text)
  "Return TEXT trimmed and cut to *REFERENCES-CONTEXT-WIDTH* characters."
  (let ((text (string-trim '(#\Space #\Tab) (or text ""))))
    (if (> (length text) *references-context-width*)
        (concatenate 'string (subseq text 0 (1- *references-context-width*)) "…")
        text)))

(defun %format-reference (stream ref)
  "Write REF, one code-find-references reference object, to STREAM."
  (let ((form-type (gethash "form_type" ref))
        (form-name (gethash "form_name" ref))
        (caller (gethash "caller" ref))
        (type (gethash "type" ref))
        (sites (coerce (or (gethash "call_sites" ref) #()) 'list)))
    (format stream "~A:~A" (gethash "path" ref) (gethash "line" ref))
    (cond
      ((and form-type form-name)
       (format stream " (~A ~A)" form-type form-name))
      ((and caller (plusp (length caller)) (string/= caller "(lambda)"))
       (format stream " (used in ~A)" caller))
      (form-type
       (format stream " (~A)" form-type)))
    (format stream " [~A]~:[~; TEST~]~@[ — ~A~]~%"
            type (gethash "test" ref) (gethash "note" ref))
    (loop for site in sites
          for shown below *references-sites-shown*
          do (let ((kind (gethash "kind" site)))
               (format stream "  ~A~:[ (~A)~;~*~]: ~A~@[  [shadowed by ~A]~]~%"
                       (gethash "line" site)
                       (equal kind type)
                       kind
                       (%clip-context (gethash "context" site))
                       (gethash "shadowed_by" site))))
    (when (> (length sites) *references-sites-shown*)
      (format stream "  +~D more~%" (- (length sites) *references-sites-shown*)))))

(defun %scan-summary (report)
  "Return a phrase saying what the source scan behind REPORT covered."
  (let ((skipped (gethash "scan_skipped" report))
        (files (or (gethash "files_scanned" report) 0))
        (matches (or (gethash "name_matches" report) 0)))
    (if skipped
        (format nil "skipped (~A)" skipped)
        (format nil "~D file~:P, ~D match~:[es~;~]" files matches (= matches 1)))))

(defun %format-references-report (report)
  "Return the content text for REPORT, a code-find-references payload."
  (let ((status (gethash "symbol_status" report))
        (refs (coerce (or (gethash "refs" report) #()) 'list))
        (count (or (gethash "count" report) 0))
        (tests (coerce (or (gethash "tests" report) #()) 'list))
        (unresolved (coerce (or (gethash "unresolved" report) #()) 'list))
        (notes (coerce (or (gethash "notes" report) #()) 'list))
        (matches (or (gethash "name_matches" report) 0)))
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
        ((zerop count)
         (format s "~A (~A) — no references.~%  xref: ~D entr~:@P   source scan: ~A~%"
                 (gethash "resolved_symbol" report) (gethash "symbol_kind" report)
                 (or (gethash "xref_count" report) 0) (%scan-summary report)))
        (t
         (format s "~A (~A) — ~D form~:P in ~D file~:P~[~:;, ~:*~D test~:P~]~%"
                 (gethash "resolved_symbol" report) (gethash "symbol_kind" report)
                 count (gethash "file_count" report) (length tests))
         (dolist (ref refs)
           (%format-reference s ref))
         (when (> count (length refs))
           (format s "… ~D more form~:P (raise limit to see them)~%"
                   (- count (length refs))))
         (when tests
           (format s "Tests: ~{~A~^, ~}~%"
                   (mapcar (lambda (test) (gethash "name" test)) tests)))))
      (when (and (member status '("not_found" "package_not_found") :test #'equal)
                 (plusp matches))
        (format s "~D textual match~:[es~;~] for that name in project files.~%"
                matches (= matches 1)))
      (when unresolved
        (let ((total (reduce #'+ unresolved :key (lambda (entry) (gethash "count" entry)))))
          (format s "+ ~D possible match~:[es~;~] in files whose package is not loaded:~%"
                  total (= total 1))
          (dolist (entry unresolved)
            (format s "  ~A (~A~@[; tests: ~{~A~^, ~}~]) — load that system to check them~%"
                    (gethash "path" entry)
                    (gethash "package" entry)
                    (coerce (or (gethash "tests" entry) #()) 'list)))))
      (dolist (note notes)
        (format s "Note: ~A~%" note)))))

(defun build-code-find-references-response (report)
  "Return REPORT, a CL-MCP/SRC/CODE-REFS-CORE:BUILD-REFERENCES-REPORT payload,
with its content text set.

The text is the only part an MCP client shows, so it carries everything a
caller needs to judge the impact of a change: one line per top-level form with
its type and name (ready for lisp-read-file or lisp-edit-form), the exact call
sites under it, a note when only one of xref and the source scan found a form,
the tests involved, and why nothing was found when that happens."
  (setf (gethash "content" report)
        (text-content (%format-references-report report)))
  report)
```

注: `%format-references-report` の `(coerce ... 'list)` を `~@[` に渡すので、テストが空リストなら `nil` になり「; tests:」は出ない。

- [ ] **Step 4: 成功を確認する**

`response-builders-test` を実行。Expected: 全 `✓`。

- [ ] **Step 5: Lint（コミットは Task 7 と一緒に行う）**

```bash
mallet src/tools/response-builders.lisp tests/response-builders-test.lisp
```

この時点では `code.lisp` と `handlers.lisp` が旧シグネチャで呼んでいて壊れているため、コミットせずに Task 7 に進む。

---

### Task 7: ツールと worker ハンドラの接続、文書

**Files:**
- Modify: `src/code.lisp`（defpackage と `code-find-references` の define-tool）
- Modify: `src/worker/handlers.lisp`（defpackage の import と `%handle-code-find-references`）
- Modify: `tests/tools-test.lisp`、`tests/worker-test.lisp`（テスト追加）
- Modify: `docs/tools.md`、`prompts/repl-driven-development.md`、`CLAUDE.md`

**Interfaces:**
- Consumes: Task 3 `scan-project`、Task 5 `code-find-references-report`、Task 6 `build-code-find-references-response (report)`
- Produces: MCP ツール `code-find-references` の新引数 `limit` と新レスポンス。worker メソッド `worker/code-find-references` の params に `"limit"` と `"scan"`

- [ ] **Step 1: 失敗するテストを書く**

`tests/tools-test.lisp` の `tools-call-code-find-references-project-only-true` の直後に追加:

```lisp
(deftest tools-call-code-find-references-reports-missing-symbols
  (testing "tools/call code-find-references says a symbol is missing, in its text"
    (let* ((req (concatenate 'string
                  "{\"jsonrpc\":\"2.0\",\"id\":25,\"method\":\"tools/call\","
                  "\"params\":{\"name\":\"code-find-references\","
                  "\"arguments\":{\"symbol\":\"cl-mcp/src/code-core::%no-such-symbol-xyz\"}}}"))
           (result (gethash "result" (parse (%pjl req))))
           (content (and result (gethash "content" result))))
      (ok (equal "not_found" (gethash "symbol_status" result)))
      (ok (and content
               (search "nothing was interned" (gethash "text" (elt content 0))))))))

(deftest tools-call-code-find-references-rejects-bad-arguments
  (testing "a non-positive limit and a keyword symbol are argument errors"
    (dolist (arguments '("{\"symbol\":\"cl:car\",\"limit\":0}"
                         "{\"symbol\":\":car\"}"))
      (let* ((req (concatenate 'string
                    "{\"jsonrpc\":\"2.0\",\"id\":26,\"method\":\"tools/call\","
                    "\"params\":{\"name\":\"code-find-references\","
                    "\"arguments\":" arguments "}}"))
             (obj (parse (%pjl req)))
             (result (gethash "result" obj)))
        (ok (or (gethash "error" obj)
                (and result (gethash "isError" result)))
            arguments)))))
```

`tests/worker-test.lisp` の `worker-code-find-references-returns-result` の直後に追加:

```lisp
(deftest worker-code-find-references-resolves-scan-sites
  (testing "worker/code-find-references resolves the sites the parent scanned"
    (with-handler-server (stream)
      (let ((site (make-hash-table :test 'equal))
            (form (make-hash-table :test 'equal))
            (scan (make-hash-table :test 'equal))
            (params (make-hash-table :test 'equal)))
        (setf (gethash "line" site) 3
              (gethash "column" site) 5
              (gethash "kind" site) "call"
              (gethash "token" site) "car"
              (gethash "context" site) "(car x)"
              (gethash "shadowed_by" site) nil)
        (setf (gethash "path" form) "virtual.lisp"
              (gethash "abs_path" form) "/virtual/virtual.lisp"
              (gethash "index" form) 0
              (gethash "start_line" form) 1
              (gethash "end_line" form) 4
              (gethash "form_type" form) "defun"
              (gethash "form_name" form) "virtual"
              (gethash "test_name" form) nil
              (gethash "test_framework" form) nil
              (gethash "in_package" form) "COMMON-LISP-USER"
              (gethash "context" form) "(defun virtual (x)"
              (gethash "sites" form) (vector site))
        (setf (gethash "target_name" scan) "CAR"
              (gethash "root" scan) "/virtual/"
              (gethash "files_scanned" scan) 1
              (gethash "files_matched" scan) 1
              (gethash "forms" scan) (vector form)
              (gethash "parse_failures" scan) (vector)
              (gethash "truncated_at" scan) nil
              (gethash "skipped_reason" scan) nil)
        (setf (gethash "symbol" params) "cl:car"
              (gethash "project_only" params) t
              (gethash "limit" params) 100000
              (gethash "scan" params) scan)
        (let* ((response (%send-and-receive stream 303 "worker/code-find-references" params))
               (result (%result-of response)))
          (ok result "handler returns a result")
          (when result
            (let ((ref (find "virtual" (coerce (gethash "refs" result) 'list)
                             :key (lambda (r) (gethash "form_name" r)) :test #'equal)))
              (ok ref "the scanned form survives the JSON round trip and is resolved")
              (when ref
                (ok (equal "source" (gethash "origin" ref)))
                (ok (= 3 (gethash "line" (elt (gethash "call_sites" ref) 0))))))))))))
```

- [ ] **Step 2: 失敗を確認する**

`<name>` = `tools-test` と `worker-test`。Expected: Task 6 のシグネチャ変更で `code.lisp` / `handlers.lisp` のコンパイルに失敗する、または新テストが `✗`。

- [ ] **Step 3: `src/code.lisp` を実装する**

`defpackage` を `lisp-edit-form`（`replace`, `form_type: "defpackage"`, `form_name: "cl-mcp/src/code"`）で置き換え:

```lisp
(defpackage #:cl-mcp/src/code
  (:use #:cl)
  (:import-from #:cl-mcp/src/code-core
                #:code-find-definition
                #:code-describe-symbol
                #:code-find-references
                #:code-find-references-report)
  (:import-from #:cl-mcp/src/code-refs-scan
                #:scan-project)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:arg-validation-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-code-find-response
                #:build-code-describe-response
                #:build-code-find-references-response)
  (:import-from #:cl-mcp/src/proxy
                #:with-proxy-dispatch)
  (:export
   #:code-find-definition
   #:code-describe-symbol
   #:code-find-references))
```

`(define-tool "code-find-references" ...)` を `lisp-edit-form`（`replace`, `form_type: "define-tool"`, `form_name: "code-find-references"`）で置き換え:

```lisp
(define-tool "code-find-references"
  :description "Find who calls or references a symbol - its callers, the exact call sites
inside them, and the tests that exercise it - to see what a change would affect
before making it (who-calls / who-references / impact analysis).

Combines SBCL xref (calls, macroexpands, binds, references, sets) with a scan of
the project's source, so it also reports:
- the exact line of every call site inside each caller ('call_sites')
- top-level uses xref never records, such as a defparameter initform or a macro
  used at top level (origin 'source')
- calls that exist only inside a macro expansion (origin 'xref')
- the deftest a reference sits in, and a 'Tests:' line listing them
Each result is one top-level form; its form_type and form_name can be passed
straight to lisp-read-file or lisp-edit-form.

PREREQUISITE: load the defining system first (load-system).  A symbol or package
that does not exist is reported as such, and nothing is interned.  'pkg:name'
also finds internal symbols.

LIMITS: matching inside a form is positional, not a code walker.  A flet, labels
or macrolet binding the same name is flagged as shadowing; other lexical bindings
are not.  Matches in files whose package is not loaded are listed as possible
matches instead of being resolved.

For plain text search without loading anything, use 'clgrep-search'."
  :args ((symbol :type :string :required t
                 :description "Symbol name like \"cl-mcp:run\" (package-qualified preferred)")
         (package :type :string
                  :description "Optional package used when SYMBOL is unqualified")
         (project-only :type :boolean :json-name "project_only" :default t
                       :description "When true (default), only include references under the project root")
         (limit :type :integer
                :description "Maximum number of forms listed (default 50); the total is always reported"))
  :body
  (progn
    ;; Checked here, before the scan and before any worker call, so a bad value
    ;; gets the same argument error with and without the worker pool.
    (when (and limit (not (and (integerp limit) (plusp limit))))
      (error 'arg-validation-error
             :arg-name "limit"
             :message "limit must be a positive integer"))
    ;; The scan runs in this (parent) process on both paths: it needs eclector,
    ;; which the worker image does not load.  It also validates SYMBOL.
    (let ((scan (scan-project symbol)))
      (with-proxy-dispatch (id "worker/code-find-references"
                              (make-ht "symbol" symbol
                                       "package" package
                                       "project_only" project-only
                                       "limit" (or limit 50)
                                       "scan" scan))
        (result id
                (build-code-find-references-response
                 (code-find-references-report symbol
                                              :package package
                                              :project-only project-only
                                              :limit (or limit 50)
                                              :scan scan)))))))
```

- [ ] **Step 4: `src/worker/handlers.lisp` を実装する**

defpackage の `(:import-from #:cl-mcp/src/code-core ...)` の `#:code-find-references` を `#:code-find-references-report` に置き換える（`lisp-patch-form`, `form_type: "defpackage"`, `form_name: "cl-mcp/src/worker/handlers"`, old_text `                #:code-find-references)`, new_text `                #:code-find-references-report)`）。

`%handle-code-find-references` を `lisp-edit-form`（`replace`）で置き換え:

```lisp
(defun %handle-code-find-references (params)
  "Find symbol references.  Returns the same structure as define-tool
\"code-find-references\".

PARAMS carries the parent's source scan under \"scan\"; this handler resolves
its sites against the symbols loaded in this image and merges them with xref."
  (let ((symbol (gethash "symbol" params))
        (package (gethash "package" params))
        (project-only (%bool-default params "project_only" t))
        (limit (or (gethash "limit" params) 50))
        (scan (gethash "scan" params)))
    (unless symbol
      (error "symbol is required"))
    (build-code-find-references-response
     (code-find-references-report symbol
                                  :package package
                                  :project-only project-only
                                  :limit limit
                                  :scan scan))))
```

- [ ] **Step 5: 成功を確認する**

`tools-test`、`worker-test`、`response-builders-test`、`code-test` を順に実行。Expected: すべて `✓`（`tools-call-code-find-references` と `worker-code-find-references-returns-result` の既存テストも通る）。

- [ ] **Step 6: 文書を更新する**

`docs/tools.md` の `## \`code-find-references\`` 節（次の `## \`clhs-lookup\`` の直前まで）を次で置き換える:

```markdown
## `code-find-references`
Find who calls or references a symbol — its callers, the exact call sites inside
them, and the tests involved — to judge what a change would affect. Combines SBCL
`sb-introspect` xref with a scan of the project's source.

Input:
- `symbol` (string, required): `pkg:name`, `pkg::name` or `name`; a single colon also finds internal symbols
- `package` (string, optional): package used when `symbol` is unqualified
- `project_only` (boolean, default `true`): limit xref results to files under the project root
- `limit` (integer, default `50`): most forms listed; `count` always gives the total

Output (the content text carries everything that matters for a decision):
- `symbol_status`: `found`, `not_found` or `package_not_found`; nothing is interned either way
- `resolved_symbol`, `symbol_kind` (`function`, `macro`, `generic-function`, `special-operator`, `variable`, `constant`, `unbound`), `lookup_package`, `lookup_name`
- `refs` (array): one element per top-level form, sorted by path and line
  - `path`, `line` (start of the form), `type` (first of `types`), `types`
  - `caller`, `caller_symbol` (package-qualified; null for lambdas and for forms xref did not see)
  - `form_type`, `form_name`: pass them to `lisp-read-file` (`name_pattern`) or `lisp-edit-form`
  - `origin`: `xref+source`; `xref` (the call exists only in a macro expansion, or the source was not scanned); `source` (a top-level use xref never records, or code not compiled since it was written)
  - `call_sites` (array): `line`, `column`, `kind` (`call`, `macro`, `function`, `quoted`, `template`, `bind`, `set`, `method`, `reference`), `context`, `shadowed_by`
  - `test`: `{name, framework}` when the form is a `deftest` (rove), `test`/`def-test` (fiveam) or `define-test` (parachute)
  - `stale`: the file changed after it was compiled; `note`: why a form lacks call sites or xref
- `count`, `file_count`, `limit`, `truncated`
- `tests` (array): `name`, `path`, `line` of every test among the references
- `unresolved` (array): `path`, `package`, `count`, `tests` for matches in files whose package is not loaded
- `notes` (array), `xref_count`, `files_scanned`, `name_matches`, `scan_skipped`, `project_only`, `symbol`

Limits: matching is positional, not a code walker. A `flet`/`labels`/`macrolet` binding the same
name is flagged in `shadowed_by`; other lexical bindings are not. The name position of any
`def...` form is treated as a definition. Sites after an `in-readtable` switch are not found.
```

`prompts/repl-driven-development.md`:
- Tool Cheat Sheet の `| Read definition | ... |` 行の直後に `| Callers / impact | \`code-find-references\` | \`symbol\` (load-system first) |` を追加
- 81 行目 `  - Find callers/references -> \`code-find-references\` (loaded) or \`clgrep-search\`` を `  - Find callers/references, call sites and affected tests -> \`code-find-references\` (loaded) or \`clgrep-search\`` に変更

`CLAUDE.md` 53 行目 `| Code Intel | \`src/code.lisp\` | Symbol lookup, describe, xref via sb-introspect |` を `| Code Intel | \`src/code.lisp\`, \`src/code-refs-scan.lisp\`, \`src/code-refs-core.lisp\` | Symbol lookup, describe; callers with call sites and tests (xref + source scan) |` に変更。

- [ ] **Step 7: Lint とコミット（Task 6 の分も含む）**

```bash
mallet src/code.lisp src/worker/handlers.lisp src/tools/response-builders.lisp \
       tests/tools-test.lisp tests/worker-test.lisp tests/response-builders-test.lisp
git add src/code.lisp src/worker/handlers.lisp src/tools/response-builders.lisp \
        tests/tools-test.lisp tests/worker-test.lisp tests/response-builders-test.lisp \
        docs/tools.md prompts/repl-driven-development.md CLAUDE.md
git commit -m "feat(code-find-references): call sites, top-level uses and tests in the tool

The parent scans the source, the worker resolves and merges it with xref, and
the content text lists each form with its call sites and a note when only one
side saw it. New limit argument; missing symbols are reported, not interned.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_016jJhPXZ53FRPgqhA3aT8m9"
```

---

### Task 8: 全体検証と実測

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
        -e '(uiop:quit 0)' 2>&1 | grep -i -A3 "code-refs\|code-core\|response-builders\|src/code.lisp\|handlers.lisp" | head -60
```
Expected: 今回触ったファイルに起因する WARNING / STYLE-WARNING がない（UIOP 由来の約 427 件は既存のノイズ）。

- [ ] **Step 3: 全テストスイート**

```bash
rove cl-mcp.asd 2>&1 | tee /tmp/claude-1000/-home-wiz--roswell-local-projects-cl-ai-project-cl-mcp/f1922901-fa82-4a7f-a3e0-350fcf4b1ea5/scratchpad/rove-full.log | tail -30
grep -c ";; testing '" /tmp/claude-1000/-home-wiz--roswell-local-projects-cl-ai-project-cl-mcp/f1922901-fa82-4a7f-a3e0-350fcf4b1ea5/scratchpad/rove-full.log
grep -n "✗" /tmp/claude-1000/-home-wiz--roswell-local-projects-cl-ai-project-cl-mcp/f1922901-fa82-4a7f-a3e0-350fcf4b1ea5/scratchpad/rove-full.log | head -20
```
Expected: `code-refs-core-test` と `code-refs-scan-test` が `;; testing` に含まれる。`✗` があれば名前を列挙し、今回の変更に関係するもの（code / refs / response-builders / tools / worker）はすべて直す。既存の無関係な失敗（通常 2 本）は名前を報告に記録する。終了コードは根拠にしない。

- [ ] **Step 4: 実物で計測する（dogfood）**

```bash
ros run -e '(asdf:load-asd (truename "cl-mcp.asd"))' \
        -e '(handler-bind ((warning (function muffle-warning))) (asdf:load-system :cl-mcp))' \
        -e '(setf cl-mcp/src/project-root:*project-root* (truename "./"))' \
        -e '(let ((cl-mcp/src/proxy:*use-worker-pool* nil))
              (dolist (sym (list "cl-mcp/src/tools/helpers:make-ht"
                                 "cl-mcp/src/tools/define-tool:define-tool"
                                 "cl-mcp/src/code-core:code-find-references"))
                (let* ((start (get-internal-real-time))
                       (resp (cl-mcp/src/protocol:process-json-line
                              (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"code-find-references\",\"arguments\":{\"symbol\":\"~A\"}}}" sym)))
                       (ms (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second))
                       (text (gethash "text" (elt (gethash "content" (gethash "result" (yason:parse resp))) 0))))
                  (format t "~&===== ~A: ~Dms, response ~D chars, text ~D chars~%~A~%"
                          sym ms (length resp) (length text) (subseq text 0 (min 2500 (length text)))))))' \
        -e '(uiop:quit 0)' 2>&1 | tail -120
```

Expected / 確認すること:
- `make-ht` の所要時間（目安: 数百 ms 以内）と応答サイズ
- `define-tool` がトップレベル使用として数十フォーム（origin `source`）報告される（計画前は 0 件）
- `code-find-references` の結果に `src/code.lisp` の `define-tool` と `src/worker/handlers.lisp` が呼び出し行つきで出る
- 結果（時間、サイズ、気付いた問題）を報告に書く。応答が極端に大きい・遅い場合は問題として報告する（その場で設計を変えない）

- [ ] **Step 5: 報告**

実行したコマンドと結果（Lint、警告、テストの ✓ 数と `;; testing` 数と ✗ の名前、dogfood の計測値）をまとめる。コミットは不要（問題を直した場合のみ、直したファイルをパス指定でコミット）。
