# 括弧診断の周辺ツール接続 実装計画

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** PR #141 の共通診断モジュール `paren-diagnostics` に、`clgrep-search` / `lisp-read-file` / `fs-write-file` / `lisp-check-parens` のスライス / 復旧手順の文言を接続し、壊れた Lisp ファイルに対してどのツールも無言で欠落させず、行番号と復旧手順を返すようにする。

**Architecture:** 新しい診断ロジックは書かない。`src/utils/clgrep.lisp` のスキャナは未終了フォームを flag 付きで返し、ツール層はそれを重複排除から外して注記を出す。`lisp-read-file` は `lisp-edit-form-core` から抽出する `make-file-unparseable-condition` / `signal-file-unparseable` を通して編集系 3 ツールと同じ `file-unparseable-error` を使う。`fs-write-file` は既存の `*lisp-file-unparseable-hook*` で書いた内容を検証する。`lisp-check-parens` は窓相対の行・列をファイル絶対に翻訳する。復旧手順は `format-overwrite-recovery` 1 関数の文言変更で 4 ツールに波及する。

**Tech Stack:** SBCL, ASDF package-inferred-system, Rove, cl-ppcre, Eclector (CST), uiop。仕様は `docs/superpowers/specs/2026-09-06-paren-diagnostics-peripheral-design.md`。

## Global Constraints

- **Lisp ソースの編集は `lisp-edit-form` / `lisp-patch-form` (MCP ツール) で行う。** テキスト編集ツールで `.lisp` を触らない (閉じ括弧のずれを parinfer が防ぐ)。Markdown (`prompts/*.md`, `docs/*.md`) は Edit ツールでよい。
- **新しい export をコードから参照するときは `defpackage` の `:import-from` に書き、`pkg:symbol` (コロン 1 個) で書かない。** 稼働中の MCP サーバの親プロセスは再起動まで古いイメージのままで、未 export のシンボルを `pkg:symbol` で含むファイルは `lisp-edit-form` が読めなくなる。`pkg::symbol` (コロン 2 個) は可。
- **テストは新規プロセスで回す。** 推奨は MCP `run-tests` ツール (`{"system": "cl-mcp/tests/<name>"}`; worker がテストシステムを強制リロードする)。シェルのフォールバックは
  ```bash
  ros -e '(ql:quickload :cl-mcp/tests/<name> :silent t)' \
      -e '(uiop:quit (if (rove:run :cl-mcp/tests/<name>) 0 1))'
  ```
  単一システムの緑は全スイートの緑を保証しない。最終タスクで `rove cl-mcp.asd` を回す。
- **本文優先。** ユーザーに見せる事実は `content[].text` に入れ、構造化フィールドは同じ事実の写しに留める。テストの本文 assert は `content[].text` に対して行う。
- **新ツール・新しい入力パラメータは追加しない。** 追加してよい出力フィールドは `unterminated` / `notes` (clgrep-search)、`window` (lisp-check-parens)、`unparseable` (fs-write-file)、`meta.unparseable` / `meta.unparseable_from_line` (lisp-read-file) のみ。
- **無言で欠落させない。** 部分結果を返すときはどこから先が読めていないかを本文で言う。
- スタイル: Google CL Style Guide、2 スペース、100 桁以内、公開関数に docstring、トップレベルフォーム間は空行 1 行。コミット前に `mallet src/*.lisp`。
- コミットメッセージは `paren-diagnostics-peripheral: <what>` で始め、末尾に次の 2 行を付ける:
  ```
  Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01BpidmyXVyFDkT2rYoHQ6y5
  ```
- ブランチは `feat/paren-diagnostics-peripheral` (既にチェックアウト済み)。
- 作業開始時に `fs-set-project-root` で `/home/wiz/.roswell/local-projects/cl-ai-project/cl-mcp` を設定する。
- `tests/tmp/` は gitignore 済みのスクラッチ領域。テストは自分が作ったファイルを `unwind-protect` で消す。

---

## ファイル構成

| ファイル | 責務 | タスク |
|---|---|---|
| `src/utils/clgrep.lisp` | 純粋スキャナ。`toplevel-form` に `unterminated-p`、EOF 時 push、結果に `:unterminated` | 1 |
| `src/clgrep.lisp` | ツール層。未終了フォームを重複排除から外す、注記、`unterminated`/`notes` | 2 |
| `src/validate.lisp` | `lisp-check-parens`。窓の行・列翻訳、`reader-info` 分岐の窓警告、`window` | 3 |
| `src/lisp-edit-form-core.lisp` | `make-file-unparseable-condition` / `signal-file-unparseable` の抽出、`file-unparseable-message` のプロジェクト外分岐 | 4 |
| `src/lisp-read-file.lisp` | truncated 検査、`error` 全体の捕捉、lenient 経路の第 2 値、ツール本体の `handler-case` | 5 |
| `src/paren-diagnostics.lisp` | `format-overwrite-recovery` の 2 段構え、`:fix-line` | 6 |
| `prompts/repl-driven-development.md` | Parenthesis Mismatch 節の復旧手順 | 6 |
| `src/fs.lisp` | 書き込み後検証、警告文、`unparseable` | 7 |
| `tests/clgrep-utils-test.lisp` `tests/clgrep-test.lisp` `tests/validate-test.lisp` `tests/lisp-edit-form-test.lisp` `tests/lisp-read-file-test.lisp` `tests/paren-diagnostics-test.lisp` `tests/fs-test.lisp` | 各タスクのテスト | 1-7 |

実装順は仕様 §7 の通り: 独立な Task 1-2 (4.1) と Task 3 (4.5) を先に、依存のある Task 4-5 (4.2) → Task 6 (4.4) → Task 7 (4.3) を後に。

---

### Task 1: スキャナが未終了フォームを返す (`src/utils/clgrep.lisp`)

**Files:**
- Modify: `src/utils/clgrep.lisp` (defpackage の export、`toplevel-form`、`scan-toplevel-forms` 末尾、`%find-form-for-line`、`search-in-file`)
- Test: `tests/clgrep-utils-test.lisp`

**Interfaces:**
- Produces: `toplevel-form-unterminated-p` (struct accessor, boolean)。`scan-toplevel-forms` が EOF で開いたままのフォームを `:end-pos (length content)`, `:unterminated-p t` で末尾に含める。`search-in-file` / `semantic-grep` の結果 alist に、未終了フォーム内のマッチだけ `(:unterminated . t)` が付く (健全なマッチには key 自体がない)。export 追加: `scan-toplevel-forms`, `toplevel-form-start-pos`, `toplevel-form-end-pos`, `toplevel-form-start-line`, `toplevel-form-end-line`, `toplevel-form-unterminated-p`。
- Consumes: なし。

- [ ] **Step 1: テストパッケージに import を足す**

`lisp-edit-form` で `tests/clgrep-utils-test.lisp` の `defpackage` (form_type `defpackage`, form_name `cl-mcp/tests/clgrep-utils-test`) を `replace`:

```lisp
(defpackage #:cl-mcp/tests/clgrep-utils-test
  (:use #:cl)
    (:import-from #:rove
                #:deftest
                #:testing
                #:ok
                #:signals)
  (:import-from #:cl-mcp/src/utils/clgrep
                #:grep-file
                #:extract-toplevel-form
                #:glob-to-regex
                #:collect-target-files
                #:target-file-p
                #:path-ignored-p
                #:extract-package-for-line
                #:semantic-grep
                #:extract-form-type-and-name
                #:extract-form-signature
                #:scan-toplevel-forms
                #:toplevel-form-start-pos
                #:toplevel-form-end-pos
                #:toplevel-form-start-line
                #:toplevel-form-end-line
                #:toplevel-form-unterminated-p))
```

- [ ] **Step 2: 失敗するテストを書く**

`lisp-edit-form` で `test-semantic-grep-user-defined-form-type` (form_type `deftest`) の後に `insert_after`:

```lisp
(deftest test-scan-toplevel-forms-keeps-unterminated-form
  (testing "a form still open at EOF is returned, flagged, and spans to the end"
    (let* ((content (format nil "(defun a ()~%  (list 1))~%~%(defun b ()~%  (list 2)~%~%~
                                 (defun c ()~%  (list 3))~%"))
           (forms (scan-toplevel-forms content)))
      (ok (= 2 (length forms)) "a closes; b swallows c, so two forms")
      (let ((a (first forms))
            (b (second forms)))
        (ok (null (toplevel-form-unterminated-p a)))
        (ok (= 1 (toplevel-form-start-line a)))
        (ok (toplevel-form-unterminated-p b))
        (ok (= 4 (toplevel-form-start-line b)))
        (ok (= (length content) (toplevel-form-end-pos b))
            "an unterminated form ends where the input ends")
        (ok (>= (toplevel-form-end-line b) 8)
            "and its line range covers the swallowed definition"))))
  (testing "EOF inside a string still yields the enclosing form, flagged"
    (let ((forms (scan-toplevel-forms
                  (format nil "(defun a ()~%  \"never closed~%(defun b () 1)~%"))))
      (ok (= 1 (length forms)))
      (ok (toplevel-form-unterminated-p (first forms)))))
  (testing "EOF inside a block comment does the same"
    (let ((forms (scan-toplevel-forms
                  (format nil "(defun a ()~%  #| never closed~%(defun b () 1)~%"))))
      (ok (= 1 (length forms)))
      (ok (toplevel-form-unterminated-p (first forms)))))
  (testing "a balanced input flags nothing"
    (let ((forms (scan-toplevel-forms (format nil "(defun a () 1)~%(defun b () 2)~%"))))
      (ok (= 2 (length forms)))
      (ok (notany #'toplevel-form-unterminated-p forms)))))

(defparameter *broken-dsl-file-content* "(in-package #:clgrep-dsl-demo)

(defun before-break (s)
  (probe-value s))

(defun swallowing (s)
  (probe-value s)

(defun after-break (s)
  (probe-value s))
"
  "The ) closing SWALLOWING's body (line 7) is missing, so it swallows AFTER-BREAK.")

(deftest test-semantic-grep-reports-matches-inside-an-unterminated-form
  (with-temp-dsl-project (dir *broken-dsl-file-content*)
    (let ((results (semantic-grep dir "probe-value" :include-form nil)))
      (testing "the match below the breakage is not dropped"
        (ok (= 3 (length results)))
        (ok (member 10 (mapcar (lambda (r) (cdr (assoc :line r))) results))
            "line 10 sits inside the swallowed definition"))
      (testing "matches inside the unterminated form are flagged, the one before it is not"
        (let ((before (find 4 results :key (lambda (r) (cdr (assoc :line r)))))
              (inside (find 10 results :key (lambda (r) (cdr (assoc :line r))))))
          (ok (null (assoc :unterminated before)) "healthy results carry no key at all")
          (ok (eq t (cdr (assoc :unterminated inside))))
          (ok (string= "swallowing" (cdr (assoc :form-name inside)))
              "attributed to the form that swallowed it, by design"))))))
```

- [ ] **Step 3: 失敗を確認する**

`run-tests` で `{"system": "cl-mcp/tests/clgrep-utils-test"}`。
Expected: `scan-toplevel-forms` / `toplevel-form-unterminated-p` が export されていないためロードまたは実行で失敗する。

- [ ] **Step 4: export と struct スロットを足す**

`lisp-patch-form` で `src/utils/clgrep.lisp` の `defpackage` (form_name `cl-mcp/src/utils/clgrep`):
- old_text: `           #:semantic-grep))`
- new_text:
```
           #:semantic-grep
           #:scan-toplevel-forms
           #:toplevel-form-start-pos
           #:toplevel-form-end-pos
           #:toplevel-form-start-line
           #:toplevel-form-end-line
           #:toplevel-form-unterminated-p))
```

`lisp-edit-form` で `defstruct toplevel-form` (form_type `defstruct`, form_name `toplevel-form`) を `replace`:

```lisp
(defstruct toplevel-form
  "Represents a top-level form with its position and line range.
UNTERMINATED-P is T when the form was still open at the end of the input
(a missing \")\", or an unterminated string or block comment inside it): its
END-POS is then the input length and END-LINE the last line, so that the
lines it swallowed still belong to a form."
  start-pos
  end-pos
  start-line
  end-line
  (unterminated-p nil))
```

- [ ] **Step 5: EOF 時に push する**

`lisp-patch-form` で `scan-toplevel-forms` (form_type `defun`):
- old_text: `    (nreverse forms)))`
- new_text:
```
    ;; A form still open at EOF swallows the rest of the file. Keep it, flagged,
    ;; so the lines it swallowed still belong to a form instead of vanishing
    ;; from every result; callers decide how to present it.
    (when form-start-pos
      (push (make-toplevel-form :start-pos form-start-pos
                                :end-pos len
                                :start-line form-start-line
                                :end-line current-line
                                :unterminated-p t)
            forms))
    (nreverse forms)))
```

- [ ] **Step 6: フォーム情報と結果に `:unterminated` を運ぶ**

`lisp-patch-form` で `%find-form-for-line` (form_type `defun`):
- old_text: `                (cons :end-byte (toplevel-form-end-pos form)))))))`
- new_text:
```
                (cons :end-byte (toplevel-form-end-pos form))
                (cons :unterminated (toplevel-form-unterminated-p form)))))))
```

`lisp-edit-form` で `search-in-file` (form_type `defun`) を `replace`:

```lisp
(defun search-in-file
       (filepath pattern &key case-insensitive form-types (include-form t))
  "Search for PATTERN in FILEPATH and return a list of match results.
   If CASE-INSENSITIVE is true, perform case-insensitive matching.
   If FORM-TYPES is a list of strings (e.g., '(\"defun\" \"defmethod\")),
   only include results where the form type matches.
   If INCLUDE-FORM is NIL, omit the :form field from results (saves tokens).
   Each result is an alist with file, line, match, package, signature, and optionally form.
   A match inside a form still open at the end of the file carries
   (:unterminated . t): the file does not parse there, and the form type,
   name and signature reported are those of the unclosed form, not of the
   definition the match sits in. Healthy matches carry no such key.

   Pre-computes toplevel form map and package map once per file for O(n)
   performance instead of O(n*m) where m is the number of matches."
  (let ((results nil))
    (handler-case
     (let ((content (uiop/stream:read-file-string filepath))
           (scanner
            (if case-insensitive
                (cl-ppcre:create-scanner pattern :case-insensitive-mode t)
                pattern)))
       ;; Pre-compute caches once per file (avoids O(n*m) re-scanning)
       (let ((forms-cache (scan-toplevel-forms content))
             (package-map (%build-package-map content)))
         (with-input-from-string (stream content)
           (loop for line = (read-line stream nil nil)
                 for line-number from 1
                 while line
                 when (scan scanner line)
                 do (let ((package (%find-package-for-line package-map line-number))
                          (form-info (%find-form-for-line
                                      forms-cache content line-number)))
                      (when form-info
                        (let* ((full-form-text
                                (subseq content
                                        (cdr (assoc :start-byte form-info))
                                        (cdr (assoc :end-byte form-info))))
                               (type-info
                                (extract-form-type-and-name full-form-text))
                               (form-type (cdr (assoc :type type-info)))
                               (form-name (cdr (assoc :name type-info)))
                               (signature
                                (extract-form-signature full-form-text)))
                          (when
                              (or (null form-types)
                                  (member form-type form-types :test
                                          #'string-equal))
                            (let ((result
                                   (list (cons :file (namestring filepath))
                                         (cons :line line-number)
                                         (cons :match line)
                                         (cons :package (or package "UNKNOWN"))
                                         (cons :form-type form-type)
                                         (cons :form-name form-name)
                                         (cons :signature signature)
                                         (cons :form-start-line
                                               (cdr
                                                (assoc :start-line form-info)))
                                         (cons :form-end-line
                                               (cdr (assoc :end-line form-info)))
                                         (cons :form-start-byte
                                               (cdr
                                                (assoc :start-byte form-info)))
                                         (cons :form-end-byte
                                               (cdr
                                                (assoc :end-byte form-info))))))
                              (when include-form
                                (setf result
                                        (append result
                                                (list
                                                 (cons :form
                                                       (cdr
                                                        (assoc :text
                                                               form-info)))))))
                              ;; Only when true, so a healthy result carries
                              ;; no key and the JSON payload gains none.
                              (when (cdr (assoc :unterminated form-info))
                                (setf result
                                        (append result
                                                (list (cons :unterminated t)))))
                              (push result results))))))))))
     (error (condition)
            (format *error-output* "Warning: Could not read ~A: ~A~%" filepath
                    condition)))
    (nreverse results)))
```

- [ ] **Step 7: テストが通ることを確認する**

`run-tests` で `{"system": "cl-mcp/tests/clgrep-utils-test"}`。
Expected: 全テスト PASS (既存テストを含む)。

- [ ] **Step 8: リントしてコミット**

```bash
mallet src/utils/clgrep.lisp
git add src/utils/clgrep.lisp tests/clgrep-utils-test.lisp
git commit -m "paren-diagnostics-peripheral: keep unterminated forms in the clgrep scanner

scan-toplevel-forms dropped a form still open at EOF, so every line it
swallowed belonged to no form and grep-file-structured discarded the
matches there without a word. Push it flagged (unterminated-p) with the
input length as its end, and carry (:unterminated . t) on the matches
inside it so the tool layer can present them.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01BpidmyXVyFDkT2rYoHQ6y5"
```

---

### Task 2: ツール層が飲み込まれたマッチを本文に出し、注記する (`src/clgrep.lisp`)

**Files:**
- Modify: `src/clgrep.lisp` (`%format-clgrep-results`、新関数 `%unparseable-notes`、`define-tool "clgrep-search"` の body)
- Test: `tests/clgrep-test.lisp`

**Interfaces:**
- Consumes: Task 1 の `(:unterminated . t)` と `:form-start-line`。
- Produces: `content[].text` に未終了フォーム内のマッチが 1 行ずつ出る。各ファイルに `NOTE: <file> does not parse: a form opened at line N is never closed. ...` が 1 つ。ペイロード `matches[i].unterminated` (true のときだけ)、`notes` (注記のベクタ、あるときだけ)。

- [ ] **Step 1: 失敗するテストを書く**

`lisp-edit-form` で `clgrep-search-path-default-and-relative-unchanged` (form_type `deftest`) の後に `insert_after`:

```lisp
(defparameter *broken-source*
  (format nil "~{~A~%~}"
          (list "(in-package #:cl-user)"
                ""
                "(defun openerp (ch)"
                "  (list ch))"
                ""
                "(defun matching-closer (ch)"
                "  (list ch)"
                ""
                "(defmacro tokenize (text)"
                "  (list text))"))
  "MATCHING-CLOSER (line 6) is missing its closing ), so it swallows TOKENIZE (line 9).")

(defparameter *healthy-source*
  (format nil "~{~A~%~}"
          (list "(defun healthy-one ()"
                "  (list :alpha)"
                "  (list :alpha))"
                ""
                "(defun healthy-two () :beta)"))
  "Balanced; HEALTHY-ONE matches \"alpha\" and \"list\" on two lines each.")

(defmacro with-broken-fixture ((relative-dir) &body body)
  "Create RELATIVE-DIR under the cl-mcp project root holding broken.lisp and
healthy.lisp, bind *project-root* to that root, run BODY, delete the directory."
  `(let* ((*project-root* (asdf:system-source-directory :cl-mcp))
          (dir (uiop:ensure-directory-pathname
                (merge-pathnames ,relative-dir *project-root*))))
     (ensure-directories-exist dir)
     (unwind-protect
          (progn
            (with-open-file (out (merge-pathnames "broken.lisp" dir)
                                 :direction :output :if-exists :supersede)
              (write-string *broken-source* out))
            (with-open-file (out (merge-pathnames "healthy.lisp" dir)
                                 :direction :output :if-exists :supersede)
              (write-string *healthy-source* out))
            ,@body)
       (uiop:delete-directory-tree dir :validate t))))

(defun %call-clgrep (pattern &rest kvs)
  "Call the clgrep-search tool handler with PATTERN plus KVS argument pairs and
return (VALUES text payload): the rendered summary and the result hash."
  (let* ((args (apply #'cl-mcp/src/tools/helpers:make-ht "pattern" pattern kvs))
         (response (cl-mcp/src/clgrep::clgrep-search-handler
                    (cl-mcp/src/state:make-state) 1 args))
         (payload (gethash "result" response))
         (content (and payload (gethash "content" payload))))
    (values (and content (plusp (length content)) (gethash "text" (aref content 0)))
            payload)))

(deftest clgrep-search-tool-lists-matches-inside-an-unterminated-form
  (with-broken-fixture ("tests/tmp/clgrep-broken/")
    (multiple-value-bind (text payload)
        (%call-clgrep "def(un|macro) (openerp|matching-closer|tokenize)"
                      "path" "tests/tmp/clgrep-broken/")
      (testing "every definition is visible in the text, the swallowed one included"
        (ok (search "broken.lisp:3 [defun] (openerp ch)" text))
        (ok (search "broken.lisp:6 [defun] (matching-closer ch)" text))
        (ok (search "broken.lisp:9 [defun] (matching-closer ch)" text)
            "the line inside the unterminated form gets its own line in the text"))
      (testing "the note names the file and the line where the unclosed form opens"
        (ok (search "NOTE: broken.lisp does not parse: a form opened at line 6" text))
        (ok (search "form type and signature are those of the unclosed form" text))
        (ok (search "lisp-check-parens" text)))
      (testing "the payload carries the same facts"
        (let* ((matches (gethash "matches" payload))
               (swallowed (find 9 matches :key (lambda (m) (gethash "line" m)))))
          (ok swallowed)
          (ok (eq t (gethash "unterminated" swallowed)))
          (ok (= 1 (length (gethash "match_lines" swallowed))))
          (ok (= 1 (length (gethash "notes" payload)))))))))

(deftest clgrep-search-tool-form-types-cannot-see-into-an-unterminated-form
  (with-broken-fixture ("tests/tmp/clgrep-broken-types/")
    (testing "a swallowed defmacro is invisible to a form_types filter, by design"
      (multiple-value-bind (text payload)
          (%call-clgrep "defmacro tokenize" "path" "tests/tmp/clgrep-broken-types/"
                        "form_types" (vector "defmacro"))
        (ok (= 0 (gethash "count" payload)))
        (ok (search "0 matches" text))))
    (testing "without the filter it is found, attributed to the form that swallowed it"
      (multiple-value-bind (text payload)
          (%call-clgrep "defmacro tokenize" "path" "tests/tmp/clgrep-broken-types/")
        (ok (= 1 (gethash "count" payload)))
        (ok (search "broken.lisp:9 [defun] (matching-closer ch)" text))
        (ok (search "NOTE: broken.lisp does not parse" text))))))

(deftest clgrep-search-tool-healthy-files-keep-grouping-and-get-no-note
  (with-broken-fixture ("tests/tmp/clgrep-healthy/")
    (multiple-value-bind (text payload)
        (%call-clgrep "alpha" "path" "tests/tmp/clgrep-healthy/")
      (testing "two matching lines in one balanced form are still one entry"
        (ok (= 1 (gethash "count" payload)))
        (ok (= 2 (length (gethash "match_lines" (aref (gethash "matches" payload) 0))))))
      (testing "no note when the search touched no unterminated form"
        (ok (null (search "NOTE:" text)))
        (ok (null (gethash "notes" payload)))))))

(deftest clgrep-search-tool-broken-file-does-not-affect-a-healthy-neighbour
  (with-broken-fixture ("tests/tmp/clgrep-mixed/")
    (multiple-value-bind (text payload)
        (%call-clgrep "list" "path" "tests/tmp/clgrep-mixed/")
      (let ((healthy (remove-if-not (lambda (m) (search "healthy.lisp" (gethash "file" m)))
                                    (coerce (gethash "matches" payload) 'list))))
        (ok (= 1 (length healthy)) "healthy-one's two list lines still group into one entry")
        (ok (notany (lambda (m) (gethash "unterminated" m)) healthy)))
      (ok (= 1 (/ (length (cl-ppcre:all-matches "NOTE:" text)) 2))
          "exactly one note, for the one broken file")
      (ok (= 1 (length (gethash "notes" payload)))))))
```

- [ ] **Step 2: 失敗を確認する**

`run-tests` で `{"system": "cl-mcp/tests/clgrep-test"}`。
Expected: `broken.lisp:9` が本文に現れない、`NOTE:` がない、`notes` が nil、で失敗。

- [ ] **Step 3: 重複排除を未終了フォームに適用しない**

`lisp-edit-form` で `%format-clgrep-results` (form_type `defun`) を `replace`:

```lisp
(defun %format-clgrep-results (results)
  "Convert clgrep results (list of alists) to a vector of hash tables.
Deduplicates results by (file, form-start-byte): when a single form
contains multiple pattern matches, the form appears once with a
MATCH_LINES array listing all individual (line, match) pairs.
A result inside an unterminated form (the file does not parse, and the
form swallowed the rest of it) is never grouped: that \"form\" is an
artifact of the breakage, and folding the swallowed definitions into one
entry would hide them from the text summary, which prints one line per
entry. Each such result keeps its own entry and a one-element MATCH_LINES."
  (let ((groups (make-hash-table :test #'equal))
        (order nil))
    (dolist (result results)
      (let* ((file (cdr (assoc :file result)))
             (form-start-byte (cdr (assoc :form-start-byte result)))
             (key (if (cdr (assoc :unterminated result))
                      (list file form-start-byte (cdr (assoc :line result)))
                      (cons file form-start-byte))))
        (unless (gethash key groups)
          (push key order))
        (push result (gethash key groups))))
    (map 'vector
         (lambda (key)
           (let* ((matches (nreverse (gethash key groups)))
                  (representative (alist-to-hash-table (first matches)))
                  (match-lines
                   (map 'vector
                        (lambda (m)
                          (let ((ht (make-hash-table :test #'equal)))
                            (setf (gethash "line" ht)
                                  (cdr (assoc :line m)))
                            (setf (gethash "match" ht)
                                  (cdr (assoc :match m)))
                            ht))
                        matches)))
             (setf (gethash "match_lines" representative) match-lines)
             representative))
         (nreverse order))))
```

- [ ] **Step 4: 注記関数を足す**

`lisp-edit-form` で `%format-clgrep-results` の後に `insert_after`:

```lisp
(defun %unparseable-notes (results)
  "Return one note string per file in RESULTS that holds a match inside an
unterminated form, in first-seen order. The note names the line where the
unclosed form opens and says how the matches below it are attributed, so a
caller can tell a swallowed definition from a real one and knows that a
form_types filter will not find it. Files without such a match get no note:
a broken file that matched nothing is not this search's problem."
  (let ((seen (make-hash-table :test #'equal))
        (notes nil))
    (dolist (result results)
      (when (cdr (assoc :unterminated result))
        (let ((file (cdr (assoc :file result))))
          (unless (gethash file seen)
            (setf (gethash file seen) t)
            (push (format nil "NOTE: ~A does not parse: a form opened at line ~D is never ~
                               closed.~%  Matches at or below that line are listed ~
                               individually; their form type and signature are those ~
                               of the unclosed form, not of the definition they sit ~
                               in. Run lisp-check-parens for the fix."
                          file (cdr (assoc :form-start-line result)))
                  notes)))))
    (nreverse notes)))
```

- [ ] **Step 5: ツール本体に注記を出す**

`lisp-edit-form` で `define-tool "clgrep-search"` (form_type `define-tool`, form_name `clgrep-search`) を `replace`。`:description` と `:args` は現行のまま、`:body` を差し替える:

```lisp
(define-tool "clgrep-search"
  :description "Perform semantic grep search for a pattern in Lisp files.
Unlike regular grep, this tool understands Lisp structure and returns
the top-level form signature containing each match.

KEY ADVANTAGE: Works WITHOUT loading systems - faster and no side effects.
Use this as the FIRST choice for code exploration before code-find/code-describe.

Default: Returns signatures only (token-efficient, ~70% reduction vs full forms).
Use 'include_form: true' to get complete form text when needed.

A file that does not parse (a form left open to the end of the file) is still
searched: matches inside the unclosed form are listed one per line, attributed
to that form, and a NOTE names the file and the line where it opens.

Recommended workflow:
1. clgrep-search to locate functions/usages across the project
2. lisp-read-file with name_pattern to read specific definitions in detail"
  :args ((pattern :type :string :required t
                  :description "cl-ppcre regular expression pattern to search for")
         (path :type :string
               :description "Search root directory (optional, defaults to project root).
Relative paths resolve against the project root. An absolute path is accepted when it is
inside the project root or inside the source directory of a registered ASDF system, so a
dependency's sources can be searched the same way lisp-read-file can read them.")
         (recursive :type :boolean :default t
                    :description "Search subdirectories recursively (default: true)")
         (case-insensitive :type :boolean :json-name "case_insensitive"
                           :description "Case-insensitive matching (default: false)")
         (form-types :type :array :json-name "form_types"
                     :description "Filter by form types, e.g., [\"defun\", \"defmethod\"] (optional)")
         (limit :type :integer
                :description "Maximum number of results to return (optional, defaults to 200)")
         (include-form :type :boolean :json-name "include_form"
                       :description "Include full form text in results (default: false, returns signatures only)"))
  :body
  (let* ((effective-limit (or limit 200))
         (results
          (clgrep-search pattern
                         :path path
                         :recursive recursive
                         :case-insensitive case-insensitive
                         :form-types form-types
                         :limit limit
                         :include-form include-form))
         (formatted (%format-clgrep-results results))
         (notes (%unparseable-notes results))
         (payload
          (make-ht "content" (text-content
                              (with-output-to-string (s)
                                (format s "~D ~:[matches~;match~] for ~S~@[ in ~A~]:~%"
                                        (length formatted) (= 1 (length formatted))
                                        pattern path)
                                (loop for match across formatted
                                      do (format s "  ~A:~A [~A] ~A~%"
                                                 (gethash "file" match)
                                                 (gethash "line" match)
                                                 (gethash "form-type" match)
                                                 (or (gethash "signature" match)
                                                     (gethash "form-name" match))))
                                ;; Notes go in the text: sibling JSON fields
                                ;; are not rendered by most clients.
                                (dolist (note notes)
                                  (format s "~A~%" note))))
                   "matches" formatted
                   "count" (length formatted)
                   "limited" (<= effective-limit (length results)))))
    (when notes
      (setf (gethash "notes" payload) (coerce notes 'vector)))
    (result id payload)))
```

- [ ] **Step 6: テストが通ることを確認する**

`run-tests` で `{"system": "cl-mcp/tests/clgrep-test"}` と `{"system": "cl-mcp/tests/clgrep-utils-test"}`。
Expected: 全テスト PASS。

- [ ] **Step 7: リントしてコミット**

```bash
mallet src/clgrep.lisp
git add src/clgrep.lisp tests/clgrep-test.lisp
git commit -m "paren-diagnostics-peripheral: list matches inside an unterminated form

%format-clgrep-results grouped every match by its enclosing form and
printed one text line per group, so once the scanner kept an unterminated
form, every definition it swallowed would have folded into that one line.
Exempt such forms from grouping, print a NOTE per broken file naming the
line where the form opens and the form_types limitation, and mirror both
in the payload (unterminated, notes).

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01BpidmyXVyFDkT2rYoHQ6y5"
```

---

### Task 3: `lisp-check-parens` の窓 (`src/validate.lisp`)

**Files:**
- Modify: `src/validate.lisp` (新関数 `%window-start` `%file-line` `%file-column`、`lisp-check-parens` への 4 パッチ、`define-tool "lisp-check-parens"` への 2 パッチ)
- Test: `tests/validate-test.lisp`

**Interfaces:**
- Produces: `position.line` / `position.column` が `offset` 指定時もファイル絶対 (区切り文字失敗・リーダーエラーの両方)。窓 (`partial`) のとき結果ハッシュとペイロードに `window` = `{"offset", "length", "first_line"}`。リーダーエラー + 窓のとき `diagnosis_text` に窓警告が入り、ツール本文にも付く。
- Consumes: なし。

- [ ] **Step 1: 失敗するテストを書く**

`lisp-edit-form` で `lisp-check-parens-limit-equal-to-a-multibyte-file-is-not-a-window` (form_type `deftest`) の後に `insert_after`:

```lisp
(defparameter *window-fixture*
  (format nil "~{~A~%~}"
          (list "(defun a ()"       ; line 1: offsets 0-10, newline at 11
                "  (list 1 2))"     ; line 2: offsets 12-24, newline at 25
                ""                  ; line 3: offset 26
                "(defun b ()"       ; line 4: offsets 27-37, newline at 38
                "  (list 3))"))     ; line 5: offsets 39-49, newline at 50
  "A balanced two-definition file, 51 characters long. Windows into it look
broken in ways the whole file is not.")

(defparameter *window-comma-fixture*
  (format nil "~{~A~%~}"
          (list "(defun a ()"            ; offsets 0-10, newline at 11
                "  \"Hello, world\")"))  ; line 2 from offset 12; the comma is offset 20
  "Balanced; the only comma sits inside a docstring.")

(defmacro with-window-fixture ((abs-var text) &body body)
  "Write TEXT to tests/tmp/check-parens-window-abs.lisp under the project root,
bind its absolute pathname to ABS-VAR and *project-root* to the root, run BODY,
then delete the file."
  `(let* ((root (asdf:system-source-directory :cl-mcp))
          (,abs-var (merge-pathnames "tests/tmp/check-parens-window-abs.lisp" root))
          (cl-mcp/src/project-root:*project-root* root))
     (ensure-directories-exist ,abs-var)
     (with-open-file (out ,abs-var :direction :output :if-exists :supersede)
       (write-string ,text out))
     (unwind-protect
          (progn ,@body)
       (ignore-errors (delete-file ,abs-var)))))

(deftest lisp-check-parens-window-positions-are-file-absolute
  (with-window-fixture (abs *window-fixture*)
    (testing "a window starting at a line start reports that line, column unchanged"
      ;; Offset 27 is the "(" of "(defun b ()"; 11 characters cover just that line.
      (let ((res (lisp-check-parens :path (namestring abs) :offset 27 :limit 11)))
        (ok (string= (%kind res) "unclosed"))
        (ok (= 4 (%pos res "line")) "line 1 of the window is line 4 of the file")
        (ok (= 1 (%pos res "column")))
        (let ((window (gethash "window" res)))
          (ok window "a window carries its own descriptor")
          (ok (= 27 (gethash "offset" window)))
          (ok (= 11 (gethash "length" window)))
          (ok (= 4 (gethash "first_line" window))))))
    (testing "a window starting mid-line adds the characters before it to the column"
      ;; Offset 20 is the "1" in "  (list 1 2))": the window is "1 2))" plus the
      ;; newline, and its first ")" (window column 4) is column 12 of line 2.
      (let ((res (lisp-check-parens :path (namestring abs) :offset 20 :limit 6)))
        (ok (string= (%kind res) "extra-close"))
        (ok (= 2 (%pos res "line")))
        (ok (= 12 (%pos res "column")))
        (ok (= 23 (%pos res "offset")) "the offset was already absolute")
        (ok (= 2 (gethash "first_line" (gethash "window" res))))))
    (testing "the prefix is measured by streaming, so the fs read cap does not matter"
      (let ((cl-mcp/src/fs::*fs-read-max-bytes* 16))
        (let ((res (lisp-check-parens :path (namestring abs) :offset 27 :limit 11)))
          (ok (= 4 (%pos res "line"))))))
    (testing "a whole-file check carries no window descriptor"
      (let ((res (lisp-check-parens :path (namestring abs))))
        (ok (%ok? res))
        (ok (null (gethash "window" res)))))))

(deftest lisp-check-parens-window-reader-error-is-flagged-and-positioned
  (with-window-fixture (abs *window-comma-fixture*)
    ;; Offset 20 is the comma: the window ", wo" balances, and the reader then
    ;; trips over a comma outside any backquote -- an artifact of the window.
    (let ((res (lisp-check-parens :path (namestring abs) :offset 20 :limit 4))
          (inline (lisp-check-parens :code ", wo")))
      (testing "the reader error is reported"
        (ok (null (%ok? res)))
        (ok (string= (%kind res) "reader-error"))
        (ok (string= (%kind inline) "reader-error")))
      (testing "at the file's line and column, not the window's"
        (ok (= 2 (%pos res "line")))
        (ok (= (+ 8 (%pos inline "column")) (%pos res "column"))
            "eight characters precede the window on line 2"))
      (testing "and the text says it may be an artifact of the window"
        (let ((text (gethash "diagnosis_text" res)))
          (ok text)
          (ok (search "Only a window" text))
          (ok (search "artifact" text)))
        (ok (gethash "window" res))))
    (testing "the tool summary carries the window warning for a reader error"
      (let* ((state (cl-mcp/src/state:make-state))
             (args (cl-mcp/src/tools/helpers:make-ht "path" (namestring abs)
                                                     "offset" 20 "limit" 4))
             (response (cl-mcp/src/validate::lisp-check-parens-handler state "cp-w" args))
             (text (gethash "text" (aref (gethash "content" (gethash "result" response)) 0))))
        (ok (search "Reader error at line 2" text))
        (ok (search "Only a window" text))
        (ok (gethash "window" (gethash "result" response)))))))
```

- [ ] **Step 2: 失敗を確認する**

`run-tests` で `{"system": "cl-mcp/tests/validate-test"}`。
Expected: 新テストが `line` 1 (窓相対) と `window` nil で失敗する。

- [ ] **Step 3: 翻訳ヘルパーを足す**

`lisp-edit-form` で `%fix->hash` (form_type `defun`) の後に `insert_after`:

```lisp
(defun %window-start (path offset)
  "Return two values for the window of PATH that begins at character OFFSET:
the number of newlines before it and the number of characters between the
last of those newlines (or the start of the file) and the window. A failure
reported at window line L, column C is at file line L + newlines and, on the
first window line only, column C + that character count. The prefix is read
one character at a time up to the same FILE-POSITION %READ-FILE-STRING seeks
to, so the count stops exactly where the window starts even in a multibyte
file, and no buffer is built, so the fs read cap does not apply. Returns
(VALUES 0 0) for OFFSET 0 or when the file cannot be read."
  (if (or (null offset) (zerop offset))
      (values 0 0)
      (handler-case
          (with-open-file (in (fs-resolve-read-path path)
                              :direction :input :element-type 'character)
            (let ((lines 0)
                  (col 0))
              (loop for ch = (and (< (file-position in) offset)
                                  (read-char in nil nil))
                    while ch
                    do (if (char= ch #\Newline)
                           (setf lines (1+ lines)
                                 col 0)
                           (incf col)))
              (values lines col)))
        (error () (values 0 0)))))

(defun %file-line (line window-start)
  "Translate window-relative LINE (1-based) to a file line using WINDOW-START,
the (newlines characters) list built from %WINDOW-START. NIL stays NIL."
  (and line (+ line (first window-start))))

(defun %file-column (line column window-start)
  "Translate window-relative COLUMN on window LINE to a file column. Only the
window's first line begins mid-line; every later line starts where the file's
does, so only there the characters before the window are added."
  (and column
       (if (eql line 1)
           (+ column (second window-start))
           column)))
```

- [ ] **Step 4: `lisp-check-parens` に 4 つのパッチを当てる**

すべて `lisp-patch-form` (form_type `defun`, form_name `lisp-check-parens`)。順に、各パッチ後に `would_change` を確認する。

パッチ 4-1 (窓の起点を束縛):
- old_text:
```
         (partial (and path
                       (or (plusp base-off)
                           (and limit (= (length text) limit) remaining)))))
```
- new_text:
```
         (partial (and path
                       (or (plusp base-off)
                           (and limit (= (length text) limit) remaining))))
         ;; Where the window starts in the file (newlines before it, characters
         ;; since the last one), so every line and column below is the file's.
         (window-start (if (and path (plusp base-off))
                           (multiple-value-list (%window-start path base-off))
                           (list 0 0))))
```

パッチ 4-2 (`window` フィールド):
- old_text:
```
        (let ((h (make-hash-table :test #'equal)))
          (cond
            ((not ok)
```
- new_text:
```
        (let ((h (make-hash-table :test #'equal)))
          (when partial
            ;; For a client that reads the payload and never the text: the
            ;; verdict below describes this window, not the whole file.
            (setf (gethash "window" h)
                  (make-ht "offset" base-off
                           "length" (length text)
                           "first_line" (1+ (first window-start)))))
          (cond
            ((not ok)
```

パッチ 4-3 (区切り文字失敗の位置):
- old_text:
```
               (setf (gethash "offset" pos) offset
                     (gethash "line" pos) line
                     (gethash "column" pos) column)
```
- new_text:
```
               (setf (gethash "offset" pos) offset
                     (gethash "line" pos) (%file-line line window-start)
                     (gethash "column" pos) (%file-column line column window-start))
```

パッチ 4-4 (リーダーエラーの位置と窓警告):
- old_text:
```
               (when r-line   (setf (gethash "line" pos) r-line))
               (when r-col    (setf (gethash "column" pos) r-col))
               (setf (gethash "position" h) pos)))
```
- new_text:
```
               (when r-line
                 (setf (gethash "line" pos) (%file-line r-line window-start)))
               (when r-col
                 (setf (gethash "column" pos) (%file-column r-line r-col window-start)))
               (setf (gethash "position" h) pos))
             ;; A window starts wherever the offset fell -- inside a string or
             ;; a comment as likely as not -- so what the reader trips over
             ;; there is often the window's own edge, not the file's fault.
             (when partial
               (setf (gethash "diagnosis_text" h)
                     (format nil "Only a window of ~A was checked (offset ~D, ~D ~
                                  characters). A reader error in a window is often ~
                                  an artifact of where the window starts (inside a ~
                                  string or a comment, say), so treat it as a hint ~
                                  only; check the whole file before acting on it."
                             path base-off (length text)))))
```

- [ ] **Step 5: ツール本体に 2 つのパッチを当てる**

`lisp-patch-form` (form_type `define-tool`, form_name `lisp-check-parens`)。

パッチ 5-1 (リーダーエラーの summary に `diagnosis_text` を付ける):
- old_text:
```
                      (if (string= kind "reader-error")
                          (format nil "Reader error~@[ at line ~D~]~@[, column ~D~]: ~A"
                                  line col (or message "unknown"))
```
- new_text:
```
                      (if (string= kind "reader-error")
                          (format nil "Reader error~@[ at line ~D~]~@[, column ~D~]: ~A~@[~%~A~]"
                                  line col (or message "unknown")
                                  (gethash "diagnosis_text" check-result))
```

パッチ 5-2 (ペイロードに `window`):
- old_text:
```
              (when (gethash "false_positive" check-result)
                (setf (gethash "false_positive" payload) t)))
            (result id payload)))
```
- new_text:
```
              (when (gethash "false_positive" check-result)
                (setf (gethash "false_positive" payload) t))
              (let ((window (gethash "window" check-result)))
                (when window
                  (setf (gethash "window" payload) window))))
            (result id payload)))
```

- [ ] **Step 6: `lisp-check-parens` の docstring を更新する**

`lisp-patch-form` (form_type `defun`, form_name `lisp-check-parens`):
- old_text:
```
readtable can fix gets the overwrite path as its next step. A window into a
file (OFFSET, or a LIMIT with input remaining) is diagnosed for its kind only."
```
- new_text:
```
readtable can fix gets the overwrite path as its next step. A window into a
file (OFFSET, or a LIMIT with input remaining) is diagnosed for its kind only;
it carries a \"window\" hash (offset, length, first_line), its line and column
are translated to the file's with %WINDOW-START, and a reader error found in
it is flagged in \"diagnosis_text\" as a likely artifact of the window."
```

- [ ] **Step 7: `lisp-check-parens` を `lisp-check-parens` で確認する**

`lisp-check-parens` に `path: "src/validate.lisp"` を渡し、`Parentheses are balanced` を確認する。

- [ ] **Step 8: テストが通ることを確認する**

`run-tests` で `{"system": "cl-mcp/tests/validate-test"}`。
Expected: 全テスト PASS (既存の窓テスト `lisp-check-parens-file-guidance-and-windows` を含む)。

- [ ] **Step 9: リントしてコミット**

```bash
mallet src/validate.lisp
git add src/validate.lisp tests/validate-test.lisp
git commit -m "paren-diagnostics-peripheral: make lisp-check-parens window positions absolute

scan-delimiters and %try-reader-check report line and column relative to
the text they were given, and for a window (offset) lisp-check-parens
passed them on unchanged, so \"line 1\" meant the window's first line.
Measure the prefix by streaming up to the window's file-position
(newlines and characters since the last one), translate both branches'
positions, add the window descriptor to the payload, and give a reader
error found in a window the same window warning a delimiter failure had.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01BpidmyXVyFDkT2rYoHQ6y5"
```

---

### Task 4: core に条件の生成関数を抽出し、プロジェクト外の分岐を足す (`src/lisp-edit-form-core.lisp`)

**Files:**
- Modify: `src/lisp-edit-form-core.lisp` (defpackage の export、新関数 `make-file-unparseable-condition` `signal-file-unparseable`、`%locate-target-form`、`file-unparseable-message`)
- Test: `tests/lisp-edit-form-test.lisp`

**Interfaces:**
- Produces:
  - `(make-file-unparseable-condition abs text cause &key readtable editable-prefix)` → `file-unparseable-error` 条件 (signal しない)。`abs` は絶対パス名、`text` はファイル全文、`cause` はリーダーまたは `parse-top-level-forms` が返した条件。
  - `(signal-file-unparseable abs text cause &key readtable editable-prefix)` → 上を `error` する。戻らない。
  - `file-unparseable-message` は、`recoverable` かつパスがプロジェクトルート外なら復旧手順を付けず「outside the project root ... fix it outside cl-mcp」と言う。
- Consumes: なし。編集系 3 ツールの挙動は変えない。

- [ ] **Step 1: テストパッケージに import を足す**

`lisp-edit-form` で `tests/lisp-edit-form-test.lisp` の `defpackage` (form_name `cl-mcp/tests/lisp-edit-form-test`) を `replace`:

```lisp
(defpackage #:cl-mcp/tests/lisp-edit-form-test
  (:use #:cl)
    (:import-from #:rove
                #:deftest
                #:testing
                #:ok
                #:ng
                #:skip)
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%normalize-string
                #:file-unparseable-error
                #:file-unparseable-message
                #:make-file-unparseable-condition)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file)
  (:import-from #:asdf
                #:system-source-directory)
  (:import-from #:uiop
                #:merge-pathnames*
                #:native-namestring
                #:ensure-directories-exist))
```

- [ ] **Step 2: 失敗するテストを書く**

`lisp-edit-form` で `file-unparseable-after-in-readtable-switch` (form_type `deftest`) の後に `insert_after`:

```lisp
(deftest file-unparseable-message-outside-project-root-promises-no-overwrite
  (testing "a recoverable file the project root does not contain gets no fs-write-file path"
    (let* ((root (system-source-directory :cl-mcp))
           (narrow (merge-pathnames* "tests/tmp/narrow-root/" root))
           (outside (merge-pathnames* "tests/tmp/outside-broken.lisp" root))
           (text (format nil "(defun a ()~%  (list 1)~%")))
      (ensure-directories-exist narrow)
      (with-open-file (out outside :direction :output :if-exists :supersede)
        (write-string text out))
      (unwind-protect
           (let* ((cl-mcp/src/project-root:*project-root* narrow)
                  (cause (handler-case
                             (progn (cl-mcp/src/cst:parse-top-level-forms text) nil)
                           (error (e) e)))
                  (message (file-unparseable-message
                            (make-file-unparseable-condition (truename outside) text cause))))
             (ok cause "the fixture must fail to parse")
             (ok (search "outside the project root" message))
             (ok (search "fix it outside cl-mcp" message))
             (ng (search "fs-write-file (path=" message)
                 "no overwrite instruction for a path fs-write-file would reject")
             (ok (search "unclosed (form starting at line 1" message)
                 "the diagnosis itself is still given"))
        (ignore-errors (delete-file outside)))))
  (testing "the same file under the project root keeps the recovery path"
    (let* ((root (system-source-directory :cl-mcp))
           (inside (merge-pathnames* "tests/tmp/inside-broken.lisp" root))
           (text (format nil "(defun a ()~%  (list 1)~%")))
      (with-open-file (out inside :direction :output :if-exists :supersede)
        (write-string text out))
      (unwind-protect
           (let* ((cl-mcp/src/project-root:*project-root* root)
                  (cause (handler-case
                             (progn (cl-mcp/src/cst:parse-top-level-forms text) nil)
                           (error (e) e)))
                  (message (file-unparseable-message
                            (make-file-unparseable-condition (truename inside) text cause))))
             (ok (search "fs-write-file (path=\"tests/tmp/inside-broken.lisp\"" message))
             (ng (search "outside the project root" message)))
        (ignore-errors (delete-file inside))))))
```

- [ ] **Step 3: 失敗を確認する**

`run-tests` で `{"system": "cl-mcp/tests/lisp-edit-form-test"}`。
Expected: `make-file-unparseable-condition` が未定義でロードまたは実行に失敗する。

- [ ] **Step 4: export を足す**

`lisp-patch-form` で `defpackage` (form_name `cl-mcp/src/lisp-edit-form-core`):
- old_text: `           #:file-unparseable-message))`
- new_text:
```
           #:file-unparseable-message
           #:make-file-unparseable-condition
           #:signal-file-unparseable))
```

- [ ] **Step 5: 生成関数を足す**

`lisp-edit-form` で `%locate-target-form` (form_type `defun`) の前に `insert_before`:

```lisp
(defun make-file-unparseable-condition (abs text cause &key readtable editable-prefix)
  "Return a FILE-UNPARSEABLE-ERROR for the file at ABS whose TEXT failed to
parse with CAUSE, the condition PARSE-TOP-LEVEL-FORMS signalled or returned as
its second value. Under a caller-supplied READTABLE the standard delimiter scan
is not evidence (a reader macro may consume raw parentheses), so no scan-based
diagnosis or recoverable verdict is attached; the message explains the
situation instead. EDITABLE-PREFIX says the lenient pass returned the forms
before the breakage, which lisp-edit-form can still address. This is the one
place the classification is made: %LOCATE-TARGET-FORM signals the condition
through SIGNAL-FILE-UNPARSEABLE, and lisp-read-file renders its message under
the forms it could still show."
  (make-condition 'file-unparseable-error
                  :path (namestring abs)
                  :readtable readtable
                  :editable-prefix editable-prefix
                  :diagnosis (if readtable
                                 (list :ok t)
                                 (diagnose-delimiters text))
                  :recoverable (and (null readtable)
                                    (%delimiter-failure-p cause))
                  :cause (sanitize-condition-text cause)))

(defun signal-file-unparseable (abs text cause &key readtable editable-prefix)
  "Signal the FILE-UNPARSEABLE-ERROR MAKE-FILE-UNPARSEABLE-CONDITION builds for
ABS, TEXT and CAUSE. Never returns."
  (error (make-file-unparseable-condition abs text cause
                                         :readtable readtable
                                         :editable-prefix editable-prefix)))
```

- [ ] **Step 6: `%locate-target-form` を生成関数に切り替える**

`lisp-edit-form` で `%locate-target-form` (form_type `defun`) を `replace`:

```lisp
(defun %locate-target-form (file-path form-type form-name readtable)
  "Shared prologue: resolve paths, read file, parse, find target, extract snippet.
Signals FILE-UNPARSEABLE-ERROR (through SIGNAL-FILE-UNPARSEABLE, which owns the
classification), carrying a delimiter diagnosis, when the file cannot be parsed
at all, or when the target form is not found and the lenient CL-reader pass
(after an IN-READTABLE switch, or under a READTABLE argument) stopped early on
a read error; on that lenient pass the forms before the breakage remain
editable, whereas the Eclector pass yields no forms at all from a file that
does not parse. A file larger than the fs read cap is reported as such
instead, because its truncated prefix would only yield a misleading delimiter
diagnosis.
Returns eight values:
  ABS — absolute pathname
  REL — relative namestring for FS write
  ORIGINAL — full file text
  NODES — parsed CST nodes
  TARGET — matched CST node
  TARGET-SNIPPET — text of the matched form
  FORM-TYPE-STR — downcased form-type string
  FILE-PACKAGE-NAME — package named by the file's first IN-PACKAGE form"
  (let ((form-type-str (string-downcase form-type)))
    (multiple-value-bind (abs rel)
        (%normalize-paths file-path)
      (multiple-value-bind (original truncated file-length)
          (fs-read-file abs)
        (when truncated
          (error "~A exceeds the read limit (~@[~D bytes, ~]only ~D characters read); ~
                  lisp-edit-form and lisp-patch-form cannot edit files this large, ~
                  and fs-write-file will not overwrite it either (a truncated read ~
                  cannot prove the file is broken). Split the file or edit it ~
                  outside cl-mcp."
                 (namestring abs) file-length (length original)))
        (multiple-value-bind (nodes swallowed)
            (handler-case
                (parse-top-level-forms original
                                       :readtable readtable
                                       :source-path abs)
              (error (e)
                (signal-file-unparseable abs original e :readtable readtable)))
          (let ((target (%find-target nodes form-type-str form-name)))
            (unless target
              (when swallowed
                ;; The lenient pass returned the forms before the breakage,
                ;; which lisp-edit-form can still address.
                (signal-file-unparseable abs original swallowed
                                        :readtable readtable
                                        :editable-prefix (and nodes t)))
              (error "Form ~A ~A not found in ~A" form-type form-name
                     (namestring abs)))
            (let ((target-snippet (subseq original
                                          (cst-node-start target)
                                          (cst-node-end target))))
              (values abs rel original nodes target target-snippet form-type-str
                      (extract-in-package-name-from-text original)))))))))
```

- [ ] **Step 7: `file-unparseable-message` にプロジェクト外の分岐を足す**

`lisp-edit-form` で `file-unparseable-message` (form_type `defun`) を `replace`:

```lisp
(defun file-unparseable-message (condition)
  "Return the guidance text for CONDITION, a FILE-UNPARSEABLE-ERROR.
When the failure is recoverable (a delimiter problem no readtable can fix),
the text opens with the shared delimiter diagnosis, or the reader error when
the scan has nothing to add, and ends with an executable recovery path
(FORMAT-OVERWRITE-RECOVERY: read, hand-apply the fix, fs-write-file, which
permits overwriting such a file). A recoverable file outside the project root
-- a dependency's source, which lisp-read-file can read but fs-write-file
cannot write -- gets no recovery path, since fs-write-file would refuse the
absolute path; the text says to fix it outside cl-mcp. When the caller
supplied a readtable, no standard-syntax verdict exists: the text names the
readtable and says how the overwrite guard will decide. Otherwise the failure
is reader-level (custom reader syntax, a disabled #. form); the file keeps its
overwrite protection, so the text points at the readtable parameter instead."
  (let* ((path (file-unparseable-path condition))
         (diagnosis (file-unparseable-diagnosis condition))
         (readtable (file-unparseable-readtable condition))
         (scan-ok (getf diagnosis :ok))
         (line (getf diagnosis :unclosed-form-line))
         (fixes (getf diagnosis :likely-fixes))
         (head (if scan-ok
                   (format nil "Cannot parse ~A~@[ under readtable ~(~S~)~]: ~A"
                           path readtable (file-unparseable-cause condition))
                   (format-delimiter-diagnosis diagnosis :target path))))
    (cond
      ((file-unparseable-recoverable-p condition)
       ;; fs-write-file takes only a project-relative path, so that is the
       ;; form the instruction gives; the absolute one stays in the head.
       (let* ((root (ignore-errors
                     (ensure-directory-pathname
                      (truename (ensure-directory-pathname *project-root*)))))
              (relative (and root
                             (subpathp (pathname path) root)
                             (ignore-errors
                              (namestring (enough-pathname (pathname path) root))))))
         (if relative
             (format nil "~A~%The file itself does not parse~:[, so lisp-edit-form and ~
                          lisp-patch-form cannot locate any form in it~; past its ~
                          broken form: the forms before it can still be edited with ~
                          lisp-edit-form, but this one is in the broken tail~].~%~
                          Run lisp-check-parens with path=~S to see the full diagnosis, ~
                          then ~A"
                     head (file-unparseable-editable-prefix-p condition) path
                     (format-overwrite-recovery relative
                                                :have-fix (not (null fixes))
                                                :where "above"
                                                :form-line line))
             ;; Outside the project root: neither the structural tools nor
             ;; fs-write-file can touch it, so no recovery path is promised.
             (format nil "~A~%The file does not parse, and it is outside the project ~
                          root, so fs-write-file cannot rewrite it and lisp-edit-form ~
                          cannot locate any form in it; fix it outside cl-mcp."
                     head))))
      (readtable
       (format nil "~A~%No standard-syntax diagnosis is offered under a custom ~
                    readtable (a reader macro may consume raw parentheses). Run ~
                    lisp-check-parens with path=~S: if it reports a missing or stray ~
                    parenthesis and the file uses no custom syntax at that point, ~
                    fs-write-file with allow_unparseable_overwrite=true can rewrite ~
                    it (that guard judges the file with the default reader); ~
                    otherwise fix the custom syntax the reader complained about."
               head path))
      (t
       ;; The reader stopped on something other than a delimiter. When the
       ;; scan also found a delimiter problem, both are shown: the reader's
       ;; complaint is what blocks parsing, and the diagnosis above may be a
       ;; second, real problem or an artifact of custom syntax.
       (format nil "~A~@[~%The reader itself reported: ~A.~]~%~
                    ~:[This~;That reader-level failure~] is not a missing or ~
                    stray parenthesis, so the overwrite path does not apply and ~
                    fs-write-file keeps refusing to overwrite the file. It may ~
                    depend on a readtable: if the file uses custom reader macros, ~
                    pass the readtable parameter (a named-readtable designator) to ~
                    lisp-edit-form / lisp-patch-form~:[.~; -- the delimiter ~
                    diagnosis above comes from the standard-syntax scan and may ~
                    then turn out to be right, or to be that syntax.~]"
               head
               (and (not scan-ok) (file-unparseable-cause condition))
               (not scan-ok)
               (not scan-ok))))))
```

- [ ] **Step 8: テストが通ることを確認する**

`run-tests` で `{"system": "cl-mcp/tests/lisp-edit-form-test"}`、`{"system": "cl-mcp/tests/lisp-patch-form-test"}`、`{"system": "cl-mcp/tests/lisp-macroexpand-test"}`。
Expected: すべて PASS (既存の `file-unparseable-*` テストが変わらず通ることがリファクタの回帰防止)。

- [ ] **Step 9: リントしてコミット**

```bash
mallet src/lisp-edit-form-core.lisp
git add src/lisp-edit-form-core.lisp tests/lisp-edit-form-test.lisp
git commit -m "paren-diagnostics-peripheral: share the file-unparseable constructor

The rules that classify a parse failure (readtable given: no scan verdict;
recoverable iff a delimiter failure; cause sanitized) lived in a flet
inside %locate-target-form. Extract make-file-unparseable-condition and
signal-file-unparseable so lisp-read-file can use them without a second
copy. Also teach file-unparseable-message that a recoverable file outside
the project root (a dependency's source, which only lisp-read-file
reaches) cannot be sent to fs-write-file, whose path must be relative.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01BpidmyXVyFDkT2rYoHQ6y5"
```

---

### Task 5: `lisp-read-file` を共通診断に接続する (`src/lisp-read-file.lisp`)

**Files:**
- Modify: `src/lisp-read-file.lisp` (defpackage、`%format-lisp-file`、`%lisp-read-file-content`、`define-tool "lisp-read-file"`)
- Test: `tests/lisp-read-file-test.lisp`

**Interfaces:**
- Consumes: Task 4 の `make-file-unparseable-condition` / `signal-file-unparseable` / `file-unparseable-message` / `file-unparseable-diagnosis`。
- Produces: collapsed 読みで
  - 読み取り上限超え → `exceeds the read limit` の `error`。
  - Eclector 経路の失敗 (欠落・余分・リーダーレベルすべて) → `file-unparseable-error`。
  - lenient 経路の途中失敗 → 読めたフォームの表示の後ろに `file-unparseable-message`、`meta.unparseable` = t、`meta.unparseable_from_line`。
  - ツール本体は `file-unparseable-error` を `tool-error` で返し、`Internal error during` を出さない。

- [ ] **Step 1: テストパッケージに import を足す**

`lisp-edit-form` で `tests/lisp-read-file-test.lisp` の `defpackage` (form_name `cl-mcp/tests/lisp-read-file-test`) を `replace`:

```lisp
(defpackage #:cl-mcp/tests/lisp-read-file-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng
                #:skip)
  (:import-from #:cl-mcp/src/lisp-read-file
                #:lisp-read-file)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:file-unparseable-error)
  (:import-from #:cl-mcp/src/fs
                #:fs-write-file
                #:fs-resolve-read-path)
  (:import-from #:asdf
                #:system-source-directory)
  (:import-from #:uiop
                #:ensure-directory-pathname)
  (:import-from #:cl-ppcre
                #:scan))
```

- [ ] **Step 2: 失敗するテストを書く**

`lisp-edit-form` で `lisp-read-file-content-pattern-sees-backquote-reader-syntax` (form_type `deftest`) の後に `insert_after`:

```lisp
(defparameter *missing-close-source*
  (format nil "~{~A~%~}"
          (list "(in-package #:cl-user)"
                ""
                "(defun probe-a (x)"
                "  (let ((y (* x 2)))"
                "    (if (> y 10)"
                "        (format t \"big\")"
                "        (format t \"small\")"
                "    y))"
                ""
                "(defun probe-c (x)"
                "  (list x x x))"))
  "PROBE-A (line 3) never closes: line 8 needs one more ), so PROBE-C is swallowed.")

(defparameter *stray-close-source*
  (format nil "~{~A~%~}"
          (list "(defun a ()"
                "  (list 1)))"
                ""
                "(defun b () 2)"))
  "One ) too many at the end of line 2.")

(defun %unparseable-message (path &rest keys)
  "Call lisp-read-file on PATH with KEYS and return the file-unparseable-error
message it signals, or NIL when it returns normally."
  (handler-case (progn (apply #'lisp-read-file path keys) nil)
    (file-unparseable-error (e) (princ-to-string e))))

(defun %read-file-tool-text (path &rest kvs)
  "Call the lisp-read-file tool handler on PATH with KVS argument pairs at the
2025-11-25 protocol and return (VALUES text is-error)."
  (let* ((state (cl-mcp/src/state:make-state))
         (args (apply #'cl-mcp/src/tools/helpers:make-ht "path" path kvs)))
    (setf (cl-mcp/src/state:protocol-version state) "2025-11-25")
    (let* ((response (cl-mcp/src/lisp-read-file::lisp-read-file-handler state 1 args))
           (payload (gethash "result" response))
           (content (and payload (gethash "content" payload))))
      (values (and content (plusp (length content)) (gethash "text" (aref content 0)))
              (and payload (gethash "isError" payload))))))

(deftest lisp-read-file-broken-file-gets-the-shared-diagnosis
  (testing "a missing ) is reported with the form, the likely fix and the recovery path"
    (with-temp-lisp-file "tests/tmp/read-file-missing-close.lisp" *missing-close-source*
      (lambda (path)
        (let ((message (%unparseable-message path)))
          (ok message "collapsed reading of a broken file signals file-unparseable-error")
          (ok (search "unclosed (form starting at line 3: \"(defun probe-a (x)\")" message))
          (ok (search "Likely fix, inferred from indentation:" message))
          (ok (search "Next top-level form probably begins at line 10" message))
          (ok (search "fs-write-file" message)
              "the recovery path is executable with cl-mcp tools alone")
          (ng (search "use lisp-check-parens)" message) "the old one-line hint is gone")))))
  (testing "a stray ) takes the same path instead of escaping as an internal error"
    (with-temp-lisp-file "tests/tmp/read-file-stray-close.lisp" *stray-close-source*
      (lambda (path)
        (let ((message (%unparseable-message path)))
          (ok message "stray-right-parenthesis is not end-of-file, and must still be caught")
          (ok (search "extra \")\" at line 2" message))
          (multiple-value-bind (text is-error) (%read-file-tool-text path)
            (ok is-error)
            (ok (search "extra \")\"" text))
            (ng (search "Internal error during" text))))))))

(deftest lisp-read-file-lenient-path-shows-the-prefix-and-says-where-it-stops
  (if (%try-load :named-readtables)
      (with-temp-lisp-file "tests/tmp/read-file-lenient-break.lisp"
          (format nil "~{~A~%~}"
                  (list "(in-package #:cl-user)"
                        "(named-readtables:in-readtable :standard)"
                        ""
                        "(defun before-break () 1)"
                        ""
                        "(defun after-break ()"
                        "  (list 1)"
                        ""
                        "(defun never-seen () 3)"))
        (lambda (path)
          (let* ((result (lisp-read-file path))
                 (content (gethash "content" result))
                 (meta (gethash "meta" result)))
            (testing "the forms before the breakage are still shown"
              (ok (search "(defun before-break ()" content)))
            (testing "and the breakage is named below them, never silently dropped"
              (ok (search "unclosed (form starting at line 6" content))
              (ok (search "forms before it can still be edited" content)
                  "the message knows the prefix is editable")
              (ok (eq t (gethash "unparseable" meta)))
              (ok (eql 6 (gethash "unparseable_from_line" meta))))
            (testing "the swallowed definition is not presented as a form"
              (ng (search ": (defun never-seen" content))))))
      (skip "named-readtables not available")))

(deftest lisp-read-file-broken-file-outside-the-project-root-gets-no-overwrite-path
  (testing "a dependency's broken source is diagnosed but not sent to fs-write-file"
    (let* ((root (system-source-directory :cl-mcp))
           (narrow (merge-pathnames "tests/tmp/narrow-root/" root))
           (outside (merge-pathnames "tests/tmp/outside-broken.lisp" root)))
      (ensure-directories-exist narrow)
      (with-open-file (out outside :direction :output :if-exists :supersede)
        (write-string *stray-close-source* out))
      (unwind-protect
           ;; The file sits under the cl-mcp system's source directory, which the
           ;; read policy allows, but outside the (narrowed) project root.
           (let* ((cl-mcp/src/project-root:*project-root* narrow)
                  (message (%unparseable-message (namestring outside))))
             (ok message)
             (ok (search "extra \")\"" message) "the diagnosis is still given")
             (ok (search "outside the project root" message))
             (ng (search "fs-write-file (path=" message)
                 "no overwrite instruction for a path fs-write-file would reject"))
        (ignore-errors (delete-file outside))))))

(deftest lisp-read-file-truncated-read-is-not-diagnosed
  (testing "a valid file larger than the read cap is reported as too large, not as broken"
    (with-temp-lisp-file "tests/tmp/read-file-large-valid.lisp"
        (format nil "(defun target ()~%  (list 1 2 3 4 5 6 7 8 9 10))~%")
      (lambda (path)
        (let ((message (handler-case
                           (let ((cl-mcp/src/fs::*fs-read-max-bytes* 16))
                             (lisp-read-file path)
                             nil)
                         (error (e) (princ-to-string e)))))
          (ok message "the read is cut, so reading must fail")
          (ok (search "exceeds the read limit" message))
          (ng (search "Unbalanced" message) "a cut-off prefix is not a delimiter verdict"))))))

(deftest lisp-read-file-raw-mode-still-reads-a-broken-file
  (testing "collapsed=false does not parse, so it shows the text as is"
    (with-temp-lisp-file "tests/tmp/read-file-raw-broken.lisp" *stray-close-source*
      (lambda (path)
        (let* ((result (lisp-read-file path :collapsed nil :offset 1 :limit 1))
               (content (gethash "content" result)))
          (ok (string= (gethash "mode" result) "raw"))
          (ok (search "(list 1)))" content)))))))
```

- [ ] **Step 3: 失敗を確認する**

`run-tests` で `{"system": "cl-mcp/tests/lisp-read-file-test"}`。
Expected: 欠落は `Unexpected end of file` の素の `error` (file-unparseable-error ではない) で `%unparseable-message` が NIL、余分な `)` は `Internal error during` を含む、lenient は `unparseable` が nil、で失敗。

- [ ] **Step 4: defpackage を更新する**

`lisp-edit-form` で `defpackage` (form_name `cl-mcp/src/lisp-read-file`) を `replace`:

```lisp
(defpackage #:cl-mcp/src/lisp-read-file
  (:use #:cl)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:cst-node
                #:cst-node-kind
                #:cst-node-value
                #:cst-node-start
                #:cst-node-end
                #:cst-node-start-line)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-resolve-read-path)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:rpc-error #:text-content
                #:arg-validation-error #:validation-message #:tool-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/state
                #:protocol-version)
  (:import-from #:cl-mcp/src/utils/lenient-read
                #:*homeless-due-to-teardown*)
  (:import-from #:cl-mcp/src/utils/paths
                #:normalize-path-for-display)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json
                #:sanitize-error-message)
  (:import-from #:cl-mcp/src/utils/strings
                #:ensure-trailing-newline)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%parse-readtable-designator
                #:file-unparseable-error
                #:file-unparseable-message
                #:file-unparseable-diagnosis
                #:make-file-unparseable-condition
                #:signal-file-unparseable)
  (:import-from #:cl-ppcre
                #:scan
                #:create-scanner)
  (:import-from #:uiop
                #:ensure-pathname
                #:pathname-type)
  (:export #:lisp-read-file
           #:lisp-source-path-p))
```

- [ ] **Step 5: `%format-lisp-file` でパースを包み、第 2 値を処理する**

`lisp-edit-form` で `%format-lisp-file` (form_type `defun`) を `replace`:

```lisp
(defun %format-lisp-file (text name-scanner content-scanner include-comments
                          comment-context &key readtable source-path)
  "Render TEXT, the Lisp source at SOURCE-PATH, in collapsed form.
Returns two values: the display string and the meta hash.
A file whose parse signals (the Eclector pass yields no forms at all from a
file that does not parse) is reported through SIGNAL-FILE-UNPARSEABLE, so the
caller sees the same delimiter diagnosis and recovery path as the editing
tools. When the lenient CL-reader pass (after an IN-READTABLE switch, or under
READTABLE) stops early instead, PARSE-TOP-LEVEL-FORMS returns the forms it
read plus the error as a second value: those forms are rendered, and the same
guidance is appended below them, because a prefix shown without a word about
where it stops would read as the whole file. Meta then carries \"unparseable\"
and, when the diagnosis names it, \"unparseable_from_line\"."
  (multiple-value-bind (source-lines-count comment-lines blank-lines)
      (%line-stats text)
    (multiple-value-bind (nodes swallowed)
        (handler-case
            (parse-top-level-forms text
                                   :readtable readtable
                                   :source-path source-path)
          (error (e)
            (signal-file-unparseable source-path text e :readtable readtable)))
      (let* ((line-width (%line-number-width source-lines-count))
             (expanded 0)
             (total-forms 0)
             (*package* *package*)
             (display
              (with-output-to-string (out)
                (let ((pending-comments nil))
                  (dolist (node nodes)
                    (cond
                      ((and include-comments (%comment-node-p node))
                       (let ((comment
                              (ensure-trailing-newline
                               (%comment-text node text))))
                         (cond
                           ((string= comment-context "all")
                            (write-string comment out))
                           ((string= comment-context "preceding")
                            (push comment pending-comments)))))
                      ((and (typep node 'cst-node)
                            (eq (cst-node-kind node) :expr))
                       (incf total-forms)
                       (when (and include-comments pending-comments)
                         (dolist (comment (nreverse pending-comments))
                           (write-string comment out))
                         (setf pending-comments nil))
                       (multiple-value-bind (line expanded?)
                           (%format-lisp-form node text name-scanner
                                              content-scanner line-width)
                         (when expanded? (incf expanded))
                         (write-string (ensure-trailing-newline line) out))
                       ;; Track in-package to set *package* for correct
                       ;; symbol printing of subsequent forms.
                       (let* ((form (cst-node-value node))
                              (head (and (consp form) (car form))))
                         (when (and (symbolp head)
                                    (string= (symbol-name head) "IN-PACKAGE")
                                    (consp (cdr form)))
                           (let* ((designator (second form))
                                  (pkg-name
                                   (cond ((stringp designator) designator)
                                         ((symbolp designator)
                                          (symbol-name designator)))))
                             (when pkg-name
                               (let ((pkg (find-package pkg-name)))
                                 (when pkg
                                   (setf *package* pkg))))))))
                      (t (setf pending-comments nil))))
                  (when (and include-comments
                             (string/= comment-context "none")
                             pending-comments)
                    (dolist (comment (nreverse pending-comments))
                      (write-string comment out))))))
             (meta (make-hash-table :test #'equal)))
        (setf (gethash "total_forms" meta) total-forms
              (gethash "expanded_forms" meta) expanded
              (gethash "comment_lines" meta) comment-lines
              (gethash "blank_lines" meta) blank-lines
              (gethash "source_lines" meta) source-lines-count)
        (if swallowed
            ;; The lenient pass stopped early: say so under what it could
            ;; show, with the same guidance the editing tools give.
            (let* ((condition (make-file-unparseable-condition
                               source-path text swallowed
                               :readtable readtable
                               :editable-prefix (and nodes t)))
                   (diagnosis (file-unparseable-diagnosis condition))
                   (from-line (or (getf diagnosis :unclosed-form-line)
                                  (getf diagnosis :line))))
              (setf (gethash "unparseable" meta) t)
              (when from-line
                (setf (gethash "unparseable_from_line" meta) from-line))
              (values (format nil "~A~%~%~A~%"
                              (string-right-trim '(#\Newline) display)
                              (file-unparseable-message condition))
                      meta))
            (values display meta))))))
```

- [ ] **Step 6: `%lisp-read-file-content` の collapsed 分岐を差し替える**

`lisp-edit-form` で `%lisp-read-file-content` (form_type `defun`) を `replace`:

```lisp
(defun %lisp-read-file-content (resolved collapsed name-scanner content-scanner offset line-limit
                                 include-comments comment-context &key readtable)
  "Return three values for RESOLVED: the content string, the meta hash and the
mode name. The collapsed Lisp branch refuses a read cut at the fs cap (a
prefix of a valid file would only yield a misleading delimiter diagnosis) and
otherwise defers to %FORMAT-LISP-FILE, which signals FILE-UNPARSEABLE-ERROR
for a file that does not parse."
  (cond
    ((and collapsed (lisp-source-path-p resolved))
     (multiple-value-bind (text truncated file-length) (fs-read-file resolved)
       (when truncated
         (error "~A exceeds the read limit (~@[~D bytes, ~]only ~D characters read), ~
                 so the collapsed view cannot be built from it. Read a region with ~
                 collapsed=false (offset and limit are lines), or split the file."
                (file-namestring resolved) file-length (length text)))
       (multiple-value-bind (display meta-table)
           (%format-lisp-file text name-scanner content-scanner include-comments
                              comment-context
                              :source-path resolved
                              :readtable readtable)
         (values display meta-table "lisp-collapsed"))))
    ((not collapsed)
     (multiple-value-bind (text total)
         (%read-lines-slice resolved (or offset 0) line-limit)
       (let* ((meta (make-hash-table :test #'equal))
              (start-line (1+ (or offset 0)))
              (end-line (min (+ (or offset 0) line-limit) total))
              (footer (when (< end-line total)
                        (format nil "[Showing lines ~D-~D of ~D. ~
                                     Use offset=~D to read more.]~%"
                                start-line end-line total end-line)))
              (eof-msg (when (and (string= text "") (> (or offset 0) 0))
                         (format nil "[Offset ~D is past end of file (~D total line~:P).]~%"
                                 (or offset 0) total)))
              (content (cond
                         (footer (concatenate 'string text footer))
                         (eof-msg eof-msg)
                         (t text))))
         (setf (gethash "truncated" meta) (if footer t nil)
               (gethash "total_lines" meta) total)
         (values content meta "raw"))))
    ((and content-scanner (not (lisp-source-path-p resolved)))
     (multiple-value-bind (text truncated)
         (%text-filter-with-context resolved content-scanner line-limit)
       (let ((meta (make-hash-table :test #'equal)))
         (setf (gethash "truncated" meta) truncated)
         (values text meta "text-filtered"))))
    (t
     (multiple-value-bind (text total)
         (%read-lines-slice resolved (or offset 0) line-limit)
       (let* ((meta (make-hash-table :test #'equal))
              (start-line (1+ (or offset 0)))
              (end-line (min (+ (or offset 0) line-limit) total))
              (footer (when (< end-line total)
                        (format nil "[Showing lines ~D-~D of ~D. ~
                                     Use offset=~D to read more.]~%"
                                start-line end-line total end-line)))
              (content (if footer
                           (concatenate 'string text footer)
                           text)))
         (setf (gethash "truncated" meta) (if footer t nil)
               (gethash "total_lines" meta) total)
         (values content meta
                 (if (lisp-source-path-p resolved)
                     "lisp-snippet"
                     "text-snippet")))))))
```

- [ ] **Step 7: ツール本体で捕まえる**

`lisp-edit-form` で `define-tool "lisp-read-file"` (form_type `define-tool`, form_name `lisp-read-file`) を `replace`:

```lisp
(define-tool "lisp-read-file"
  :description "Read a file with Lisp-aware collapsed view to save context window tokens.
ALWAYS prefer this tool over 'fs-read-file' when reading .lisp or .asd files,
unless you need exact raw bytes.
Use 'name_pattern' to locate specific definitions (e.g., functions, classes)
without reading the entire file.
Use 'collapsed=true' (default) to see only signatures, or 'collapsed=false'
for full source.
A form expanded by name_pattern or content_pattern is echoed from the file
verbatim, comments inside it included, and each 'NNN:' prefix is that line of
the file -- so text copied out of an expanded form can be used directly as
lisp-patch-form's 'old_text', which matches raw text exactly. Collapsed
signature lines are printed rather than quoted, as is the form
content_pattern matches against.
When reading in raw mode (collapsed=false) and output is truncated, a
'[Showing lines A-B of N. Use offset=B to read more.]' footer is appended
to guide pagination. Use the suggested offset value in a follow-up call.
A file that does not parse cannot be collapsed: the error names the broken
form, the likely fix and the recovery path (the same diagnosis lisp-check-parens
and lisp-edit-form give). Raw mode still works on such a file."
  :args ((path :type :string :required t
               :description "Path to read; absolute inside project or registered ASDF system,
or relative to project root")
         (collapsed :type :boolean :default t
                    :description "When true (default) collapse Lisp definitions to signatures")
         (name_pattern :type :string
                       :description "Regex to match definition names to expand (CL-PPCRE syntax)")
         (content_pattern :type :string
                          :description "Regex to match form bodies or text lines to expand")
         (offset :type :integer
                 :description "0-based line offset when collapsed=false (raw mode only)")
         (limit :type :integer
                :description "Maximum lines to return; defaults to 500")
         (readtable :type :string
                    :description "Named-readtable designator for files using custom reader macros.
Supports both keyword style ('interpol-syntax') and package-qualified style
('pokepay-syntax:pokepay-syntax'). NOTE: When specified, the standard CL reader
is used instead of Eclector, which means comments are NOT preserved."))
  :body
  ;; Mirrors lisp-macroexpand's tool body: a file that does not parse is an
  ;; expected operational failure whose guidance is multi-line and long, so
  ;; it is presented as its own message instead of behind define-tool's
  ;; generic "Internal error during ..." wrapper, which reads as a cl-mcp bug
  ;; and invites a retry.
  ;;
  ;; ARG-VALIDATION-ERROR is a subtype of ERROR and MUST stay listed first:
  ;; HANDLER-CASE takes the first matching clause, so the ERROR clause below
  ;; would otherwise catch validation failures and strip them of the
  ;; protocol-aware treatment define-tool's own handler gives them.
  (handler-case
      (let ((file-result
              (lisp-read-file path
                              :collapsed collapsed
                              :name-pattern name_pattern
                              :content-pattern content_pattern
                              :offset offset
                              :limit limit
                              :readtable (%parse-readtable-designator readtable))))
        (result id
                (make-ht "content" (text-content (gethash "content" file-result))
                         "text" (gethash "content" file-result)
                         "path" (gethash "path" file-result)
                         "mode" (gethash "mode" file-result)
                         "meta" (gethash "meta" file-result))))
    (arg-validation-error (e)
      (tool-error id (validation-message e)
                  :protocol-version (protocol-version state)))
    (file-unparseable-error (e)
      (tool-error id (sanitize-for-json (princ-to-string e))
                  :protocol-version (protocol-version state)))
    (error (e)
      (let ((msg (sanitize-for-json
                  (sanitize-error-message (format nil "~A" e)))))
        (if (and (protocol-version state)
                 (string>= (protocol-version state) "2025-11-25"))
            (result id (make-ht "content" (text-content msg) "isError" t))
            (rpc-error id -32603 msg))))))
```

- [ ] **Step 8: テストが通ることを確認する**

`run-tests` で `{"system": "cl-mcp/tests/lisp-read-file-test"}`。
Expected: 全テスト PASS。`lisp-read-file-lenient-path-...` は named-readtables がなければ skip。

- [ ] **Step 9: リントしてコミット**

```bash
mallet src/lisp-read-file.lisp
git add src/lisp-read-file.lisp tests/lisp-read-file-test.lisp
git commit -m "paren-diagnostics-peripheral: connect lisp-read-file to the shared diagnosis

The collapsed reader caught only end-of-file and answered with its own
one-line hint behind define-tool's Internal error wrapper; a stray )
(stray-right-parenthesis) escaped it entirely, the lenient in-readtable
pass dropped the broken tail without a word, and a read cut at the fs cap
was parsed as if it were the file. Refuse a truncated read, route every
parse failure through signal-file-unparseable, render the lenient pass's
second value under the forms it could show, and catch the condition in the
tool body as lisp-macroexpand does.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01BpidmyXVyFDkT2rYoHQ6y5"
```

---

### Task 6: 復旧手順を 2 段構えにする (`src/paren-diagnostics.lisp` ほか)

**Files:**
- Modify: `src/paren-diagnostics.lisp` (`format-overwrite-recovery`)
- Modify: `src/validate.lisp` (`format-overwrite-recovery` 呼び出しに `:fix-line`、`%maybe-add-lisp-edit-guidance` docstring)
- Modify: `src/lisp-edit-form-core.lisp` (`file-unparseable-message` の呼び出しに `:fix-line`)
- Modify: `prompts/repl-driven-development.md` (Parenthesis Mismatch 節)
- Test: `tests/paren-diagnostics-test.lisp`

**Interfaces:**
- Produces: `(format-overwrite-recovery relative-path &key have-fix (where "below") form-line fix-line)`。`fix-line` (1 起点) があれば `lisp-read-file (collapsed=false, offset=<fix-line - 1>, limit=1 ...)` を文面に埋め込む。全文取得は `fs-read-file` を案内し続ける。
- Consumes: Task 4 の `file-unparseable-message` (変数 `fixes` あり)、Task 3 の `lisp-check-parens` (変数 `likely-fixes` あり)。

- [ ] **Step 1: テストパッケージに import を足す**

`lisp-edit-form` で `tests/paren-diagnostics-test.lisp` の `defpackage` (form_name `cl-mcp/tests/paren-diagnostics-test`) を `replace`:

```lisp
(defpackage #:cl-mcp/tests/paren-diagnostics-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:scan-delimiters
                #:diagnose-delimiters
                #:count-delimiter-depth
                #:lexical-state-at
                #:repair-line-differences
                #:format-repair-lines
                #:format-delimiter-diagnosis
                #:format-overwrite-recovery))
```

- [ ] **Step 2: 失敗するテストを書く**

`lisp-edit-form` で `+stray-bracket+` (form_type `defparameter`) の後に `insert_after`:

```lisp
(deftest format-overwrite-recovery-names-both-read-tools
  (testing "the line is confirmed with lisp-read-file at a precomputed offset"
    (let ((text (format-overwrite-recovery "src/a.lisp" :have-fix t :fix-line 41)))
      (ok (search "lisp-read-file (collapsed=false, offset=40, limit=1" text)
          "offset is 0-based lines, so line 41 is offset 40")
      (ok (search "read the whole file with fs-read-file" text)
          "the full text for writing back still comes from fs-read-file")
      (ok (search "do not copy from lisp-read-file's raw mode" text))
      (ok (search "fix shown under \"Likely fix\"" text))
      (ok (search "fs-write-file (path=\"src/a.lisp\", allow_unparseable_overwrite=true" text))))
  (testing "without a fix line no offset is invented"
    (let ((text (format-overwrite-recovery "src/a.lisp" :where "above" :form-line 3)))
      (ng (search "offset=" text))
      (ok (search "lisp-read-file (collapsed=false;" text)
          "raw mode is still named as the way to look at the file")
      (ok (search "change described above to the form starting at line 3" text)))))
```

- [ ] **Step 3: 失敗を確認する**

`run-tests` で `{"system": "cl-mcp/tests/paren-diagnostics-test"}`。
Expected: `:fix-line` が未知のキーワード引数でエラー。

- [ ] **Step 4: `format-overwrite-recovery` を書き換える**

`lisp-edit-form` で `format-overwrite-recovery` (form_type `defun`) を `replace`:

```lisp
(defun format-overwrite-recovery (relative-path &key have-fix (where "below") form-line fix-line)
  "Return the recovery steps for a file that fails on a delimiter no readtable
can fix, worded once for both lisp-check-parens and file-unparseable-error.
Two read tools, for two jobs: confirm the line with lisp-read-file in raw mode
(collapsed=false works on a file that does not parse, and its offset and limit
are lines, so FIX-LINE -- the 1-based line of the likely fix, when known --
becomes a ready-made offset=FIX-LINE-1), then read the whole file with
fs-read-file, whose text is exact (the raw mode re-joins lines and may append
a footer, so it must not be the source of the write-back), apply the fix
(HAVE-FIX: the one shown under \"Likely fix\"; otherwise the change described
WHERE -- \"below\" or \"above\" -- optionally to the form starting at
FORM-LINE), and write it back with fs-write-file. RELATIVE-PATH is the
project-relative path that fs-write-file requires. Ends with the
custom-reader-syntax caveat."
  (format nil "confirm the line with lisp-read-file (collapsed=false~@[, offset=~D, ~
               limit=1~]; offset and limit are 0-based lines, and raw mode works on a ~
               file that does not parse), read the whole file with fs-read-file (exact ~
               bytes; do not copy from lisp-read-file's raw mode, which re-joins lines ~
               and may append a footer), apply the ~:[change described ~A~;fix shown ~
               under \"Likely fix\"~*~]~@[ to the form starting at line ~D~], and write ~
               the whole file back with fs-write-file (path=~S, ~
               allow_unparseable_overwrite=true; it refuses to overwrite an existing ~
               Lisp file otherwise). If the file uses custom reader syntax that the ~
               default reader cannot parse, pass the readtable parameter to ~
               lisp-edit-form instead of overwriting."
          (and fix-line (1- fix-line)) have-fix where form-line relative-path))
```

- [ ] **Step 5: 呼び出し元に `:fix-line` を渡す**

`lisp-patch-form` で `src/validate.lisp` の `lisp-check-parens` (form_type `defun`):
- old_text:
```
                                  :have-fix (and likely-fixes t)
                                  :where "below"))))
```
- new_text:
```
                                  :have-fix (and likely-fixes t)
                                  :where "below"
                                  :fix-line (and likely-fixes
                                                 (getf (first likely-fixes) :line))))))
```

`lisp-patch-form` で `src/lisp-edit-form-core.lisp` の `file-unparseable-message` (form_type `defun`):
- old_text:
```
                                                :have-fix (not (null fixes))
                                                :where "above"
                                                :form-line line))
```
- new_text:
```
                                                :have-fix (not (null fixes))
                                                :where "above"
                                                :form-line line
                                                :fix-line (and fixes
                                                               (getf (first fixes) :line))))
```

- [ ] **Step 6: docstring の `fs-read-file` 案内を直す**

`lisp-patch-form` で `src/validate.lisp` の `%maybe-add-lisp-edit-guidance` (form_type `defun`)。old_text は docstring 内の 1 行の一部で、末尾に改行を含めない:
- old_text: `fs-read-file, apply the fix, fs-write-file with`
- new_text:
```
lisp-read-file (raw mode) to confirm the line, fs-read-file for the exact
full text, apply the fix, fs-write-file with
```

- [ ] **Step 7: prompt の復旧手順を直す**

Edit ツールで `prompts/repl-driven-development.md` の次の段落を置き換える。

old:
```
If the **file itself** no longer parses, `lisp-edit-form` and `lisp-patch-form` cannot
locate any form in it. Recover with `fs-read-file`, apply the likely fix by hand, and
write the whole file back with `fs-write-file` with `allow_unparseable_overwrite: true`
(its `path` must be relative to the project root; the error message prints that path)
(it refuses to overwrite an existing `.lisp` file otherwise, and the flag never applies
to a file that parses). If the file only looks broken because it uses custom reader
syntax such as `#?"..."`, pass the `readtable` parameter to `lisp-edit-form` instead.
```

new:
```
If the **file itself** no longer parses, `lisp-edit-form` and `lisp-patch-form` cannot
locate any form in it. Recover in two steps. First confirm the reported line with
`lisp-read-file` (`collapsed: false`, `offset: <line - 1>`, `limit: 1`; raw mode works
on a broken file and its offset/limit are lines). Then read the whole file with
`fs-read-file` (exact bytes; do not copy from `lisp-read-file`'s raw mode, which
re-joins lines and may append a `[Showing lines ...]` footer), apply the likely fix by
hand, and write the whole file back with `fs-write-file` with
`allow_unparseable_overwrite: true` (its `path` must be relative to the project root;
the error message prints that path) (it refuses to overwrite an existing `.lisp` file
otherwise, and the flag never applies to a file that parses). If the file only looks
broken because it uses custom reader syntax such as `#?"..."`, pass the `readtable`
parameter to `lisp-edit-form` instead.
```

- [ ] **Step 8: テストが通ることを確認する**

`run-tests` で `{"system": "cl-mcp/tests/paren-diagnostics-test"}`、`{"system": "cl-mcp/tests/validate-test"}`、`{"system": "cl-mcp/tests/lisp-edit-form-test"}`、`{"system": "cl-mcp/tests/lisp-read-file-test"}`。
Expected: すべて PASS (`lisp-edit-form-broken-file-gives-guidance` の `"write the whole file back with fs-write-file"` は新文面にも含まれる)。

- [ ] **Step 9: リントしてコミット**

```bash
mallet src/paren-diagnostics.lisp src/validate.lisp src/lisp-edit-form-core.lisp
git add src/paren-diagnostics.lisp src/validate.lisp src/lisp-edit-form-core.lisp \
        prompts/repl-driven-development.md tests/paren-diagnostics-test.lisp
git commit -m "paren-diagnostics-peripheral: send recovery through both read tools

The recovery text said to read the file with fs-read-file, whose offset
and limit are characters, while the diagnosis names a line. Split the
step: confirm the line with lisp-read-file in raw mode at a precomputed
offset (fix-line - 1; raw mode works on a broken file), then take the
exact full text from fs-read-file, since raw mode re-joins lines and may
append a footer. One function carries the wording for all four tools; the
prompt's Parenthesis Mismatch section says the same.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01BpidmyXVyFDkT2rYoHQ6y5"
```

---

### Task 7: `fs-write-file` が書いた内容を検証する (`src/fs.lisp`)

**Files:**
- Modify: `src/fs.lisp` (defpackage、新関数 `%post-write-parse-warning`、`define-tool "fs-write-file"`)
- Test: `tests/fs-test.lisp`

**Interfaces:**
- Consumes: `*lisp-file-unparseable-hook*` (fs 自身)、`paren-diagnostics` の `diagnose-delimiters` / `format-delimiter-diagnosis`。
- Produces: `.lisp`/`.asd` 書き込み後、hook が `t` なら本文に `WARNING: the file was written but does not parse.` + 診断 + 「次の書き込みには `allow_unparseable_overwrite=true`」。ペイロード `unparseable` = t。書き込みは常に成功。

- [ ] **Step 1: 失敗するテストを書く**

`lisp-edit-form` で `fs-resolve-read-path-trailing-slash-normalization` (form_type `deftest`) の後に `insert_after`:

```lisp
(defun %call-fs-write (path content &key allow)
  "Call the fs-write-file tool handler and return (VALUES text payload error):
the summary text, the result hash, and the JSON-RPC error hash, if any."
  (let ((args (cl-mcp/src/tools/helpers:make-ht "path" path "content" content)))
    (when allow
      (setf (gethash "allow_unparseable_overwrite" args) t))
    (let* ((response (cl-mcp/src/fs::fs-write-file-handler
                      (cl-mcp/src/state:make-state) 1 args))
           (payload (gethash "result" response))
           (content (and payload (gethash "content" payload))))
      (values (and content (plusp (length content)) (gethash "text" (aref content 0)))
              payload
              (gethash "error" response)))))

(defmacro with-scratch-file ((relative) &body body)
  "Run BODY under the test project root, then delete RELATIVE if it exists."
  `(with-test-project-root
     (unwind-protect
          (progn ,@body)
       (ignore-errors
        (delete-file (merge-pathnames ,relative cl-mcp/src/project-root:*project-root*))))))

(deftest fs-write-file-warns-when-the-written-lisp-does-not-parse
  (with-scratch-file ("tests/tmp/write-warn-new.lisp")
    (multiple-value-bind (text payload err)
        (%call-fs-write "tests/tmp/write-warn-new.lisp"
                        (format nil "(defun a (x)~%  (list x)~%~%(defun b (y)~%  (list y))~%"))
      (testing "the write itself succeeds"
        (ok (null err))
        (ok (eq t (gethash "success" payload)))
        (ok (search "Wrote tests/tmp/write-warn-new.lisp" text))
        (ok (probe-file (merge-pathnames "tests/tmp/write-warn-new.lisp"
                                         cl-mcp/src/project-root:*project-root*))))
      (testing "the text says the file does not parse and shows the diagnosis"
        (ok (search "WARNING: the file was written but does not parse." text))
        (ok (search "unclosed (form starting at line 1" text))
        (ok (search "Likely fix" text)))
      (testing "and it says the next write needs the flag"
        (ok (search "allow_unparseable_overwrite=true" text))
        (ok (eq t (gethash "unparseable" payload)))))
    (testing "the second write without the flag is refused, which is why the warning says so"
      (multiple-value-bind (text payload err)
          (%call-fs-write "tests/tmp/write-warn-new.lisp"
                          (format nil "(defun a (x)~%  (list x))~%~%(defun b (y)~%  (list y))~%"))
        (declare (ignore text payload))
        (ok err "an existing unparseable .lisp needs the opt-in")))
    (testing "the write the warning asked for succeeds and warns no more"
      (multiple-value-bind (text payload err)
          (%call-fs-write "tests/tmp/write-warn-new.lisp"
                          (format nil "(defun a (x)~%  (list x))~%~%(defun b (y)~%  (list y))~%")
                          :allow t)
        (ok (null err))
        (ng (search "WARNING" text))
        (ok (null (gethash "unparseable" payload)))))))

(deftest fs-write-file-does-not-warn-for-parseable-or-non-lisp-content
  (testing "a balanced .lisp gets the plain summary"
    (with-scratch-file ("tests/tmp/write-warn-ok.lisp")
      (multiple-value-bind (text payload)
          (%call-fs-write "tests/tmp/write-warn-ok.lisp" (format nil "(defun a () 1)~%"))
        (ng (search "WARNING" text))
        (ok (null (gethash "unparseable" payload))))))
  (testing "a .md file is never parsed"
    (with-scratch-file ("tests/tmp/write-warn-notes.md")
      (multiple-value-bind (text payload)
          (%call-fs-write "tests/tmp/write-warn-notes.md" (format nil "# Notes~%(((~%"))
        (ng (search "WARNING" text))
        (ok (null (gethash "unparseable" payload))))))
  (testing "custom reader syntax that only fails the default reader is not called broken"
    (with-scratch-file ("tests/tmp/write-warn-custom.lisp")
      (multiple-value-bind (text payload)
          (%call-fs-write "tests/tmp/write-warn-custom.lisp"
                          (format nil "(defun f ()~%  #?[(])~%"))
        (ng (search "WARNING" text) "the hook says nil for a reader-level failure")
        (ok (null (gethash "unparseable" payload)))))))

(deftest fs-write-file-warning-follows-the-hook
  (testing "without a hook there is no verdict, so no warning and no error"
    (with-scratch-file ("tests/tmp/write-warn-nohook.lisp")
      (let ((cl-mcp/src/fs:*lisp-file-unparseable-hook* nil))
        (multiple-value-bind (text payload err)
            (%call-fs-write "tests/tmp/write-warn-nohook.lisp" (format nil "(defun a ()~%"))
          (ok (null err))
          (ng (search "WARNING" text))
          (ok (null (gethash "unparseable" payload)))))))
  (testing "a hook verdict on balanced-looking text still warns, with a plain sentence"
    (with-scratch-file ("tests/tmp/write-warn-stub.lisp")
      (let ((cl-mcp/src/fs:*lisp-file-unparseable-hook*
              (lambda (pn text) (declare (ignore pn text)) t)))
        (multiple-value-bind (text payload)
            (%call-fs-write "tests/tmp/write-warn-stub.lisp" (format nil "(defun a () 1)~%"))
          (ok (search "WARNING" text))
          (ok (search "cannot parse the file as written" text))
          (ok (search "allow_unparseable_overwrite=true" text))
          (ok (eq t (gethash "unparseable" payload))))))))
```

`tests/fs-test.lisp` の `defpackage` は `rove` から `#:ng` を import していないので、`lisp-edit-form` で `defpackage` (form_name `cl-mcp/tests/fs-test`) を `replace`:

```lisp
(defpackage #:cl-mcp/tests/fs-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:ng)
  (:import-from #:uiop #:getcwd #:ensure-directory-pathname)
  (:import-from #:asdf #:system-source-directory)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file
                #:fs-write-file
                #:fs-list-directory
                #:fs-resolve-read-path
                #:fs-get-project-info
                #:fs-set-project-root))
```

- [ ] **Step 2: 失敗を確認する**

`run-tests` で `{"system": "cl-mcp/tests/fs-test"}`。
Expected: `WARNING` が本文になく `unparseable` が nil で失敗。「2 回目の書き込みが拒否される」だけは現状でも通る (それが警告の必要な理由)。

- [ ] **Step 3: defpackage に `paren-diagnostics` を import する**

`lisp-patch-form` で `src/fs.lisp` の `defpackage` (form_name `cl-mcp/src/fs`):
- old_text:
```
  (:import-from #:uiop/utility #:string-prefix-p)
  (:import-from #:uiop/filesystem #:ensure-directories-exist)
```
- new_text:
```
  (:import-from #:uiop/utility #:string-prefix-p)
  (:import-from #:uiop/filesystem #:ensure-directories-exist)
  ;; No cycle: paren-diagnostics depends on parinfer and uiop only.
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:diagnose-delimiters
                #:format-delimiter-diagnosis)
```

- [ ] **Step 4: 警告関数を足す**

`lisp-edit-form` で `%existing-lisp-overwrite-error` (form_type `defun`) の後に `insert_after`:

```lisp
(defun %post-write-parse-warning (pn path content)
  "Return a warning for the caller of fs-write-file when CONTENT, just written
to the Lisp source file PN (PATH is its project-relative name), does not
parse; NIL otherwise, and NIL for non-Lisp files. The verdict is the one
*LISP-FILE-UNPARSEABLE-HOOK* gives, i.e. exactly the condition under which the
overwrite guard would let this file be rewritten: a delimiter failure no
readtable can fix. A reader-level failure (an unknown reader macro) gets no
warning, since the hook cannot tell it from custom syntax. Without a hook (a
partial image that loaded fs alone) there is no verdict and no warning. The
text carries the shared delimiter diagnosis -- or, should the reader fail
where the scan sees balance, a plain sentence -- and says that the next write
needs allow_unparseable_overwrite=true, because the file now exists and does
not parse, so the guard would otherwise refuse the very fix it asks for."
  (when (and *lisp-file-unparseable-hook*
             (%lisp-source-pathname-p pn)
             (funcall *lisp-file-unparseable-hook* pn content))
    (format nil "WARNING: the file was written but does not parse.~%~A~%~
                 Fix it and write it again with fs-write-file (path=~S, ~
                 allow_unparseable_overwrite=true; the file now exists and does not ~
                 parse, so the overwrite guard requires the flag)."
            (or (format-delimiter-diagnosis (diagnose-delimiters content) :target path)
                (concatenate 'string
                             "The editing tools' reader cannot parse the file as "
                             "written; run lisp-check-parens for the position."))
            path)))
```

- [ ] **Step 5: ツール本体を差し替える**

`lisp-edit-form` で `define-tool "fs-write-file"` (form_type `define-tool`, form_name `fs-write-file`) を `replace`:

```lisp
(define-tool "fs-write-file"
  :description "Write text content to a file relative to project root.
Parent directories are automatically created if they do not exist.
Use this for creating NEW files or editing non-Lisp files (e.g., markdown, config files).
For editing EXISTING Lisp source code, you MUST use 'lisp-edit-form' instead
to preserve structure and comments. The one exception: when an existing .lisp
file no longer parses (a missing or stray parenthesis), lisp-edit-form cannot
locate any form in it, so overwriting it here is the repair path -- but only
with allow_unparseable_overwrite=true, because a file that only looks broken
to the default reader may be valid under a custom readtable.
After writing a .lisp/.asd file its content is checked with the parser the
overwrite guard uses: the write still succeeds, but if the file does not parse
the response says so, shows the diagnosis, and reminds you that the next write
to it needs allow_unparseable_overwrite=true."
  :args ((path :type :string :required t
               :description "Relative path under the project root; absolute paths are rejected")
         (content :type :string :required t
                  :description "Text content to write")
         (allow-unparseable-overwrite
          :type :boolean :default nil
          :description "Permit overwriting an existing .lisp/.asd file that does not parse
(missing or stray parenthesis). Pass true only when you know the file uses no custom
reader syntax; otherwise use lisp-edit-form with the readtable parameter. Never
overrides the guard for a file that parses."))
  :body
  (or (%existing-lisp-overwrite-error id path allow-unparseable-overwrite)
      (progn
        (fs-write-file path content)
        (let* ((warning (%post-write-parse-warning (ensure-write-path path) path content))
               (payload (make-ht "success" t
                                 "content" (text-content
                                            (format nil "Wrote ~A (~D chars)~@[~%~A~]"
                                                    path (length content) warning))
                                 "path" path
                                 "bytes" (length content))))
          (when warning
            (setf (gethash "unparseable" payload) t))
          (result id payload)))))
```

- [ ] **Step 6: テストが通ることを確認する**

`run-tests` で `{"system": "cl-mcp/tests/fs-test"}` と `{"system": "cl-mcp/tests/tools-test"}` (プロトコル経由の `fs-write-file` ガードテストが同じ挙動を保つこと)。
Expected: すべて PASS。

- [ ] **Step 7: prompt に fs-write-file の警告を一文足す**

Edit ツールで `prompts/repl-driven-development.md` の「New Files workflow」節の 2 行目
```
2. Verify with `lisp-check-parens` on the written file
```
を
```
2. Read `fs-write-file`'s response: if the file does not parse it says `WARNING`, shows the
   diagnosis, and the next write to it needs `allow_unparseable_overwrite: true`
```
に置き換える。

- [ ] **Step 8: リントしてコミット**

```bash
mallet src/fs.lisp
git add src/fs.lisp tests/fs-test.lisp prompts/repl-driven-development.md
git commit -m "paren-diagnostics-peripheral: warn when fs-write-file writes a broken Lisp file

fs-write-file wrote whatever it was given and said Wrote N chars, so a
new file with a missing ) went unnoticed until some later tool tripped on
it, and the next write to that file was refused by the overwrite guard
without the caller knowing why. Check the written content with
*lisp-file-unparseable-hook* -- the guard's own verdict, so the warning
fires exactly when overwriting would be permitted -- append the shared
diagnosis, and say that the fix must be written with
allow_unparseable_overwrite=true. The write itself always succeeds.

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01BpidmyXVyFDkT2rYoHQ6y5"
```

---

### Task 8: 全体検証と統合確認

**Files:**
- なし (検証のみ)。必要なら前タスクのファイルを修正して追加コミット。

- [ ] **Step 1: 全体を強制コンパイルして警告を洗う**

```bash
ros -e '(ql:quickload :cl-mcp :silent t)' \
    -e '(asdf:compile-system :cl-mcp :force t)' 2>&1 | grep -iE "warning|error" | grep -v "UIOP" | head -40
```
Expected: 本 PR で触ったファイル (`clgrep`, `validate`, `lisp-edit-form-core`, `lisp-read-file`, `paren-diagnostics`, `fs`) に新しい WARNING が出ない。UIOP 由来の約 427 件は既知のノイズ。

- [ ] **Step 2: 全スイートを新規プロセスで回す**

```bash
rove cl-mcp.asd
```
Expected: 全テスト PASS。単一システムの緑が全スイートで赤になった経験があるので、ここで初めて緑を主張する。失敗があれば該当タスクに戻って直し、`paren-diagnostics-peripheral: fix <what>` でコミットする。

- [ ] **Step 3: リント**

```bash
mallet src/*.lisp
```
Expected: 指摘なし。

- [ ] **Step 4: 手動の統合確認 (仕様 §5 統合)**

`experiments/paren-lab/src/tokens.lisp` の `matching-closer` から `)` を 1 個削り (gitignore 下のスクラッチなので直接編集してよい)、MCP ツールで次を確認する。MCP サーバの親プロセスは古いイメージなので、**この確認は新しいコードで起動した cl-mcp サーバで行う** (ユーザーに再起動を依頼するか、この項目を再起動後に持ち越す)。

1. `clgrep-search` `pattern="defun (matching-closer|tokenize)"` `path=experiments/paren-lab/src/tokens.lisp` → 本文に 2 行と `NOTE:`。
2. `lisp-read-file` `path=experiments/paren-lab/src/tokens.lisp` → `Internal error` なし、`unclosed (form starting at line 39`、`Likely fix`、`confirm the line with lisp-read-file (collapsed=false, offset=40, limit=1`。
3. 案内通り `lisp-read-file collapsed=false offset=40 limit=1` で行を確認し、`fs-read-file` で全文を取り、`)` を足して `fs-write-file` `allow_unparseable_overwrite=true` で書き戻す → `WARNING` なし。
4. わざと直し損ねた内容を `fs-write-file` で書く → `WARNING` と `allow_unparseable_overwrite=true` の案内。もう一度正しく書く → 成功。
5. `lisp-check-parens` `path=...matcher.lisp offset=700 limit=300` → `Reader error at line <ファイル絶対行>` と `Only a window`。

最後に `tokens.lisp` を元に戻す (`git -C experiments/paren-lab status` は無関係; ファイルは gitignore 下なので手で復元するかバックアップから戻す)。

- [ ] **Step 5: 仕様書のステータスを更新してコミット**

Edit ツールで `docs/superpowers/specs/2026-09-06-paren-diagnostics-peripheral-design.md` の
```
- ステータス: 設計レビュー反映済み(実装未着手、再承認待ち)
```
を
```
- ステータス: 実装済み(ブランチ feat/paren-diagnostics-peripheral)
```
に変え、コミットする:

```bash
git add docs/superpowers/specs/2026-09-06-paren-diagnostics-peripheral-design.md
git commit -m "paren-diagnostics-peripheral: mark the design as implemented

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01BpidmyXVyFDkT2rYoHQ6y5"
```

- [ ] **Step 6: ブランチの仕上げ**

`superpowers:finishing-a-development-branch` に従い、PR 作成の可否をユーザーに確認する。PR 本文には仕様書 §8 の改訂履歴を要約し、末尾に
```
🤖 Generated with [Claude Code](https://claude.com/claude-code)

https://claude.ai/code/session_01BpidmyXVyFDkT2rYoHQ6y5
```
を付ける。
