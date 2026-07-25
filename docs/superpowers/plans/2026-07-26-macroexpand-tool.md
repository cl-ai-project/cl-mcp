# マクロ展開ツール 実装プラン

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `repl-eval` の結果印字を修正して全ツールの出力可読性を上げ、続いてファイル中のフォームを直接展開できる `lisp-macroexpand` ツールを追加する。

**Architecture:** Phase A は `src/repl-core.lisp` の `%do-repl-eval` 末尾ブロック1箇所の修正。Phase C は3層構成 — parent 側の `src/lisp-macroexpand.lisp` が CST でフォームを特定してソーステキストを切り出し、worker 側の `%handle-macroexpand` がそれを実パッケージで再読込して展開する。展開と整形の実ロジックは `src/macroexpand-core.lisp` に純粋関数として置き、worker 経路とインラインフォールバック経路が同じ関数を共有する（既存の `repl-core` / `code-core` と同じ慣習）。

**Tech Stack:** SBCL, ASDF `package-inferred-system`, Eclector (CST), Rove, `sb-cltl2:macroexpand-all`

**元設計文書:** `docs/superpowers/specs/2026-07-26-macroexpand-tool-design.md`

---

## 実装者への必読事項

以下は調査で確認済みの事実。思い込みで動くと確実に嵌まる。

1. **`.lisp` ファイルの編集にテキストベースの Edit ツールを使ってはいけない。**
   必ず `lisp-patch-form`（部分置換）または `lisp-edit-form`（フォーム単位）を使う。
   本プランのパッチ手順は `lisp-patch-form` の `old_text` / `new_text` として
   そのまま渡せる形で書いてある。`old_text` は空白まで含めて検証済みのバイト列。

2. **`cl-mcp.asd` は新規ファイル追加では編集不要。** `:class :package-inferred-system` で
   `:components` を持たない。`src/<n>.lisp` が定義するパッケージ名 `cl-mcp/src/<n>` が
   パスと一致し、他ファイルの `:import-from` から辿れれば自動的にロードされる。
   逆に **どこからも `:import-from` されない新規ファイルは、エラーも警告もなく
   永久にロードされない**。（`CLAUDE.md` と `AGENTS.md` の .asd に関する記述は現状と不一致。）

3. **新規テストファイルはリポジトリルートの `tests.lisp`** に
   `(:import-from #:cl-mcp/tests/<name>-test)` を追加しないと
   `rove cl-mcp.asd` と CI から不可視になる。`tests/` ディレクトリではなくルート。

4. **`(asdf:compile-system :cl-mcp :force t)` は `src/worker/*.lisp` をコンパイルしない。**
   parent の依存グラフから worker は辿れない。`handlers.lisp` を編集したら
   必ず worker テストを走らせること。

5. **`tests/fixtures/` は使わない。** git 未追跡かつ参照ゼロの死んだディレクトリ。
   テスト用ファイルは `tests/tmp/`（gitignore 済み）に一時生成して `unwind-protect` で消す。

6. **rove の `signals` は使わない。** `restart-case` 内の条件を捕捉できない。
   `handler-case` + `ok` を使う（`prompts/repl-driven-development.md` の既知の落とし穴）。

7. 単一テストファイルの実行は `rove tests/<name>-test.lisp`（リポジトリルートから）。
   全体は `rove cl-mcp.asd`。rove のサマリ行の件数は過少表示されるので、
   個別の ✓/✗ 行と終了コードで判断すること。

---

# PR 1 — Phase A: `repl-eval` の結果印字修正

## Task 1: 結果印字を eval パッケージ基準・小文字・整形付きにする

**Files:**
- Modify: `src/repl-core.lisp:145-231`（`%do-repl-eval`）
- Test: `tests/repl-test.lisp`

**背景（なぜこの箇所か）:** `%do-repl-eval` の最終印字ブロック（223-231行）は
`handler-bind` の**兄弟**であって内側ではない。`*package*` を束縛している箇所は
207行の読み取り時と `%eval-forms` 内の119行だけで、どちらも223行に到達する前に
動的エクステントを抜けている。結果として `prin1-to-string` は呼び出し側の
`*package*` で印字され、全シンボルが完全修飾される。

**この束縛を `handler-bind` の外側に広げてはいけない。** ハンドラ内の
`(princ-to-string (type-of e))` と `capture-error-context` の出力が変わり、
`tests/repl-test.lisp:140` と、454/473/499/523/550/554 行の6つの
大文字前提のフレーム名アサーションが壊れる。修正は223-231行に限定すること。

- [ ] **Step 1: 失敗するテストを4件書く**

`tests/repl-test.lisp` の末尾に `lisp-edit-form` の `insert_after` で追加する
（`form_type: "deftest"`, `form_name: "repl-eval-truncate-sanitizes-correctly"` の後ろ）。

```lisp
(deftest repl-eval-prints-relative-to-eval-package
  (testing "result symbols print relative to the eval package, not the caller's"
    (multiple-value-bind (printed value)
        (repl-eval "'cl-mcp-print-package-probe" :package "CL-USER")
      (declare (ignore value))
      (ok (string= printed "cl-mcp-print-package-probe")
          "a symbol interned in the eval package prints unqualified and downcased"))))

(deftest repl-eval-prints-downcased
  (testing "symbols print in lower case so results can be pasted back into source"
    (multiple-value-bind (printed value)
        (repl-eval "(list :alpha :beta)")
      (declare (ignore value))
      (ok (string= printed "(:alpha :beta)")))))

(deftest repl-eval-pretty-prints-wide-results
  (testing "results wider than the right margin are broken across lines"
    (multiple-value-bind (printed value)
        (repl-eval "(list (make-list 12 :initial-element :aaaaaaaaaa)
                          (make-list 12 :initial-element :bbbbbbbbbb))")
      (declare (ignore value))
      (ok (find #\Newline printed)
          "output wider than *print-right-margin* should contain a newline"))))

(deftest repl-eval-short-results-stay-on-one-line
  (testing "short results are not broken across lines by the pretty printer"
    (multiple-value-bind (printed value)
        (repl-eval "(list 1 2 3)")
      (declare (ignore value))
      (ok (string= printed "(1 2 3)")))))
```

- [ ] **Step 2: 失敗を確認する**

Run: `rove tests/repl-test.lisp`
Expected: `repl-eval-prints-relative-to-eval-package` が
`"COMMON-LISP-USER::CL-MCP-PRINT-PACKAGE-PROBE"` を返して FAIL。
`repl-eval-prints-downcased` が `"(:ALPHA :BETA)"` で FAIL。
`repl-eval-pretty-prints-wide-results` が改行なしで FAIL。
`repl-eval-short-results-stay-on-one-line` は既に PASS（回帰ガード）。

- [ ] **Step 3: `eval-package` を外側の `let` に追加する**

`lisp-patch-form` で `file_path: "src/repl-core.lisp"`,
`form_type: "defun"`, `form_name: "%do-repl-eval"`。

old_text:
```
  (let ((last-value nil)
        (error-context nil)
        (stdout (make-string-output-stream))
```

new_text:
```
  (let ((last-value nil)
        (error-context nil)
        (eval-package nil)
        (stdout (make-string-output-stream))
```

- [ ] **Step 4: 解決済みパッケージを `eval-package` に控える**

同じフォームに対して `lisp-patch-form`。

old_text:
```
      (let ((pkg (%resolve-eval-package package)))
        (let ((forms (handler-case
```

new_text:
```
      (let ((pkg (%resolve-eval-package package)))
        (setf eval-package pkg)
        (let ((forms (handler-case
```

- [ ] **Step 5: 最終印字ブロックの束縛を差し替える**

同じフォームに対して `lisp-patch-form`。
`*print-circle* t` は**必ず残す**こと（循環構造でのハング防止、
`tests/repl-test.lisp:595` の `repl-eval-print-circle-prevents-hang` が検証している）。

old_text:
```
    (let ((*print-level* print-level)
          (*print-length* print-length)
          (*print-readably* nil)
          (*print-circle* t))
```

new_text:
```
    (let ((*package* (or eval-package *package*))
          (*print-level* print-level)
          (*print-length* print-length)
          (*print-readably* nil)
          (*print-case* :downcase)
          (*print-pretty* t)
          (*print-right-margin* 100)
          (*print-circle* t))
```

- [ ] **Step 6: 小文字化で壊れる既存アサーション2件を更新する**

`tests/repl-test.lisp` の2つの `deftest` にそれぞれ `lisp-patch-form` を適用する。
これが**全リポジトリで唯一壊れる箇所**（`tests/` 全ファイルの走査で確認済み）。

1つ目: `form_type: "deftest"`, `form_name: "repl-eval-sanitizes-ansi-escape-codes"`

old_text:
```
      (ok (string= printed ":OK"))
```
new_text:
```
      (ok (string= printed ":ok"))
```

2つ目: `form_type: "deftest"`, `form_name: "repl-eval-sanitizes-control-chars"`

old_text:
```
      (ok (string= printed ":OK"))
```
new_text:
```
      (ok (string= printed ":ok"))
```

- [ ] **Step 7: テストを通す**

Run: `rove tests/repl-test.lisp`
Expected: 全 deftest が PASS、終了コード 0。
特に `repl-eval-print-circle-prevents-hang` が PASS していること
（FAIL する場合は5秒タイムアウト経由になるので、実行時間が急に伸びたら疑う）。

- [ ] **Step 8: コミット**

```bash
mallet src/repl-core.lisp
git add src/repl-core.lisp tests/repl-test.lisp
git commit -m "$(cat <<'EOF'
fix(repl-eval): print results in the eval package, downcased and pretty

%do-repl-eval の最終印字ブロックは handler-bind の兄弟にあり、*package* を
束縛する動的エクステントを既に抜けていた。そのため全シンボルが呼び出し側基準で
完全修飾され、トークンを浪費していた。eval パッケージを控えて印字時に束縛する。

あわせて *print-case* :downcase と *print-pretty* を有効化。*print-circle* t は
循環構造でのハング防止として維持する。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 2: `*print-pretty*` をユーザーコードから隔離する

**Files:**
- Modify: `src/repl-core.lisp:117-132`（`%eval-forms`）
- Test: `tests/repl-test.lisp`

**背景:** `%eval-forms` は「ユーザーコードの `(setf *print-base* 16)` が
JSON シリアライズを壊さないように」9個の印字変数を再束縛しているが、
`*print-pretty*` が漏れている。Task 1 で `*print-pretty*` を意図的に使い始める以上、
隔離リストも揃える。

- [ ] **Step 1: 失敗するテストを書く**

`tests/repl-test.lisp` に `lisp-edit-form` の `insert_after` で追加
（`form_type: "deftest"`, `form_name: "repl-eval-short-results-stay-on-one-line"` の後ろ）。

```lisp
(deftest repl-eval-print-pretty-isolated-from-user-code
  (testing "user (setf *print-pretty*) inside repl-eval does not change the global value"
    (let ((before *print-pretty*))
      (repl-eval "(setf *print-pretty* nil)")
      (ok (eq *print-pretty* before)
          "global *print-pretty* must be unchanged after evaluation"))))
```

- [ ] **Step 2: 失敗を確認する**

Run: `rove tests/repl-test.lisp`
Expected: `repl-eval-print-pretty-isolated-from-user-code` が FAIL
（グローバルの `*print-pretty*` が `NIL` に書き換わっている）。

- [ ] **Step 3: 隔離リストに `*print-pretty*` を足す**

`lisp-patch-form` で `file_path: "src/repl-core.lisp"`,
`form_type: "defun"`, `form_name: "%eval-forms"`。

old_text:
```
          (*print-array* t)
          (*read-default-float-format* 'single-float))
```

new_text:
```
          (*print-array* t)
          (*print-pretty* t)
          (*read-default-float-format* 'single-float))
```

- [ ] **Step 4: テストを通す**

Run: `rove tests/repl-test.lisp`
Expected: 全件 PASS、終了コード 0。

- [ ] **Step 5: 全体をコンパイルして警告ゼロを確認する**

Run: `ros -e '(asdf:load-system :cl-mcp)' -e '(asdf:compile-system :cl-mcp :force t)' -q`
Expected: `cl-mcp` 由来の warning がゼロ。
UIOP の `redefining ...` 警告が約427件出るが、これは既知のノイズで無視してよい。

- [ ] **Step 6: 全テストを通す**

Run: `rove cl-mcp.asd`
Expected: 終了コード 0。

- [ ] **Step 7: コミット**

```bash
mallet src/repl-core.lisp
git add src/repl-core.lisp tests/repl-test.lisp
git commit -m "$(cat <<'EOF'
fix(repl-eval): isolate *print-pretty* from user code

%eval-forms は印字変数9個を再束縛してユーザーコードの副作用を封じ込めているが
*print-pretty* だけが漏れており、(setf *print-pretty* nil) がイメージ全体に
残っていた。隔離リストに追加する。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

# PR 2 — Phase C: `lisp-macroexpand` ツール

## Task 3: SBCL contrib `sb-cltl2` を事前ロードする

**Files:**
- Modify: `cl-mcp.asd:1-11`

**背景:** `level: "all"` は `sb-cltl2:macroexpand-all` を使う。SBCL は
FASL に埋め込まれたパッケージ名をロード時に解決するため、ソースファイル内で
`(require :sb-cltl2)` を書いても遅い。既存の `(require :sb-posix)` と同じ理由で
`.asd` の先頭に置く必要がある。この行は worker（bare SBCL）にも効く。

- [ ] **Step 1: `.asd` に require を追加する**

`cl-mcp.asd` は `defsystem` フォームの外側の行なので、この1箇所だけは
テキスト編集で構わない（`lisp-edit-form` はトップレベルフォームを対象とする）。

old_text:
```
#+sbcl (require :sb-posix)
```
new_text:
```
#+sbcl (require :sb-posix)
#+sbcl (require :sb-cltl2)
```

- [ ] **Step 2: contrib が読めることを確認する**

Run: `ros -e '(asdf:load-asd "cl-mcp.asd")' -e '(princ (and (find-symbol "MACROEXPAND-ALL" "SB-CLTL2") t))' -q`
Expected: `T` が出力される。

- [ ] **Step 3: コミット**

```bash
git add cl-mcp.asd
git commit -m "$(cat <<'EOF'
build: preload sb-cltl2 contrib for macroexpand-all

sb-posix と同じ理由 — SBCL は FASL 内のパッケージ名をロード時に解決するため、
contrib の require はソースファイルではなく .asd に置く必要がある。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 4: `src/macroexpand-core.lisp` — 展開と整形の純粋ロジック

**Files:**
- Create: `src/macroexpand-core.lisp`
- Create: `tests/lisp-macroexpand-test.lisp`
- Modify: `tests.lisp`（リポジトリルート）

**設計上の要点:**
- worker とインラインフォールバックの両方がこの1つの関数を呼ぶ。JSON も MCP も知らない。
- `*print-circle*` は**通常 NIL** にして `#1=` 共有マーカーを消す。ただし
  **循環している場合に限り T にする**。SBCL のプリティプリンタは `quote` 略記で
  `*print-level*` を尊重せず、自己参照構造を渡すと制御スタックを食い尽くして
  プロセスごと落ちるため、`*print-level*` の有限束縛だけでは守れない（実測確認済み）。
- パッケージが存在しなければ**スタブを合成せず**、行動可能なエラーを出す。
  スタブ上で展開すると「何も起きなかった」結果が静かに返り、呼び出し側を誤らせる。

- [ ] **Step 1: テストファイルを先に作る（失敗させる）**

`fs-write-file` で `tests/lisp-macroexpand-test.lisp` を新規作成する。

```lisp
;;;; tests/lisp-macroexpand-test.lisp
;;;;
;;;; Tests for the macro-expansion tool: the pure expansion core
;;;; (src/macroexpand-core.lisp) and the parent-side form addressing
;;;; (src/lisp-macroexpand.lisp).  Expansion runs in this image, which is
;;;; the inline (no worker pool) path; the worker path is covered by
;;;; tests/worker-test.lisp.

(defpackage #:cl-mcp/tests/lisp-macroexpand-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/macroexpand-core
                #:macroexpand-source
                #:macroexpand-forms
                #:macroexpand-package-error))

(in-package #:cl-mcp/tests/lisp-macroexpand-test)

;;; ---------------------------------------------------------------------------
;;; Fixture macros.  They live in this package so the tests can name it as the
;;; expansion package without touching any production source.
;;; ---------------------------------------------------------------------------

(defmacro double-it (x)
  "Test macro: expands to (* 2 X)."
  `(* 2 ,x))

(defmacro double-it-twice (x)
  "Test macro: expands into DOUBLE-IT, which expands again."
  `(double-it (double-it ,x)))

(defmacro shared-literal-macro ()
  "Test macro whose expansion shares one literal in two places.
Under *PRINT-CIRCLE* the sharing would surface as #1= / #1# markers."
  (let ((shared "shared"))
    `(list ,shared ,shared)))

(defmacro big-list-macro ()
  "Test macro whose expansion is deliberately long, for truncation tests."
  `(list ,@(loop for i from 0 below 200 collect i)))

(defmacro cyclic-expansion-macro ()
  "Test macro whose expansion is a self-referential list.
Exercises the circularity check that decides whether to switch
*PRINT-CIRCLE* on.  Without it SBCL's pretty printer exhausts the control
stack on this input and takes the whole process down."
  (let ((cell (list 'quote nil)))
    (setf (second cell) cell)
    cell))

(defmacro exploding-macro ()
  "Test macro whose expander signals, to check per-entry error reporting."
  (error "expander failed on purpose"))

(defparameter *fixture-package* "CL-MCP/TESTS/LISP-MACROEXPAND-TEST"
  "Name of this test package, used as the expansion package.")

;;; ---------------------------------------------------------------------------
;;; macroexpand-core
;;; ---------------------------------------------------------------------------

(deftest macroexpand-source-expands-one-step
  (testing "level once expands the head macro exactly one step"
    (multiple-value-bind (printed expanded-p steps truncated-p)
        (macroexpand-source "(double-it 21)" :package *fixture-package*)
      (ok (string= printed "(* 2 21)"))
      (ok expanded-p)
      (ok (= steps 1))
      (ok (null truncated-p)))))

(deftest macroexpand-source-reports-non-macro
  (testing "a form whose head is not a macro reports expanded-p NIL"
    (multiple-value-bind (printed expanded-p steps)
        (macroexpand-source "(+ 1 2)" :package "CL-USER")
      (ok (string= printed "(+ 1 2)"))
      (ok (null expanded-p) "must not silently claim an expansion happened")
      (ok (= steps 0)))))

(deftest macroexpand-source-signals-on-missing-package
  (testing "an absent package produces an actionable error, not a silent no-op"
    (ok (handler-case
            (progn (macroexpand-source "(double-it 1)"
                                       :package "NO-SUCH-PACKAGE-XYZZY")
                   nil)
          (macroexpand-package-error (e)
            (and (search "load-system" (princ-to-string e)) t)))
        "the error message should tell the caller to load the system")))

(deftest macroexpand-source-full-repeats-until-fixpoint
  (testing "level full keeps expanding while the head is a macro"
    (multiple-value-bind (printed expanded-p steps)
        (macroexpand-source "(double-it-twice 3)"
                            :package *fixture-package* :level "full")
      (ok expanded-p)
      (ok (= steps 2) "double-it-twice -> double-it -> (* 2 ...)")
      (ok (string= printed "(* 2 (double-it 3))")))))

(deftest macroexpand-source-all-walks-nested-forms
  (testing "level all expands nested macro calls too"
    (multiple-value-bind (printed expanded-p)
        (macroexpand-source "(double-it-twice 3)"
                            :package *fixture-package* :level "all")
      (ok expanded-p)
      (ok (null (search "double-it" printed))
          "no macro call should remain after a full code walk"))))

(deftest macroexpand-source-prints-downcased-without-circle-markers
  (testing "output is lower case and free of #N= sharing markers"
    (let ((printed (macroexpand-source "(shared-literal-macro)"
                                       :package *fixture-package*)))
      (ok (null (search "#1=" printed)) "no *print-circle* sharing markers")
      (ok (null (find-if #'upper-case-p printed))
          "symbols print in lower case"))))

(deftest macroexpand-source-does-not-crash-on-cyclic-expansion
  (testing "a self-referential expansion prints with circle markers instead of crashing"
    (multiple-value-bind (printed expanded-p)
        (macroexpand-source "(cyclic-expansion-macro)"
                            :package *fixture-package* :print-level 5)
      (ok expanded-p)
      (ok (search "#1=" printed)
          "circle notation is switched on for a genuinely circular form")
      (ok (< (length printed) 200) "output must be bounded"))))

(deftest macroexpand-source-truncates-long-output
  (testing "output longer than max-output-length is cut and flagged"
    (multiple-value-bind (printed expanded-p steps truncated-p)
        (macroexpand-source "(big-list-macro)"
                            :package *fixture-package* :max-output-length 50)
      (declare (ignore expanded-p steps))
      (ok truncated-p)
      (ok (search "...(truncated)" printed)))))

(deftest macroexpand-source-rejects-unknown-level
  (testing "an unknown level is rejected instead of silently defaulting"
    (ok (handler-case
            (progn (macroexpand-source "(double-it 1)"
                                       :package *fixture-package*
                                       :level "everything")
                   nil)
          (error (e) (and (search "once" (princ-to-string e)) t)))
        "the error should list the accepted level values")))

(deftest macroexpand-forms-keeps-going-after-one-entry-fails
  (testing "a bad entry is reported in place without aborting the batch"
    (let ((results (macroexpand-forms (list (cons "good" "(double-it 1)")
                                            (cons "bad" "(unclosed")
                                            (cons "also-good" "(double-it 2)"))
                                      :package *fixture-package*)))
      (ok (= 3 (length results)) "every entry yields a result")
      (ok (null (getf (first results) :error)))
      (ok (getf (second results) :error) "the unreadable entry carries an error")
      (ok (string= "(* 2 2)" (getf (third results) :printed))
          "the entry after the failure is still expanded"))))

(deftest macroexpand-source-reports-expander-errors
  (testing "an error signaled by the macro expander is reported, not swallowed"
    (let ((results (macroexpand-forms (list (cons "boom" "(exploding-macro)"))
                                      :package *fixture-package*)))
      (ok (getf (first results) :error)
          "the failure is recorded on the entry")
      (ok (search "on purpose" (getf (first results) :error))
          "the expander's own message reaches the caller"))))
```

- [ ] **Step 2: `tests.lisp` に登録する**

`lisp-patch-form` で `file_path: "tests.lisp"`, `form_type: "defpackage"`,
`form_name: "cl-mcp/tests"`。これを忘れると CI から不可視になる。

old_text:
```
  (:import-from #:cl-mcp/tests/project-scaffold-test))
```
new_text:
```
  (:import-from #:cl-mcp/tests/project-scaffold-test)
  (:import-from #:cl-mcp/tests/lisp-macroexpand-test))
```

- [ ] **Step 3: 失敗を確認する**

Run: `rove tests/lisp-macroexpand-test.lisp`
Expected: `Component "cl-mcp/src/macroexpand-core" not found` 系のエラーで失敗する
（`src/macroexpand-core.lisp` がまだ存在しないため）。

- [ ] **Step 4: `src/macroexpand-core.lisp` の骨格を作る**

`fs-write-file` で新規作成する。`defpackage` と `in-package` と1つのスタブだけ。
新規ファイルはまず最小で作り、`lisp-edit-form` で育てるのが本リポジトリの手順。

```lisp
;;;; src/macroexpand-core.lisp
;;;;
;;;; Pure macro-expansion logic shared by the worker handler and the
;;;; inline (no worker pool) fallback.  Knows nothing about JSON-RPC or
;;;; MCP: callers pass (LABEL . SOURCE) conses and receive plists.

(defpackage #:cl-mcp/src/macroexpand-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:export #:macroexpand-forms
           #:macroexpand-source
           #:macroexpand-package-error
           #:macroexpand-package-error-name
           #:*expansion-print-level*
           #:*expansion-print-length*
           #:*expansion-max-output-length*))

(in-package #:cl-mcp/src/macroexpand-core)

(defparameter *expansion-print-level* 50
  "Default `*print-level*` for printed macro expansions.
Must stay finite so a pathologically deep expansion cannot produce
unbounded output.  Note that it does NOT protect against a circular
expansion — SBCL's pretty printer ignores it for the QUOTE abbreviation.
That case is handled by `%circular-p` in `%print-expansion`.")
```

- [ ] **Step 5: 括弧を検証する**

Run: `lisp-check-parens` on `src/macroexpand-core.lisp`
Expected: balanced。

- [ ] **Step 6: 残りの定義を追加する**

`lisp-edit-form` の `insert_after` を使い、`form_type: "defparameter"`,
`form_name: "*expansion-print-level*"` の後ろに順に追加していく。
以下を上から順に1フォームずつ入れる。

```lisp
(defparameter *expansion-print-length* 1000
  "Default `*print-length*` for printed macro expansions.")
```

```lisp
(defparameter *expansion-max-output-length* 50000
  "Default maximum characters for one printed expansion.")
```

```lisp
(defparameter *max-expansion-steps* 100
  "Upper bound on repeated MACROEXPAND-1 steps for level \"full\".
Guards against a macro that expands into itself forever.")
```

```lisp
(define-condition macroexpand-package-error (error)
  ((name :initarg :name :reader macroexpand-package-error-name))
  (:report
   (lambda (condition stream)
     (format stream
             "Package ~A does not exist in this image, so its macros cannot be ~
expanded. Load the system that defines it with the 'load-system' tool, then retry."
             (macroexpand-package-error-name condition))))
  (:documentation "Signaled when the requested expansion package is absent.
Never synthesize a stub package instead: expanding in a stub silently
produces an unexpanded form and misleads the caller."))
```

```lisp
(defun %resolve-package (name)
  "Return the package named NAME, or CL-USER when NAME is NIL.
Signals MACROEXPAND-PACKAGE-ERROR when NAME names no existing package."
  (if (null name)
      (find-package :cl-user)
      (or (find-package name)
          (find-package (string-upcase name))
          (error 'macroexpand-package-error :name name))))
```

```lisp
(defun %parse-readtable-name (designator)
  "Convert the DESIGNATOR string into a symbol for FIND-READTABLE.
Accepts \"pkg:sym\", \"pkg::sym\", \":kw\" and bare \"kw\"."
  (let ((colon (position #\: designator)))
    (cond
      ((null colon)
       (intern (string-upcase designator) :keyword))
      ((zerop colon)
       (intern (string-upcase (string-left-trim ":" designator)) :keyword))
      (t
       (let* ((package-name (subseq designator 0 colon))
              (symbol-name (string-left-trim ":" (subseq designator colon)))
              (package (or (find-package (string-upcase package-name))
                           (error "Package ~A not found for readtable ~A"
                                  package-name designator))))
         (intern (string-upcase symbol-name) package))))))
```

```lisp
(defun %resolve-readtable (designator)
  "Return the named readtable for DESIGNATOR, or NIL when it is absent.
Looks the name up in whichever named-readtables package is loaded, so
this file does not have to depend on the parent-only tool layer."
  (when (and designator (stringp designator) (string/= designator ""))
    (let* ((package (or (find-package "NAMED-READTABLES")
                        (find-package "EDITOR-HINTS.NAMED-READTABLES")))
           (finder (and package (find-symbol "FIND-READTABLE" package)))
           (table (and finder
                       (ignore-errors
                        (funcall finder (%parse-readtable-name designator))))))
      (unless table
        (error "Readtable ~A not found. Load the system that defines it first."
               designator))
      table)))
```

```lisp
(defun %read-source (source package readtable)
  "Read the first form in SOURCE with *PACKAGE* bound to PACKAGE."
  (let ((*package* package)
        (*readtable* (or readtable *readtable*)))
    (with-input-from-string (stream source)
      (read stream))))
```

```lisp
(defun %expand-once (form)
  "Expand FORM one step.  Returns (values expansion steps)."
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1 form)
    (values expansion (if expanded-p 1 0))))
```

```lisp
(defun %expand-full (form)
  "Repeatedly expand FORM while its head is a macro.
Returns (values expansion steps).  Stops at *MAX-EXPANSION-STEPS* so a
self-reproducing macro cannot loop forever."
  (let ((current form)
        (steps 0))
    (loop
      (when (>= steps *max-expansion-steps*)
        (return))
      (multiple-value-bind (expansion expanded-p)
          (macroexpand-1 current)
        (unless expanded-p
          (return))
        (setf current expansion)
        (incf steps)))
    (values current steps)))
```

```lisp
(defun %expand-all (form)
  "Walk FORM with SB-CLTL2:MACROEXPAND-ALL, expanding nested macros.
Returns (values expansion steps), where STEPS is 1 when the walk changed
FORM and 0 otherwise."
  (let ((expansion (sb-cltl2:macroexpand-all form)))
    (values expansion (if (equal expansion form) 0 1))))
```

```lisp
(defun %expand (form level)
  "Expand FORM according to LEVEL.  Returns (values expansion steps)."
  (cond
    ((string-equal level "once") (%expand-once form))
    ((string-equal level "full") (%expand-full form))
    ((string-equal level "all") (%expand-all form))
    (t (error "Unknown level ~S: expected \"once\", \"full\" or \"all\"."
              level))))
```

```lisp
(defun %circular-p (form)
  "Return T when FORM contains a cycle reachable through conses.

Only conses are traversed.  A cycle through a literal vector or structure
is not something this guards against; macro expansions do not produce
those, and walking them would cost more than it saves.

Finished nodes are memoized, so a heavily shared but acyclic expansion
costs one visit per distinct cons instead of blowing up exponentially."
  (let ((on-path (make-hash-table :test #'eq))
        (finished (make-hash-table :test #'eq)))
    (labels ((walk (node)
               (cond
                 ((not (consp node)) nil)
                 ((gethash node on-path) t)
                 ((gethash node finished) nil)
                 (t
                  (setf (gethash node on-path) t)
                  (let ((found (or (walk (car node)) (walk (cdr node)))))
                    (remhash node on-path)
                    (setf (gethash node finished) t)
                    found)))))
      (walk form))))
```

```lisp
(defun %print-expansion (form package print-level print-length)
  "Print FORM as readable source relative to PACKAGE.

*PRINT-CIRCLE* is normally NIL so that shared-structure markers such as
#1= do not pollute the output: backquote expansions share literals all
the time and those markers make the result hard to read.

It is switched on only for a genuinely circular FORM.  *PRINT-LEVEL*
cannot be relied on to bound that case, because SBCL's pretty printer
does not honour it for the QUOTE abbreviation and would exhaust the
control stack, killing the whole worker process."
  (let ((*package* package)
        (*print-circle* (%circular-p form))
        (*print-level* (max 1 (or print-level *expansion-print-level*)))
        (*print-length* (max 1 (or print-length *expansion-print-length*)))
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* 100)
        (*print-readably* nil)
        (*print-escape* t)
        (*print-gensym* t)
        (*print-base* 10)
        (*print-radix* nil))
    (prin1-to-string form)))
```

```lisp
(defun %truncate (string max-output-length)
  "Truncate STRING then sanitize it for JSON.
Returns (values text truncated-p).  Truncation happens before
sanitization so the limit applies to the characters actually emitted."
  (let ((limit (or max-output-length *expansion-max-output-length*)))
    (if (and limit (> (length string) limit))
        (values (sanitize-for-json
                 (concatenate 'string (subseq string 0 limit) "...(truncated)"))
                t)
        (values (sanitize-for-json string) nil))))
```

```lisp
(defun %expand-one-entry (source package readtable level
                          print-level print-length max-output-length)
  "Expand SOURCE in the already-resolved PACKAGE.
Returns the tail of the result plist (everything except :LABEL)."
  (let ((form (%read-source source package readtable)))
    (multiple-value-bind (expansion steps)
        (%expand form level)
      (multiple-value-bind (printed truncated-p)
          (%truncate (%print-expansion expansion package print-level print-length)
                     max-output-length)
        (list :printed printed
              :expanded-p (plusp steps)
              :steps steps
              :truncated-p truncated-p
              :error nil)))))
```

```lisp
(defun macroexpand-forms (entries &key package level readtable
                                       print-level print-length
                                       max-output-length)
  "Expand every entry of ENTRIES, a list of (LABEL . SOURCE) conses.

Returns a list of plists in the same order, each with the keys
:LABEL :PRINTED :EXPANDED-P :STEPS :TRUNCATED-P :ERROR.  A failure in one
entry is recorded in that entry's :ERROR and does not abort the batch, so
a caller can render every entry uniformly.

PACKAGE is a package-name string; NIL means CL-USER.  LEVEL is \"once\"
(default), \"full\" or \"all\".  Both the package and the level are
validated up front, before any entry is attempted, because those failures
apply to the whole request."
  (let ((resolved-package (%resolve-package package))
        (resolved-readtable (%resolve-readtable readtable))
        (effective-level (or level "once")))
    (unless (member effective-level '("once" "full" "all") :test #'string-equal)
      (error "Unknown level ~S: expected \"once\", \"full\" or \"all\"."
             effective-level))
    (loop for (label . source) in entries
          collect (list* :label label
                         (handler-case
                             (%expand-one-entry source resolved-package
                                                resolved-readtable
                                                effective-level
                                                print-level print-length
                                                max-output-length)
                           (error (condition)
                             (list :printed nil
                                   :expanded-p nil
                                   :steps 0
                                   :truncated-p nil
                                   :error (sanitize-for-json
                                           (princ-to-string condition)))))))))
```

```lisp
(defun macroexpand-source (source &key package level readtable
                                       print-level print-length
                                       max-output-length)
  "Expand the single form in SOURCE.
Returns (values printed expanded-p steps truncated-p).  This is a thin
convenience wrapper over MACROEXPAND-FORMS so both entry points share one
implementation; unlike MACROEXPAND-FORMS it re-signals a per-entry
failure as an error instead of returning it in a plist."
  (let ((entry (first (macroexpand-forms (list (cons nil source))
                                         :package package
                                         :level level
                                         :readtable readtable
                                         :print-level print-level
                                         :print-length print-length
                                         :max-output-length max-output-length))))
    (when (getf entry :error)
      (error "~A" (getf entry :error)))
    (values (getf entry :printed)
            (getf entry :expanded-p)
            (getf entry :steps)
            (getf entry :truncated-p))))
```

- [ ] **Step 7: 括弧を検証してテストを通す**

Run: `lisp-check-parens` on `src/macroexpand-core.lisp`, then
`rove tests/lisp-macroexpand-test.lisp`
Expected: この時点で `macroexpand-*` 系の11件が PASS。終了コード 0。

注意: `macroexpand-source-rejects-unknown-level` は `macroexpand-forms` の
先頭バリデーションで弾かれる。`%expand` 内の同じ `error` は
`%expand-one-entry` 経由なので `handler-case` に捕まり `:error` になる。
二重にチェックしているのは意図的（`%expand` は単独でも安全であるべき）。

- [ ] **Step 8: コミット**

```bash
mallet src/macroexpand-core.lisp
git add src/macroexpand-core.lisp tests/lisp-macroexpand-test.lisp tests.lisp
git commit -m "$(cat <<'EOF'
feat(macroexpand): add the pure expansion core

worker handler とインラインフォールバックが共有する展開・整形ロジック。
*print-circle* は通常 NIL にして共有マーカーを排除し、循環を検出した場合に
限り有効化する。SBCL のプリティプリンタは quote 略記で *print-level* を
尊重せず、自己参照構造を渡すと制御スタックを食い尽くしてプロセスごと
落ちるため、*print-level* の有限束縛だけでは守れない。

パッケージ不在時はスタブを合成せず行動可能なエラーを返す。スタブ上で
展開すると「何も起きなかった」結果が静かに返り、呼び出し側を誤らせる。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 5: `build-macroexpand-response` — 共有レスポンスビルダ

**Files:**
- Modify: `src/tools/response-builders.lisp`

**背景:** 過去の総合テストで判明した教訓 — **sibling JSON フィールドはクライアントが
描画せず、`content[].text` だけが見える**。展開結果とメタ情報は必ずテキストに入れる。

- [ ] **Step 1: `json-bool` を import に足す**

`lisp-patch-form` で `file_path: "src/tools/response-builders.lisp"`,
`form_type: "defpackage"`, `form_name: "cl-mcp/src/tools/response-builders"`。

old_text:
```
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:text-content)
```
new_text:
```
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:text-content #:json-bool)
```

- [ ] **Step 2: export に追加する**

同じフォームに `lisp-patch-form`。

old_text:
```
           #:build-inspect-response))
```
new_text:
```
           #:build-inspect-response
           #:build-macroexpand-response))
```

- [ ] **Step 3: ビルダ本体を追加する**

`lisp-edit-form` の `insert_after` で `form_type: "defun"`,
`form_name: "build-inspect-response"` の後ろに3フォーム入れる。

```lisp
(defun %macroexpand-result->ht (result)
  "Convert one MACROEXPAND-FORMS plist into a JSON-ready hash-table."
  (make-ht "label" (getf result :label)
           "printed" (getf result :printed)
           "expanded" (json-bool (getf result :expanded-p))
           "steps" (getf result :steps)
           "truncated" (json-bool (getf result :truncated-p))
           "error" (getf result :error)))
```

```lisp
(defun %format-macroexpand-text (results level package note)
  "Render RESULTS as the agent-facing content text.
Everything a caller needs must appear here: sibling JSON fields are not
displayed by MCP clients."
  (with-output-to-string (stream)
    (format stream "lisp-macroexpand (level: ~A, package: ~A)~%"
            (or level "once")
            (or package "COMMON-LISP-USER"))
    (when note
      (format stream "~A~%" note))
    (loop for result in results
          for index from 1
          do (format stream "~%[~D] ~A~%" index (or (getf result :label) "form"))
             (cond
               ((getf result :error)
                (format stream "ERROR: ~A~%" (getf result :error)))
               ((not (getf result :expanded-p))
                (format stream
                        "NOT EXPANDED: the head of this form has no macro ~
definition in this image. If you expected a macro, load its system with ~
'load-system' first.~%~A~%"
                        (getf result :printed)))
               (t
                (format stream "expanded in ~D step~:P~A~%~A~%"
                        (getf result :steps)
                        (if (getf result :truncated-p) " (truncated)" "")
                        (getf result :printed)))))))
```

```lisp
(defun build-macroexpand-response (results &key level package note)
  "Build the standard lisp-macroexpand response hash-table.
RESULTS is the list of plists returned by MACROEXPAND-FORMS.  NOTE is an
optional advisory line (for example, that the sub-form match list was
capped) that must be visible to the caller."
  (let ((payload (make-ht "content" (text-content
                                     (%format-macroexpand-text results level
                                                               package note))
                          "level" (or level "once")
                          "package" package
                          "note" note
                          "count" (length results)
                          "expansions" (map 'vector #'%macroexpand-result->ht
                                            results))))
    (when (some (lambda (result) (getf result :error)) results)
      (setf (gethash "isError" payload) t))
    payload))
```

- [ ] **Step 4: 動作を確認する**

Run:
```bash
ros -e '(asdf:load-system :cl-mcp)' \
    -e '(princ (gethash "text" (aref (gethash "content" (cl-mcp/src/tools/response-builders:build-macroexpand-response (list (list :label "probe" :printed "(if t (progn 1))" :expanded-p t :steps 1 :truncated-p nil :error nil)) :level "once" :package "COMMON-LISP-USER")) 0)))' -q
```
Expected: 次のようなテキストが出る。
```
lisp-macroexpand (level: once, package: COMMON-LISP-USER)

[1] probe
expanded in 1 step
(if t (progn 1))
```

- [ ] **Step 5: コミット**

```bash
mallet src/tools/response-builders.lisp
git add src/tools/response-builders.lisp
git commit -m "$(cat <<'EOF'
feat(macroexpand): add build-macroexpand-response

worker とインラインフォールバックが同じ形のペイロードを返すための共有ビルダ。
クライアントは content[].text しか描画しないため、展開結果もメタ情報も
すべてテキストに入れる。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 6: worker handler `%handle-macroexpand`

**Files:**
- Modify: `src/worker/handlers.lisp`
- Test: `tests/worker-test.lisp`

**背景:** handler は**ハッシュテーブルを返さなければならない**（plist やリストを返すと
JSON 配列になってツール層が壊れる）。params は文字列キーのハッシュテーブルで届き、
JSON 配列は**ベクタ**として届く（`yason:*parse-json-arrays-as-vectors*` がイメージ全体で T）。

- [ ] **Step 1: 失敗する worker テストを2件書く**

`tests/worker-test.lisp` に `lisp-edit-form` の `insert_after` で
`form_type: "deftest"`, `form_name: "worker-code-describe-returns-info"` の後ろに追加。

```lisp
(deftest worker-macroexpand-expands-form
  (testing "worker/macroexpand expands a form in an existing package"
    (with-handler-server (stream)
      (let ((entry (make-hash-table :test 'equal))
            (params (make-hash-table :test 'equal)))
        (setf (gethash "label" entry) "probe")
        (setf (gethash "source" entry) "(when t 1)")
        (setf (gethash "forms" params) (vector entry))
        (setf (gethash "package" params) "COMMON-LISP-USER")
        (setf (gethash "level" params) "once")
        (let* ((response (%send-and-receive stream 300 "worker/macroexpand" params))
               (result (%result-of response)))
          (ok result "response has a result")
          (when result
            (ok (gethash "content" result) "result has content")
            (ok (= 1 (gethash "count" result)) "one expansion returned")
            (let ((text (gethash "text" (aref (gethash "content" result) 0))))
              (ok (search "if" text) "(when t 1) expands into an IF"))))))))

(deftest worker-macroexpand-missing-package-is-actionable
  (testing "worker/macroexpand reports an absent package as a tool error"
    (with-handler-server (stream)
      (let ((entry (make-hash-table :test 'equal))
            (params (make-hash-table :test 'equal)))
        (setf (gethash "label" entry) "probe")
        (setf (gethash "source" entry) "(when t 1)")
        (setf (gethash "forms" params) (vector entry))
        (setf (gethash "package" params) "NO-SUCH-PACKAGE-XYZZY")
        (let* ((response (%send-and-receive stream 301 "worker/macroexpand" params))
               (result (%result-of response)))
          (ok result "handled as a tool-error payload, not a transport failure")
          (when result
            (ok (gethash "isError" result) "isError is set")
            (let ((text (gethash "text" (aref (gethash "content" result) 0))))
              (ok (search "load-system" text)
                  "the message tells the caller how to recover"))))))))
```

- [ ] **Step 2: 失敗を確認する**

Run: `rove tests/worker-test.lisp`
Expected: 両テストが `Method not found: worker/macroexpand` を含む
JSON-RPC エラー（`%result-of` が NIL）で FAIL。

- [ ] **Step 3: handler の import を追加する**

`lisp-patch-form` で `file_path: "src/worker/handlers.lisp"`,
`form_type: "defpackage"`, `form_name: "cl-mcp/src/worker/handlers"`。

old_text:
```
                #:build-inspect-response)
```
new_text:
```
                #:build-inspect-response
                #:build-macroexpand-response)
  (:import-from #:cl-mcp/src/macroexpand-core
                #:macroexpand-forms
                #:macroexpand-package-error)
```

- [ ] **Step 4: handler 本体を追加する**

`lisp-edit-form` の `insert_after` で `form_type: "defun"`,
`form_name: "%handle-inspect-object"` の後ろに追加。

パッケージ不在を明示的に捕捉しているのは UX のため。捕捉しないと
`%dispatch-request` の汎用ハンドラが `"Internal error: ..."` を前置してしまい、
行動可能なメッセージが埋もれる。

```lisp
(defun %handle-macroexpand (params)
  "Expand macro forms.  Returns the same structure as define-tool
\"lisp-macroexpand\".

PARAMS carries \"forms\", a JSON array (so: a vector) of objects with
\"label\" and \"source\" keys.  The parent has already located the forms
and extracted their source text; this handler only re-reads that text in
the real, loaded package and expands it."
  (let ((forms (gethash "forms" params))
        (package (gethash "package" params))
        (level (or (gethash "level" params) "once"))
        (readtable (gethash "readtable" params))
        (note (gethash "note" params)))
    (unless (and forms (plusp (length forms)))
      (error "forms is required"))
    (let ((entries (map 'list
                        (lambda (form)
                          (cons (gethash "label" form) (gethash "source" form)))
                        forms)))
      (handler-case
          (build-macroexpand-response
           (macroexpand-forms entries
                              :package package
                              :level level
                              :readtable readtable
                              :print-level (gethash "print_level" params)
                              :print-length (gethash "print_length" params)
                              :max-output-length (gethash "max_output_length"
                                                          params))
           :level level :package package :note note)
        (macroexpand-package-error (condition)
          (make-ht "content" (text-content (princ-to-string condition))
                   "isError" t))))))
```

- [ ] **Step 5: メソッドを登録する**

`lisp-patch-form` で `file_path: "src/worker/handlers.lisp"`,
`form_type: "defun"`, `form_name: "register-all-handlers"`。
ハードコードされた件数もあわせて更新すること。

old_text:
```
  (register-method server "worker/inspect-object" #'%handle-inspect-object)
```
new_text:
```
  (register-method server "worker/inspect-object" #'%handle-inspect-object)
  (register-method server "worker/macroexpand" #'%handle-macroexpand)
```

続けて同じフォームにもう1回 `lisp-patch-form`:

old_text:
```
  (log-event :info "worker.handlers.registered" "count" 10)
```
new_text:
```
  (log-event :info "worker.handlers.registered" "count" 11)
```

- [ ] **Step 6: worker テストを通す**

Run: `rove tests/worker-test.lisp`
Expected: 新規2件を含め全件 PASS、終了コード 0。

`(asdf:compile-system :cl-mcp :force t)` は `src/worker/*.lisp` を
コンパイルしないので、このテスト実行が handlers.lisp のコンパイル検証を兼ねる。

- [ ] **Step 7: コミット**

```bash
mallet src/worker/handlers.lisp
git add src/worker/handlers.lisp tests/worker-test.lisp
git commit -m "$(cat <<'EOF'
feat(macroexpand): add the worker/macroexpand method

parent が切り出したソーステキストを、実際にロード済みのパッケージで
再読込して展開する。パッケージ不在は明示的に捕捉し、汎用ハンドラの
"Internal error:" 前置に埋もれないようにする。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 7: `src/lisp-macroexpand.lisp` — parent 側アドレッシングとツール定義

**Files:**
- Create: `src/lisp-macroexpand.lisp`
- Modify: `tests/lisp-macroexpand-test.lisp`

**設計上の要点:**
- parent は**ソーステキストそのもの**を worker に渡す。解析済み S 式を渡さないのは、
  `parse-top-level-forms` が合成したスタブパッケージを戻る前に削除するため、
  CST 内のシンボルが **homeless** になり `symbol-package` が NIL になるから。
  文字列比較だけが安全であり、再読込は worker の実パッケージで行うのが正しい。
- `%locate-target-form` が8つの値を返す（docstring は "seven" と書いてあるが誤り）。

- [ ] **Step 1: 失敗するテストを4件追加する**

`tests/lisp-macroexpand-test.lisp` の `defpackage` に import を足してから、
末尾にテストを追加する。

まず `lisp-patch-form` で `form_type: "defpackage"`,
`form_name: "cl-mcp/tests/lisp-macroexpand-test"`:

old_text:
```
  (:import-from #:cl-mcp/src/macroexpand-core
                #:macroexpand-source
                #:macroexpand-forms
                #:macroexpand-package-error))
```
new_text:
```
  (:import-from #:cl-mcp/src/macroexpand-core
                #:macroexpand-source
                #:macroexpand-forms
                #:macroexpand-package-error)
  (:import-from #:cl-mcp/src/lisp-macroexpand
                #:lisp-macroexpand))
```

次に `lisp-edit-form` の `insert_after` で
`form_type: "deftest"`, `form_name: "macroexpand-forms-keeps-going-after-one-entry-fails"`
の後ろに以下を追加する。

```lisp
;;; ---------------------------------------------------------------------------
;;; Parent-side addressing.  Files are written under tests/tmp/ (gitignored)
;;; and removed afterwards; tests/fixtures/ is untracked and unused.
;;; ---------------------------------------------------------------------------

(defun project-path (relative)
  "Return an absolute namestring under the cl-mcp project for RELATIVE."
  (uiop:native-namestring
   (uiop:merge-pathnames* relative (asdf:system-source-directory :cl-mcp))))

(defun call-with-temp-source (relative content thunk)
  "Write CONTENT to RELATIVE under the project, call THUNK with the
absolute path, then delete the file."
  (let ((absolute (project-path relative)))
    (ensure-directories-exist absolute)
    (with-open-file (out absolute :direction :output :if-exists :supersede)
      (write-string content out))
    (unwind-protect (funcall thunk absolute)
      (ignore-errors (delete-file absolute)))))

(defun response-text (payload)
  "Return the content[0].text string of a tool response PAYLOAD."
  (gethash "text" (aref (gethash "content" payload) 0)))

(deftest lisp-macroexpand-file-mode-top-level
  (testing "the addressed top-level form is expanded when sub_form is omitted"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-toplevel.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(define-thing *thing-a* 1)

(define-thing *thing-b* 2)
"
       (lambda (path)
         (let* ((payload (lisp-macroexpand :path path
                                           :form-type "define-thing"
                                           :form-name "*thing-b*"))
                (text (response-text payload)))
           (ok (= 1 (gethash "count" payload)) "exactly one form addressed")
           (ok (search "defparameter" text) "the macro expanded")
           (ok (search "*thing-b*" text) "the second definition was selected")
           (ok (null (search "*thing-a*" text))
               "the first definition was not selected")))))))

(deftest lisp-macroexpand-file-mode-sub-form
  (testing "sub_form expands a macro call nested inside the addressed form"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-subform.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(defun uses-double (n)
  (let ((base n))
    (double-it base)))
"
       (lambda (path)
         (let* ((payload (lisp-macroexpand :path path
                                           :form-type "defun"
                                           :form-name "uses-double"
                                           :sub-form "double-it"))
                (text (response-text payload)))
           (ok (= 1 (gethash "count" payload)))
           (ok (search "(* 2 base)" text)
               "the nested call was expanded, not the enclosing defun")))))))

(deftest lisp-macroexpand-sub-form-multiple-matches
  (testing "every matching sub-form is expanded and labelled with its position"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-multi.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(defun uses-double-twice (a b)
  (list (double-it a)
        (double-it b)))
"
       (lambda (path)
         (let* ((payload (lisp-macroexpand :path path
                                           :form-type "defun"
                                           :form-name "uses-double-twice"
                                           :sub-form "double-it"))
                (text (response-text payload)))
           (ok (= 2 (gethash "count" payload)) "both calls were expanded")
           (ok (search "(* 2 a)" text))
           (ok (search "(* 2 b)" text))
           (ok (search "[1/2]" text)
               "labels carry the position out of the total")))))))

(deftest lisp-macroexpand-rejects-sub-form-with-readtable
  (testing "sub_form combined with readtable fails with an actionable message"
    (ok (handler-case
            (progn (lisp-macroexpand :path "irrelevant.lisp"
                                     :form-type "defun"
                                     :form-name "f"
                                     :sub-form "g"
                                     :readtable "some-syntax")
                   nil)
          (error (e) (and (search "sub_form" (princ-to-string e)) t)))
        "the error should name the offending argument")))

(deftest lisp-macroexpand-requires-exactly-one-mode
  (testing "path and code are mutually exclusive, and one of them is required"
    (ok (handler-case (progn (lisp-macroexpand) nil)
          (error () t))
        "neither path nor code is rejected")
    (ok (handler-case (progn (lisp-macroexpand :path "a.lisp" :code "(f)") nil)
          (error () t))
        "both path and code is rejected")))

(deftest lisp-macroexpand-code-mode
  (testing "code mode expands a caller-supplied form without touching disk"
    (let* ((payload (lisp-macroexpand :code "(double-it 5)"
                                      :package *fixture-package*))
           (text (response-text payload)))
      (ok (search "(* 2 5)" text)))))
```

`define-thing` フィクスチャマクロも必要。`lisp-edit-form` の `insert_after` で
`form_type: "defmacro"`, `form_name: "cyclic-expansion-macro"` の後ろに追加する。

```lisp
(defmacro define-thing (name value)
  "Test macro that expands into a DEFPARAMETER, for top-level addressing tests."
  `(defparameter ,name ,value))
```

- [ ] **Step 2: 失敗を確認する**

Run: `rove tests/lisp-macroexpand-test.lisp`
Expected: `cl-mcp/src/lisp-macroexpand` が見つからずロード失敗。

- [ ] **Step 3: `src/lisp-macroexpand.lisp` の骨格を作る**

`fs-write-file` で新規作成する。

```lisp
;;;; src/lisp-macroexpand.lisp
;;;;
;;;; MCP tool definition for lisp-macroexpand.  This file owns the
;;;; parent-side half of the job: resolving the path, locating the target
;;;; form in the CST, and slicing out its source text.  The expansion
;;;; itself happens in the worker (src/worker/handlers.lisp), because only
;;;; the worker image has the macro definitions loaded.
;;;;
;;;; The parent deliberately ships raw source TEXT rather than a parsed
;;;; form: parse-top-level-forms deletes any package it synthesized before
;;;; returning, so symbols recovered from the CST can be homeless and
;;;; would print with the wrong package qualification.

(defpackage #:cl-mcp/src/lisp-macroexpand
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:arg-validation-error)
  (:import-from #:cl-mcp/src/tools/response-builders
                #:build-macroexpand-response)
  (:import-from #:cl-mcp/src/proxy
                #:with-proxy-dispatch)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:%locate-target-form
                #:%parse-readtable-designator)
  (:import-from #:cl-mcp/src/cst
                #:cst-node-kind
                #:cst-node-value
                #:cst-node-children
                #:cst-node-start
                #:cst-node-end
                #:cst-node-start-line)
  (:import-from #:cl-mcp/src/macroexpand-core
                #:macroexpand-forms)
  (:export #:lisp-macroexpand))

(in-package #:cl-mcp/src/lisp-macroexpand)

(defparameter *max-sub-form-matches* 10
  "Maximum number of SUB_FORM matches expanded in one call.
When more match, the extras are dropped and the response carries a note
saying so — a silently truncated list would read as full coverage.")
```

- [ ] **Step 4: 括弧を検証する**

Run: `lisp-check-parens` on `src/lisp-macroexpand.lisp`
Expected: balanced。

- [ ] **Step 5: アドレッシングの各関数を追加する**

`lisp-edit-form` の `insert_after` で、`form_type: "defparameter"`,
`form_name: "*max-sub-form-matches*"` の後ろに順に追加していく。

```lisp
(defun %bare-symbol-name (name)
  "Return NAME without any package qualifier: \"pkg:sym\" becomes \"sym\"."
  (let ((colon (position #\: name :from-end t)))
    (if colon
        (subseq name (1+ colon))
        name)))
```

```lisp
(defun %find-sub-forms (target sub-form)
  "Return the CST nodes strictly inside TARGET whose head names SUB-FORM.

Comparison is case-insensitive and ignores any package qualifier written
in SUB-FORM, because symbols recovered from the CST may be homeless after
package-context teardown and only their SYMBOL-NAME is reliable."
  (let ((wanted (%bare-symbol-name sub-form))
        (found '()))
    (labels ((head-name (node)
               (let ((children (cst-node-children node)))
                 (when children
                   (let ((head (cst-node-value (first children))))
                     (and (symbolp head) (symbol-name head))))))
             (walk (node)
               (when (eq (cst-node-kind node) :expr)
                 (let ((name (head-name node)))
                   (when (and name (string-equal name wanted))
                     (push node found)))
                 (dolist (child (cst-node-children node))
                   (walk child)))))
      (dolist (child (cst-node-children target))
        (walk child)))
    (nreverse found)))
```

```lisp
(defun %sub-form-entries (original target sub-form file)
  "Return (values ENTRIES NOTE) for every SUB-FORM call inside TARGET.
ENTRIES is a list of (LABEL . SOURCE) conses; NOTE is non-NIL when the
match list was capped at *MAX-SUB-FORM-MATCHES*."
  (let* ((matches (%find-sub-forms target sub-form))
         (total (length matches)))
    (when (null matches)
      (error "No call to ~A found inside the addressed form in ~A."
             sub-form file))
    (let* ((kept (if (> total *max-sub-form-matches*)
                     (subseq matches 0 *max-sub-form-matches*)
                     matches))
           (note (when (> total *max-sub-form-matches*)
                   (format nil "NOTE: ~D calls to ~A matched; showing the first ~D."
                           total sub-form *max-sub-form-matches*))))
      (values
       (loop for node in kept
             for index from 1
             collect (cons (format nil "~A (~A line ~D)~A"
                                   sub-form
                                   (file-namestring file)
                                   (cst-node-start-line node)
                                   (if (> total 1)
                                       (format nil " [~D/~D]" index total)
                                       ""))
                           (subseq original
                                   (cst-node-start node)
                                   (cst-node-end node))))
       note))))
```

```lisp
(defun %collect-file-sources (path form-type form-name sub-form package readtable)
  "Locate the addressed form in PATH.
Returns (values ENTRIES PACKAGE-NAME NOTE)."
  (unless form-type
    (error 'arg-validation-error :arg-name "form_type"
           :message "form_type is required when 'path' is given."))
  (unless form-name
    (error 'arg-validation-error :arg-name "form_name"
           :message "form_name is required when 'path' is given."))
  (when (and sub-form readtable)
    (error 'arg-validation-error :arg-name "sub_form"
           :message "sub_form cannot be combined with 'readtable': a custom ~
readtable forces the standard CL reader, which does not record sub-form ~
positions. Drop one of the two arguments."))
  (let ((designator (%parse-readtable-designator readtable)))
    (multiple-value-bind (absolute relative original nodes target snippet
                          form-type-string file-package-name)
        (%locate-target-form path form-type form-name designator)
      (declare (ignore relative nodes))
      (let ((package-name (or package file-package-name "COMMON-LISP-USER")))
        (if sub-form
            (multiple-value-bind (entries note)
                (%sub-form-entries original target sub-form
                                   (namestring absolute))
              (values entries package-name note))
            (values (list (cons (format nil "~A ~A (~A line ~D)"
                                        form-type-string form-name
                                        (file-namestring absolute)
                                        (cst-node-start-line target))
                                snippet))
                    package-name
                    nil))))))
```

```lisp
(defun %collect-expansion-sources (&key path form-type form-name sub-form code
                                        package readtable)
  "Resolve the request into expandable source text.

Returns (values ENTRIES PACKAGE-NAME NOTE), where ENTRIES is a list of
(LABEL . SOURCE) conses.  Exactly one of PATH and CODE must be supplied."
  (when (and path code)
    (error 'arg-validation-error :arg-name "code"
           :message "Provide either 'path' (with form_type and form_name) or ~
'code', not both."))
  (when (and (null path) (null code))
    (error 'arg-validation-error :arg-name "path"
           :message "Provide either 'path' (with form_type and form_name) or ~
'code'."))
  (if code
      (values (list (cons nil code))
              (or package "COMMON-LISP-USER")
              nil)
      (%collect-file-sources path form-type form-name sub-form package
                             readtable)))
```

```lisp
(defun %entries->json (entries)
  "Convert (LABEL . SOURCE) conses into a JSON-ready vector of hash-tables."
  (map 'vector
       (lambda (entry)
         (make-ht "label" (car entry) "source" (cdr entry)))
       entries))
```

```lisp
(defun lisp-macroexpand (&key path form-type form-name sub-form code package
                              level readtable print-level print-length
                              max-output-length)
  "Locate the target form(s) and expand them in THIS image.

Returns the tool response payload hash-table.  This is the inline path
used when the worker pool is disabled; normally the tool proxies to the
worker instead, because only the worker has the target system loaded."
  (multiple-value-bind (entries package-name note)
      (%collect-expansion-sources :path path :form-type form-type
                                  :form-name form-name :sub-form sub-form
                                  :code code :package package
                                  :readtable readtable)
    (let ((effective-level (or level "once")))
      (build-macroexpand-response
       (macroexpand-forms entries
                          :package package-name
                          :level effective-level
                          :readtable readtable
                          :print-level print-level
                          :print-length print-length
                          :max-output-length max-output-length)
       :level effective-level :package package-name :note note))))
```

- [ ] **Step 6: `define-tool` を追加する**

`lisp-edit-form` の `insert_after` で `form_type: "defun"`,
`form_name: "lisp-macroexpand"` の後ろに追加する。

`:body` は**1つのフォームでなければならない**（マクロは `,body` であって
`,@body` ではない）。`with-proxy-dispatch` の params フォームは proxy 分岐でのみ
評価されるので、parent 側のアドレッシングをそこに置くのが正しい。

```lisp
(define-tool "lisp-macroexpand" :description
 "Expand a macro call and show the resulting source.

Two ways to name what to expand:
1. FILE MODE — 'path' plus 'form_type' and 'form_name', the same addressing
   'lisp-edit-form' uses. Add 'sub_form' to expand a macro call nested INSIDE
   that top-level form (e.g. a 'with-...' block inside a defun). This is the
   reason to prefer this tool over calling macroexpand-1 through 'repl-eval':
   you never have to copy the form into a string and escape it.
2. CODE MODE — 'code' plus 'package', for a call site you compose yourself.

'level' controls how far to go:
  once (default) — one macroexpand-1 step, best for checking a macro you just wrote
  full           — repeat until the head is no longer a macro
  all            — walk the whole form and expand nested macros too. Output can
                   get very large; loop and defun expand down to special forms.

PREREQUISITE: the macro must be DEFINED in the worker image. If the package
does not exist you get an error telling you to run 'load-system' first; after
editing a defmacro on disk, reload before expanding.

Output is printed lower-case, pretty-printed, and relative to the target
package, so it can be read as source. Shared-structure markers (#1=) are
suppressed; depth and length are bounded instead.

LIMITATIONS: expansion uses a null lexical environment, so a form enclosed in
macrolet or symbol-macrolet may expand differently than the compiler sees it.
'sub_form' cannot be combined with 'readtable'. Expanding runs the macro's
expander function, i.e. arbitrary code, in the isolated worker process."
 :args
 ((path :type :string :description
   "File containing the form; use with form_type and form_name")
  (form_type :type :string :description
   "Defining form type, e.g. defun, defmacro, define-tool")
  (form_name :type :string :description
   "Name of the form; supports 'name[N]' to pick the Nth match")
  (sub_form :type :string :description
   "Macro name to expand INSIDE the addressed form; expands every call, up to 10")
  (code :type :string :description
   "Source text of a form to expand; alternative to path")
  (package :type :string :description
   "Package to read the form in; defaults to the file's in-package, else CL-USER")
  (level :type :string :enum ("once" "full" "all") :description
   "Expansion depth: once (default), full, or all")
  (readtable :type :string :description
   "Named-readtable designator for files using custom reader macros")
  (print_level :type :integer :description
   "Maximum printed nesting depth (default 50; must stay finite)")
  (print_length :type :integer :description
   "Maximum printed list length (default 1000)")
  (max_output_length :type :integer :description
   "Maximum characters per expansion (default 50000)"))
 :body
 (with-proxy-dispatch (id "worker/macroexpand"
                         (multiple-value-bind (entries package-name note)
                             (%collect-expansion-sources
                              :path path :form-type form_type
                              :form-name form_name :sub-form sub_form
                              :code code :package package
                              :readtable readtable)
                           (make-ht "forms" (%entries->json entries)
                                    "package" package-name
                                    "note" note
                                    "level" (or level "once")
                                    "readtable" readtable
                                    "print_level" print_level
                                    "print_length" print_length
                                    "max_output_length" max_output_length)))
   (result id
           (lisp-macroexpand :path path :form-type form_type
                             :form-name form_name :sub-form sub_form
                             :code code :package package :level level
                             :readtable readtable :print-level print_level
                             :print-length print_length
                             :max-output-length max_output_length))))
```

- [ ] **Step 7: ツールモジュールをロード対象に登録する**

これを忘れるとツールは**エラーも警告もなく** `tools/list` に現れない。

`lisp-patch-form` で `file_path: "src/tools/all.lisp"`,
`form_type: "defpackage"`, `form_name: "cl-mcp/src/tools/all"`:

old_text:
```
  (:import-from #:cl-mcp/src/project-scaffold
                #:project-scaffold))
```
new_text:
```
  (:import-from #:cl-mcp/src/project-scaffold
                #:project-scaffold)
  (:import-from #:cl-mcp/src/lisp-macroexpand
                #:lisp-macroexpand))
```

- [ ] **Step 8: 公開 API に追加する**

既存15ツールすべてが `main.lisp` にも載っている慣習に従う。

`lisp-patch-form` で `file_path: "main.lisp"`, `form_type: "defpackage"`,
`form_name: "cl-mcp/main"`:

old_text:
```
  (:import-from #:cl-mcp/src/lisp-patch-form
                #:lisp-patch-form)
```
new_text:
```
  (:import-from #:cl-mcp/src/lisp-patch-form
                #:lisp-patch-form)
  (:import-from #:cl-mcp/src/lisp-macroexpand
                #:lisp-macroexpand)
```

続けて同じフォームにもう1回 `lisp-patch-form`:

old_text:
```
           #:lisp-patch-form
```
new_text:
```
           #:lisp-patch-form
           #:lisp-macroexpand
```

- [ ] **Step 9: 括弧を検証してテストを通す**

Run: `lisp-check-parens` on `src/lisp-macroexpand.lisp`, then
`rove tests/lisp-macroexpand-test.lisp`
Expected: 全17件が PASS、終了コード 0。

- [ ] **Step 10: ツールが登録されたことを確認する**

Run:
```bash
ros -e '(asdf:load-system :cl-mcp)' \
    -e '(princ (and (gethash "lisp-macroexpand" cl-mcp/src/tools/registry:*tool-registry*) t))' -q
```
Expected: `T`。NIL なら Step 7 の `src/tools/all.lisp` の編集が効いていない。

- [ ] **Step 11: コミット**

```bash
mallet src/lisp-macroexpand.lisp
git add src/lisp-macroexpand.lisp src/tools/all.lisp main.lisp \
        tests/lisp-macroexpand-test.lisp
git commit -m "$(cat <<'EOF'
feat(macroexpand): add the lisp-macroexpand tool

lisp-edit-form と同じアドレッシングでファイル中のフォームを直接展開する。
sub_form で defun 等の内側にネストしたマクロ呼び出しも指定できる。
parent は CST でフォームを特定してソーステキストを切り出すだけで、
展開は worker が実パッケージで行う。

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 8: ドキュメント更新

**Files:**
- Modify: `prompts/repl-driven-development.md`

- [ ] **Step 1: ツール早見表に行を足す**

`fs-read-file` で該当箇所を確認してから編集する。`**Tool Cheat Sheet:**` の表の
`| Check syntax | \`lisp-check-parens\` | \`path\` or \`code\` (string) |` の直後に挿入する。

```markdown
| Expand macro | `lisp-macroexpand` | `path`+`form_type`+`form_name`, or `code`; `sub_form`, `level` |
```

- [ ] **Step 2: ワーカープール一覧を更新する**

`## Worker Pool Architecture` の **Parent process** リストの
`- Lisp-aware:` 行に `lisp-macroexpand` を足す。展開自体は worker だが、
フォーム特定は parent で行い、ツール自体は proxy 経由なので
**Worker process** 側の一覧に入れる。

`- \`repl-eval\`, \`load-system\`, \`run-tests\`` を
`- \`repl-eval\`, \`load-system\`, \`run-tests\`, \`lisp-macroexpand\`` に変更する。

- [ ] **Step 3: ツール選択ガイドに項目を足す**

`## Tool Selection` の `- **EDIT**` ブロックの直後に以下を挿入する。

```markdown
- **EXPAND MACROS**
  - Macro call in a file -> `lisp-macroexpand` with `path`+`form_type`+`form_name`
  - Macro call nested inside a defun -> add `sub_form` with the macro's name
  - Call site you compose yourself -> `lisp-macroexpand` with `code`+`package`
  - Requires the macro's system to be loaded (`load-system`) in the worker
```

- [ ] **Step 4: コミット**

```bash
git add prompts/repl-driven-development.md
git commit -m "$(cat <<'EOF'
docs: document lisp-macroexpand in the agent prompt

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
)"
```

---

## Task 9: PR 前の最終検証

- [ ] **Step 1: 全体を強制再コンパイルして警告を確認する**

Run: `ros -e '(asdf:load-system :cl-mcp)' -e '(asdf:compile-system :cl-mcp :force t)' -q`
Expected: `cl-mcp` 由来の warning がゼロ。UIOP の `redefining ...` 約427件は既知のノイズ。

これは `src/worker/*.lisp` を**コンパイルしない**ことに注意（Step 2 が必要な理由）。

- [ ] **Step 2: worker を含む全テストを通す**

Run: `rove cl-mcp.asd`
Expected: 終了コード 0。個別の ✓/✗ 行を目視で確認する
（rove のサマリ件数は過少表示される）。

- [ ] **Step 3: lint**

Run: `mallet src/*.lisp`
Expected: 指摘ゼロ。

- [ ] **Step 4: 実際に動かして確認する**

新しい MCP サーバプロセスを起動し、cl-mcp 自身のソースに対して
ツールを1回叩く。`src/repl.lisp` の `repl-eval` 内にある
`with-proxy-dispatch` は sub_form の実例として最適。

```bash
ros -e '(asdf:load-system :cl-mcp)' \
    -e '(setf cl-mcp/src/project-root:*project-root* (asdf:system-source-directory :cl-mcp))' \
    -e '(princ (gethash "text" (aref (gethash "content" (cl-mcp/src/lisp-macroexpand:lisp-macroexpand :path "src/repl.lisp" :form-type "define-tool" :form-name "repl-eval" :sub-form "with-proxy-dispatch")) 0)))' -q
```
Expected: `(if cl-mcp/src/proxy:*use-worker-pool* ...)` を含む小文字の展開が
1件表示され、`#1=` 共有マーカーが含まれないこと。

- [ ] **Step 5: PR を作る**

```bash
git push -u origin feature/macroexpand-tool
gh pr create --title "feat: add lisp-macroexpand tool and fix repl-eval result printing" --body "$(cat <<'EOF'
## 概要

設計文書: `docs/superpowers/specs/2026-07-26-macroexpand-tool-design.md`

マクロ展開は `repl-eval` で既に可能だったため機能ギャップはなかったが、
実測すると (1) 印字設定が悪く出力がほぼ読めない、(2) ファイル中のフォームを
直接展開する手段がない、という2点が確認された。二段階で対応している。

## Phase A — `repl-eval` の結果印字修正

`%do-repl-eval` の最終印字ブロックは `handler-bind` の兄弟にあり、`*package*` を
束縛する動的エクステントを既に抜けていた。そのため全シンボルが呼び出し側基準で
完全修飾されていた（`define-tool` の展開ではトークンの約6割がパッケージ接頭辞）。

- eval パッケージを控えて印字時に束縛
- `*print-case* :downcase` と `*print-pretty*` を有効化
- `*print-circle* t` は循環構造でのハング防止として**維持**
- `*print-pretty*` を `%eval-forms` の隔離リストにも追加

マクロ展開に限らず全ツールの結果に効く。

## Phase C — `lisp-macroexpand` ツール

`lisp-edit-form` と同じアドレッシングでファイル中のフォームを直接展開する。
`sub_form` で defun 等の内側にネストした呼び出しも指定できる。

3層構成:
- `src/macroexpand-core.lisp` — 展開と整形の純粋ロジック（worker とインライン経路が共有）
- `src/lisp-macroexpand.lisp` — parent 側の CST アドレッシングとツール定義
- `src/worker/handlers.lisp` — `worker/macroexpand`

parent はソーステキストを切り出して渡すだけで、展開は worker が実パッケージで行う。
`parse-top-level-forms` は合成したスタブパッケージを戻る前に削除するため、
CST 内のシンボルは homeless になりうる。テキストを渡すのが唯一堅牢な方法。

## 設計上のトレードオフ

`*print-circle*` はツール側では NIL にして `#1=` 共有マーカーを排除し、
ハング防止は `*print-level*` の有限束縛で担保している。`repl-eval` の汎用結果は
本当に循環しうるのに対しマクロ展開結果は実質有限の木なので、要件が異なる。

## テスト

- `tests/lisp-macroexpand-test.lisp`（新規17件）
- `tests/worker-test.lisp` に worker 経路2件
- `tests/repl-test.lisp` に Phase A 用5件、既存2件を小文字化に合わせて更新

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

---

## 補遺: 実装中に踏みやすい罠

調査で確認済み。該当したら思い出すこと。

| 症状 | 原因 |
|---|---|
| 新しいツールが `tools/list` に出ない | `src/tools/all.lisp` への import 漏れ。エラーも警告も出ない |
| 新しいテストが CI で走らない | ルートの `tests.lisp` への `:import-from` 漏れ |
| `handlers.lisp` の構文エラーに気づかない | `compile-system` は `src/worker/*` をコンパイルしない。worker テストで検出する |
| handler が返した値でツール層が壊れる | handler はハッシュテーブルを返すこと。plist は JSON 配列になる |
| ブール引数の既定値が効かない | `define-tool` は宣言済みキーを必ず params に書き込む（NIL は JSON null）。既定値は parent 側の `:default` で宣言する |
| 配列パラメータで `dolist` が落ちる | JSON 配列は**ベクタ**で届く（`yason:*parse-json-arrays-as-vectors*` が T） |
| `sub_form` が何も見つけない | `readtable` を併用した。CL リーダー経路では `cst-node-children` が常に NIL |
| CST のシンボルで `eq` 比較が効かない | スタブパッケージが削除済みで homeless。`symbol-name` の文字列比較のみ安全 |
| `%locate-target-form` の値が足りない | docstring は "seven" だが実際は**8つ**返す |
| ASDF エラー: ファイルが期待するパッケージを定義していない | `src/<n>.lisp` は `cl-mcp/src/<n>` をバイト一致で定義しなければならない |
| grep が重複ヒットする | `*.lisp~` バックアップファイルが実ソースの隣にある。除外すること |
