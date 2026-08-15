# 深いネストによる制御スタック枯渇と接続ハングの修正 実装計画

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 深くネストした Lisp フォームが接続スレッドを永久停止させる Critical を、深さ上限と接続スレッド単位のデバッガフックの 2 層で塞ぐ。

**Architecture:** 既存の文字列・コメント対応スキャナ `%scan-parens` を `src/utils/` に降ろして深さ追跡を足し、CST 経路と標準リーダー経路の双方が同じ判定を使う。あわせて TCP / HTTP / stdio の各接続スレッドで `sb-ext:*invoke-debugger-hook*` を束縛し、デバッガに入る代わりにその接続だけを畳む。

**Tech Stack:** SBCL 2.5.8 / Eclector CST / Rove / ASDF package-inferred-system / bordeaux-threads / Hunchentoot

**設計仕様:** `docs/superpowers/specs/2026-08-02-deep-nesting-control-stack-exhaustion-design.md`

## Global Constraints

- 対象は SBCL のみ。他処理系への配慮は不要
- **Lisp ソースの編集は `lisp-edit-form` / `lisp-patch-form` を使う。** テキスト編集ツールは括弧を壊す。新規ファイルの雛形作成のみ `fs-write-file` を使い、その後は構造化ツールで広げる
- **ASDF に新ファイルを登録する場所**: `cl-mcp.asd` の編集は不要。`src/utils/paren-scan.lisp` は package-inferred-system が自動で拾う。ただし新しい `defpackage` を作るので、参照する側の `:import-from` を正しく書くこと
- **`%scan-parens` の移動は挙動を変えない移動に限る。** 同関数は `src/validate.lisp` の中核で、同ファイルは式カバレッジ 85.5%。移動後も既存テストが全て緑であることが等価性の根拠
- **上限値は推測せず Task 1 の実測に基づいて決める**
- **グローバルな `(sb-ext:disable-debugger)` は使わない。** `cl-mcp:run` は本番の入口であると同時にテストが呼ぶ関数でもある（`tests/core-test.lisp:25,38`）
- コミットメッセージ末尾に `Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>` を付ける
- 作業ブランチ: `fix/deep-nesting-control-stack-exhaustion`（`main` から分岐、仕様が `9b10be5` に載っている）
- コミット前に `mallet src/*.lisp` が通ること

## 実測済みの前提（再調査不要）

| 事実 | 根拠 |
|------|------|
| `sb-ext:disable-debugger` はワーカー子プロセスのみ | `src/worker/main.lisp:188` |
| このリポジトリ 123 ファイルの最大ネスト深さは **20** | `src/proxy.lisp`、`src/pool.lisp` |
| 監査で致命的だった深さは **20,000** | `docs/plans/2026-08-01-audit-edge-cases.md` 所見1 |
| 過大入力は例外でなく構造化結果で返す慣習 | `src/validate.lisp:295` の `"kind": "too-large"` |
| スキャナは開き括弧のスタックを持つが**最大深さは記録していない** | `src/validate.lisp:128-183` |
| 括弧の push / pop は `%scan-handle-normal` の 1 箇所ずつ | `src/validate.lisp:106-117` |
| CST 経路の単一の入口 | `src/cst.lisp:256` `parse-top-level-forms` |
| TCP の接続スレッド本体 | `src/tcp.lisp:233` `%tcp-handle-client` |
| HTTP の要求ごとの入口 | `src/http.lisp:448` `acceptor-dispatch-request` |
| stdio ループ | `src/run.lisp:42` `(:stdio ...)` 分岐 |

## File Structure

| ファイル | 責務 |
|---------|------|
| `src/utils/paren-scan.lisp` (新規) | Lisp テキストの文字レベルスキャナ。括弧の釣り合いと**最大ネスト深さ**を返す。`validate` と `cst` の共通土台 |
| `src/validate.lisp` (変更) | スキャナを import に切り替え、`too-deep` を返す |
| `src/cst.lisp` (変更) | `parse-top-level-forms` の入口で深さを検査 |
| `src/tcp.lisp` / `src/http.lisp` / `src/run.lisp` (変更) | 接続スレッドでデバッガフックを束縛 |
| `tests/utils-paren-scan-test.lisp` (新規) | スキャナの深さ追跡 |
| `tests/deep-nesting-test.lisp` (新規) | 4 ツールの回帰と、デバッガフックの 3 経路 |
| `tests.lisp` (変更) | 新テストパッケージを 2 つ登録 |

---

### Task 1: 破綻深度を実測する

**上限値を推測で決めない。** このリポジトリが直前のブランチで学んだのは「測っていない数値を出荷しない」こと。

**Files:** なし（測定のみ。コミットなし）

**Interfaces:**
- Produces: 破綻深度の実測値（Task 3 の `*max-nesting-depth*` の根拠）

- [ ] **Step 1: 測定スクリプトを書く**

`/tmp/depth-probe.sh` に置く（リポジトリには入れない）:

```bash
#!/bin/bash
# 深さ N のネストを CST 経路に通し、生き残るかを見る。
# 破綻するとプロセスごと落ちるので、必ず 1 深さ 1 プロセスで測る。
probe() {
  local d=$1
  env -u ASDF_OUTPUT_TRANSLATIONS timeout 180 ros -Q \
    -e '(asdf:load-system :cl-mcp)' \
    -e "(let ((s (concatenate 'string
                              (make-string $d :initial-element #\\()
                              \":deep\"
                              (make-string $d :initial-element #\\)))))
          (cl-mcp/src/cst::parse-top-level-forms s)
          (format t \"~&PROBE-OK ~D~%\" $d))" \
    > /tmp/depth-probe-$d.log 2>&1
  if grep -q "PROBE-OK" /tmp/depth-probe-$d.log; then echo "OK"; else echo "DEAD"; fi
}
for d in "$@"; do echo "depth=$d -> $(probe $d)"; done
```

- [ ] **Step 2: 粗く当たりをつける**

```bash
chmod +x /tmp/depth-probe.sh && /tmp/depth-probe.sh 100 500 1000 2000 5000 10000 20000
```

期待: 小さい深さは `OK`、どこかで `DEAD` に変わる。**20000 は必ず `DEAD` になる**（監査の実測）。
そうならなければ環境が違うので、そのまま報告して止まること。

- [ ] **Step 3: 境界を二分探索で詰める**

Step 2 で `OK` だった最大値と `DEAD` だった最小値の間を、`/tmp/depth-probe.sh` に中間値を渡して詰める。
**3 回程度で十分**であり、1 の位まで求める必要はない。桁が分かればよい。

- [ ] **Step 4: 標準リーダー経路も同じ深さで確認する**

`lisp-check-parens` は CST ではなく標準 CL リーダーを通る（`src/validate.lisp:208` `%try-reader-check`）。
経路が違えば破綻深度も違いうる:

```bash
env -u ASDF_OUTPUT_TRANSLATIONS timeout 180 ros -Q \
  -e '(asdf:load-system :cl-mcp)' \
  -e '(let ((s (concatenate (quote string) (make-string 3000 :initial-element #\() ":deep" (make-string 3000 :initial-element #\)))))
        (with-input-from-string (in s) (read in))
        (format t "~&READER-OK~%"))' 2>&1 | tail -3
```

3000 を Step 3 で求めた境界付近の値に置き換えて確認する。**2 経路の低い方**を採用の基準にする。

- [ ] **Step 5: 上限値を決めて記録する**

`*max-nesting-depth*` の値を決める。基準は 2 つ:

- 実コードの最大 **20** をはるかに上回ること
- Step 3/4 の破綻深度をはるかに下回ること

`macroexpand-core.lisp` の `*max-walk-expansions*` は実コスト 14 に対し 1000（約 70 倍）で、致命的深度
5,800 の約 1/6 に置いている。同じ比率感で決める。

測定結果（各深さの OK/DEAD、採用した上限値とその理由）を次のタスクに渡せるよう控える。
コミットするものはない。

---

### Task 2: スキャナを `src/utils/paren-scan.lisp` に移す

**挙動を変えない移動。** この 1 タスクで新機能は入れない。既存テストが全て緑であることが等価性の根拠。

**Files:**
- Create: `src/utils/paren-scan.lisp`
- Modify: `src/validate.lisp`

**Interfaces:**
- Produces: パッケージ `cl-mcp/src/utils/paren-scan`、export する `#:scan-parens`（旧 `%scan-parens`）

- [ ] **Step 1: 移動前のベースラインを取る**

```bash
rove cl-mcp.asd 2>&1 | tail -5
```

期待: 終了コード 0。ログに現れる `× 1 of 1 test failed` は `project-scaffold` の意図的な
`rove-red` フィクスチャで**正常**。所要 6 分程度。この結果を控える。

- [ ] **Step 2: 新ファイルの雛形を作る**

`fs-write-file` で `src/utils/paren-scan.lisp` を作る:

```lisp
;;;; src/utils/paren-scan.lisp
;;;;
;;;; Lisp テキストを文字単位で走査し、括弧の釣り合いを調べる。
;;;;
;;;; 文字列リテラル・文字リテラル・行コメント・ネストしたブロックコメントの
;;;; 中の括弧は数えない。素朴な括弧カウントでは "(((((" を含む正当なファイルを
;;;; 誤って弾くため、この区別に意味がある。
;;;;
;;;; src/validate.lisp から移設。validate は fs と tools/* に依存するツール層の
;;;; モジュールで、低レベルの src/cst.lisp から参照させたくないため、双方の共通
;;;; 土台としてここに置く。

(defpackage #:cl-mcp/src/utils/paren-scan
  (:use #:cl)
  (:export #:scan-parens))

(in-package #:cl-mcp/src/utils/paren-scan)

(defun scan-parens (text &key (base-offset 0))
  "Placeholder replaced in Step 3."
  (declare (ignore text base-offset))
  (list :ok t))
```

- [ ] **Step 3: 6 つの関数と 1 つの構造体を移す**

`src/validate.lisp` の次を `src/utils/paren-scan.lisp` へ移す。**中身は 1 文字も変えない。**
`%scan-parens` だけ名前を `scan-parens` にする（パッケージ外に出るため `%` を落とす）。

| 移すもの | 元の位置 |
|---|---|
| `%closing` | `src/validate.lisp:20` |
| `%scan-parens-push-open` | `:26` |
| `%scan-parens-pop-open` | `:29` |
| `scan-state` (defstruct) | `:56` |
| `%scan-handle-line-comment` | `:63` |
| `%scan-handle-string` | `:67` |
| `%scan-handle-block-comment` | `:76` |
| `%scan-handle-normal` | `:81` |
| `%scan-advance-position` | `:120` |
| `%scan-parens` → `scan-parens` | `:128` |

`lisp-edit-form` で 1 つずつ `insert_after` していく。Step 2 の雛形 `scan-parens` は
最後に `replace` で本体に置き換える。**`%scan-parens` 本体の `return-from %scan-parens` は
`return-from scan-parens` に直すこと**（3 箇所）。名前変更に伴う唯一の必然的な変更である。

- [ ] **Step 4: 括弧の釣り合いを確認する**

```bash
mallet src/utils/paren-scan.lisp
```

期待: エラーなし。

- [ ] **Step 5: `validate.lisp` を import に切り替える**

`src/validate.lisp` から移した 10 個の定義を削除し、`defpackage` に追加する:

```lisp
  (:import-from #:cl-mcp/src/utils/paren-scan
                #:scan-parens)
```

`%scan-parens` の呼び出し箇所を `scan-parens` に直す。呼び出し箇所は次で確認する:

```bash
grep -n "%scan-parens\|%closing\|scan-state\|%scan-handle\|%scan-advance" src/validate.lisp
```

期待: 置き換え後は `scan-parens` の呼び出しだけが残り、`%scan-` で始まる名前は 0 件。

- [ ] **Step 6: 全スイートで等価性を確認する**

```bash
mallet src/*.lisp src/utils/*.lisp && rove cl-mcp.asd 2>&1 | tail -5
```

期待: 終了コード 0、Step 1 と同じ結果。**1 件でも差があれば移動が挙動を変えている。**
その場合は先に進まず報告すること。

- [ ] **Step 7: コミット**

```bash
git add src/utils/paren-scan.lisp src/validate.lisp
git commit -F - <<'EOF'
refactor(validate): move the paren scanner down to src/utils

The scanner knows how to skip parens inside strings, character literals and
nested block comments -- knowledge src/cst.lisp needs too, and cannot reach
from where it sits: validate pulls in fs and tools/*, and a low-level parser
should not depend on a tool-layer module to count brackets.

Pure move, no behaviour change.  The only edit is the name: %scan-parens
becomes scan-parens now that it crosses a package boundary.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
```

---

### Task 3: 最大ネスト深さを追跡して返す

**Files:**
- Modify: `src/utils/paren-scan.lisp`
- Create: `tests/utils-paren-scan-test.lisp`
- Modify: `tests.lisp`

**Interfaces:**
- Consumes: Task 2 の `scan-parens`、Task 1 の実測値
- Produces: `scan-parens` の戻り plist に `:max-depth`（fixnum）が加わる。`*max-nesting-depth*` を export

- [ ] **Step 1: 失敗するテストを書く**

`fs-write-file` で `tests/utils-paren-scan-test.lisp` を作る:

```lisp
;;;; tests/utils-paren-scan-test.lisp

(defpackage #:cl-mcp/tests/utils-paren-scan-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/utils/paren-scan
                #:scan-parens
                #:*max-nesting-depth*))

(in-package #:cl-mcp/tests/utils-paren-scan-test)

(deftest scan-parens-reports-max-depth
  (testing "a flat form is depth 1"
    (ok (= 1 (getf (scan-parens "(a b c)") :max-depth))))
  (testing "nesting is counted, not paren count"
    (ok (= 3 (getf (scan-parens "(a (b (c)) d)") :max-depth))))
  (testing "the deepest branch wins, not the last one"
    (ok (= 4 (getf (scan-parens "(((( )))) (a)") :max-depth))))
  (testing "an empty string has depth 0"
    (ok (= 0 (getf (scan-parens "") :max-depth)))))

(deftest scan-parens-max-depth-ignores-non-code
  (testing "parens inside a string literal do not count"
    (ok (= 1 (getf (scan-parens "(f \"(((((\")") :max-depth))))
  (testing "parens inside a line comment do not count"
    (ok (= 1 (getf (scan-parens "(f) ; ((((((") :max-depth))))
  (testing "parens inside a block comment do not count"
    (ok (= 1 (getf (scan-parens "(f) #| (((( |#") :max-depth))))
  (testing "a character literal open paren does not count"
    (ok (= 1 (getf (scan-parens "(f #\\()") :max-depth)))))

(deftest max-nesting-depth-is-far-above-real-code
  (testing "the limit leaves ordinary source far below it"
    ;; src/proxy.lisp と src/pool.lisp が実測 20 で、このリポジトリの最大。
    (ok (> *max-nesting-depth* 200))))
```

- [ ] **Step 2: テストパッケージを登録する**

`tests.lisp` の `defpackage` に `lisp-edit-form` で追加する（既存の `:import-from` 行の並びに合わせる）:

```lisp
  (:import-from #:cl-mcp/tests/utils-paren-scan-test)
```

- [ ] **Step 3: 失敗を確認する**

```bash
rove tests/utils-paren-scan-test.lisp 2>&1 | tail -15
```

期待: FAIL。`*max-nesting-depth*` が存在せず、`:max-depth` が plist にないため。

- [ ] **Step 4: 深さ追跡を実装する**

`src/utils/paren-scan.lisp` を `lisp-edit-form` で 3 箇所直す。

まず `defpackage` の `:export` に `#:*max-nesting-depth*` を足す。次にパラメータを足す
（`<TASK1>` は Task 1 で決めた実測値に置き換える。**この文字列を残してはならない**）:

```lisp
(defparameter *max-nesting-depth* <TASK1>
  "1 つのフォームに許すネストの深さの上限。

これを超える入力は、リーダーに渡す前に拒否する。深いネストは Eclector CST 経路でも
標準 CL リーダー経路でも再帰で処理され、到達すれば SBCL の制御スタックを枯渇させる。
枯渇は捕捉に頼れない（src/macroexpand-core.lisp の *max-walk-expansions* に同じ実測が
記録されている）ので、届かせないことだけが効く。

この値は Task 1 で実測した破綻深度 <TASK1-BREAK> のはるか下、このリポジトリの実コードの
最大ネスト深さ 20（src/proxy.lisp、src/pool.lisp）のはるか上に置いてある。")
```

`scan-state` に 2 つスロットを足す:

```lisp
  (depth 0 :type fixnum)
  (max-depth 0 :type fixnum)
```

`%scan-handle-normal` の開き括弧の分岐で深さを進め、閉じ括弧の分岐で戻す。
**`length` で数えてはならない** — push ごとに O(n) になり、走査全体が O(n²) になる:

```lisp
   ((or (char= ch #\() (char= ch #\[) (char= ch #\{))
    (setf (scan-state-stack state)
            (%scan-parens-push-open (scan-state-stack state)
             (scan-state-line state) (scan-state-col state) base-offset ch
             idx))
    (incf (scan-state-depth state))
    (setf (scan-state-max-depth state)
            (max (scan-state-max-depth state) (scan-state-depth state)))
    (values nil nil))
   ((or (char= ch #\)) (char= ch #\]) (char= ch #\}))
    (multiple-value-bind (new-stack err)
        (%scan-parens-pop-open (scan-state-stack state) (scan-state-line state)
         (scan-state-col state) base-offset ch idx)
      (setf (scan-state-stack state) new-stack)
      ;; エラー時（extra-close）はスタックが縮んでいないので深さも戻さない。
      (unless err (decf (scan-state-depth state)))
      (values err nil)))
```

**`:max-depth` を足すと既存の呼び出し側が壊れる。** `src/validate.lisp:309` は

```lisp
(destructuring-bind (&key ok kind expected found
                          (offset base-off) (line 1) (column 1))
    paren-result
```

と受けており、`&key` は未知のキーでエラーになる。**同じタスクの中で `&allow-other-keys` を
足すこと**（`lisp-patch-form` で `(column 1))` を `(column 1) &allow-other-keys)` に）。
これを忘れると Step 6 の `rove tests/validate-test.lisp` が赤くなる。

最後に `scan-parens` の 4 つの `return-from` / 戻り値すべてに `:max-depth` を足す。
**早期 return する経路（extra-close、mismatch、unclosed、unclosed-block-comment）でも
そこまでに観測した最大深さを返す。** 呼び出し側が「釣り合いは崩れているが深すぎもする」を
判別できるようにするため:

```lisp
;; 例: 正常終了の枝
    (list :ok t :max-depth (scan-state-max-depth state))
```

早期 return の枝は `%scan-handle-normal` が組み立てた plist を返しているので、
`scan-parens` 側で受け取ってから `:max-depth` を付け足す形にする。

- [ ] **Step 5: テストが通ることを確認する**

```bash
rove tests/utils-paren-scan-test.lisp 2>&1 | tail -8
```

期待: 全て PASS。

- [ ] **Step 6: 既存テストの回帰を確認する**

```bash
mallet src/utils/paren-scan.lisp && rove tests/validate-test.lisp 2>&1 | tail -5
```

期待: 緑。`:max-depth` の追加は既存キーを壊していない。

**赤くなり `unknown &KEY argument :MAX-DEPTH` が出た場合**は、Step 4 の
`&allow-other-keys` を入れ忘れている。

- [ ] **Step 7: コミット**

```bash
git add src/utils/paren-scan.lisp tests/utils-paren-scan-test.lisp tests.lisp
git commit -F - <<'EOF'
feat(paren-scan): report the deepest nesting the scan saw

The scanner already walked a stack of open parens; what it never kept was
how deep that stack ever got.  Tracking it costs one counter -- measuring
with LENGTH on every push would have made the scan quadratic.

Depth is reported on the error paths too, so a caller can tell "unbalanced"
from "unbalanced and also far too deep".

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
```

---

### Task 4: `lisp-check-parens` が深すぎる入力を拒否する

**Files:**
- Modify: `src/validate.lisp`
- Create: `tests/deep-nesting-test.lisp`
- Modify: `tests.lisp`

**Interfaces:**
- Consumes: Task 3 の `scan-parens` の `:max-depth`、`*max-nesting-depth*`
- Produces: `lisp-check-parens` が `"kind": "too-deep"` を返す

- [ ] **Step 1: 失敗するテストを書く**

`fs-write-file` で `tests/deep-nesting-test.lisp` を作る:

```lisp
;;;; tests/deep-nesting-test.lisp
;;;;
;;;; 2026-08-01 監査の Critical の回帰テスト。
;;;; 深さ 20,000 のネストが接続スレッドを永久停止させた。

(defpackage #:cl-mcp/tests/deep-nesting-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/validate
                #:lisp-check-parens)
  (:import-from #:cl-mcp/src/utils/paren-scan
                #:*max-nesting-depth*))

(in-package #:cl-mcp/tests/deep-nesting-test)

(defun nested-source (depth)
  "DEPTH 重にネストした、構文的に正しいフォームの文字列を返す。"
  (concatenate 'string
               (make-string depth :initial-element #\()
               ":deep"
               (make-string depth :initial-element #\))))

(deftest check-parens-accepts-depth-at-the-limit
  (testing "exactly the limit is still accepted"
    (let ((res (lisp-check-parens :code (nested-source *max-nesting-depth*))))
      (ok (eq t (gethash "ok" res))
          "a form at the limit must not be rejected"))))

(deftest check-parens-rejects-depth-over-the-limit
  (testing "one past the limit is rejected as too-deep, not as a hang"
    (let ((res (lisp-check-parens :code (nested-source (1+ *max-nesting-depth*)))))
      (ok (null (gethash "ok" res)))
      (ok (string= "too-deep" (gethash "kind" res))))))

(deftest check-parens-survives-the-audit-reproduction
  (testing "depth 20000 returns an error instead of exhausting the stack"
    (let ((res (lisp-check-parens :code (nested-source 20000))))
      (ok (string= "too-deep" (gethash "kind" res))))))

(deftest check-parens-does-not-count-string-parens
  (testing "a long string literal of open parens is not too deep"
    (let* ((source (format nil "(f ~S)" (make-string 30000 :initial-element #\()))
           (res (lisp-check-parens :code source)))
      (ok (eq t (gethash "ok" res))
          "parens inside a string literal must not trip the depth limit"))))
```

**`lisp-check-parens` はハッシュ表を直接返す** — MCP の `result` ラッパではない
(`src/validate.lisp:306` の `(return-from lisp-check-parens h)`)。`tests/validate-test.lisp:13-17`
の `%ok?` / `%kind` / `%pos` も同じ前提で書かれている。ラッパを剥がすヘルパは要らない。

- [ ] **Step 2: テストパッケージを登録する**

`tests.lisp` に `lisp-edit-form` で追加する:

```lisp
  (:import-from #:cl-mcp/tests/deep-nesting-test)
```

- [ ] **Step 4: 失敗を確認する**

```bash
rove tests/deep-nesting-test.lisp 2>&1 | tail -20
```

期待: `check-parens-rejects-depth-over-the-limit` と
`check-parens-survives-the-audit-reproduction` が FAIL。
**`check-parens-survives-the-audit-reproduction` がプロセスごと落ちる、あるいは
返ってこない場合も「期待どおりの赤」である** — それがまさに直そうとしている症状だからである。
その場合はそのことを記録し、Step 5 に進む。

- [ ] **Step 5: `lisp-check-parens` に深さ検査を足す**

`src/validate.lisp:295` の `too-large` 検査の**直後**に、同じ形で深さ検査を足す。
`lisp-patch-form` を使う。位置が重要である: リーダーに渡す前に弾かなければ意味がない。

```lisp
    (let ((scan (scan-parens text :base-offset base-off)))
      (when (> (getf scan :max-depth 0) *max-nesting-depth*)
        (let ((h (make-hash-table :test #'equal)))
          (setf (gethash "ok" h) nil
                (gethash "kind" h) "too-deep"
                (gethash "expected" h) (format nil "~D" *max-nesting-depth*)
                (gethash "found" h) (format nil "~D" (getf scan :max-depth 0)))
          (let ((pos (make-hash-table :test #'equal)))
            (setf (gethash "offset" pos) base-off
                  (gethash "line" pos) 1
                  (gethash "column" pos) 1)
            (setf (gethash "position" h) pos))
          (setf (gethash "message" h)
                  (format nil "Nesting is ~D levels deep, over the limit of ~D. ~
                               Deeply nested forms exhaust the control stack, and that ~
                               exhaustion cannot be caught, so they are rejected before ~
                               the reader sees them."
                          (getf scan :max-depth 0) *max-nesting-depth*))
          (return-from lisp-check-parens h))))
```

**`too-large` の枝（`src/validate.lisp:295-306`）と同じ形にすること。** そこは
`(return-from lisp-check-parens h)` でハッシュ表を直接返しており、MCP のラッパは通していない。
この計画のコードと既存の枝が食い違ったら、既存の枝が正しい。

`text` を得た直後、**`%try-reader-check` に渡る前**に置くこと。リーダーに渡した後では意味がない。

`defpackage` に import を足す:

```lisp
  (:import-from #:cl-mcp/src/utils/paren-scan
                #:scan-parens
                #:*max-nesting-depth*)
```

- [ ] **Step 6: テストが通ることを確認する**

```bash
mallet src/validate.lisp && rove tests/deep-nesting-test.lisp 2>&1 | tail -10
```

期待: 4 テスト全て PASS。特に `check-parens-survives-the-audit-reproduction` がハングせず終わること。

- [ ] **Step 7: コミット**

```bash
git add src/validate.lisp tests/deep-nesting-test.lisp tests.lisp
git commit -F - <<'EOF'
fix(check-parens): reject nesting too deep for the reader to survive

Depth 20,000 of syntactically valid parens exhausted the control stack and
parked the connection thread in the debugger forever -- on the very tool the
docs recommend as the safe way to diagnose syntax problems.

The check runs before the reader is handed the text, because exhaustion
cannot be caught once it happens.  It reports the limit and the depth found,
so the caller learns why rather than losing the connection.

Parens inside string literals still do not count: the scan that decides this
is the one that already knew the difference.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
```

---

### Task 5: CST 経路（`lisp-read-file` / `lisp-edit-form` / `lisp-patch-form`）を守る

**Files:**
- Modify: `src/cst.lisp`
- Modify: `tests/deep-nesting-test.lisp`

**Interfaces:**
- Consumes: Task 3 の `scan-parens`、`*max-nesting-depth*`
- Produces: `parse-top-level-forms` が深すぎる入力で `nesting-too-deep` を signal する

- [ ] **Step 1: 失敗するテストを追加する**

`tests/deep-nesting-test.lisp` に `lisp-edit-form` で追加する。`defpackage` に import を足す:

```lisp
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:nesting-too-deep)
```

テスト本体:

```lisp
(deftest cst-rejects-depth-over-the-limit
  (testing "the CST path signals instead of exhausting the stack"
    (ok (handler-case
            (progn (parse-top-level-forms (nested-source 20000)) nil)
          (nesting-too-deep () t))
        "depth 20000 must signal nesting-too-deep")))

(deftest cst-accepts-real-source
  (testing "the deepest file in this repo still parses"
    ;; src/proxy.lisp は実測でネスト深さ 20、このリポジトリの最大。
    (let ((text (uiop:read-file-string
                 (asdf:system-relative-pathname :cl-mcp "src/proxy.lisp"))))
      (ok (parse-top-level-forms text)
          "ordinary source must be unaffected by the depth limit"))))
```

`handler-case` を使うのは、Rove の `signals` が `restart-case` の内側で確実に働かないため
（CLAUDE.md の Rove testing pitfalls）。

- [ ] **Step 2: 失敗を確認する**

```bash
rove tests/deep-nesting-test.lisp 2>&1 | tail -15
```

期待: `cst-rejects-depth-over-the-limit` が FAIL（`nesting-too-deep` が未定義）。
Task 4 Step 4 と同様、プロセスごと落ちる場合も期待どおりの赤である。

- [ ] **Step 3: 条件と検査を実装する**

`src/cst.lisp` に `lisp-edit-form` で追加する。まず `defpackage`:

```lisp
  (:import-from #:cl-mcp/src/utils/paren-scan
                #:scan-parens
                #:*max-nesting-depth*)
```

`:export` に `#:nesting-too-deep` と `#:nesting-too-deep-depth` を足す。

条件を `parse-top-level-forms` の直前に置く:

```lisp
(define-condition nesting-too-deep (error)
  ((depth :initarg :depth :reader nesting-too-deep-depth)
   (limit :initarg :limit :reader nesting-too-deep-limit))
  (:report
   (lambda (condition stream)
     (format stream
             "Nesting is ~D levels deep, over the limit of ~D. ~
              Deeply nested forms exhaust the control stack while being read, ~
              and that exhaustion cannot be caught, so the form is rejected ~
              before the reader sees it."
             (nesting-too-deep-depth condition)
             (nesting-too-deep-limit condition)))))
```

`parse-top-level-forms` の本体の**先頭**（`let` に入る前）で検査する。CST 経路の唯一の入口なので、
ここ 1 箇所で `lisp-read-file` / `lisp-edit-form` / `lisp-patch-form` の 3 ツールが守られる:

```lisp
  (let ((depth (getf (scan-parens text) :max-depth 0)))
    (when (> depth *max-nesting-depth*)
      (error 'nesting-too-deep :depth depth :limit *max-nesting-depth*)))
```

- [ ] **Step 4: テストが通ることを確認する**

```bash
mallet src/cst.lisp && rove tests/deep-nesting-test.lisp 2>&1 | tail -10
```

期待: 6 テスト全て PASS。`cst-accepts-real-source` が通ることが実コードの回帰防止になる。

- [ ] **Step 5: 3 ツールが実際に守られていることを確認する**

条件が入口 1 箇所にあることの確認を、ツール経由で実際に行う。`repl-eval` で:

```lisp
(dolist (thunk (list (lambda () (cl-mcp:lisp-read-file "/tmp/deep-probe.lisp"))))
  (format t "~&~A~%" (handler-case (progn (funcall thunk) "NO ERROR — GUARD MISSING")
                       (error (e) (format nil "guarded: ~A" (type-of e))))))
```

事前に深さ 20,000 のファイルを書いておくこと。`lisp-edit-form` / `lisp-patch-form` も
同じファイルに対して同様に確認する。**3 ツールとも `nesting-too-deep` になること。**

期待どおりでないツールがあれば、そのツールが `parse-top-level-forms` を経由していないという
ことなので、報告して止まること。

- [ ] **Step 6: コミット**

```bash
git add src/cst.lisp tests/deep-nesting-test.lisp
git commit -F - <<'EOF'
fix(cst): reject too-deep forms before Eclector recurses into them

parse-top-level-forms is the single door into the CST path, so one check
here covers lisp-read-file, lisp-edit-form and lisp-patch-form.  Two of
those were never probed during the audit -- the tester stopped to avoid
leaking more stuck threads -- and they were exposed the whole time.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
```

---

### Task 6: 接続スレッドをデバッガに入れない

**Critical の直接原因はこちらである。** 深さ上限はこの経路を塞ぐが、親側ツールで未処理の
シリアス condition が起きればどれでも同じ永久停止が起きる。

**Files:**
- Modify: `src/tcp.lisp`, `src/http.lisp`, `src/run.lisp`
- Modify: `tests/deep-nesting-test.lisp`

**Interfaces:**
- Produces: `cl-mcp/src/log` を使うヘルパ。3 経路が同じ形で束縛する

- [ ] **Step 1: 失敗するテストを書く**

`tests/deep-nesting-test.lisp` に `lisp-edit-form` で追加する。import を足す:

```lisp
  (:import-from #:cl-mcp/src/tcp
                #:serve-tcp)
```

```lisp
(deftest serious-condition-does-not-park-the-connection-thread
  (testing "a serious condition closes one connection and leaves the server serving"
    (let* ((port nil)
           (ready (bordeaux-threads:make-semaphore))
           (server (bordeaux-threads:make-thread
                    (lambda ()
                      (serve-tcp :host "127.0.0.1" :port 0 :accept-once nil
                                 :on-listening
                                 (lambda (p)
                                   (setf port p)
                                   (bordeaux-threads:signal-semaphore ready)))))))
      (unwind-protect
           (progn
             (ok (bordeaux-threads:wait-on-semaphore ready :timeout 10)
                 "server must come up")
             ;; 壊れた JSON-RPC を送りつけた後、別接続が生きていることを確かめる。
             ;; 目的はサーバが応答し続けることであって、この行が何を返すかではない。
             (let ((socket (usocket:socket-connect "127.0.0.1" port)))
               (unwind-protect
                    (progn
                      (format (usocket:socket-stream socket) "~A~%" "{")
                      (force-output (usocket:socket-stream socket)))
                 (ignore-errors (usocket:socket-close socket))))
             (let ((socket (usocket:socket-connect "127.0.0.1" port
                                                   :timeout 10)))
               (unwind-protect
                    (ok socket "the server must still accept a new connection")
                 (ignore-errors (usocket:socket-close socket)))))
        (ignore-errors (bordeaux-threads:destroy-thread server))))))
```

**このテストだけでは Step 3 の実装の質を判定できない。** 実際にデバッガに入る condition を
起こす検証は Step 4 で行う。

- [ ] **Step 2: 失敗を確認する**

```bash
rove tests/deep-nesting-test.lisp 2>&1 | tail -10
```

期待: PASS するかもしれない（壊れた JSON はデバッガまで行かず既存の `handler-case` が捕まえる）。
**PASS してもよい** — これは回帰の網であって、Step 3 の駆動テストではない。
Step 4 が本当の検証である。

- [ ] **Step 3: デバッガフックのヘルパを書いて 3 経路で束縛する**

`src/log.lisp` に依存するヘルパを作るのではなく、**各ファイルに同じ形で書く**のは重複になる。
`src/utils/` に置く:

`fs-write-file` で `src/utils/serving.lisp`:

```lisp
;;;; src/utils/serving.lisp
;;;;
;;;; サーバが要求を処理しているスレッドをデバッガに入れないための土台。
;;;;
;;;; 親プロセスはワーカーと違い (sb-ext:disable-debugger) を呼べない。
;;;; cl-mcp:run は本番の入口であると同時にテストが呼ぶ関数でもあり、
;;;; グローバルに無効化するとテストと REPL のデバッガ体験まで壊れる。
;;;; 危険なのは「サーバが処理する要求の中でデバッガに入ること」だけなので、
;;;; 束縛をその範囲に閉じる。

(defpackage #:cl-mcp/src/utils/serving
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:export #:call-without-debugger))

(in-package #:cl-mcp/src/utils/serving)

(defun call-without-debugger (label thunk)
  "THUNK を呼ぶ。デバッガに入る事態になったらログを出して THUNK から脱出する。

デバッガに入ったスレッドはフォアグラウンドを永久に待つ。サーバのスレッドでは
それは応答しない接続とスレッドリークを意味するので、代わりにここまで巻き戻す。

**脱出が要である。** *invoke-debugger-hook* に束縛した関数が非局所脱出せずに
戻ると、戻った先で結局デバッガに入る。CATCH がその脱出先である。"
  (let ((tag (gensym "SERVING")))
    (catch tag
      (let ((sb-ext:*invoke-debugger-hook*
              (lambda (condition hook)
                (declare (ignore hook))
                (log-event :error "serving.debugger-suppressed"
                           "label" label
                           "condition_type" (string (type-of condition))
                           "message" (ignore-errors
                                      (princ-to-string condition)))
                (throw tag :debugger-suppressed))))
        (funcall thunk)))))
```

`src/tcp.lisp` の `%tcp-handle-client`（`:233`）の `%process-stream` 呼び出しを包む。
`src/http.lisp` の `acceptor-dispatch-request`（`:448`）の本体を包む。
`src/run.lisp` の `:stdio` 分岐のループを包む。それぞれ `defpackage` に import を足す:

```lisp
  (:import-from #:cl-mcp/src/utils/serving
                #:call-without-debugger)
```

- [ ] **Step 4: 脱出が実際に効くことを確かめる**

**「ログを出して return するだけ」の実装は症状を変えずログだけ増やす。** それを検出する:

```bash
env -u ASDF_OUTPUT_TRANSLATIONS timeout 60 ros -Q \
  -e '(asdf:load-system :cl-mcp)' \
  -e '(format t "~&RESULT=~S~%"
        (cl-mcp/src/utils/serving:call-without-debugger
          "probe" (lambda () (error "boom") :never-reached)))' 2>&1 | tail -5
```

期待: `RESULT=:DEBUGGER-SUPPRESSED` が印字され、プロセスが 60 秒以内に終了する。

- `RESULT=:NEVER-REACHED` なら脱出していない（フックが値を返して処理が続いた）
- タイムアウトするならデバッガに入っている

どちらの場合も実装が誤っているので、直してから進むこと。

- [ ] **Step 5: 3 経路すべてで束縛されていることを確認する**

```bash
grep -n "call-without-debugger" src/tcp.lisp src/http.lisp src/run.lisp
```

期待: 3 ファイルすべてに 1 件以上。**1 経路だけ直すと、そこだけ安全という誤解を生む。**

- [ ] **Step 6: 全スイートで回帰を確認する**

```bash
mallet src/*.lisp src/utils/*.lisp && rove cl-mcp.asd 2>&1 | tail -5
```

期待: 終了コード 0。**テストは `run` と `serve-tcp` を呼ぶので、束縛がテストの
デバッガ体験を壊していないことがここで分かる。**

- [ ] **Step 7: コミット**

```bash
git add src/utils/serving.lisp src/tcp.lisp src/http.lisp src/run.lisp tests/deep-nesting-test.lisp
git commit -F - <<'EOF'
fix: stop a served request from parking its thread in the debugger

A thread that enters the debugger waits for a foreground it will never be
given, so the client never hears back and the thread never returns -- the
audit found two still alive nine minutes on.  The server keeps accepting
new connections, which is what makes it easy to miss.

Binding the hook per connection rather than disabling the debugger globally:
cl-mcp:run is both the production entry point and what the tests call, and
a served request reaching the debugger is always wrong while a REPL doing so
is right.  The hook throws rather than returns -- returning would land the
thread in the debugger anyway, one stack frame later.

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
EOF
```

---

### Task 7: 監査の再現手順で仕上げを確認する

**Files:** なし（検証のみ）

- [ ] **Step 1: 監査の再現手順をそのまま実行する**

`docs/plans/2026-08-01-audit-edge-cases.md` 所見1 の Python スクリプトを、別プロセスで
立てた cl-mcp に対して実行する。起動は:

```bash
env -u ASDF_OUTPUT_TRANSLATIONS ros -Q \
  -e '(asdf:load-system :cl-mcp)' \
  -e '(cl-mcp:run :transport :tcp :port 18001 :accept-once nil)' \
  > /tmp/cl-mcp-verify.log 2>&1 &
disown
echo "PID=$!"
```

ポートは 18001 を使う（18000 は監査で使用済み）。

- [ ] **Step 2: 深さ 20,000 が 4 ツールすべてでエラーを返すことを確認する**

再現スクリプトの最後の `tools/call` を `lisp-read-file` / `lisp-check-parens` /
`lisp-edit-form` / `lisp-patch-form` に差し替えて、それぞれ実行する。

期待: **4 ツールすべてが 30 秒以内にエラー応答を返す。** ハングは 1 件もないこと。

- [ ] **Step 3: サーバが生きていることを確認する**

4 回のプローブの後、新規接続で `initialize` が通ることを確認する。

期待: 正常応答。スレッドリークがないことは、プローブ前後で応答性が変わらないことで見る。

- [ ] **Step 4: 後片付けと全スイート**

```bash
kill <PID>; ps -p <PID> >/dev/null && kill -9 <PID>
mallet src/*.lisp src/utils/*.lisp && rove cl-mcp.asd 2>&1 | tail -5
```

期待: プロセスが**SIGTERM で**終了すること（監査時のインスタンスは SIGKILL を要した）。
全スイート緑。

- [ ] **Step 5: 完了条件を照合する**

設計仕様 §9 の 7 項目を 1 つずつ確認し、満たしていない項目があれば明記する。
コミットするものはない。

---

## 完了条件（設計仕様 §9）

1. [ ] 破綻深度が隔離プロセスで実測され、上限値がその実測に基づいて決まっている（Task 1）
2. [ ] `scan-parens` が `src/utils/` に移り、`validate` と `cst` の双方から使われている（Task 2, 4, 5）
3. [ ] 深さ 20,000 が 4 ツールすべてでハングせずエラーを返す（Task 5 Step 5, Task 7 Step 2）
4. [ ] 接続スレッドでシリアス condition が起きてもデバッガに入らず、サーバが他の接続を処理し続ける（Task 6）
5. [ ] 仕様 §6 のテスト 5 件が揃っている（Task 3, 4, 5, 6）
6. [ ] 全スイートが緑（Task 6 Step 6, Task 7 Step 4）
7. [ ] `mallet src/*.lisp` が通る（各タスク）
