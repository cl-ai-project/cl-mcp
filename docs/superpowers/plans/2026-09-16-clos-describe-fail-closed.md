# clos-describe fail-closed 対応付けと編集ガード Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A) `clos-describe` は、現在のソースで同じ定義だと確認できたときだけ編集用の `form_type` /
`form_name` を返す（三値判定・構造化識別子・EQL の許可リスト・コンテナ検証・編集側との往復確認）。
B) その編集情報にファイルと対象フォームを固定する `edit_guard` を付け、`lisp-edit-form` が
書き込み前に検証して、観察後に変わっていたら失敗する。

**Architecture:** worker は実行時の識別情報（パッケージ名 + symbol-name + EQL datum のタグ）を返し、
親は CST からソース側のトークンと範囲を取り出す。解決と同一性判定は worker の `find-symbol`（intern 禁止）
で行い、親はその判定結果に従って編集情報とガードを付ける。責務分離（実行時 = worker、CST = 親）は維持。

**Tech Stack:** SBCL（sb-mop, sb-introspect, sb-md5）, eclector（親のみ）, yason, Rove

**Spec:** `docs/superpowers/specs/2026-09-16-clos-describe-fail-closed-design.md`（先に読むこと）

**着手時 HEAD:** `264dd82`。作業ツリーは clean（`coverage/` と `src/specs/` は無関係な untracked）。

## Global Constraints

- SBCL 専用。Google CL Style Guide（2 スペース、≤100 桁、トップレベルフォーム間に空行、公開関数に docstring）
- mallet は `::` を禁止（src も tests も）。SBCL 内部関数は `find-symbol` 経由。テストからの sb-* 呼び出しは `uiop:symbol-call`
- **fail-closed**: 例外・欠落・印字失敗・未対応構文・解決不能はすべて `unverified`。`matched` にフォールバックしない
- **本番コードで `intern` / `eval` / `macroexpand` / initfunction 呼び出し / `#.` 実行をしない**。シンボル解決は `find-package` + `find-symbol` のみ
- **同一性判定に表示文字列を使わない**: パッケージ接頭辞の除去、`string-equal`、`equalp`、`string-downcase` を使わない（`string=` による厳密比較と `eq` を使う）
- worker は eclector に依存しない。`src/clos-core.lisp` と新しい worker 側コアは `cst` / `code-refs-scan` / `lisp-edit-form-core` / `tools/clos-response-builders` を import しない
- JSON 配列は組み立て時 vector、真偽値は `json-bool`。読む側は `sequence->list` と既存の `%true-p` を通し、worker 経由（list / vector、NIL / `yason:false`）の両方で同じ結果にする
- 数値は JSON の浮動小数点にしない（大整数・ratio は 10 進文字列で運ぶ）
- Rove で condition を検査するときは `handler-case`
- package-inferred-system: `cl-mcp.asd` は編集しない。新しい src は `src/tools/all.lisp` と `main.lisp`、新しいテストはルートの `tests.lisp` に登録
- 先行コミット `34f3ea1`（`%read-form-starts` の読み取りポリシー）を保持する。アクセス方針の変更やヒープ走査の性能改善を混ぜない
- A は B が無くても単独で誤対応を減らす修正として成立させる。B の都合で A の fail-closed を弱めない
- コミットは変更ファイルをパス指定。`coverage/` と `src/specs/` は絶対に add しない。**push と PR 作成はしない**
- コミットメッセージ末尾:
  ```
  Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01FwJGaMhQXB93nGDWsR7eSS
  ```

### テストの実行方法（全タスク共通）

```bash
ros run -e '(ql:quickload :rove :silent t)' \
        -e '(asdf:load-asd (truename "cl-mcp.asd"))' \
        -e '(handler-bind ((warning (function muffle-warning))) (asdf:load-system "cl-mcp/tests/<name>"))' \
        -e '(uiop:quit (if (rove:run :cl-mcp/tests/<name>) 0 1))' 2>&1 | tail -40
```

判定は終了コードではなく `✗` / `×` の有無。`rove cl-mcp.asd`（全スイート）は**バックグラウンドで実行しない**
（この環境のメモリ監視に kill される）。全スイートはコントローラが最後にフォアグラウンドで回す。

### Lint（コミット前に必ず）

```bash
mallet src/*.lisp src/*/*.lisp tests/*.lisp
```

---

## File Structure

| ファイル | 側 | 変更 | 責務 |
|---|---|---|---|
| `src/clos-core.lisp` | worker | 変更 (A1) | 各要素に `identity` を足す |
| `src/code-refs-scan.lisp` | 親 | 変更 (A2) | ソース側の構造化署名（トークン + 範囲）、行で始まる全フォーム |
| `src/clos-verify-core.lisp` | worker | 新規 (A3) | トークン解決（intern 禁止）と三値判定 |
| `src/worker/handlers.lisp` | worker | 変更 (A3) | `worker/clos-verify-source` |
| `src/lisp-edit-form-core.lisp` | 親 | 変更 (A4, B2) | `locate-form-in-nodes` の切り出し、ガード検証 |
| `src/tools/clos-response-builders.lisp` | 親 | 変更 (A4, B3) | 三値の注釈、往復確認、テキスト、`edit_guard` |
| `src/clos.lisp` | 親 | 変更 (A4) | 2 段の RPC |
| `src/source-snapshot.lisp` | 親 | 新規 (B1) | 1 回読みのスナップショットと md5 |
| `src/lisp-edit-form.lisp` | 親 | 変更 (B2) | `guard` 引数 |
| `tests/clos-core-test.lisp` / `tests/code-refs-scan-test.lisp` / `tests/clos-verify-core-test.lisp`(新) / `tests/clos-response-builders-test.lisp` / `tests/lisp-edit-form-test.lisp` / `tests/source-snapshot-test.lisp`(新) / `tests/clos-describe-integration-test.lisp`(新) | - | | テスト |
| `tests/fixtures/clos-identity-fixture.lisp`(新) | - | | 別パッケージ同名・escaped symbol・EQL 各種・defgeneric 内メソッド・アクセサ |
| `docs/tools.md` / `prompts/repl-driven-development.md` / `CLAUDE.md` | - | | 文書 |

---

## A. ソース対応付けの厳密化

### Task 1: (A1) worker が構造化識別子を返す

**Files:** Modify `src/clos-core.lisp`, `tests/clos-core-test.lisp`; Create `tests/fixtures/clos-identity-fixture.lisp`

**Interfaces (produces):** 各 `methods` 要素・`generic_functions` 要素・`class` に `"identity"`（hash-table）。
spec §3.2 の形。加えて EQL datum は spec §3.3 のタグ付き（worker 側は実行時オブジェクトから作る）:
`{"kind":"integer","value":"3"}` / `{"kind":"ratio","numerator":"1","denominator":"3"}` /
`{"kind":"character","value":"A"}` / `{"kind":"keyword","name":"UNIT"}` / `{"kind":"boolean","value":"T"}` /
`{"kind":"symbol","package":"PKG-A","name":"FOO"}` / `{"kind":"unverifiable","reason":"<1 文>"}`
（float・complex・文字列・リスト・配列・uninterned symbol はすべて `unverifiable`）。

- [ ] **Step 1: フィクスチャを作る** — `tests/fixtures/clos-identity-fixture.lisp`。2 つのパッケージ
  （`cl-mcp-identity-a` / `cl-mcp-identity-b`）に同名クラス `probe` と同名総称関数 `act` を定義。
  `|Foo|` と `|FOO|` の 2 クラス。EQL メソッド: `(eql :unit)`, `(eql 3)`, `(eql 1/3)`, `(eql #\A)`,
  `(eql #\B)`, `(eql t)`, `(eql nil)`, `(eql 'sym)`, `(eql "str")`, `(eql *probe-var*)`（`defparameter` で値 7）。
  `(setf act)` の総称関数。`defgeneric` 内 `(:method ...)`。`defclass` のアクセサ。
  日本語コメントを 1 行入れる。クラスを `make-instance` しない。
- [ ] **Step 2: 失敗するテストを書く** — `tests/clos-core-test.lisp` に `identity` の内容を検証する
  deftest 群（総称関数名のパッケージ・`setf` 区別、修飾子、クラス特化子のパッケージ、EQL の各 kind、
  `unverifiable` の理由、アクセサの `slot` / `access`、クラスの `class`）。実行して RED を確認。
- [ ] **Step 3: 実装** — `%identity-symbol (symbol)` → `{"package": <package-name or null>, "name": <symbol-name>}`
  （uninterned は package null）。`%identity-function-name (name)` → `{"package","name","setf"}`。
  `%eql-datum-identity (object)` → 上のタグ付き。`%method-entry` / `%generic-function-entry` /
  `%class-entry` に `"identity"` を追加（表示用フィールドは現状のまま残す）。
- [ ] **Step 4: GREEN を確認**（`clos-core-test`）、mallet、コミット
  `feat(clos-core): report structured identity for methods, classes and generic functions`

### Task 2: (A2) 親がソース側の構造化署名と範囲を作る

**Files:** Modify `src/code-refs-scan.lisp`, `tests/code-refs-scan-test.lisp`

**Interfaces (produces):**
`top-level-forms-at (abs-path lines &key text)` の戻り値を変える:
`(values TABLE FAILURE)`、TABLE は 行 → **リスト**（その行で始まるフォームすべて）で、各要素は
plist `(:form-type F :form-name N :signature S :start I :end J)`（`start`/`end` は文字オフセット、
`end` は exclusive）。`SIGNATURE` は spec §3.2 の `source_signature` の Lisp 表現:

```lisp
(:kind :defmethod        ; :defmethod | :defgeneric | :defclass | :define-condition | :defstruct | :other
 :head (:token "defmethod" :in-package "PKG-A")
 :name (:token "area" :setf nil :in-package "PKG-A")
 :qualifiers ((:token ":around" :in-package "PKG-A") ...)
 :specializers ((:kind :class :token "pkg-a:circle" :in-package "PKG-A")
                (:kind :eql :datum (:kind :keyword :name "UNIT"))
                (:kind :unverifiable :reason "...")
                ...)
 :methods (...)          ; :defgeneric のとき (:method ...) 各要素の :qualifiers/:specializers
 :slots (...))           ; :defclass / :define-condition のとき (:name トークン :readers (...) :writers (...))
```

`:token` は CST ノードの範囲から取った**ソースの文字列そのもの**、`:in-package` はその位置で有効な
`in-package` のパッケージ名（無ければ null）。EQL datum は spec §3.3 の許可リストだけをタグ付けし、
それ以外は `(:kind :unverifiable :reason ...)`。`'x` は先頭文字が `'` であることで、`(quote x)` は
head トークンを `:token` として渡して worker に確認させる（親は解決しない）。

- [ ] **Step 1: 失敗するテストを書く** — `tests/code-refs-scan-test.lisp`:
  同一行に 2 フォーム → 2 要素返る。`(defmethod pkg-a:area :around ((s pkg-a:circle) (n (eql 3/4))))`
  のトークンがソースのまま（`pkg-a:circle`）返る。`(eql "s")` / `(eql *v*)` / `(eql (f))` /
  `(eql #\A)` / `(eql 'foo)` / `(eql :k)` / `(eql t)` の datum タグ。`defgeneric` の `(:method ...)`。
  `defclass` のスロット `:reader` / `:writer` / `:accessor`。`in-package` 切り替え後の `:in-package`。
  範囲 `:start` / `:end` が `subseq` でそのフォームの文字列になること。RED を確認。
- [ ] **Step 2: 実装** — `%definition-signature` を CST ノード + 本文を受け取る
  `%definition-source-signature (node text in-package)` に置き換える。`%expr-children` と
  `%unwrap` を使い、トークン文字列は `(subseq text (cst-node-start c) (cst-node-end c))`。
  `top-level-forms-at` は行ごとに**リストを蓄積**し、`:text` が渡されたらそれを使う（B1 のスナップショット用）。
- [ ] **Step 3: 呼び出し側の暫定追従** — `src/tools/clos-response-builders.lisp` の
  `annotate-report-forms` は、この時点では「その行のフォームがちょうど 1 つで、
  従来の判定（`%form-describes-entry-p`）を通ったときだけ従来どおり」に直す（A4 で置き換える）。
  既存のテストが壊れないことを確認する。
- [ ] **Step 4: GREEN**（`code-refs-scan-test`, `clos-response-builders-test`）、mallet、コミット
  `feat(code-refs-scan): describe source definitions by their tokens and spans`

### Task 3: (A3) worker 側の解決と三値判定

**Files:** Create `src/clos-verify-core.lisp`, `tests/clos-verify-core-test.lisp`; Modify `src/worker/handlers.lisp`, `tests/worker-test.lisp`, `tests.lisp`

**Interfaces (produces):**
`cl-mcp/src/clos-verify-core:verify-entries (entries)` — ENTRIES は JSON 由来の配列で、各要素
`{"id": <string>, "identity": <A1 の identity>, "candidates": [<A2 の signature を JSON にしたもの>...]}`。
戻り値は `{"results": [{"id", "status": "matched"|"mismatched"|"unverified", "reason": <string or null>,
"candidate_index": <integer or null>}...]}`。判定は spec §3.1〜§3.4。
`candidate_index` は `matched` のときだけ、その候補の位置。
worker メソッド `worker/clos-verify-source`（params: `{"entries": [...]}`）。

判定規則の要点（fail-closed）:
- head トークンを解決して `CL:DEFMETHOD` / `CL:DEFGENERIC` / `CL:DEFCLASS` / `CL:DEFINE-CONDITION` /
  `CL:DEFSTRUCT` と `eq` でなければ `unverified`（理由: unsupported or shadowed definition form）
- 名前・修飾子・クラス特化子はトークンを `parse-symbol-text` + `find-package` + `find-symbol` で解決し、
  identity の `(find-symbol name package)` と `eq` かで比較。解決できなければ `unverified`
- EQL は spec §3.3 の表のとおり。`unverifiable` タグが片側にでもあれば `unverified`
- `defgeneric` の候補は、内包メソッドのうち identity と一致するものがちょうど 1 つなら `matched`
- アクセサはクラス名・スロット名・reader/writer 種別・総称関数名がすべて一致したら `matched`
- 候補が複数あり `matched` が 2 つ以上なら `unverified`（理由: ambiguous）、
  `matched` 0 かつ `mismatched` ≥ 1 なら `mismatched`、それ以外は `unverified`

- [ ] **Step 1: 失敗するテストを書く** — `tests/clos-verify-core-test.lisp`（新規、`tests.lisp` に登録）。
  spec §5 の表のうち解決に関わるものを、フィクスチャ（A1）をロードした上で検証する:
  別パッケージ同名 → `mismatched`、keyword と非 keyword、escaped の大小、`(eql 'foo)` と `(eql *v*)`、
  EQL 各 kind、shadow された `defmethod` 名、`defgeneric` 内メソッド、アクセサ、
  副作用カウンタ付き `(eql (incf *n*))` が判定で増えないこと、intern しないこと
  （`find-symbol` が前後で変わらない）。RED を確認。
- [ ] **Step 2: 実装** — `src/clos-verify-core.lisp`（worker 側。import は `code-refs-core`
  （`parse-symbol-text`, `sequence->list`）, `tools/helpers`, `log` まで。eclector 系は import しない）。
- [ ] **Step 3: worker ハンドラ** — `%handle-clos-verify-source` を追加し `register-all-handlers` に登録。
  `tests/worker-test.lisp` に往復テスト（JSON 経由で status が返る）。
- [ ] **Step 4: GREEN**（`clos-verify-core-test`, `worker-test`）、mallet、コミット
  `feat(clos-verify): resolve source tokens in the worker and judge three ways`

### Task 4: (A4) 親の 2 段 RPC・三値の注釈・編集側との往復確認

**Files:** Modify `src/lisp-edit-form-core.lisp`, `src/tools/clos-response-builders.lisp`, `src/clos.lisp`, `src/proxy.lisp`（必要なら）, `tests/lisp-edit-form-test.lisp`, `tests/clos-response-builders-test.lisp`, `tests/tools-test.lisp`

**Interfaces (produces):**
- `lisp-edit-form-core:locate-form-in-nodes (nodes form-type form-name)` →
  `(values NODE ERROR-STRING)`。`%find-target` の中身を切り出し、`%find-target` はこれを呼ぶだけにする。
  観察側はこれを使って往復確認する（`lisp-edit-form` ツールを `dry_run` で呼ばない）。
- 各要素の JSON: `source_match`（`matched`/`mismatched`/`unverified`）、`source_match_reason`、
  `edit_unit`（`defgeneric` / `defclass` などコンテナを編集する場合のみ）。
  `form_type` / `form_name` は `matched` かつ往復確認 OK のときだけ。
- `clos-describe` の流れ: `worker/clos-describe` → 親のスキャン → `worker/clos-verify-source` →
  注釈 → テキスト。pool 無効時は同じ 2 段を同じプロセスで。

- [ ] **Step 1: 失敗するテストを書く** — `clos-response-builders-test`:
  三値がそのまま JSON とテキストに出ること、`matched` 以外で `form_type`/`form_name` が**無い**こと、
  テキストが編集を促さないこと、往復確認に失敗した `matched` が `unverified` に落ちること
  （同名メソッドが 2 つある `Multiple matches` を作る）。`lisp-edit-form-test`:
  `locate-form-in-nodes` の単体テスト（一意解決、複数一致、不在）。RED を確認。
- [ ] **Step 2: 実装（編集側の切り出し）** — `locate-form-in-nodes` を export し、`%find-target` を委譲に。
- [ ] **Step 3: 実装（注釈）** — `annotate-report-forms` を三値に置き換える。
  ファイルごとに 1 回スキャン → 候補（A2）→ `worker/clos-verify-source` に渡す形へ変換 →
  返った status を要素に反映 → `matched` は `locate-form-in-nodes` で往復確認して初めて
  `form_type` / `form_name` を入れる。`%form-describes-entry-p` と
  `%same-name-p` / `%same-specializers-p` / `%eql-*` は削除する（表示用の短縮 `%short` は残す）。
- [ ] **Step 4: 実装（ツール）** — `src/clos.lisp` を 2 段に。pool 経由は `proxy-to-worker` を 2 回呼ぶ。
  検証 RPC がエラー／クラッシュ通知を返したら、全要素を `unverified`（理由: verification unavailable）にして
  報告は返す（ツール全体は失敗させない）。
- [ ] **Step 5: GREEN**（`lisp-edit-form-test`, `lisp-patch-form-test`, `lisp-macroexpand-test`,
  `clos-response-builders-test`, `tools-test`）、mallet、コミット（2 つに分けてよい）
  `refactor(lisp-edit-form): share the form locator with the observer`
  `feat(clos-describe): return edit information only for verified definitions`

### Task 5: (A5) 実イメージでの統合テストと A の文書

**Files:** Create `tests/clos-describe-integration-test.lisp`; Modify `tests.lisp`, `docs/tools.md`, `prompts/repl-driven-development.md`, `CLAUDE.md`

- [ ] **Step 1: 統合テストを書く** — 一時ファイルに定義を書き、`compile-file` + `load`（実 SBCL イメージ）、
  その後ファイルを書き換えて再ロードし、旧メソッドが残る状況を作る。次を検証:
  `(eql :old)` → `(eql :new)`、特化子クラスの変更、メソッドの削除、`defgeneric` 内メソッドの変更、
  アクセサ名の変更 → いずれも編集情報が返らず、状態と理由が返る。
  正常系（未変更のファイル）は `matched` で `form_type` / `form_name` が返り、
  それを `lisp-edit-form` に渡すと**意図したフォームだけ**が変わること。
- [ ] **Step 2: pool 経由と pool 無効の一致** — 同じシンボルで両経路の `source_match` と `identity` が
  同じ意味になること（大整数・`NIL`・配列/リスト・JSON boolean を含む）。
- [ ] **Step 3: 文書** — spec §6 の項目を `docs/tools.md`（`clos-describe`）、ツール description、
  `prompts/repl-driven-development.md`、`CLAUDE.md` に反映。「すべての form_name をそのまま編集へ渡せる」
  という趣旨の既存記述を直す。
- [ ] **Step 4: GREEN**、mallet、コミット
  `test(clos-describe): verify source matching against a reloaded image`
  `docs(clos-describe): document matched / mismatched / unverified`

### Task 10: (A6) condition のアクセサを識別と検証に加える

**Files:** Modify `src/clos-core.lisp`, `src/clos-verify-core.lisp`, `tests/clos-core-test.lisp`,
`tests/clos-verify-core-test.lisp`

Task 3 で判明: SBCL は `define-condition` のスロット読み取りを `standard-accessor-method` にしないため、
Task 1 の identity に `class` / `slot` / `access` が入らず、condition のアクセサは常に `unverified` になる。
spec §3.4 はこれを照合対象としているので塞ぐ。

**Interfaces:** identity の `class` / `slot` / `access` を、次の場合にも埋める:
メソッドの特化子が 1 つで、それが condition クラス（またはクラス一般）であり、そのクラスの
`sb-mop:class-direct-slots` のいずれかの `slot-definition-readers` / `slot-definition-writers` に
そのメソッドの総称関数名が含まれるとき。`readers` に含まれれば `access` は `"reader"`、
`writers` に含まれれば `"writer"`。両方・複数スロットに該当する場合は埋めない（fail-closed）。
`clos-verify-core` は `:define-condition` コンテナでもアクセサ判定を行えるようにする。

- [ ] **Step 1: 失敗するテストを書く** — `clos-core-test`: `cl-mcp-clos-fixture:probe-error-code` の
  identity に `class` / `slot` / `access` が入る。`clos-verify-core-test`: その identity と
  `define-condition` の候補署名で `matched` になり、スロット名やアクセサ名を変えた候補では `mismatched`。RED を確認
- [ ] **Step 2: 実装**（MOP 読み取りは既存と同じく `ignore-errors` で包み、判定できなければ埋めない）
- [ ] **Step 3: GREEN**（`clos-core-test`, `clos-verify-core-test`）、mallet、コミット
  `feat(clos-core): identify a condition slot accessor by its class and slot`

### Task 11: (A7) 同じ秒に 2 回ロードしたファイルの行を取り違えない

**Files:** Modify `src/code-core.lisp`, `tests/code-test.lisp`

Task 5 のレビューで確認された既存不具合。`%debug-sources-by-namestring` は、`DEBUG-SOURCE-CREATED`
（秒単位）が同じ記録が複数あるとき「記録したフォーム数が多い方」を採る。これは `defpackage` が残す
短い記録を避けるための規則だが、**1 秒以内に 2 回保存してフォーム数が減った**場合、古い記録が勝ち、
分岐点より後ろの定義の行がすべてずれる。Task 5 の統合テストは `(sleep 1.1)` で回避している。

**Interfaces:** `%debug-sources-by-namestring` の同秒タイブレークを次にする。
同じ秒の記録のうち、位置ベクタが互いに**先頭一致（一方が他方の prefix）**ならこれまでどおり長い方を採る
（同じロードの部分記録なので安全）。prefix 関係にない記録が 2 つ以上あるときは、どちらが新しいか判断できないので
その**ファイルの記録を使わない**（`%form-start-offset` はファイルを読む経路にフォールバックする）。

- [ ] **Step 1: 失敗するテストを書く** — `tests/code-test.lisp`: 1 つのファイルを書いてコンパイル・ロードし、
  **同じ秒のうちに**フォーム数が減るよう書き換えて再コンパイル・ロードし、残っているクラス／メソッドの行が
  現在のファイルの行と一致すること（ずれた古い記録が採られないこと）。`sleep` を使わないこと。RED を確認
- [ ] **Step 2: 実装**（`%form-start-offset` の読み取りフォールバックは既存のものを使う）
- [ ] **Step 3: Task 5 の統合テストから `(sleep 1.1)` を外せるか確認し、外せるなら外す**
- [ ] **Step 4: GREEN**（`code-test`, `clos-describe-integration-test`）、mallet、コミット
  `fix(code-core): ignore ambiguous same-second debug sources instead of guessing`

---

## B. 編集ガード

### Task 6: (B1) スナップショットとダイジェスト

**Files:** Create `src/source-snapshot.lisp`, `tests/source-snapshot-test.lisp`; Modify `src/code-refs-scan.lisp`（スナップショットを使う）, `src/tools/all.lisp` は不要（ツールではない）, `tests.lisp`

**Interfaces (produces):**
```lisp
(read-source-snapshot abs-path)   ; => (values SNAPSHOT FAILURE)
;; SNAPSHOT: (:abs-path S :text S :octet-count N :digest "md5:<hex>")
(snapshot-range-digest snapshot start end)  ; => "md5:<hex>"（本文の [start,end) のオクテット）
(digest-string-octets string)               ; => "md5:<hex>"
```
- 読み取りは `%readable-path` と同じポリシー（`allowed-read-path`）を通す。拒否は `FAILURE` `:denied`。
- オクテットを 1 回だけ読み、UTF-8（不正バイトは `#\?`）で復号して `:text` にする。
  `:digest` は**オクテット列**の md5。範囲ダイジェストは `(subseq text start end)` の UTF-8 オクテット。
- `sb-md5` は `(require :sb-md5)` を `%ensure-sb-md5` のような形で遅延ロードし、
  使えないときは digest を NIL にして呼び出し側が `unverified` にできるようにする。

- [ ] **Step 1: 失敗するテストを書く** — 同じ内容 → 同じ digest、1 バイト違い → 違う digest、
  mtime とサイズを保ったまま内容を変えると digest が変わる、読み取り拒否、範囲ダイジェスト、
  不正バイトを含むファイルでも読めること。RED。
- [ ] **Step 2: 実装**。
- [ ] **Step 3: `top-level-forms-at` がスナップショットの `:text` を受け取れるようにする**
  （A2 の `:text` 引数）。親は 1 ファイル 1 回の読みで CST とダイジェストを作る。
- [ ] **Step 4: GREEN**（`source-snapshot-test`, `code-refs-scan-test`）、mallet、コミット
  `feat(source-snapshot): read a file once and digest it for edit guards`

### Task 7: (B2) `lisp-edit-form` の `guard` 引数と検証

**Files:** Modify `src/lisp-edit-form-core.lisp`, `src/lisp-edit-form.lisp`, `tests/lisp-edit-form-test.lisp`, `tests/tools-test.lisp`, `docs/tools.md`

**Interfaces (produces):**
- `lisp-edit-form` に任意引数 `guard`（JSON オブジェクト。`clos-describe` の `edit_guard` をそのまま）。
- `lisp-edit-form-core:check-edit-guard (guard abs-path snapshot node)` →
  `(values OK-P CONFLICT)`。CONFLICT は `(:reason <英語 1 文> :expected <string> :actual <string>)`。
  検証順は spec §4.2 の 1〜6。
- 失敗時のツール応答: `isError` + `conflict` オブジェクト + 「`clos-describe` を実行し直す」案内。
  `dry_run` でも同じ検証。`replace` / `insert_before` / `insert_after` / `delete` すべてに適用。

- [ ] **Step 1: 失敗するテストを書く** — spec §5 の表のガード関連すべて:
  対象フォームの置換・移動・削除後の拒否、対象外の場所や `in-package` の変更で拒否、
  mtime とサイズを保ったままの変更で拒否、同じ古いガードでの 2 回目の編集で拒否、
  `delete` / `insert_before` / `insert_after` / `dry_run` でも同じ検証、
  未知の `version` で拒否、拒否時にファイル内容が 1 バイトも変わらないこと、
  ガード無しの呼び出しは従来どおり通ること。RED。
- [ ] **Step 2: 実装** — `%locate-target-form` がスナップショットを使い、検証に通った本文をそのまま
  編集内容の生成に使う（読み直さない）。既存の書き込み経路の同期機構を確認し、同じ排他に参加する。
- [ ] **Step 3: GREEN**（`lisp-edit-form-test`, `lisp-patch-form-test`, `tools-test`）、mallet、コミット
  `feat(lisp-edit-form): refuse an edit when the guarded file or form changed`

### Task 8: (B3) `clos-describe` がガードを返す、文書、統合テスト

**Files:** Modify `src/tools/clos-response-builders.lisp`, `tests/clos-response-builders-test.lisp`, `tests/clos-describe-integration-test.lisp`, `docs/tools.md`, `prompts/repl-driven-development.md`

- [ ] **Step 1: 失敗するテストを書く** — `matched` かつ往復確認 OK の要素にだけ `edit_guard` が付く
  （`unverified` / `mismatched` には付かない）。`edit_guard` の各フィールドがスナップショットと
  CST 範囲に一致する。統合テスト: `clos-describe` → `edit_guard` 付きで `lisp-edit-form` →
  意図したフォームだけが変わる。観察後にファイルを変えると同じガードでの編集が拒否され、
  ファイルが変わらない。RED。
- [ ] **Step 2: 実装**。
- [ ] **Step 3: 文書** — `docs/tools.md` の `clos-describe` に `edit_guard` と推奨フロー、
  `lisp-edit-form` の `guard`、ガード有り／無しの保証の違い、競合時は再観察、
  防げる競合と残る制約（検証と書き込みの間の外部変更は防げない）を書く。
  `prompts/repl-driven-development.md` の CLOS 節にも 2 行で書く。
- [ ] **Step 4: GREEN**（`clos-response-builders-test`, `clos-describe-integration-test`）、mallet、コミット
  `feat(clos-describe): hand out an edit guard with verified edit information`

---

### Task 9: (V) 全体検証

- [ ] `mallet src/*.lisp src/*/*.lisp tests/*.lisp`
- [ ] `ros run ... (asdf:compile-system :cl-mcp :force :all)` で新しい警告が出ないこと
- [ ] 全スイート（コントローラがフォアグラウンドで `timeout 590 rove cl-mcp.asd`）。
      `;; testing '` の数と ✓ / × を数える。既知の × は project-scaffold の 1 本のみ
- [ ] 実物で `clos-describe` を実行し、正常系が `matched` のままであることと所要時間を記録
      （`hunchentoot:acceptor`, `cl:print-object`, `cl-mcp/src/utils/bounded-stream:bounded-output-stream`）
- [ ] 報告: 着手時 HEAD、A/B のコミット、修正前に再現した誤対応、識別データと三値の契約、
      ガードの schema と検証順序、実行したテスト・lint・コンパイルの結果、
      意図的に `unverified` としたケース、互換性への影響と残る制約
