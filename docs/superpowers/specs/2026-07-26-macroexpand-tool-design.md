# マクロ展開ツールの追加検討と設計

- 日付: 2026-07-26
- ステータス: 設計承認済み（実装未着手）
- 想定受益者: cl-mcp を利用する AI エージェント

## 1. 背景と問い

「マクロ展開のためのツールを cl-mcp に追加する価値があるか」を評価した。
結論は **条件付きで Yes**。ただし価値の内訳は「新機能」ではなく
「出力品質」と「導線（発見可能性）」にある。そのため安価で全体に効く
`repl-eval` の印字修正（Phase A）を先に行い、その後に
`repl-eval` では原理的に埋まらないギャップだけを埋める専用ツール
（Phase C）を追加する二段階構成とする。

## 2. 現状調査（実測）

### 2.1 機能ギャップは存在しない

`repl-eval` で `(macroexpand-1 '(...))` は既に実行できる。新規の能力は不要。

### 2.2 デフォルト出力の品質が低い

`(macroexpand-1 '(with-proxy-dispatch ...))` を `CL-MCP/SRC/REPL` で実行した実測結果:

```
(IF CL-MCP/SRC/PROXY:*USE-WORKER-POOL*
    (CL-MCP/SRC/TOOLS/HELPERS:RESULT CL-MCP/SRC/REPL::ID
                                     (CL-MCP/SRC/PROXY:PROXY-TO-WORKER ...
```

問題点:

- 全シンボルが完全修飾される。`define-tool` の展開ではトークンの約6割がパッケージ接頭辞だった
- 全体が UPCASE で、ソースに貼り戻す形式と一致しない
- `*print-circle*` により `#1=` / `#5#` の共有マーカーが混入する。
  `define-tool` の展開では `(validation-message . #5#)` というドット対が現れた。
  これは CL リーダーとしては正しく再読込可能だが、LLM や人間には誤読を招く
- `#:LIST206` の gensym、`SB-KERNEL:THE*` などの処理系内部が露出する

### 2.3 印字変数を束縛すれば出力は良質になる

同じ展開を `*print-case* :downcase` / `*print-circle*` nil / `*print-right-margin* 90`
のもとで `pprint` した実測結果:

```lisp
(if cl-mcp/src/proxy:*use-worker-pool*
    (result id (cl-mcp/src/proxy:proxy-to-worker id "worker/eval" (make-ht "code" code)))
    (progn (result id "x")))
```

つまりギャップは機能ではなく**デフォルト設定と導線**にある。

### 2.4 原因の特定

`src/repl-core.lisp` の `%do-repl-eval` 末尾（現行 247-251 行）:

```lisp
(let ((*print-level* print-level)
      (*print-length* print-length)
      (*print-readably* nil)
      (*print-circle* t))
  (values (%truncate-output (prin1-to-string last-value) max-output-length)
          ...))
```

- この束縛ブロックは `%eval-forms` 内の `(let ((*package* pkg)) ...)` の**外側**にある。
  よって印字時の `*package*` は worker のアンビエントなパッケージになり、完全修飾が発生する
- `*print-case*` は束縛されておらず、既定の `:upcase` が使われる
- `prin1-to-string` を使っており、`*print-pretty*` による整形が効かない

### 2.5 `*print-circle* t` は意図的で保護されている

`tests/repl-test.lisp:595` の `repl-eval-print-circle-prevents-hang` が
循環リストでのハング防止として `*print-circle* t` を検証している。
`repl-eval` からこれを外してはならない。

### 2.6 `macroexpand-all` は既定で利用不可

`sb-cltl2` は contrib モジュールであり未ロード。`(require :sb-cltl2)` が必要で、
発見可能性のギャップが実在する。

### 2.7 ファイル中フォームの直接展開手段がない

ファイル内の既存フォームを展開するには、読む → 文字列にコピー → エスケープ →
パッケージを推測 → `repl-eval`、という多段作業になる。
エスケープ失敗はエージェントの典型的な失敗パターンであり、
フォームが文脈に2〜3回出るトークン浪費も伴う。
**これが `repl-eval` では原理的に埋まらない唯一のギャップ。**

### 2.8 既存インフラの確認

- `src/package-context.lisp` の `call-with-file-package-context` が
  ファイルの `in-package` からリーダー文脈を復元する
- `src/cst.lisp` の `cst-node` は `start` / `end` / `start-line` / `end-line` を持つ。
  実測により**全ネスト階層で正確な文字範囲が付与される**ことを確認した。
  未定義シンボル（未ロードのシステム）でも Eclector は問題なく解析する
- `src/worker/handlers.lisp` には `%handle-set-project-root` があり、
  worker も project root を保持する

### 2.9 テスト影響の実測

| 変更 | 破壊されるアサーション |
|---|---|
| `*package*` 束縛 | 0件（パッケージ修飾を検査するテストは存在しない） |
| `*print-pretty* t` | 0件。既存の `string=` 期待値は `"3"` `"255"` 等すべて短く折返し不発 |
| `*print-case* :downcase` | 2件（`tests/repl-test.lisp:236,248` の `":OK"`） |
| `*print-circle*` オフ | 1件 + ハング再発。**採用しない** |

## 3. 判断の根拠

ROI の核心はツール枠の予算である。cl-mcp は既に約21ツールを公開しており、
ツール説明は全セッションの文脈に常駐する。マクロ展開の使用頻度は
`lisp-edit-form`（編集のたび）と比べれば低い。

それでも専用ツールを追加する決め手は、**受益者が AI エージェントである**点にある。
ドキュメントは読まれないかもしれないが、ツール一覧は必ず文脈に入る。
「`repl-eval` に印字変数を束縛するイディオム」を想起するエージェントは稀だが、
`lisp-macroexpand` がツール一覧にあれば使われる。
Phase A だけでは、改善した出力にエージェントが辿り着く導線がない。

一方、コード文字列だけを受け取る最小ツールは `repl-eval` と9割重複し、
ツール枠を恒久的に1つ消費する割に新規価値がほぼゼロのため採用しない
（第9節参照）。ファイル起点のアドレッシングこそが専用ツールを正当化する。

### 想定利用場面

1. 自作マクロの検証（`defmacro` を書いた直後に展開を確認）
2. 未知の DSL・マクロの読解（`define-tool` のようなマクロ多用コードベース）
3. コンパイルエラーの原因究明（展開後コードで出るエラーの出所特定）

場面 2 と 3 は対象フォームがファイル内にあるため、file モードが必須。
場面 1 は呼び出し側をエージェントが自分で組み立てるため、code モードが対応する。

## 4. Phase A: `repl-eval` の結果印字修正

変更箇所は `src/repl-core.lisp` の `%do-repl-eval` 末尾ブロック1箇所のみ。

現行では `pkg` が 228 行目の `let` に閉じ込められており印字ブロックから見えない。
`pkg` の束縛を印字ブロックまでスコープが届くよう巻き上げたうえで、以下を適用する。

```lisp
(let ((*package* pkg)              ; 追加: eval パッケージ基準で最短修飾
      (*print-level* print-level)
      (*print-length* print-length)
      (*print-readably* nil)
      (*print-case* :downcase)     ; 追加
      (*print-pretty* t)           ; 追加
      (*print-right-margin* 100)   ; 追加
      (*print-circle* t))          ; 維持（ハング防止・テスト済）
  ...)
```

各変更の位置づけ:

- **`*package*` 束縛 — 必須。本命。** マクロ展開に限らずあらゆる非プリミティブ結果の
  トークン効率と可読性に効く。情報の欠落はない（そのパッケージから見て曖昧なシンボルは
  修飾が保持される）
- **`*print-pretty* t` + `*print-right-margin* 100`** — 深くネストした結果が
  1行に潰れる問題を解消する
- **`*print-case* :downcase`** — エージェントがソースへ貼り戻す形式に一致させる

`*print-circle*` は維持する。したがって `#1=` 共有マーカーは `repl-eval` では残る。
これは Phase C のツールが自前の印字設定で解決する（第5.4節）。
`repl-eval` の汎用結果は本当に循環しうるのに対し、マクロ展開結果は実質的に
有限の木であるため、要件が異なる。設定を分けるのが妥当である。

## 5. Phase C: `lisp-macroexpand`

### 5.1 命名

`lisp-macroexpand`。既存の `lisp-read-file` / `lisp-edit-form` / `lisp-patch-form` /
`lisp-check-parens` という「Lisp 構造を理解するツール」族に揃える。

### 5.2 責務分割

| 層 | 担当 | 理由 |
|---|---|---|
| Parent | project-root ガード、ファイル読み、CST でのフォーム特定、`in-package` の抽出、ソーステキストの切り出し | 既存の `lisp-edit-form-core` / `cst.lisp` / `package-context.lisp` を再利用でき、セキュリティ境界が1箇所に留まる |
| Worker | 受け取ったフォームのテキストを実パッケージで読み、展開し、整形して返す | マクロ定義はロード済みイメージにしか存在しない |

Parent は**ソーステキストそのもの**とパッケージ名だけを worker に渡す。
parent 側で解析済み S 式を渡さないのは、parent が持つのがスタブパッケージの
場合にシンボルのパッケージ修飾が壊れうるため。生テキストを worker の
実パッケージで再読込するほうが堅牢である。

新規 worker handler は `%handle-macroexpand` 1つ。

展開と整形のロジックは `src/macroexpand-core.lisp` に純粋関数として切り出し、
worker handler と、`MCP_NO_WORKER_POOL=1` 時のインラインフォールバックの
**両方が同じ関数を呼ぶ**。これは既存の `repl-core` / `code-core` /
`test-runner-core` と同じ慣習であり、二経路の挙動がずれるのを防ぐ。
`macroexpand-core` は JSON も MCP も知らず、`(ラベル . ソース)` のリストを受けて
plist のリストを返す。

### 5.3 入力

モードは排他。

**モード1（ファイル起点）**

- `path`（必須）
- `form_type`, `form_name` — `lisp-edit-form` と同一のアドレッシング
- `sub_form`（任意）— 特定したトップレベルフォーム内のマクロ呼び出し名。
  CST を再帰的に走査し、先頭要素のテキストが（大文字小文字を無視して）一致する
  サブフォームを探し、`(subseq text start end)` でソーステキストを切り出す。
  パッケージ修飾付きで書かれている場合は最後の `:` 以降とも照合する。
  複数マッチした場合は行番号付きで番号を振り、すべて展開して返す。
  マッチ数の上限は 10 とし、超過時は「N 件中 10 件を表示」と報告する
- `readtable`（任意）— `lisp-read-file` と同じくカスタムリードテーブル対応

**モード2（コード）**

- `code`, `package`

**共通**

- `level`: `once`（既定、`macroexpand-1`）/ `full`（`macroexpand`）/
  `all`（`sb-cltl2:macroexpand-all`。ツール内で `(require :sb-cltl2)` を吸収）
- `print_level`（既定 50）, `print_length`（既定 1000）,
  `max_output_length`（既定は `repl-core` の `*default-max-output-length*` と揃えて 50000）

### 5.4 印字設定

`*print-circle*` を **nil** にして共有マーカーを排除する。
ハング防止は `*print-circle*` に頼らず、**`*print-level*` / `*print-length*` を
有限値（既定 50 / 1000）で必ず束縛する**ことで担保する。`*print-level*` が有限であれば
循環構造も `#` で打ち切られ、無限再帰しない。加えて `max_output_length` で最終的に切る。
呼び出し側が `print_level` に `nil` を渡して無制限にすることは許可しない。

その他: `*print-case* :downcase`、`*print-pretty* t`、`*print-right-margin* 100`、
`*package*` は対象パッケージ、`*print-gensym*` は既定（T）のまま維持する。

### 5.5 エラー設計

- **パッケージ不在**: parent 側の `call-with-file-package-context` は
  パッケージが存在しなければスタブを合成するが、**worker 側では合成してはならない**。
  スタブ上で展開すると「何も起きなかった」結果が静かに返り、エージェントを誤らせる。
  worker は `find-package` を厳密に使い、
  「パッケージ X が存在しません。`load-system` で対象システムをロードしてください」
  という行動可能なエラーを返す
- **展開されなかった**: `macroexpand-1` の結果が入力と `eq` の場合、
  入力をそのまま返すと成功に見えてしまう。`expanded: false` を立て、
  「X はマクロではありません」と明示する
- **expander 自体がエラーを signal**: 展開時エラーとして捕捉し原因を返す
- **出力肥大**: `max_output_length` で切り、`truncated` を立てる

### 5.6 レスポンス形状

過去の総合テストで判明している教訓（sibling JSON フィールドはクライアントが描画せず、
`content[].text` のみが可視）に従い、**展開結果とメタ情報は必ず `content[].text` に入れる**。
`expanded` / `steps` / `truncated` は構造化フィールドにも置くが、
テキスト要約にも反映させる。

## 6. 既知の制約

ツール説明とドキュメントに明記する。

1. **null レキシカル環境での展開**。切り出したサブフォームは囲む環境なしで展開される。
   `macrolet` / `symbol-macrolet` に囲まれている場合、コンパイラが見る展開とは異なる。
   実務上 `macrolet` は稀だが、正確さのため明記する
2. **`level: all` の出力肥大**。`macroexpand-all` は `loop` や `defun` まで
   特殊形式へ展開するため巨大化しうる。既定は `once` とし、`all` は明示指定のみ
3. **カスタムリードテーブル**。`readtable` パラメータで対応するが、指定が必要。
   さらに **`readtable` と `sub_form` は併用できない**: `readtable` を指定すると
   `parse-top-level-forms` は Eclector ではなく標準 CL リーダー経路
   （`%read-remaining-with-cl-reader`）を通り、`cst-node-children` が常に NIL になるため
   サブフォームを走査できない。併用時は行動可能なエラーを返す
4. **worker 側での再読込**。parent はソーステキストを切り出して渡すだけなので、
   worker 側の読み取りも同じ `readtable` を必要とする。`macroexpand-core` は
   自前の軽量な named-readtable 解決を持つ（`lisp-edit-form-core` はツール層寄りで
   worker から引くには重すぎるため）
5. **任意コード実行**。展開は expander 関数を走らせる。worker 隔離下で
   `repl-eval` と同等であり新規リスクは増えないが、ツール説明に明記する

## 7. テスト戦略

**Phase A**（`tests/repl-test.lisp`）

- 既存2件を更新（`":OK"` → `":ok"`）
- 新規: eval パッケージ指定時にシンボルの修飾が短縮されること
- 新規: 長い結果に `*print-pretty*` の改行が入ること
- 回帰保護: `repl-eval-print-circle-prevents-hang` が引き続き通ること

**Phase C**（`tests/lisp-macroexpand-test.lisp`。テスト対象ファイルは `tests/tmp/` に一時生成する
— `tests/fixtures/` は git 未追跡かつ参照ゼロの死んだディレクトリで、実際の慣習は
`tests/tmp/` + `with-temp-file` パターン）

- file モード top-level
- file モード `sub_form`（単一マッチ・複数マッチ）
- code モード
- `level` の3値（`once` / `full` / `all`）
- パッケージ未ロード → 行動可能なエラーメッセージ
- 非マクロ → `expanded: false` が立ち、入力の黙ったエコーバックにならないこと
- expander が error を signal するケース
- 循環構造を返すマクロでハングしないこと（`*print-level*` バックストップの回帰保護）
- `max_output_length` での切り詰め

## 8. フェーズ分割と影響ファイル

### PR 1（Phase A）— 単独でマージ可能、Phase C に依存しない

- `src/repl-core.lisp` — `%do-repl-eval` の1箇所
- `tests/repl-test.lisp` — 既存2件更新、新規3件

### PR 2（Phase C）

- `src/macroexpand-core.lisp`（新規）— 展開と整形の純粋ロジック。worker handler と
  インラインフォールバックの両方が共有する（`repl-core` / `code-core` と同じ慣習）
- `src/lisp-macroexpand.lisp`（新規）— parent 側アドレッシング + `define-tool`
- `src/tools/response-builders.lisp` — `build-macroexpand-response` 追加
- `src/worker/handlers.lisp` — `%handle-macroexpand` 追加、`register-all-handlers` に登録
  （ハードコードされた `"count" 10` を 11 に更新すること）
- `src/tools/all.lisp` — import 追加。**これがないとツールモジュールは静かにロードされず、
  エラーも警告もなく `tools/list` に現れない**
- `main.lisp` — `:import-from` と `:export` に追加（既存15ツール全てがこの慣習に従う）
- `tests.lisp`（**リポジトリルート**、`tests/` ではない）— `(:import-from #:cl-mcp/tests/lisp-macroexpand-test)`
  を追加。これがないと `rove cl-mcp.asd` と CI から不可視になる
- `cl-mcp.asd` — `#+sbcl (require :sb-cltl2)` を既存の `(require :sb-posix)` の隣に追加。
  contrib は FASL ロード前に存在している必要があるため（同ファイルのコメント参照）
- `tests/lisp-macroexpand-test.lisp`（新規）
- `prompts/repl-driven-development.md` — ツール表に追記

**`cl-mcp.asd` について**: このシステムは `:class :package-inferred-system` で
`:components` を一切持たない。ファイル追加そのものでは `.asd` の編集は不要で、
`src/<n>.lisp` が定義するパッケージ名 `cl-mcp/src/<n>` がパスと一致していれば
他ファイルの `:import-from` 経由で自動的にロードされる。上記の `require` 追加は
SBCL contrib の事前ロードという別の理由による。なお `CLAUDE.md` の
「Add new files by updating cl-mcp.asd dependencies」と `AGENTS.md` の
「extend the cl-mcp/tests ASDF component list」は**いずれも現状と合っていない**。

## 9. 採用しなかった案

- **A のみ（ツール追加を見送る）**: `repl-eval` の印字修正とプロンプトへの
  イディオム記載だけで打ち止めにする案。ツール枠を消費しない利点があるが、
  エージェントが改善後の出力に辿り着く導線がなく、ファイル起点のギャップも残る
- **コード文字列のみを取る最小ツール**: `repl-eval` と9割重複し、
  ツール枠を恒久的に消費する割に新規価値がほぼゼロ
- **行番号指定（`path` + `line`）**: コンパイラのエラーメッセージと直接紐づく利点があるが、
  既存の `form_type` / `form_name` アドレッシングと体系が二重になる
- **`repl-eval` で `*print-circle*` をオフにする**: ハング防止として意図的に
  導入されテストで保護されているため不可（第2.5節）
