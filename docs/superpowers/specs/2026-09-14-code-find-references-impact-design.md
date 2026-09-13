# code-find-references の影響範囲分析強化 設計

- 日付: 2026-09-14
- ステータス: 設計承認済み（実装未着手）
- 想定受益者: cl-mcp を利用する AI エージェント（変更前に影響範囲を把握したい場面）
- ブランチ: `feat/code-find-references-impact`

## 1. 背景と問い

提案は「quasi/cl-mcp-server にある `who-calls` / `who-references` を取り込むべき（優先度 S）」
というものだった。LLM がコードを変更するとき本当に知りたいのは「定義はどこか」ではなく
「これを変えると何が壊れうるか」であり、その答えを構造化して返す専用ツールには価値がある、
という主張である。

調査の結論: **cl-mcp には既に `code-find-references`（`src/code-core.lisp:416`）があり、
SBCL の `who-calls` / `who-macroexpands` / `who-binds` / `who-references` / `who-sets`
を統合して返している。** 足りないのは機能の有無ではなく、

1. xref の盲点（トップレベル使用）を黙って 0 件として返すこと
2. 呼び出し行・テスト名・修飾名など、影響範囲の判断に必要な粒度がないこと
3. 空結果・未解決を区別せず沈黙すること、問い合わせで intern する副作用
4. 提案者自身が既存ツールを見落とした＝発見可能性が低いこと

である。よって新ツールは追加せず、`code-find-references` を後方互換に拡張する。

## 2. 現状調査（実測）

worker に `cl-mcp` と `cl-mcp/tests/code-test` をロードして計測した。

| # | 実測 | 影響 |
|---|---|---|
| 1 | `define-tool` の使用箇所が **0 件**（実際は数十か所） | トップレベルで展開されるマクロや `defparameter` 初期値は SBCL xref に記録されない。0 件が「呼び出し元なし」を意味しない |
| 2 | 0 件のとき本文テキストが**空** | 「呼び出し元なし」「シンボル未定義」「システム未ロード」の区別がつかない |
| 3 | `cl-mcp/src/code-core:%parse-symbol` がエラー（`::` なら動く） | 内部シンボルを `pkg:` 形式で指定できない |
| 4 | 存在しない `code-core::%nonexistent-fn` を問い合わせると、そのシンボルが **intern された**（`find-symbol` が `:internal` を返す） | 問い合わせが worker に副作用を残す。プロジェクト方針（動的 intern 禁止）にも反する |
| 5 | テストからの参照が `tests/code-test.lisp:172 [call]` とだけ出る | xref の呼び出し元が `(lambda () :in "…/code-test.lisp")` のため、どの deftest か分からない |
| 6 | 行番号は呼び出し元フォームの**先頭行** | 実際の呼び出し行ではない。`definition-source` の `form-path` もトップレベルフォームの番号だけで、サブフォームの位置は持たない |
| 7 | 呼び出し元は小文字化した文字列だけ | パッケージ付きで構造化された名前がない |

補足の実測:

- xref の `character-offset` は呼び出し元の**トップレベルフォームの開始位置**を指す。
  `define-tool` が生成したハンドラ（`code-find-references-handler`）でも
  `src/code.lisp:68`（`define-tool` フォームの先頭）だった
- マクロ展開の中に隠れた呼び出しは xref にしか現れない
  （例: `with-proxy-dispatch` の 12 か所の使用は `who-macroexpands` で取れる）
- worker は `cl-mcp/src/worker/main` だけをロードする最小構成
  （`src/worker-client.lisp:336`）で、eclector / `cl-mcp/src/cst` を含まない
- ソース走査のコスト: `make-ht`（最頻出級の名前）で、対象 147 ファイル中
  ヒットした 39 ファイル・約 1.05MB を `parse-top-level-forms` で解析して **181ms**

## 3. quasi/cl-mcp-server との比較

quasi の `who-calls` / `who-references`（`src/introspection.lisp`）は位置がファイル単位だけで、
情報量は現行 cl-mcp より少ない。取り込む価値があるのは次の設計判断である。

- `find-symbol` で解決し、無ければ `Symbol X not found in package Y` と明示する（intern しない）
- 0 件で `No callers found for X` と明示する
- 呼び出し元を `caller-package` + `caller` で構造化する
- 同一呼び出し元の重複を件数に畳む。repl-eval で定義した関数を session-defined と区別する
  （cl-mcp は `repl-eval` パスを既に除外している）

`who-calls` / `who-references` という別名ツールの追加は、`tools/list` に重複を増やすため採らない。
代わりに description に "who-calls / callers / impact" を明記して発見可能性を上げる。

## 4. スコープ

含める:

- **#6 正確な呼び出し行**: 呼び出し元フォームの中の実際の行と列
- **#1 トップレベル使用の検出**: xref に記録されない使用をソース走査で補う
- **#5 テストとの対応付け**: `(lambda)` を囲む deftest 名に解決する
- 不具合修正 #2 #3 #4 #7

含めない（非ゴール）:

- 推移的な呼び出し元（depth=N）、callees（逆方向）
- cl-spec の spec との対応付け
- `defpackage` / `uiop:define-package` の export / import 一覧
- `flet` / `labels` / `macrolet` 以外のレキシカルスコープの厳密な扱い
- プロジェクト外のソース走査（`project_only=false` の外部 xref 結果には `call_sites` を付けない）
- `code-find` / `code-describe` の同じ intern 問題（`%parse-symbol` 共用）。新リゾルバの流用は別 PR

## 5. アーキテクチャ

### 5.1 採用案: 親と worker で分担し、RPC は 1 往復

検討した案:

| 案 | 概要 | 判定 |
|---|---|---|
| **A** | 親が CST で候補を集め、worker が xref と `find-symbol` で解決・マージ | **採用**。同名の別シンボルを誤検出せず、worker は最小構成のまま |
| B | すべて worker で処理し、eclector を必要時にロード | 不採用。ユーザーのイメージに eclector と CST 基盤が入り、別バージョンの eclector に依存するプロジェクトと衝突しうる |
| C | 親が名前だけで走査し、パッケージは解決しない | 不採用。`result` や `run` などで別シンボルが混ざり、目的（正確な影響範囲）に反する |

パターンは `lisp-macroexpand` と同じ: `with-proxy-dispatch` の params-form を親で評価して
候補を同梱し、worker で解決する。worker pool 無効時は同じ関数を同一プロセスで順に呼ぶ。

### 5.2 データの流れ

```
親プロセス                                     worker
─────────────────────────────                 ─────────────────────────────
1. symbol 文字列から名前部分を
   テキストとして取り出す（intern しない）
2. プロジェクト内の .lisp/.asd を列挙
   （collect-target-files、.gitignore を尊重）
3. 名前で大文字小文字を無視して絞り込み、
   ヒットしたファイルだけ CST で解析
4. 名前が一致するシンボルごとに候補を作る：
   path, line, column, in-package,
   綴り, 位置の分類,
   囲むトップレベルフォーム（開始位置・型・名前・テスト名）
5. ── worker/code-find-references ──────────▶ 6. 対象シンボルを find-symbol で解決
   （candidates を params に同梱）                （intern しない。未解決なら理由を返す）
                                              7. xref（5 種）を実行
                                              8. 候補の綴りを、その in-package の中で
                                                 解決し、対象と eq のものだけ残す
                                              9. (path, トップレベルフォームの開始位置) で
                                                 xref と候補を突き合わせてマージ
                                              10. 応答を組み立てる（response-builders 共用）
```

マージの結果は 3 通り:

| xref | 候補 | `origin` | 意味 |
|---|---|---|---|
| あり | あり | `xref+source` | 実際の呼び出し行を `call_sites` に入れる |
| あり | なし | `xref` | マクロ展開の中に隠れた呼び出し。`call_sites` は空で理由を注記 |
| なし | あり | `source` | トップレベル使用、未コンパイルのコード、ロード後に変更されたファイルのいずれか |

ロード後の変更は、xref の `definition-source-file-write-date` と現在のファイルの
`file-write-date` を比べて `stale` とする。

### 5.3 ファイル構成

| ファイル | 側 | 責務 |
|---|---|---|
| `src/code-refs-scan.lisp`（新規） | 親 | 候補の収集。依存: `cl-mcp/src/cst`、`cl-mcp/src/utils/clgrep`、`cl-mcp/src/project-root` |
| `src/code-refs-core.lisp`（新規） | worker | intern しないシンボル解析と解決、候補の判定、マージ（純粋関数）。eclector に依存しない |
| `src/code-core.lisp` | worker | `code-find-references` が `:candidates` を受け取り、`code-refs-core` に委譲する |
| `src/code.lisp` | 親 | 引数追加、params-form で走査を実行、description 更新 |
| `src/worker/handlers.lisp` | worker | `%handle-code-find-references` が candidates と limit を渡す |
| `src/tools/response-builders.lisp` | 両方 | `build-code-find-references-response` を新形式に |

`code-core.lisp` は既に約 470 行あるため、新ロジックは別ファイルに分ける。
worker が新たに依存するのは `code-refs-core` だけで、`code.lisp`（と走査ファイル）は
worker の依存グラフに入らない。

## 6. インターフェース

### 6.1 引数

| 引数 | 型 | 既定 | 変更 |
|---|---|---|---|
| `symbol` | string | 必須 | 既存。`pkg:internal` も受け付けるようになる |
| `package` | string | CL-USER | 既存 |
| `project_only` | boolean | true | 既存 |
| `limit` | integer | 50 | **新規**。本文と `refs` に載せるフォーム数の上限 |

### 6.2 構造化データ

既存フィールドはすべて残し、意味も変えない（`line` は従来どおり呼び出し元フォームの先頭行）。

```json
{
  "symbol": "code-core:code-find-references",
  "resolved_symbol": "CL-MCP/SRC/CODE-CORE:CODE-FIND-REFERENCES",
  "symbol_status": "found",
  "symbol_kind": "function",
  "count": 4,
  "project_only": true,
  "truncated": false,
  "refs": [{
    "path": "src/code.lisp", "line": 68, "type": "call",
    "caller": "code-find-references-handler", "context": "(define-tool \"code-find-references\"",
    "types": ["call"],
    "caller_symbol": "CL-MCP/SRC/CODE::CODE-FIND-REFERENCES-HANDLER",
    "form_type": "define-tool", "form_name": "code-find-references",
    "origin": "xref+source",
    "call_sites": [{"line": 92, "column": 5, "kind": "call", "context": "(code-find-references symbol ..."}],
    "test": null,
    "stale": false
  }],
  "tests": [{"name": "code-find-references-returns-project-refs", "path": "tests/code-test.lisp", "line": 172}],
  "unresolved": [{"path": "tests/foo-test.lisp", "package": "CL-MCP/TESTS/FOO-TEST", "count": 3, "tests": ["foo-test"]}],
  "notes": []
}
```

- `symbol_status`: `found` / `not_found` / `package_not_found`
- `symbol_kind`: `function` / `macro` / `generic-function` / `variable` / `constant` / `unbound`
- `origin`: `xref+source` / `xref` / `source`
- `caller_symbol`: パッケージ付きの呼び出し元名。`(lambda)` やトップレベル使用では null
- `form_type` / `form_name`: `lisp-edit-form` / `lisp-read-file` の指定にそのまま使える値。
  名前を持たないトップレベルフォーム（`progn` など）では `form_name` が null
- `test`: `{"name", "framework"}` または null
- `call_sites[].shadowed_by`: 6.4 のシャドウ検出時のみ付く
- 1 エントリ = 1 トップレベルフォーム。同じフォームに複数の xref 種別があれば `types` に並べ、
  `type` には先頭（互換用）を入れる。`origin` が `source` のエントリでは、`types` は
  `call_sites[].kind` の重複を除いたもの
- 1 つのトップレベルフォームに xref の呼び出し元が複数ある場合（`flet` の内側関数と外側の関数など）は、
  シンボル名を持つ呼び出し元を `caller` / `caller_symbol` に採る
- `count` は `limit` で切る前の総フォーム数。`refs` の長さは `min(count, limit)`。
  `truncated` は `count > limit` のとき true

### 6.3 本文テキスト

MCP クライアントに表示されるのは `content[].text` だけなので、判断に必要な情報はすべてここに載せる。
出力は英語（既存ツールに合わせる）。

通常（行番号は形式を示すための例）:

```
CL-MCP/SRC/CODE-CORE:CODE-FIND-REFERENCES (function) — 4 forms in 3 files, 2 tests
src/code.lisp:68 (define-tool "code-find-references") [call]
  92: (code-find-references symbol :package package :project-only project-only)
src/worker/handlers.lisp:284 (defun %handle-code-find-references) [call]
  291: (code-find-references (gethash "symbol" params)
tests/code-test.lisp:172 (deftest code-find-references-returns-project-refs) [call] TEST
  177: (code-find-references "cl-mcp:process-json-line")
tests/code-test.lisp:190 (deftest code-find-references-includes-caller) [call] TEST
  194: (code-find-references "cl-mcp:process-json-line")
Tests: code-find-references-returns-project-refs, code-find-references-includes-caller
```

行末の注記:

- `— call not visible in source (produced by a macro expansion)`: `origin` が `xref`
- `— top-level use, not in xref`: `origin` が `source`
- `— file changed since load; reload for accurate results`: `stale`
- `— shadowed by flet`: シャドウ検出
- 1 フォームの呼び出し箇所は 5 件まで表示し、残りは `+N more`
- `limit` を超えたら末尾に `… N more forms (raise limit to see them)`

0 件や未解決:

```
Symbol "%nonexistent-fn" not found in CL-MCP/SRC/CODE-CORE (nothing was interned).
7 textual matches in 2 files. Is the system loaded? Run load-system first.
```

```
CL-MCP/SRC/FOO:BAR (function) — no references.
  xref: 0 entries   source scan: 147 files, 0 matches
```

```
+ 3 possible matches in files whose package is not loaded:
  tests/foo-test.lisp (CL-MCP/TESTS/FOO-TEST; deftest foo-test) — load that system to check them
```

### 6.4 判定ルール（親側の CST 走査）

名前の照合は 2 段階で行う。ファイルの絞り込みは生テキストに対する大文字小文字を無視した検索
（取りこぼしを防ぐため緩く）。候補の採否は、CST が読んだシンボルの `symbol-name` と、
入力から 6.5 の規則で取り出した名前との `string=`（エスケープで小文字を含む名前も正しく扱うため厳密に）。

名前が一致したシンボルは、位置によって分類する。

| 位置 | `kind` |
|---|---|
| リストの先頭 `(foo ...)` | `call`（対象がマクロなら worker 側で `macroexpand` に置き換える） |
| `#'foo` / `(function foo)` | `function` |
| `'foo` | `quoted` |
| バッククォートの中の、アンクォートされていない位置 | `template` |
| `let` / `let*` のバインディング | `bind` |
| `setf` / `setq` / `psetf` / `psetq` の place | `set` |
| `(defmethod foo ...)` の名前位置 | `method` |
| それ以外 | `reference` |

除外:

- 対象自身の定義名の位置: `defun` / `defmacro` / `defgeneric` / `defvar` / `defparameter` /
  `defconstant` / `define-compiler-macro` など、`def` で始まる定義フォームの第 2 要素
  （`defmethod` だけは上表のとおり `method` として含める）
- キーワード（`:foo`）と uninterned シンボル（`#:foo`）
- `defpackage` / `uiop:define-package` のフォーム全体
- コメントと文字列（CST 上シンボルにならないため自然に除外される）

シャドウ: `flet` / `labels` / `macrolet` が同じ名前を束縛している下の位置には `shadowed_by` を付ける。
規則は `%find-sub-forms`（`src/lisp-macroexpand.lisp:83`）と同じ。
走査の実装は、位置情報つきで全シンボルを列挙する必要があるため `%find-sub-forms` そのものではなく、
その判定規則（`quote` / `function` / `quasiquote` / 束縛位置の扱い）を共有する形で書く。

囲むフォーム: `form_type` / `form_name` は `lisp-edit-form` がフォームを特定するときと同じ規則で作る
（`defmethod` は specializer 付き、`(setf foo)` など）。

テスト: 囲むトップレベルフォームの先頭の名前が `deftest`（rove）、`test` / `def-test`（fiveam）、
`define-test`（parachute）なら `test` を付ける。xref の `(lambda () :in ...)` も
(path, フォームの開始位置) で対応する deftest に解決する。

in-package: その候補より前にある最後の `in-package`（`%package-in-effect-at` と同じ規則）。
無ければ CL-USER。

### 6.5 シンボルの解決（worker 側）

- 入力文字列は `|...|` と `\` のエスケープ、標準 readtable-case の大文字化を処理してから
  `find-package` と `find-symbol` だけで解決する。リーダーは使わない
- `pkg:internal` も受け付ける。`resolved_symbol` は実際の状態に合わせて `pkg::name` と表記する
- 接頭辞なしは `package` 引数、無ければ CL-USER で解決する
- 候補の綴りは、`*package*` をその候補の in-package に束縛してから解決する
  （package-local-nicknames を効かせるため）
- 解決結果が対象と `eq` の候補だけを残す。別シンボルに解決された候補は黙って捨てる
- in-package のパッケージ、または接頭辞のパッケージが worker に無い候補は `unresolved` に分類し、
  ファイルごとに件数と（テストフォームなら）テスト名を添える。未検証と明示する

## 7. エラー処理

| 状況 | 挙動 |
|---|---|
| 空文字・構文不正な `symbol`、キーワード | `arg-validation-error` |
| `limit` が正でない | `arg-validation-error` |
| パッケージが無い / シンボルが無い | 通常の結果で `symbol_status` を返す。名前だけで一致した件数をヒントに添える |
| 個別ファイルが解析できない | そのファイルを飛ばし、件数と先頭 3 件のパスを `notes` と本文に出す |
| project root が未設定 | ソース走査を省き xref のみ。`source scan skipped: project root not set` と注記 |
| 候補が 5000 件を超える | そこで打ち切り、`truncated` と注記 |
| `project_only=false` の外部ライブラリ参照 | ソース走査しないので `call_sites` なし（注記あり） |
| worker のクラッシュ・タイムアウト | 既存の proxy の処理に従う |

## 8. テスト

マージは純粋関数にし、xref の結果と候補をデータとして受け取ってエントリを返す。
xref の実行とファイル読み込みはその外側の薄い層に置く。

| テストファイル | 対象 | 主なケース |
|---|---|---|
| `tests/code-refs-scan-test.lisp`（新規） | 親側の走査 | 文字列から候補を作る。6.4 の分類すべて、除外、`flet` シャドウ、テストフォーム判定、`defmethod` / `(setf foo)` の `form_name`、`in-package` 切り替え、`#+sbcl` 付きフォーム |
| `tests/code-refs-core-test.lisp`（新規） | worker 側の解決とマージ | エスケープや `pkg:internal` の解析、**問い合わせ前後で `find-symbol` の結果が変わらない**こと、PLN 経由の解決、別パッケージ同名シンボルの除外、`unresolved` 分類、3 通りのマージ、stale 判定 |
| `tests/response-builders-test.lisp`（更新） | 本文テキスト | 各注記、0 件、not_found、unresolved、truncated、`+N more`。既存の完全一致テストは新形式に書き換える |
| `tests/code-test.lisp`（更新） | 統合 | `tests/fixtures/xref-fixture.lisp` をロードして実際に xref を記録させ、`call_sites` の行番号を完全一致で検証 |
| `tests/tools-test.lisp` / `tests/worker-test.lisp`（更新） | tools/call とプロセス内実行、worker ハンドラ | candidates を載せた params が worker で解釈されること |

新規テストファイルはルートの `tests.lisp` に登録する。

フィクスチャ `tests/fixtures/xref-fixture.lisp` に入れるケース:

- 普通の関数呼び出し
- `defparameter` 初期値からの呼び出し（トップレベル使用）
- マクロ展開の中にしか現れない呼び出し
- トップレベルで使われるマクロ
- rove の `deftest` の中からの呼び出し
- generic function とそのメソッド
- `flet` による同名シャドウ
- 別パッケージの同名シンボル（誤検出しないこと）
- PLN を使った呼び出し
- `eval-when` で包まれた `defun`

既知の落とし穴:

- JSON の `false` はプロセス内実行で `yason:false`、worker 経由で `NIL` になる。
  候補の真偽値フィールドは両方で検証する
- xref を使うテストは既存テストに合わせて macOS ではスキップする

## 9. 完了条件

- `mallet src/*.lisp src/*/*.lisp tests/*.lisp` が通る
- `(asdf:compile-system :cl-mcp :force :all)` で新しい warning が出ない
- `rove cl-mcp.asd` で全スイートを実行し、終了コードではなく ✓ の数と `;; testing` 行数で確認する
- cl-mcp 自身に対して `make-ht` と `define-tool` で実行し、所要時間と出力量を記録する
- `docs/tools.md` の `code-find-references` 節と `prompts/repl-driven-development.md` の早見表
  （呼び出し元・影響範囲を調べるならこのツール）を更新する

## 10. 実装初期に確認するリスク

| リスク | 確認方法 | 外れた場合 |
|---|---|---|
| `eval-when` / `progn` で包んだ定義で、xref の `character-offset` が外側のトップレベルフォームを指さない | フィクスチャで実測 | `form-path` を使って内側のフォームに対応付ける |
| `#+sbcl` 付きフォームで、xref の開始位置が `#+` と本体のどちらを指すか | フィクスチャで実測 | 突き合わせ時に reader conditional の分だけ許容する |
| 名前が一致する候補が多すぎて params が大きくなる（`make-ht` 級） | 実際に計測 | 候補の送信形式をファイル単位にまとめて圧縮する |
| `in-readtable` を使うファイルで CST 解析が失敗する | 既存のフィクスチャで確認 | 解析失敗として飛ばし、注記する（7 章どおり） |
