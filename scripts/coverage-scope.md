# カバレッジ計測の適用範囲

`docs/coverage-summary.md` の数値を読む前にこれを読むこと。
**カバレッジ 0% は「未テスト」を意味しない。**

- 計測日: 2026-08-01
- 対象コミット: 30d9ccc（`docs/coverage-summary.md` を追加したコミット。実測データ自体は
  一つ前の `8f5b970` のコールドキャッシュ実行で生成されたものをそのまま採用しており、
  `30d9ccc` の時点で再実行はしていない）
- SBCL: 2.5.8.roswell

## 分類

| 分類 | 意味 | 数値の読み方 |
|------|------|-------------|
| 親で実行 | 親プロセス内で動くのでカバレッジが正しく出る | そのまま信じてよい |
| ワーカー専用 | ワーカー子プロセス内でのみ動く。sb-cover は自プロセスしか見ない | 0% でも未テストとは限らない |
| 計測不能 | 計装されなかった、または計測実行から除外した | 除外理由を各行に書く |

**結論を先に書く**: 59 ファイルのうち計測不能は 0 件、ワーカー専用は `src/worker/main.lisp`
1 ファイルのみ。残り 58 ファイルは親で実行であり、そのまま信じてよい（ただし低い数字が
「定義系トップレベルフォームの影響」で説明できる場合は下記参照）。「ワーカー専用は
`src/worker/` 全体」という設計時の想定は、`src/worker/` 4 ファイル中 3 ファイル
（`handlers.lisp`/`server.lisp`/`init-hook.lisp`）について検証の結果否定された。根拠は
次節。

## `src/worker/` と `*-core` の判定根拠

設計時の想定は「ワーカー専用 = `src/worker/` 配下」だったが、これは検証対象としてこの
タスクに残されていた仮定であり、正しくなかった。`docs/coverage-summary.md` の実測値と
テストコードを突き合わせた結果は次の通り。

### `src/worker/` 配下 4 ファイルの実測値

| ファイル | 式カバレッジ | 判定 |
|---|---|---|
| `src/worker/main.lisp` | 46/264 (17.4%) | ワーカー専用 |
| `src/worker/handlers.lisp` | 301/377 (79.8%) | 親で実行 |
| `src/worker/server.lisp` | 263/349 (75.4%) | 親で実行 |
| `src/worker/init-hook.lisp` | 202/223 (90.6%) | 親で実行 |

`handlers.lisp`・`server.lisp`・`init-hook.lisp` は 0% どころか 75〜90% という高い値が
出ている。理由は `tests/worker-test.lisp` の `with-handler-server` マクロ（247〜273行目）
にある。このマクロは `bordeaux-threads:make-thread` で `start-accept-loop` を**計測対象
プロセスと同じ SBCL イメージ内のスレッド**として起動し、そこへ実 TCP ソケットで接続して
JSON-RPC をやり取りする。`sb-ext:run-program` によるプロセス起動は一切行っていない。
`tests/worker-init-hook-test.lisp`（125〜166行目）も同じ仕組みで検証している:
`bt:make-thread` でサーバーを計測プロセス内のスレッドとして起動し、実 TCP ソケット越しに
`worker/init-start`・`worker/init-status` という JSON-RPC メソッドを送って応答を確認して
いる（`handle-init-start`/`handle-init-status` という関数を直接呼んでいるわけではない。
その2関数名は `tests/` 配下のどこにも出現しない）。つまりこの 3 ファイルの本番での実行
場所（ワーカー子プロセス内）と、計測パイプラインがそのロジックを実際に踏む場所（計測
プロセス自身）が一致しており、sb-cover の「自プロセスしか見えない」制約に引っかからない。
数値は正しく出ている。

対照的に `src/worker/main.lisp` の `start`（175行目）と、それが呼ぶ `%try-start-swank`
（41行目）・`%install-signal-handlers`（115行目）・`%start-parent-watchdog`（128行目）は、
`tests/worker-test.lisp` 631行目の `worker-start-creates-server-and-handshakes` テスト
内、635〜636行目のコメントが明言する通り
「We cannot call start directly because it blocks in start-accept-loop」——実際に spawn
された子プロセスの中でしか実行できない。これは `tests/pool-startup-latency-test.lisp` や
`tests/pool-test.lisp` が `spawn-available-p` を確認した上で本物の SBCL 子プロセスを
起動して確認している e2e パスであり、そこで実行される `start` 以下のコードは計測プロセス
から見て完全に別プロセスなので sb-cover には映らない。`main.lisp` の 46/264 (17.4%) の
内訳は、`%output-handshake`・`%setup-project-root` のような**プロセス起動を伴わない
純粋関数**が `tests/worker-test.lisp`（591, 610, 623, 684, 883行目）で親プロセス内から
直接ユニットテストされている分（`%get-pid` は単体では直接呼ばれないが、`%output-handshake`
が内部で呼ぶため間接的にカバーされる）であり、`start` 以下の本体（プロセス生存確認・
シグナルハンドラ・Swank 起動・接続受理ループ）は計測不能な設計上のワーカー専用コードと
して残る。0% ではなく 17.4% だからといって「一部しかテストされていない」と読むのは誤り
――純粋関数の部分は完全にテストされており、低い % は「ワーカー専用の本体コードが計測に
乗らない」ことをそのまま反映している。

### `*-core` 5 ファイルの実測値

`src/worker/handlers.lisp` は `repl-core`・`code-core`・`system-loader-core`・
`test-runner-core`・`macroexpand-core` への薄いディスパッチ層であり、これらは本番では
ワーカー子プロセス内でのみ呼ばれる実処理を担う。しかし実測値はいずれも高い。

| ファイル | 式カバレッジ | 直接呼び出しているテストの例 |
|---|---|---|
| `src/repl-core.lisp` | 410/478 (85.8%) | `repl-test.lisp` が `cl-mcp/src/repl-core::%repl-eval-with-timeout` を直接呼ぶ |
| `src/code-core.lisp` | 657/803 (81.8%) | `code-test.lisp` が `cl-mcp/src/code-core::%offset->line`・`%format-xref-caller` を直接呼ぶ |
| `src/system-loader-core.lisp` | 382/464 (82.3%) | `system-loader-test.lisp` が `%redefinition-warning-p`・`%decide-suppress-redefinition` 等を直接呼ぶ |
| `src/test-runner-core.lisp` | 1587/2258 (70.3%) | `test-runner-test.lisp` が `%ensure-system-loaded`・`%rows-purge-ghost-suites` 等を直接呼ぶ |
| `src/macroexpand-core.lisp` | 439/469 (93.6%) | `lisp-macroexpand-test.lisp` が `%parse-readtable-name` 等を直接呼ぶ |

いずれも「本番の呼び出し元はワーカー」だが「テストは `*-core` の関数をパッケージ修飾
シンボルで計測プロセス内から直接呼ぶ」ため、ワーカー経由の実行を待たずにロジック自体が
計測プロセスで実行されカバーされる。よって 5 ファイルとも親で実行に分類する。

**まとめ**: ワーカー専用として扱うべきなのは、実際に子プロセスの中でしか実行できない
コード（`src/worker/main.lisp` の起動・シグナル・監視ロジック）だけであり、
「ワーカーが最終的に呼ぶ処理を書いたファイル」という基準では分類できない
（`handlers.lisp`/`server.lisp`/`init-hook.lisp`/`*-core` 5 ファイルはこの基準だと
誤ってワーカー専用に分類されてしまう）。

## 定義系トップレベルフォームの影響

`defpackage`・`in-package`・`declaim`・`defvar`・`defparameter`・`defclass`・
`define-condition`・`defstruct` は、そのファイルが計測プロセス内でコンパイルされる限り
**実行済みとして計上されない**（`scripts/coverage.ros` の `run-full`/`run-smoke` docstring
より）。`defun` の外側フォーム自身はロード時に関数を定義する副作用として実行済み扱いに
なる（Task 3 の検算で確認済み）が、上記 8 種はそうならない。src 全体で約 323 フォーム、
全 23,915 式の約 1.35% にあたり、ファイルが小さいほど比率として支配的になる。

これは「計測パイプラインが壊れている」ことを意味しない。**この分類表のすべての行の
数字はこの影響を差し引いてから読む必要がある**、というのがこの節を置く理由である。

### 具体例1: 影響がほぼ全てを占める行（`src/core.lisp`・`src/state.lisp`）

`src/core.lisp`（2/6, 33.3%）は6式のうち `defpackage`・`in-package`・`defparameter
+server-version+`・`declaim (ftype ...)` の4式が定義系フォームで計上対象外になり、
残る2式（`defun version` の外側フォームと、その本体である `+server-version+` という
1式）は `version` が呼ばれるたびに実行されてカバー済みになる。**関数の実体は 2/2 =
100% カバーされている**。33.3% という数字だけを見て「未テスト」と判断するのは誤りで、
分母 6 のうち 4 が最初から計上不可能な定義系フォームであることが原因である。

`src/state.lisp`（2/6, 33.3%）も同型: `defpackage`・`in-package`・`defclass
server-state`・`defvar *current-session-id*` の4式が定義系フォームで計上対象外、
残る2式（`defun make-state` の外側フォームと本体 `(make-instance 'server-state)`）は
テストで呼ばれておりカバー済み。

`src/tools/all.lisp`（0/2, 0.0%）はさらに極端で、ファイルの中身が `defpackage` と
`in-package` の2式のみ。ファイル自身のコメントが「This package intentionally has no
code」と明記する通りロード専用モジュールであり、0/2 は定義系フォーム2つがそのまま
計上対象外になった結果そのもので、テスト不足の兆候ではない。

`src/project-scaffold-templates.lisp`（0/11, 0.0%）も同様: `defpackage`・
`in-package` と、テンプレート文字列を保持する9個の `defparameter`
（`*asd-template-rove*` ほか）で計11式、全式が定義系フォーム。ロジックを一切含まない
静的データファイルであり、0/11 は未テストではなく計上対象がそもそも無いことを示す。

### 具体例2: 影響で説明できない行（`src/system-loader.lisp`）

`src/system-loader.lisp`（1/38, 2.6%）は、これと混同してはならない対照例である。
38式のうち定義系（`defpackage`・`in-package`）は2式のみで、残り36式は
`define-tool` マクロが展開する `load-system` ツール本体（引数検証、
`with-proxy-dispatch` によるワーカーへのRPC呼び出し）である。定義系フォームの影響を
すべて差し引いても 1/36 しか実行されておらず、これは定義系フォームの効果ではなく、
このツールのラッパー本体を実プロセス経由のワーカーに接続した状態で通す統合テストが
薄いことによる**正味のテスト不足**である。Task 3 の 0% 検算の結論（下記）とも整合する:
sb-cover は「フォームが存在するだけ」では ok にせず「実行されたか」を見ているので、
定義系でない36式のうち35式が未実行なら、それはそのまま実行されていないということ。

## `src/lisp-read-file.lisp` の計測アーティファクト（自己再ロードによる計装解除）

`src/lisp-read-file.lisp` は 55/1078 (5.1%)、分岐 10/148 (6.8%) と、`src/tools/all.lisp`
（ロード専用モジュール）や `src/project-scaffold-templates.lisp`（静的データファイル）を
除けば表全体で最も低い部類に入る。**この数字はテストが薄いことを意味しない。**
`tests/lisp-read-file-test.lisp` はこのファイルに対して最も手厚いテストの一つ
（`deftest` 約60個）であり、collapsed/rawモード、name_pattern/content_pattern の
組み合わせ、offset/limitの境界値とページネーション、コメントコンテキスト、メソッド
修飾子、パッケージローカルニックネーム、カスタムreadtable（自動検出含む）、`#.`
read-eval無効化のセキュリティテスト、不正な正規表現の検証、空パスの検証、括弧
不整合時の親切なエラー、バッククォート/カンマのレンダリングとラウンドトリップまで、
ほぼ全機能を網羅している。

**原因**: `tests/lisp-read-file-test.lisp` の `source-pprint-dispatch-is-rebuilt-on-reload`
（958行目付近）が、テストスイート実行の**途中**で

```lisp
(load (asdf:system-relative-pathname "cl-mcp" "src/lisp-read-file.lisp"))
```

を実行している。`*SOURCE-PPRINT-DISPATCH*` が `DEFVAR` ではなく `DEFPARAMETER` で
あることを検証するには実際のファイル再ロードが要る、というのがテスト自身のコメントが
説明する理由である（`rove cl-mcp.asd` は `ASDF:OPERATE` のネスト内で `:force t` を
許さないため、`(asdf:load-system ... :force t)` ではなく生の `LOAD` を使わざるを得ない、
という制約も明記されている）。`tests/` 全体を `\(load\s+\(asdf:system-relative-pathname`
で検索した結果、測定対象の `src/*.lisp` を実行中に生 `LOAD` で再ロードしているのはこの
1箇所のみ（他に `\(load\s` でヒットする2件は `/tmp` の一時ファイルや合成faslのロードで、
測定対象外）。

`scripts/coverage.ros` の `run-full` はテスト実行**前**に計装を明示的にOFFへ戻す
（`cl-mcp` を計装ありでコンパイルした後、`(proclaim '(optimize (sb-cover:store-coverage-data 0)))`
を実行してから `cl-mcp/tests` をロードし `rove:run` する）。したがって上記の `LOAD` は
テスト実行フェーズ、すなわち計装が既にOFFの区間で走る。この再ロードにより
`src/lisp-read-file.lisp` の全関数（`lisp-read-file` 本体、`%format-lisp-form`、
`%collapse-def-form`、`%form->string`、両方のpprintディスパッチテーブル構築など）が
非計装のコードに置き換わり、**それ以降どのテストが何回それらを呼んでも sb-cover には
一切記録されなくなる**。55/1078 という数字は、この `LOAD` が走るまでに実行された分だけが
残った結果である。

**結論**: `docs/coverage-summary.md` の 5.1% を根拠に「`lisp-read-file` がほとんど
テストされていない」と読んではならない。実態は約60個の `deftest` が機能のほぼ全体を
網羅している。低い数字は計測パイプライン側の見落とし（アーティファクト）であり、
正味のテスト不足を示す `src/system-loader.lisp`（1/38, 2.6%。上記「定義系トップレベル
フォームの影響」参照）とは性質が異なるので混同しないこと。

対策候補（本タスクの範囲外、記録のみ）: `source-pprint-dispatch-is-rebuilt-on-reload`
の `LOAD` 呼び出しの前後で `(proclaim '(optimize sb-cover:store-coverage-data))` を
保存・復元する、あるいは `scripts/coverage.ros` 側でこのテストだけ別プロセスに切り出す。

## `sb-cover` のソース位置警告があるファイル

以下の6ファイルはレポート生成時に `Error finding source location for source path ...`
という警告を出す: `src/frame-inspector.lisp`・`src/inspect.lisp`・
`src/project-scaffold-core.lisp`・`src/test-runner-core.lisp`・
`src/tools/define-tool.lisp`・`src/utils/lenient-read.lisp`。

`docs/coverage-summary.md` の集計値（式ok/式all/分岐ok/分岐all）は `sb-cover::report-file`
の返り値をそのまま使っており、HTMLへの行単位描画とは別経路で計算されるため、この警告は
**集計値そのものの正しさには影響しない**（実際この6ファイルの集計値はいずれも70%台後半〜
98%台の妥当な値になっている）。影響を受けるのは `coverage/html/` 以下でどの行が赤いかを
目視確認する用途であり、この6ファイルに限っては行単位のHTML着色を根拠に「このファイルの
どこが未テストか」を特定する際の信頼性が他のファイルより低い。集計%だけを見る分には
問題ない。

## ファイル別

| ファイル | 分類 | 理由 |
|---|---|---|
| src/asdf-tools.lisp | 親で実行 | 親プロセス内で動く |
| src/clgrep.lisp | 親で実行 | 親プロセス内で動く |
| src/clhs.lisp | 親で実行 | 親プロセス内で動く |
| src/code-core.lisp | 親で実行 | 本番はワーカー内の code-find/code-describe/code-find-references 実処理だが、code-test.lisp が %offset->line・%format-xref-caller を計測プロセス内から直接呼び出しており、数値(657/803, 81.8%)は正しく出る。詳細は「src/worker/ と *-core の判定根拠」参照 |
| src/code.lisp | 親で実行 | 親プロセス内で動く。define-tool ラッパー本体で、実処理は code-core.lisp に分離されている |
| src/core.lisp | 親で実行 | 6式中4式(defpackage/in-package/defparameter/declaim)が定義系トップレベルフォームで計上対象外。残り2式(defun version とその本体)は呼ばれる度にカバーされ実質100%。33.3%は未テストではない。詳細は「定義系トップレベルフォームの影響」参照 |
| src/cst.lisp | 親で実行 | 親プロセス内で動く |
| src/frame-inspector.lisp | 親で実行 | 親プロセス内で動く。sb-cover のソース位置警告対象ファイルの1つ(集計値は妥当、行単位のHTML着色のみ信頼性が低い) |
| src/fs.lisp | 親で実行 | 親プロセス内で動く |
| src/http.lisp | 親で実行 | 親プロセス内で動く |
| src/inspect.lisp | 親で実行 | 親プロセス内で動く。sb-cover のソース位置警告対象ファイルの1つ(集計値は妥当、行単位のHTML着色のみ信頼性が低い) |
| src/lisp-edit-form-core.lisp | 親で実行 | 親プロセス内で動く |
| src/lisp-edit-form.lisp | 親で実行 | 親プロセス内で動く |
| src/lisp-macroexpand.lisp | 親で実行 | 親プロセス内で動く |
| src/lisp-patch-form.lisp | 親で実行 | 親プロセス内で動く |
| src/lisp-read-file.lisp | 親で実行 | 親プロセス内で動くが、55/1078 (5.1%) という数字は計測アーティファクト。`tests/lisp-read-file-test.lisp` の `source-pprint-dispatch-is-rebuilt-on-reload` が計装OFF区間で自身をLOADし直すため、以後の実行が記録されない。実際のテストは約60 deftestと厚い。詳細は「`src/lisp-read-file.lisp` の計測アーティファクト」参照 |
| src/log.lisp | 親で実行 | 親プロセス内で動く |
| src/macroexpand-core.lisp | 親で実行 | 本番はワーカー内の lisp-macroexpand 実処理だが、lisp-macroexpand-test.lisp が %parse-readtable-name 等を計測プロセス内から直接呼び出しており、数値(439/469, 93.6%)は正しく出る。詳細は「src/worker/ と *-core の判定根拠」参照 |
| src/object-registry.lisp | 親で実行 | 親プロセス内で動く |
| src/package-context.lisp | 親で実行 | 親プロセス内で動く |
| src/parinfer.lisp | 親で実行 | 親プロセス内で動く |
| src/pool.lisp | 親で実行 | 親プロセス内で動く。ワーカーの生成・監視自体は親プロセスの責務 |
| src/project-root.lisp | 親で実行 | 親プロセス内で動く |
| src/project-scaffold-core.lisp | 親で実行 | 親プロセス内で動く。sb-cover のソース位置警告対象ファイルの1つ(集計値は妥当、行単位のHTML着色のみ信頼性が低い) |
| src/project-scaffold.lisp | 親で実行 | 親プロセス内で動く |
| src/project-scaffold-templates.lisp | 親で実行 | 11式全て(defpackage/in-package/defparameter×9)が定義系トップレベルフォーム。テンプレート文字列を保持するだけのデータファイルでロジックを含まないため、0/11は未テストの兆候ではない。詳細は「定義系トップレベルフォームの影響」参照 |
| src/protocol.lisp | 親で実行 | 親プロセス内で動く |
| src/proxy.lisp | 親で実行 | 親プロセス内で動く |
| src/repl-core.lisp | 親で実行 | 本番はワーカー内の repl-eval 実処理だが、repl-test.lisp が %repl-eval-with-timeout を計測プロセス内から直接呼び出しており、数値(410/478, 85.8%)は正しく出る。詳細は「src/worker/ と *-core の判定根拠」参照 |
| src/repl.lisp | 親で実行 | 親プロセス内で動く |
| src/run.lisp | 親で実行 | 親プロセス内で動く |
| src/state.lisp | 親で実行 | 6式中4式(defpackage/in-package/defclass/defvar)が定義系トップレベルフォームで計上対象外。残り2式(defun make-state とその本体)はテストで呼ばれカバー済みで実質100%。33.3%は未テストではない。詳細は「定義系トップレベルフォームの影響」参照 |
| src/system-loader-core.lisp | 親で実行 | 本番はワーカー内の load-system 実処理だが、system-loader-test.lisp が %redefinition-warning-p・%decide-suppress-redefinition 等を計測プロセス内から直接呼び出しており、数値(382/464, 82.3%)は正しく出る。詳細は「src/worker/ と *-core の判定根拠」参照 |
| src/system-loader.lisp | 親で実行 | 38式中定義系はdefpackage/in-packageの2式のみ。残り36式(define-toolマクロが展開するload-systemツール本体)のうち35式が未実行で、定義系トップレベルフォームの影響では説明できない正味のテスト不足。詳細は「定義系トップレベルフォームの影響」参照 |
| src/tcp.lisp | 親で実行 | 親プロセス内で動く |
| src/test-runner-core.lisp | 親で実行 | 本番はワーカー内の run-tests 実処理だが、test-runner-test.lisp が %ensure-system-loaded・%rove-purge-ghost-suites 等を計測プロセス内から直接呼び出しており、数値(1587/2258, 70.3%)は正しく出る。sb-cover のソース位置警告対象ファイルでもある(集計値は妥当)。詳細は「src/worker/ と *-core の判定根拠」参照 |
| src/test-runner.lisp | 親で実行 | 親プロセス内で動く |
| src/tools/all.lisp | 親で実行 | 2式全て(defpackage/in-package)が定義系トップレベルフォーム。ファイル自身のコメントが「意図的にコードを持たない」ロード専用モジュールと明記しており、0/2は未テストではなく計上対象がそもそも無いことを示す。詳細は「定義系トップレベルフォームの影響」参照 |
| src/tools/define-tool.lisp | 親で実行 | 親プロセス内で動く。sb-cover のソース位置警告対象ファイルの1つ(集計値は妥当、行単位のHTML着色のみ信頼性が低い) |
| src/tools/helpers.lisp | 親で実行 | 親プロセス内で動く |
| src/tools/pool-kill-worker.lisp | 親で実行 | 親プロセス内で動く |
| src/tools/pool-status.lisp | 親で実行 | 親プロセス内で動く |
| src/tools/registry.lisp | 親で実行 | 親プロセス内で動く |
| src/tools/response-builders.lisp | 親で実行 | 親プロセス内で動く |
| src/utils/clgrep.lisp | 親で実行 | 親プロセス内で動く |
| src/utils/hash.lisp | 親で実行 | 親プロセス内で動く |
| src/utils/lenient-read.lisp | 親で実行 | 親プロセス内で動く。sb-cover のソース位置警告対象ファイルの1つ(集計値は妥当、行単位のHTML着色のみ信頼性が低い) |
| src/utils/paths.lisp | 親で実行 | 親プロセス内で動く |
| src/utils/printing.lisp | 親で実行 | 親プロセス内で動く |
| src/utils/random.lisp | 親で実行 | 親プロセス内で動く |
| src/utils/sanitize.lisp | 親で実行 | 親プロセス内で動く |
| src/utils/strings.lisp | 親で実行 | 親プロセス内で動く。scripts/coverage.ros --smoke の検算対象ファイル(既知の答え 18/21) |
| src/utils/system.lisp | 親で実行 | 親プロセス内で動く |
| src/validate.lisp | 親で実行 | 親プロセス内で動く |
| src/worker-client.lisp | 親で実行 | 親プロセス内で動く。ワーカー子プロセスを spawn・監視する側のコードであり、これ自体は親プロセスの責務 |
| src/worker/handlers.lisp | 親で実行 | 本番はワーカー子プロセス内で動くディスパッチ層だが、tests/worker-test.lisp の with-handler-server マクロが実プロセスを spawn せず bordeaux-threads のスレッドとして計測プロセス内で起動し実TCP接続で検証しているため、数値(301/377, 79.8%)は正しく出る。詳細は「src/worker/ と *-core の判定根拠」参照 |
| src/worker/init-hook.lisp | 親で実行 | 本番はワーカー内の非同期初期化ハンドラだが、tests/worker-init-hook-test.lisp:125-166 が bt:make-thread でサーバーを計測プロセス内のスレッドとして起動し、実TCPソケット越しに worker/init-start・worker/init-status という JSON-RPC メソッドを送って検証しており(handlers.lisp/server.lispと同じ機序で、関数を直接呼んでいるわけではない)、数値(202/223, 90.6%)は正しく出る。詳細は「src/worker/ と *-core の判定根拠」参照 |
| src/worker/main.lisp | ワーカー専用 | start・%try-start-swank・%install-signal-handlers・%start-parent-watchdogは実際にspawnされたワーカー子プロセス内でのみ実行される。tests/worker-test.lisp自身が「startを直接呼べない、start-accept-loop内でブロックするため」と明記しており、pool-startup-latency-test.lisp等が本物のSBCL子プロセスを起動して検証している。%output-handshake等の純粋関数のみ親プロセス内で直接テストされ、それが46/264(17.4%)の内訳。詳細は「src/worker/ と *-core の判定根拠」参照 |
| src/worker/server.lisp | 親で実行 | 本番はワーカー子プロセス内で動くTCPサーバーだが、with-handler-serverハーネス(bordeaux-threadsによるスレッド、実プロセスspawnなし)で計測プロセス内から直接駆動されており、数値(263/349, 75.4%)は正しく出る。詳細は「src/worker/ と *-core の判定根拠」参照 |

## 計測実行時に除外したテスト

なし。

Task 4 のフル実行では計測プロセス内で `cl-mcp/tests` 全体（3552件の `✓`）が走り、
テスト失敗は0件だった（`project-scaffold-test` 内の意図的に赤くなる `rove-red`
フィクスチャ ―― `test-op` が失敗を正しく報告するかを確認するためにそれ自体が失敗する
ことをアサートしている ―― を除く。これはテストの失敗ではなく検証対象そのもの）。
`pool-startup-latency-test` の2つのタイミングアサーション（`initialize-pool` が
warmup=0/2 でそれぞれ 1.0秒/2.0秒以内に返ることを確認するテスト）もいずれも成功し
（実測 0.000秒）、しきい値を緩める変更は一切行っていない。計測を通すために除外・
無効化したテストは無い。

## 0% 検出の検算記録

一度も呼ばれない関数を一時的に追加して確認した結果（設計仕様 §10 検算 2）:

- 追加した関数: `src/utils/strings.lisp` に `coverage-probe-never-called`（`if`/
  `evenp`/2つの`format`呼び出しを含み、一度も呼ばれない）
- 観測値: `expected (21 21 4 4)` に対して `actual (22 32 4 6)`
  （式ok +1: 21→22、式all +11: 21→32、分岐ok ±0: 4→4、分岐all +2: 4→6）
- HTML で目視確認した内訳: 式ok の +1 は新しい `defun` トップレベルフォーム自身
  （ロード時に関数を定義する副作用として実行済み扱いになる）に対応し、関数の**本体**
  （`if`/`evenp`/両方の`format`呼び出し）は `state-2`（Not executed）/`state-10`
  （Neither branch taken）としてすべて未実行のまま記録されていた。分岐カバレッジは
  `branch-ok` が不変のまま `branch-all` のみ+2で、`(evenp x)` の分岐は「どちらも
  通っていない」と正しく記録された。
- 結論: sb-cover は「フォームが存在するだけ」では ok にせず「実行されたか」を見て
  いる（ok と all が同率で増えていない。分岐カバレッジは想定通り不変）。

**重要な留保事項**: 上記の具体的な数値（`expected (21 21 4 4)` / `actual
(22 32 4 6)`）は、後に上書きされた `21/21` というベースラインの下で、かつ**ウォーム
キャッシュから**測定されたものである。現在のコールドキャッシュでの正しいベースラインは
`18/21`（`defpackage`/`in-package`/`declaim` の3式が定義系トップレベルフォームとして
最初から計上対象外という、上と同じ効果によるもの）である。**結論（sb-cover は存在では
なく実行を見ている）自体は揺るがないが、上記の絶対値はウォームキャッシュという条件に
依存した値であり、コールドキャッシュで再検算すれば数字自体は変わる**という点をセットで
記録しておく。
