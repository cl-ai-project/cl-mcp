# cl-spec 仕様取得・Property 検証アダプタ設計

- 日付: 2026-09-09
- ステータス: 設計承認済み(ブランチ `feat/cl-spec-adapter`)
- 対象 cl-spec revision: `8ab6ffb`(v0.2-draft, MVP vertical slice)
- 参照: `/home/wiz/.roswell/local-projects/cl-spec/docs/cl-spec-specification-v0.2-draft.md`
  §0, §14〜16, §27〜31, §38, §47〜49, §60, §72, §73
- 想定受益者: cl-mcp を利用する AI エージェント

## 1. 背景と問い

cl-spec は Common Lisp 向けの実行可能な意味論 IR と Property framework である。
MVP vertical slice として、正規化・検証・structured explain・introspection・
check-it generator backend・`defproperty`・seed / replay / shrinking を実装済み。
一方 **cl-mcp adapter は未実装**(cl-spec `AGENTS.md`、仕様書 §0.2 実装状況表)。

cl-spec 仕様書 §73.2「次の実証順」の 2 番目が、
「既存の `semantic-data`・`property-data`・runner を最小限の cl-mcp adapter へ
接続する。初回は副作用のない対象を選び、実行ホストで時間上限を設ける」である。
本設計はこれを実装する。

解く問いは 1 つ。**LLM が Common Lisp の関数を変更するとき、
「関連する契約を取得する → Property を実行する → 反例を確認する →
修正後に再検証する」というループを cl-mcp の tool として回せるようにする。**

### 1.1 今回の範囲

1. 対象 symbol について、登録された Spec・Property を発見する
2. 必要な Spec・Property の詳細を取得する
3. 指定した Property、または対象 symbol に直接関連する Property を実行する
4. 構造化された実行結果と反例を取得する
5. seed と実行設定を指定して Property を再実行する

### 1.2 今回の範囲外

Function checker (`check-function` は stub)、custom generator DSL
(`defgenerator` は stub)、instrumentation、state-machine testing。
`cl-spec:run-property` の `:timeout` 引数(未実装)。
保存反例を直接入力する再検査 API(cl-spec 側 §73 D3 が未決定)。

**未実装の cl-spec API に依存して完成扱いにしない。**

## 2. 現状調査

### 2.1 cl-spec 側で実際に使える API

公開 symbol の存在ではなくソースと実機で確認した(2026-09-09、新規 SBCL プロセス)。

| API | 確認した挙動 |
|---|---|
| `semantic-data` (symbol &key registry) | `(:symbol :package :spec :function-spec :property :properties-about)` の固定キー plist。**登録名の routing table であり本文・signature・methods は返さない**。未登録 symbol でも同じキー集合を返し signal しない |
| `spec-data` (designator &key registry) | `(:name :kind <node固有> :source-form :source-location)`、子があれば `:children`。未登録名は `unknown-spec` を signal |
| `property-data` (designator &key registry) | `(:name :kind :targets :tags :documentation :trials :arguments :body :source-form :source-location :metadata)`。`:arguments` は `(:variable <sym> :spec <spec-data plist>)` の list。未登録名は `unknown-property` |
| `properties-for` (target &optional registry) | `:about` 逆索引。sorted |
| `run-property` (designator &key profile seed options registry) | `property-result` を返す |
| `replay-property` (designator seed &key profile options registry) | seed は整数または `property-result`。`run-property` に委譲 |
| `property-result-{status,property,trials,seed,profile,counterexample,shrunk-counterexample,condition,elapsed}` | 実測: status `:PASSED` / `:FAILED`。counterexample は `(A 68 B 85)` の名前付き plist |
| `property-trials` / `backend-default-trials` | 予算導出に必要な素材。**解決済み予算を返す API は無い** |
| `*generator-backend*` / `*registry*` | special |

実測ログ(抜粋):

```
PASS   status=:PASSED trials=100 seed=3827825384193791716 profile=:NORMAL
FAIL   status=:FAILED trials=1   seed=3963993791726803706 profile=:NORMAL
       ce=(A 68 B 85) sce=(A 0 B 0) cond=NIL
REPLAY status=:FAILED trials=1   seed=3963993791726803706 ce=(A 68 B 85) sce=(A 0 B 0)
```

### 2.2 stub である API(利用しない)

`describe-spec` / `describe-property` / `defspec-function` /
`check-function` / `defgenerator` は `not-implemented` を signal する。
`function-spec-data` に相当する projection API は**存在しない**。

### 2.3 cl-spec 側の制約で判明した事実

- **seed は最大 `(expt 2 62)` 未満**(`src/utils/random.lisp` `+seed-limit+`)。
  実測値 `3963993791726803706` は JSON の安全整数 2^53 を超える。
  JSON number で往復させると JavaScript クライアントで丸められ、
  **再現できない seed を再現できると誤報告する**。
- `check-it` backend の `run-generated-test` が返す status は
  `:passed` / `:failed` / `:error` の 3 つのみ。
  `property-result` の docstring が挙げる `:skipped` / `:pending` は
  現 backend からは出ない。仕様書 §14 も「すべての候補を現在の backend が
  返すとは限らない」と明記している。
- `run-property` は `options` を `(list* :trials trials :registry registry options)`
  と組むため、呼び出し側 options の `:trials` は先頭の値に隠れる。
  check-it backend は `:trials` と `:registry` しか読まない。
- 整数 seed 対応は SBCL のみ(`unsupported-seed`)。cl-mcp は SBCL 専用なので
  実害はないが、環境情報として報告する。

### 2.4 cl-mcp 側で再利用する仕組み

| 仕組み | 場所 | 用途 |
|---|---|---|
| `define-tool` | `src/tools/define-tool.lisp` | descriptor + handler + 登録の一括生成 |
| `with-proxy-dispatch` | `src/proxy.lisp` | worker routing と inline fallback |
| `call-with-deadline-thread` | `src/utils/deadline.lisp` | 時間上限。leaked thread の記録まで面倒を見る |
| worker 退役ゲート | `src/worker/server.lisp` `%retire-if-carrying-leaked-threads` | 停止不能 thread を抱えた worker は次の要求前に退役 |
| `register-object` / `generate-result-preview` | `src/object-registry.lisp`, `src/inspect.lisp` | 巨大値を `object_id` 経由で `inspect-object` に委譲 |
| `json-bool` | `src/tools/helpers.lisp` | 汎用 boolean → 厳密 JSON boolean |
| `sanitize-for-json` | `src/utils/sanitize.lisp` | 制御文字除去 |
| `code-describe-symbol` | `src/code-core.lisp` | §28 の signature / source location join |

**制約(過去の計測より)**: MCP クライアントは `content[].text` しか描画しない。
兄弟 JSON フィールドに置いた情報は人間にもモデルにも見えない。
したがって判断に必要な情報は必ず content text にも出す。

## 3. アーキテクチャ

```
MCP client
  │  tools/call  spec-symbol / spec-describe / spec-check
  ▼
parent  src/tools/spec-tools.lisp          define-tool ×3 + with-proxy-dispatch
  │  worker/spec-symbol | worker/spec-describe | worker/spec-check
  ▼
worker  src/worker/handlers.lisp           3 メソッド追加
  ├─ src/spec-adapter-core.lisp            cl-spec 遅延解決 / symbol 解決 /
  │                                        値の外部表現 / digest / 実行 + deadline
  └─ src/tools/spec-response-builders.lisp plist → hash-table + content text
        │
        ▼ 関数呼び出しのみ
      cl-spec:semantic-data / spec-data / property-data / properties-for
      cl-spec:run-property / property-result-* / *registry* / *generator-backend*
```

### 3.1 責務分離

| 側 | 担当 |
|---|---|
| cl-spec | Spec・Property・registry・実行 backend |
| cl-mcp | tool 公開、symbol 解決、ソース/実行時情報との統合、外部表現への変換、worker での実行管理と時間上限 |

- registry の内部索引(`properties-by-target` 等)を cl-mcp に複製しない。
  入口は常に `cl-spec:semantic-data`、本文は `spec-data` / `property-data`。
- **cl-spec core に cl-mcp や check-it への依存を追加しない。**
- **cl-mcp.asd に cl-spec を追加しない。** アダプタは呼び出しごとに
  `find-package "CL-SPEC"` + `find-symbol`(固定リテラル名のみ)で API を解決する。
  これは `src/proxy.lisp` の `%resolve` と `src/code-core.lisp` の
  `%ensure-sb-introspect` が既に採っているパターンである。
  cl-spec を使わないプロジェクトでも既存 tool は無変更で動く。

### 3.2 実行場所

registry は `load-system` を実行した worker image に載る。
したがって**発見・取得・実行のすべてを worker で行う**。
これにより「定義のロードと Property 実行が同じ worker / session」が
構造的に保証され、`load-system` で再ロードした変更が次の `spec-check` に反映される。
プール無効時(`MCP_NO_WORKER_POOL=1`)は既存 tool と同じ inline fallback。

registry は special の値を**呼び出しスレッドで読んで明示的に引数へ渡す**。
`call-with-deadline-thread` が起こす新スレッドは dynamic binding を継承しないため、
`*registry*` を rebind したテストや将来の分離 registry でも正しく動く。

### 3.3 新規ファイル

`cl-mcp.asd` は package-inferred-system なので編集不要。
登録先は `src/tools/all.lisp`(load 副作用)、`main.lisp`(export)、
`tests.lisp`(テストスイート)。

| ファイル | 責務 |
|---|---|
| `src/spec-adapter-core.lisp` | cl-spec API 遅延解決、symbol 解決、値の外部表現、digest |
| `src/spec-adapter-report.lisp` | 発見・取得・実行の 3 操作を plist で組み立てる。deadline と予算配分もここ |
| `src/tools/spec-response-builders.lisp` | plist → hash-table、content text 生成 |
| `src/tools/spec-entry.lisp` | API 解決 → report → hash-table の入口。tool と worker handler の共通部 |
| `src/tools/spec-tools.lisp` | `define-tool` ×3 |
| `tests/spec-adapter-core-test.lisp` | 単体(cl-spec 非依存) |
| `tests/spec-response-builders-test.lisp` | 応答形状 |
| `tests/spec-tools-test.lisp` | tool 経由(cl-spec があれば実物、無ければ skip) |

### 3.4 cl-spec API の遅延解決

```lisp
(defstruct cl-spec-api
  semantic-data spec-data property-data properties-for
  run-property property-result-readers registry backend ...)
```

`resolve-cl-spec-api` が `find-package` → `find-symbol` → `fdefinition` /
`symbol-value` を 1 回で行い、欠けているものがあれば NIL と欠落理由を返す。
**テストはこの構造体に lambda を差し込むことで、cl-spec が無い環境でも
実行系の全分岐を検証できる。**

## 4. 4 つの状態の区別

推測で埋めない。ゼロ・false・空の結果と、取得できない値を区別する。

| 状態 | 判定 | `status` | content text の要点 |
|---|---|---|---|
| cl-spec 未ロード | `CL-SPEC` package 不在 / 必須 symbol 欠落 | `cl-spec-not-loaded` | `load-system` で `cl-spec/check-it`(実行込み)か `cl-spec`(取得のみ)を先に。**対象に契約が無い証拠ではない** |
| backend 未ロード | `cl-spec:*generator-backend*` が NIL | `backend-not-loaded` | 取得系は動く。実行は不可。**成功ではない** |
| 定義未登録 | `semantic-data` が全て NIL / 空 | `not-registered` | registry に登録が無い。未ロードの可能性があり、契約不要を意味しない |
| 機能未対応 | 例: `function-spec-data` が cl-spec に無い | `unsupported` | 欠けている cl-spec API 名を明示 |

全応答に `environment` を付ける。

```json
{"cl_spec_loaded": true,
 "cl_spec_version": "0.1.0",
 "cl_spec_system_directory": "/home/wiz/.roswell/local-projects/cl-spec/",
 "generator_backend": "CL-SPEC/SRC/BACKENDS/CHECK-IT:CHECK-IT-BACKEND",
 "lisp": "SBCL 2.4.x",
 "registry": "#<HASH-TABLE-REGISTRY {1004A2B3}>"}
```

`cl_spec_version` は `(asdf:component-version (asdf:find-system "cl-spec" nil))`。
取得できない場合は null とし、推測しない。

## 5. symbol 解決(reader 評価・動的 intern を使わない)

tool 入力の symbol 文字列を解決するために任意の reader 評価や
`intern` を使わない(§72.6)。

```
"PKG:SYM"  → package PKG の external symbol のみ許可
"PKG::SYM" → internal も許可
"SYM"      → 引数 package、無ければ CL-USER
```

- package 名・symbol 名とも **exact → `string-upcase` の順**で `find-package` /
  `find-symbol` を試す。exact を先にするのは、小文字名で作られた package や
  symbol を上書きしないため。
- `find-symbol` のみを使い、`intern` は決して呼ばない。
  存在しない名前を解決しようとしても image に symbol が増えない。
- 失敗は `unresolved-symbol` + 理由
  (`package-not-found` / `symbol-not-found` / `not-external` / `malformed`)。
- 応答の symbol は常に `{"package": "PROBE", "name": "ADD", "qualified": "PROBE::ADD"}`。
  **package の異なる同名 symbol を取り違えない**ための最小形式。
- 表示文字列を reader 入力として評価し直すことはない。

## 6. tool 1: `spec-symbol` — 発見

対象 symbol について、registry が知っていることと cl-mcp が知っていることを
1 回で返す(§27〜28 の `describe_symbol` join)。

### 6.1 引数

| 引数 | 型 | 既定 | 意味 |
|---|---|---|---|
| `symbol` | string(必須) | — | 対象 symbol |
| `package` | string | `COMMON-LISP-USER` | 未修飾名のときのみ使用 |
| `include_runtime` | boolean | true | §28 の signature / source location join |

### 6.2 応答

```json
{"status": "ok",
 "symbol": {"package":"PROBE","name":"ADD","qualified":"PROBE::ADD"},
 "runtime": {"type":"function","arglist":"(A B)","documentation":null,
             "source_file":"probe.lisp","source_line":42},
 "runtime_unavailable_reason": null,
 "registry": {"spec": null, "function_spec": null, "property": null,
              "properties_about": [{"package":"PROBE","name":"ADD-COMMUTES"}]},
 "properties": [
   {"name": {"package":"PROBE","name":"ADD-COMMUTES"},
    "kind": "commutativity",
    "tags": ["math"],
    "targets": [{"package":"PROBE","name":"ADD"}],
    "documentation": "Addition commutes.",
    "arguments": [{"variable": {"package":"PROBE","name":"A"},
                   "spec_kind": "reference",
                   "spec_name": null,
                   "spec_target": {"package":"PROBE","name":"SMALL-INT"}}],
    "trials_table": "(:NORMAL 100)",
    "shrink_enabled": true,
    "source_location": {"file":"probe.lisp","package":"PROBE"},
    "definition_digest": "a41f9c2b7d0e5518",
    "body_forms": 1,
    "body_omitted": true,
    "detail_via": "spec-describe kind=property"}],
 "environment": {...},
 "notes": ["properties_about lists direct (:about ...) registrations only"]}
```

`runtime` は worker 内で `code-core:code-describe-symbol` を呼ぶ。
失敗時は `runtime: null` + `runtime_unavailable_reason` に理由を書く
(**null と「取得しなかった」を区別する**)。

`code-describe-symbol` は文字列引数を reader で読み戻すため、**エスケープが
必要な名前(コロン・空白・縦棒を含む、あるいは小文字を含む symbol 名や
package 名)の場合は join を行わず理由を返す**。無理に読ませると別の symbol
に解決し、その signature をこの symbol の名前で報告しかねない。
join は registry の事実に対する付加情報であって同一性ではない。
symbol の解決自体は `find-symbol` の完全一致なので、この種の名前でも
`spec-symbol` は正しく解決し、`runtime` だけが欠ける。

Property 本文と source-form はここでは返さない(`body_omitted: true`)。
概要から詳細へ辿れる形にし、省略したことを明示する(§72.6)。

## 7. tool 2: `spec-describe` — 詳細取得

### 7.1 引数

| 引数 | 型 | 既定 | 意味 |
|---|---|---|---|
| `kind` | string(必須) | — | `property` / `spec` / `function-spec` |
| `name` | string(必須) | — | 登録名 |
| `package` | string | `COMMON-LISP-USER` | 未修飾名のときのみ使用 |
| `max_chars` | integer | 8000 | 本文 printed 表現の上限 |

### 7.2 挙動

- `property` → `cl-spec:property-data`。`body` と `source_form` を printed text
  で返す。上限超過で `truncated: true` + `omitted_chars: N`。
  引数の spec は `spec-data` の木をそのまま射影(子ノード込み)。
- `spec` → `cl-spec:spec-data`。
- `function-spec` → **`unsupported`**。
  理由: cl-spec に `function-spec-data` に相当する projection API が無く、
  `defspec-function` は stub。公開 reader から cl-mcp 側で projection を
  組み立てることは、cl-spec の introspection 責務の複製になるので行わない。
  必要な cl-spec 側変更として最終報告に挙げる。

`unknown-spec` / `unknown-property` は `not-registered` に変換して返し、
condition を RPC エラーとして漏らさない。

## 8. tool 3: `spec-check` — 実行・再実行

### 8.1 引数

| 引数 | 型 | 既定 | 意味 |
|---|---|---|---|
| `property` | string | — | 明示指定。`symbol` と排他 |
| `symbol` | string | — | `:about` 逆索引で関連 Property を選択 |
| `package` | string | `COMMON-LISP-USER` | 未修飾名の解決用 |
| `profile` | string | `normal` | trial 予算の profile |
| `seed` | string または integer | — | 10 進。再実行用 |
| `expect_definition_digest` | string | — | 与えると定義変化を検出 |
| `timeout_seconds` | number | 60 | **呼び出し全体**の予算 |
| `max_value_chars` | integer | 2000 | 反例 1 値あたりの printed 上限 |

`property` と `symbol` の同時指定、どちらも無しは引数エラー。
**この検査は cl-spec の可用性判定より前に行う。** 引数が誤っていることは
cl-spec の状態と無関係であり、先に「cl-spec 未ロード」を返すと呼び出し側を
誤った修正へ誘導する(`seed` の検査を API 解決より前に置くのと同じ理由)。

### 8.2 seed を文字列で扱う

cl-spec の seed は最大 2^62 未満で、JSON の安全整数 2^53 を超える。
**seed は常に 10 進文字列で返す。JSON number としては返さない。**
入力も文字列のみを受け付け、`parse-integer` で解釈する
(reader は使わない)。JSON number を受理しないのは仕様であって不便ではない。
呼び出し側が数値として seed を持っている時点でその値は既に丸められており、
受理すれば「再現できない seed を再現できる」と報告することになる。
10 進数字のみでなければ引数エラー。

### 8.3 選択根拠

`mode` は 2 種類。`property` 引数を使ったときは `explicit`、
`symbol` 引数を使ったときは `about`。

`symbol` mode は `semantic-data` の `:properties-about` のみを使う。
対象 symbol 自身が登録済み Property でもある場合
(`semantic-data` の `:property` が非 NIL)、それは `:about` 関連とは
別の関係なので選択に含めず、`notes` に
「SYMBOL is itself a registered property; run it with property= instead」
を出す。**黙って実行対象を広げない。**

```json
"selection": {
  "mode": "about",
  "requested": {"symbol": {"package":"PROBE","name":"ADD"}},
  "selected": [{"package":"PROBE","name":"ADD-COMMUTES"}],
  "count": 1,
  "source": "cl-spec:semantic-data -> :properties-about (registry :about reverse index)",
  "coverage": "Direct (:about ...) registrations only. Callers, generic-function methods, macro users and shared mutable state are NOT analysed. This is not a change-impact analysis (cl-spec spec §31, §72.5)."}
```

`:about` による関連取得を完全な変更影響解析と説明しない。

### 8.4 status の語彙

**個別 Property**

| status | 意味 |
|---|---|
| `passed` | 生成・検査した範囲で反証されなかった |
| `failed` | 反例あり |
| `error` | Property body が condition を signal |
| `skipped` / `pending` | cl-spec が定義するが現 backend は返さない。来たらそのまま報告 |
| `timeout` | cl-mcp の deadline に到達 |
| `generator-error` | `generator-unavailable` / `no-generator-backend` |
| `backend-error` | その他の `cl-spec-error` |
| `not-run` | 全体予算切れ(`reason: budget-exhausted`) |

**呼び出し全体**

| status | 条件 |
|---|---|
| `no-properties` | 選択が 0 件 |
| `completed` | 全 Property が verdict に到達 |
| `incomplete` | timeout / error / not-run を含む |

**`verified`**: `(and (plusp count) (every passed))` のときだけ true。

0 件は必ず `no-properties` + `verified: false` とし、content text に
`0 properties selected -- this is NOT a successful verification.` を出す。
skip・timeout・generator error・backend error を Property 成功にまとめない。

### 8.5 時間上限

`timeout_seconds` は proxy が worker の deadline として読む値である
(`src/proxy.lisp` `%effective-rpc-timeout` / `%clamp-timeout-param`)。
per-property の値にすると、複数 Property の合計が proxy の待ち時間を超えて
**まだ働いている worker が kill される**。したがって
**`timeout_seconds` は呼び出し全体の予算**とする。

- worker 内で `call-with-deadline-thread` を使う(`run-tests` と同じ機構)。
- 複数 Property は逐次実行し、各 `run-property` には**残予算**を渡す。
- 残予算が尽きたら以降は `not-run` / `budget-exhausted`。
- thread を停止できなかった場合は `thread_leaked: true` を返し、
  既存の worker 退役ゲートがそのまま働く。復旧案内は `run-tests` の
  `make-timeout-result` と同じ文言(pool-kill-worker で worker を差し替える)。
- `cl-spec:run-property` に `:timeout` は渡さない(cl-spec 側未実装)。

§48 が将来区別するとしている「全体予算」と「trial 単位予算」のうち、
今回は全体予算のみを扱う。trial 単位予算は cl-spec 側 API が無いので未対応。

### 8.6 trial の実行数と予算を分離

```json
"trials": {"executed": 1,
           "budget": 100,
           "budget_source": "backend-default",
           "property_trials": null,
           "backend_default": 100,
           "budget_derivation": "derived by cl-mcp from cl-spec:property-trials and cl-spec:backend-default-trials; cl-spec does not expose the resolved budget"}
```

`property-result-trials` は「停止した試行番号」であって予算ではない。
予算は cl-mcp が公開 reader 2 つから導出し、**導出であることを明記する**。
cl-spec が解決済み予算を公開すればこの導出は不要になる(最終報告に記載)。

### 8.7 反例の外部表現

```json
"counterexample": [
  {"variable": {"package":"PROBE","name":"A"},
   "printed": "68",
   "printed_complete": true,
   "type": "(INTEGER 0 100)",
   "object_id": null}],
"shrunk_counterexample": [
  {"variable": {"package":"PROBE","name":"A"},
   "printed": "0", "printed_complete": true,
   "type": "(INTEGER 0 100)", "object_id": null}],
"shrink_enabled": true,
"shrink_note": "Backend-searched reduction. NOT a guaranteed global minimum (cl-spec spec §16, §72.4)."
```

- **すべての Lisp 値は printed 文字列で返す。JSON number は使わない。**
  整数・有理数の正確さを落とさないため。
- printing は `*print-circle*` t / `*print-readably*` nil /
  `*print-level*`・`*print-length*` nil で行い、`max_value_chars` を超えたら
  切り詰めて `printed_complete: false` を立てる。
  **`printed_complete: false` の値は復元可能な反例データではない**
  (表示用プレビューと復元形式を混同しない、§72.6)。
- 非プリミティブ値は `register-object` で `object_id` を付け、
  既存 `inspect-object` で深掘りできるようにする(同一 worker なので ID が有効)。
- `condition` は `{"type":"DIVISION-BY-ZERO","message":"...","object_id":N}`。
- `counterexample` が null になるのは status が `passed` のときのみ。
  `shrunk_counterexample` が null でも `shrink_enabled` で理由が読める。
- Property の predicate は単一の汎用 boolean を返すので多値は関与しない。
  Lisp の `NIL` はフィールドの型で意味が決まる
  (boolean は `json-bool` で厳密化、list は空配列、未取得は null + 理由フィールド)。

### 8.8 再現性

```json
"reproduce": {
  "seed": "3963993791726803706",
  "profile": "normal",
  "definition_digest": "a41f9c2b7d0e5518",
  "definition_match": true,
  "options": null,
  "call": {"tool":"spec-check","property":"PROBE::ADD-IS-WRONG",
           "seed":"3963993791726803706","profile":"normal",
           "expect_definition_digest":"a41f9c2b7d0e5518"},
  "scope": "Regenerates the trial sequence from SEED under the same definitions, backend, profile and image. It does NOT reproduce code revision, external I/O, time, or shared mutable state. This is NOT replay of a saved counterexample against a fixed implementation (cl-spec spec §15, §72.3)."}
```

**definition digest**

- FNV-1a 64bit(依存追加なし、16 桁 hex)。
- 対象は `property-data` の printed 表現に加え、引数 spec から**推移的に
  到達可能な名前付き spec の `spec-data`** を名前順に連結したもの。
  参照先 spec の変更も検出できる。循環は visited 集合で止める。
- printing は `*package*` を `KEYWORD` に束縛し(symbol が常に package 修飾
  される)、`*print-pretty*` nil / level・length nil / `*print-base*` 10 /
  `*print-case*` `:upcase` で決定的にする。
- `expect_definition_digest` 不一致なら `definition_match: false` を
  status と content text の両方に出す。
  **「元の実行を厳密に再現した」とは報告しない。**

**options**

tool 引数として公開しない。理由は §2.3 のとおり check-it backend が
`:trials` と `:registry` しか読まず、呼び出し側 options が実質無効なため。
公開しないことで「黙って元と異なる条件で再実行する」経路自体を作らない。
応答には `options: null` を明記する。

**replay-property を使わない理由**

`replay-property` は seed と profile を引き継いで `run-property` に委譲する
だけであり、cl-mcp は seed と profile を明示引数として往復させる。
整数 seed を渡す場合 `replay-property` は profile を引き継がないので、
どちらにせよ呼び出し側が profile を指定する必要がある。
**現在の `replay-property` は生成列の再実行であって、保存反例を修正後の
実装へ直接入力する操作ではない。** この区別を `scope` 文言で明示する。

## 9. content text

MCP クライアントは `content[].text` しか描画しない。判断に必要な情報は
必ずここに出す。

見出しは 4 種類にする。既存の `run-tests` の `✓ PASS` / `✗ FAIL` /
`⚠ NO TESTS RAN` に合わせつつ、**反証された run と完走できなかった run を
別の語で報告する**。1 語にまとめると timeout が反例のように読める。

| 見出し | 条件 |
|---|---|
| `✓ VERIFIED` | 1 件以上選択され、全件 passed |
| `✗ FAILED` | 1 件以上が failed |
| `⚠ NOT VERIFIED` | failed は無いが timeout / error / not-run がある |
| `⚠ NO PROPERTIES` | 選択が 0 件 |

`spec-check` の実出力(§10.3 の実証より、値は実行結果そのもの):

```
✗ FAILED
Selected 2 properties via cl-spec:semantic-data -> :properties-about (registry :about reverse index).
  Direct (:about ...) registrations only. Callers, generic-function methods, macro users and shared mutable state are NOT analysed. This is not a change impact analysis (cl-spec specification 31 and 72.5).

[1] SPEC-DEMO::CLAMP-RESPECTS-HIGH  failed
    trials: 2 executed of 100 budget (property-profile)
    counterexample:        VALUE = 62
    shrunk counterexample: VALUE = 51
      Backend-searched reduction. NOT a guaranteed global minimum, and the backend does not report whether shrinking completed, exhausted its budget or was interrupted (cl-spec specification 16 and 72.4).
    seed: 3013752598065164257   profile: normal
    definition_digest: aa4b67c3804d69b0

[2] SPEC-DEMO::CLAMP-RESPECTS-LOW  passed
    trials: 100 executed of 100 budget (backend-default)
    seed: 2441597211547797803   profile: normal
    definition_digest: 030c291c2298a4b5

verified: false   1 passed, 1 failed, 0 errored, 0 timed out, 0 not run
Replay: spec-check property=SPEC-DEMO::CLAMP-RESPECTS-HIGH seed=3013752598065164257 profile=normal expect_definition_digest=aa4b67c3804d69b0
Regenerates the trial sequence from this seed under the same definitions, backend, profile and image. ...
```

`Replay:` 行は**最初に passed でなかった結果**を指す。3 件中 3 件目が失敗した
とき、1 件目の seed を返しても再現する理由がない。

0 件の場合:

```
⚠ NO PROPERTIES  COMMON-LISP::CAR
Selected 0 properties via cl-spec:semantic-data -> :properties-about (registry :about reverse index).
0 properties selected -- this is NOT a successful verification. Nothing was executed, and a registry with no property registered about this symbol says nothing about whether it is correct.
verified: false
```

## 10. テスト計画

### 10.1 単体(cl-spec 非依存)

`cl-spec-api` 構造体に lambda を差し込むので、cl-spec が無くても
実行系の全分岐を検証できる。

1. **package の異なる同名 symbol** — `A::FOO` と `B::FOO` を作り、
   `spec-symbol` が取り違えないこと。未定義名の解決後に
   `find-symbol` が NIL のままであること(intern していない証拠)
2. **4 状態の区別** — 未ロード / backend 未 / 未登録 / 未対応
3. **0 件** — `no-properties` かつ `verified: false`、text に警告文
4. **成功に畳まない** — failed / error / timeout / generator-error / not-run
5. **seed 往復** — 2^62 級整数が文字列で欠損なく往復。整数入力も受理
6. **値の外部表現** — `printed_complete: false` の付与、`object_id` の付与
7. **digest** — 決定性、参照 spec 変更時に変化、`definition_match: false`
8. **deadline** — 停止しない thunk で `timeout` + `thread_leaked`。
   `forget-leaked-threads` で image を元に戻す

### 10.2 統合(cl-spec 実物)

`tests/integration-test.lisp` と同じ `process-json-line` 経由、
`*use-worker-pool*` nil。cl-spec が解決できない環境ではスキップ理由を
明示して skip する(cl-mcp のテストを cl-spec 必須にしない)。
仕様取得 → 失敗取得 → 再実行の一周。

### 10.3 実 tool 経由の実証

新規 SBCL プロセス、scratchpad の使い捨て fixture。
外部 I/O も共有可変状態も持たない小関数を対象にする。

1. `spec-symbol` で契約を発見
2. `spec-describe` で Property 本文を取得
3. `spec-check` で意図的失敗と反例を取得
4. 実装を修正して `fs-write-file` で保存、`load-system` で再ロード
5. 同一 seed で `spec-check` を再実行し、解消を確認
   (`definition_match: false` が出ることも確認 — 実装を直せば digest は
   変わらないが、Property を書き換えた場合は変わる)

fixture は scratchpad にのみ置き、cl-mcp / cl-spec のソースツリーにも
稼働中の MCP worker にも残さない。

### 10.4 既存機能の非破壊

- `mallet src/*.lisp src/*/*.lisp tests/*.lisp`
- 新規プロセスで `(asdf:compile-system :cl-mcp :force :all)`
- 新規プロセスで `rove cl-mcp.asd` 全スイート
- cl-spec を一切ロードしない状態で既存 tool が動くこと

## 11. §72 の到達範囲

| 要件 | 今回 | 備考 |
|---|---|---|
| LLM-01 検証結果と検証範囲 | 対応 | 0 件・timeout・error を成功に畳まない。選択根拠と網羅範囲を明示。予算は導出であることを明記 |
| LLM-02 契約の由来と変更 | **未対応** | registry が由来・レビュー状態・version を持たない。cl-spec 側 §73 D8 |
| LLM-03 再生成と反例の再検査 | 部分対応 | 生成列 replay と digest 不一致検出は対応。**保存反例の直接再検査は未対応**(cl-spec §73 D3 未決定) |
| LLM-04 状態・縮小・時間上限 | 部分対応 | 全体予算と leaked thread 時の worker 退役は対応。trial 単位予算・縮小の完了/予算切れ/中断の区別・失敗同一性は cl-spec 側 API が無く未対応 |
| LLM-05 変更影響と image の整合性 | 部分対応 | `:about` 直接関連のみと明示。同一 worker でのロードと実行は保証。registry 世代・cache invalidation は cl-spec §73 D6 |
| LLM-06 機械可読境界 | 部分対応 | package 区別・省略の明示・値の正確さ・reader 非使用は対応。**外部 JSON の schema version と capability API は未対応**(cl-spec §73 D7 未決定) |

## 12. cl-spec 側に必要な変更(報告のみ、今回は実装しない)

1. `function-spec-data` — `spec-data` / `property-data` と同形の projection。
   無いと cl-mcp が公開 reader から projection を組み立てることになり、
   introspection 責務の複製になる。
2. 解決済み trial 予算の公開 — `resolve-trials` は内部関数で、
   `property-result` にも予算が載らない。cl-mcp が
   `property-trials` + `backend-default-trials` から導出している。
3. `run-property` の `:timeout`(§48) — 今は実行ホスト側で扱っている。
4. 保存反例の直接再検査 API(§73 D3)。
5. 外部表現の schema version と capability API(§73 D7)。
