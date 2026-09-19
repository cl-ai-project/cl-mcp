# cl-spec アダプタ忠実性改善 — 設計

- 日付: 2026-09-19
- 対象: cl-mcp `src/spec-adapter-report.lisp`, `src/tools/spec-response-builders.lisp`
- 対象リビジョン: cl-mcp `8c45516` / cl-spec `4f149e1`（PR #33 `check-call` マージ済み）

## 1. 目的

cl-spec が既に構造化して提供している契約情報と実行証拠を、MCP 境界で
落とさず・推測し直さず・adapter 側の弱い近似で置き換えずに運ぶ。

役割分担を次のとおり固定する。

```
cl-spec = 契約の意味論と証拠の生産者
cl-mcp  = 安全・有界・後方互換な輸送路
LLM     = 何を読み何を直すか決める消費者
```

達成条件は、MCP の応答だけから次を区別できること。

```
target 実装のバグ / 契約のバグ / 入力生成の限界 / 未到達の契約分岐
state-post 違反 / target 呼び出し前の失敗 / target 呼び出し後の失敗
縮小が実行不能 / 検証が未完了
```

## 2. 中核ルール

すべてのフィールドに単一の優先順位を適用する。

```
versioned record (result-data / function-spec-data) の値
  > cl-spec の公開 legacy reader
  > adapter 側の導出
```

- adapter 導出が残る箇所は、コード中と `docs/tools.md` の両方に理由を書く。
- 既存の legacy パスは削除せず fallback に降格する。互換性のために残すもので
  あって、整理のために消すものではない。
- 同じ意味の値に対して矛盾しうる 2 つの公開フィールドを作らない。供給元だけ
  差し替え、公開フィールドは 1 つに保つ。
- 「core 優先」は **どの reader を呼ぶか** の規則であって、2 つの値を実行時に
  突き合わせて選ぶ規則ではない。`result-data` が読める revision では、
  **同じ semantic fact については** そのレコードだけを読み、legacy reader を
  呼ばない。両者が食い違いうる状況をそもそも作らない。

- **例外（補助 reader）**: versioned record が持たない補助情報を取りにいく
  legacy reader は使ってよい。ただしその情報で core record の意味を上書き・
  再分類してはならない。実測で確認した唯一の該当例は live condition object:

  ```
  failure reason        -> result-data が権威
  condition report      -> result-data が権威（:CONDITION-REPORT は文字列）
  live condition object -> property-result-condition から補助的に取得し
                           object_id を付ける（result-data には :condition キー
                           自体が無い。実測で確認）
  ```

  役割分担は `cl-spec record = 過去の証拠` / `live condition object =
  cl-mcp の inspection hook` であり、semantic authority を壊さない。

- **互換 alias**: 「意味の異なる重複フィールドを新設しない」が原則であって、
  既存の公開フィールドを互換 alias として残すことは禁じない。ただし alias は
  **core record を解析した同一の値から生成し**、両者が食い違うことが構造上
  不可能な形にする。

  ```
  parsed core record
       ├── core_result.data.failure_reason   (canonical)
       └── contract.failure_reason           (compatibility alias)
  ```

## 3. 可用性（availability）の四状態

`result-data` 由来の情報について、cl-mcp がそれを**取得できたか**を表す。

**`status` という名前は使わない。** cl-spec 自身が `:status`（`:passed` /
`:failed` / `:error` / `:skipped`）を持っており、同じ key 名にすると
「cl-spec が実行をどう分類したか」と「MCP がその情報を取得できたか」という
まったく別の概念が衝突する。前者は `status`、後者は `availability` とし、
設計書・JSON・コードで統一する。

| availability | 意味 |
|---|---|
| `collected` | cl-spec が実測値を返した |
| `not-collected` | cl-spec が明示的に `:NOT-COLLECTED` を返した |
| `absent` | record にその key が無い（**構文的事実のみ**。意味は §3.2.1 で field ごとに決める） |
| `unavailable` | この cl-spec に `result-data` 自体が無い。問うことができなかった |

### 3.1 形

`core_result` は「MCP 側のメタデータ」と「cl-spec のレコードそのもの」を
構造的に分ける。`data` は cl-spec の record の純粋な写像であり、MCP が足した
key は一つも入らない。

```json
"core_result": {
  "availability": "collected",
  "schema_supported": true,
  "schema_version": 1,
  "field_availability": {
    "failure_phase": "collected",
    "shrink_report": "not-collected",
    "case_report": "collected"
  },
  "unknown_keys": [],
  "projection": {"complete": true, "issues": []},
  "data": {
    "status": "passed",
    "failure_phase": null,
    "shrink_report": null,
    "case_report": { "declared_cases": [...], "never_called": [...] }
  }
}
```

`availability` / `schema_supported` / `schema_version` /
`field_availability` / `unknown_keys` / `projection` は **cl-mcp の transport
metadata** であり、`data` の外に置く。`data` に MCP が足した key を一つも
混ぜないための境界であって、切り詰め記録（§6.2.4）も未知 key の通知も
この外側に出す。

```
data                    = cl-spec semantic data
それ以外の core_result  = cl-mcp transport metadata
```

サブレコードごとに wrapper を付けることはしない。`shrink_report` が
`:NOT-COLLECTED` のときは `data.shrink_report` が `null`、
`field_availability.shrink_report` が `"not-collected"` になる。

### 3.2 scalar の present-NIL と absent

`(getf plist :failure-phase)` は「key が無い」と「key があって値が NIL」を
区別できない。`:failure-phase` の NIL は
**「通常の target observation であり特殊な phase は無い」という正当な実測値**
であって、古い revision に key が無いのとは別物である。

したがって `field_availability` の判定には `getf` を使わず、`get-properties`
（cl-mcp が `%optional-bool` と `definition-digest` で既に使っている手法）か
sentinel を使う。

```lisp
(let ((found (get-properties record '(:failure-phase))))
  (if found :collected :absent))
```

これは §3 で掲げた区別を scalar フィールドでも実際に成立させるための要件で
あって、飾りではない。回帰テストを 1 件立てる（§12）。

### 3.2.1 `absent` の意味は field ごとに決める

`absent` は「この revision が新しい key を知らない」とは限らない。**現行 v1
でも、意味的に不要だから key を省略する**フィールドがある。

| field | v1 で absent のとき | 根拠 |
|---|---|---|
| `metadata :state-constraints` | state 制約なし | `(when state (list :state-constraints state))` |
| `function-spec-data :cases` | case 未宣言 | `(when (function-spec-cases contract) ...)` |
| `function-spec-data :capture` | capture 未宣言 | 同上の `when` |
| `function-spec-data :state-post` | state-post 未宣言 | 同上 |
| `function-spec-data :post-value-variables` | `:primary`（既定） | `(unless (eq :primary ...))` |
| `arguments[].kind` | `:required` | `(unless (eq :required ...))` |
| `arguments[].supplied-p` | required 引数なので無い | 同上 |
| `arguments[].keyword` | `&key` 引数ではない | `(when (eq :key ...))` |
| 上記以外の v1 field | **この revision では取得不能の可能性** | — |

`arguments[].kind` の absent を `required` と読む規則（§4）は例外ではなく、
この一般則の一例である。`absent` という availability 値は構文的事実だけを
伝え、意味は field ごとの表で決める。

### 3.3 `:NOT-COLLECTED` は場所によってはデータそのもの

**`:NOT-COLLECTED` を見つけたら availability に変換する、という再帰規則を
作らない。** cl-spec では同じ keyword が場所によって semantic value である。

実測で確認した該当例:

| 出現位置 | 意味 | 扱い |
|---|---|---|
| `failure.outcome` | **target を一度も呼んでいない**（§6.1） | semantic value。`outcome.kind = "not-collected"` として保持 |
| `provenance.collection_states.target_revision` | その項目を収集しなかった | semantic value。そのまま保持 |
| `shrink_report` / `generation_report` / `case_report`（top-level） | backend がレポートを作らなかった | availability sentinel |
| `digest_omissions` / `digest_exclusions`（top-level） | `result-data` が明示的に埋める（`(getf metadata field :not-collected)`） | availability sentinel |

したがって **availability への変換は、schema 上その sentinel を使うと分かって
いる top-level field に限る**。任意の nested value へ再帰適用しない。
回帰テストを 1 件立てる（§12）。

### 3.4 未知の schema version

「`result-data` はある / 呼び出しも成功した / `schema-version` が 2 で、
adapter は v1 しか知らない」は四状態のどれでもない。unavailable でも absent
でも not-collected でもなく、adapter が意味を理解できる collected でもない。

availability を 5 値に増やさず、独立した真偽値で表す。

```json
"core_result": {
  "availability": "collected",
  "schema_supported": false,
  "schema_version": 2,
  "field_availability": null,
  "data": null
}
```

**unsupported な core schema は verification evidence にしない。**

```
schema_supported: false
  -> verified = false
  -> verification_gaps += core-schema-unsupported
```

そのうえで、**同じ semantic fact について legacy reader へ fallback しない**。
これをしないと「`result-data` は v2 で adapter は読めない → legacy reader から
`status = passed` を取って `verified = true`」という状態が起きる。legacy
reader を使う場合も display-only の診断に限り、verification evidence には
使わない。

**契約宣言の schema が unsupported なら `verified` も false にする（設計判断）。**
`result-data` が v1 で読めていても、`function-spec-data` が v2 で読めない場合、
現状の記述だけでは `status = passed` / `case_report = not-collected` で
`verified = true` になる余地がある。

```
Function Spec definition schema unsupported
  -> verification_gaps += contract-schema-unsupported
  -> verified = false
```

より緩い定義（「cl-spec 自身が pass と判定し result schema は理解できている
のだから verified は許す。ただし契約宣言の詳細は unknown」）も論理的には
成立するが、**採らない**。cl-mcp が契約宣言そのものを理解できていない状態で
`✓ VERIFIED` という強い見出しを出すのは、このプロジェクトが
`%evaluated-p` の時点から採ってきた「unknown は証拠ではない」という判断と
食い違う。

**`%contract-facts` にも同じ gate が要る。** `spec-describe`（Task A）だけでなく、
`spec-check` が内部で呼ぶ `%contract-facts` も `function-spec-data` から
`:cases` と `:preconditions` を `getf` している。v2 の Function Spec を v1 だと
思って読んではならない。schema が unsupported なら `:known nil` 相当として
扱い、`case-coverage-unknown` の判定（§5.5）も「case があるか分からない」側に
倒す。

**v2 に対して v1 の absence 規則を適用しない。** §4 の
「`:kind` の欠落は `:required`」は v1 についての規則であって、v2 の欠落が
同じ意味だと仮定してはならない。`schema-info` が v1 について
`unknown-keys :ignore` と言っているのは「未来の schema version も v1 として
読め」という意味ではない。

### 3.5 `result-data` の呼び出しが失敗した場合

四状態のどれでもない第五の状態として扱う。

| 状況 | 扱い |
|---|---|
| `RESULT-DATA` シンボルが無い / fbound でない | `availability: unavailable` + legacy fallback |
| シンボルはあり、呼んだら condition が飛んだ | **`internal-error`。legacy fallback しない** |
| 正常 return したが `NIL` / proper plist でない | **`internal-error`（incompatible-core-record）。legacy fallback しない** |
| `:schema-version` が無い | 同上 |
| `:schema-version` が 1 なのに v1 の required metadata が欠けている | 同上 |
| `record-kind` が期待と食い違う（`result-data` なのに `:definition` 等） | 同上 |

`result-data` は必ず `:schema-version` を含む（captured metadata が無くても
既定の plist が `:schema-version 1` を入れる）ので、それが無い record は
壊れている。

**検証は 1 か所にまとめ、`function-spec-data` にも同じものを掛ける。**

```
validate-versioned-record(record, expected-record-kind, expected-entity-kind)
```

| 呼び出し元 | expected-record-kind | expected-entity-kind |
|---|---|---|
| `result-data` | `:result` | — |
| `function-spec-data` | `:definition` | `:function-spec` |

v1 の required metadata は `schema-info` が明示している 7 つ
（`:schema-version` `:record-kind` `:entity-kind` `:definition-digest`
`:definition-digest-complete` `:definition-digest-covers` `:capabilities`）で、
`:schema-version` が 1 なのにこのどれかが欠けている record は
`field_availability: absent` として続行するより壊れていると見るべきである。
`record-kind` も検証する — 実測で `result-data` は `:RESULT`、
`function-spec-data` は `:DEFINITION` / `:ENTITY-KIND :FUNCTION-SPEC` を返す。

Task A（`spec-describe kind=function-spec`）も同じ validator を通す。schema
gate だけ掛けて required metadata を見ないと、v1 を名乗る壊れた record が
`spec-describe` 側からだけ通ってしまう。

後 2 者を `unavailable` にしたり legacy へ落としたりしてはならない。
「問うことができなかった」のではなく「問えたが versioned API が壊れていた」
であり、signature mismatch のような不具合を「旧 reader で何となく応答できた」
状態に隠してしまう。`%describe-function-spec` が既に同じ判断をしている —
`function-spec-data` が NIL を返したとき、引数も `:returns` も無い契約として
描画せず `:unsupported` を返す。`%contract-plist` の `read-slot` が既に
「名前が解決できたか」と「呼び出しが成功したか」を分けており、その思想の
延長である。

既知の前方互換レコード内の未知 key は adapter を失敗させない。既知 key を
投影したうえで、未知 key は名前だけ `unknown_keys` に載せる（Task F / H）。

## 4. Task A — `spec-describe kind=function-spec`

`%describe-function-spec` は現在 `function-spec-data` の以下を落としている。

| cl-spec `function-spec-data` key | 現在の cl-mcp | 変更後の MCP field | 変換 | 旧 cl-spec fallback |
|---|---|---|---|---|
| `:name` `:documentation` | あり | 変更なし | — | — |
| `arguments[].variable` | あり | 変更なし | `symbol_data` | — |
| `arguments[].spec` | あり | 変更なし | `%spec-tree` | — |
| `arguments[].kind` | **欠落** | `arguments[].kind` | keyword→string、**absent は `"required"` に正規化** | 非対応 revision は `null` |
| `arguments[].supplied-p` | **欠落** | `arguments[].supplied_p` | `symbol_data` | `null` |
| `arguments[].keyword` | **欠落** | `arguments[].keyword` | keyword→string | `null` |
| `:argument-generator` | **欠落** | `argument_generator` | `symbol_data` | `null` |
| `:argument-schema` | **欠落** | `argument_schema` | `%spec-tree` | `null` |

`function-spec-data` は required 引数について `:kind` を**省略する**
（`(unless (eq :required (argument-binding-kind binding)) ...)`。実測でも
required 引数のエントリに `:KIND` は無い）。つまり現行 v1 では
**key の欠落は `:required` を意味する**。

そのまま `null` にすると、LLM から見て「required なのか」「古い revision で
kind 情報が無いのか」が区別できない。したがって:

- レコードが v1（`:schema-version` が 1）で `:kind` が無い → `"required"` に正規化
- `:kind` フィールド自体を持たない非対応 revision と判定 → `null`

`%spec-tree` は現在 `spec->data` ノードの `:generator` を落としている。実測では
`:argument-schema` の tuple ノードが
`(:KIND :TUPLE :GENERATOR SCRIPTED-ARGUMENTS ...)` を返しており、これが
Task A の言う「引数ジェネレータ情報」の在り処である。`%spec-tree` に
`generator`（`symbol_data`）を追加する。これは spec / property の describe に
も効く共通の改善で、`argument_schema` だけの特例にはしない。
| `:preconditions` | あり | 変更なし | 有界 form | — |
| `:returns` | あり | 変更なし | `%spec-tree` | — |
| `:signals` | **欠落** | `signals` | `%spec-tree` | `null` |
| `:postconditions` | あり | 変更なし | 有界 form | — |
| `:post-value-variables` | **欠落** | `post_value_variables` | `symbol_data[]` | `null` |
| `:capture` | **欠落** | `capture[]` = `{name, form, form_complete, form_omitted_chars}` | 有界 form | `null` |
| `:state-post` | **欠落** | `state_post` + `state_post_complete` / `_omitted_chars` | 有界 form | `null` |
| `:case-selection` | **欠落** | `case_selection` | keyword→string | `null` |
| `:cases` | **欠落** | `cases[]`（下表） | — | `null` |

`cases[]` 各要素:

| cl-spec key | MCP field | 変換 |
|---|---|---|
| `:name` | `name` | keyword→string |
| `:documentation` | `documentation` | 文字列そのまま |
| `:when` | `guard` + `guard_complete` / `guard_omitted_chars` | 有界 form |
| `:outcome` | `outcome` | keyword→string（`returns` / `signals`） |
| `:returns` | `returns` | `%spec-tree` |
| `:signals` | `signals` | `%spec-tree` |
| `:postconditions` | `postconditions` + complete/omitted | 有界 form |
| `:post-value-variables` | `post_value_variables` | `symbol_data[]` |
| `:state-post` | `state_post` + complete/omitted | 有界 form |

**root metadata も同じ扱いにする。** `function-spec-data` の root は
`definition-metadata` が作る envelope をそのまま持ち、state 監視契約では
`:state-constraints` が付く。現行の `core-schema-data` が拾う 7 key に加えて
`digest_omissions` / `digest_exclusions` / `state_constraints` を
`spec-describe` の応答にも載せる（§5 の `core_result.data` と一貫させる）。

**同じ schema gate を Task A にも掛ける。** `function-spec-data` の root にも
同じ envelope があり、ここで v1 固有の正規化（`:kind` の欠落 → `required`）を
入れる以上、v2 を v1 として読まない保証が要る。

```json
// spec-describe kind=function-spec, schema-version 2 の record
"schema_supported": false,
"schema_version": 2
```

このとき **v1 固有の field 正規化を一切行わず**、不完全な投影として返す
（§3.4 と同じ規則）。`:kind` が無いことを `required` と読んではならない。

**実行しないこと**: `function-spec-data` は guard・capture・post・state-post の
コンパイル済み関数を投影しないので、adapter は読むだけで済む。target も
`:pre` も capture form も case guard も post form も state-post form も
一切呼ばない。

**描画**: JSON/オブジェクト表現が一次。テキストはその要約であって、唯一の
表現にはしない。大きな form は既存の `max_chars` 機構で有界にし、切られた
場合は既存の complete/omitted メタデータのパターンを保つ。

## 5. Task B〜I — `spec-check` の `results[].core_result`

`result-data` を 1:1 で写す専用オブジェクトを `results[]` に新設する。既存
フィールドは位置を変えず、供給元だけ `result-data` 優先に切り替える。

| `result-data` key | 現在の cl-mcp | 変更後 | 旧 cl-spec fallback |
|---|---|---|---|
| envelope 7 key（`:schema-version` `:record-kind` `:entity-kind` `:definition-digest` `:definition-digest-complete` `:definition-digest-covers` `:capabilities`） | `results[].core_schema` のみ | **`core_result.data.*` に入れる**。既存 `core_schema` はその互換 alias | `null` |
| `:digest-omissions` | **欠落**（`core-schema-data` は 7 key しか拾わない） | `core_result.data.digest_omissions` | `field_availability` |
| `:digest-exclusions` | **欠落**（同上） | `core_result.data.digest_exclusions` | `field_availability` |
| `:state-constraints` | **欠落** | `core_result.data.state_constraints` | `field_availability` |
| `:name` | 引数 `name` から | 変更なし | — |
| `:status` | legacy `property-result-status` | `results[].status`（core 優先） | legacy reader |
| `:trials` | legacy `property-result-trials` | `results[].trials.executed`（core 優先） | legacy reader |
| `:budget` | **欠落**（`%trials-budget` で再導出） | `results[].trials.budget` + `core_result.data.budget` | `%recorded-budget` → `%trials-budget` |
| `:rejected` | `contract.rejected`（legacy reader） | 同 field（core 優先）+ `core_result.data.rejected` | legacy reader |
| `:seed` | legacy reader | `results[].seed`（core 優先） | legacy reader |
| `:profile` | legacy reader | `results[].profile`（core 優先） | legacy reader |
| `:options` | **欠落**（常に `options: null` + note） | `core_result.data.options` | `field_availability: unavailable` |
| `:provenance` | **欠落** | `core_result.data.provenance` | `field_availability: unavailable` |
| `:counterexample` | legacy reader | `results[].counterexample`（core 優先） | legacy reader |
| `:shrunk-counterexample` | legacy reader | `results[].shrunk_counterexample`（core 優先） | legacy reader |
| `:shrunk-outcome` | **欠落** | `core_result.data.shrunk_outcome` | `null` + `field_availability` で absent/unavailable を区別 |
| `:shrink-report` | **欠落** | `core_result.data.shrink_report` | `field_availability: unavailable` |
| `:generation-report` | **欠落** | `core_result.data.generation_report` | `field_availability: unavailable` |
| `:failure-phase` | **欠落** | `core_result.data.failure_phase` | `null` + `field_availability` で absent/unavailable を区別 |
| `:failure-reason` | `contract.failure_reason`（legacy reader） | 同 field（core 優先）+ `core_result.data.failure_reason` | legacy reader |
| `:case-report` | **欠落** | `core_result.data.case_report` | `field_availability: unavailable` |
| `:failure` | **欠落** | `core_result.data.failure` | `null` + `field_availability` で absent/unavailable を区別 |
| `:shrunk-failure` | **欠落** | `core_result.data.shrunk_failure` | `null` + `field_availability` で absent/unavailable を区別 |
| `:elapsed` | legacy reader | `results[].elapsed`（core 優先） | legacy reader |

`:state-constraints` は state 監視契約でのみ現れる（`definition-metadata`
が `(when state (list :state-constraints state))` で前置する）。現行
`core-schema-data` は envelope 7 key しか拾わないので、`digest-omissions` /
`digest-exclusions` / `state-constraints` は今まで一つも通っていない。

`core_result.data` は **v1 で既知のフィールドについて cl-spec の record の
semantic 1:1 投影**である。JSON 表現上どうしても必要な構造変換だけを行い、
既知フィールドについては何も足さず何も落とさない。未知の extension field は
意味を推測せず、**key 名のみ `unknown_keys` に載せ、値は投影しない**。

```
known v1 semantics    = lossless
unknown future field  = 存在を通知するが、意図的に解釈しない
```Lisp の plist をそのまま JSON にはできないので、次の形を決めて
おく。

| フィールド | Lisp | JSON |
|---|---|---|
| `counterexample` / `shrunk_counterexample` | `(BALANCE 5 AMOUNT 5)` | `[{"variable": <symbol_data>, "value": <externalized>}, ...]`（既存 `%named-values` と同じ） |
| `capabilities` | `(:GENERATION :AVAILABLE :SHRINKING :NONE :INSTRUMENTATION :UNAVAILABLE)` | `{"generation": "available", "shrinking": "none", "instrumentation": "unavailable"}` |
| `digest_exclusions` | `(:TARGET-IMPLEMENTATION ...)` | `["target-implementation", ...]` |
| `digest_omissions` | `NIL` / 省略記録のリスト | `[]` / 各要素を §6.2 の再帰 projector で |
| `options` | 呼び出し側 plist | **`externalize-value`**。cl-spec v1 は options の内部構造を公開していない |
| `provenance` | plist、`collection_states` はネスト plist | オブジェクト。`:not-collected` は §3.3 のとおり値として保持 |
| `state_constraints` | `:PRESENT` のみ（`(when (state-observing-contract-p contract) :present)`） | 文字列 `"present"` |

上表の右列を cl-spec の綴りのまま snake_case にした写像であり、既存の `results[].status` / `.trials.budget` / `.seed` /
`contract.rejected` / `contract.failure_reason` などは**互換 alias**として残り、
§2 のとおり `data` と同じ解析済み値から生成する。両者が食い違うことは構造上
起こらない。

`result-data` が無い revision では `core_result` は
`{"availability": "unavailable", "field_availability": null, "data": null}` に
なり、既存フィールドは legacy reader から埋まる。

### 5.1 `case_report`（Task C）

cl-spec の `case-run-report` をそのまま写す。

| cl-spec key | MCP field |
|---|---|
| `:selection` | `selection` |
| `:unit` | `unit` |
| `:declared-cases` | `declared_cases[]` |
| `:cases[]` `{:name :documentation :called :passed :failed :error}` | `cases[]` 同名（`error`→`error`） |
| `:case-selection-errors` | `case_selection_errors` |
| `:capture-errors` | `capture_errors` |
| `:never-called` | `never_called[]` |

`:passed` を case coverage として読み替えない。`results[].status` は cl-spec の
値のまま出す。

### 5.2 `generation_report`（Task E）

`generation-request-report` の top-level 14 key をそのまま写す。

`scope` / `unit` / `policy` / `budget` / `budget_source` / `default_coefficient` /
`requested_values` / `generated_values` / `attempts` / `rejections` /
`phases.generation.{attempts,rejections}` / `phases.shrinking.{attempts,rejections}` /
`termination` / `exhaustion_phase` / `exhausted_at`

**generation report の termination を一律「検証未完了」と読まない。** generation
フェーズと shrinking フェーズは別物であり、このレポートは両方の attempt を
数えている。

```
:PHASES (:GENERATION (...) :SHRINKING (...))
:TERMINATION :BUDGET-EXHAUSTED
:EXHAUSTION-PHASE :GENERATION | :SHRINKING
```

`exhaustion-phase = :shrinking` は「target を呼び、契約違反を見つけ、反例は
確立済みで、その反例を縮小している途中で予算が尽きた」状態でありうる。
このとき未完了なのは**反例をどこまで縮小できたか**であって、検証そのもの
ではない。「この契約は破られている」という判断は既に確定している。

したがって判定は cl-spec が既に持つ `failure_phase` を権威とする。

| 核となる値 | 意味 | verification gap か |
|---|---|---|
| `failure_phase = generation` | 検証対象へ十分到達できなかった | **はい**（`generation-incomplete`） |
| `generation_report.exhaustion_phase = generation` | generation 側の未完了 | 上と同時に立つのでそちらで表す |
| `generation_report.exhaustion_phase = shrinking` | 反例縮小の未完了 | **いいえ**。既存の failure evidence は有効 |
| `shrink_report.termination`（値を問わず） | 縮小の状態。§5.3 を見よ | **いいえ**。元の failure を無効化しない |

generation フェーズの枯渇では cl-spec 自身が `:status :error` /
`:failure-reason :generation-budget-exhausted` / `:failure-phase :generation`
を返す（`src/backends/check-it.lisp` で確認）ので、`verified` は通常どおり
false になる。adapter が termination から verification status を再推論しすぎ
ないほうが §2 の中核ルールとも整合する。

生成予算の枯渇は、仕様が充足不能である証拠でも target 実装が誤っている証拠でも
ない。target の失敗に翻訳しない。

実測の注意 — 正常値であって異常ではないもの:

- 成功した run の `shrink_report` は `:NOT-COLLECTED`。縮小が失敗したのでは
  なく、失敗が無いので縮小自体が起きていない。§3 の `not-collected` に
  落とし、テキストでは何も言わない。
- 有界 AND フィルタを使わない契約の `generation_report` は
  `attempts: 0` / `rejections: 0`。これは異常ではないので、
  `termination` が `completed` であれば何も言わない。
- case-selection error でも `counterexample` は存在する（実測: `balance 5
  amount 5`）。target は呼ばれていないので、テキストはこの値を
  「この入力で関数が失敗した」と読ませてはならない。§7 の
  `failure phase:` 行がその役目を負う。

### 5.3 `shrink_report`（Task F）

`{candidates, budget, termination}` を写す。`termination` は
**閉じた列挙として検証しない**。未知の値はそのまま文字列として通し、
adapter エラーにはしない。

**`completed` という termination は存在しない。** ソースの全リテラルを
数えた結果は次のとおりで、`grep -c ":termination :completed"` は 0 である。

```
budget-exhausted  shrinker-error  mutation  validation-error  execution-error
exhausted  state-restoration-unavailable  not-a-target-failure  disabled
no-shrinker  generation-budget-exhausted
```

したがって「`termination != completed` なら縮小が不完全」という判定は書け
ない。さらに `:exhausted` は「探索を尽くし、それ以上小さくならなかった」と
いう**成功側**の値であって、不完全を意味しない。

規則:

- `shrink_report.termination` を **verification の成否判定に一切使わない**
- 意味が確定している既知の値だけテキスト化する。少なくとも
  `state-restoration-unavailable` / `generation-budget-exhausted` /
  `interrupted` 系は「縮小が実行不能 / 未完了」、`exhausted` は
  「探索を尽くした」、`disabled` / `no-shrinker` は「縮小機構が無い」
- **未知の値は値をそのまま表示し、`complete` / `incomplete` に勝手に分類
  しない**

**`shrink_report` の `not-collected` は「縮小しなかった」ではない。**
通常経路の縮小（汎用ジェネレータの `shrink`）はレポートを作らないため、
`run-property` が `(or ... :not-collected)` で `:NOT-COLLECTED` を入れる。
つまり `not-collected` は「失敗が無かった」か「ふつうに縮小した」のどちらか
である。**通常の縮小を説明するのは `shrunk_outcome`**（`:used` /
`:none` / `:different-failure`）であり、こちらが第一の情報源になる。

現行の「`shrunk_counterexample` が空 → 縮小して何も出なかった」という推論は、
`shrunk_outcome` が読める限り使わない。特に
`state-restoration-unavailable` を「shrinking ran and found no smaller
counterexample」と描画しない。両者は別のことを言っている。

### 5.4 `provenance`（Task G）

走行前に記録された値をそのまま出す。`backend` / `lisp_implementation_type` /
`lisp_implementation_version` / `cl_spec_version` / `target_revision` /
`collection_states.*`。

実行後に現在の image を読み直した値で**上書きしない**。既存の `environment`
（今の image を読む）とは別の問いに答える別フィールドとして共存させ、
`docs/tools.md` にそう書く。

### 5.5 `verified` と `verification_gaps`

- `never_called` が非空の結果が 1 つでもあれば `verified: false`。
- `verification_gaps` に追加する値と、それを出す条件:

  | gap | 出す条件 |
  |---|---|
  | `cases-never-called` | `field_availability.case_report` が `collected` かつ `never_called` が非空 |
  | `case-coverage-unknown` | 契約が `:cases` を宣言している（`%contract-facts` が `function-spec-data` から読めた）のに `field_availability.case_report` が `collected` でない |
  | `generation-incomplete` | `data.failure_phase` が `generation` |
  | `core-schema-unsupported` | `core_result.schema_supported` が false（§3.4）。legacy reader の値を verification evidence にしない |
  | `contract-schema-unsupported` | contract run で `function-spec-data` の schema が unsupported（§3.4 の `%contract-facts` gate） |

  `cases-never-called` と `case-coverage-unknown` は contract 実行にのみ
  適用する。property 実行には case も `case_report` も無いので出さない。
  `generation-incomplete` は property 実行にも適用する — property も生成
  予算を使い切りうる。

  **gap にしないもの**: `generation_report.exhaustion_phase = shrinking` と
  `shrink_report.termination`（値を問わず）。前者は反例が確立済みで未完了なのは
  縮小だけであり（§5.2）、後者はそもそも `completed` という値が存在せず、
  それ単独で verification gap を決められない（§5.3）。

  **テキストの言い回しも termination 一般には広げない。**
  「failure established, shrinking incomplete」と書けるのは、本当に未完了だと
  分かる場合 — `generation_report.exhaustion_phase = shrinking` や
  `shrink_report.termination` が `generation-budget-exhausted` などのとき —
  に限る。`exhausted` は探索を尽くした成功側、`disabled` / `no-shrinker` は
  機構が無い、未知値は分類しない。termination 一般の描画は §5.3 の
  termination 別の規則に従う。テキストには
  「failure established, shrinking incomplete」として出すが、
  `verification_gaps` には入れないし `verified` も動かさない。

  `function-spec-data` 自体が読めなかったとき（`%contract-facts` の
  `:known nil`）は `case-coverage-unknown` を出さない。契約が case を
  宣言しているかどうか自体が不明であり、gap を出せば adapter が
  「case がある」と推測したことになる。既存の `:known nil` 経路が既に
  そのための状態を持っている。
- `%verified-p` の既存 3 条件（1 件以上選択・全件 passed・各々 1 試行以上評価）
  に「未到達 case が無い」と「case coverage が unknown でない」を加える。
- cl-spec の `:passed` の意味は変えない。変えるのは cl-mcp 側の
  `verified`（= これは証拠か）の定義だけであり、その定義は既に
  「1 試行以上評価されたこと」を要求している延長線上にある。

## 6. Task D — failure observation

`:failure` / `:shrunk-failure` は `observation-data` の投影。

| cl-spec key | MCP field | 変換 |
|---|---|---|
| `:arguments` | `arguments[]` | 各値 `externalize-value` |
| `:status` | `status` | keyword→string |
| `:reason` | `reason` | keyword→string |
| `:signature` | `signature` | **再帰 projector・配列強制**（§6.2.6） |
| `:explanation` | `explanation` | **構造化オブジェクト**（後述） |
| `:outcome` | `outcome` | **構造化オブジェクト**（後述） |
| `:value` | `value` | `externalize-value` |
| `:case` | `case` | keyword→string |
| `:condition-report` | `condition_report` | 文字列（有界） |
| `:state` | `state`（下記） | — |

### 6.1 `outcome` は keyword ではない

**キー名は cl-spec の綴りのまま**である。`target_outcome` / `primary_value` /
`selected_case` という読みやすい別名は採らない — §3.1 と §5 が `data` を
cl-spec の record の純粋な写像と規定しており、改名は §2 原則 2 が禁じる
「cl-mcp の中に第二の cl-spec スキーマを作る」ことにあたる。読みやすい語は
テキスト描画（§7）が担う。判定内容は変わらない:
`outcome.kind != "not-collected"` が「target が呼ばれた」である。


実測（target が呼ばれた失敗）:

```lisp
(:KIND :RETURNED :VALUES (0))     ; type-of => CONS
```

target が呼ばれていない場合（実測: case-selection error）:

```lisp
:NOT-COLLECTED
```

signal した場合は `(:KIND :SIGNALED :CONDITION-TYPE ... :CONDITION-REPORT ...)`。
`call-outcome` 構造体の reader（`CALL-OUTCOME-KIND` 等）は **CL-SPEC から
export されていない**（実測で確認）ので、この plist を読む以外の経路は無い。

```json
"outcome": { "kind": "returned", "values": [...] }
"outcome": { "kind": "signaled",
                    "condition_type": "...", "condition_report": "..." }
"outcome": { "kind": "not-collected" }
```

投影規則: `values[]` は各値を `externalize-value`、`condition_type` は
シンボルなので `symbol_data`、`condition_report` は文字列（有界）、
`kind` は keyword→string。`:NOT-COLLECTED` は §3.3 のとおり
**この field 自身の semantic value** であり、availability へ変換しない。

**この判定は `entity_kind = function-spec` のときだけ有効である。**

通常 Property の `evaluate-trial` は 6 値しか返さない。

```lisp
;; src/execution.lisp:271
(values :passed nil nil nil nil value)   ; 第 7 値 outcome は NIL
;; src/execution.lisp:302
((null outcome) :not-collected)          ; NIL -> :NOT-COLLECTED
```

つまり **Property 本体は実行されているのに `:NOT-COLLECTED` になる**。
framework が target-call evidence を記録しないだけであって、何も実行され
なかったという意味ではない。

| entity_kind | `kind = returned` / `signaled` | `kind = not-collected` |
|---|---|---|
| `function-spec` | function target が呼ばれた | **function target は呼ばれていない** |
| `property` | （起きない） | target-call evidence を持たないだけ。**Property body が実行されなかったことを意味しない** |

将来 property の結果に同じ renderer を通したときに
「target was not called」と誤って描画しないよう、この分岐は renderer にも
入れる。テストで固定する（§12）。

**これが Function Spec についてもっとも保存したい情報である。** contract run で
`kind` が `not-collected` でないことが「target が実際に呼ばれた」ことの
**核となる事実**であり、adapter 側の推論ではない。したがって

```
failure_phase   = state-post
outcome.kind = returned
```

から「target は正常に返り、状態契約だけが失敗した」が機械的に決まる。

### 6.2 `explanation` は plist

`:explanation` は文字列ではなく plist である。実測（case-selection error）:

```lisp
(:KIND :CASE-SELECTION-ERROR :CASE-ERROR :AMBIGUOUS-CASE
 :FUNCTION OVERLAPPING-BALANCE :CASES (:AT-LEAST :AT-MOST)
 :CASE NIL :CONDITION-TYPE NIL :CONDITION-REPORT NIL)
```

`:KIND` ごとに key が異なる。**値ごと `externalize-value` にかけてはいけない**
— それでは `:errors` が `{"printed": "((:KIND ...))"}` という文字列 1 本に
戻ってしまい、cl-spec がわざわざ構造化した explain が無駄になる。
`:CASES (:AT-LEAST :AT-MOST)` も同じである。

専用の**再帰 projector** を作る。葉の型で決め、**plist かどうかを形から
推測しない**。

| Lisp | JSON |
|---|---|
| keyword | 文字列（lower-case） |
| その他のシンボル | `symbol_data` オブジェクト |
| 文字列 | 文字列 |
| 整数 | §6.2.3 の safe integer 規則 |
| その他の数・文字 | `externalize-value` |
| cons | **既定では配列**。オブジェクトになるのは §6.2.1 の既知位置だけ |
| それ以外（実際のユーザ値、CLOS インスタンス等） | `externalize-value` |

### 6.2.1 既定は `externalize-value`。構造化するのは schema で分かる位置だけ

**任意の cons を配列にするのも危険である。** 形からの plist 推測をやめても、
「cons はとりあえず配列」ではまだ足りない。`explain-data` の error datum が
その例で、同じ cons が key によって意味が逆になる。

```lisp
(:kind ... :path ... :actual <ユーザ値> :expected <expected-descriptor> ...)
```

`:actual` はユーザ値なので、`(1 2 3)` であっても `[1, 2, 3]` と構造化しては
ならず、`externalize-value` の対象である。`:expected` は
`expected-descriptor` が組んだ cl-spec 自身の構造なので、構造のまま保ちたい。

state evidence の `:values` はさらに極端で、実測は

```lisp
((BALANCE-BEFORE . 30) (ID-BEFORE . 7))
```

という alist である。dotted pair は proper list ですらないので、汎用の
「cons → 配列」では扱いが定義できない。

したがって**既定を反転する**。

```
schema で構造だと分かる cons  -> structured projector（再帰）
application / user の葉の cons -> externalize-value
未知の cons                    -> externalize-value
```

### 6.2.2 構造化する位置の一覧

| 位置 | JSON |
|---|---|
| `result-data` root | object |
| `failure` / `shrunk_failure`（observation） | object |
| `counterexample` / `shrunk_counterexample` | `[{variable, value}]`（既存 `%named-values`） |
| `outcome` | object。`values[]` は各要素 `externalize-value` |
| `state` / `state.capture` / `state.state_post` | object |
| `state.capture.declared` | `symbol_data[]` |
| `state.capture.values` | `[{name, value}]`。`name` は `symbol_data`、`value` は `externalize-value`、ただし §6.3 の opaque marker はそのまま通す |
| `state.capture.error` | object |
| `case_report` | object |
| `case_report.cases[]` | object[] |
| `case_report.declared_cases` / `.never_called` | 文字列配列 |
| `generation_report` / `.phases` / `.phases.generation` / `.phases.shrinking` | object |
| `shrink_report` | object |
| `provenance` / `.collection_states` | object |
| `capabilities` | object |
| `digest_exclusions` | 文字列配列 |
| `digest_omissions[]`（`(:kind :path :target :reason)`） | object[]。`:path` は配列 |
| `explanation` root | object |
| `explanation` の `:errors` / `:branches` / `:conjuncts` | error datum object[] |
| error datum の `:expected` | **再帰配列**（§6.2.6）。`signature` と同じ扱い |
| error datum の `:path` / `:tuple-path` / `:field-path` / `:known-tags` | 配列 |
| error datum の `:actual` / `:key` | **`externalize-value`** |
| error datum の `:actual-length` / `:expected-length` / `:violated-bound` | §6.2.3 の整数規則 |
| error datum の `:condition-report` | 文字列（有界） |
| `signature` | §6.2.5 |
| **上記以外のすべて** | **`externalize-value`** |

`options` は cl-spec v1 が内部構造を公開していないので `externalize-value`
に入る。将来 `:generation-budget` や `:target-revision` を structured に
したくなったら、cl-spec 側で options schema を正式に定義してからでよい。
`state_constraints` は現行実装では `:PRESENT` しか返さないので、keyword
として文字列化するだけでよく、再帰 projector は要らない。`:errors` / `:branches` / `:conjuncts` の 3 つは
cl-spec 自身が `*failure-shape-containers*` として分類しているので schema 由来
である。`:actual` / `:key` / `:actual-length` / `:path` /
`:condition-report` が値由来だという分類も cl-spec 自身のもので、
`*failure-shape-keys*` の docstring がそう書いている。

葉の型規則:

| Lisp | JSON |
|---|---|
| keyword | 文字列（lower-case） |
| その他のシンボル | `symbol_data` オブジェクト |
| 文字列 | 文字列 |
| 整数 | §6.2.3 の safe integer 規則 |
| それ以外 | `externalize-value` |

### 6.2.3 整数と JSON の安全範囲

**fixnum をそのまま JSON number にしない。** 64bit SBCL の fixnum は
2^62 付近まであり、JSON / JavaScript の safe integer（±(2^53 − 1)）より
はるかに広い。cl-mcp が seed をわざわざ文字列にしているのはこの理由で、
忠実性を上げる作業で新たに精度を落としては本末転倒である。

| 値 | JSON |
|---|---|
| `-(2^53 - 1) <= n <= 2^53 - 1` | 数値 |
| それ以外の整数 | 10 進文字列 |
| seed | **常に** 10 進文字列（`core_result.data.seed` も含む。semantic 1:1 でも数値にしない） |
| trials / budget / rejected / candidates / attempts 等のカウンタ | safe 範囲なら数値 |
| 任意のユーザ整数（`counterexample` の値など） | `externalize-value` |

### 6.2.4 切り詰めたことを消費側に伝える

深さと要素数には `*value-print-level*` / `*value-print-length*` と同じ上限を
掛ける。ただし**切ったことが分からない切り方はしない**。

切り詰めの記録は `core_result.data` の**外**に置く（§3.1）。`data` は
cl-spec の record の写像であって、MCP が足した key を混ぜない。

```json
"projection": {
  "complete": false,
  "issues": [
    {"path": ["failure", "explanation", "errors"],
     "reason": "length-limit", "omitted_items": 5},
    {"path": ["options", 3], "reason": "depth-limit"}
  ]
}
```

- 長さで切った場合: 先頭 N 件を残し、`issues` にそのパスと
  `omitted_items` を記録する。
- 深さで切った場合: その位置には **`externalize-value` の plist** を置く。
  これは `data` の中で任意の Lisp 値を表すのに既に使っている標準形なので、
  新しい key の発明にはならない。パスは `issues` に記録する。

error が 10 個あるのに JSON には 5 個しか無く、5 個しか無いように見える、
という状態を作らない。

### 6.2.5 `expected` も位置的タグ付きリストである

当初この表は `:expected` を「object」と書いていた。`explain.lisp` の
`expected-descriptor` メソッドのうち plist を返す 5 つ（`spec` / `plist-spec` /
`keyed-field-spec` / `object-spec` / `tagged-union-spec`）だけを読んだ誤りで、
**18 中 13 は位置的なタグ付きリスト**である。

```lisp
(list :range :min N :max M)                       ; タグ + plist 尾部
(list* :member VALUES)                            ; タグ + 任意個の値
(list* :and (mapcar #'expected-descriptor ...))   ; タグ + 入れ子 descriptor
(list :nullable (expected-descriptor ...))
(list :type X) (list :satisfies P) (list :spec TARGET)
(list :instance-of C) (list :not D)
(list* :tuple ...) (list* :list-of D . constraints) (list* :vector-of ...)
(list* :or ...)
```

object として walk すると `(:range :min 0 :max 100)` は key 位置に `0` を置き、
`%json-key` が `symbol-name` を整数に呼んで落ちる。実測で統合テスト 8 件が
これで落ちた。

したがって `:expected` は **§6.2.5 の `signature` と同じ扱い** — 再帰的な配列に
投影する。先頭のキーワードは key ではなく tag であり、key として読めば
cl-spec が宣言していない関係の発明になる。入れ子の `expected-descriptor`
（`:and` / `:or` / `:tuple` / `:list-of` / `:nullable` / `:not` の内側）は
再帰的に同じ規則で投影する。

`:kind` で始まる 5 つの形（`tagged-union-spec` を含む）も配列になる。無損失であり、`data` を読む側は
先頭要素でどの形かを判別できる。object にする分岐を設けないのは、
「形から役割を推測しない」という §6.2.1 の規則そのものである。

### 6.2.6 `signature` — 構造化するが、オブジェクトにはしない

`signature` も文字列に潰さず、**同じ再帰 projector** を通す。ただし
**flat array にはならないし、オブジェクトにもしない。**

`failure-signature` が実際に組む形:

```lisp
(:return-value :return-spec (<failure-shape plist> ...))  ; ネストする
(:condition-spec SIMPLE-ERROR (<failure-shape plist> ...))
(:missing-condition)                                      ; 1 要素
(:target-signal SIMPLE-ERROR)                             ; 位置的タグ
(:case :sufficient-funds :state-postcondition 0)          ; 実測
```

先頭の keyword は **key ではなくタグ**である。`(:target-signal SIMPLE-ERROR)`
を `{"target-signal": "SIMPLE-ERROR"}` と読ませると、cl-spec が宣言していない
key/value 関係を adapter が発明したことになる。また `:return-spec` の
signature は `failure-shape` の plist を入れ子に持つので、スカラーの
flat array でも表せない。

`signature` のトップレベルは **array** に投影する。ただし
「既定は `externalize-value`」に倒すと内側の `failure-shape` plist まで
潰れてしまうので、`failure-signature` が実際に組む文法を 2 つだけ持たせる。

```
(:return-value :return-spec <failure-shapes>)
(:condition-spec <condition-type> <failure-shapes>)
```

この `<failure-shapes>` の位置だけ **failure-shape の object[]**（error datum
と同じ projector）として投影し、それ以外の要素は葉の型規則に従う。
他の形（`(:missing-condition)`、`(:target-signal TYPE)`、
`(:case :sufficient-funds :state-postcondition 0)`）はすべて葉の並びなので、
配列のままで過不足ない。

```json
"signature": ["case", "sufficient-funds", "state-postcondition", 0]
"signature": ["return-value", "return-spec", [ { "kind": "...", "expected": [...] } ]]
```

これは failure identity の比較にも使える（順序を保つ無損失表現）。

`state` は単一の boolean に潰さない。

```
state.capture    = {status, declared[], values[], error{binding,index,condition_type}}
   status: not-evaluated | completed | error
state.state_post = {status, reason, case, index, form, condition_type}
   status: not-evaluated | passed | violation | error
```

### 6.3 cl-spec が「凍結できなかった」と言った捕捉値

`project-capture-value` は、スナップショットが同一性で保持してしまう値
（CLOS インスタンス、構造体、ハッシュテーブル、関数など）を

```lisp
(:UNAVAILABLE :REASON :OPAQUE-VALUE :TYPE <type>)
```

として投影する。これは「この値は証拠として凍結できなかった」という
**cl-spec 側の明示的な宣言**である。

cl-mcp がこれをさらに `externalize-value` にかけて `object_id` を付けると、
「そのオブジェクトを証拠として取得できた」ように見えてしまう。マーカーを
検出して、そのまま unavailable として通す。`object_id` を付けない。
テストを 1 件立てる（§12）。

これにより

```
target は正常復帰 / 返り値・post 契約は通過 / state-post が落ちた
```

を

```
target 自体が落ちた
```

から区別できる。この区別はどのコードを直すかを直接左右する。

## 7. Task J — テキストと JSON の整合

テキストは構造化レポートの下流に置く。以下の不整合を禁止し、テストで固定する。

```
NG: text "VERIFIED"           / json never_called = ["insufficient-funds"]
NG: text "no smaller counterexample exists"
                              / json shrink_report.termination = "state-restoration-unavailable"
NG: text "target failed"      / json failure_phase = "generation"
```

per-result ブロックに追加する行（値があるときだけ出す）:

```
    cases: success 100 called (100 passed) | insufficient-funds NEVER CALLED
    failure phase: state-post -- the target WAS called; the contract's
                   state-post clause is what failed
    captured: balance-before = 100
    state-post: violated at form 0 -- (= (balance a) (- $before amount))
    generation: budget-exhausted in the generation phase (4000 of 4000
                candidates) -- verification did NOT complete
    shrinking: not attempted -- state-restoration-unavailable
```

見出し行は既存の coverage qualifier と同形で、未到達 case と生成未完了を拾う。

```
⚠ NOT VERIFIED (1 declared case was never reached: insufficient-funds)
```

## 8. Task I — 導出フィールドの棚卸し

| 導出 | 分類 | 備考 |
|---|---|---|
| `%trials-budget` の `:budget` | ② legacy fallback | `result-data :budget` が走行時の解決済み予算を持つ |
| `%trials-budget` の `:property-trials` / `:backend-default` | ① 必要 | cl-spec は出さない |
| `%recorded-budget`（`function-check-result-budget`） | ② legacy fallback | |
| `shrink_status` の空リスト推論 | ② legacy fallback | `shrink_report` / `shrunk_outcome` が優先 |
| `counterexample_status` の argument-count 推論 | ② legacy fallback | 下記のとおり `failure` の有無だけでは答えられない |
| `contract.rejected` / `contract.failure_reason` の legacy reader | ② legacy fallback | |
| `rejection_status` の三値判定（overcount / contradicted / negative） | ① 必要 | cl-spec は「この計数が信用できるか」を言わない |
| `contract.explanation`（`property-result-explanation`） | ① 必要 | cl-spec 側で reason フィルタ済みの選択値。observation の生 explanation は `core_result.failure.explanation` に別途出す |
| `environment`（現 image の backend / registry） | ① 必要 | `provenance` とは別の問い。上書きしない |
| `%verification-gaps` / `%verified-p` | ① 必要 | cl-mcp 側の「これは証拠か」判定 |

**`counterexample_status` と failure evidence を混同しない。** failure
observation は必ずしも target の反例ではない。capture error と
case-selection error も failure observation を持ち、そのとき target は
一度も呼ばれていない。実測では case-selection error でも
`:COUNTEREXAMPLE (BALANCE 5 AMOUNT 5)` が入る。

これを LLM が「この入力で関数が失敗した」と読むと、まさに本タスクが防ごうと
している誤編集につながる。したがって

- `counterexample_status` は従来どおり「反例の値が取れたか」だけを言う
- 「target が呼ばれたか」は **contract run について**
  `core_result.data.failure.outcome.kind` が `not-collected` でない
  ことで答える（§6.1。core の事実であって adapter の推論ではない）。
  property run にこの判定を適用しない — そちらの `not-collected` は
  「target-call evidence が無い」であって「実行されなかった」ではない
- テキストは `failure phase:` 行でどちらなのかを明示する（§7）

③（陳腐化・削除可）に分類したものは無い。互換性のための fallback を整理目的で
消さない。

新しい late-bound reader は追加しない。`result-data` 一本に寄せる。

## 9. 後方互換

- `result-data` が無い revision: 既存の legacy reader 経路をそのまま残す。
  `core_result` は `{"availability": "unavailable", "field_availability": null,
  "data": null}` になり、既存フィールドは legacy reader から埋まる。
  現行 core の事実を合成しない。
- `result-data` はあるが新しい key が無い revision: その key だけ `absent` と
  し、レコード全体を拒否しない。
- 既知の前方互換レコード内の未知 key は adapter を失敗させない。
- 次の区別を 1 つのエラーに潰さない（現行どおり維持）。

```
cl-spec 未ロード / この revision が API 非対応 / 定義が未登録
実行失敗 / adapter 内部エラー / 検証未完了
```

- cl-spec への ASDF 依存は追加しない。コンパイル時に cl-spec のパッケージ
  シンボルを直接参照しない（現行の遅延解決を維持）。

## 10. セキュリティ・堅牢性

現行の保証を維持する。

- 出力の有界化（`max_chars` / `max_value_chars` / `*value-print-level*` /
  `*value-print-length*`）
- MCP から受け取ったデータに対する reader / eval を一切追加しない
- 信頼できない名前によるパッケージ汚染なし（`find-symbol` のみ、`intern` しない）
- 敵対的オブジェクトの無制限な印字なし（`externalize-value` / 有界ストリーム）
- deadline 挙動と worker 分離

**任意の application / user の葉の値**は `externalize-value`（`printed` /
`printed_complete` / `omitted_chars` / `restorable` / `type` / `object_id`）を
通す。**cl-spec が schema として定義している plist / list 構造**は、対応する
structured projector（§6.2）を通す — ここを「すべて externalize-value」と
読むと `explanation` が再び文字列化され、§6.2 と矛盾する。

source form は `%print-bounded-form`。keyword は文字列化。整数は §6.2.2 の
safe integer 規則に従い、範囲外は 10 進文字列にする。

## 11. `check-call` — 意図的な見送り

cl-spec main には `check-call` / `call-check-data` がある。本設計では MCP
公開を見送り、必要条件を `docs/tools.md` に記す。

理由: `check-call` の引数は生の Lisp オブジェクト列で、JSON から供給するには

- (a) reader を通す — 禁止事項に真っ向から反する
- (b) worker の object ID レジストリを使う — 「JSON リテラルと ID の混在列」
  という新しい直列化契約の設計そのもので、小さくない
- (c) JSON リテラルの部分集合に限る — `:capture` / state 契約が必要とする
  構造体・CLOS インスタンスを表現できず、`check-call` の主用途を外す

「小さく、曖昧さのない拡張」に該当しない。先に既存 adapter が Function Spec と
生成チェックのデータを忠実に運べるようにし、直接呼び出しは別 PR とする。
実装するなら (b) を基礎に、既存 object ID を再利用する方向を第一候補とする。

## 12. テスト計画

### 実 cl-spec に対する統合（`tests/spec-integration-test.lisp`）

fixture は `tests/fixtures/spec-fixture-contracts.lisp` に追加する。

1. **named cases** — 2 つ以上の named case を持つ契約を、片方の case にしか
   到達しないように走らせる。`status` / `case_report` / 到達した case /
   `never_called` が保たれ、完全な case coverage を示唆しないこと。
2. **state-post 失敗** — `:capture` + 正常復帰 + `:state-post` を持つ契約で、
   target は成功復帰しつつ状態を誤って変更する。`outcome` /
   捕捉値 / state-post 失敗 / `failure_phase: state-post` / `failure_reason` /
   該当すれば `case` が保たれること。
3. **case-selection error** — guard が重複または欠落した契約。target が
   呼ばれていないこと、`failure_phase: case-selection`、構造化された
   case-selection 証拠が保たれること、応答が「target 実装が失敗した」と
   言わないこと。
4. **生成予算の枯渇** — 予算を使い切る spec/generator の組み合わせ。
   `generation_report` があること、`termination` があること、target の失敗を
   捏造しないこと、テキストが「検証は完了しなかった」と伝えること。
5. **state による縮小不能** — target 呼び出し後に失敗する state 監視契約。
   元の失敗証拠があること、`shrink_report` がなぜ縮小が試みられ / 完了され
   なかったかを言うこと、応答が「最小の反例」「これ以上縮小できない反例」と
   主張しないこと。

### stub API（`tests/spec-adapter-report-test.lisp`）

6. **旧 cl-spec fixture** — `result-data` を持たない API ハンドルで、
   有効な応答が返り、各フィールドが明示的に unavailable / unknown となり、
   クラッシュしないこと。
7. `result-data` はあるが新しい key を持たない revision — 該当 key だけ
   `absent` となり、レコード全体は拒否されないこと。

8. **present-NIL と absent の分離** — `:failure-phase` が key として存在し
   値が NIL の record と、key 自体が無い record で `field_availability` が
   `collected` / `absent` に分かれること。§3.2 の最重要回帰テスト。
9. **`result-data` はあるが呼ぶと signal する** — legacy fallback せず
   `internal-error` になること（§3.5）。
10. **live condition object の維持** — 現行 cl-spec 経路でも既存の
    `condition.object_id` が失われないこと（§2 の補助 reader 例外）。
11. **`:NOT-COLLECTED` の field 固有性** — `failure.outcome` が
    `:NOT-COLLECTED` のとき、`outcome.kind = "not-collected"` として
    残り、`field_availability` へ誤変換されないこと。
    `provenance.collection_states.target_revision` も同様。§3.3 の回帰テスト。
12. **未知の schema version** — `schema-version 2` の record に対して
    `schema_supported: false` / `data: null` になり、v1 の absence 規則
    （`:kind` 欠落 → required 等）が適用されないこと（§3.4）。
13. **explanation / signature が文字列にならない** — `:errors` が
    ネストしたオブジェクトの配列として残り、`signature` が配列として残ること
    （§6.2 / §6.2.6）。
14. **通常 Property の outcome semantics** — Property body が 1 回実行されて
    失敗した run で `failure.outcome.kind = "not-collected"` になり、
    かつテキストが「target was not called」と**言わない**こと。
    `not-collected = Function Spec の target evidence が無い` と
    `何も実行されていない` の混同を防ぐ、§6.1 の回帰テスト。
15. **`:actual` と `:expected` の非対称** — error datum の `:expected` が
    構造のまま残り、`:actual` が `externalize-value` の形になること。
    同じ cons でも扱いが逆になることの回帰テスト（§6.2.1）。
16. **alist の捕捉値** — `state.capture.values` の `(NAME . VALUE)` が
    `[{name, value}]` になること（§6.2.2）。
17. **transport metadata が `data` に混ざらない** — 切り詰めが起きても
    `core_result.data` に `_complete` / `_omitted_items` 等が現れず、
    `core_result.projection.issues` に出ること（§3.1 / §6.2.4）。
18. **unsupported schema の verdict** — `schema_supported: false` のとき
    legacy reader が `passed` を返しても `verified: false` になり、
    `core-schema-unsupported` が gap に入ること（§3.4 / §5.5）。
19. **keyword の list を plist と誤認しない** — `:CASES (:AT-LEAST :AT-MOST)`
    が JSON 配列になり、`{"at-least": "at-most"}` にならないこと（§6.2.1）。
    projector の根幹の回帰テスト。
20. **大きな整数の精度** — 2^60 相当の値が JSON number として丸められず、
    10 進文字列で出ること。seed が `core_result.data` でも文字列であること
    （§6.2.3）。
21. **malformed な `result-data`** — API が `NIL` や `:schema-version` の
    無い値を返したとき、legacy fallback せず `internal-error` になること
    （§3.5）。
22. **不透明な捕捉値** — cl-spec が
    `(:UNAVAILABLE :REASON :OPAQUE-VALUE :TYPE ...)` を返したとき、
    `object_id` を付けず unavailable のまま通ること（§6.3）。

### builder（`tests/spec-response-builders-test.lisp`）

テキストと JSON の不一致を禁止するアサーション（§7 の 3 パターン）に加えて:

23. **shrinking 中の生成予算枯渇** — 反例は確立済みで
    `generation_report.termination = budget-exhausted` /
    `exhaustion_phase = shrinking` のとき、「verification incomplete」と
    表示せず「failure established, shrinking incomplete」と表示すること。
    `verification_gaps` にも入らないこと（§5.2 / §5.5）。
24. **未知の shrink termination** — 知らない termination 値を
    `complete` / `incomplete` に分類せず、値そのものを表示すること。
    `exhausted` を「縮小が不完全」と表示しないこと（§5.3）。

## 13. 非目標

新しい DSL 節、新しい property 意味論、生成アルゴリズム、新しい縮小
アルゴリズム、状態復元、任意 Lisp オブジェクトの直列化、`(setf name)` 対応、
CLOS メソッド単位の契約合成、汎用検証プランナ、cl-spec issue #19 / #20、
既存 spec ツールの単一ツールへの統合 — いずれも本設計の対象外。

これは adapter の忠実性の作業である。

## 14. 検証

```
rove cl-mcp.asd                              # 全スイート
mallet src/*.lisp src/*/*.lisp tests/*.lisp  # Lint CI と同じ glob
(asdf:compile-system :cl-mcp :force :all)    # cold compile
```

に加えて、cl-spec main を読み込んだ新規プロセスで統合テストのみを回した結果と、
stub fixture（旧 revision 相当）での結果の双方を報告する。

## 15. 納品

ブランチ `feat/cl-spec-adapter-fidelity`、コミット 2 段階。

1. Task A（`spec-describe kind=function-spec` の忠実化）
2. Task B〜J（`spec-check` の `result-data` 透過）+ 統合テスト + docs
