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
  突き合わせて選ぶ規則ではない。`result-data` が読める revision ではその
  レコードだけを読み、legacy reader は呼ばない。両者が食い違いうる状況を
  そもそも作らない。

## 3. 可用性の四状態

`result-data` 由来のサブレコードは、値の代わりに次の `status` を持ちうる。
Task H が要求する「取得できなかった」と「測定されたゼロ・空」の区別を、
散文ではなく構造で担保する。

| status | 意味 |
|---|---|
| `collected` | cl-spec が実測値を返した |
| `not-collected` | cl-spec が明示的に `:NOT-COLLECTED` を返した（backend が参加しなかった等） |
| `absent` | この cl-spec の `result-data` にその key が無い（新しい key を知らない revision） |
| `unavailable` | この cl-spec に `result-data` 自体が無い。adapter は問うことすらできなかった |

既知 key を投影したうえで、未知 key は名前だけ `unknown_keys` に載せる。
未知 key があること自体を adapter のエラーにはしない（Task F / H）。

## 4. Task A — `spec-describe kind=function-spec`

`%describe-function-spec` は現在 `function-spec-data` の以下を落としている。

| cl-spec `function-spec-data` key | 現在の cl-mcp | 変更後の MCP field | 変換 | 旧 cl-spec fallback |
|---|---|---|---|---|
| `:name` `:documentation` | あり | 変更なし | — | — |
| `arguments[].variable` | あり | 変更なし | `symbol_data` | — |
| `arguments[].spec` | あり | 変更なし | `%spec-tree` | — |
| `arguments[].kind` | **欠落** | `arguments[].kind` | keyword→string | key 無し→`null` |
| `arguments[].supplied-p` | **欠落** | `arguments[].supplied_p` | `symbol_data` | `null` |
| `arguments[].keyword` | **欠落** | `arguments[].keyword` | keyword→string | `null` |
| `:argument-generator` | **欠落** | `argument_generator` | `symbol_data` | `null` |
| `:argument-schema` | **欠落** | `argument_schema` | `%spec-tree` | `null` |

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
| envelope 7 key | `results[].core_schema` | 変更なし | `null` |
| `:name` | 引数 `name` から | 変更なし | — |
| `:status` | legacy `property-result-status` | `results[].status`（core 優先） | legacy reader |
| `:trials` | legacy `property-result-trials` | `results[].trials.executed`（core 優先） | legacy reader |
| `:budget` | **欠落**（`%trials-budget` で再導出） | `results[].trials.budget` + `core_result.budget` | `%recorded-budget` → `%trials-budget` |
| `:rejected` | `contract.rejected`（legacy reader） | 同 field（core 優先）+ `core_result.rejected` | legacy reader |
| `:seed` | legacy reader | `results[].seed`（core 優先） | legacy reader |
| `:profile` | legacy reader | `results[].profile`（core 優先） | legacy reader |
| `:options` | **欠落**（常に `options: null` + note） | `core_result.options` | `{status:"unavailable"}` |
| `:provenance` | **欠落** | `core_result.provenance` | `{status:"unavailable"}` |
| `:counterexample` | legacy reader | `results[].counterexample`（core 優先） | legacy reader |
| `:shrunk-counterexample` | legacy reader | `results[].shrunk_counterexample`（core 優先） | legacy reader |
| `:shrunk-outcome` | **欠落** | `core_result.shrunk_outcome` | `null` |
| `:shrink-report` | **欠落** | `core_result.shrink_report` | `{status:"unavailable"}` |
| `:generation-report` | **欠落** | `core_result.generation_report` | `{status:"unavailable"}` |
| `:failure-phase` | **欠落** | `core_result.failure_phase` | `null` |
| `:failure-reason` | `contract.failure_reason`（legacy reader） | 同 field（core 優先）+ `core_result.failure_reason` | legacy reader |
| `:case-report` | **欠落** | `core_result.case_report` | `{status:"unavailable"}` |
| `:failure` | **欠落** | `core_result.failure` | `null` |
| `:shrunk-failure` | **欠落** | `core_result.shrunk_failure` | `null` |
| `:elapsed` | legacy reader | `results[].elapsed`（core 優先） | legacy reader |

`core_result` 自身も `status` を持ち、`result-data` が無い revision では
`{status:"unavailable"}` ひとつになる。

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

`generation-request-report` の 13 key をそのまま写す。

`scope` / `unit` / `policy` / `budget` / `budget_source` / `default_coefficient` /
`requested_values` / `generated_values` / `attempts` / `rejections` /
`phases.generation.{attempts,rejections}` / `phases.shrinking.{attempts,rejections}` /
`termination` / `exhaustion_phase` / `exhausted_at`

`termination` が `completed` 以外のときは検証未完了として扱う（後述 5.5）。
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
**閉じた列挙として検証しない**。cl-spec が公開する閉じた列挙が無い以上、
未知の値はそのまま文字列として通し、adapter エラーにはしない。

現行の「`shrunk_counterexample` が空 → 縮小して何も出なかった」という推論は、
`shrunk_outcome` / `shrink_report` が読める限り使わない。特に
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
  | `cases-never-called` | `case_report.status` が `collected` かつ `never_called` が非空 |
  | `case-coverage-unknown` | 契約が `:cases` を宣言している（`%contract-facts` が `function-spec-data` から読めた）のに `case_report.status` が `collected` でない |
  | `generation-incomplete` | `generation_report.status` が `collected` かつ `termination` が `completed` 以外 |

  `cases-never-called` と `case-coverage-unknown` は contract 実行にのみ
  適用する。property 実行には case も `case_report` も無いので出さない。
  `generation-incomplete` は property 実行にも適用する — property も生成
  予算を使い切りうる。

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
| `:signature` | `signature` | 有界 form |
| `:explanation` | `explanation` | **構造化オブジェクト**（後述） |
| `:outcome` | `target_outcome` | keyword→string |
| `:value` | `primary_value` | `externalize-value` |
| `:case` | `selected_case` | keyword→string |
| `:condition-report` | `condition_report` | 文字列（有界） |
| `:state` | `state`（下記） | — |

`:explanation` は文字列ではなく plist である。実測（case-selection error）:

```lisp
(:KIND :CASE-SELECTION-ERROR :CASE-ERROR :AMBIGUOUS-CASE
 :FUNCTION OVERLAPPING-BALANCE :CASES (:AT-LEAST :AT-MOST)
 :CASE NIL :CONDITION-TYPE NIL :CONDITION-REPORT NIL)
```

`:KIND` ごとに key が異なるので、`shrink_report` / `generation_report` と同じ
汎用 plist 投影（既知 key はそのまま、値は `externalize-value`、未知 key は
`unknown_keys` に名前のみ）を使い、構造を保ったまま運ぶ。有界印字した文字列
1 本に潰すと、`:CASES (:AT-LEAST :AT-MOST)` のような「どの case が衝突したか」
が散文の中に埋もれる。

既存の `contract.explanation`（`property-result-explanation` の選択値を有界
印字した文字列）はそのまま残す。§8 のとおり別の問いに答えるフィールドである。

`state` は単一の boolean に潰さない。

```
state.capture    = {status, declared[], values[], error{binding,index,condition_type}}
   status: not-evaluated | completed | error
state.state_post = {status, reason, case, index, form, condition_type}
   status: not-evaluated | passed | violation | error
```

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
| `counterexample_status` の argument-count 推論 | ② legacy fallback | `core_result.failure` の有無が直接答える |
| `contract.rejected` / `contract.failure_reason` の legacy reader | ② legacy fallback | |
| `rejection_status` の三値判定（overcount / contradicted / negative） | ① 必要 | cl-spec は「この計数が信用できるか」を言わない |
| `contract.explanation`（`property-result-explanation`） | ① 必要 | cl-spec 側で reason フィルタ済みの選択値。observation の生 explanation は `core_result.failure.explanation` に別途出す |
| `environment`（現 image の backend / registry） | ① 必要 | `provenance` とは別の問い。上書きしない |
| `%verification-gaps` / `%verified-p` | ① 必要 | cl-mcp 側の「これは証拠か」判定 |

③（陳腐化・削除可）に分類したものは無い。互換性のための fallback を整理目的で
消さない。

新しい late-bound reader は追加しない。`result-data` 一本に寄せる。

## 9. 後方互換

- `result-data` が無い revision: 既存の legacy reader 経路をそのまま残す。
  `core_result` は `{status:"unavailable"}` になり、各サブレコードは
  「未対応・未収集・不明」を明示する。現行 core の事実を合成しない。
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

`result-data` 中の任意 Lisp 値は必ず `externalize-value`（`printed` /
`printed_complete` / `omitted_chars` / `restorable` / `type` / `object_id`）を
通す。source form は `%print-bounded-form`。keyword / 整数はそのまま。

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
   target は成功復帰しつつ状態を誤って変更する。`target_outcome` /
   捕捉値 / state-post 失敗 / `failure_phase: state-post` / `failure_reason` /
   該当すれば `selected_case` が保たれること。
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

### builder（`tests/spec-response-builders-test.lisp`）

テキストと JSON の不一致を禁止するアサーション（§7 の 3 パターン）。

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
