# cl-spec 開発への申し送り — cl-mcp adapter を実装して分かったこと

- 日付: 2026-09-10
- 送り元: cl-mcp `feat/cl-spec-adapter`（`spec-symbol` / `spec-describe` / `spec-check`）
- 対象 cl-spec revision: `d1cf1af`（`src/` は `8ab6ffb` と同一）
- 立場: **cl-spec の consumer**。cl-spec 側のコードは 1 行も変更していない

cl-spec 仕様書 §73.2 の実証順 2「既存の `semantic-data`・`property-data`・runner を
最小限の cl-mcp adapter へ接続する」を実施した結果の報告。実証順 4
「実行結果の不足、再現条件、取得情報量を測り、LLM-01〜06 の未達項目を明示する」に
あたる内容を含む。

**この文書の読み方**: 「実測」は新規 SBCL プロセスで実行して確認したもの、
「ソース確認」は該当ファイルを読んで確認したもの。推測は書かない。
`docs/cl-spec-skeleton-followups.md` に既にある項目は【既知】と付す。

---

## 0. 要約

adapter は動いた。§73.2 の 3「契約取得・反例取得・修正・再検査を一周させる」も
通っている（cl-mcp 側 `docs/superpowers/plans/2026-09-09-cl-spec-mcp-adapter.md`
の「実証結果」に実出力を記録）。

そのうえで、**consumer が正しく振る舞うために回避策を書かざるを得なかった箇所**が
9 件あった（1.1〜1.5、2.1〜2.4）。

うち 3 件（1.1・1.2・1.3）は「区別されるべき 2 つの状態が同じ値になる」型で、
いずれも §72.6 の受け入れ条件（「取得できない値と、ゼロ・false・空の結果を区別する」）に
関わる。cl-mcp は別の API を併読して回避したが、**その API が読めない状況では
区別が復元できない**。残る 6 件は、情報が取得できない（2.1〜2.3）、
実行環境が引数で渡せない（1.5）、契約が無い（1.4）、
静かに上書きされる（2.4）というそれぞれ別の型である。

優先度は「adapter が回避策を持てたか」で付けた。P1 は回避策が原理的に不完全なもの。

---

## 1. P1: 区別できない状態（consumer 側で埋められない）

### 1.1 `(:trials (:normal 0))` が `:PASSED` / `trials 0` を返す【実測】

```lisp
(cl-spec:defproperty zero-budget ((x si)) (:trials (:normal 0)) (= x x))
(cl-spec:run-property 'zero-budget :profile :normal)
;; => status=:PASSED trials=0
```

`resolve-trials` が `(or (getf table :normal) default)` なので `0` は偽ではなく採用され、
`run-generated-test` の `(loop for trial from 1 to 0 ...)` が即座に `finally` へ落ちる。

**consumer への影響**: 「1 件以上選択され、全件 passed」だけで検証成功を判定すると、
**何も評価していない run が verified になる**。cl-mcp は `trials.executed >= 1` を
第 3 条件に加えて回避したが、これは `property-result-trials` が
「実行済み試行数」であるという解釈に依存している。

**提案**: `resolve-trials` が 0 を返す場合の扱いを決める。候補は
(a) `run-property` が `:skipped` を返す（`property-result` の docstring が既に
`:skipped` を挙げており、現 backend からは出ない status の初めての用途になる）、
(b) `invalid-property-form` を `defproperty` 時点で出す、
(c) 現状維持だが `property-result` に「予算 0 で走らなかった」ことが読める slot を足す。
§72.1 の「実行件数がゼロのケースが成功した検証として報告されない」に直結する。

### 1.2 引数ゼロの Property の反例が `NIL`【実測】

```lisp
(cl-spec:defproperty no-args () (:kind :invariant) nil)
(cl-spec:run-property 'no-args)
;; => status=:FAILED trials=1 ce=NIL
```

**consumer への影響**: 「反例を取得できた（ただし引数が無いので空）」と
「反例を取得できなかった（timeout・実行エラー・未実行）」が同じ `NIL` になる。
外部表現では両者とも `[]` に落ちる。cl-mcp は `property-data` の `:arguments` の
長さを別途読んで `counterexample_status` を組み立てて回避したが、
**`property-data` が読めない状況では区別できない**（そのときは `unknown` と報告している）。

**提案**: `property-result` が「反例が存在するか」を値とは別に持つ。
`counterexample` slot の初期値を `:none` のような明示的な値にするか、
`property-result-counterexample-p` を足す。§72.6 の受け入れ条件そのもの。

### 1.3 `(:shrink nil)` の失敗と「縮小できなかった」が同じ `NIL`【実測】

```lisp
(cl-spec:defproperty no-shrink ((x si)) (:shrink nil) (< x 0))
;; => status=:FAILED ce=(X 1) shrunk=NIL
```

`run-generated-test` は `(when shrink-p (shrink ...))` なので、shrink 無効時と
「縮小して何も出なかった」が同じ `NIL`。

**consumer への影響**: cl-mcp は `property-metadata` の `:shrink` を読んで
`shrink_status: disabled` と区別した。`:shrink` は `defproperty` が常に metadata へ
入れるので現状は回避できているが、metadata の存在に依存した回避策である。

**提案**: §72.4 が要求する「縮小の完了・予算切れ・中断を区別する」の一部として、
縮小の状態を `property-result` に持たせる。無効・完了・予算切れ・中断・
「より小さい入力なし」の 5 値が consumer 側で欲しい形。

### 1.4 中断後の状態復元について契約が無い（§73 D5）

`run-property` に timeout が無い（§48）ため、時間上限は実行ホストが持つ。
cl-mcp は別スレッドで走らせて deadline で巻き戻すが、
**巻き戻した後にその image を再利用してよいかを判断する材料が無い**。
cl-spec 側に adapter から観測できる cleanup が無く、Property が共有状態を
途中まで変えたまま終わった可能性を否定できない。

**consumer への影響**: cl-mcp は timeout 後に必ず `worker_reuse: unknown` を返し、
worker の差し替え（pool 有効時）またはプロセス再起動（inline 時）を案内している。
これは安全側だが、**純粋な Property の timeout でもセッションの Lisp 状態を
捨てさせる**ので代償が大きい。

**提案**: §73 D5 の決定時に、最低限
「Property が副作用を宣言していない場合、中断後の image は再利用可能」と
言えるだけの契約（`(:effects ...)` メタデータ、あるいは cleanup protocol）が
あると、ホスト側が過剰に捨てずに済む。

### 1.5 `run-property` が実行スレッドで backend を読む【ソース確認・実害あり】

`src/property-runner.lisp` の `run-property` は `(current-generator-backend)` を
自身の動的環境で読む。予算導出（`resolve-trials`）も同じ呼び出し内。

**consumer への影響**: §48 が「現在は実行ホスト側で時間上限を管理する」と
定めている以上、**ホストは別スレッドで走らせる**。SBCL の新スレッドは
dynamic binding を継承しないので、呼び出し側が `*generator-backend*` を
束縛していても実行スレッドは global を見る。cl-mcp は API シンボルを保持して
`progv` で束縛し直して回避した（回避前は「予算を導出した backend」と
「実際に走った backend」が食い違いうる状態だった）。

**提案**: `run-property` に `:backend` キーワードを足す。
`:registry` が既にそうなっているのと同じ理由で、
**実行に必要な環境は引数で渡せる**のが望ましい。1 行の追加で、
ホスト側の `progv` という壊れやすい回避策が丸ごと不要になる。

---

## 2. P2: consumer が導出せざるを得ない情報

### 2.1 解決済み trial 予算が取得できない【ソース確認】

`resolve-trials` は `src/property-runner.lisp` の内部関数で export されておらず
（`main.lisp` の export 一覧に無い）、`property-result` にも予算の slot が無い。
`property-result-trials` は「停止した試行番号」であって予算ではない。

**consumer への影響**: cl-mcp は `property-trials` + `backend-default-trials` から
予算を**再導出**し、応答に「これは cl-mcp による導出値である」と明記している。
cl-spec が解決規則を変えると静かに乖離する。

**提案**: `property-result` に予算 slot を足すのが最小。あるいは
`resolve-trials` を export する。§72.1 の「要求した試行予算」を報告する
唯一の手段が現状は再実装である。

### 2.2 `property-result` に backend が無い【ソース確認】

仕様書 §14 の概念クラスは `(backend ...)` を挙げるが実装には無い。

**consumer への影響**: §72.3 が要求する再現 artifact の
「framework/backend/Lisp の version」を result から組み立てられず、
別途 `*generator-backend*` を読むことになる（1.5 のスレッド問題と重なる）。

### 2.3 `function-spec-data` が無い【ソース確認】

`spec-data` / `property-data` と同形の projection が function-spec に無い。
公開 reader（`function-spec-argument-specs` 他）は揃っているので consumer 側で
組み立てられるが、それは cl-spec の introspection 責務を境界の向こうに複製することになる。

**consumer への影響**: cl-mcp の `spec-describe kind=function-spec` は
`unsupported` を返し、欠けている API 名を明示する実装にした。
`defspec-function` が stub である以上、現状は登録もできないので実害は無い。
**D1 の決定時に、projection API も同時に決めてほしい。**

なお【既知】として `function-spec-argument-specs` が生 designator を持ち
`property-arguments` が正規化済み IR を持つ不揃いが followups に記録されている。
consumer から見ると、この 2 つが同形であることが projection の前提になる。

### 2.4 `run-property` の `options` が呼び出し側の値を静かに隠す【ソース確認】

```lisp
(run-generated-test backend property
                    :options (list* :trials trials :registry registry options))
```

`list*` で先頭に置くので、呼び出し側が `:options '(:trials 10)` を渡しても
`getf` は解決済みの `trials` を先に見つける。診断は出ない。

**consumer への影響**: cl-mcp は `options` を tool 引数として**公開しないこと**で
回避した（「黙って元と異なる条件で再実行する」経路を作らないため）。
公開する consumer は、渡した options が効かないことに気づけない。

**提案**: 予約キーを明示して、衝突時に `invalid-property-form` 相当を出すか、
少なくとも docstring で「`:trials` と `:registry` は runner が上書きする」と述べる。

---

## 3. P3: 外部表現に効く観測

### 3.1 生成 seed の範囲と受理 seed の範囲が違う【実測】

`make-seed` は `(random (expt 2 62))`。一方 `seed->random-state` は
`(check-type seed (integer 0))` で、実測で `2^80` も受理される。

**consumer への影響**: 「seed は 2^62 未満」と読むと外部表現の設計を誤る。
cl-mcp は seed を**10 進文字列でのみ**受け渡す設計にした（生成値 19 桁は
JSON の安全整数 2^53 を超え、JSON number にすると丸められる）。
仕様書 §14 の概念 JSON が `"seed": 18372918` と数値で書いているので、
**そのまま実装すると再現できない seed を再現できると報告することになる**。

**提案**: 仕様書 §14 の概念 JSON か §72.6 に、seed は任意精度整数であり
外部表現では文字列にすべき旨を一行足す。D7（JSON の型表現）の入力材料。

### 3.2 `:skipped` / `:pending` は現 backend から出ない【ソース確認】

`property-result` の docstring は 5 値を挙げるが `run-generated-test` は
`:passed` / `:failed` / `:error` の 3 値しか返さない。仕様書 §14 も
「すべての候補を現在の backend が返すとは限らない」と明記しており、
**この点は文書が正しい**。

**consumer への影響**: cl-mcp は 5 値すべてを扱えるようにしたうえで、
tool 説明に「現 backend は produce しない」と書いた。将来 `:skipped` が
出るようになったときに consumer が壊れない形になっている。

### 3.2b 仕様書 §14 の概念クラスは `duration`、実装は `elapsed`【ソース確認】

概念クラスの slot 名が `(duration ...)` で、実装の reader は
`property-result-elapsed`。§14 本文の実装済み一覧は `elapsed` と正しく書いており、
食い違うのは概念クラスの listing だけ。

**consumer への影響**: 実害は無いが、仕様書を先に読んだ実装者が
`property-result-duration` を探すことになる。§14 の listing を `elapsed` に
揃えるか、`backend` と同じく「概念であって現行 slot ではない」ことが
listing 単体でも読めるようにすると迷いが減る。

### 3.3 `custom-spec` の `handler` が `spec-data` から落ちる【既知】

followups §8 に記録済み。**consumer 視点の補足**として:
cl-mcp は定義の同一性を判定するため `property-data` + 到達可能な `spec-data` の
digest を取っている。`spec-data` から静かに落ちる slot があると、
**その slot だけが変わった定義変更を digest が検出できない**。
`custom-spec` は現状 normalize / dsl のどちらからも作れないので実害は無いが、
`defgenerator`（§73 の D なし・§0.2 で未実装）を実装する際に効く。

### 3.4 `*size*` が全引数の最大値まで引き上げられる【既知】

followups §8「生成の分布」に記録済み。**consumer 視点の補足**として:
これは再現性を壊さない（seed が同じなら同じ列）が、
§72.1 が要求する「生成 domain や重要な境界値の検査状況」の報告を
consumer が正しく書けない要因になる。cl-mcp は現状
`input-coverage-unmeasured` として「不明」と報告している。

---

## 4. cl-mcp が依存している API（壊さないでほしいもの）

adapter は次を前提にしている。変更する場合は事前に分かると助かる。

| API | 依存している性質 |
|---|---|
| `semantic-data` | 固定キー集合を必ず返し、未登録 symbol でも signal しない |
| `spec-data` / `property-data` | plist の形。`:children` は子がある node のみ、`:arguments` は `(:variable :spec)` |
| `properties-for` | sorted。`&optional registry`（キーワードではない） |
| `run-property` | `:profile` `:seed` `:options` `:registry` |
| `property-result-*` | 9 個の reader 名 |
| `*registry*` / `*generator-backend*` | この名前の special であること（`progv` で束縛するため） |
| condition クラス | `unknown-spec` / `unknown-property` / `no-generator-backend` / `generator-unavailable` / `cl-spec-error` |

adapter は `find-package` + `find-symbol` で遅延解決し、欠けている名前を
`environment.missing` として報告する。したがって**名前の追加は壊さない**が、
**署名の変更は静かに壊れる**（`fboundp` しか見ていないため）。
署名を変える場合は名前も変えてもらえると、consumer が
「version mismatch」として検出できる。

---

## 5. §72 の到達状況（consumer 側から見た測定結果）

§73.2 の 4「LLM-01〜06 の未達項目を明示する」への回答。

| 要件 | consumer 側の到達 | cl-spec 側に残る不足 |
|---|---|---|
| LLM-01 検証結果と検証範囲 | 部分 | **棄却件数**と**生成 domain の到達範囲**が runner から取得できない。cl-mcp は常に `rejection-counts-unmeasured` / `input-coverage-unmeasured` を付けて「不明」と報告している。1.1 の予算 0 も同じ系統 |
| LLM-02 契約の由来と変更 | **未対応** | registry が由来・作成者・レビュー状態・version を持たない（D8）。cl-mcp の `definition_digest` は内容の同一性のみで trust 状態ではない |
| LLM-03 再生成と反例の再検査 | 部分 | 生成列 replay は動く。**保存反例の直接再検査 API が無い**（D3）。artifact schema も未決 |
| LLM-04 状態・縮小・時間上限 | 部分 | 1.3（縮小の状態）と 1.4（中断後の契約）と 2.1（予算）が塞がっていない。trial 単位予算も無い |
| LLM-05 変更影響と image の整合性 | 部分 | `:about` 直接関連のみであることは consumer 側で明示できた。registry 世代・cache invalidation は D6 |
| LLM-06 機械可読境界 | 部分 | 1.2 / 1.3 が「空と取得不能の区別」を consumer 側で完結させない。3.1 は仕様書の概念 JSON の修正で足りる |

---

## 6. 次に一つだけ選ぶなら

**1.5 の `run-property` への `:backend` キーワード**を推す。

- 差分が最小（`:registry` と同じ扱いを 1 つ増やすだけ）
- §48 が「時間上限は実行ホストが管理する」と定めている以上、
  ホストは別スレッドで走らせざるを得ず、**すべてのホストがこの罠を踏む**
- 現在の回避策（consumer が special シンボルを保持して `progv`）は、
  cl-spec が読む special を 1 つ増やすたびに静かに壊れる

次点は **1.1（予算 0）** と **1.2（反例の有無）**。どちらも
§72 の受け入れ条件に直接対応し、`property-result` への小さな追加で塞がる。
