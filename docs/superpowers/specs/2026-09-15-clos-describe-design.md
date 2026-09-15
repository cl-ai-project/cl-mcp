# clos-describe（CLOS introspection ツール）設計

- 日付: 2026-09-15
- ステータス: 設計承認済み（実装未着手）
- 想定受益者: CLOS を多用するコードベースを読む・変更する AI エージェント
- ブランチ: `feat/clos-describe`

## 1. 背景と問い

提案は「quasi/cl-mcp-server の `class-info` / `find-methods` / `describe-symbol` のような
CLOS introspection を補強すべき」というものだった。

```lisp
(defgeneric process (x))
(defmethod process ((x foo)) ...)
(defmethod process ((x bar)) ...)
(defmethod process :around ((x foo)) ...)
```

の意味を静的なソース検索だけで組み立てるには、LLM は多くの探索を要する。
総称関数のメソッド（修飾子・特化子・位置）、クラス階層、直接スロットと実効スロットは
実行時にしか確定しない構造であり、`repl-eval` 任せにせず専用ツールで渡す価値がある。

調査の結論: **cl-mcp には CLOS 構造を返す手段がない。** 新ツール `clos-describe` を 1 本追加する。
あわせて、新ツールの前提になる既存の不具合（2 章の #3〜#6）を同じブランチで直す。

## 2. 現状調査（実測）

worker に `cl-mcp` とプローブ用の CLOS 定義をロードして計測した。

| # | 実測 | 影響 |
|---|---|---|
| 1 | `code-describe` は総称関数 `AREA` に `AREA :: generic-function (S)` と doc だけを返す | メソッドの数・修飾子・特化子・位置が分からない |
| 2 | `code-describe` はクラス `CIRCLE` に `class (radius)` だけを返す | 上位/下位クラス、継承スロット、initform、アクセサ、メソッドが分からない |
| 3 | `code-find` は `server-state`（defclass）、`bounded-output-stream`（defclass）、`arg-validation-error`（define-condition）の**行を返さない**（パスのみ） | SBCL はクラス定義に文字位置を記録せず、`form-path`（トップレベルフォーム番号）だけを持つ。現行コードは文字位置しか見ていない |
| 4 | `lisp-edit-form` に `form_name: "stream-write-char ((stream bounded-output-stream) character)"` を渡すと **not found**（名前だけなら一致） | `%defmethod-candidates` がラムダリストを親プロセスの `*package*` 基準で `prin1` するため、候補が `((stream cl-mcp/src/utils/bounded-stream:bounded-output-stream) character)` になる。`&optional` を含む長いラムダリストは pretty print で改行も混じる |
| 5 | `code-find-references` は上の defmethod に `form_name` `stream-write-char ((stream bounded-output-stream) character)` を返す | #4 のため、docs の「そのまま lisp-edit-form に渡せる」が CL-USER 以外のパッケージの defmethod で成り立たない |
| 6 | 主メソッド `area ((shape circle))` と `area :around ((shape circle))` が並ぶファイルで、前者の完全な form_name を渡すと **Multiple matches** になる（計画時の往復テストで発見） | `%defmethod-candidates` は修飾子付きメソッドにも「名前＋ラムダリスト」の候補を作るため、主メソッドの名前が `:around` にも一致する |

補足の実測:

- **メソッドの位置**: `sb-introspect:find-definition-source` をメソッドオブジェクトに使うと、
  文字位置は NIL で `form-path` だけが返る（`acceptor-dispatch-request` の 3 メソッドすべて）。
  `sb-pcl::%method-function-fast-function` 経由なら文字位置が取れるが、アクセサメソッドでは PCL 内部を指す。
  アクセサメソッドの `form-path` は defclass のフォーム番号を正しく指す
- **フォーム番号 → 位置**: コードオブジェクトの `sb-c::debug-source-start-positions` が
  ファイルのトップレベルフォーム開始位置のベクタを持つ。http.lisp の要素 36 は 17729 で、
  `acceptor-dispatch-request` メソッドの文字位置と一致した。
  これで `mcp-acceptor` 445 / `server-state` 16 / `arg-validation-error` 103 /
  `bounded-output-stream` 21 行を得て、4 件ともソースと一致した
- **コスト**: `sb-vm:list-allocated-objects` でコードオブジェクトを走査し、
  ファイル名 → debug-source の表を作るのに約 60ms（コードオブジェクト 27,883、ファイル 692）
- **未 finalize のクラス**: 一度も `make-instance` されていないクラスは
  `class-precedence-list` で `unbound-slot`、`class-slots` でエラーになる。
  `sb-mop:compute-class-precedence-list` は finalize せずに CPL を返し、クラスは未 finalize のまま残る。
  上位クラスが未定義（forward-referenced）なら、その名前を含むエラーになる
- **initform**: `sb-mop:slot-definition-initform` は評価前の式（`(RANDOM 10)`）を返す
- **condition / struct**: `sb-mop:class-direct-slots` / `class-slots` はどちらにも使える。
  condition のスロットには readers が載るが、struct のアクセサは readers に載らない
- **メソッド結合子**: `sb-pcl::method-combination-type-name` / `method-combination-options` で
  `+ (:most-specific-first)` を得られる
- **`print-object`** のメソッドは 215 件あり、上限なしでは出力が膨らむ

## 3. quasi/cl-mcp-server との比較

quasi の `class-info` / `find-methods`（`src/introspection.lisp`）から取り込む設計判断:

- クラスのメタクラス、直接/実効スロット、上位/下位クラス、CPL、default initargs を 1 回で返す
- 継承メソッドを集めるとき、`COMMON-LISP` / `SB-*` のクラス（`standard-object`、`t` など）に
  特化した標準プロトコルのメソッドを省き、**省いたことを明示する**

取り込まない判断:

- initform と default initarg を**評価**して表示する（`(funcall initfunction)`）。
  副作用のある initform を問い合わせで実行してしまう。本設計は式のまま返す
- 無条件の `finalize-inheritance`。問い合わせがイメージの状態を変え、
  カスタムメタクラスでは利用者のコードが走る。本設計は finalize しない
- 検索起点がクラスだけ。本設計は総称関数を起点にメソッドを並べられる
- ツールを 2 本に分ける。本設計はシンボル 1 つを受け取り、それが名指すもの（総称関数とクラスの両方でもよい）をすべて返す
- メソッドの位置がない。本設計は行と `lisp-edit-form` 用の `form_name` を返す

## 4. スコープ

含める:

- 新ツール `clos-describe`（総称関数の節、クラスの節、クラスに特化したメソッド）
- 不具合修正 #3: クラス・condition・struct とメソッドの行を `form-path` から解決する。
  `code-find` / `code-describe` もこれで行を返すようになる
- 不具合修正 #4 #5: `lisp-edit-form` の defmethod の form_name 照合を、パッケージ接頭辞と改行に依存しない形にする
- 不具合修正 #6: 完全な署名が form_name と一致するフォームがあれば、それを略記として一致するだけのフォームより優先する
- `code-describe` の本文に、総称関数・クラスのとき `clos-describe` を案内する 1 行を足す
- `inspect-object` の本文に、調べたオブジェクトが名前付きのクラスか総称関数のとき
  `clos-describe` を案内する 1 行を足す（6.6）

含めない（非ゴール）:

- 実効メソッドの計算（引数の型を与えたときの dispatch 順のシミュレーション）
- struct のアクセサ（MOP の readers に載らない。制限事項として docs に書く）。
  struct のスロットは SBCL がいつも initfunction を持たせるので、`:initform` を書かなくても `NIL` と表示される
- `compute-slots` などをカスタマイズしたメタクラスでの、未 finalize 時の厳密な実効スロット
- 推移的な下位クラスの木（直接の下位クラスだけを返す）
- `symbol` に `(setf foo)` と書ける入力形式（`foo` を問い合わせれば `(setf foo)` の総称関数も返す）

## 5. アーキテクチャ

### 5.1 採用案: worker で実行時情報、親でソース注釈、RPC は 1 往復

```
parent: src/clos.lisp (define-tool "clos-describe")
  │ limit を検証（不正なら worker を呼ぶ前に arg-validation-error）
  │
  ├── worker/clos-describe ──────────────▶ worker: clos-describe-report
  │     (symbol, package, limit)            resolve-target（intern しない）
  │                                         sb-mop で総称関数 / クラスを読む
  │                                         各定義の abs_path / path / line / stale を解決
  │◀──────────── report（content なし）──────
  │
  │ report でなければ（isError を持つクラッシュ通知や Worker error）そのまま返す
  │ annotate-report-forms: 定義ごとに abs_path と line から
  │   top-level-forms-at でフォームを引き、form_type / form_name を付ける
  │   abs_path を取り除く
  │ build-clos-describe-response: content テキストを組み立てる
  ▼
```

worker プールを使わない場合も同じ 3 段（report → 注釈 → 組み立て）を同じプロセスで通す。

親で注釈する理由:

- worker イメージは eclector と `cl-mcp/src/cst` をロードしない
- 実行時情報だけでは form_type を決められない。マクロが生成した defmethod、
  defgeneric 内の `(:method ...)`、defclass 由来のアクセサがそうである
- `code-find-references` と同じ `%form-metadata` を使えば、両ツールの form_name の書式が揃う

### 5.2 ファイル構成

| ファイル | 変更 | 役割 |
|---|---|---|
| `src/code-core.lisp` | 追記 | `definition-source-line`、`definition-source-location`、`with-definition-source-cache`（5.3）。`code-find-definition` がこれを使う（xref の位置を作る `%definition->path/line` は変えない。`code-find-references` の結果を変えないため）。`generic-function-method-count`（6.5） |
| `src/clos-core.lisp` | 新規（worker） | `clos-describe-report`。総称関数・メソッド・クラス・スロットを JSON 化できる hash-table にする |
| `src/code-refs-scan.lisp` | 追記（親） | `top-level-forms-at`（5.4） |
| `src/lisp-edit-form-core.lisp` | 修正 | `%defmethod-candidates` と `%find-target` の照合（5.5） |
| `src/tools/clos-response-builders.lisp` | 新規 | `annotate-report-forms`、`build-clos-describe-response` |
| `src/clos.lisp` | 新規 | `define-tool "clos-describe"` |
| `src/tools/response-builders.lisp` | 修正 | `build-code-describe-response` にヒント行 |
| `src/inspect.lisp` | 修正 | `inspect-object-by-id` が `hint` を付け、`format-inspect-elements` がそれを出す（6.6） |
| `src/worker/handlers.lisp` | 修正 | `%handle-clos-describe` を `worker/clos-describe` に登録 |
| `src/tools/all.lisp`、`main.lisp`、`tests.lisp` | 修正 | 新ファイルの登録（`cl-mcp.asd` は変更不要） |

### 5.3 行の解決（worker 側）

`definition-source-line (source)` は sb-introspect の definition-source から 1 始まりの行を返す。

1. 文字位置があれば、既存の `%offset->line` で行にする
2. 文字位置がなく、`pathname` と `form-path` があれば、その先頭要素をトップレベルフォーム番号 N とし、
   ファイルのトップレベルフォーム開始位置の N 番目を `%offset->line` で行にする。開始位置は次の順に求める
   1. 同じファイルの debug-source の `start-positions`（独自リーダーマクロを使うファイルでも正確）
   2. 無ければ、ファイルを標準リードテーブル・`*read-suppress*` t・`read-preserving-whitespace` で読み、
      各フォームを読む直前の `file-position` を集める（`%read-form-starts`）
3. どちらもできなければ NIL

2-2 が要る理由: debug-source はそのファイルからコンパイルされた関数が生きている間しか残らない。
defclass だけのファイルはロード後にコードが残らず、`(sb-ext:gc :full t)` の後には開始位置が消えて
行が取れなくなった（計画時の実測）。2-2 の位置は、`#+(or)`・多段の読み取り条件・`#.`・ブロックコメント・
マルチバイト文字を含むプローブと、cl-mcp の src 66 ファイルのすべてで、コンパイラが記録した位置と一致した。
読み取った位置も `with-definition-source-cache` の中ではファイルごとに 1 回だけ求める。

ファイル名 → debug-source の表は、`sb-vm:list-allocated-objects :all :type sb-vm:code-header-widetag`
でコードオブジェクトを走査して作る。

- 同じファイル名の debug-source が複数あれば（再ロード）、`debug-source-created` が最新のものを使う。
  同じ秒のものが複数あれば、記録したフォーム数が多いものを使う。`defpackage` で始まるファイルをコンパイルすると、
  パッケージができる前に読んだフォームだけを記録した短い debug-source が同じ秒に並ぶため（実装時に発見）
- 選んだ記録がフォーム番号 N に届かなければ、2-2 のファイル読み取りに切り替える
- 表は `with-definition-source-cache` が束縛する動的変数に置き、最初に必要になったときに 1 回だけ作る
- 1 回の `clos-describe` や `code-find` でメソッドを 50 件解決しても、走査は 1 回で済む
- 呼び出しをまたいではキャッシュしない。再ロードで古くなるのを避けるため
- `N` がベクタの範囲外なら NIL にする

メソッドの位置にはメソッドオブジェクトの definition-source を使う。
fast-function の文字位置は、アクセサメソッドで誤るので使わない。
`repl-eval` で定義されたもの（パス `repl-eval`）は line を NIL にする（現状どおり）。

`stale` は既存の `%source-stale-p` で判定する（ファイルの書き込み時刻 > 記録された時刻）。
記録された時刻は definition-source の `file-write-date` を使い、それが NIL なら
上で選んだ debug-source の `debug-source-created` を使う。どちらも NIL なら `stale` は偽とする。

### 5.4 form_type / form_name の付与（親側）

`top-level-forms-at (abs-path lines)` は、ファイルを 1 回だけ読んで、LINES の各行で**始まる**
トップレベルフォームの `form_type` / `form_name` を返す。

- 読み取りの可否は `%readable-path`（`fs-read-file` と同じ読み取りポリシー）で判定する
- パースは `scan-text` と同じく `parse-top-level-forms` に `:source-path` を渡し、
  `in-package` を追跡して `%form-metadata` を呼ぶ
- 行の一致は `cst-node-start-line` と比べる。`#+feature` で包まれたフォームは `%unwrap` した中身の開始行とも比べる
- 戻り値は行 → `(form-type . form-name)` の表と、失敗理由（読み取り禁止 / パース不能）

`annotate-report-forms` は report 内のすべての位置付き要素を集め、`abs_path` ごとに
`top-level-forms-at` を 1 回呼ぶ。対象は総称関数、各メソッド、クラス、クラスのメソッドである。

| 状況 | form_type / form_name | note |
|---|---|---|
| その行で始まるフォームがある | 付ける | なし |
| その行で始まるフォームがない | null | `stale` なら `file changed since load; reload for accurate results`、そうでなければ `no top-level form starts at this line` |
| 読み取りポリシーの外（SBCL 自身のソースなど） | null | なし（本文は path:line だけを出す） |
| パース不能（`#.` を含むファイルなど。CST は `*read-eval*` を無効にして読む） | null | `file could not be parsed: <理由の 1 行目>` |

注釈を終えたら `abs_path` を取り除き、応答に絶対パスを残さない。

### 5.5 lisp-edit-form の defmethod 照合の修正

`%defmethod-candidates` が qualifier とラムダリストを文字列にするとき:

- キーワード以外のシンボルは**名前だけ**（パッケージ接頭辞なし）で書く
- `*print-pretty*` は t のまま `*print-right-margin*` を十分大きくし、改行を入れない。
  `'foo` が `(quote foo)` にならないようにするため
- 小文字化と `#:` の除去は現状どおり

`%find-target` は利用者の `form_name` を次のように正規化してから比べる。

- `pkg:` / `pkg::` の接頭辞を落とす。先頭がコロンのキーワード（`:around`、`(eql :unit)`）は残す
- 連続する空白（改行を含む）を 1 つの空白にする

これにより、パッケージが違うだけの同名メソッドは衝突しうる。その場合は既存の
`Multiple matches ... Specify an index` と `[N]` 指定で選ぶ。

`[N]` が付いていないとき、一致したフォームのうち**最も詳しい候補（署名全体）**が form_name と完全に
等しいものがあれば、それだけに絞る（#6）。`area ((shape circle))` は主メソッドに完全一致し、
`:around` メソッドには略記として一致するだけなので、主メソッドが選ばれる。

`%form-metadata`（`code-find-references` と `clos-describe` の form_name）も同じ候補生成を使うので、
両ツールの form_name は修正後の照合と必ず一致する。

## 6. インターフェース

### 6.1 引数

| 名前 | 型 | 既定 | 説明 |
|---|---|---|---|
| `symbol` | string（必須） | - | `pkg:name` / `pkg::name` / `name`。単一コロンで内部シンボルも引ける |
| `package` | string | `CL-USER` | `symbol` が修飾されていないときに使うパッケージ |
| `limit` | integer | 50 | 1 つのメソッド一覧に並べる最大件数。総数は常に返す。正の整数以外は `arg-validation-error` |

### 6.2 構造化データ

トップレベル:

- `symbol`, `symbol_status`（`found` / `not_found` / `package_not_found`）, `resolved_symbol`,
  `symbol_kind`（`code-refs-core:symbol-kind`）, `lookup_package`, `lookup_name`
- `generic_functions`（配列、0〜2 要素）: シンボルの関数と `(setf シンボル)` の関数のうち、総称関数であるもの
- `class`（オブジェクトまたは null）
- `limit`, `notes`（配列）

総称関数オブジェクト:

- `name`: `CLOS-PROBE::AREA` / `(SETF CLOS-PROBE::LABEL)`
- `lambda_list`（文字列）, `documentation`
- `method_combination`: `standard` / `+ :MOST-SPECIFIC-FIRST`
- `path`, `line`, `stale`, `form_type`, `form_name`: defgeneric の位置。defmethod だけで暗黙に作られた総称関数では `path` が null
- `method_count`, `methods`（`limit` 件まで）, `truncated`

メソッドオブジェクト:

- `generic_function`（名前）, `qualifiers`（`[":AROUND"]`、`["+"]`）
- `specializers`: 完全修飾の文字列。`CLOS-PROBE::CIRCLE`、`COMMON-LISP:T`、`(EQL :UNIT)`。
  eql の対象は `*package*` を KEYWORD にして `prin1` する。本文では問い合わせたシンボルの
  パッケージと `COMMON-LISP` の接頭辞を落として `CIRCLE`、`T` と出す
- `kind`: `method` / `reader` / `writer`。アクセサメソッド（`standard-reader-method` / `standard-writer-method`）の場合は `slot` も返す
- `via`: クラスの `methods` にだけ付く。特化しているクラス名
- `path`, `line`, `stale`, `form_type`, `form_name`, `note`

クラスオブジェクト:

- `name`, `metaclass`（`STANDARD-CLASS` / `SB-PCL::CONDITION-CLASS` / `STRUCTURE-CLASS` など）, `documentation`
- `path`, `line`, `stale`, `form_type`, `form_name`
- `finalized`（真偽値）
- `direct_superclasses`, `direct_subclasses`（完全修飾名の配列）
- `precedence_list`（配列、計算できなければ null）, `undefined_superclasses`（forward-referenced な祖先の名前）
- `direct_slots`, `effective_slots`（配列、計算できなければ null）
- `default_initargs`: `initarg`, `form`, `from`
- `method_count`, `methods`（`limit` 件まで）, `truncated`, `omitted_classes`

スロットオブジェクト:

- `name`, `from`（実効スロットだけ。定義している最も特定的なクラス）
- `initargs`（配列）
- `initform`: 評価前の式を印字した文字列。initform なしは null、`:initform nil` は `"NIL"`
- `type`（文字列、既定 `T`）, `allocation`（`instance` / `class`）
- `readers`, `writers`: 実効スロットでは CPL 上の同名の直接スロットすべての和
- `documentation`

式と名前の印字:

- 式は、`*package*` をクラス名のホームパッケージにし、
  `*print-length*` 10 / `*print-level*` 4 / `*print-right-margin*` 大 で `prin1` する
- JSON の名前は `qualified-symbol-name` で完全修飾する。名前のない（または別のクラスに名前を奪われた）クラスは、
  上位クラス・CPL・`from`・`via`・`omitted_classes` では COMMON-LISP-USER で印字した `#<STANDARD-CLASS NIL {...}>` で出し、
  `direct_subclasses` からは省く（実装時のレビューで発見。以前は `COMMON-LISP:NIL` と出ていた）

### 6.3 本文テキスト

クライアントが表示するのは `content[].text` だけなので、判断に必要な情報はすべて本文に載せる。
名前は問い合わせたシンボルのホームパッケージから見た表記で出す。

```
Generic function CLOS-FIXTURE::AREA (S) — standard combination, 4 methods
Area of S.
Defined at tests/fixtures/clos-fixture.lisp:17 (defgeneric area)
  :AROUND (CIRCLE)  tests/fixtures/clos-fixture.lisp:27 (defmethod area :around ((s circle)))
  (CIRCLE)          tests/fixtures/clos-fixture.lisp:21 (defmethod area ((s circle)))
  (SQUARE)          tests/fixtures/clos-fixture.lisp:24 (defmethod area ((s square)))
  ((EQL :UNIT))     tests/fixtures/clos-fixture.lisp:17 (defgeneric area)

Class CLOS-FIXTURE::CIRCLE (standard-class, not finalized) — tests/fixtures/clos-fixture.lisp:10 (defclass circle)
Superclasses: SHAPE
Subclasses: (none)
Precedence: CIRCLE SHAPE STANDARD-OBJECT SB-PCL::SLOT-OBJECT T
Slots (3):
  RADIUS    direct      :initarg :RADIUS :initform (RANDOM 10) :type REAL  accessor RADIUS
  NAME      from SHAPE  :initarg :NAME :initform "anon"  reader SHAPE-NAME
  REGISTRY  from SHAPE  :allocation :class :initform (MAKE-HASH-TABLE)
Default initargs: (none)
Methods (6; standard protocol on STANDARD-OBJECT, T omitted):
  AREA :AROUND (CIRCLE)             tests/fixtures/clos-fixture.lisp:27 (defmethod area :around ((s circle)))
  AREA (CIRCLE)                     tests/fixtures/clos-fixture.lisp:21 (defmethod area ((s circle)))
  RADIUS (CIRCLE) [reader]          tests/fixtures/clos-fixture.lisp:10 (defclass circle)
  (SETF RADIUS) (T CIRCLE) [writer] tests/fixtures/clos-fixture.lisp:10 (defclass circle)
  (SETF LABEL) (T SHAPE) via SHAPE  tests/fixtures/clos-fixture.lisp:32 (defmethod (setf label) (v (s shape)))
  SHAPE-NAME (SHAPE) [reader] via SHAPE  tests/fixtures/clos-fixture.lisp:5 (defclass shape)
Note: not finalized; precedence list and slots were computed without finalizing the class.
```

行番号は説明用の例で、実際のフィクスチャの行とは一致しない。

- スロット行の reader / writer: 同じ総称関数名の reader と `(setf 名前)` writer がそろっていれば `accessor 名前` とまとめ、そうでなければ `reader 名前` / `writer 名前` と出す
- 位置が取れないメソッドは `(no source)`、form_name がなければ `path:line` だけを出す
- `limit` を超えたら `… and 180 more (limit 50)` と書く
- 状態の文言:
  - `not_found`: `Symbol X not found in package Y (nothing was interned). Is the system loaded?`
  - `package_not_found`: `Package Y not found.`
  - クラスでも総称関数でもない: `X names a function, not a generic function or class; code-describe describes it.`
  - 何も名指さない: `X names nothing in this image. Is the system loaded?`

### 6.4 判定ルール（worker 側）

**総称関数のメソッドの並び**:

- standard 結合子なら `:around` → `:before` → 修飾子なし → `:after` → その他の順
- 同じ役割の中は、project root 内のファイル → 外のファイル → 位置なし の順とし、それぞれ path・line で並べる
- standard 以外の結合子なら、役割でまとめず上の path 順だけで並べる

**アクセサの判定**: メソッドが `sb-mop:standard-accessor-method` なら、
`accessor-method-slot-definition` のスロット名を `slot` にし、reader / writer を `kind` にする。

**CPL**:

- finalize 済みなら `class-precedence-list`
- そうでなければ `compute-class-precedence-list`
- エラーなら null とし、CPL をたどれる範囲で forward-referenced な祖先を `undefined_superclasses` に集める

**実効スロット**:

- finalize 済みなら `class-slots` を使い（集合と順序はメタクラスが決めたもの）、`from` と readers / writers を直接スロットから補う
- 未 finalize で CPL が取れれば、CLHS 7.5.3 の規則で CPL 上の直接スロットを名前ごとにマージする:
  - allocation と documentation: 最も特定的なもの
  - initform: initform を持つ最も特定的なもの
  - initargs: 和
  - type: T 以外の型が 1 つならそれ、複数なら `(AND ...)`
- あわせて `notes` に `not finalized; precedence list and slots were computed without finalizing the class` を足す
- CPL が取れなければ null

**default initargs**:

- finalize 済みなら `class-default-initargs`
- そうでなければ CPL 上の `class-direct-default-initargs` を、同じキーは最初のものだけ残して並べる
- `from` はそのキーを直接持つ最も特定的なクラス

**クラスのメソッド**:

- CPL（取れなければクラス自身だけ）の各クラス C について `specializer-direct-methods` を集め、`via` を C にする
- 問い合わせたクラス自身は、言語レベルのクラスでも省かない
- 上位クラスのうち、クラス名のパッケージが `COMMON-LISP` か `SB-` で始まるものは省く。
  その中で直接メソッドを持つものを `omitted_classes` に入れる
- 同じメソッドが複数の C で見つかったら、最初（最も特定的な C）だけを残す
- 並びは、C の CPL 順 → 総称関数名 → 役割の順とする。総称関数名は基になるシンボル名で比べ、
  同じ名前なら `x` を `(setf x)` より前に置く（文字列のまま比べると `(` が先頭に来るため）

**finalize しないことの保証**: report はどの経路でも `finalize-inheritance`、`make-instance`、
initfunction の呼び出しを行わない。

### 6.5 既存ツールの変更

- `code-find` / `code-describe`: クラス・condition・struct の `line` を返すようになる（5.3）
- `code-describe`: `code-core` に `generic-function-method-count (symbol-name &key package)` を足す
  （`resolve-target` で解決し、総称関数ならメソッド数、そうでなければ NIL）。
  `code-describe-symbol` の戻り値は変えない（ftype 宣言が値の数を固定しているため）。
  `build-code-describe-response` はキーワード引数 `:method-count` を受け取り、`type` とあわせて本文の最後に次を足す:
  - 総称関数: `4 methods; clos-describe lists them with their specializers and source lines.`
  - クラス / condition / struct: `clos-describe shows its slots, superclasses, subclasses and methods.`
- `lisp-edit-form`: defmethod の form_name 照合（5.5）

### 6.6 inspect-object の案内

`inspect-object` は値を見るツールで、クラスオブジェクトには PCL 内部スロットのダンプを返す
（`%TYPE`、`WRAPPER`、`CAN-PRECEDE-LIST` など）。総称関数には `[function] #<STANDARD-GENERIC-FUNCTION AREA (4)>` の
1 行しか返さない（実測）。型の設計を知りたい利用者を `clos-describe` へ案内する。

- `inspect-object-by-id` が、調べた**ルートのオブジェクト**について判定し、結果の hash-table に `hint`（文字列）を足す。
  ネストした要素と `repl-eval` の `result_preview`（`generate-result-preview`）には付けない
- 判定:
  - クラス: `(typep object 'class)` で、`class-name` がシンボルであり、かつ `(find-class name nil)` が
    そのオブジェクト自身であるもの。名前のない・置き換えられたクラスは対象外
  - 総称関数: `(typep object 'generic-function)` で、`generic-function-name` がシンボルか `(setf シンボル)` であり、
    その名前の `fdefinition` がそのオブジェクト自身であるもの
- 文言（シンボルは `qualified-symbol-name` で完全修飾する。`(setf foo)` の場合は基のシンボル `foo` を案内する）:
  - クラス: `This is the class CLOS-PROBE::CIRCLE; clos-describe CLOS-PROBE::CIRCLE shows its slots, superclasses, subclasses and methods with source lines.`
  - 総称関数: `This is the generic function CLOS-PROBE::AREA; clos-describe CLOS-PROBE::AREA lists its methods with their specializers and source lines.`
- `format-inspect-elements` は `[object-id: N]` 行の直後に `Hint: <hint>` を出す
- 判定で何かエラーが起きても `hint` を付けないだけにし、検査の結果は変えない

## 7. エラー処理

| 状況 | 扱い |
|---|---|
| `limit` が正の整数でない | worker を呼ぶ前に `arg-validation-error` |
| シンボルやパッケージが存在しない | エラーにせず `symbol_status` と本文で明示。intern しない |
| クラスでも総称関数でもない | エラーにせず、何を名指しているかと `code-describe` を案内する |
| 上位クラスが未定義 | `precedence_list` / `effective_slots` を null にし、`undefined_superclasses` と note を出す |
| 個々のメソッドやスロットの読み取りでエラー（特殊なメタクラスなど） | その要素だけ該当フィールドを null にして `note` に 1 行のエラー要約。全体は失敗させない |
| 行が取れない（repl-eval 定義、SBCL 自身のソース、範囲外のフォーム番号） | `line` を null |
| ロード後にファイルが変わった | `stale` を立て、form_name が引けなければ note |
| 注釈でファイルが読めない・パースできない | 5.4 の表のとおり |
| worker のクラッシュ・タイムアウト・Worker error | proxy が返す結果（`isError` 付き）をそのまま返す |
| JSON 経由の値 | 配列は list でも vector でも、偽は NIL でも `yason:false` でも受ける（`sequence->list` を使う） |

## 8. テスト

report の組み立ては純粋な関数に分け、MOP オブジェクトから plist / hash-table を作る部分と
位置解決の部分を別々にテストできるようにする。

| テストファイル | 対象 | 主なケース |
|---|---|---|
| `tests/clos-core-test.lisp`（新規） | worker 側の report | 下記フィクスチャの全ケース、メソッドの並び、`kind` と `slot`、`method_combination`、暗黙の総称関数、CPL と実効スロットと `from`、default initargs の `from`、クラスのメソッドと `omitted_classes`、**問い合わせ後もクラスが未 finalize のまま**、**`find-symbol` の結果が変わらない**（intern しない）、`limit` と `truncated`、forward-referenced |
| `tests/code-test.lisp`（更新） | 行解決と既存ツール | `code-find` / `code-describe` がクラス・condition・struct の行を完全一致で返す、ヒント行、`definition-source-line` の範囲外番号 |
| `tests/lisp-edit-form-test.lisp`（更新） | 照合の修正 | CL-USER 以外のパッケージの defmethod を `name ((x class) y)` で引ける、接頭辞付きの form_name でも引ける、`&optional` を含む長いラムダリスト、`(eql :key)`、`(setf name)` メソッド、パッケージ違いの同名メソッドの `[N]` |
| `tests/code-refs-scan-test.lisp`（更新） | `top-level-forms-at` | 行一致、`in-package` 切り替え後の form_name、`#+sbcl` 付きフォーム、一致なし、読み取り禁止、パース不能 |
| `tests/clos-response-builders-test.lisp`（新規） | 注釈と本文 | 6.3 の各行、状態の文言、`truncated`、位置なし、list と vector・NIL と `yason:false` の両方の入力、isError 結果の素通し、`abs_path` が応答に残らない |
| `tests/inspect-test.lisp`（更新） | 案内行 | クラスオブジェクト・総称関数・`(setf foo)` の総称関数で `hint` と `Hint:` 行が出る。インスタンス、名前のないクラス、普通の関数、ネストした要素、`generate-result-preview` には出ない |
| `tests/tools-test.lisp` / `tests/worker-test.lisp`（更新） | 登録と経路 | `tools/list` に載る、`limit` 検証、worker ハンドラの登録と JSON 往復 |
| 往復テスト（`tests/clos-core-test.lisp` 内） | form_name の約束 | フィクスチャの全メソッドとクラスについて、`clos-describe` の form_type / form_name で `lisp-edit-form` の dry_run が成功する |

フィクスチャ `tests/fixtures/clos-fixture.lisp`（独自パッケージ）に入れるケース:

- マルチバイト文字のコメント（バイト位置と文字位置のずれ）
- 上位クラスを持つクラス、`:allocation :class`、`:type`、`:documentation`、`:default-initargs`
- `defgeneric` とその中の `(:method ...)`（eql 特化子）
- 主メソッド 2 つと `:around` メソッド
- `(setf name)` の総称関数とメソッド
- `+` 結合子の総称関数
- `defgeneric` なしで `defmethod` だけの総称関数
- アクセサ（reader / accessor）
- `define-condition`、`defstruct`
- 未定義の上位クラスを持つクラス
- `&optional` を含むラムダリストのメソッド
- 別パッケージの総称関数へのメソッド（`print-object` など）

既知の落とし穴:

- JSON の偽は inline で `yason:false`、worker 経由で NIL になる。builder はどちらも受ける
- src に export を足すと、稼働中の親イメージはそのシンボルを参照するファイルを読めなくなる。
  検証は `rove cl-mcp.asd` の新規プロセスで行う
- フィクスチャのクラスを他のテストが `make-instance` すると finalize されてしまう。
  未 finalize を確かめるクラスは、そのテスト専用にする

## 9. 完了条件

- `mallet src/*.lisp src/*/*.lisp tests/*.lisp` が通る
- `(asdf:compile-system :cl-mcp :force :all)` で新しい warning が出ない
- `rove cl-mcp.asd` で全スイートを実行し、終了コードではなく ✓ の数と `;; testing` 行数で確認する
- cl-mcp 自身と依存ライブラリに対して実行し、所要時間と出力量を記録する。対象は
  `hunchentoot:acceptor`、`hunchentoot:acceptor-dispatch-request`、`print-object`（`limit` 既定）
- docs を更新する:
  - `docs/tools.md` に `clos-describe` の節を足し、`code-describe` の type 一覧（現状 `function|macro|variable|unbound` のまま古い）と行の記述を直す。`inspect-object` の節に `hint` を足し、値を見る `inspect-object` と型の設計を見る `clos-describe` の使い分けを 1 段落で書く
  - `prompts/repl-driven-development.md` の早見表、worker 側ツール一覧、Tool Selection に「クラス・総称関数の構造を知るなら clos-describe」を足す
  - `CLAUDE.md` のアーキテクチャ表に足す
  - README にツール一覧があれば、そこにも足す

## 10. リスク

設計時に解消したもの:

| リスク | 結果 |
|---|---|
| メソッド・クラスの行が取れない | `form-path` と `debug-source-start-positions` で 4 件ともソースと一致（2 章） |
| 未 finalize のクラスで CPL が取れない | `compute-class-precedence-list` は finalize せずに返す |
| worker が form_type を決められない | 親でソースから引く（5.4） |

残るもの:

| リスク | 確認方法 | 外れた場合 |
|---|---|---|
| `sb-c::debug-source-start-positions`、`sb-vm:list-allocated-objects`、`sb-pcl::method-combination-*` は SBCL の内部 API | テストで値を固定し、各呼び出しを `ignore-errors` で包む | その項目を null にして動作は続ける |
| ヒープ走査のコストがイメージの大きさに比例する | 完了条件の実測。走査は必要なときだけ、1 呼び出しに 1 回 | ファイル名ごとの遅延探索に変える |
| `debug-source-created` が NIL のことがある（実測で遭遇） | 比較では NIL を 0 とみなす | - |
| debug-source が GC で消えたファイルでは、記録された時刻が取れず `stale` を判定できない | 制限事項として docs に書く | `stale` は偽のまま。行は 2-2 で求める |
| 独自リーダーマクロを使い、かつ debug-source も消えたファイル | 2-2 の読み取りが失敗して行は NIL | path だけを出す |
| `#+feature` で包んだ定義で、`%offset->line` の行と CST の開始行がずれる | `top-level-forms-at` のテスト | `%unwrap` した中身の行でも比べる（5.4 に反映済み） |
| eql 特化子の対象がシンボルで、ソースが `(eql 'foo)` のとき、本文の特化子表記 `(EQL FOO)` とソースの字面が違う | form_name はソースから引くので、編集には影響しない | - |
| パッケージ接頭辞を無視する照合で、同名メソッドが衝突する | `lisp-edit-form-test` | 既存の `[N]` 指定で選ぶ |
