# clos-describe の fail-closed なソース対応付けと編集ガード 設計

- 日付: 2026-09-16
- ステータス: 設計承認済み（実装未着手）
- ブランチ: `feat/clos-describe`（PR #156 の続き。着手時 HEAD `264dd82`）
- 想定受益者: `clos-describe` の結果で `lisp-edit-form` を呼ぶ AI エージェント

## 1. 目的

A. `clos-describe` が観察した実行時の定義について、**現在のソースで同じ定義だと確認できたときだけ**
   編集用の `form_type` / `form_name` を返す。

B. その編集情報を使うとき、**観察後にファイルや対象フォームが変わっていたら、書き込み前に失敗する**。

原則: **「別の定義だと分からなかった」は「同じ定義だと確認できた」ではない。**

保証するのは *確認した定義の識別* と *編集対象の一致* だけである。ロード済みコードとソース本文の
完全一致、プログラムの振る舞いの正しさ、外部プロセスを含む完全なファイルトランザクションは保証しない。
文書にもそう書く。

## 2. 現状の問題（HEAD 264dd82 で実測）

| # | 実測 | 影響 |
|---|---|---|
| 1 | `%same-name-p`（`src/tools/clos-response-builders.lisp`）はパッケージ接頭辞を落として `string-equal` で比べる | `pkg-a::foo` と `pkg-b::foo`、`:foo` と `pkg-a::foo`、`\|Foo\|` と `\|FOO\|` が一致してしまう |
| 2 | `%same-specializers-p` は EQL の datum が空・比較不能なら一致扱い | `(eql "old")` → `(eql "new")` の差し替えを検出できない |
| 3 | 照合は二値（受理／`*note-different-definition*`）。署名を持たないフォーム（ユーザーマクロ、`progn`、`eval-when`）は受理 | 確認できていないものを編集可能として返す |
| 4 | 親のパーサはスタブパッケージを作る。`(defmethod area ((s pkg-a:circle)))` を親で読むと値は `#:circle`（未 intern） | 親の値だけでは実シンボルと同一か判定できない |
| 5 | `top-level-forms-at` は行 → 先頭 1 フォームだけを持つ | 同一行に複数のトップレベルフォームがあると取り違える |
| 6 | 返した `form_name` を `lisp-edit-form` に渡したとき、同じフォームに解決される確認をしていない | 編集側の緩い照合が別フォームを選びうる |
| 7 | `defgeneric` 内 `(:method ...)` は総称関数名だけ、アクセサはクラス名だけで受理 | 内包メソッドやスロットの差し替えを検出できない |

先行コミット `34f3ea1` の `%read-form-starts` への読み取りポリシー適用は保持する。
アクセス方針の変更やヒープ走査の性能改善はこの設計に含めない。

## 3. A: ソース対応付けの厳密化

### 3.1 三値の判定

各定義（総称関数・クラス・メソッド）は次のいずれかを持つ。

| 状態 | 意味 | 編集用フィールド |
|---|---|---|
| `matched` | 必要な識別情報がすべて揃い、同じ定義だと確認できた | `form_type` / `form_name` を返す |
| `mismatched` | 識別情報に明確な不一致がある | 返さない |
| `unverified` | 情報不足・未対応構文・比較不能・解決不能 | 返さない |

合成規則: 構成要素に 1 つでも `mismatched` があれば `mismatched`。無く、`unverified` があれば `unverified`。
必須要素がすべて `matched` のときだけ `matched`。

例外、欠落フィールド、印字失敗、旧形式データ、未対応フォームを `matched` にフォールバックさせない。

JSON: 各要素に `source_match`（上の 3 語）と `source_match_reason`（英語 1 文、`matched` では null）。
`matched` のときだけ `form_type` / `form_name` を入れる。`mismatched` / `unverified` でも
`path` / `line` / `stale` と実行時情報は返す。本文テキストは `matched` のときだけ `(form_type form_name)` を
書き、そうでなければ `path:line [unverified: <reason>]` のように状態と理由を書く。
**テキストだけが編集を促し JSON は未検証、という不整合を作らない。**

`stale`（ファイルの更新時刻が記録より新しい）が真なら `unverified` 以上にしない。
逆に `stale` が偽であることや mtime の一致を、同一性の根拠にしない。

### 3.2 構造化識別子（表示文字列を識別に使わない）

**worker → 親**（報告の各要素に `identity`）:

```
identity:
  kind: "method" | "generic-function" | "class"
  generic_function: {package: "PKG-A", name: "AREA", setf: true|false}   ; method / generic-function
  qualifiers: [{package: "KEYWORD", name: "AROUND"}, ...]                ; method
  specializers: [ {kind: "class",  package: "PKG-A", name: "CIRCLE"}
                | {kind: "eql",    datum: <tagged datum, 3.3>}
                | {kind: "unverifiable", reason: "<1 文>"} ]             ; method
                ; 無名クラス・forward-referenced クラス・その他の特化子は unverifiable
  class: {package: "PKG-A", name: "CIRCLE"}                              ; class / accessor
  slot:  {package: "PKG-A", name: "RADIUS"}                              ; accessor
  access: "reader" | "writer"                                            ; accessor
  ; class / slot / access はメソッドの identity に常に存在し、アクセサ以外では null
  ; 通常のクラスのアクセサは MOP の standard-accessor-method として判る。
  ; それ以外の素の standard-method をアクセサ扱いするのは特化子が CONDITION の
  ; 部分型のときだけ（SBCL は define-condition のリーダを standard-accessor-method
  ; にしないため）。通常のクラスで生成されたリーダを手書きの defmethod が上書きした
  ; 場合は、アクセサではなく普通のメソッドとして報告する。
```

パッケージ名は `package-name`（プライマリ名）、`name` は `symbol-name` をそのまま（大小文字を保つ）。

**親 → worker**（`source_signature`。CST のトークンを*書かれたまま*送る）:

```
source_signature:
  head:        {token: "defmethod", in_package: "PKG-A"}      ; 演算子も解決して確認する
  name:        {token: "area", in_package: "PKG-A"}           ; (setf x) は setf: true と name トークン
  qualifiers:  [{token: ":around", in_package: "PKG-A"}, ...]
  specializers: [ {kind: "class", token: "pkg-a:circle", in_package: "PKG-A"}
                | {kind: "eql",   datum: <tagged datum, 3.3>}
                | {kind: "unverifiable", reason: "..."} ]
  class / slot / access: アクセサとクラスの場合
```

`token` は CST ノードの範囲から取ったソースの文字列そのもの。`in_package` はその位置で有効な
`in-package` のパッケージ名（無ければ null）。

例外は特化子を書いていない必須パラメータで、引用すべきソーストークンが無いため
`{kind: "class", token: "T", in_package: "COMMON-LISP"}` を合成する。これが指すのは常に標準クラス
`COMMON-LISP:T` であって、ファイル自身のパッケージが `T` をどう見ているか（`(:use)` で
`COMMON-LISP` を継承していない、あるいは `(:shadow #:t)` している）には依存しない。ファイルの
パッケージで解決すると、手を加えていないメソッドが `unverified`、`T` を shadow しているパッケージ
では `mismatched` になってしまう。

**worker 側の解決**: `code-refs-core:parse-symbol-text` でトークンを分解し、`find-package` と
`find-symbol` だけで解決する（`intern` しない）。解決したシンボルと、`identity` の
`(find-symbol name package)` が `eq` かどうかで判定する。
どちらかが解決できなければ `unverified`。

禁止: パッケージ接頭辞の除去、`string-equal` / `equalp` / 表示文字列の `string-downcase` による同一性判定。

### 3.3 EQL 特化子（式と評価結果を区別する）

ソースの `(eql FORM)` の `FORM` は定義時に評価される式であり、worker の
`eql-specializer-object` はその評価結果である。印字結果同士の比較を根拠にしない。

親が CST から作れる **タグ付き datum**（許可リスト）:

| kind | 内容 | worker 側の判定 |
|---|---|---|
| `integer` | `{value: "-12345678901234567890"}`（10 進文字列） | `(and (integerp o) (= o (parse-integer v)))` |
| `ratio` | `{numerator: "1", denominator: "3"}` | 分子分母を `parse-integer` して比較 |
| `character` | `{value: "A"}`（1 文字の文字列） | `(and (characterp o) (char= o c))` 大小文字を区別 |
| `keyword` | `{name: "UNIT"}` | `(and (keywordp o) (string= (symbol-name o) name))` |
| `boolean` | `{value: "T"}` / `{value: "NIL"}` | `(eq o t)` / `(null o)` |
| `symbol` | `{token: "foo", in_package: "PKG-A", quoted: "reader"}`、`(quote x)` は `quoted: "operator"` と `quote_token: {token: "quote", in_package: "PKG-A"}` | トークンを解決して `eq`。`quoted: "operator"` のときは `quote_token` も解決して `CL:QUOTE` と `eq` か確かめ、違えば `unverified` |

`symbol` は **`'` リーダーマクロ、または `CL:QUOTE` だと確認できた `(quote x)`** で引用された
interned symbol に限る。`'` はソース文字列の先頭が `'` であることで確認し、`(quote x)` は
head トークンを解決して `CL:QUOTE` と `eq` であることで確認する。

ソース側が `symbol` タグのときは、worker 側の kind を見る前に引用を解決する。`':unit` /
`'t` / `'nil` は引用を確認したうえで `:unit` / `t` / `nil` と同じ同一性に正規化し、worker 側の
datum が示すシンボル（`keyword` は `(find-symbol name "KEYWORD")`、`boolean` は `T` / `NIL`、
`symbol` は `(find-symbol name package)`）と `eq` で照合する。kind の一致を先に見ると、同じ
datum の 2 通りの綴りを矛盾と呼んでしまう。解決できたうえで別のシンボルなら `mismatched`
（整数・ratio・character のようにシンボルになり得ない datum との比較も含む）。引用を
`CL:QUOTE` だと確認できない、`quote_token` が無い、トークンのパッケージがこのイメージに無い
といった「解決できなかった」場合は `unverified` であり、`mismatched` にはしない。

次は **`unverified`** とする（独立した確実な根拠がない限り）:
変数参照 `(eql *x*)`、任意の呼び出し `(eql (f))`、`(eql (load-time-value ...))`、
文字列・リスト・配列・任意オブジェクト、uninterned symbol、`#.`、
印字が切り詰められた値、float、complex。

- 文字は大小文字を区別する。
- 大整数・ratio を JSON の浮動小数点にしない（文字列で運ぶ）。
- `NIL` という値を「情報欠落」と混同しない（`boolean` タグで区別する）。
- 型が違う値を「印字が似ている」で `matched` にしない。

照合のための `eval`、ユーザーマクロの `macroexpand`、initfunction の呼び出し、`#.` の実行、
新規 `intern` を禁止する。

### 3.4 コンテナと生成元

| ソースのフォーム | 判定 | 編集単位 |
|---|---|---|
| `defmethod` | 総称関数名・修飾子・特化子をすべて照合 | その `defmethod` |
| `defgeneric` の `(:method ...)` | 内包メソッドの署名（修飾子・特化子）を照合し、一致するものが**ちょうど 1 つ**のときだけ `matched` | 外側の `defgeneric`（`form_type` は `defgeneric`。本文と JSON の `edit_unit` に明示） |
| `defclass` / `define-condition` のアクセサ | クラス名、スロット名、`:reader` / `:writer` / `:accessor` の種類、総称関数名を照合。総称関数名は SETF フラグまで含めて同一性の一部（`:reader x` と `:writer x` は `x`、`:writer (setf x)` は `(setf x)`、`:accessor x` は `x` と `(setf x)` の両方を定義するので `:writer x` と `:accessor x` は交換可能ではない）。`:reader` / `:accessor` の値が裸のシンボルでなければ推測せず `unverified`。アクセサ形状の識別情報でも候補フォームが `defclass` / `define-condition` でなければ通常のメソッド照合にフォールスルーする（生成されたリーダを上書きした手書きの `defmethod` は、その `defmethod` 自身で確認される） | 外側のクラス定義（`edit_unit` に明示）。`defmethod` に一致したときはその `defmethod` 自身（`edit_unit` なし） |
| `defstruct` | クラス名のみ照合（アクセサは MOP に出ないので対象外） | その `defstruct` |
| ユーザーマクロ、`progn`、`eval-when` などのラッパー | `unverified`（理由: unsupported container） | なし |

フォームの種類は head トークンを worker で解決して `CL:DEFMETHOD` などと `eq` か確かめる。
別パッケージで shadow された同名演算子を標準フォームとして扱わない。

同一行に複数のトップレベルフォームが始まる場合、行番号だけで先頭を採らない。
`top-level-forms-at` はその行で始まるフォームを**すべて**返し、識別情報で `matched` になるものが
ちょうど 1 つのときだけ採用する。0 個なら `mismatched`（別の定義がある）か `unverified`、
2 個以上なら `unverified`（ambiguous）。

### 3.5 編集ツールでの往復確認

`matched` でも、`form_type` / `form_name` を返すのは
**編集側の解決処理が同じファイルの同じ CST フォームに一意に解決するとき**だけとする。

- 比較は行番号ではなく CST の `start` / `end` の範囲で行う。
- 共通処理は `src/lisp-edit-form-core.lisp` の
  `locate-form-in-nodes (nodes form-type form-name)`（`%find-target` の中身を切り出したもの）を
  観察側と編集側の両方から使う。比較ロジックを二重に書かない。
- 観察側の本番処理から `lisp-edit-form` ツール全体を `dry_run` で呼ばない。
- 一意でない（`Multiple matches`）、見つからない、別の範囲になる場合は `unverified`
  （理由: not uniquely locatable for editing）。先頭候補や適当な `[N]` を採用しない。

既存の手入力向けの略記・正規化（接頭辞の無視、改行の畳み込み、`[N]`）は互換のため残すが、
それを厳密な同一性確認として扱わない。

### 3.6 処理の流れ（RPC は 2 往復）

```
親: 引数検証
  └─ worker/clos-describe ──▶ report（identity 付き、content なし）
親: ファイルごとに 1 回読む（スナップショット: 本文 + ダイジェスト + CST）
    行で始まる全フォームの source_signature と範囲を作る
  └─ worker/clos-verify-source ──▶ 要素ごとの {status, reason}（find-symbol のみ、intern しない）
親: matched の要素だけ 3.5 の往復確認 → form_type / form_name と edit_guard（B）
    それ以外は source_match と reason
親: テキスト整形
```

worker プールを使わない経路でも同じ 2 段を同じプロセスで通す。
「実行時情報は worker、CST 解析は親」という責務分離は維持し、worker にソースパーサを導入しない。

## 4. B: 編集ガード

### 4.1 ガードの内容

`matched` かつ 3.5 の一意解決に成功した要素にだけ `edit_guard` を付ける。

```
edit_guard:
  version:      1
  path:         "src/foo.lisp"          ; 表示用の相対パス（検証は abs_path 側で行う）
  abs_path:     "/abs/src/foo.lisp"
  file_digest:  "md5:<hex>"             ; スナップショット全体のダイジェスト
  form_start:   1234                    ; 文字単位・0 起点
  form_end:     1300                    ; 文字単位・0 起点・end は exclusive
  form_digest:  "md5:<hex>"             ; 上の範囲の本文のダイジェスト
```

- ダイジェストは SBCL 同梱の `sb-md5`（`(require :sb-md5)`）。新しい依存は足さない。
  ファイルの**オクテット列**に対して計算する（本文はそのオクテット列を UTF-8 として、
  不正バイトを `#\?` に置換して復号したもの）。
- ダイジェストと CST は**同一のスナップショット**から作る。別々に読み直した内容を混ぜない。
- ファイル全体のダイジェストを必須とする（前方の `in-package` 変更などを見落とさないため）。
- スナップショットの読み取りは `cl-mcp/src/fs:fs-read-source-octets`（オクテット列をそのまま返す
  `fs-read-source-text` の対応物）を通す。読み取りポリシー（`allowed-read-path`）は変えず、
  ファイルを開く箇所を fs 層に集約するだけ。
- ガードは**編集の前提条件**であって、アクセス権限を与えるトークンではない。
  既存のパス検証・読み取り制限・書き込み制限はそのまま適用する。
- 永続レジストリ、署名付きトークン、汎用トランザクション基盤は作らない。

### 4.2 `lisp-edit-form` の検証

`lisp-edit-form` に任意引数 `guard`（JSON オブジェクト、`edit_guard` をそのまま渡す）を足す。
`dry_run` を含むすべての操作（`replace` / `insert_before` / `insert_after` / `delete`）で、
**書き込み前に**次の順に検証する。

1. `version` が既知（1）であること。未対応なら `unsupported guard version`
2. `abs_path` が、これから編集するファイルの truename と一致すること
3. スナップショットを 1 回読み、`file_digest` が一致すること
4. `form_start` / `form_end` が本文の範囲内で、`form_end` > `form_start` であること
5. 照合で選ばれたフォームの CST 範囲が `form_start` / `form_end` と一致すること
6. `form_digest` があれば、その範囲の本文のダイジェストと一致すること

いずれかが失敗したら、**一切書き込まず**、構造化した競合エラーを返す
（`isError` + `conflict: {reason, expected, actual}`、理由は上の番号に対応する英語 1 文）。

検証は 2 つに分けて実行する。1〜4 はガード・パス・スナップショットだけで判定できるので
**パース前**に、5〜6 は照合で選ばれたフォームが要るので**照合後**に走らせる
（`%check-edit-guard-pre-parse` と `%check-edit-guard-post-match`。照合後の呼び出しは
`check-edit-guard` 経由で 6 つすべてを同じ順に通すので、順序は 1 箇所で読める）。

照合に失敗した場合もガードを優先する。ガード付きで `file_digest` が一致しない
（＝観察後にファイルが変わった）なら、対象フォームが見つからない・改名された・同名が複数に
なった・ファイルがパースできなくなった、のいずれであっても、`not found` / `Multiple matches` /
パース不能エラーではなく `file_digest` 不一致の構造化競合を返す。逆にファイルが**変わっていない**
（`file_digest` 一致）のに `form_type` / `form_name` が見つからない・曖昧なのは呼び出し側の誤り
なので、従来どおりのエラーのままにする。読み取りポリシーによる拒否・読み取り上限超過・UTF-8 と
して不正なファイルも、競合ではなく素のエラーのままにする。

- 不一致を見つけた後にガードを無視して通常の名前検索へ戻らない。
- 新しいダイジェストを勝手に採用して続行しない。
- 案内は「`clos-describe` を実行し直して新しい編集情報を取得する」こと。
  `fs-write-file` やガード無しの別ツールでの迂回を促さない。
- ガード無しの既存呼び出しは互換のため残す。ただし文書に
  「観察した対象と同じである保証はない」と明記し、`clos-describe` からの推奨フローでは必須にする。

### 4.3 書き込み競合

- ガード検証に使ったスナップショット本文を、そのまま編集内容の生成にも使う
  （読み直して別の内容を編集しない）。
- 既存の書き込み経路（`fs-write-file` / `lisp-edit-form` / `lisp-patch-form`）の同期機構を確認し、
  同じ排他に参加する。ガード付き編集だけが取る新しいロックで他経路との競合も防げる、とは書かない。
  **実装済み**（`cl-mcp/src/fs:with-file-lock`）。確認した結果、参加すべき排他機構は存在しなかった
  ので新設した: 解決済み絶対パス（`truename`）をキーとする再帰ロックのテーブルを `src/fs.lisp`
  に置き、`fs-write-file` は自身の書き込みを、`lisp-edit-form` と `lisp-patch-form` は
  read → verify → build → write の全区間をこのロックの下で実行する（ガード付き編集専用ではなく
  3 経路共通）。あわせて `%write-string-to-file` の一時ファイル名を呼び出しごとに一意にし
  （pid + 連番）、拡張子も `.tmp` にして、同じ固定名の一時ファイルを 2 つの書き手が共有したり、
  残骸が `.lisp` としてソース走査に拾われたりしないようにした。
  `fs-write-file` ツール本体も同じロックの下に置いてあり、上書きガードの読み取り
  （`allow_unparseable_overwrite` の判定を含む）・書き込み・書き込み後のパース検査が
  ひとつのクリティカルセクションになる。これで 1 つの cl-mcp プロセス内では、自身の書き込み同士が
  lost update を起こさない。ロックはイメージ内にあるので、同じチェックアウトを見る**別の
  cl-mcp プロセス**は外部エディタと同様に協調しない。
- 外部エディタなど非協調的な変更に対しては、read → verify → write の間に更新されうる。
  これは compare-and-swap ではない。防げる競合（観察後の変更、古いガードの再利用、
  同じ古いガードでの二重編集）と、残る制約（検証と書き込みの間の他プロセスの変更）を文書化する。

## 5. テスト

「まず再現テストを書き、修正前に失敗することを確認する」。最低限、次を含める（依頼の表に対応）。

| ケース | 期待 |
|---|---|
| `(eql :old)` → `(eql :new)` に変更して再ロード | 旧メソッドに新定義の編集情報を返さない |
| `(eql #\A)` と `(eql #\B)` | `matched` にならない |
| 別パッケージの同名クラス・シンボル | 混同しない |
| keyword と同名の非 keyword | 混同しない |
| escaped symbol の大小文字差（`\|Foo\|` と `\|FOO\|`） | 混同しない |
| 引用シンボル `(eql 'foo)` と変数参照 `(eql *foo*)` | 混同しない |
| EQL の任意式・文字列・リスト・uninterned symbol | 根拠が無ければ `unverified` |
| EQL 式に副作用カウンタ | 問い合わせ・照合で追加実行しない |
| 主メソッドと `:around` / `:before` / `:after` | 意図したメソッドだけに対応付ける |
| 通常の関数名と `(setf name)` | 混同しない |
| `defgeneric` 内メソッドの変更 | 名前だけで旧メソッドを受理しない |
| アクセサ名・スロットオプションの変更 | クラス名だけで旧アクセサを受理しない |
| 未対応ラッパー・ユーザーマクロ | 編集情報を返さない |
| 同一行に複数フォーム | 先頭を暗黙に選ばない |
| パース失敗・欠落情報・読み取り拒否 | `matched` にフォールバックしない |
| 観察後にメソッドを置換・移動・削除 | 古いガードでの書き込みを拒否 |
| 観察後に対象外の場所や `in-package` を変更 | ファイル全体の変更として拒否 |
| 内容を変え mtime・サイズを同じに保つ | ダイジェストで拒否 |
| 同じ古いガードで複数回編集 | 先行変更後の編集を拒否 |
| ガード付き `delete` / `insert_*` / `dry_run` | `replace` と同じ前提条件検証 |

加えて:

- **実際の SBCL イメージ**で、ファイルを書き換えて再ロードし旧メソッドが残る状況を再現するテスト
  （報告データを手作りした単体テストだけにしない）。
- worker 経由の JSON 往復と pool 無効の両方で、識別情報と判定結果の意味が同じこと。
  特に大整数、`NIL`、配列とリスト、JSON boolean。
- 正常系の維持: 通常のクラス特化メソッド、基本的な EQL リテラル、確認できるアクセサと
  `defgeneric` 内メソッドが `matched` のままであること（すべてが `unverified` になる修正は不可）。
- 返した編集情報を実際に `lisp-edit-form` に渡す統合テスト。`dry_run` の行一致だけでなく、
  実編集で意図したフォームだけが変わること。ガード拒否時はファイル内容が 1 バイトも変わらないこと。

## 6. 文書

`docs/tools.md`（`clos-describe` / `lisp-edit-form` / `lisp-patch-form`）、ツールの description、
`prompts/repl-driven-development.md`、`CLAUDE.md`、本設計文書に次を明記する。

- `matched` / `mismatched` / `unverified` の意味
- 観察情報（実行時）と編集情報（ソース確認済み）の違い
- 対応できる EQL 値と未対応のケース
- コンテナ全体を編集する場合の編集単位（`defgeneric` / `defclass`）
- ガード付き編集とガード無し編集の保証の違い
- 競合時は再観察すること
- 同じ定義の識別と、ロード済み実装の完全一致は別であること

既存の「すべての `form_name` をそのまま編集へ渡せる」という趣旨の記述を、新しい契約に合わせて直す。

## 7. 互換性と残る制約

- `clos-describe` の応答に `identity` / `source_match` / `source_match_reason` / `edit_guard` /
  `edit_unit` が増える。`form_type` / `form_name` は **`matched` のときだけ**現れる（破壊的変更）。
- `lisp-edit-form` の `guard` は任意引数。既存の呼び出しは変わらない。
- 残る制約: 検証と書き込みの間の外部変更、float / complex の EQL、
  `defstruct` のアクセサ、ロード済みコードとソース本文の完全一致は保証しない。
