# 括弧診断の周辺ツールへの適用設計

- 日付: 2026-09-06
- ステータス: 実装済み(ブランチ feat/paren-diagnostics-peripheral)
- 改訂: 2026-09-06 に設計レビューの指摘を反映。差分の要約は §8
- 前提: PR #141 (`feat/paren-diagnostics`, main 6f80ab0) がマージ済み
- 想定受益者: cl-mcp を利用する AI エージェント。特に推論力の弱いモデル

## 1. 背景と問い

PR #141 は共通診断モジュール `src/paren-diagnostics.lisp` を新設し、
`lisp-check-parens` / `lisp-edit-form` / `lisp-patch-form` / `lisp-macroexpand`
の 4 ツールを接続した。壊れたファイルは「どの行に何を足す/消すか」と
復旧手順を伴って報告されるようになった。

2026-09-06 の dogfooding (`experiments/paren-lab`, FiveAM, 12 テスト) で、
括弧を意図的に壊してからの復旧を実測した結果、**接続済みの 4 ツールは
推測もリトライもなしにバックアップとバイト一致まで復旧できた**。
残る摩擦はすべて接続されていない周辺ツール側にあった。

問いは「その周辺ツールに何を接続するか」。結論は 5 項目。
**新しい診断ロジックは書かない。既存モジュールの利用者を増やし、
案内文を 1 箇所直すだけ。** 新ツール・新しい入力パラメータはゼロ。
出力フィールドの追加は許容する (§3)。

## 2. 現状調査

### 2.0 実測

`experiments/paren-lab/src/tokens.lisp` の `matching-closer` から `)` を 1 個
削り、そのフォームがファイル末尾まで飲み込む状態を作って各ツールに投げた。

| ツール | 出力 | 判定 |
|---|---|---|
| `lisp-check-parens` | 39 行目のフォーム、41 行目に `)` 追加、43 行目に次のトップレベル、復旧手順 | 接続済み |
| `lisp-edit-form` | 同上 + 編集ツールが位置特定できない旨 | 接続済み |
| `lisp-patch-form` | 同上 | 接続済み |
| `lisp-macroexpand` | 同上 | 接続済み |
| `lisp-read-file` | `Internal error during lisp-read-file: Unexpected end of file while parsing tokens.lisp; check for unbalanced parentheses (use lisp-check-parens)` | **未接続** |
| `clgrep-search` | 成功。ただし 39 行目以降の定義がすべて消える。警告なし | **未接続・誤答** |
| `fs-write-file` | 壊れた内容を書いて `Wrote ... (65 chars)` | **未接続** |

実測したのは `)` 欠落 1 パターンのみ。余分な `)`、`in-readtable` 切替後の破損、
読み取り上限超えは実測しておらず、以下の各節で「コード確認」と明記した項目は
コードを読んで導いた結論である。実装時に再現テストで裏を取る。

### 2.1 `clgrep-search` の機序

`src/utils/clgrep.lisp:63` `scan-toplevel-forms` は `paren-depth` が 0 に
戻ったときにだけ `toplevel-form` を push する。EOF 到達時に `form-start-pos`
が non-nil でも、末尾の `(nreverse forms)` はそれを含めない。

結果、未終了フォームの開始行から EOF までの全行が「どのフォームにも属さない」
状態になり、`grep-file-structured` の `(when form-info ...)` がそれらの
マッチを丸ごと落とす。

実測 (`tokens.lisp`, 39 行目で破損):

```
clgrep-search pattern="defun (matching-closer|tokenize|openerp)" path=.../src/tokens.lisp
→ 1 match ... tokens.lisp:31 [defun] (openerp ch)
```

`matching-closer` (39 行目) と `tokenize` (47 行目) はどちらも正規表現に
マッチするが報告されない。エラーも注記もない。

これは「見つからない」ではなく **「間違った答えを返す」** ため、
本設計で唯一の P1。

**ツール層にも同種の穴がある (コード確認)。** `src/clgrep.lisp:91`
`%format-clgrep-results` は結果を `(file . form-start-byte)` でグループ化し、
本文 (`content[].text`) には代表 1 行だけを出す。残りのマッチは `match_lines`
という兄弟フィールドに入り、多くのクライアントでは描画されない。
スキャナ層だけ直して未終了フォームを採用すると、破損行以降のマッチは
すべてその 1 フォームにまとまり、本文からは依然として消える。4.1 で扱う。

### 2.2 `lisp-read-file` の 5 つの欠陥

`src/lisp-read-file.lisp:621` `%lisp-read-file-content` の
`handler-case (end-of-file ...)` が独自の短い文面を作っている。

1. 共通診断モジュールを呼んでいない。行番号も Likely fix も復旧手順もなく、
   別ツールを呼べと言うだけ。
2. `src/tools/define-tool.lisp:178` の汎用ラッパに落ちて
   **`Internal error during ...`** と表示される。これはユーザーデータの
   問題であって内部障害ではない。この文言はエージェントに cl-mcp のバグと
   誤認させるか、同じ呼び出しの再試行を促す。
3. **捕まえているのが `end-of-file` だけ (コード確認)。** 余分な `)` は
   `src/cst.lisp:400` で `stray-right-parenthesis` として signal され、
   これは `end-of-file` の下位型ではない (`%delimiter-failure-p` が両者を
   別々に列挙していることからも分かる)。この handler を抜けて 2. の
   `Internal error` になる。実測表にある `)` 欠落は `unterminated-source`
   (`end-of-file` の下位型) なので、たまたま捕まっていただけである。
4. **lenient 経路の破損が無言で消える (コード確認)。** `src/lisp-read-file.lisp:496`
   `%format-lisp-file` は `parse-top-level-forms` の**第 1 値しか受け取らない**。
   `src/cst.lisp:427` の docstring 通り、`in-readtable` 切替後や `readtable`
   指定時の lenient 経路は、途中で読めなくなっても signal せず、読めた分の
   ノードと第 2 値のエラーを返す。第 2 値を捨てているので、破損以降の
   フォームは表示から消え、エラーも注記も出ない。2.1 と同じ
   「間違った答えを返す」類であり、P1 相当。
5. **読み取り上限を見ていない (コード確認)。** `fs-read-file` の第 2 値
   `truncated` を無視して prefix を解析する。1 MB を超える正常なファイルは
   途中で切れた prefix として不均衡に見え、偽の診断になる。
   `%locate-target-form` (`src/lisp-edit-form-core.lisp:462`) は同じ状況を
   専用の too-large 文面で報告している。

`prompts/repl-driven-development.md` は `.lisp` ファイルについて
**`fs-read-file` より `lisp-read-file` を優先せよ**と指示している。
つまりこれは壊れたファイルへの最初の接触点であり、4 ツールの中で唯一
接続されていない。

なお `collapsed=false` (raw モード) は CST を使わないので**壊れたファイルでも
動作する**。実測:

```
lisp-read-file path=.../src/matcher.lisp collapsed=false offset=33 limit=8
→ (defmethod matcher-depth ((m matcher))
     "Return how many brackets are currently open in M."
     (length (matcher-stack m))))
  [Showing lines 34-41 of 72. Use offset=41 to read more.]
```

### 2.3 復旧手順が案内する読み取りツールが噛み合っていない

`src/paren-diagnostics.lisp:993` `format-overwrite-recovery` が出す手順は
「`fs-read-file` で読み、`Likely fix` を当てて、`fs-write-file` で全文書き戻す」。

しかし `fs-read-file` の `offset`/`limit` は**文字数**で、診断は
**`line 41: "..." -> add 1 ")"`** という**行**で出る。41 行目に到達する手段がなく、
全文を読むしかない。2.2 で示した通り `lisp-read-file collapsed=false` は
壊れたファイルでも動き、`offset`/`limit` は行である。

ただし `lisp-read-file collapsed=false` は**全文コピーの元には向かない**
(コード確認)。`src/lisp-read-file.lisp:560` `%read-lines-slice` は行を
`read-line` で切って `~%` で連結し直すため、末尾改行のないファイルには
改行が足される。また `limit` (既定 500 行) を超えると
`[Showing lines A-B of N. ...]` のフッタが本文に付く。これをそのまま
`fs-write-file` に渡すとフッタが混入する。dogfooding が達成した
「バイト一致まで復旧」は `fs-read-file` の正確さに依っている。

### 2.4 `fs-write-file` は書いたものを検証しない

`allow_unparseable_overwrite=true` は壊れたファイルの修復ループを閉じるためだけに
存在するのに、書いた内容が読めるかを確認しない。実測:

```
fs-write-file path=.../probe.lisp content='(defun a (x)\n  (list x)\n\n(defun b (y)\n  (list y))\n'
→ Wrote ... (61 chars)              ← 新規 .lisp、不均衡、警告なし

fs-write-file (同じパス) allow_unparseable_overwrite=true
  content='(defun a (x)\n  (list x)))\n\n(defun b (y)\n  (list y))\n'
→ Wrote ... (65 chars)              ← まだ壊れている、やはり無言
```

新規ファイル手順は「`fs-write-file` で書いたら `lisp-check-parens` で確認」を
**手動で**求めている。ツール側で吸収できる往復である。

さらに、壊れた `.lisp` を**新規作成した直後の 2 回目の書き込み**は、
`src/fs.lisp:207` `%existing-lisp-overwrite-error` の判定で
「存在する・パースできない `.lisp`」への上書きになる。
`allow_unparseable_overwrite=true` を付けなければ
`Cannot overwrite existing .lisp/.asd with fs-write-file; use lisp-edit-form`
で拒否され、`lisp-edit-form` はフォームを特定できないので、エージェントは
ループに入る。警告文はこの 2 回目の書き込みまで案内しなければならない。

### 2.5 `lisp-check-parens` のスライスに 2 つの穴

窓警告の概念は**既に存在する**。`src/validate.lisp:262` の `partial` フラグと
`:349` の文面:

> Only a window of ~A was checked (offset ~D, ~D characters), so this may be an
> artifact of the window and no repair hint is offered; check the whole file for one.

穴は 2 つ。

**(a) `reader-info` 分岐に窓警告がない。** `partial` を見ているのは
区切り文字失敗 (`(not ok)`) の分岐だけ。`src/validate.lisp:404` の
`reader-info` 分岐は `kind`/`message`/`position` を設定するが
`diagnosis_text` を一切設定しないため、窓の注意書きが出ない。

スライスは文字列の途中で切れるので、**リーダーエラーこそ窓の副産物に
なりやすい**。実測 (均衡の取れた健全なファイルに対して):

```
lisp-check-parens path=.../src/matcher.lisp offset=700 limit=300
→ Reader error at line 1, column 9: Comma not inside a backquote.
```

オフセット 700 は docstring の途中 (`... closed, innermost first.")`) に落ちる。
散文中のカンマがカンマトークンとして読まれただけで、このファイルに
バッククォートは 1 つもない。

**(b) 行番号と列番号がスライス相対のまま。** これは `scan-delimiters` の仕様である。
`src/paren-diagnostics.lisp:201` の docstring:

> BASE-OFFSET is added to :offset only; :line and :column are always relative
> to the start of TEXT.

`:offset` はファイル絶対に補正されるが `:line`/`:column` は補正されない。
翻訳していないのは `lisp-check-parens` 層。上の実測の「line 1」は
**スライスの 1 行目**であってファイルの 1 行目ではない。
列も同様で、`offset` は行の途中に落ちるので、窓の 1 行目の列は
行頭からではなく `offset` からの距離になっている。
`%try-reader-check` が返す `:line`/`:column` も `text` 先頭からの相対値であり、
同じ翻訳が要る。

## 3. 判断

スコープは次の 5 項目。`main` (6f80ab0) から切った別ブランチ・別 PR とする。
PR #141 は 8,800 行・レビュー 22 ラウンドの規模だったので、
「診断モジュールの利用者を増やす」という別テーマとして独立してレビューできる。

方針の要点:

- **新しい診断ロジックを書かない。** `paren-diagnostics` は完成している。
- **新ツール・新しい入力パラメータを増やさない。** `apply_fix` のような修復適用
  プリミティブは検討したが見送る (4.6 参照)。
- **出力フィールドの追加は許容する。** 本文 (`content[].text`) を一次情報とし、
  JSON だけを読むクライアントのために同じ事実を構造化フィールドにも載せる。
  本設計で足すのは `clgrep-search` の `unterminated`/`notes`、
  `lisp-check-parens` の `window`、`fs-write-file` の `unparseable`。
  いずれも既存フィールドの意味は変えない。
- 各ツールの既存の性格を壊さない。特に `clgrep-search` は
  「何もロードせずに常に動く」ことが利点なので、他ツールのように
  拒否させない。
- **無言で欠落させない。** 2.1 と 2.2-4 のように「読めた分だけ返して
  何も言わない」挙動は、拒否より悪い。部分結果を返すなら、どこから先が
  読めていないかを必ず本文で言う。

## 4. 各項目の設計

### 4.1 `clgrep-search`: 未終了フォームを採用し警告する

**スキャナ層** (`src/utils/clgrep.lisp`) — 新しい依存は追加しない。

- `toplevel-form` 構造体に `unterminated-p` スロットを追加 (既定 `nil`)。
- `scan-toplevel-forms` のループ終了後、`form-start-pos` が non-nil なら
  `end-pos` を `len`、`end-line` を `current-line`、`unterminated-p` を `t` として
  push する。EOF 時の状態が `:string` や `:block-comment` でも同じ扱いにする
  (`form-start-pos` が non-nil なら囲みフォームは開いたままである)。

これだけで、破損位置以降のマッチが囲みフォームに紐づき、
`grep-file-structured` の `(when form-info ...)` から落ちなくなる。

`scan-toplevel-forms` は `paren-depth > 0` を自分で知っているので、
検出に `paren-diagnostics` は要らない。この層は `cl-ppcre` のみに依存する
純粋なスキャナのまま保つ。

`search-in-file` は各結果に `:unterminated` を載せ、`%normalize-result` を
通してツール層まで運ぶ。

**ツール層** (`src/clgrep.lisp`) — 2 箇所を直す。

1. **重複排除を未終了フォームには適用しない。** `%format-clgrep-results` は
   `unterminated-p` のフォームに属する結果をグループ化せず、マッチ行ごとに
   1 エントリとして残す。本文の 1 行 1 マッチも同じ。健全なフォームの
   グループ化は変えない。未終了フォームは「フォーム」ではなく破損の副産物
   なので、1 つにまとめる意味がない。
2. **未終了フォームを含んでいたファイルごとに注記を出す。**

```
NOTE: src/tokens.lisp does not parse: a form opened at line 39 is never closed.
  Matches at or below that line are listed individually; their form type and
  signature are those of the unclosed form, not of the definition they sit in.
  Run lisp-check-parens for the fix.
```

- 文面は「フォームが閉じていない」と言い、「unclosed form」とは言わない。
  未終了の原因が文字列や `#|` のこともあり、`lisp-check-parens` がそれを言い分ける。
- **`form_types` フィルタの制限を注記に含める。** 破損行以降のマッチは
  囲みフォームの `form-type` を継承するので、飲み込まれた `defmacro` を
  `form_types=["defmacro"]` で探すと依然として見つからない。これは
  本設計では直さない (正しい境界を推定するのは新しい診断ロジックになる) が、
  注記で「その行以降の型と署名は囲みフォームのもの」と言っておけば、
  エージェントは `form_types` を外して再検索できる。
- ファイル単位。複数ファイル検索で 1 ファイルが壊れていても他は無影響。
- 注記は該当ファイルにマッチがあった場合にのみ出す。壊れているが
  マッチのないファイルについて注記を並べても雑音にしかならない。
- 注記は `content[].text` に入れる。兄弟 JSON フィールドは多くの
  クライアントで描画されない (2026-03-07 総合テスト報告、および
  PR #141 設計書と同じ知見)。
- 構造化フィールド: 各エントリに `"unterminated": true`、ペイロードに
  `"notes"` (注記文字列のベクタ)。本文と同じ事実を載せるだけで、
  本文を読まないクライアントへの保険である。

`include_form=true` のとき、未終了フォームの `form` は破損行から EOF までの
テキストになる。既存の `truncate-form` が 2000 文字で切るので出力量は
抑えられるが、内容は「飲み込まれた末尾」であってその定義ではない。
注記がその旨を言っているので追加の処理はしない。

### 4.2 `lisp-read-file`: 共通診断に接続する

`src/lisp-read-file.lisp` は既に `cl-mcp/src/lisp-edit-form-core` を
import している (`%parse-readtable-designator`)。import 句を広げれば
`file-unparseable-error` / `file-unparseable-message` が使える。
ただし 2.2 の 5 つの欠陥を全部塞ぐには、core 側にも小さな変更が要る。

**core 側 (`src/lisp-edit-form-core.lisp`)** — 2 点。いずれも
`lisp-edit-form` / `lisp-patch-form` / `lisp-macroexpand` の挙動は変えない。

- **条件の生成関数を抽出する。** `file-unparseable-error` を組み立てる
  `unparseable` は `%locate-target-form` 内の `flet` である。これを
  `signal-file-unparseable (abs cause &key readtable editable-prefix)` として
  トップレベルに出し、`%locate-target-form` と `lisp-read-file` の両方から
  呼ぶ。「readtable 指定時は診断を付けない」「`recoverable` は
  `%delimiter-failure-p` で決める」「`cause` は `sanitize-condition-text` を
  通す」という規則が 1 箇所に留まり、`lisp-read-file` 側に診断ロジックの
  複製が生まれない。
- **`file-unparseable-message` にプロジェクト外の分岐を足す。**
  `lisp-read-file` は登録 ASDF システムの依存ソースも読める。現在の
  `file-unparseable-message` は `enough-pathname` で相対パスを作るが、
  プロジェクト外のパスに対して `enough-pathname` は引数をそのまま返すので、
  復旧手順は絶対パスを `fs-write-file` に案内し、それは拒否される
  (`path` は相対のみ)。`subpathp` でプロジェクト外と判れば、
  `src/validate.lisp` が既に持つ「The file does not parse, and it is outside
  the project root, so fs-write-file cannot rewrite it and lisp-edit-form
  cannot locate any form in it; fix it outside cl-mcp」と同趣旨の文面に
  切り替え、復旧手順は付けない。編集系 3 ツールは `%normalize-paths` が
  プロジェクト外を先に拒否するので、この分岐には到達しない。

**`lisp-read-file` 側** — `%lisp-read-file-content` の collapsed 分岐。

- **読み取り上限を先に見る。** `fs-read-file` の第 2 値 `truncated` が真なら、
  `%locate-target-form` と同じ too-large 文面で `error` する。切れた prefix を
  解析して偽の診断を出さない。
- **Eclector 経路: `handler-case (end-of-file ...)` を `error` 全体に広げ、
  `signal-file-unparseable` に渡す。** `end-of-file` 系も
  `stray-right-parenthesis` も、`#.` 無効化や未知のディスパッチ文字も、
  すべて同じ経路で `file-unparseable-error` になる。分類
  (`recoverable` か否か) は `%delimiter-failure-p` が行う。これで
  行番号・Likely fix・列 0 ヒント・復旧手順が他 3 ツールと同一になる。
- **lenient 経路: 第 2 値を捨てない。** `%format-lisp-file` で
  `parse-top-level-forms` の第 2 値 `swallowed` を受け取り、non-nil なら
  読めた分の表示の**後ろに** `file-unparseable-message` の本文を
  そのまま付ける (`signal-file-unparseable` で条件を作り、`princ-to-string`
  する。`editable-prefix` は `(and nodes t)`)。`meta` には
  `"unparseable_from_line"` を載せる。
  読めた prefix は正しい情報なので捨てず、どこから先が読めていないかを
  必ず言う。Eclector 経路は失敗時にノードを 1 つも返さないので、表示するものが
  なく、他 3 ツールと同じくエラーになる。両経路の共通点は「無言にしない」
  ことであり、それが §3 の要件である。
- **ツール本体で catch し、`define-tool` の汎用 `Internal error during ~A`
  に落とさない。** `src/lisp-macroexpand.lisp:513` に同じ判断の前例とその理由が
  コメントで残っているので、それに倣う。`arg-validation-error` を `error` 節より
  先に置く規則も同じく踏襲する。

`collapsed=false` は今も動くので挙動を変えない。壊れたファイルで
自動的に raw に落とすフォールバックは**採らない**。呼ばれていない
モードに黙って切り替えるのは、他 3 ツールが揃って拒否するのと
一貫しないうえ、大きなファイルで意図しない出力量になる。

`collapsed=false` で生読みできる旨の案内は、`lisp-read-file` 専用の一文として
足すのではなく、4.4 で `format-overwrite-recovery` に入れる。復旧手順は
4 ツールが共有しているので、そこに置けば 4 ツールすべてに波及する。

### 4.3 `fs-write-file`: 書いた内容を検証して警告する

`src/fs.lisp` は `cl-mcp/src/lisp-edit-form-core` を import できない。
`*lisp-file-unparseable-hook*` (`src/fs.lisp:174`) の docstring が
その循環回避の理由を明記している。

しかし**その hook 自体が使える**。署名は
「2 引数 (絶対パス名と、そのファイルのテキスト) の述語」なので、
今書いたばかりの内容をそのまま渡せる。ディスクから読み直す必要はない。

- `.lisp`/`.asd` の書き込み後に hook を呼ぶ (対象拡張子は fs の
  `%lisp-source-pathname-p` と同じ集合にする。上書きガードと同じ範囲で
  警告するのが不変条件である)。
- `t` が返ったら `paren-diagnostics` の `diagnose-delimiters` +
  `format-delimiter-diagnosis` で警告文を組み立て、結果テキストに追記する。
  `fs` から `paren-diagnostics` への import は循環しない
  (`paren-diagnostics` は `parinfer` と `uiop` にしか依存せず、
  `parinfer` は `uiop` のみ)。
- **hook が `t` でも `diagnose-delimiters` が `:ok` を返す場合に備える。**
  スキャナとリーダーには既知の差異がある (`foo#|bar|` など)。この組み合わせでは
  `format-delimiter-diagnosis` が NIL を返すので、汎用の一文
  「the editing tools' reader cannot parse the file as written; run
  lisp-check-parens」をフォールバックにする。
- **警告文は 2 回目の書き込みまで案内する。** 2.4 の通り、この時点でファイルは
  「存在する・パースできない `.lisp`」になっており、修正版を書く次の
  `fs-write-file` には `allow_unparseable_overwrite=true` が要る。
  警告文の末尾はこうする:

  ```
  WARNING: the file was written but does not parse.
  <format-delimiter-diagnosis の出力>
  Fix it and write it again with fs-write-file (path="...",
  allow_unparseable_overwrite=true; the file now exists and does not parse,
  so the overwrite guard requires the flag).
  ```

- **書き込み自体は必ず成功させる。** 段階的にファイルを組み上げる
  ワークフロー (新規ファイル手順がまさにそれ) を壊さない。
- 構造化フィールド: ペイロードに `"unparseable": true` を足す。`success` は
  `t` のまま (書き込みは成功している)。
- hook が `nil` (fs だけをロードした部分イメージ) なら黙って skip する。
  既存の「弱いフォールバック定義は置かない」方針を踏襲する。
- コスト: `.lisp`/`.asd` の書き込みごとに CST パースが 1 回増える。
  `allow_unparseable_overwrite=true` の上書きではガードが旧内容を、
  検証が新内容を解析するので 2 回になる。いずれもファイル 1 個分の
  パースであり、往復 1 回を省く利益の方が大きい。

この設計で次の不変条件が立つ:

> **`fs-write-file` は、上書きを許可するのと厳密に同じ条件でだけ警告する。**

hook は「どんな readtable でも直せない形で壊れている」ときだけ `t` を返すので、
`in-readtable` を使うファイルは自動的に除外され、誤警告しない。
この対称性はテストしやすく、説明もしやすい。

副作用として、リーダーレベルの破損 (`#z` など) には警告が出ない。
hook が `nil` を返すためである。確実なときだけ警告するという保守的な
選択であり、誤警告を出すよりよい。

### 4.4 復旧手順の案内先を直す

`src/paren-diagnostics.lisp:993` `format-overwrite-recovery` の文言を、
**位置確認と全文取得を分けた 2 段構え**にする。

- **位置確認**: `lisp-read-file collapsed=false offset=<行-1> limit=1`。
  `offset`/`limit` が行単位であること、壊れたファイルでも動くことを明記する。
  `format-overwrite-recovery` に `:fix-line` キーワード引数を足し、
  呼び出し元が Likely fix の先頭行 (なければ `form-line`) を渡す。
  文面には計算済みの値を埋め込む (`offset=40 limit=1` のように)。
  推論力の弱いモデルに「行番号から 1 を引く」計算をさせない。
- **全文取得と書き戻し**: 今まで通り `fs-read-file` で全文を読み、
  `fs-write-file` で書き戻す。2.3 の通り `lisp-read-file collapsed=false` は
  行を連結し直し、`limit` 超過でフッタが付くので、コピーの元にしない。
  文面に「the whole text for writing back comes from fs-read-file
  (exact bytes; lisp-read-file re-joins lines and may append a footer)」と書く。
- **`collapsed=false` で生読みできる旨**をここに置く (4.2 参照)。

**1 関数の文言変更が 4 ツールすべてに波及する。**
呼び出し元は `src/validate.lisp` と `src/lisp-edit-form-core.lisp` の 2 箇所で、
どちらも `likely-fixes` を手元に持っているので `:fix-line` を渡せる。

`fs-read-file` は非 Lisp ファイル用と、全文コピー用に残る。

同じ復旧手順は `prompts/repl-driven-development.md` の
「Parenthesis Mismatch」節にも書かれている (`Recover with fs-read-file, ...`)。
ツールの文面と食い違わないよう、同じ PR で 2 段構えに直す。
`%maybe-add-lisp-edit-guidance` と `file-unparseable-message` の docstring も
`fs-read-file` を案内しているので合わせる。

実装順序は 4.2 の後。今日は `collapsed` 既定でエラーになるため、
先に案内だけ変えると、案内された経路が失敗するように見える。

### 4.5 `lisp-check-parens` のスライス

**(a) `reader-info` 分岐に窓警告を適用する。**
`src/validate.lisp:404` の分岐で、`partial` が真なら
`:349` と同じ趣旨の `diagnosis_text` を設定する。文面も概念も既にあるので
新規に考えることはない。スライスにおけるリーダーエラーは窓の副産物である
可能性が高い旨を含める。

**(b) `offset` 指定時に行番号と列番号をファイル絶対にする。**

- **前置の計測は流し読みで行う。** ファイルを開き、`file-position` が
  `offset` に達するまで `read-char` で 1 文字ずつ進めながら、改行数 `N` と
  最後の改行以降の文字数 `C` を数える。バッファに溜めないので
  `*fs-read-max-bytes*` の上限に当たらず、「読めなかったので相対値」という
  フォールバック分岐が要らない。停止条件を `file-position` にするのは、
  スライス本体 (`%read-file-string`) が `file-position` で位置決めしている
  からで、同じ基準で止めないと多バイト文字を含むファイルで窓の先頭と
  ずれる。
- 窓相対の行 `L` (1 起点) はファイル絶対では `L + N`。
  列は **`L = 1` のときだけ** `C` を足す (窓の 1 行目は行の途中から始まる)。
  2 行目以降の列はそのまま。
- **区切り文字失敗と `reader-info` の両分岐に適用する。** `position` の
  `line`/`column` は `scan-delimiters` 由来でも `%try-reader-check` 由来でも
  窓相対なので、翻訳は `lisp-check-parens` 層で分岐の外側にまとめて行う。
  ツール本体の summary (`at line ~D, column ~D`) は `position` を読むので
  自動的に追従する。
- `partial` のときは `likely_fixes` も `next_top_level_line` も出さない
  (現状の `(cond (partial ...) (t ...))` の構造がそう保証している) ので、
  他に翻訳が要る行番号はない。
- **ペイロードに `"window"` を載せる**:
  `{"offset": <文字位置>, "length": <窓の長さ>, "first_line": <N+1>}`。
  `diagnosis_text` は本文にしか出ないので、JSON だけを読むクライアントが
  窓であることを知る手段になる。
- `scan-delimiters` の契約 (`:line`/`:column` は TEXT 先頭からの相対) は
  変更しない。この契約は docstring に書かれており、`diagnose-delimiters` の
  他の呼び出し元 (edit-form / patch-form は常に全文を渡す) が依存している。

これにより「line 37」がどのツールでも常にファイルの 37 行目を意味するようになり、
4.4 で案内先を `lisp-read-file collapsed=false` (行オフセット) に変えた件とも整合する。

### 4.6 スコープ外

- **修復適用プリミティブ (`apply_fix` 等)。** 診断は既に
  `line 41: add 1 ")"` という実行可能な修正を計算しているので、
  それを適用するツールを足せば往復が激減する。しかし dogfooding で、
  インデントが平坦な入力に対して parinfer の修復が
  **1 個の関数を 3 つの無関係なトップレベルフォームに再構成する**ことを
  確認した (下記)。推定が外れたときに黙ってコードを壊すリスクを
  本 PR で背負わない。別途設計する。

  ```
  (defun f (x)
  (let ((a 1)
  (b 2)
  (+ a b)))
  → line 1: add 1 ")" / line 2: add 2 ")" / line 4: remove 2 ")" at columns 8, 9
  ```

  不足は `)` 1 個なのに 3 行を編集する提案になる。
  なお `lisp-edit-form` はこの入力を、parinfer の修復結果が複数の
  トップレベルフォームになることを理由に**拒否**した。
  「content はトップレベルフォーム 1 個」という不変条件が安全弁として
  効いており、この挙動は正しい。

- **`clgrep-search` で飲み込まれた定義の境界を推定すること。**
  列 0 の `(` を次のトップレベルの手掛かりにする発想は `paren-diagnostics` の
  `next-top-level-hint-line` にあるが、これをスキャナに持ち込むと
  「純粋なスキャナ」でなくなり、推定が外れたときの誤帰属も生む。
  4.1 の注記で制限を明示するに留める。

- 括弧と無関係な dogfooding 指摘 (`run-tests` の FiveAM `Form:` が `~A` 印字、
  `repl-eval` の `max_output_length` が `[object-id: N]` を削る、
  `*print-circle*` の `#1=`、`locals_preview_frames` の無言)。
  `claudedocs/dogfooding-feedback.md` の 2026-09-06 セクションに記録済み。

## 5. テスト方針

既存のテストファイルに追加する。新規テストシステムは作らない。
**本文に対する assert は `content[].text` に対して行う。** 兄弟フィールドに
入っているだけでは「見える」ことの証明にならない (2.1 のツール層の穴が
その実例)。

**`tests/clgrep-utils-test.lisp`** (スキャナ層)
- `scan-toplevel-forms` が未終了フォームを `unterminated-p` 付きで返すこと。
- その `end-pos` が入力長、`end-line` が最終行であること。
- EOF が文字列内・`#|` 内でも未終了フォームとして返ること。
- 健全な入力で `unterminated-p` が立たないこと (回帰防止)。

**`tests/clgrep-test.lisp`** (ツール層)
- 未終了フォーム内 (破損行以降) の**各**マッチが `content[].text` に
  1 行ずつ現れること (重複排除されないこと)。
- それらのエントリに `unterminated: true` が付くこと。
- 破損ファイルに注記が出ること。注記が `form_types` の制限に触れること。
- 飲み込まれた `defmacro` が `form_types=["defmacro"]` では見つからず、
  フィルタなしでは見つかること (制限の文書化)。
- 健全なファイルでは注記が出ず、重複排除が従来通り働くこと (回帰防止)。
- 複数ファイル検索で、健全なファイルの結果が破損ファイルの影響を受けないこと。

**`tests/lisp-read-file-test.lisp`**
- `)` 欠落: 行番号と Likely fix を含み、`Internal error` を含まないこと。
- **余分な `)`**: 同上 (`stray-right-parenthesis` 経路)。
- **`in-readtable` 切替後の破損**: 切替前のフォームが表示され、その後ろに
  診断が付き、`meta.unparseable_from_line` が入ること。
- **プロジェクト外のファイル**: 「outside the project root」の文面になり、
  `fs-write-file` への指示を含まないこと。
- **読み取り上限超え**: `*fs-read-max-bytes*` を小さく束縛し、too-large の
  文面になること (偽の不均衡診断を出さないこと)。
- `collapsed=false` が破損ファイルで従来通り動くこと (回帰防止)。

**`tests/lisp-edit-form-test.lisp`**
- `signal-file-unparseable` 抽出後も既存の `file-unparseable-error` 系テストが
  変わらず通ること (リファクタの回帰防止)。

**`tests/fs-test.lisp`**
- 壊れた `.lisp` の新規作成で警告が出ること、かつ書き込みは成功すること。
- **警告文が `allow_unparseable_overwrite=true` を案内し、その通りに
  修正版を書いた 2 回目が成功して警告が消えること** (ループにならないこと)。
- ペイロードに `unparseable: true` が入り、`success` は `t` のままであること。
- 健全な `.lisp` で警告が出ないこと。
- `allow_unparseable_overwrite=true` で壊れた内容を書いたとき警告が出ること。
- `in-readtable` を使うファイルで誤警告が出ないこと。
- 非 Lisp ファイル (`.md` 等) では検証しないこと。
- hook が `nil` のとき例外を出さず黙って通ること。
- hook を `t` を返すスタブに差し替え、スキャンが `:ok` の内容を書いたとき
  フォールバックの一文が出ること。

**`tests/validate-test.lisp`**
- スライスのリーダーエラーに窓警告が付くこと。
- `offset` 指定時の行番号がファイル絶対であること (区切り文字失敗と
  リーダーエラーの両方)。
- **窓の 1 行目に落ちた失敗の列が行頭からの値になること。**
- `*fs-read-max-bytes*` より大きい `offset` でも行番号が絶対であること
  (流し読みなので上限に依存しない)。
- ペイロードに `window` が入ること。
- `offset` なしの経路が変わっていないこと (回帰防止)。

**`tests/paren-diagnostics-test.lisp`**
- `format-overwrite-recovery` が `lisp-read-file collapsed=false` を
  計算済みの `offset` 付きで案内すること。
- 全文取得には `fs-read-file` を案内し続けること。

**統合**: `experiments/paren-lab` を再度壊し、
1) `clgrep-search` で破損位置以降の定義が**本文に**見えること、
2) `lisp-read-file` が行番号を返すこと (欠落・余分の両方)、
3) 案内通り `lisp-read-file collapsed=false offset=N` で行を確認し、
   `fs-read-file` → `fs-write-file` で復旧できること、
4) 途中で壊れた内容を書いたら警告が出て、その案内通りの 2 回目で直ること
を手動で確認する。

**リント**: コミット前に `mallet src/*.lisp`。
**PR 前**: `(asdf:compile-system :cl-mcp :force t)` で警告を洗い、
`rove cl-mcp.asd` を新規プロセスで実行する
(単一ファイルのテスト実行は失敗を隠すため)。

## 6. 影響範囲

| ファイル | 変更 |
|---|---|
| `src/utils/clgrep.lisp` | `toplevel-form` にスロット追加、EOF 時 push、結果に `:unterminated` |
| `src/clgrep.lisp` | 未終了フォームを重複排除から除外、破損ファイルの注記、`unterminated`/`notes` |
| `src/lisp-read-file.lisp` | import 拡張、truncated 検査、`handler-case` を `error` 全体に、lenient 経路の第 2 値処理、ツール本体で catch |
| `src/lisp-edit-form-core.lisp` | `signal-file-unparseable` の抽出 (振る舞い不変)、`file-unparseable-message` にプロジェクト外分岐 |
| `src/fs.lisp` | `paren-diagnostics` を import、書き込み後検証、2 回目の書き込みの案内、`unparseable` |
| `src/paren-diagnostics.lisp` | `format-overwrite-recovery` を 2 段構えに、`:fix-line` 引数 |
| `src/validate.lisp` | `reader-info` 分岐の窓警告、行・列の絶対化 (両分岐)、`window`、`:fix-line` を渡す、docstring |
| `prompts/repl-driven-development.md` | 「Parenthesis Mismatch」節の復旧手順を 2 段構えに |
| `tests/*` | `clgrep-utils-test` `clgrep-test` `lisp-read-file-test` `lisp-edit-form-test` `fs-test` `validate-test` `paren-diagnostics-test` |

`src/lisp-edit-form.lisp` / `src/lisp-patch-form.lisp` / `src/lisp-macroexpand.lisp`
は変更しない。`src/lisp-edit-form-core.lisp` の変更は関数抽出と、
これら 3 ツールが到達しない分岐の追加に限る。4.4 の文言変更の効果は
3 ツールにも及ぶが、呼び出し側の変更は不要である。

## 7. 実装順序

依存のない 4.1 と 4.5 を先に、依存のある 4.2 → 4.4 → 4.3 を後にする。

1. **4.1** `clgrep-search`。唯一の P1。他項目と独立。
2. **4.5** `lisp-check-parens` のスライス。他項目と独立。
3. **4.2** `lisp-read-file`。core の `signal-file-unparseable` 抽出を含む。
4. **4.4** 復旧手順の文言。4.2 の後でないと、案内された経路が失敗するように見える。
5. **4.3** `fs-write-file`。警告文が 4.4 の 2 段構えを前提にするので最後。

各項目は独立にコミットできる粒度にし、レビューは項目単位で追えるようにする。

## 8. 改訂履歴

**2026-09-06 改訂** — 承認済み初版に対する設計レビューの反映。方針と 5 項目の
選定は変えていない。

- **2.1 / 4.1**: `%format-clgrep-results` の重複排除が、救ったマッチを本文から
  消すことを追記。未終了フォームは重複排除から除外する。`form_types` の制限を
  注記に含める。構造化フィールド `unterminated`/`notes` を追加。
- **2.2 / 4.2**: 欠陥を 2 から 5 に (余分な `)` が `Internal error` になる、
  lenient 経路の破損が無言で消える、読み取り上限を見ていない)。
  `handler-case` は `end-of-file` ではなく `error` 全体に。第 2 値を処理する。
  `signal-file-unparseable` を core に抽出。プロジェクト外ファイルの分岐を
  `file-unparseable-message` に追加。
- **2.4 / 4.3**: 警告後の 2 回目の書き込みに `allow_unparseable_overwrite=true`
  が要ることを警告文で案内する。hook `t` かつスキャン `:ok` のフォールバック。
  `unparseable` フィールド。
- **2.3 / 4.4**: `lisp-read-file collapsed=false` は全文コピーの元に向かない
  (行の連結し直し、フッタ) ため、位置確認と全文取得の 2 段構えに。
  `:fix-line` で計算済みの `offset` を文面に埋め込む。
  `prompts/repl-driven-development.md` を影響範囲に追加。
- **2.5 / 4.5**: 列の翻訳 (窓の 1 行目)、`reader-info` 分岐への適用、
  流し読みによる前置計測 (フォールバック分岐を廃止)、`window` フィールド。
- **§3**: 出力フィールド追加の許容と「無言で欠落させない」を方針に明記。
- **§7**: 実装順序を追加。
