# 括弧診断の周辺ツールへの適用設計

- 日付: 2026-09-06
- ステータス: 設計承認済み(実装未着手)
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
案内文を 1 箇所直すだけ。** 新ツール・新パラメータもゼロ。

## 2. 現状調査(実測)

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

### 2.2 `lisp-read-file` の 2 つの欠陥

`src/lisp-read-file.lisp:619` `%lisp-read-file-content` の
`handler-case (end-of-file ...)` が独自の短い文面を作っている。

1. 共通診断モジュールを呼んでいない。行番号も Likely fix も復旧手順もなく、
   別ツールを呼べと言うだけ。
2. `src/tools/define-tool.lisp:178` の汎用ラッパに落ちて
   **`Internal error during ...`** と表示される。これはユーザーデータの
   問題であって内部障害ではない。この文言はエージェントに cl-mcp のバグと
   誤認させるか、同じ呼び出しの再試行を促す。

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

**(b) 行番号がスライス相対のまま。** これは `scan-delimiters` の仕様である。
`src/paren-diagnostics.lisp:201` の docstring:

> BASE-OFFSET is added to :offset only; :line and :column are always relative
> to the start of TEXT.

`:offset` はファイル絶対に補正されるが `:line`/`:column` は補正されない。
翻訳していないのは `lisp-check-parens` 層。上の実測の「line 1」は
**スライスの 1 行目**であってファイルの 1 行目ではない。

## 3. 判断

スコープは次の 5 項目。`main` (6f80ab0) から切った別ブランチ・別 PR とする。
PR #141 は 8,800 行・レビュー 22 ラウンドの規模だったので、
「診断モジュールの利用者を増やす」という別テーマとして独立してレビューできる。

方針の要点:

- **新しい診断ロジックを書かない。** `paren-diagnostics` は完成している。
- **新ツール・新パラメータを増やさない。** `apply_fix` のような修復適用
  プリミティブは検討したが見送る (5.6 参照)。
- 各ツールの既存の性格を壊さない。特に `clgrep-search` は
  「何もロードせずに常に動く」ことが利点なので、他ツールのように
  拒否させない。

## 4. 各項目の設計

### 4.1 `clgrep-search`: 未終了フォームを採用し警告する

**スキャナ層** (`src/utils/clgrep.lisp`) — 新しい依存は追加しない。

- `toplevel-form` 構造体に `unterminated-p` スロットを追加 (既定 `nil`)。
- `scan-toplevel-forms` のループ終了後、`form-start-pos` が non-nil なら
  `end-pos` を `len`、`end-line` を `current-line`、`unterminated-p` を `t` として
  push する。

これだけで、破損位置以降のマッチが囲みフォームに紐づき、
`grep-file-structured` の `(when form-info ...)` から落ちなくなる。

`scan-toplevel-forms` は `paren-depth > 0` を自分で知っているので、
検出に `paren-diagnostics` は要らない。この層は `cl-ppcre` のみに依存する
純粋なスキャナのまま保つ。

**ツール層** (`src/clgrep.lisp`) — 未終了フォームを含んでいたファイルごとに
1 行の注記を出力する。

```
NOTE: src/tokens.lisp does not parse (unclosed form starting at line 39).
  Results at or below that line are attributed to it. Run lisp-check-parens for the fix.
```

- ファイル単位。複数ファイル検索で 1 ファイルが壊れていても他は無影響。
- 注記は該当ファイルにマッチがあった場合にのみ出す。壊れているが
  マッチのないファイルについて注記を並べても雑音にしかならない。
- 注記は `content[].text` に入れる。兄弟 JSON フィールドは多くの
  クライアントで描画されない (2026-03-07 総合テスト報告、および
  PR #141 設計書 2.2 と同じ知見)。

### 4.2 `lisp-read-file`: 共通診断に接続する

`src/lisp-read-file.lisp` は既に `cl-mcp/src/lisp-edit-form-core` を
import している (`%parse-readtable-designator`)。import 句を広げるだけで
`file-unparseable-error` / `file-unparseable-message` が使える。

- `%lisp-read-file-content` の `handler-case (end-of-file ...)` を
  `lisp-edit-form-core` の unparseable 経路に差し替え、
  `file-unparseable-message` の文面をそのまま使う。
  これで行番号・Likely fix・列 0 ヒント・復旧手順が他 3 ツールと同一になる。
- ツール本体で catch し、`define-tool` の汎用
  `Internal error during ~A` に落とさない。
  `src/lisp-macroexpand.lisp:513` に同じ判断の前例とその理由がコメントで
  残っているので、それに倣う。
  `arg-validation-error` を `error` 節より先に置く規則も同じく踏襲する。
- 文面に 1 行足す: **`collapsed=false` なら壊れたファイルでも生読みできる。**
  既定は `collapsed=true` なので、これを知らせないとエージェントは
  raw モードの存在に気づけない。

`collapsed=false` は今も動くので挙動を変えない。壊れたファイルで
自動的に raw に落とすフォールバックは**採らない**。呼ばれていない
モードに黙って切り替えるのは、他 3 ツールが揃って拒否するのと
一貫しないうえ、大きなファイルで意図しない出力量になる。

### 4.3 `fs-write-file`: 書いた内容を検証して警告する

`src/fs.lisp` は `cl-mcp/src/lisp-edit-form-core` を import できない。
`*lisp-file-unparseable-hook*` (`src/fs.lisp:174`) の docstring が
その循環回避の理由を明記している。

しかし**その hook 自体が使える**。署名は
「2 引数 (絶対パス名と、そのファイルのテキスト) の述語」なので、
今書いたばかりの内容をそのまま渡せる。ディスクから読み直す必要はない。

- `.lisp`/`.asd` の書き込み後に hook を呼ぶ。
- `t` が返ったら `paren-diagnostics` の `diagnose-delimiters` +
  `format-delimiter-diagnosis` で警告文を組み立て、結果テキストに追記する。
  `fs` から `paren-diagnostics` への import は循環しない
  (`paren-diagnostics` は `parinfer` と `uiop` にしか依存せず、
  `parinfer` は `uiop` のみ)。
- **書き込み自体は必ず成功させる。** 段階的にファイルを組み上げる
  ワークフロー (新規ファイル手順がまさにそれ) を壊さない。
- hook が `nil` (fs だけをロードした部分イメージ) なら黙って skip する。
  既存の「弱いフォールバック定義は置かない」方針を踏襲する。

この設計で次の不変条件が立つ:

> **`fs-write-file` は、上書きを許可するのと厳密に同じ条件でだけ警告する。**

hook は「どんな readtable でも直せない形で壊れている」ときだけ `t` を返すので、
`in-readtable` を使うファイルは自動的に除外され、誤警告しない。
この対称性はテストしやすく、説明もしやすい。

副作用として、リーダーレベルの破損 (`#z` など) には警告が出ない。
hook が `nil` を返すためである。確実なときだけ警告するという保守的な
選択であり、誤警告を出すよりよい。

### 4.4 復旧手順の案内先を直す

`src/paren-diagnostics.lisp:993` `format-overwrite-recovery` の文言を
`fs-read-file` から `lisp-read-file collapsed=false` に変更する。
行ベースであること、および壊れたファイルでも動くことを明記する。

**1 関数の文言変更が 4 ツールすべてに波及する。**

`fs-read-file` は非 Lisp ファイルのフォールバックとしてのみ残す。

実装順序は 4.2 の後。今日は `collapsed` 既定でエラーになるため、
先に案内だけ変えると、案内された経路が失敗するように見える。

### 4.5 `lisp-check-parens` のスライス

**(a) `reader-info` 分岐に窓警告を適用する。**
`src/validate.lisp:404` の分岐で、`partial` が真なら
`:349` と同じ趣旨の `diagnosis_text` を設定する。文面も概念も既にあるので
新規に考えることはない。スライスにおけるリーダーエラーは窓の副産物である
可能性が高い旨を含める。

**(b) `offset` 指定時に行番号をファイル絶対にする。**

- `offset` までの前置テキストを読み、その改行数を `N` とする。
  窓の先頭はファイルの `N+1` 行目なので、`scan-delimiters` が返す
  窓相対の行 `L` (1 起点) はファイル絶対では `L + N` になる。
  報告する `line` にこの加算を行う。
- 窓の絶対文字範囲も併記する。
- 前置が `fs-read-file` の上限 (`*fs-read-max-bytes*`) を超えて読めない場合は
  加算を諦め、**「この行番号は窓の先頭からの相対値である」と明示**して
  フォールバックする。黙って相対値を出すのが今の問題なので、
  フォールバック時こそ明示が要る。
- `scan-delimiters` の契約 (`:line`/`:column` は TEXT 先頭からの相対) は
  変更しない。翻訳は `lisp-check-parens` 層で行う。
  この契約は docstring に書かれており、`diagnose-delimiters` の
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

- 括弧と無関係な dogfooding 指摘 (`run-tests` の FiveAM `Form:` が `~A` 印字、
  `repl-eval` の `max_output_length` が `[object-id: N]` を削る、
  `*print-circle*` の `#1=`、`locals_preview_frames` の無言)。
  `claudedocs/dogfooding-feedback.md` の 2026-09-06 セクションに記録済み。

## 5. テスト方針

既存のテストファイルに追加する。新規テストシステムは作らない。

**`tests/clgrep-utils-test.lisp`** (スキャナ層)
- `scan-toplevel-forms` が未終了フォームを `unterminated-p` 付きで返すこと。
- その `end-pos` が入力長、`end-line` が最終行であること。
- 健全な入力で `unterminated-p` が立たないこと (回帰防止)。

**`tests/clgrep-test.lisp`** (ツール層)
- 未終了フォーム内 (破損行以降) のマッチが結果に現れること。
- そのマッチに囲みフォームの署名が付くこと。
- 破損ファイルに注記が出ること。
- 健全なファイルでは注記が出ないこと (回帰防止)。
- 複数ファイル検索で、健全なファイルの結果が破損ファイルの影響を受けないこと。

**`tests/lisp-read-file-test.lisp`**
- 破損ファイルに対し行番号と Likely fix を含むこと。
- `Internal error` を含まないこと。
- `collapsed=false` の案内が含まれること。
- `collapsed=false` が破損ファイルで従来通り動くこと (回帰防止)。

**`tests/fs-test.lisp`**
- 壊れた `.lisp` の新規作成で警告が出ること、かつ書き込みは成功すること。
- 健全な `.lisp` で警告が出ないこと。
- `allow_unparseable_overwrite=true` で壊れた内容を書いたとき警告が出ること。
- `in-readtable` を使うファイルで誤警告が出ないこと。
- 非 Lisp ファイル (`.md` 等) では検証しないこと。
- hook が `nil` のとき例外を出さず黙って通ること。

**`tests/validate-test.lisp`**
- スライスのリーダーエラーに窓警告が付くこと。
- `offset` 指定時の行番号がファイル絶対であること。
- 前置が読めない場合に相対である旨が明示されること。
- `offset` なしの経路が変わっていないこと (回帰防止)。

**`tests/paren-diagnostics-test.lisp`**
- `format-overwrite-recovery` が `lisp-read-file` を案内すること。

**統合**: `experiments/paren-lab` を再度壊し、
1) `clgrep-search` で破損位置以降の定義が見えること、
2) `lisp-read-file` が行番号を返すこと、
3) 案内通り `lisp-read-file collapsed=false` → `fs-write-file` で復旧できること、
4) 途中で壊れた内容を書いたら警告が出ること
を手動で確認する。

**リント**: コミット前に `mallet src/*.lisp`。
**PR 前**: `(asdf:compile-system :cl-mcp :force t)` で警告を洗い、
`rove cl-mcp.asd` を新規プロセスで実行する
(単一ファイルのテスト実行は失敗を隠すため)。

## 6. 影響範囲

| ファイル | 変更 |
|---|---|
| `src/utils/clgrep.lisp` | `toplevel-form` にスロット追加、EOF 時 push |
| `src/clgrep.lisp` | 破損ファイルの注記 |
| `src/lisp-read-file.lisp` | import 拡張、`handler-case` 差し替え、ツール本体で catch |
| `src/fs.lisp` | `paren-diagnostics` を import、書き込み後検証 |
| `src/paren-diagnostics.lisp` | `format-overwrite-recovery` の文言 |
| `src/validate.lisp` | `reader-info` 分岐の窓警告、行番号の絶対化 |
| `tests/*` | `clgrep-utils-test` `clgrep-test` `lisp-read-file-test` `fs-test` `validate-test` `paren-diagnostics-test` |

`src/lisp-edit-form.lisp` / `src/lisp-edit-form-core.lisp` /
`src/lisp-patch-form.lisp` / `src/lisp-macroexpand.lisp` は変更しない。
4.4 の文言変更の効果はこれらにも及ぶが、呼び出し側の変更は不要である。
