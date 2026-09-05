# 括弧不整合の診断強化(check-parens / edit-form / patch-form)設計

- 日付: 2026-09-03
- ステータス: 設計承認済み(実装未着手)
- 想定受益者: cl-mcp を利用する AI エージェント。特に推論力の弱いモデル

## 1. 背景と問い

「括弧の対応が壊れたフォームが `lisp-check-parens`、`lisp-edit-form`、
`lisp-patch-form` に渡されたとき、どこをどう直せばよいかの情報を増やすべきか。
増やすなら何を増やすか」を評価した。

結論は **増やすべき**。現状の出力は「壊れている」ことしか伝えておらず、
弱いモデルが最も必要とする「どの行に何を足す/消すか」がない。
ファイル自体が壊れた状態では edit/patch が「end of file on」だけを返し、
行も次に呼ぶべきツールも示さない。

## 2. 現状調査(実測)

一時ファイルを作り、壊れた入力を各ツールに投げた結果。

| 入力 | ツール | 実際の出力 | 問題 |
|---|---|---|---|
| `(let ((y 1)` の後に `)` 欠落 | check-parens | `unclosed at line 1, column 1` | 位置が常に最外フォームの先頭。欠落箇所は分からない |
| `when` の閉じ忘れ(次行で本体に戻る) | check-parens | `unclosed at line 1, column 1` | 同上 |
| 末尾に `)` が 1 個多い | check-parens | `extra-close at line 3, column 14` | 位置は正しいが、消すべきなのはそこか判断できない |
| 閉じ忘れ、dry_run なし | edit-form | `WARNING: 1 closing delimiter was added by parinfer` | 修復後のフォームを見せない。何が書かれたか不明 |
| `)` が 1 個多い | edit-form | `WARNING: -1 closing delimiters were added` | 負数で意味不明。実際は削除している |
| `((y 1]` | edit-form | `WARNING: 2 closing delimiters were added` で成功 | `(y 1])` を書き込む。`]` はシンボル構成文字なので無警告で破損 |
| new_text の `)` 欠落 | patch-form | `patch operation produced invalid Lisp: end of file on .` | 位置なし、不足数なし |
| new_text の `)` 過多 | patch-form | `trailing content after form` | 「余分な `)`」と読み替えられない |
| ファイル自体が壊れている | edit-form / patch-form | `end of file on` | 行も対象フォームも `lisp-check-parens` への誘導もない |

### 2.1 原因

- `%scan-parens`(`src/validate.lisp`)はスタック走査なので、閉じ忘れは
  末尾の連続する `)` に吸収され、残るのは最外フォームの開き括弧になる。
  欠落位置は括弧の数だけでは原理的に特定できない。
- `%validate-and-repair-content`(`src/lisp-edit-form.lisp`)は
  `(length repaired) - (length content)` を「追加数」として報告するため負数が出る。
  parinfer は `]` を閉じ括弧として扱わないので、読める形になれば成功扱いになる。
- `%validate-form-parseable`(`src/lisp-patch-form.lisp`)は読み直しの例外文面を
  `sanitize-error-message` で加工するだけで、位置情報を持たない。
- `%locate-target-form`(`src/lisp-edit-form-core.lisp`)は
  `parse-top-level-forms` の失敗をそのまま上に投げる。

### 2.2 兄弟 JSON フィールドは描画されない

`position`、`next_tool`、`fix_code` などの兄弟フィールドは多くのクライアントで
描画されない(2026-03-07 の総合テスト報告と同じ知見)。追加情報は必ず
`content[].text` に入れる。

### 2.3 parinfer 差分は欠落位置を当てられる

`cl-mcp/src/parinfer:apply-indent-mode` の出力と原文の行差分をワーカーで試した。
上記 4 ケースすべてで、括弧を足す/消すべき行を正しく指した。

```
;; (let ((y 1) の閉じ忘れ
((:line 2 :original "  (let ((y 1)" :repaired "  (let ((y 1))"))
;; 末尾の余分な )
((:line 3 :original "    (+ x y))))" :repaired "    (+ x y)))"))
;; when 本体の閉じ忘れ
((:line 4 :original "      (format t \"~A\" y)" :repaired "      (format t \"~A\" y))"))
;; ファイル中間フォームの閉じ忘れ
((:line 8 :original "        (format t \"small ~A~%\" y)"
         :repaired "        (format t \"small ~A~%\" y))"))
```

インデントが正しい前提の推定なので、文面にその旨を含める。

### 2.4 列 0 ヒューリスティック

深さが 0 でないのに列 0 に `(` が現れた行は、直前のトップレベルフォームが
閉じていないことをほぼ確実に示す。ファイル中間フォームの閉じ忘れの例では
11 行目と 14 行目を検出し、閉じていないフォームの開始行 3 を同時に返せた。

## 3. 判断

質問への回答は次の 2 点で確定した。

- スコープ: check-parens の推定位置、patch-form の診断強化、edit-form の可視化、
  ファイル破損時の誘導の **すべて** を含める。
- `lisp-edit-form` の parinfer 自動修復は **現状通り自動適用し、結果を可視化する**。
  試した 4 ケースはすべて正しく修復できており、互換性も保てる。

実現方式は **共通診断モジュールの新設(案 A)**。3 ツールが同じ関数を呼び、
同じ状況で同じ文面を返す。

## 4. 診断コア `src/paren-diagnostics.lisp`

### 4.1 パッケージと依存

パッケージ名は `cl-mcp/src/paren-diagnostics`。依存は `cl-mcp/src/parinfer` のみ。
ツール層(`tools/helpers`、`fs`)には依存しない純関数群とする。

`src/validate.lisp` から括弧スキャナ(`%scan-parens`、`%scan-parens-push-open`、
`%scan-parens-pop-open`、`%scan-handle-*`、`%scan-advance-position`、
`scan-state` 構造体、`%closing`)をここへ移し、`validate.lisp` は import して使う。
`tests/validate-test.lisp` と `tests/parinfer-test.lisp` は公開関数しか参照して
いないので、移設による既存テストの変更はない。

新規ファイルは `cl-mcp.asd` の編集を要しない(package-inferred-system)。
`src/main.lisp` への export 追加は不要(ツールではないため)。

### 4.2 公開関数

**`scan-delimiters (text &key (base-offset 0))`**
移設したスキャナ本体。戻り値は現行と同一の plist
`(:ok bool :kind string-or-nil :expected :found :offset :line :column)`。
`kind` は `"extra-close"`、`"mismatch"`、`"unclosed"`、`"unclosed-block-comment"`
のいずれか。`lisp-check-parens` の既存フィールドはこの値をそのまま使うので変わらない。

**`diagnose-delimiters (text &key (base-offset 0))`**
`scan-delimiters` の結果に次のキーを追加した plist を返す。`:ok t` のときは
追加キーを持たない。

- `:likely-fixes` は `apply-indent-mode` の出力と原文の行差分のリスト。
  各要素は `(:line n :original "..." :repaired "..." :delta d)`。
  `d` は当該行で追加した閉じ括弧数(削除なら負)。行番号は現行スキャナと同じく
  `text` 内の 1 始まり行番号で、`base-offset` は `:offset` にだけ加算する。
- `:repair-failed` は修復結果に(文字列・コメント外の)`]` または `}` が残る、
  または修復結果を `scan-delimiters` にかけても釣り合わないときに `t`。
  このとき `:likely-fixes` は `nil`。標準リーダーでの読み直しは行わない
  (未知パッケージで偽陽性になるため)。
- `:next-top-level-line` は深さが 0 でないのに列 0 に `(` が現れた最初の行番号。
  なければ `nil`。文字列、行コメント、ブロックコメント、文字リテラルは
  スキャナと同じ規則で除外する。
- `:unclosed-form-line` と `:unclosed-form-head` は、スキャナが報告する
  閉じられていないフォーム(スタックに残った最内の開き括弧。末尾に閉じ括弧が
  連なる典型例では最外フォームと一致する)の開始行番号と、その行を trim した
  先頭最大 40 文字。`kind` が `"unclosed"` のときのみ。

**`count-delimiter-depth (text)`**
文字列、行コメント、ブロックコメント、文字リテラルを除いた開き括弧数と
閉じ括弧数を 2 値で返す。`(`/`)` のみ数える(`[` `{` は CL では構成文字)。
`lisp-patch-form` が old_text と new_text を比較するのに使う。

**`format-delimiter-diagnosis (diagnosis &key target)`**
診断 plist を人間向け文面にする。`target` は文面の主語で、
`"code"`、`"content"`、`"new_text"`、またはファイルパス文字列。
3 ツールがこの関数を通すことで文面を揃える。

### 4.3 文面

kind ごとの雛形。`Likely fix` 以下は `:likely-fixes` が空でないときだけ出す。
差分行は `:original` を `~S` で示し、`:delta` の符号で「add」「remove」を選ぶ。

`unclosed`:
```
Unbalanced parentheses in <target>: unclosed (form starting at line 3: "(defun probe-a").
Likely fix, inferred from indentation:
  line 8: "        (format t \"small ~A~%\" y)"  ->  add 1 ")"
Next top-level form begins at line 11, so the missing ")" must come before it.
```
最終行は `:next-top-level-line` があるときだけ出す。

`extra-close`:
```
Unbalanced parentheses in <target>: extra ")" at line 3, column 14.
Either remove that ")" or check for a form opened earlier that was never closed.
Likely fix, inferred from indentation:
  line 3: "    (+ x y))))"  ->  remove 1 ")"
```

`mismatch`:
```
Unbalanced parentheses in <target>: expected ")" but found "]" at line 2, column 13.
"]" and "}" are ordinary symbol characters in Common Lisp and cannot be auto-repaired.
Replace it with ")".
```

`:repair-failed t` のときは `Likely fix` 節の代わりに
`Automatic repair could not produce a readable form; fix the delimiters by hand.` を出す。

`unclosed-block-comment` は現行文面のまま。

## 5. ツールごとの仕様

### 5.1 `lisp-check-parens`(`src/validate.lisp`)

- `lisp-check-parens` 関数は `scan-delimiters` の代わりに `diagnose-delimiters` を
  呼び、戻りハッシュに `likely_fixes`(配列。各要素は `line`、`original`、
  `repaired`、`delta` を持つハッシュ)と `next_top_level_line` を追加する。
  既存キー(`ok`、`kind`、`expected`、`found`、`position`、`fix_code`、
  `next_tool`、`required_args`)は変更しない。
- define-tool の要約文は、現行の 1 行目の後に `format-delimiter-diagnosis` の
  文面を続ける。`reader-error` と `too-large` の経路は変更しない。
- 失敗時のみ parinfer を 1 回追加実行する。入力上限は既存の
  `*check-parens-max-bytes*`(2MB)に従う。

### 5.2 `lisp-edit-form`(`src/lisp-edit-form.lisp`)

- `%validate-and-repair-content` は修復成功時に、警告文字列に加えて
  変更行差分(`repair-line-differences` の結果)を第 3 値として返す。
  修復後テキストは従来通り第 1 値。警告文は差分の `delta` を集計して、
  「N closing delimiter(s) added by parinfer」と
  「N extra closing delimiter(s) dropped by parinfer」を別々に出す。
  負数は出さない。両方起きた場合は両方を並べる。
- 修復後テキストに `]` `}` が残る場合、または修復後も読めない場合は
  `content-unrepairable-error` を signal する。文面は `content` を主語にした
  `format-delimiter-diagnosis` の出力。書き込みは行わない。
- define-tool の成功応答(dry_run の有無を問わず)は、警告があるとき
  WARNING の後に修復後フォーム(`%truncate-snippet` で 2048 文字に切り詰め)と
  `Likely fix` 形式の変更行差分を続ける。
- `content-unrepairable-error` と次節の `file-unparseable-error` は、既存の
  `multiple-top-level-forms-error` と同じく protocol version で
  `isError` 結果か `rpc-error` かを選ぶ。

### 5.3 ファイル自体が壊れている場合(`src/lisp-edit-form-core.lisp`)

- `%locate-target-form` は `parse-top-level-forms` を `handler-case` で囲み、
  失敗時に `file-unparseable-error` に変換する。条件は `path` と診断 plist を持つ。
- 文面はファイルパスを主語にした診断文面に、次の手順を続ける。
  ```
  The file itself does not parse, so no form can be located.
  Run lisp-check-parens with path="<abs>" to see the full diagnosis, then
  use lisp-edit-form (operation "replace") on the form starting at line 3.
  ```
- `lisp-edit-form` と `lisp-patch-form` の両 define-tool でこの条件を捕まえ、
  上記文面をそのまま返す。`patch-form` は既存の `tool-error` 経路を使う。

### 5.4 `lisp-patch-form`(`src/lisp-patch-form.lisp`)

- `lisp-patch-form` 関数は `%locate-target-form` を呼ぶ前に
  `count-delimiter-depth` で old_text と new_text の `open - close` を比べ、
  差があれば `patch-operation-error` を signal する。ファイルは読まない。
  ```
  new_text closes 1 fewer ")" than old_text (old_text: 2 open / 1 close,
  new_text: 2 open / 0 close). The patch would leave the form unclosed.
  Add 1 ")" to new_text, or remove 1 "(". No changes were written to disk.
  ```
  過多の場合は「closes 1 more」「Remove 1 ")" from new_text, or add 1 "("」。
- 差が 0 でも `%validate-form-parseable` が失敗する場合は、修正後フォーム全体に
  `diagnose-delimiters` を適用し、`new_text` を主語にした診断文面を
  `patch-operation-error` に載せる。「No changes were written to disk.」は残す。
- 既存の「end of file on .」「trailing content after form」文面は使わない。

## 6. 挙動変更と互換性

破壊的になりうる変更は 3 点。

1. `]` `}` 混入時の `lisp-edit-form` が成功からエラーへ変わる。現在は壊れた
   フォームを書き込んで成功扱いなので、修正として扱う。
2. `lisp-patch-form` の失敗文面が変わる。「No changes were written to disk」は
   新文面にも残す。
3. `lisp-edit-form` の警告文面が変わる。既存テスト 2 件は
   「closing delimiter」「parinfer」の部分一致なので影響しない。

正常系では parinfer を余分に走らせないため性能は変わらない。

## 7. テスト戦略

Rove で TDD 順(失敗テストを先に書く)で進める。

- **`tests/paren-diagnostics-test.lisp`(新規)**
  2.3 節の 4 ケースを固定入力にして、`:likely-fixes` の行番号と `:delta`、
  `:next-top-level-line`、`:unclosed-form-line`、`:unclosed-form-head` を検証する。
  `]` 残存で `:repair-failed t` かつ `:likely-fixes nil` になること、
  `count-delimiter-depth` が文字列、コメント、`#\(` を数えないこと、
  `base-offset` 付きで行番号がずれないこと、
  `format-delimiter-diagnosis` が kind ごとの雛形を出すことを含める。
  ルートの `tests.lisp` にスイートを登録する。
- **`tests/validate-test.lisp`**
  要約文に「Likely fix」と行番号が含まれること、`likely_fixes` フィールドの形。
  既存テストは無変更で通ること。
- **`tests/lisp-edit-form-test.lisp`**
  非 dry_run の応答に修復後フォームが含まれること、`]` 混入がエラーになり
  ファイルが変わらないこと、`)` 過多で「dropped」文面になること、
  壊れたファイルへの edit が `lisp-check-parens` への誘導と開始行を含むこと。
- **`tests/lisp-patch-form-test.lisp`**
  深さ差分の事前検出(不足と過多)、差 0 で入れ子だけ壊れる場合の推定行、
  壊れたファイルへの patch の文面。

## 8. フェーズ分割と影響ファイル

### PR 1: 診断コアと `lisp-check-parens`(単独でマージ可能)

- 新規: `src/paren-diagnostics.lisp`、`tests/paren-diagnostics-test.lisp`
- 変更: `src/validate.lisp`(スキャナ移設、`diagnose-delimiters` 呼び出し、要約文)、
  `tests/validate-test.lisp`、`tests.lisp`

### PR 2: `lisp-edit-form` の可視化とエラー化、ファイル破損時の誘導

- 変更: `src/lisp-edit-form.lisp`、`src/lisp-edit-form-core.lisp`
  (`file-unparseable-error` 定義と `%locate-target-form`)、
  `tests/lisp-edit-form-test.lisp`

### PR 3: `lisp-patch-form` の深さ差分と診断文面

- 変更: `src/lisp-patch-form.lisp`、`tests/lisp-patch-form-test.lisp`
- PR 2 の `file-unparseable-error` に依存する。

各 PR の前に `(asdf:compile-system :cl-mcp :force t)` と `rove cl-mcp.asd`、
`mallet src/*.lisp` を通す。

## 9. 採用しなかった案

- **括弧数は釣り合っているが入れ子が意図と違う patch の検出**: 原理的に
  不可能。dry_run の案内に留める。
- **自動修復を末尾追加だけに制限する案**: let 束縛の閉じ忘れのような
  途中行への挿入が正しい修復であるケースをわざわざ失敗させることになる。
  質問 2 の決定通り不採用。
- **`validate.lisp` を拡張して他ツールから import する案(案 B)**:
  ツール定義と診断ロジックが同居したまま膨らみ、`lisp-edit-form-core` が
  check-parens ツールのファイルに依存する向きになる。
- **ツールごとに個別対応する案(案 C)**: parinfer 差分の計算と整形が
  3 箇所に複製され、文面がずれる。

## 10. 実装時の決定と既知の制約

- **§5.2 の dry_run 応答**: `--- preview ---` が修復後のフォームそのもの
  なので、`--- repaired form ---` を重複して表示しない。修復が起きたことは
  `repair` の要約文と変更行の一覧で伝える。
- **§5.4 の深さ検査**: 「ファイルを読む前に拒否」ではなく、パッチ適用後に
  パースが失敗した場合にのみ深さメッセージを使う方式に変更した。文字列や
  コメントの中で括弧を足す正当なパッチ(docstring の `(1-based` を
  `(1-based)` に直すなど)を誤って拒否しないため。
- **`apply-indent-mode` の制約**: 複数行 docstring を跨ぐ行では誤った閉じ
  括弧を挿入するため、そのようなフォームでは `:repair-failed` となり
  `Likely fix` は出ない(`:unclosed-form-line` と `:next-top-level-line` は
  出る)。parinfer 側の修正はフォローアップとする。
- **§4.2 の `:repair-failed` 判定**: 修復結果に `]`/`}` が残るだけでは失敗と
  しない。`scan-delimiters` で釣り合わない場合のみ失敗とする(`]` が `(` を
  閉じている打ち間違いは `mismatch` として引き続き拒否される)。リーダー
  マクロで `{...}` や `[...]` を対で使う内容を誤拒否しないため(Codex レビュー
  P1 への対応)。
- **§5.3 の誘導文**: 「`lisp-edit-form` で replace せよ」ではループする
  (`lisp-edit-form` も同じファイルをパースして同じエラーになる)。誘導は
  「`fs-read-file` で読み、Likely fix を手で当てて `fs-write-file` で書き戻す」に
  変更し、`fs-write-file` の既存 `.lisp` 上書き禁止は「ファイルがパースできない
  間は許可」に緩めた(Codex レビュー P2 への対応)。「パースできない」の判定は
  標準リーダー(`*read-eval*` nil)のみで行い、括弧スキャナは使わない。
  スキャナは `[`/`{` を開き括弧として扱うため、`foo[` のようなシンボルを含む
  正当なファイルの保護が外れてしまうから。さらに、判定を編集ツール自身の
  パーサ(`parse-top-level-forms`)と一致させるため、`fs` に
  `*lisp-file-unparseable-hook*` を置き、`lisp-edit-form-core` がロード時に
  そのパーサを使う述語を登録する(`fs` から `cst` を直接呼ぶと循環依存になる)。
  これにより `(in-readtable ...)` は構造的に検出され、コメント内の言及では
  誤判定しない。フック未登録時のみ標準リーダーによるフォールバックを使い、
  その場合は `in-readtable` を含むファイルを「パース可能」扱いにする。
- **読み取り上限との関係**: `fs-read-file` は 1 MiB で切り詰めるため、切り詰め
  られた読み取りは「パース可能」扱い(上書き不可)とし、`%locate-target-form`
  は括弧の診断ではなく「読み取り上限を超えている」というエラーを返す。
- **`in-readtable` 切り替え後の読み取りエラー**: `cst` の CL リーダー経路は
  読み取りエラーを握りつぶして途中までのノードを返す(`lisp-read-file` が
  壊れたファイルの前半を表示できるよう寛容さは維持)。代わりに
  `parse-top-level-forms` が第 2 値でそのエラーを返し、フックはそれを
  「パース不能」と扱い、`%locate-target-form` は対象フォームが見つからず
  第 2 値がある場合に「Form not found」ではなく `file-unparseable-error` を
  返す。壊れた位置より前のフォームは従来通り編集できる。復旧手順の文面は
  括弧診断が出せない場合(リーダーエラー等)にも必ず含める。
  併せて `package-context` のヘッダーフォーム抽出(`in-package` 探索)を
  ベストエフォートにした。従来はヘッダーフォームの無いファイルを末尾まで
  CL リーダーで読み、壊れたフォームで `end-of-file` を `parse-top-level-forms`
  より先に投げていたため、「壊れた位置より前は編集可能」が成り立たなかった。
- **上書き許可は「区切り記号の破損」に限る**: `readtable` 引数でカスタム構文
  (`#?"..."` 等)を読ませる運用のファイルは既定リーダーでは失敗するが、編集
  ツールは `readtable` を渡せば編集できる。括弧スキャンはリーダーマクロが
  データとして消費する `(` を見分けられない(`#?[(]` 等)ので証拠にならない。
  代わりにパース失敗の **条件型** で判定する: 閉じ忘れ(`end-of-file` /
  `cst:unterminated-source`)と余分な `)`(`cst:stray-right-parenthesis`、
  CL リーダー経路では SBCL の「unmatched close parenthesis」)だけを、どの
  リーダーテーブルでも直らない破損として上書き可とし、未知のディスパッチ
  マクロなど他のリーダーエラーでは保護を維持する。`file-unparseable-error` は
  この分類を `recoverable` として持ち、誘導文もそれに従って「復旧手順」か
  「`readtable` 引数の案内と上書き拒否の明記」かを切り替える。
  ただし条件型でも決着しない(`[` を `]` まで読むマクロなら `[(]` は既定リーダー
  で `end-of-file` になる)。リーダーテーブルを知らない限り「リーダーの挙動と
  独立に破損を立証する」ことは原理的に不可能なので、最終的な判断は呼び出し側に
  委ねる: `fs-write-file` に `allow_unparseable_overwrite`(既定 false)を追加し、
  既存 `.lisp` の上書きは「この引数が真 **かつ** 区切り記号の破損でパースが失敗
  する」場合のみ許可する。パースできるファイルはフラグがあっても上書きしない。
  余分な `)` は CL リーダー経路でも `cst` 自身が空白と行・ブロックコメントを
  読み飛ばしたうえで `peek-char` で検出して `stray-right-parenthesis` を返すので、
  処理系のエラー文面には依存しない。閉じられていない `#|` で入力が終わった
  場合は、コメントの正常終了ではなく `unterminated-source` として第 2 値で返す。
  この構造的検出は、そのリーダーテーブルで `)` が標準のマクロ関数のまま
  (`get-macro-character` が標準と同一)である場合にだけ行う。`)` を再定義した
  独自構文ではリーダー自身に解釈させる。コメントの読み飛ばしも同様に、`;` と
  `#|` が標準のマクロ関数のままである場合にだけ行い、再定義されていれば
  そこで止めて `read` に任せる。
- **単一エスケープ**: 文字列外の `\` は次の文字をシンボルの一部にするので、
  両方の走査器(`scan-delimiters`、`%map-code-characters`)で `\(` `\)` を
  区切り記号として数えない。
- **`likely_fixes` 配列の上限**: 要約文と同じ `*repair-lines-limit*`(10)で
  切り、省略数を `likely_fixes_omitted` で返す。
- **§5.4 の深さ計算の文脈**: `old_text`/`new_text` を単独で数えるのではなく、
  フォーム全文の中で各断片が占める区間だけを、先頭からの文字列・コメント状態を
  保って数える(`count-delimiter-depth` の `start`/`end`)。文字列内の `)` を
  コードと誤認して、無関係なリーダーエラーに誤った深さメッセージを出さないため。
  さらに、置換終端の字句状態(`lexical-state-at`: コード / 文字列 / 行コメント /
  ブロックコメント)が元フォームと置換後フォームで一致する場合にだけ深さ
  メッセージを使う。`new_text` が文字列やコメントを開いたまま終わると区間外の
  同一テキストが再分類され、区間だけの計数では偽の差分が生じるため。
- **`repair-line-differences` の行長上限**: `:original`/`:repaired` は 120 文字で
  切り詰めて `...` を付ける(`:delta` は全文で計算)。1 行が極端に長い入力で
  `likely_fixes` と要約文が数 MB になるのを防ぐ。
