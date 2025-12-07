# ツール仕様書: `edit-lisp-form`

## 1\. 概要

`edit-lisp-form` は、Common Lispソースファイル内のトップレベルフォーム（`defun`, `defmacro`, `defclass` 等）を、**構造を認識して安全に編集**するためのツールです。

単純なテキスト置換（sed/regex）とは異なり、Lispの構文木（CST: Concrete Syntax Tree）を操作するため、以下のメリットがあります。

  * **安全性**: 括弧の対応（Balanced Parentheses）が崩れるのを防ぎます。
  * **保存性**: 編集対象以外のコメントやインデント、空白を維持します。
  * **確実性**: 行番号ではなく、「定義の種類」と「名前」で対象を特定するため、行ズレによる誤編集を防ぎます。

## 2\. 入力パラメータ (JSON Schema)

| パラメータ名 | 型 | 必須 | 説明 |
| :--- | :--- | :--- | :--- |
| `file_path` | string | Yes | 編集対象のファイルパス（絶対パス推奨）。 |
| `form_type` | string | Yes | 探すフォームの種類（シンボル名）。<br>例: `"defun"`, `"defmacro"`, `"defvar"`, `"defmethod"` |
| `form_name` | string | Yes | 探すフォームの名前。<br>`defmethod` の場合は特殊化子も含めた識別子を指定する（後述）。 |
| `operation` | string | Yes | 実行する操作の種類。<br>`"replace"`: 置換<br>`"insert_before"`: 直前に挿入<br>`"insert_after"`: 直後に挿入 |
| `content` | string | Yes | 新しく挿入/置換する完全なLispコード文字列。 |

## 3\. 動作仕様

### 3.1 処理フロー

1.  **入力検証 (Linting)**

      * 受け取った `content` 文字列を `read-from-string` でパースし、単体で構文エラー（括弧の不整合など）がないか確認する。
      * エラーがある場合は、ファイルには一切触れず即座にエラーを返す。

2.  **ファイル解析 (CST Parse)**

      * 対象ファイルを `eclector.concrete-syntax-tree:read` を用いて読み込み、CST（具象構文木）のリストとしてメモリ上に展開する。これによりコメントや空白情報も保持される。

3.  **ターゲット特定**

      * トップレベルのフォームを走査し、以下の条件に一致するものを探す。
          * 第1要素（オペレータ）が `form_type` と一致する（パッケージプレフィックスは無視または解決して比較）。
          * 第2要素（名前）が `form_name` と一致する。

    > **注意: `defmethod` の扱い**
    > `defmethod` の場合、名前だけでは一意に定まらないため、`form_name` にはメソッド名だけでなく、修飾子や特定化子（specializers）を含めた文字列表現のマッチングを試みるか、独自の検索ロジックを適用する。

    >   * 例: `form_name: "print-object (my-class t)"`

4.  **編集操作 (Tree Transformation)**

      * **`replace`**: 特定したCSTノードを、`content` から生成した新しいCSTノードに差し替える。
      * **`insert_before`**: 特定したノードの直前の空白ノードの前に、新しいノードと改行を挿入する。
      * **`insert_after`**: 特定したノードの直後の空白ノードの後に、改行と新しいノードを挿入する。

5.  **再構築と書き出し**

      * 編集されたCSTリストを文字列に再構築し、ファイルに書き出す。

### 3.2 エラーハンドリング

  * **フォームが見つからない場合**:
      * 指定された `form_type` と `form_name` に一致するものがファイル内に存在しない場合、エラーを返す。
      * 可能であれば、「似た名前の関数」や「同名の別タイプ定義（defgeneric等）」をヒントとしてエラーメッセージに含める。
  * **構文エラー**:
      * `content` が不正なLispコードである場合、具体的なリーダーエラー（例: `Unmatched close parenthesis`）を返す。

## 4\. `defmethod` の特定ロジック詳細

Common Lispの `defmethod` は同名の定義が複数存在しうるため、以下のルールで特定を行う。

1.  ユーザーは `form_name` に `"メソッド名 [修飾子] (引数リスト...)"` のような形式を指定する。
      * 例: `"calculate-area (shape rectangle)"`
2.  ツール側は、ファイル内の全 `defmethod` をスキャンし、第2要素（メソッド名）が一致するものを抽出する。
3.  抽出した候補の中から、引数リスト（lambda list）の特定化子がユーザー指定と一致するものを探す。

## 5\. 実装に必要なライブラリ (Common Lisp)

  * **`eclector`**: CST（Concrete Syntax Tree）を扱うためのポータブルなリーダー。標準の `read` では捨ててしまうコメント等を保持するために必須。
  * **`closer-mop`**: （オプション）より高度なメソッド特定が必要な場合に使用。

## 6\. 使用例 (Prompt Example)

### ケース1: 関数のバグ修正（置換）

**User (LLM)**:

```json
{
  "file_path": "src/utils.lisp",
  "form_type": "defun",
  "form_name": "string-split",
  "operation": "replace",
  "content": "(defun string-split (str delimiter)\n  (uiop:split-string str :separator delimiter))"
}
```

### ケース2: 新しいヘルパー関数の追加（前挿入）

**User (LLM)**:

```json
{
  "file_path": "src/server.lisp",
  "form_type": "defun",
  "form_name": "start-server",
  "operation": "insert_before",
  "content": "(defun port-available-p (port)\n  ;; Check logic...\n  t)"
}
```

### ケース3: メソッドの追加

**User (LLM)**:

```json
{
  "file_path": "src/shapes.lisp",
  "form_type": "defmethod",
  "form_name": "draw ((shape circle))", 
  "operation": "replace",
  "content": "(defmethod draw ((shape circle))\n  (format t \"Drawing a circle radius ~a\" (radius shape)))"
}
```

## 7\. 開発者向けメモ

  * **パッケージの扱い**: `in-package` フォームを解析し、現在のパッケージコンテキストを把握した上でシンボル比較を行うのが理想ですが、初期実装では `string-equal`（大文字小文字無視の文字列比較）でシンボル名を比較する簡易実装でも十分実用的です。
  * **安全性**: `file_write` (全書き換え) とは異なり、このツールは「既存のコード構造を壊さない」ことを保証するため、LLMに対しては「既存ファイルの修正には必ず `file_write` ではなく `edit-lisp-form` を使うこと」とプロンプトで強く指示する必要があります。
  
