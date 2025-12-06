# ツール仕様書: `lisp-read-file`

## 1\. 概要

`lisp-read-file` は、ファイルシステム上のファイルを読み込むための多機能ツールです。
特にCommon Lispソースファイル（`.lisp`, `.asd`, `.ros` 等）に対しては、トップレベルフォーム（`defun`や`defmacro`等）のシグネチャのみを表示する「折りたたみ表示 (Collapsed View)」をデフォルトで提供し、トークン消費を抑えつつ全体像を把握可能にします。

また、正規表現パターンによるフィルタリングを提供し、特定の関数定義や、特定のキーワードを含む定義のみを展開して表示することができます。

## 2\. 入力パラメータ (Schema)

| パラメータ名 | 型 | 必須 | デフォルト | 説明 |
| :--- | :--- | :--- | :--- | :--- |
| `path` | string | Yes | - | 読み込むファイルのパス（絶対パス、またはプロジェクトルートからの相対パス）。 |
| `collapsed` | boolean | No | `true` | Common Lispファイルの場合、定義の中身を省略して表示するかどうか。`false` の場合はファイルをそのまま読み込む。 |
| `name_pattern` | string | No | `nil` | 関数名や定義名にマッチさせる正規表現（CL-PPCRE互換）。マッチした定義は `collapsed=true` でも展開される。 |
| `content_pattern` | string | No | `nil` | 定義の本体（body）にマッチさせる正規表現。マッチした定義は `collapsed=true` でも展開される。 |
| `offset` | integer | No | 0 | 読み込み開始行（0始まり）。`collapsed=false` の時のみ有効。 |
| `limit` | integer | No | 2000 | 最大読み込み行数。 |

## 3\. 動作仕様

### 3.1 パス検証

  * 入力された `path` は、サーバー設定で許可されたディレクトリ（`allowed-directories`）内にあるか検証しなければなりません。
  * 許可外のパスへのアクセスはエラーとして返します。

### 3.2 ファイル種別の判定

拡張子に基づいて処理を分岐します。

  * **Lispファイル** (`.lisp`, `.lsp`, `.cl`, `.asd`, `.ros`): Lisp専用の「構造化読み込み」を行います。
  * **テキストファイル** (その他): 通常のテキストとして読み込みます。
  * **バイナリ/画像**: (オプション) Base64エンコードして返すか、読み込み不可とします。

### 3.3 Lispファイルの構造化読み込み (`collapsed: true`)

ファイル全体をテキストとして読み込むのではなく、S式（Top-level Forms）として解析し、以下のルールで整形したテキストを返します。

1.  **トップレベルフォームの解析**:

      * リーダー（`eclector` 推奨、または `read`）を使用して、トップレベルのS式を順次読み込みます。
      * **注意**: `read` 時にコードが評価されないように注意してください（`#.` リーダーマクロの無効化など）。また、コメントを保持するために `eclector.concrete-syntax-tree` のようなCST（Concrete Syntax Tree）対応ライブラリの使用が望ましいですが、簡易実装では正規表現やシンプルなパーサでも可とします。

2.  **表示ルールの適用**:

      * **`in-package`**: 常に完全な形で表示します。（コンテキスト把握に必須のため）
      * **定義フォーム** (`defun`, `defmacro`, `defvar`, `defparameter`, `defclass`, `defmethod`, `defgeneric`, `defstruct` 等):
          * **マッチング**: `name_pattern` が定義名（シンボル）にマッチするか、あるいは `content_pattern` がフォーム全体の文字列表現にマッチするかを確認します。
          * **展開**: マッチした場合、そのフォーム全体を表示します。
          * **省略**: マッチしない場合、シグネチャのみを表示し、本体を `...` で置換します。
              * 例: `(defun my-func (a b) ...)`
              * ドキュメント文字列（docstring）がある場合は、先頭の1行だけ表示することを推奨します。
      * **その他のフォーム**:
          * マッチする場合は表示、しない場合は省略（`(progn ...)` など）します。

3.  **リーダー条件 (`#+`, `#-`) の扱い**:

      * 可能な限り、条件分岐を含めたまま表示するか、現在の処理系（SBCL等）で有効な方のみを表示します。Clojure MCPのように `#+(or sbcl ccl) ...` のようにソースコードのまま表示するのが理想です。

### 3.4 テキストファイルの読み込み

  * `collapsed: true` の場合、`content_pattern` が指定されていれば `grep` のようにマッチした行とその前後（例: 5行）のみを抽出して表示します。
  * `collapsed: false` の場合、`offset` から `limit` 行分をそのまま返します。

## 4\. 出力フォーマット (Return Value)

ツールは以下のJSON構造（をCommon Lispの連想リストやハッシュテーブル等で表現したもの）を返します。

```json
{
  "content": "...",       // 整形されたファイル内容
  "path": "/abs/path",    // 正規化されたパス
  "mode": "lisp-collapsed", // "raw", "lisp-collapsed", "text-filtered" 等
  "meta": {
    "total_forms": 50,    // Lispモード時: 総フォーム数
    "expanded_forms": 2,  // Lispモード時: 展開表示されたフォーム数
    "truncated": false    // 行数制限等で途中できれているか
  }
}
```

## 5\. 実装イメージ (Common Lisp)

以下は、このツールのコアとなる「省略表示」ロジックの簡易的な実装イメージです。

```lisp
(defun format-form (form &key expand?)
  (if expand?
      (write-to-string form :pretty t :right-margin 80 :case :downcase)
      (case (car form)
        ((defun defmacro)
         (format nil "(~(~A~) ~(~A~) ~A ...)"
                 (first form)  ; defun
                 (second form) ; name
                 (third form))) ; args
        ((defmethod)
         ;; defmethod name qualifiers... args
         (let* ((name (second form))
                (rest (cddr form))
                (args (find-if #'listp rest))) ; 引数リストを探す
           (format nil "(defmethod ~(~A~) ... ~A ...)" name args)))
        ((in-package)
         (write-to-string form :case :downcase))
        (t
         (format nil "(~(~A~) ...)" (first form))))))

;; 使用例
;; (read-lisp-file "src/main.lisp" :name-pattern "start-server")
```

## 6\. 開発ロードマップ

1.  **v0.1**: `uiop:read-file-string` を使った単純なテキスト読み込み（`offset`, `limit` 対応）の実装。
2.  **v0.2**: `read` 関数を使った基本的なLispフォームのパースと、`defun` / `defmacro` の単純な省略表示の実装。
3.  **v0.3**: `cl-ppcre` を統合し、`name_pattern` と `content_pattern` によるフィルタリング機能の実装。
4.  **v0.4**: コメントや空白を可能な限り維持するための、より高度なリーダー（CSTベース）への移行。

この仕様に基づき実装することで、LLMエージェントはCommon Lispの巨大なコードベースであっても、必要な情報だけを効率的に収集できるようになります。
