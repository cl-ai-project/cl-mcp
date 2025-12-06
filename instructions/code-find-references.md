# ツール仕様書: `code-find-references`

## 1. 概要
`code-find-references` は、指定されたシンボルがプロジェクト内の「どこで使用されているか」を特定するためのツールです。
リファクタリング（関数名の変更や引数の変更）を行う際、`code-find` で定義元を確認した後に、本ツールを用いて影響範囲（呼び出し元）を洗い出し、一括修正を行うフローを支援します。

SBCL の `sb-introspect` (XREF機能) を利用し、テキスト検索ではなく「意味的な参照」を検索します。これにより、コメントアウトされたコードや、偶然同じ名前を持つ別の変数を誤って検出することを防ぎます。

## 2. 入力パラメータ (Schema)

| パラメータ名 | 型 | 必須 | デフォルト | 説明 |
| :--- | :--- | :--- | :--- | :--- |
| `symbol` | string | Yes | - | 検索対象のシンボル名（例: `cl-mcp:run`, `my-func`）。パッケージ修飾付きを推奨。 |
| `package` | string | No | `CL-USER` | シンボルが修飾されていない場合に解決に使用するパッケージ名。 |
| `project_only` | boolean | No | `true` | `true` の場合、検索結果を `*project-root*` 配下のファイルに限定する。`false` の場合、ロードされている全ライブラリから検索する。 |

## 3. 動作仕様

### 3.1 シンボル解決
入力された `symbol` 文字列を、`package` コンテキスト下で Lisp シンボルとして解決します（`code-find` と同様のロジック）。シンボルが存在しない、または解決できない場合はエラーを返します。

### 3.2 参照の収集 (SBCL依存)
`sb-introspect` の XREF 機能を利用し、以下の種別の参照を統合して収集します。

* **Calls**: 関数呼び出し (`who-calls`)
* **Macroexpands**: マクロ展開 (`who-macroexpands`)
* **Binds**: 変数束縛 (`who-binds` / `let` や引数など)
* **References**: グローバル変数などの参照 (`who-references`)
* **Sets**: 代入 (`who-sets` / `setf` 等)

### 3.3 フィルタリングと正規化
収集された参照リスト（`definition-source` オブジェクト）に対し、以下の処理を行います。

1.  **パス解決**: ソースファイルパス (`pathname`) と行番号 (`line`) を取得します。
2.  **プロジェクトフィルタ**: `project_only` が `true` の場合、パスが `cl-mcp/src/fs:*project-root*` の配下にあるものだけを残します。これにより、依存ライブラリ（Alexandria等）内部での呼び出しを除外し、ユーザーが編集すべきコードに集中させます。
3.  **重複排除**: 同じ行での複数回の参照は1つにまとめます。

## 4. 出力フォーマット (Return Value)

ヒットした箇所のリストを返します。参照が見つからない場合は空リストを返します。

```json
{
  "refs": [
    {
      "path": "src/run.lisp",    // プロジェクトルートからの相対パス
      "line": 42,                // 1始まりの行番号
      "type": "call",            // 参照タイプ (call, macro, variable 等)
      "context": "(run :transport :tcp ...)" // 該当行のテキスト（スニペット）
    },
    {
      "path": "tests/integration-test.lisp",
      "line": 105,
      "type": "call",
      "context": "(cl-mcp:run ...)"
    }
  ],
  "count": 2,
  "symbol": "cl-mcp:run"
}
````

## 5\. 実装イメージ (Common Lisp)

`src/code.lisp` に以下の関数を追加するイメージです。

```lisp
(defun code-find-references (symbol-name &key package (project-only t))
  "Return a list of references for SYMBOL-NAME."
  (let* ((sym (%parse-symbol symbol-name :package package))
         (refs '()))
    #+sbcl
    (let ((pkg (%ensure-sb-introspect)))
      ;; 複数のXREF関数を呼び出して統合
      (dolist (finder '("WHO-CALLS" "WHO-MACROEXPANDS" "WHO-BINDS" "WHO-REFERENCES" "WHO-SETS"))
        (let ((fn (find-symbol finder pkg)))
          (when fn
            (let ((sources (funcall fn sym)))
              (dolist (src sources)
                (push (cons finder src) refs)))))))

    ;; パス解決とフィルタリング
    (let ((results '())
          (seen (make-hash-table :test #'equal)))
      (dolist (item refs)
        (destructuring-bind (type . source) item
          (let* ((path-fn (find-symbol "DEFINITION-SOURCE-PATHNAME" (%ensure-sb-introspect)))
                 (pathname (funcall path-fn source)))
            (when pathname
              (let ((normalized-path (%normalize-path pathname))) ;; 既存関数を利用
                ;; プロジェクト内かチェック (normalize-pathは相対パスを返す前提)
                (when (or (not project-only)
                          (uiop:relative-pathname-p (uiop:ensure-pathname normalized-path)))
                  (let* ((line (code-find-line pathname source)) ;; 行番号取得(既存ロジック流用)
                         (key (format nil "~A:~D" normalized-path line)))
                    (unless (gethash key seen)
                      (setf (gethash key seen) t)
                      (push (make-ref-object :path normalized-path
                                             :line line
                                             :type (string-downcase type)
                                             :context (%read-line-content pathname line))
                            results)))))))))
      (nreverse results))))
```

## 6\. 既存コードへの影響・依存

  * **依存ファイル**: `src/code.lisp`
      * `%parse-symbol`, `%ensure-sb-introspect`, `%normalize-path` などの既存ヘルパー関数を再利用します。
  * **新規ヘルパー**:
      * `%read-line-content (pathname line)`: 指定行のテキストを読み取ってスニペットとして返す機能が `src/fs.lisp` の `fs-read-file` ロジックを再利用する形で必要になります。
  * **プロトコル定義**:
      * `src/protocol.lisp` に `tools-descriptor-code-references` とハンドラの追加が必要です。

## 7\. 制約事項

  * **コンパイル済みコードのみ**: `sb-introspect` はコンパイル済みのコード（メモリ上のイメージ）から情報を取得します。編集直後で未コンパイルのファイルや、ロードされていないファイルの参照は見つけられません。
      * *対策*: Agent は `fs-write-file` 後に `project.load` や `project.build`、あるいは `repl-eval` で定義を更新してから検索することを推奨ワークフローとします。
