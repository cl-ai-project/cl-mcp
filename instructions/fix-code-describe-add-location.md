# 改善課題: code-describe-symbol に定義位置情報を追加

## 優先度
🟡 **Medium** - 便利だが code-find-definition で代替可能

## 課題の詳細

### 現在の問題

`code-describe-symbol` は関数シグネチャの確認には便利だが、定義ファイルのパスと行番号を返さないため、定義場所へのナビゲーションに別のツール呼び出しが必要。

### 現在の出力

```lisp
(code-describe-symbol "cl-mcp:run")
;; =>
;; name: "RUN"
;; type: "function"
;; arglist: "(&KEY (TRANSPORT :STDIO))"
;; documentation: "Start the MCP server..."
```

### 期待される出力

```lisp
(code-describe-symbol "cl-mcp:run")
;; =>
;; name: "RUN"
;; type: "function"
;; arglist: "(&KEY (TRANSPORT :STDIO))"
;; documentation: "Start the MCP server..."
;; path: "src/run.lisp"
;; line: 42
```

### ユースケース

#### Before (現状)
```
User: run 関数の使い方を教えて
AI: code-describe-symbol("cl-mcp:run")
AI: 関数シグネチャは (&key (transport :stdio)) です
User: 定義を見たい
AI: code-find-definition("cl-mcp:run")
AI: src/run.lisp の 42 行目に定義されています
```

#### After (改善後)
```
User: run 関数の使い方を教えて
AI: code-describe-symbol("cl-mcp:run")
AI: 関数シグネチャは (&key (transport :stdio)) で、
    src/run.lisp:42 に定義されています
```

## 修正方針

### 基本方針

`code-describe-symbol` の返り値を拡張し、パスと行番号を追加する。

既存の `code-find-definition` のロジックを再利用する。

### 返り値の形式

#### Option A: 複数値を追加（推奨）

```lisp
;; Before: 4つの値
(values name type arglist doc)

;; After: 6つの値
(values name type arglist doc path line)
```

**利点**: Common Lisp の慣例に従う
**欠点**: protocol.lisp の変更が必要

#### Option B: hash-table で返す

```lisp
;; 返り値を hash-table に変更
{
  "name": "RUN",
  "type": "function",
  "arglist": "(&KEY ...)",
  "documentation": "...",
  "path": "src/run.lisp",
  "line": 42
}
```

**利点**: 拡張が容易
**欠点**: 後方互換性を破壊

#### Option C: オプショナルパラメータ

```lisp
(code-describe-symbol symbol-name &key package include-location)
```

**利点**: 後方互換性を保持
**欠点**: デフォルトで位置情報が含まれない

### 推奨実装

**Option A** を採用。protocol.lisp のハンドラーで hash-table に変換。

## 実装

### 修正対象ファイル

1. `src/code.lisp`: `code-describe-symbol` の実装
2. `src/protocol.lisp`: ハンドラーの更新

### src/code.lisp の修正

#### 修正前 (line 131-169)

```lisp
(defun code-describe-symbol (symbol-name &key package)
  "Return NAME, TYPE, ARGLIST string and DOCUMENTATION for SYMBOL-NAME.
Signals an error when the symbol is unbound."
  (let* ((sym (%parse-symbol symbol-name :package package))
         (name (princ-to-string sym))
         (type ...))
    ...
    (values name type arglist doc)))
```

#### 修正後

```lisp
(declaim (ftype (function (string &key (:package (or null package symbol string)))
                          (values string string
                                  (or null string)
                                  (or null string)
                                  (or null string)  ; path
                                  (or null fixnum)  ; line
                                  &optional))
                code-describe-symbol))

(defun code-describe-symbol (symbol-name &key package)
  "Return NAME, TYPE, ARGLIST, DOCUMENTATION, PATH, and LINE for SYMBOL-NAME.
Signals an error when the symbol is unbound.
PATH and LINE may be NIL when the definition location cannot be determined."
  (let* ((sym (%parse-symbol symbol-name :package package))
         (name (princ-to-string sym))
         (type (cond
                 ((macro-function sym) "macro")
                 ((fboundp sym) "function")
                 ((boundp sym) "variable")
                 (t "unbound"))))
    (when (string= type "unbound")
      (error "Symbol ~A is not bound as a function or variable" sym))

    #+sbcl
    (%ensure-sb-introspect)

    (let* ((fn (cond
                 ((macro-function sym))
                 ((fboundp sym) (symbol-function sym))
                 (t nil)))
           (arglist (when fn
                      (handler-case
                          (let* ((fn-ll (%sb-introspect-symbol "FUNCTION-LAMBDA-LIST"))
                                 (args (and fn-ll (funcall fn-ll fn))))
                            (cond
                              ((null args) "()")
                              ((listp args) (princ-to-string args))
                              (t (princ-to-string args))))
                        (error (e)
                          (log-event :warn "code.describe.arglist-error" "symbol" symbol-name
                                     "error" (princ-to-string e))
                          "()"))))
           (doc (cond
                  ((or (macro-function sym) (fboundp sym))
                   (documentation sym 'function))
                  ((boundp sym) (documentation sym 'variable))
                  (t nil)))
           ;; 定義位置の取得 (code-find-definition のロジックを再利用)
           (path nil)
           (line nil))

      ;; SBCL で定義位置を取得
      #+sbcl
      (let* ((pkg (%ensure-sb-introspect))
             (find-by-name (and pkg (find-symbol "FIND-DEFINITION-SOURCES-BY-NAME" pkg)))
             (find (and pkg (find-symbol "FIND-DEFINITION-SOURCE" pkg)))
             (path-fn (and pkg (find-symbol "DEFINITION-SOURCE-PATHNAME" pkg)))
             (offset-fn (and pkg (find-symbol "DEFINITION-SOURCE-CHARACTER-OFFSET" pkg)))
             (source (or (and find-by-name
                              (first (ignore-errors
                                      (funcall find-by-name sym
                                               (cond ((macro-function sym) :macro)
                                                     ((fboundp sym) :function)
                                                     ((boundp sym) :variable)
                                                     (t :function))))))
                         (and find (ignore-errors (funcall find sym))))))
        (when (and source path-fn)
          (let* ((pathname (funcall path-fn source))
                 (char-offset (and offset-fn (funcall offset-fn source))))
            (setf line (%offset->line pathname char-offset)
                  path (%normalize-path pathname)))))

      (values name type arglist doc path line))))
```

### src/protocol.lisp の修正

#### tools-descriptor-code-describe の更新

```lisp
(defun tools-descriptor-code-describe ()
  (%make-ht
   "name" "code-describe"
   "description"
   "Describe a symbol (function, macro, or variable): return name, type, arglist, documentation, definition path, and line number."
   "inputSchema" ...))
```

#### ハンドラーの更新

```lisp
((member local '("code-describe" "code_describe" "code.describe" "describe-symbol")
         :test #'string=)
 (handler-case
     (let* ((symbol (and args (gethash "symbol" args)))
            (package (and args (gethash "package" args))))
       (unless symbol
         (return-from handle-tools-call
           (%error id -32602 "Missing required parameter: symbol")))
       (multiple-value-bind (name type arglist doc path line)
           (code-describe-symbol symbol :package package)
         (let ((result (make-hash-table :test #'equal)))
           (setf (gethash "name" result) name
                 (gethash "type" result) type
                 (gethash "arglist" result) arglist
                 (gethash "documentation" result) doc)
           ;; path と line は nil の可能性があるため条件付きで追加
           (when path
             (setf (gethash "path" result) path))
           (when line
             (setf (gethash "line" result) line))
           (%result id result))))
   (error (e)
     (%error id -32603
             (format nil "Internal error during code-describe: ~A" e)))))
```

## 検証方法

### テストケース 1: 関数の説明（定義位置あり）

```lisp
(code-describe-symbol "cl-mcp:run")
;; =>
;; name: "RUN"
;; type: "function"
;; arglist: "(&KEY (TRANSPORT :STDIO))"
;; doc: "Start the MCP server..."
;; path: "src/run.lisp"
;; line: 42
```

### テストケース 2: 標準ライブラリの関数（定義位置なし）

```lisp
(code-describe-symbol "mapcar")
;; =>
;; name: "MAPCAR"
;; type: "function"
;; arglist: "(FUNCTION LIST &REST MORE-LISTS)"
;; doc: "Apply FUNCTION to..."
;; path: nil
;; line: nil
```

### テストケース 3: マクロ

```lisp
(code-describe-symbol "with-open-file")
;; =>
;; type: "macro"
;; ...
```

### テストケース 4: 変数

```lisp
(code-describe-symbol "cl-mcp:*project-root*")
;; =>
;; type: "variable"
;; arglist: nil
;; path: "src/fs.lisp"
;; line: 28
```

## 期待される効果

### 修正前
- ❌ 定義位置を知るには2回のツール呼び出しが必要
- ❌ AIエージェントのレスポンスが遅い
- ❌ ユーザー体験が分断される

### 修正後
- ✅ 1回のツール呼び出しで完全な情報を取得
- ✅ AIエージェントのレスポンスが高速化
- ✅ シームレスなナビゲーション体験
- ✅ 定義位置が不明な場合も適切にハンドリング

## 後方互換性

### protocol.lisp レベル

hash-table で返すため、新しいキー (`path`, `line`) を追加しても既存のクライアントは影響を受けない。

### Common Lisp API レベル

```lisp
;; 古いコード (4つの値のみ使用)
(multiple-value-bind (name type arglist doc)
    (code-describe-symbol "foo")
  ...)  ; 動作する

;; 新しいコード (6つの値すべて使用)
(multiple-value-bind (name type arglist doc path line)
    (code-describe-symbol "foo")
  ...)  ; 動作する
```

Common Lisp の `multiple-value-bind` は余分な値を無視するため、後方互換性が保たれる。

## 関連課題

- **code-find-definition**: 定義位置取得のロジックを共有

## 実装時間見積もり
**25分** (実装 15分 + テスト 10分)

## オプション機能 (将来的な拡張)

### Phase 2: より詳細な型情報

```lisp
{
  "name": "RUN",
  "type": "function",
  "arglist": "(&KEY (TRANSPORT :STDIO))",
  "ftype": "(FUNCTION (&KEY (:TRANSPORT KEYWORD)) T)",
  "path": "src/run.lisp",
  "line": 42
}
```

### Phase 3: 関連シンボルの提案

```lisp
{
  ...
  "see_also": ["start-tcp-server-thread", "process-json-line"]
}
```
