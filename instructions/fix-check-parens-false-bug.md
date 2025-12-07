# 修正課題: check-parens の :false バグ

## 優先度
🔴 **Critical** - 機能が動作しない

## 課題の詳細

### 現在の問題
`check-parens` ツールで `path` パラメータを使用すると、MCPクライアントから "Unexpected response type" エラーが返され、結果を取得できない。

### 根本原因
`src/validate.lisp:105` で、エラー時の `"ok"` キーに Common Lisp のキーワードシンボル `:false` を設定している。

```lisp
;; src/validate.lisp:105 (現在のコード)
(setf (gethash "ok" h) :false  ; ❌ キーワードシンボル
```

yason の JSON エンコーダーは `:false` キーワードを正しく boolean の `false` に変換せず、文字列 `"FALSE"` やその他の予期しない形式にシリアライズする可能性がある。

一方、line 117 では正しく boolean 変換している：

```lisp
;; src/validate.lisp:117 (正しい実装)
(setf (gethash "ok" h) (and ok t))  ; ✅ nil または t
```

## 修正方針

### 修正内容
line 105 の `:false` を `nil` に変更する。

```lisp
;; Before:
(setf (gethash "ok" h) :false

;; After:
(setf (gethash "ok" h) nil
```

### yason の boolean 変換ルール
yason は以下のように Common Lisp の値を JSON にマッピングする：
- `nil` → `false`
- `t` (または任意の non-nil) → `true`
- `:false`, `:true` などのキーワード → 文字列化される可能性

### 影響範囲
- `src/validate.lisp` の `check-parens` 関数のみ
- line 105 の1箇所のみ修正

## 実装

### 修正対象ファイル
`src/validate.lisp`

### 修正前 (line 103-107)
```lisp
(when (> (length text) *check-parens-max-bytes*)
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "ok" h) :false
          (gethash "kind" h) "too-large"
          (gethash "expected" h) nil
          (gethash "found" h) nil)
```

### 修正後 (line 103-107)
```lisp
(when (> (length text) *check-parens-max-bytes*)
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "ok" h) nil
          (gethash "kind" h) "too-large"
          (gethash "expected" h) nil
          (gethash "found" h) nil)
```

## 検証方法

### 1. 手動テスト
```lisp
;; REPL で確認
(check-parens :code "(defun foo (x) (+ x 1))")
;; => #<HASH-TABLE {ok: T}>

(check-parens :code "(defun foo (x")
;; => #<HASH-TABLE {ok: NIL, kind: "unclosed", ...}>
```

### 2. JSON シリアライゼーション確認
```lisp
;; yason での変換確認
(yason:encode (check-parens :code "(defun foo (x") *standard-output*)
;; => {"ok":false,"kind":"unclosed",...}
```

### 3. MCP プロトコル経由でのテスト
```bash
# stdio 経由で check-parens を呼び出し
echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"check-parens","arguments":{"code":"(defun foo"}}}' | ros run -l cl-mcp -- stdio
```

期待される出力:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "ok": false,
    "kind": "unclosed",
    "expected": ")",
    "found": null,
    "position": {"offset": 10, "line": 1, "column": 11}
  }
}
```

## 期待される効果

### 修正前
- ❌ MCP クライアントが "Unexpected response type" エラーを返す
- ❌ check-parens ツールが実質的に使用不可能
- ❌ AIエージェントが括弧バランスを検証できない

### 修正後
- ✅ 正しい JSON boolean として `false` が返される
- ✅ check-parens ツールが正常に動作
- ✅ AIエージェントが構文エラーを検出可能
- ✅ ファイルサイズ超過エラーも正しく報告される

## 関連課題

- なし (独立した単純なバグ修正)

## 実装時間見積もり
**5分** (1行の修正 + 動作確認)
