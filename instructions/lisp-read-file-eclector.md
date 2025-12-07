# ツール設計変更: `lisp-read-file` のeclector移行

## 1. 変更の目的

現在の `lisp-read-file` は標準の `read-from-string` を使用しているため、以下の問題があります:

* **コメント情報の喪失**: ソースコード中のコメントが完全に失われる
* **位置情報の不正確さ**: 標準readerはバイトオフセット情報しか返さず、正確な行番号が取得できない
* **空白の破棄**: フォーム間の空行やインデント情報が失われる
* **実装の不統一**: `edit-lisp-form` はeclectorを使用しているが、`lisp-read-file` は標準readerを使用

eclectorのCST (Concrete Syntax Tree) パーサーに移行することで、これらの問題を解決し、AIエージェントがより正確なソースコード理解を得られるようにします。

## 2. 現在の実装の問題点

### 2.1 コメント情報の喪失

```lisp
;; Before: 元のソースコード
(defun calculate-tax (price)
  "Calculate sales tax."
  ;; Tax rate is 10%
  (* price 0.1))

;; After: 現在の collapsed 表示
(defun calculate-tax (price) ... ;; Calculate sales tax.)
```

`;; Tax rate is 10%` というコメントが完全に失われます。

### 2.2 位置情報の不正確さ

`%read-top-level-forms` (line 123-137) はバイトオフセット位置しか取得できないため:
- 正確な行番号を計算するには追加の走査が必要
- マルチバイト文字 (日本語コメント等) で位置がずれる可能性

### 2.3 実装の重複

`edit-lisp-form.lisp` で既にeclector CSTパーサーを実装しているのに、`lisp-read-file.lisp` では標準readerを使用しており、コードの重複と不統一が発生しています。

## 3. 設計変更の方針

### 3.1 共通CSTパーサーの抽出

`edit-lisp-form.lisp` と `lisp-read-file.lisp` で共通のCST解析ロジックを抽出し、新しいモジュール `src/cst.lisp` に配置します。

```lisp
(defpackage #:cl-mcp/src/cst
  (:use #:cl)
  (:import-from #:eclector.parse-result
                #:parse-result-client
                #:make-expression-result
                #:make-skipped-input-result)
  (:export #:cst-node
           #:cst-node-kind
           #:cst-node-value
           #:cst-node-children
           #:cst-node-start
           #:cst-node-end
           #:parse-top-level-forms))
```

### 3.2 CST構造の拡張

現在の `cst-node` 構造体に、行番号情報を追加します:

```lisp
(defstruct cst-node
  kind          ; :expr | :skipped
  value         ; S式 or スキップ理由
  children      ; 子ノードのリスト
  (start 0 :type fixnum)   ; バイトオフセット開始位置
  (end 0 :type fixnum)     ; バイトオフセット終了位置
  (start-line 1 :type fixnum)  ; 開始行番号 (1-indexed)
  (end-line 1 :type fixnum))   ; 終了行番号 (1-indexed)
```

### 3.3 位置情報の計算

テキスト全体を1回走査して、バイトオフセット→行番号の変換テーブルを構築します:

```lisp
(defun %build-line-table (text)
  "Return a vector mapping byte positions to line numbers."
  (let ((table (make-array (1+ (length text)) :element-type 'fixnum :initial-element 1))
        (line 1))
    (loop for i from 0 below (length text)
          for ch = (char text i)
          do (setf (aref table i) line)
             (when (char= ch #\Newline)
               (incf line)))
    (setf (aref table (length text)) line)
    table))
```

## 4. 新しい実装の仕様

### 4.1 コメント保持

eclectorの `:skipped` ノードを保持し、collapsed表示時に以下の情報を含めます:

#### 定義直前のコメント (Docstring的な役割)

```lisp
;; Original source:
;; Helper function for validation
;; Returns T if valid, NIL otherwise
(defun validate-input (x)
  (> x 0))

;; Collapsed with comments:
;; Helper function for validation
;; Returns T if valid, NIL otherwise
(defun validate-input (x) ...)
```

#### 定義内の重要コメント (オプション: 詳細モード)

```lisp
;; Original source:
(defun complex-logic (data)
  ;; Step 1: Normalize input
  (let ((normalized (normalize data)))
    ;; Step 2: Apply transformation
    (transform normalized)))

;; Collapsed (basic):
(defun complex-logic (data) ...)

;; Collapsed (with-comments):
(defun complex-logic (data) ...
  ;; Step 1: Normalize input
  ;; Step 2: Apply transformation
)
```

### 4.2 新しいパラメータ

`lisp-read-file` に以下のパラメータを追加します:

| パラメータ名 | 型 | デフォルト | 説明 |
|:---|:---|:---|:---|
| `include-comments` | boolean | `false` | collapsed表示時にコメントを含めるか |
| `comment-context` | string | `"preceding"` | コメント表示範囲: `"preceding"` (直前のみ), `"all"` (全て), `"none"` (なし) |

### 4.3 出力フォーマットの変更

#### meta情報の拡張

```json
{
  "total_forms": 10,
  "expanded_forms": 2,
  "truncated": false,
  "comment_lines": 15,        // NEW: コメント行数
  "blank_lines": 8,           // NEW: 空行数
  "source_lines": 150         // NEW: 総行数
}
```

#### collapsed表示の強化

**現在**:
```
(defun calculate-tax (price) ... ;; Calculate sales tax.)
(defvar *tax-rate* ...)
```

**変更後 (include-comments=true)**:
```
;; Helper function for tax calculation
;; Uses the global *tax-rate* variable
(defun calculate-tax (price) ... ;; Calculate sales tax.)

;; Default tax rate: 10%
(defvar *tax-rate* ...)
```

### 4.4 行番号付き展開表示

name-pattern または content-pattern にマッチしたフォームを展開する際、行番号を含めます:

**現在**:
```lisp
(defun calculate-tax (price)
  "Calculate sales tax."
  (* price 0.1))
```

**変更後**:
```lisp
  42: (defun calculate-tax (price)
  43:   "Calculate sales tax."
  44:   (* price 0.1))
```

## 5. 実装手順

### Phase 1: 共通CSTモジュールの抽出

1. `src/cst.lisp` を作成
2. `edit-lisp-form.lisp` から以下を移動:
   - `cst-node` 構造体定義
   - `%ensure-cst-methods`
   - eclector.parse-result メソッド定義
3. `cst-node` に `start-line`, `end-line` フィールドを追加
4. `parse-top-level-forms` 関数を実装:
   ```lisp
   (defun parse-top-level-forms (text)
     "Parse TEXT into list of cst-node with line number information."
     ...)
   ```

### Phase 2: lisp-read-file.lisp の書き換え

5. `%read-top-level-forms` を `parse-top-level-forms` の呼び出しに変更
6. `%format-lisp-form` を拡張:
   - CST nodeから直前のコメントノードを抽出
   - `include-comments` パラメータに応じてコメントを含める
7. `%format-lisp-file` を拡張:
   - meta情報に `comment_lines`, `blank_lines`, `source_lines` を追加
8. 行番号付き表示機能を追加

### Phase 3: edit-lisp-form.lisp のリファクタリング

9. `edit-lisp-form.lisp` を `src/cst` モジュールを使用するように変更
10. 重複コードを削除

### Phase 4: テストの追加

11. `tests/cst-test.lisp` を作成
    - `parse-top-level-forms` の基本動作
    - 行番号計算の正確性
    - コメント・空白の保持
12. `tests/lisp-read-file-test.lisp` を拡張
    - `include-comments` パラメータのテスト
    - コメント付きcollapsed表示のテスト
    - 行番号付き展開表示のテスト

## 6. 後方互換性

### 6.1 既存の動作を維持

- `include-comments` のデフォルトは `false` とし、既存の挙動を維持
- meta情報の新しいフィールドは追加のみで、既存フィールドは変更しない
- collapsed表示の基本フォーマットは変更しない

### 6.2 段階的な移行

1. Phase 1-2 で基本的なeclector移行を完了 (後方互換性維持)
2. Phase 3 でコード重複を解消
3. Phase 4 でテストを充実
4. 新機能 (`include-comments`) は段階的にドキュメント化

## 7. 期待される効果

### 7.1 AIエージェントへのメリット

- **より豊かなコンテキスト**: コメント情報により、コードの意図や背景を理解可能
- **正確な位置特定**: 行番号により、エラー箇所や修正対象を正確に特定
- **効率的な理解**: collapsed表示でも重要なコメントを見逃さない

### 7.2 実装の一貫性

- `edit-lisp-form` と `lisp-read-file` が同じCSTインフラを共有
- コードの重複が減り、メンテナンス性が向上
- バグ修正や機能追加が1箇所で済む

### 7.3 将来の拡張性

- CST情報を活用した高度な解析 (静的解析、リファクタリング支援)
- シンタックスハイライト情報の提供
- より洗練されたコード整形・表示機能

## 8. 実装時の注意点

### 8.1 パフォーマンス

- eclectorは標準readerより遅い可能性がある
- 大きなファイル (>10,000行) での性能測定が必要
- 必要に応じてキャッシング機構を検討

### 8.2 エラーハンドリング

- eclectorのパースエラーを適切にハンドリング
- 壊れたLispファイルでも可能な限り情報を返す (partial parse)
- 標準readerへのフォールバック機構は不要 (eclectorは常に動作する)

### 8.3 テストカバレッジ

- 各種Lisp構文のテスト (reader macros, #+ conditional, etc.)
- マルチバイト文字を含むコメントのテスト
- 各種改行コード (LF, CRLF) のテスト

## 9. 実装の優先度

**High Priority** (必須):
- Phase 1: 共通CSTモジュールの抽出
- Phase 2: lisp-read-file.lisp の基本的なeclector移行
- Phase 4 (partial): 基本的な動作のテスト

**Medium Priority** (推奨):
- Phase 2 (extended): コメント表示機能の追加
- Phase 3: edit-lisp-form.lisp のリファクタリング
- Phase 4 (full): 包括的なテストスイート

**Low Priority** (オプション):
- 詳細なコメント表示モード (`comment-context: "all"`)
- パフォーマンス最適化
- 高度な解析機能 (シンタックスハイライト等)
