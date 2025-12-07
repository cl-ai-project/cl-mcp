# 新機能候補: 汎用 Lisp 構文チェックツール

## 優先度
🟢 **Low** - check-parens で大半のケースはカバー可能

## 課題の詳細

### 現在の問題

`check-parens` は括弧・ブラケットのバランスのみをチェックし、Lisp構文全体の検証はしない。成功時は `{"ok": true}` のみで追加情報がない。

### 現在の check-parens の範囲

**検出できるエラー**:
- 括弧の不一致 `(defun foo (x)`
- 余分な閉じ括弧 `(+ 1 2))`
- ブラケットの不一致 `[1 2 3)`

**検出できないエラー**:
- 未定義変数 `(defun foo (x) undefined-var)`
- マクロ展開エラー
- 構文エラー `(defun (x) ...)` (名前が欠落)
- パッケージ未定義 `unknown:symbol`

### ユースケース

#### シナリオ 1: コード提出前の検証

```
User: このコードは正しいか確認して
Code: (defun calculate (x) (+ x undefined-variable))

AI: check-parens → OK (括弧はバランス)
AI: lint-lisp-code → Warning: UNDEFINED-VARIABLE is not defined
```

#### シナリオ 2: ファイル全体の検証

```
AI: lint-lisp-code(path="src/main.lisp")
Result: {
  "ok": true,
  "forms_count": 15,
  "warnings": []
}
```

## 修正方針

### Option A: check-parens の拡張（推奨）

`check-parens` に構文チェック機能を追加し、成功時に追加情報を返す。

```lisp
(check-parens :code "(defun foo (x) (+ x 1))")
;; => {
;;   "ok": true,
;;   "forms_count": 1,
;;   "balanced": true
;; }
```

**利点**: 既存ツールの強化、新ツール不要
**欠点**: ツール名と機能が乖離

### Option B: 新しい lint-lisp-code ツール

専用の構文チェックツールを作成。

```lisp
(lint-lisp-code :code code :package package)
;; => {
;;   "ok": true/false,
;;   "forms_count": 5,
;;   "errors": [...],
;;   "warnings": [...]
;; }
```

**利点**: 責任が明確、将来的な拡張が容易
**欠点**: 実装コストが高い

### Option C: 何もしない（推奨）

現在の `check-parens` で十分。追加の構文チェックは `repl-eval` で代替可能。

```lisp
;; 構文チェックの代替
(repl-eval "(progn (read-from-string code) nil)")
```

**利点**: 実装不要、既存機能で十分
**欠点**: AIエージェントが工夫する必要

## 実装（Option A の場合）

### 修正対象ファイル
`src/validate.lisp`

### 修正内容

#### check-parens の拡張

```lisp
(defun check-parens (&key path code offset limit enhanced)
  "Check balanced parentheses/brackets in CODE or PATH slice.
When ENHANCED is true, also attempt to read forms and return additional information."
  ...
  (let ((result (%scan-parens text :base-offset base-off)))
    (destructuring-bind (&key ok ...) result
      (let ((h (make-hash-table :test #'equal)))
        (setf (gethash "ok" h) (and ok t))

        ;; Enhanced mode: try to read forms
        (when (and enhanced ok)
          (handler-case
              (let ((forms (read-lisp-forms text)))
                (setf (gethash "forms_count" h) (length forms)))
            (error (e)
              (setf (gethash "ok" h) nil
                    (gethash "kind" h) "read-error"
                    (gethash "message" h) (princ-to-string e)))))

        (unless ok
          (setf (gethash "kind" h) kind
                ...))
        h))))

(defun read-lisp-forms (text)
  "Read all top-level forms from TEXT and return as a list."
  (let ((*readtable* (copy-readtable nil))
        (*read-eval* nil))
    (with-input-from-string (in text)
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            collect form))))
```

### protocol.lisp の更新

```lisp
(defun tools-descriptor-check-parens ()
  (%make-ht
   "name" "check-parens"
   "description"
   "Check balanced parentheses/brackets and optionally validate Lisp syntax."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   ...
                   (setf (gethash "enhanced" p)
                         (%make-ht "type" "boolean"
                                   "description"
                                   "When true, also attempt to read and validate Lisp forms"))
                   ...)))
```

## 実装（Option B の場合）

### 新規ファイル: src/lint.lisp

```lisp
;;;; src/lint.lisp

(defpackage #:cl-mcp/src/lint
  (:use #:cl)
  (:export #:lint-lisp-code))

(in-package #:cl-mcp/src/lint)

(defun lint-lisp-code (&key code path package)
  "Validate Lisp syntax and return errors/warnings.
Returns a hash-table with keys:
  - ok: boolean
  - forms_count: number of top-level forms
  - errors: array of error objects
  - warnings: array of warning objects (if available)"
  (let ((h (make-hash-table :test #'equal))
        (text (or code (fs-read-file path)))
        (errors '())
        (warnings '())
        (forms-count 0))

    ;; Try to read forms
    (handler-bind
        ((warning (lambda (w)
                    (push (make-warning-object w) warnings)
                    (muffle-warning)))
         (error (lambda (e)
                  (push (make-error-object e) errors)
                  (return-from lint-lisp-code
                    (progn
                      (setf (gethash "ok" h) nil
                            (gethash "errors" h) (coerce (nreverse errors) 'vector))
                      h)))))
      (let ((*readtable* (copy-readtable nil))
            (*read-eval* nil)
            (*package* (find-package (or package :cl-user))))
        (with-input-from-string (in text)
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (incf forms-count)))))

    (setf (gethash "ok" h) t
          (gethash "forms_count" h) forms-count)
    (when warnings
      (setf (gethash "warnings" h) (coerce (nreverse warnings) 'vector)))
    h))

(defun make-error-object (error)
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "type" h) "error"
          (gethash "message" h) (princ-to-string error))
    h))

(defun make-warning-object (warning)
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "type" h) "warning"
          (gethash "message" h) (princ-to-string warning))
    h))
```

## 検証方法

### テストケース 1: 正常なコード

```lisp
(check-parens :code "(defun foo (x) (+ x 1))" :enhanced t)
;; => {"ok": true, "forms_count": 1}
```

### テストケース 2: 読み込みエラー

```lisp
(check-parens :code "(defun (x) (+ x 1))" :enhanced t)
;; => {"ok": false, "kind": "read-error", "message": "..."}
```

### テストケース 3: 括弧エラー（従来通り）

```lisp
(check-parens :code "(defun foo (x)")
;; => {"ok": false, "kind": "unclosed", ...}
```

## 期待される効果

### Option A（check-parens拡張）の場合
- ✅ 既存ツールの強化
- ✅ 成功時に有用な情報を返す
- ✅ 実装コストが低い
- ⚠️ ツール名と機能の乖離

### Option B（新ツール）の場合
- ✅ 責任の明確な分離
- ✅ 将来的な拡張が容易
- ✅ エラー/警告の詳細な報告
- ❌ 実装コストが高い

### Option C（何もしない）の場合
- ✅ 実装不要
- ✅ repl-eval で代替可能
- ⚠️ AIエージェントが工夫する必要

## 推奨事項

**Option C を推奨**: 以下の理由から新ツールは不要

1. **check-parens で大半のケースをカバー**
   - 括弧バランスが最も一般的なエラー

2. **repl-eval で構文チェック可能**
   ```lisp
   (repl-eval "(progn (read-from-string code) nil)")
   ```

3. **実装コストが高い割に効果が限定的**
   - コンパイラ警告の捕捉は困難（fix-repl-eval-error-output.md参照）
   - 完全な静的解析は Common Lisp の動的性質上困難

4. **代替手段が豊富**
   - AIエージェントが repl-eval を活用すれば十分
   - edit-lisp-form は自動で構文検証を行う

## 関連課題

- **fix-check-parens-false-bug.md**: 先にこのバグを修正
- **fix-repl-eval-error-output.md**: コンパイラ警告の捕捉

## 実装時間見積もり

- **Option A**: 30分
- **Option B**: 60分
- **Option C**: 0分（推奨）

## 結論

現時点では新ツールの実装は保留し、以下の対応で十分：

1. check-parens の :false バグを修正
2. AIエージェントに repl-eval の活用を促す
3. 必要に応じて将来的に再検討
