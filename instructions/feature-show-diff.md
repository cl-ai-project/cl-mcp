# 機能検討: 差分表示ツール (show-diff)

## 優先度
🟢 **Low** - 対象ツールが存在しない、代替手段あり

## 課題の詳細

### ユーザーからのフィードバック

> apply_patch は編集に便利だが、確認用にファイル差分を簡易表示する show-diff（直近パッチ結果を確認するだけの乾式ビュー）があると安全に編集できる。

### 問題の確認

**重要**: cl-mcp には `apply_patch` というツールは存在しません。

**現在のファイル編集ツール**:
- `fs-write-file`: ファイル全体を上書き
- `edit-lisp-form`: 構造的にLispフォームを編集（直接ファイルを書き換え）

どちらも:
- ❌ パッチファイルを生成しない
- ❌ 編集履歴を保存しない
- ❌ undo/redo 機能なし

### ユーザーの意図を推測

以下のいずれかの可能性：

#### 可能性1: edit-lisp-form のプレビュー機能

```
User: このdefunを書き換えたいけど、まず変更内容を確認したい
AI: edit-lisp-form(...) → 実際に書き換わる（プレビューなし）
```

**課題**: 編集前に結果を確認できない

#### 可能性2: 編集後の差分確認

```
User: さっき編集したファイルの変更内容を確認したい
AI: （現状では git diff に頼るしかない）
```

**課題**: 編集履歴がない

#### 可能性3: 他のMCPサーバーとの混同

他のMCPサーバー（例: Anthropic公式の filesystem MCP）には `apply_patch` が存在する可能性。

## 修正方針

### Option A: edit-lisp-form にドライランモード追加（推奨）

編集を実行せず、結果のプレビューを返す。

```lisp
(edit-lisp-form :file-path "src/main.lisp"
                :form-type "defun"
                :form-name "foo"
                :operation "replace"
                :content "(defun foo (x) (* x 2))"
                :dry-run t)  ; 新パラメータ
;; => {
;;   "would_change": true,
;;   "preview": "(defun foo (x)\n  (* x 2))",
;;   "original": "(defun foo (x)\n  (+ x 1))",
;;   "diff": "- (+ x 1)\n+ (* x 2)"
;; }
```

**利点**: 編集前に確認可能
**欠点**: diff 生成のロジックが必要

### Option B: 編集履歴の保存

ファイル編集時に自動的にバックアップを作成。

```lisp
;; 編集時に自動保存
(edit-lisp-form ...) → /tmp/cl-mcp-backup/main.lisp.20231207-143022

;; 履歴確認
(fs-edit-history "src/main.lisp")
;; => [
;;   {"timestamp": "2023-12-07T14:30:22", "backup": "/tmp/..."},
;;   ...
;; ]

;; 差分表示
(show-diff :file "src/main.lisp" :backup-index 0)
```

**利点**: 編集後も確認可能、undo機能の基盤
**欠点**: 実装が複雑、ストレージ管理が必要

### Option C: git diff の活用（推奨）

すでにgitリポジトリがある場合、git diffを使う。

```lisp
;; Git経由で差分表示
(repl-eval "(uiop:run-program '(\"git\" \"diff\" \"src/main.lisp\") :output :string)")
```

**利点**: 実装不要、標準的な方法
**欠点**: git リポジトリが必要

### Option D: 新しい show-diff ツール

unified diff フォーマットで2つのファイルを比較。

```lisp
(show-diff :file1 "src/main.lisp" :file2 "src/main.lisp.bak")
;; => unified diff 文字列
```

**利点**: 汎用的
**欠点**: 実装コストが高い、ユースケースが不明確

## 実装（Option A の場合）

### 修正対象ファイル
`src/edit-lisp-form.lisp`

### 修正内容

```lisp
(defun edit-lisp-form (&key file-path form-type form-name operation content dry-run)
  "Structured edit of a top-level Lisp form.
When DRY-RUN is true, return preview without modifying the file."
  ...
  (let ((updated (%apply-operation original target op-key content)))
    (if dry-run
        ;; Dry-run mode: return preview
        (let ((h (make-hash-table :test #'equal)))
          (setf (gethash "would_change" h) t
                (gethash "original" h) (subseq original start end)
                (gethash "preview" h) updated
                (gethash "file_path" h) (namestring abs))
          h)
        ;; Normal mode: write file
        (progn
          (log-event :debug "edit-lisp-form" ...)
          (fs-write-file rel updated)
          updated))))
```

### protocol.lisp の更新

```lisp
(defun tools-descriptor-edit-lisp-form ()
  (%make-ht
   ...
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   ...
                   (setf (gethash "dry_run" p)
                         (%make-ht "type" "boolean"
                                   "description"
                                   "When true, preview changes without modifying the file"))
                   ...)))
```

## 実装（Option D の場合）

### 新規ファイル: src/diff.lisp

```lisp
;;;; src/diff.lisp

(defpackage #:cl-mcp/src/diff
  (:use #:cl)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file)
  (:export #:show-diff))

(in-package #:cl-mcp/src/diff)

(defun show-diff (file1 file2 &key context-lines)
  "Generate unified diff between FILE1 and FILE2.
Returns a string in unified diff format."
  (let ((content1 (fs-read-file file1))
        (content2 (fs-read-file file2))
        (ctx (or context-lines 3)))
    ;; Simplified diff implementation or use external tool
    (uiop:run-program
     (list "diff" "-u"
           (format nil "--context=~D" ctx)
           (namestring file1)
           (namestring file2))
     :output :string
     :ignore-error-status t)))
```

**注意**: 完全な diff アルゴリズムの実装は複雑。外部ツール（diff コマンド）の使用を推奨。

## 検証方法

### Option A: ドライランモード

```lisp
;; プレビュー
(edit-lisp-form :file-path "src/main.lisp"
                :form-type "defun"
                :form-name "foo"
                :operation "replace"
                :content "(defun foo (x) (* x 2))"
                :dry-run t)

;; 実際の編集
(edit-lisp-form :file-path "src/main.lisp"
                :form-type "defun"
                :form-name "foo"
                :operation "replace"
                :content "(defun foo (x) (* x 2))")
```

### Option C: Git Diff

```bash
# 編集前
git add src/main.lisp

# 編集
edit-lisp-form(...)

# 差分確認
git diff src/main.lisp
```

## 推奨事項

**Option A を推奨**: edit-lisp-form にドライランモードを追加

### 理由

1. **ユーザーの実際のニーズに合致**
   - 編集前に確認したいというニーズは妥当

2. **実装コストが低い**
   - 既存の edit-lisp-form を拡張するだけ
   - 20-30分で実装可能

3. **git がない環境でも使える**
   - 純粋なCommon Lisp実装

4. **安全性の向上**
   - 意図しない編集を防ぐ

### Option C（Git Diff）も並行して推奨

- Git リポジトリがある場合は git diff が最適
- AIエージェントに git diff の使用を促す

## 制限事項

### diff アルゴリズムの複雑さ
- 完全な unified diff の実装は複雑
- Myers' diff アルゴリズムの実装が必要
- または外部ツール（diff コマンド）に依存

### 履歴管理の範囲
- 簡易的なプレビューのみ
- 完全な undo/redo は別途設計が必要

## 関連課題

- **edit-lisp-form.md**: 基本的な編集機能
- **lisp-read-file-eclector.md**: ファイル内容の表示

## 実装時間見積もり

- **Option A（ドライラン）**: 30分
- **Option B（履歴保存）**: 120分
- **Option C（git活用）**: 0分（既存機能）
- **Option D（新ツール）**: 60分

## 結論

1. **短期**: Option A（ドライランモード）を実装
   - 編集前の確認機能として有用
   - 実装コストが低い

2. **中期**: Git Diff の活用を推奨
   - ドキュメントやガイドに記載
   - AIエージェントへの指示を改善

3. **長期**: 必要に応じて完全な履歴管理を検討
   - ユーザーからのフィードバック次第

## ユーザーへの確認事項

以下の点をユーザーに確認することを推奨：

1. `apply_patch` とは何を指していたか？
   - 他のMCPサーバーのツール？
   - edit-lisp-form の呼び方？

2. 具体的なユースケースは？
   - 編集前のプレビュー？
   - 編集後の差分確認？

3. Git リポジトリの使用状況は？
   - Git があれば git diff で十分な可能性
