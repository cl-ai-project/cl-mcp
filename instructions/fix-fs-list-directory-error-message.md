# 修正課題: fs-list-directory のエラーメッセージ改善

## 優先度
🔴 **High** - デバッグを著しく困難にする

## 課題の詳細

### 現在の問題
`fs-list-directory` が存在するディレクトリに対しても「does not exist or is not readable」と返すことがあり、実際にどのパスを解決したのかが分からないため、デバッグが困難。

### ユーザー体験の問題

**シナリオ例**:
```
User: "src ディレクトリを一覧表示して"
AI: fs-list-directory("src")
Error: "Directory src does not exist or is not readable"
```

この時、AIエージェントは以下の情報が不足している：
- `"src"` が実際にどこに解決されたのか？
- 作業ディレクトリは何か？
- プロジェクトルートは何か？

**実際の解決パス**: `/home/user/projects/myapp/src`

### 根本原因

`src/fs.lisp:163-167` のエラーメッセージに解決後のパスを含めていない：

```lisp
(let* ((pn (%allowed-read-path-p path)))
  (unless pn
    (error "Read not permitted for path ~A" path))
  (unless (uiop:directory-exists-p pn)
    (error "Directory ~A does not exist or is not readable" path))  ; ❌ path のみ
```

`path` は相対パス（例: `"src"`）だが、実際には `pn` (解決後の絶対パス) を使ってチェックしている。

## 修正方針

### 基本方針
エラーメッセージに以下の情報を含める：
1. ユーザーが指定した元のパス
2. 解決後の絶対パス
3. プロジェクトルートとの関係

### 修正方法

#### Option A: エラーメッセージに解決パスを追加（推奨）

```lisp
;; Before:
(error "Directory ~A does not exist or is not readable" path)

;; After:
(error "Directory ~A (resolved to ~A) does not exist or is not readable"
       path (namestring pn))
```

**利点**: 最小限の変更、即座に実装可能

#### Option B: より詳細な情報を提供

```lisp
(error "Directory ~A does not exist or is not readable.~%  Resolved path: ~A~%  Project root: ~A"
       path
       (namestring pn)
       (namestring *project-root*))
```

**利点**: プロジェクトルートも明示、相対パスの基準が明確

#### Option C: 構造化エラー情報

```lisp
;; エラーオブジェクトとして詳細情報を含める
(error 'directory-not-found
       :input-path path
       :resolved-path (namestring pn)
       :project-root (namestring *project-root*))
```

**利点**: プログラマティックに情報を取得可能
**欠点**: 実装コストが高い、他のエラーとの一貫性

### 推奨実装
**Option A** を採用。シンプルで効果的。

## 実装

### 修正対象ファイル
`src/fs.lisp`

### 修正箇所 1: fs-list-directory (line 163-167)

**修正前**:
```lisp
(defun fs-list-directory (path)
  "List directory entries at PATH respecting read allow-list.
Returns a vector of hash-tables with keys \"name\" and \"type\" (file|directory)."
  (let* ((pn (%allowed-read-path-p path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    (unless (uiop:directory-exists-p pn)
      (error "Directory ~A does not exist or is not readable" path))
```

**修正後**:
```lisp
(defun fs-list-directory (path)
  "List directory entries at PATH respecting read allow-list.
Returns a vector of hash-tables with keys \"name\" and \"type\" (file|directory)."
  (let* ((pn (%allowed-read-path-p path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    (unless (uiop:directory-exists-p pn)
      (error "Directory ~A (resolved to ~A) does not exist or is not readable"
             path (namestring pn)))
```

### 修正箇所 2: fs-read-file のエラーメッセージも統一 (line 119-120)

**現在のコード**:
```lisp
(unless pn
  (error "Read not permitted for path ~A" path))
```

**修正提案** (オプション):
```lisp
(unless pn
  (error "Read not permitted for path ~A (outside project root ~A)"
         path (namestring *project-root*)))
```

## 検証方法

### テストケース 1: 存在しないディレクトリ

```lisp
;; 修正前の動作
(fs-list-directory "nonexistent")
;; Error: Directory nonexistent does not exist or is not readable

;; 修正後の動作
(fs-list-directory "nonexistent")
;; Error: Directory nonexistent (resolved to /home/user/project/nonexistent) does not exist or is not readable
```

### テストケース 2: 存在するが権限がないディレクトリ

```bash
# 準備
mkdir test-no-perm
chmod 000 test-no-perm
```

```lisp
(fs-list-directory "test-no-perm")
;; Error: Directory test-no-perm (resolved to /home/user/project/test-no-perm) does not exist or is not readable
```

### テストケース 3: プロジェクト外のパス

```lisp
(fs-list-directory "/etc")
;; Error: Read not permitted for path /etc
```

## 期待される効果

### 修正前
- ❌ エラーメッセージが曖昧
- ❌ 相対パスの解決先が不明
- ❌ デバッグに追加の調査が必要
- ❌ AIエージェントが迷子になる

### 修正後
- ✅ エラーメッセージに解決後のパスを含む
- ✅ 問題のパスを即座に特定可能
- ✅ デバッグ時間が短縮
- ✅ AIエージェントが状況を正確に把握
- ✅ ユーザーへのフィードバックが改善

## 追加の改善提案

この修正に加えて、「プロジェクト情報を取得するツール」(fix-add-fs-get-project-info.md) を実装することで、さらにデバッグが容易になります。

## 関連課題

- **fix-add-fs-get-project-info.md**: プロジェクト情報取得ツールの追加

## 実装時間見積もり
**10分** (2箇所の修正 + テスト)
