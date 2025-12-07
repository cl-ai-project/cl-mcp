# 新機能: fs-get-project-info ツールの追加

## 優先度
🔴 **High** - 実装が簡単で効果大、デバッグを大幅に改善

## 課題の詳細

### 現在の問題

AIエージェントが以下の情報にアクセスできない：
- プロジェクトルート (`*project-root*`) の値
- 現在の作業ディレクトリ (cwd)
- 相対パスがどの基準で解決されるか

これにより以下の問題が発生：
1. パス指定ミスの原因特定が困難
2. 相対パスと絶対パスの使い分けが不明確
3. エラー発生時のデバッグが非効率

### ユースケース

#### シナリオ 1: パス指定の確認
```
User: "src/main.lisp を読んで"
AI: まず現在位置を確認します
AI: fs-get-project-info()
Result: {
  "project_root": "/home/user/projects/myapp",
  "cwd": "/home/user/projects/myapp"
}
AI: 了解。相対パス "src/main.lisp" は /home/user/projects/myapp/src/main.lisp として解決されます
```

#### シナリオ 2: fs-list-directory エラーのデバッグ
```
AI: fs-list-directory("src") → Error
AI: fs-get-project-info() で状況確認
Result: {
  "project_root": "/home/user/projects/myapp",
  "cwd": "/tmp"
}
AI: 問題発見！cwdがプロジェクトルートと異なります。絶対パスで再試行します。
```

## 修正方針

### 新規ツールの追加

**ツール名**: `fs-get-project-info` または `fs.project-info`

**機能**: プロジェクトルート、作業ディレクトリ、および関連情報を返す

**入力**: なし (パラメータ不要)

**出力**: 以下のキーを持つ hash-table
- `"project_root"`: プロジェクトルートの絶対パス
- `"cwd"`: 現在の作業ディレクトリの絶対パス
- `"project_root_source"`: プロジェクトルートの決定方法 ("env", "cwd", "asdf")
- `"relative_cwd"`: cwdのproject_rootからの相対パス (optional)

### 設計の考慮事項

#### 情報の粒度
基本情報のみで十分か、詳細情報も含めるか？

**Minimal版**:
```json
{
  "project_root": "/home/user/projects/myapp",
  "cwd": "/home/user/projects/myapp"
}
```

**Extended版** (推奨):
```json
{
  "project_root": "/home/user/projects/myapp",
  "cwd": "/home/user/projects/myapp",
  "project_root_source": "env",
  "env_mcp_project_root": "/home/user/projects/myapp",
  "relative_cwd": "."
}
```

#### ツール名の選択
- `fs-get-project-info` ✅ 推奨 (他のfs-*ツールと統一)
- `project-info` ⚪ シンプルだが名前空間が不明確
- `fs-pwd` ❌ UNIX風だが情報が限定的

## 実装

### 修正対象ファイル

1. `src/fs.lisp`: 関数実装
2. `src/protocol.lisp`: ツール記述子とハンドラー
3. `main.lisp`: エクスポート

### 実装コード

#### src/fs.lisp に追加

```lisp
(defun fs-get-project-info ()
  "Return project root and working directory information.
Returns a hash-table with keys:
  - project_root: absolute path to project root
  - cwd: current working directory
  - project_root_source: how project root was determined (env|cwd|asdf)
  - relative_cwd: cwd relative to project_root (when inside project)"
  (let ((h (make-hash-table :test #'equal))
        (root-source (cond
                       ((uiop:getenv "MCP_PROJECT_ROOT") "env")
                       ((ignore-errors (uiop:getcwd)) "cwd")
                       (t "asdf"))))
    (setf (gethash "project_root" h) (namestring *project-root*)
          (gethash "cwd" h) (namestring (uiop:getcwd))
          (gethash "project_root_source" h) root-source)
    ;; relative_cwd の計算
    (let* ((cwd (uiop:getcwd))
           (root (uiop:ensure-directory-pathname *project-root*)))
      (when (uiop:subpathp cwd root)
        (setf (gethash "relative_cwd" h)
              (uiop:native-namestring (uiop:enough-pathname cwd root)))))
    h))
```

#### src/protocol.lisp に追加

**ツール記述子**:

```lisp
(defun tools-descriptor-fs-project-info ()
  (%make-ht
   "name" "fs-get-project-info"
   "description"
   "Get project root and current working directory information for path resolution context."
   "inputSchema" (%make-ht "type" "object"
                            "properties" (%make-ht)
                            "required" (vector))))
```

**ハンドラー** (handle-tools-call に追加):

```lisp
((member local '("fs-get-project-info" "fs_get_project_info"
                 "fs.project-info" "project-info")
         :test #'string=)
 (handler-case
     (let ((result (fs-get-project-info)))
       (%result id result))
   (error (e)
     (%error id -32603
             (format nil "Internal error during fs-get-project-info: ~A" e)))))
```

**tools-list に追加**:

```lisp
(defmethod handle-tools-list (...)
  ...
  (vector (tools-descriptor-fs-read)
          (tools-descriptor-fs-write)
          (tools-descriptor-fs-list)
          (tools-descriptor-fs-project-info)  ; 追加
          ...))
```

#### main.lisp に追加

```lisp
(:export #:run
         ...
         #:fs-read-file
         #:fs-write-file
         #:fs-list-directory
         #:fs-get-project-info  ; 追加
         ...)
```

### エラーハンドリング

`getcwd` が失敗する可能性があるため、適切なフォールバックを用意：

```lisp
(setf (gethash "cwd" h)
      (or (ignore-errors (namestring (uiop:getcwd)))
          "(unavailable)"))
```

## 検証方法

### 手動テスト

```lisp
;; REPL での確認
(fs-get-project-info)
;; => #<HASH-TABLE {
;;      "project_root": "/home/user/projects/myapp",
;;      "cwd": "/home/user/projects/myapp",
;;      "project_root_source": "env",
;;      "relative_cwd": "."
;;    }>
```

### MCP プロトコル経由

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"fs-get-project-info","arguments":{}}}' | ros run -l cl-mcp -- stdio
```

期待される出力:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "project_root": "/home/user/projects/myapp",
    "cwd": "/home/user/projects/myapp",
    "project_root_source": "env",
    "relative_cwd": "."
  }
}
```

### 異なる状況でのテスト

```bash
# ケース1: MCP_PROJECT_ROOT が設定されている
export MCP_PROJECT_ROOT=/path/to/project
ros run -l cl-mcp

# ケース2: 環境変数なし、cwd をプロジェクトルートとして使用
unset MCP_PROJECT_ROOT
cd /path/to/project
ros run -l cl-mcp

# ケース3: プロジェクト外のディレクトリから実行
cd /tmp
ros run -l cl-mcp
```

## 期待される効果

### AIエージェントへのメリット
- ✅ パス解決の基準を明確に把握
- ✅ 相対パス vs 絶対パスの使い分けを判断可能
- ✅ エラー時のデバッグが高速化
- ✅ ユーザーへの状況説明が正確に

### 開発者へのメリット
- ✅ トラブルシューティングが容易
- ✅ 環境依存の問題を早期発見
- ✅ プロジェクトルート設定の検証が簡単

### 実際の使用例

#### Before (現状)
```
AI: fs-list-directory("src") を実行しましたが、エラーが発生しました。
User: どのパスを見ようとしたの？
AI: すみません、それは分かりません。
```

#### After (修正後)
```
AI: まず現在の状況を確認します。
AI: fs-get-project-info() の結果:
    - project_root: /home/user/myapp
    - cwd: /home/user/myapp
AI: "src" は /home/user/myapp/src として解決されます。
AI: fs-list-directory("src") を実行します...
```

## 関連課題

- **fix-fs-list-directory-error-message.md**: エラーメッセージ改善と連携
- **lisp-read-file-eclector.md**: 行番号情報との統合

## 実装時間見積もり
**20分** (実装 15分 + テスト 5分)

## オプション機能 (将来的な拡張)

### Phase 2: 環境情報の追加
```json
{
  "project_root": "...",
  "cwd": "...",
  "lisp_implementation": "SBCL 2.3.9",
  "os": "Linux",
  "asdf_central_registry": ["/path1", "/path2"]
}
```

### Phase 3: ファイルシステム統計
```json
{
  "project_root": "...",
  "stats": {
    "total_files": 157,
    "lisp_files": 42,
    "total_size_bytes": 1048576
  }
}
```
