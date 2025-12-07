# 新機能: asdf-system-info ツールの追加

## 優先度
🟡 **Medium** - 便利だが repl-eval で代替可能

## 課題の詳細

### 現在の問題

大規模プロジェクトでASDFシステムの依存関係やロード順序を確認する際、repl-eval で手動でクエリする必要がある。専用ツールがあると把握が容易になる。

### 現在の方法

```lisp
;; repl-eval で手動クエリ
(repl-eval "(asdf:system-depends-on (asdf:find-system :cl-mcp))")
;; => (:ALEXANDRIA :CL-PPCRE :YASON :USOCKET :BORDEAUX-THREADS :ECLECTOR)

(repl-eval "(asdf:system-source-file (asdf:find-system :cl-mcp))")
;; => #P"/path/to/cl-mcp.asd"
```

**問題点**:
- 複数の repl-eval 呼び出しが必要
- AIエージェントが適切なクエリを知っている必要がある
- 出力形式が一貫していない

### ユースケース

#### シナリオ 1: 依存関係の確認
```
User: このプロジェクトはどんなライブラリに依存してる？
AI: asdf-system-info("cl-mcp")
Result: {
  "name": "cl-mcp",
  "depends_on": ["alexandria", "cl-ppcre", "yason", ...],
  "version": "0.2.0"
}
AI: 以下のライブラリに依存しています: alexandria, cl-ppcre, yason...
```

#### シナリオ 2: システムのロード状態確認
```
User: quicklisp でロードされているシステムは？
AI: asdf-system-info("cl-mcp")
Result: {
  "name": "cl-mcp",
  "loaded": true,
  "source_file": "/path/to/cl-mcp.asd"
}
```

#### シナリオ 3: マルチシステムプロジェクトの把握
```
User: このプロジェクトに含まれるシステムをすべて教えて
AI: asdf-list-systems()
Result: ["cl-mcp", "cl-mcp/tests"]
```

## 修正方針

### 新規ツールの追加

#### ツール1: `asdf-system-info`

**機能**: 指定されたASDFシステムの詳細情報を返す

**入力**:
- `system_name` (required): システム名 (文字列)

**出力**: 以下のキーを持つ hash-table
- `"name"`: システム名
- `"version"`: バージョン文字列 (あれば)
- `"description"`: 説明文 (あれば)
- `"author"`: 著者情報 (あれば)
- `"license"`: ライセンス (あれば)
- `"depends_on"`: 直接依存するシステムのリスト
- `"defsystem_depends_on"`: defsystem依存のリスト
- `"source_file"`: .asdファイルのパス
- `"source_directory"`: ソースディレクトリのパス
- `"loaded"`: ロード済みかどうか (boolean)

#### ツール2: `asdf-list-systems` (オプション)

**機能**: 登録されているシステムの一覧を返す

**入力**: なし

**出力**: システム名の配列

## 実装

### 修正対象ファイル

1. `src/asdf-tools.lisp` (新規作成)
2. `src/protocol.lisp`: ツール記述子とハンドラー
3. `main.lisp`: エクスポート
4. `cl-mcp.asd`: 新ファイルの追加

### src/asdf-tools.lisp (新規作成)

```lisp
;;;; src/asdf-tools.lisp

(defpackage #:cl-mcp/src/asdf-tools
  (:use #:cl)
  (:import-from #:asdf
                #:find-system
                #:system-depends-on
                #:system-defsystem-depends-on
                #:component-name
                #:component-version
                #:system-source-file
                #:system-source-directory
                #:system-description
                #:system-author
                #:system-license
                #:registered-systems
                #:component-loaded-p)
  (:import-from #:uiop
                #:native-namestring)
  (:export #:asdf-system-info
           #:asdf-list-systems))

(in-package #:cl-mcp/src/asdf-tools)

(defun asdf-system-info (system-name)
  "Return detailed information about SYSTEM-NAME.
Returns a hash-table with keys:
  - name: system name
  - version: version string (may be nil)
  - description: description text (may be nil)
  - author: author information (may be nil)
  - license: license string (may be nil)
  - depends_on: list of direct dependencies
  - defsystem_depends_on: list of defsystem dependencies
  - source_file: path to .asd file
  - source_directory: path to source directory
  - loaded: whether the system is currently loaded"
  (unless (stringp system-name)
    (error "system_name must be a string"))

  (handler-case
      (let* ((sys (asdf:find-system system-name))
             (h (make-hash-table :test #'equal)))
        (setf (gethash "name" h) (asdf:component-name sys))

        ;; Version (may be nil)
        (let ((version (asdf:component-version sys)))
          (when version
            (setf (gethash "version" h) version)))

        ;; Description (may be nil)
        (let ((desc (ignore-errors (asdf:system-description sys))))
          (when (and desc (stringp desc))
            (setf (gethash "description" h) desc)))

        ;; Author (may be nil)
        (let ((author (ignore-errors (asdf:system-author sys))))
          (when (and author (stringp author))
            (setf (gethash "author" h) author)))

        ;; License (may be nil)
        (let ((license (ignore-errors (asdf:system-license sys))))
          (when (and license (stringp license))
            (setf (gethash "license" h) license)))

        ;; Dependencies
        (let ((deps (asdf:system-depends-on sys)))
          (setf (gethash "depends_on" h)
                (coerce (mapcar #'string-downcase
                               (mapcar (lambda (d)
                                        (if (consp d) (string (car d)) (string d)))
                                      deps))
                       'vector)))

        ;; Defsystem dependencies
        (let ((defsys-deps (asdf:system-defsystem-depends-on sys)))
          (when defsys-deps
            (setf (gethash "defsystem_depends_on" h)
                  (coerce (mapcar #'string-downcase
                                 (mapcar #'string defsys-deps))
                         'vector))))

        ;; Source file
        (let ((source-file (asdf:system-source-file sys)))
          (when source-file
            (setf (gethash "source_file" h) (uiop:native-namestring source-file))))

        ;; Source directory
        (let ((source-dir (asdf:system-source-directory sys)))
          (when source-dir
            (setf (gethash "source_directory" h) (uiop:native-namestring source-dir))))

        ;; Loaded status
        (setf (gethash "loaded" h) (asdf:component-loaded-p sys))

        h)
    (error (e)
      (error "Failed to find system ~A: ~A" system-name e))))

(defun asdf-list-systems ()
  "Return a vector of all registered ASDF system names."
  (coerce (mapcar #'string-downcase (asdf:registered-systems)) 'vector))
```

### src/protocol.lisp への追加

#### ツール記述子

```lisp
(defun tools-descriptor-asdf-system-info ()
  (%make-ht
   "name" "asdf-system-info"
   "description"
   "Get detailed information about an ASDF system including dependencies, version, and source location."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "system_name" p)
                         (%make-ht "type" "string"
                                   "description"
                                   "Name of the ASDF system (e.g., \"cl-mcp\", \"alexandria\")"))
                   (%make-ht "type" "object"
                             "properties" p
                             "required" (vector "system_name")))))

(defun tools-descriptor-asdf-list-systems ()
  (%make-ht
   "name" "asdf-list-systems"
   "description"
   "List all registered ASDF systems."
   "inputSchema" (%make-ht "type" "object"
                            "properties" (%make-ht)
                            "required" (vector))))
```

#### ハンドラー

```lisp
;; asdf-system-info
((member local '("asdf-system-info" "asdf_system_info" "asdf.system-info")
         :test #'string=)
 (handler-case
     (let* ((system-name (and args (gethash "system_name" args))))
       (unless system-name
         (return-from handle-tools-call
           (%error id -32602 "Missing required parameter: system_name")))
       (let ((result (asdf-system-info system-name)))
         (%result id result)))
   (error (e)
     (%error id -32603
             (format nil "Internal error during asdf-system-info: ~A" e)))))

;; asdf-list-systems
((member local '("asdf-list-systems" "asdf_list_systems" "asdf.list-systems")
         :test #'string=)
 (handler-case
     (let ((result (asdf-list-systems)))
       (%result id result))
   (error (e)
     (%error id -32603
             (format nil "Internal error during asdf-list-systems: ~A" e)))))
```

### main.lisp への追加

```lisp
(:import-from #:cl-mcp/src/asdf-tools
              #:asdf-system-info
              #:asdf-list-systems)
...
(:export ...
         #:asdf-system-info
         #:asdf-list-systems
         ...)
```

### cl-mcp.asd への追加

```lisp
(asdf:defsystem "cl-mcp"
  ...
  :depends-on (...
               "cl-mcp/src/asdf-tools"
               ...)
  ...)
```

## 検証方法

### テストケース 1: 既知のシステム情報

```lisp
(asdf-system-info "cl-mcp")
;; => {
;;   "name": "cl-mcp",
;;   "version": "0.2.0",
;;   "description": "Model Context Protocol server for Common Lisp",
;;   "license": "MIT",
;;   "depends_on": ["alexandria", "cl-ppcre", "yason", ...],
;;   "source_file": "/path/to/cl-mcp.asd",
;;   "source_directory": "/path/to/cl-mcp/",
;;   "loaded": true
;; }
```

### テストケース 2: 依存ライブラリの確認

```lisp
(asdf-system-info "alexandria")
;; => {
;;   "name": "alexandria",
;;   "depends_on": [],
;;   "loaded": true,
;;   ...
;; }
```

### テストケース 3: 存在しないシステム

```lisp
(asdf-system-info "nonexistent-system")
;; => Error: Failed to find system nonexistent-system: ...
```

### テストケース 4: システム一覧

```lisp
(asdf-list-systems)
;; => ["alexandria", "bordeaux-threads", "cl-mcp", "cl-ppcre", ...]
```

## 期待される効果

### AIエージェントへのメリット
- ✅ 依存関係を1回のツール呼び出しで取得
- ✅ プロジェクト構造を素早く把握
- ✅ 欠落している依存の検出が容易
- ✅ ライセンス情報の確認

### 開発者へのメリット
- ✅ プロジェクトのドキュメント化
- ✅ 依存関係のトラブルシューティング
- ✅ マルチシステムプロジェクトの管理
- ✅ バージョン情報の一元管理

### 実際の使用例

```
User: このプロジェクトの依存関係を調べて、足りないものがないか確認して

AI: asdf-system-info("cl-mcp")
Result: {
  "depends_on": ["alexandria", "cl-ppcre", "yason", "usocket",
                 "bordeaux-threads", "eclector"]
}

AI: 6つのライブラリに依存しています。それぞれの状態を確認します...

AI: asdf-system-info("alexandria")
Result: {"loaded": true}

AI: すべての依存ライブラリが正しくロードされています。
```

## 制限事項

### ASDF API の制限
- システムがロードされていない場合、一部の情報が取得できない可能性
- カスタム system-class を使用しているシステムでは動作が異なる可能性

### セキュリティ考慮事項
- システム情報はプロジェクト構造を露出する
- ただし、すでに `fs-list-directory` で同様の情報が取得可能

## 関連課題

- **fs-get-project-info**: プロジェクト情報との統合

## 実装時間見積もり
**40分** (新ファイル作成 20分 + protocol統合 10分 + テスト 10分)

## 将来的な拡張

### Phase 2: 依存関係グラフ

```lisp
(asdf-dependency-graph "cl-mcp" :depth 2)
;; => {
;;   "cl-mcp": {
;;     "alexandria": {...},
;;     "cl-ppcre": {...}
;;   }
;; }
```

### Phase 3: コンポーネント情報

```lisp
(asdf-system-components "cl-mcp")
;; => ["main.lisp", "src/core.lisp", "src/fs.lisp", ...]
```
