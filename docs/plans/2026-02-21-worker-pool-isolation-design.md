# Worker Pool Isolation Design

**Date**: 2026-02-21
**Status**: Approved
**Goal**: eval暴走からMCPサーバを保護する障害回復能力の実現

## 概要

repl-eval等のLispイメージ依存操作を子プロセス（SBCL）に委譲し、クラッシュ時は自動再起動する。セッション単位でワーカーを専有し、マルチエージェント対応を実現する。

## 1. 全体アーキテクチャ

```
┌─────────────────────────────────────────────────────┐
│                  MCP Server (Parent)                 │
│                                                     │
│  ┌───────────┐  ┌──────────┐  ┌──────────────────┐ │
│  │ Transport  │  │ Tool     │  │ Worker Pool      │ │
│  │ HTTP/TCP/  │→ │ Dispatch │→ │ Manager          │ │
│  │ stdio      │  │          │  │                  │ │
│  └───────────┘  └──────────┘  │ ┌──────────────┐ │ │
│                               │ │ Affinity Map │ │ │
│  ┌───────────────────────┐   │ │ session→worker│ │ │
│  │ Local Tools (parent)  │   │ └──────────────┘ │ │
│  │ - fs-read-file        │   │                  │ │
│  │ - fs-write-file       │   │ Health Monitor   │ │
│  │ - fs-list-directory    │   │ - heartbeat      │ │
│  │ - fs-set-project-root │   │ - crash detect   │ │
│  │ - fs-get-project-info │   │ - auto-restart   │ │
│  │ - lisp-read-file      │   └──────────────────┘ │
│  │ - lisp-edit-form      │          │              │
│  │ - lisp-check-parens   │          │ TCP          │
│  │ - clgrep-search       │          ▼              │
│  │ - clhs-lookup         │  ┌───────────────────┐  │
│  └───────────────────────┘  │ Worker Processes  │  │
│                             │ ┌───────┐┌───────┐│  │
│                             │ │Wkr 0  ││Wkr 1  ││  │
│                             │ │SBCL   ││SBCL   ││  │
│                             │ │+Swank ││+Swank ││  │
│                             │ └───────┘└───────┘│  │
│                             └───────────────────┘  │
└─────────────────────────────────────────────────────┘
```

### ツール分類

| 親プロセス（ローカル実行） | 子プロセス（ワーカー委譲） |
|---|---|
| fs-read-file, fs-write-file, fs-list-directory | repl-eval |
| fs-set-project-root, fs-get-project-info | load-system |
| lisp-read-file (Eclector CST解析) | run-tests |
| lisp-edit-form (CST編集) | code-find, code-describe |
| lisp-check-parens | code-find-references |
| clgrep-search (ファイルベース検索) | inspect-object |
| clhs-lookup (HyperSpecローカル参照) | |

## 2. ワーカープロトコル

### メッセージ形式

既存のJSON-RPC 2.0をそのまま内部通信に再利用する。

```
Parent → Worker (request):
{"jsonrpc":"2.0","id":1,"method":"worker/eval",
 "params":{"code":"(+ 1 2)","package":"CL-USER","timeout_seconds":30}}

Worker → Parent (response):
{"jsonrpc":"2.0","id":1,"result":{"content":"3","stdout":"","stderr":""}}
```

### 内部メソッド一覧

| メソッド | 対応MCP tool | 説明 |
|---|---|---|
| `worker/eval` | repl-eval | フォーム評価 |
| `worker/load-system` | load-system | ASDFシステムロード |
| `worker/run-tests` | run-tests | テスト実行 |
| `worker/code-find` | code-find | シンボル定義検索 |
| `worker/code-describe` | code-describe | シンボル情報 |
| `worker/code-find-references` | code-find-references | 参照検索 |
| `worker/inspect-object` | inspect-object | オブジェクト検査 |
| `worker/set-project-root` | (内部) | project-root同期 |
| `worker/ping` | (内部) | ヘルスチェック |

### タイムアウト

タイムアウトは**親プロセス側で制御**する。

```
Parent                          Worker
  │                               │
  │── worker/eval ───────────────→│
  │   (timeout managed by parent) │── eval実行
  │                               │
  │   [timeout超過]               │
  │── uiop:terminate-process      │
  │   (SIGTERM, 猶予2秒後SIGKILL)│
  │── ワーカー再起動              │
  │── エラー応答をクライアントへ  │
```

現在の「ポーリング + destroy-thread」方式から「TCP接続のタイムアウト + プロセスkill」に変わるため、FFIブロック中のスレッドが殺せない問題が解消される。

### 接続管理

- 親プロセスは各ワーカーへの永続TCP接続を1本維持
- 接続が切れた = ワーカーがクラッシュした、として再起動をトリガー
- ワーカー側は単一接続のみ受け付け（1:1対応）

## 3. ワーカープールマネージャ

### プール構成

```lisp
(defparameter *worker-pool-warmup* 2)      ; ウォームスタンバイ数
(defparameter *worker-restart-delay* 1.0)  ; 再起動待機(秒)
(defparameter *worker-max-restarts* 5)     ; 再起動上限(窓内)
(defparameter *worker-restart-window* 60)  ; 再起動カウント窓(秒)
```

`*worker-pool-warmup*`はリクエスト到着前にあらかじめ起動しておくウォームスタンバイの数。上限ではない。

### セッションとワーカーの1:1専有

```
Session A ──── Worker 0 (専有)
Session B ──── Worker 1 (専有)
Session C ──── Worker 2 (スケールアウトで新規起動)
Session D ──── Worker 3 (スケールアウトで新規起動)

[ウォームスタンバイ]
  Worker 4 (未割当、ready、次のセッション用)
  Worker 5 (未割当、ready、次のセッション用)
```

**ルール:**
- 1セッション = 1ワーカー。開始から終了まで完全専有。**フォールバック禁止**
- 新セッション到着時: ウォームスタンバイから1つ割り当て
- スタンバイが空の場合: 新ワーカーを起動して割り当て（初回レイテンシ発生）
- セッション終了時: ワーカーをkillし、代わりにウォームスタンバイを1つ補充起動
- ウォームスタンバイは常に`*worker-pool-warmup*`個を維持するよう補充

### ワーカーの状態遷移

```
            起動
  [dead] ─────→ [starting]
    ↑               │
    │          TCP接続確立
    │               ↓
    │          [standby] ─────── 未割当、セッション待ち
    │               │
    │          セッション割当
    │               ↓
    │          [bound] ←────┐
    │               │       │完了
    │          リクエスト    │
    │               ↓       │
    │          [busy] ──────┘
    │               │
    │          クラッシュ/タイムアウト
    │               ↓
    │          [crashed]
    │               │
    │          再起動 (同セッション維持)
    │               ↓
    └────────── [restarting] → [bound] (要リセット通知)
```

### クラッシュ時のリセット通知

ワーカーがクラッシュ→再起動した場合、ワーカーに`needs-reset-notification`フラグを立てる。次にそのワーカーへのリクエストが来た時:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Worker process crashed and was restarted. All Lisp state (loaded systems, defined functions, package state) has been reset. Please run load-system again to restore your environment."
      }
    ],
    "isError": true
  }
}
```

- リセット通知は**一度だけ**返す。通知後にフラグをクリア
- タイムアウトやクラッシュ時のエラーレスポンス自体が通知を兼ねるため、その場合は次回の通知は不要

### スケールアウトとリソース制御

```
新セッション到着
  → スタンバイあり？
     Yes → スタンバイから割り当て [standby] → [bound]
     No  → 新ワーカー起動 [dead] → [starting] → [bound]
           (クライアントはTCP接続確立まで待つ)

  → スタンバイ補充チェック
     現スタンバイ数 < *worker-pool-warmup* ?
     Yes → バックグラウンドでスタンバイ追加起動

セッション終了
  → ワーカーをkill
  → スタンバイ補充チェック (同上)
```

リソースの安全弁はOSレベル（ulimit等）に委ねる。アプリ内でのハードリミットは設けない。

## 4. ワーカープロセスの内部構造

### ワーカーの起動

親プロセスが`uiop:launch-program`でSBCLプロセスを起動する。

```lisp
(uiop:launch-program
  (list "ros" "run" "-s" "cl-mcp/src/worker/main"
        "-e" "(cl-mcp/src/worker/main:start)")
  :environment (list* (format nil "MCP_PROJECT_ROOT=~A" *project-root*)
                      ...)
  :output :stream
  :error-output :stream)
```

**環境変数の伝播**: 親プロセスの`*project-root*`を`MCP_PROJECT_ROOT`として子プロセスに渡す。親プロセスで`fs-set-project-root`が呼ばれた場合は、バインド済み全ワーカーに`worker/set-project-root`メッセージを送信する。

### ポート割り当て（動的ポート）

ワーカーは常に`:port 0`で起動し、OSがエフェメラルポートを自動割当する。`*worker-port-base*`のようなパラメータは不要。

```
親プロセス                              ワーカープロセス
    │                                       │
    │── uiop:launch-program ───────────────→│ 起動
    │   (:output :stream)                   │
    │                                       │── usocket:socket-listen :port 0
    │                                       │── Swank create-server :port 0
    │                                       │
    │                                       │── stdout に1行JSON出力:
    │   ← {"tcp_port":41023,               │   {"tcp_port":41023,
    │       "swank_port":41524,             │    "swank_port":41524,
    │       "pid":12345}                    │    "pid":12345}
    │                                       │
    │── stdout から JSON 読み取り           │
    │   (*worker-startup-timeout* 以内)     │
    │── tcp_port に TCP 接続 ──────────────→│── accept
    │                                       │
    │── ハンドシェイク ────────────────────⇄│── worker/hello
    │   (sbcl_version等の追加メタデータ)    │
```

これにより、ポート競合（Address already in use）やTIME_WAIT問題を完全に排除する。

### ワーカー側のコンポーネント

```
Worker Process (SBCL)
  │
  ├─ TCP Server (単一接続のみ受付, 動的ポート)
  │    └─ JSON-RPC handler
  │
  ├─ Swank Server (動的ポート, :port 0)
  │    └─ 人間が M-x slime-connect で接続可能
  │    └─ ワーカーの全Lisp状態を観察・操作可能
  │
  ├─ Tool Handlers
  │    ├─ worker/eval
  │    ├─ worker/load-system
  │    ├─ worker/run-tests
  │    ├─ worker/code-find
  │    ├─ worker/code-describe
  │    ├─ worker/code-find-references
  │    ├─ worker/inspect-object
  │    ├─ worker/set-project-root
  │    └─ worker/ping
  │
  └─ Signal Handler
       └─ SIGTERM → graceful shutdown
```

### Swank連携: 人間とAIの協調

各ワーカーは起動時にSwankサーバを動的ポートで起動する。Swankは`.asd`依存に含めず、動的ロードする。

```lisp
;; src/worker/main.lisp
(defun start ()
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 ...))
         (tcp-port (usocket:get-local-port listener))
         (swank-port (start-swank-if-available)))
    ;; stdout に1行JSONで出力
    (format t "~A~%"
            (yason:with-output-to-string* ()
              (yason:encode-plist
                (list "tcp_port" tcp-port
                      "swank_port" swank-port
                      "pid" (sb-posix:getpid)))))
    (force-output *standard-output*)
    (accept-and-serve listener)))

(defun start-swank-if-available ()
  "Swankを動的ロードして起動。失敗時はnilを返す。"
  (handler-case
      (progn
        (ql:quickload :swank :silent t)
        (funcall (find-symbol "CREATE-SERVER" "SWANK")
                 :port 0 :dont-close t))
    (error (e)
      (log-warn "Swank not available: ~A" e)
      nil)))
```

親プロセスはワーカー情報を`fs-get-project-info`のレスポンスに含めて公開する。

```json
{
  "project_root": "/home/wiz/my-project",
  "workers": [
    {
      "id": 0,
      "session": "abc-123",
      "tcp_port": 41023,
      "swank_port": 41524,
      "pid": 12345,
      "state": "bound",
      "uptime_seconds": 3600
    }
  ]
}
```

人間の開発者は `M-x slime-connect RET 127.0.0.1 RET 41524 RET` でワーカーに接続し、AIが作業中のLispイメージをリアルタイムで観察・介入できる。

### 既存コードの再利用方針

ワーカー委譲するツールの既存実装は、**ロジック部分を共有パッケージ（`*-core.lisp`）に抽出**し、親・ワーカー両方から利用する。

### タイムアウト処理

```
親プロセス側:
  1. ワーカーにJSON-RPCリクエスト送信
  2. ソケットreadにタイムアウト設定
  3. タイムアウト超過:
     → uiop:terminate-process でSIGTERM送信
     → 猶予期間(2秒)後に応答なければ SIGKILL
     → ワーカー再起動 + needs-reset-notification フラグ
     → クライアントにタイムアウトエラー返却

ワーカー側:
  - タイムアウト制御なし。リクエストを素直に実行
  - 暴走 → 親がプロセスごとkill
```

## 5. 親プロセス側のプロキシ層

### ツールディスパッチの変更

```
現在:
  クライアント → tools/call "repl-eval" → define-tool handler → %do-repl-eval → 結果

変更後:
  クライアント → tools/call "repl-eval" → define-tool handler (プロキシ)
                                            → セッションID取得
                                            → アフィニティでワーカー特定
                                            → worker/eval をワーカーへ送信
                                            → 応答をそのまま返却
```

### セッションIDの解決

| トランスポート | セッションID | 取得方法 |
|---|---|---|
| HTTP | Mcp-Session-Id ヘッダ | 既存の`*current-session*`動的変数 |
| TCP | 接続単位 | 接続ハンドラが動的変数にバインド |
| stdio | 固定（単一セッション） | `"stdio"` 固定値 |

### プロキシの共通パターン

```lisp
(defun proxy-to-worker (method params)
  (let* ((session-id (current-session-id))
         (worker (get-or-assign-worker session-id)))
    (cond
      ((worker-needs-reset-notification-p worker)
       (clear-reset-notification worker)
       (make-tool-error
         "Worker process crashed and was restarted. All Lisp state (loaded systems, defined functions, package state) has been reset. Please run load-system again to restore your environment."))
      (t
       (worker-rpc worker method params)))))
```

### get-or-assign-worker のフロー

```
get-or-assign-worker(session-id)
  │
  ├─ アフィニティマップにsession-idが存在？
  │    Yes → そのワーカーを返す
  │    No  ↓
  │
  ├─ スタンバイプールに空きワーカーがある？
  │    Yes → 割り当て、アフィニティ登録、[standby]→[bound]、返す
  │    No  ↓
  │
  └─ 新ワーカーを起動
       → プレースホルダを登録してロック解放
       → ロック外でワーカー起動（秒オーダー）
       → 起動完了後、プレースホルダを実ワーカーに差し替え
       → バックグラウンドでスタンバイ補充開始
```

### worker-rpc

```lisp
(defun worker-rpc (worker method params &key timeout)
  (bt:with-lock-held ((worker-stream-lock worker))
    (let ((id (next-request-id worker)))
      (send-json-rpc (worker-stream worker)
                     :id id :method method :params params)
      (handler-case
          (with-socket-timeout ((worker-stream worker) timeout)
            (read-json-rpc-response (worker-stream worker) id))
        (socket-timeout ()
          (kill-and-restart-worker worker)
          (make-tool-error
            (format nil "Evaluation timed out after ~A seconds. Worker was killed and restarted."
                    timeout)))
        (end-of-file ()
          (restart-worker worker)
          (make-tool-error
            "Worker process crashed during execution. Worker has been restarted."))))))
```

### セッション終了時のクリーンアップ

```
セッション終了 (HTTP DELETE /mcp, TCP切断, stdio EOF)
  → アフィニティマップからsession-id削除
  → ワーカーをSIGTERM → SIGKILL
  → スタンバイ補充チェック
```

### スレッドセーフティ

#### ロック構造の全体像

```
*pool-lock* (グローバル)
  │ プールとアフィニティマップの構造変更を保護
  │
  ├─ *affinity-map* 読み書き
  ├─ *standby-workers* 読み書き
  └─ worker.state 変更

worker.stream-lock (ワーカー単位)
  │ 同一ワーカーへのRPC送受信の直列化を保護
  │
  └─ send → receive サイクルのアトミック性

placeholder.lock + placeholder.condvar (プレースホルダ単位)
  │ ワーカー起動完了の通知を保護
  │
  └─ 起動待ちスレッドの効率的ブロック/通知
```

#### プレースホルダの条件変数による待機

```lisp
(defstruct worker-placeholder
  (session-id nil)
  (lock (bt:make-lock "placeholder-lock"))
  (condvar (bt:make-condition-variable :name "placeholder-ready"))
  (state :spawning)    ; :spawning | :ready | :failed
  (worker nil)         ; 起動成功時に実ワーカーをセット
  (error-message nil)) ; 起動失敗時にエラー情報をセット
```

待機側スレッド:

```lisp
(defun wait-for-placeholder (placeholder timeout)
  (bt:with-lock-held ((placeholder-lock placeholder))
    (loop while (eq (placeholder-state placeholder) :spawning)
          do (bt:condition-wait (placeholder-condvar placeholder)
                                (placeholder-lock placeholder)
                                :timeout timeout))
    (case (placeholder-state placeholder)
      (:ready (placeholder-worker placeholder))
      (:failed (error 'worker-spawn-failed
                      :message (placeholder-error-message placeholder)))
      (:spawning (error 'worker-spawn-timeout)))))
```

起動完了通知（**condition-broadcast** で全待機スレッドを起こす）:

```lisp
(defun finalize-placeholder (placeholder worker)
  (bt:with-lock-held ((placeholder-lock placeholder))
    (setf (placeholder-worker placeholder) worker)
    (setf (placeholder-state placeholder) :ready)
    (bt:condition-broadcast (placeholder-condvar placeholder))))
```

#### ワーカー起動失敗時のデッドロック防止

```lisp
(defun spawn-and-bind-worker (session-id placeholder)
  (let ((worker nil))
    (unwind-protect
        (progn
          (setf worker (%spawn-worker))
          (%wait-for-handshake worker)
          (bt:with-lock-held (*pool-lock*)
            (setf (gethash session-id *affinity-map*) worker))
          (finalize-placeholder placeholder worker))
      ;; 失敗時クリーンアップ (unwind-protect cleanup)
      (when (null (placeholder-worker placeholder))
        (bt:with-lock-held (*pool-lock*)
          (remhash session-id *affinity-map*))
        (bt:with-lock-held ((placeholder-lock placeholder))
          (setf (placeholder-state placeholder) :failed)
          (setf (placeholder-error-message placeholder)
                "Worker process failed to start. Please retry.")
          (bt:condition-broadcast (placeholder-condvar placeholder)))
        (when worker
          (ignore-errors (uiop:terminate-process
                           (worker-process-info worker) :urgent t)))))))
```

#### ワーカー構造体

```lisp
(defstruct worker
  (id nil)
  (state :dead)
  (process-info nil)      ; uiop:launch-program の返り値
  (stream nil)            ; TCP ストリーム
  (stream-lock (bt:make-lock "worker-stream-lock"))
  (tcp-port nil)
  (swank-port nil)
  (pid nil)
  (needs-reset-notification nil)
  (session-id nil))
```

#### ロック取得順序の規約（デッドロック防止）

```
*pool-lock* → placeholder.lock → worker.stream-lock
(外側)                                      (内側)
```

逆順でのロック取得を禁止。

## 6. システム構成

### package-inferred-system に準拠

cl-mcpは`:class :package-inferred-system`を採用。ファイルパス=パッケージ名=システム名として自動解決されるため、明示的な`defsystem`の追加は不要。

**ワーカー起動コマンド:**
```
ros run -s cl-mcp/src/worker/main \
    -e "(cl-mcp/src/worker/main:start)"
```

### ファイル構成

```
cl-mcp/
  ├─ cl-mcp.asd                      (depends-on 整理のみ)
  ├─ src/
  │    ├─ main.lisp                   (既存: depends-on 更新)
  │    │
  │    ├─ repl-core.lisp              ← 新規: eval ロジック抽出
  │    ├─ code-core.lisp              ← 新規: sb-introspect ロジック抽出
  │    ├─ system-loader-core.lisp     ← 新規: ASDF load ロジック抽出
  │    ├─ test-runner-core.lisp       ← 新規: Rove 実行ロジック抽出
  │    │
  │    ├─ repl.lisp                   (既存→プロキシに変更)
  │    ├─ code.lisp                   (既存→プロキシに変更)
  │    ├─ system-loader.lisp          (既存→プロキシに変更)
  │    ├─ test-runner.lisp            (既存→プロキシに変更)
  │    │
  │    ├─ pool.lisp                   ← 新規: プールマネージャ
  │    ├─ worker-client.lisp          ← 新規: 親→ワーカー RPC 通信
  │    │
  │    └─ worker/
  │         ├─ main.lisp              ← 新規: エントリポイント
  │         ├─ server.lisp            ← 新規: TCP サーバ + ハンドシェイク
  │         └─ handlers.lisp          ← 新規: worker/* ハンドラ群
  │
  └─ tests/
       ├─ pool-test.lisp              ← 新規
       └─ worker-test.lisp            ← 新規
```

### 依存関係グラフ

```
親プロセス側:
  cl-mcp/main
    → cl-mcp/src/pool
    → cl-mcp/src/worker-client
    → cl-mcp/src/repl           (プロキシ版)
    → cl-mcp/src/code           (プロキシ版)
    → cl-mcp/src/system-loader  (プロキシ版)
    → cl-mcp/src/test-runner    (プロキシ版)
    → ... (fs, lisp-read-file 等は変更なし)

ワーカープロセス側:
  cl-mcp/src/worker/main
    → cl-mcp/src/worker/server
    → cl-mcp/src/worker/handlers
    → cl-mcp/src/repl-core
    → cl-mcp/src/code-core
    → cl-mcp/src/system-loader-core
    → cl-mcp/src/test-runner-core
    → cl-mcp/src/object-registry (既存)
```

### 既存ファイルの変更影響

| ファイル | 変更内容 |
|---|---|
| `src/repl.lisp` | ロジックを`repl-core.lisp`に抽出。`define-tool`はプロキシに |
| `src/code.lisp` | ロジックを`code-core.lisp`に抽出。`define-tool`はプロキシに |
| `src/system-loader.lisp` | ロジックを`system-loader-core.lisp`に抽出 |
| `src/test-runner.lisp` | ロジックを`test-runner-core.lisp`に抽出 |
| `src/protocol.lisp` | セッションID取得の仕組み追加 |
| `src/http.lisp` | セッション終了時のワーカークリーンアップ追加 |
| `src/tcp.lisp` | 接続単位のセッションID生成追加 |
| `src/main.lisp` | 新パッケージへの依存追加 |

### Swankのロード戦略

Swankは`.asd`依存に含めない。ワーカー起動時に`(ql:quickload :swank :silent t)`で動的ロード。Swankが利用不可でもワーカーは正常起動する（graceful degradation）。`find-symbol`で関数を取得し呼び出すことでコンパイル時のSwank依存を完全に排除。

## 7. 移行戦略とテスト

### 段階的移行

**フェーズ1: コア分離（リファクタリングのみ、動作変更なし）**
```
repl.lisp → repl-core.lisp + repl.lisp(既存define-toolが直接core呼出)
code.lisp → code-core.lisp + code.lisp(同上)
system-loader.lisp → system-loader-core.lisp + system-loader.lisp(同上)
test-runner.lisp → test-runner-core.lisp + test-runner.lisp(同上)
```
既存テストが全てパスすることを確認。外部から見た動作は完全に同一。

**フェーズ2: ワーカープロセスの実装**
```
src/worker/main.lisp, server.lisp, handlers.lisp を新規作成
単体でワーカーが起動し、TCP経由でJSON-RPCリクエストを処理できることを確認
```

**フェーズ3: プールマネージャとプロキシ**
```
src/pool.lisp, src/worker-client.lisp を新規作成
既存 define-tool をプロキシに切り替え
```

**フェーズ4: 統合テスト + Swank連携**
```
エンドツーエンドで MCP クライアント → 親 → ワーカー → 応答 が動作
Swank接続の動作確認
クラッシュ回復の動作確認
```

### フォールバックモード

```lisp
(defparameter *use-worker-pool* t
  "NIL なら従来のインプロセス実行。T ならワーカープロセスに委譲。")
```

```lisp
(define-tool "repl-eval" ...
  (if *use-worker-pool*
      (proxy-to-worker "worker/eval" params)
      (%do-repl-eval ...)))  ; 従来パス
```

### テスト戦略

| テスト対象 | テスト種別 | ファイル |
|---|---|---|
| repl-core, code-core 等 | ユニット | 既存テストがそのまま通る |
| ワーカーTCPサーバ | ユニット | `tests/worker-test.lisp` |
| プールマネージャ | ユニット | `tests/pool-test.lisp` |
| プロキシ→ワーカー往復 | 統合 | `tests/pool-test.lisp` |
| クラッシュ検知・再起動 | 統合 | `tests/pool-test.lisp` |
| リセット通知 | 統合 | `tests/pool-test.lisp` |
| 並行アクセス（ロック） | 並行 | `tests/pool-test.lisp` |
| Swank接続 | 手動 | ドキュメントに手順記載 |

### 設定パラメータ一覧

| パラメータ | デフォルト | 説明 |
|---|---|---|
| `*use-worker-pool*` | `t` | ワーカー委譲の有効/無効 |
| `*worker-pool-warmup*` | `2` | ウォームスタンバイ数 |
| `*worker-restart-delay*` | `1.0` | 再起動待機(秒) |
| `*worker-max-restarts*` | `5` | 窓内の最大再起動回数 |
| `*worker-restart-window*` | `60` | 再起動カウント窓(秒) |
| `*worker-heartbeat-interval*` | `5` | ヘルスチェック間隔(秒) |
| `*worker-heartbeat-timeout*` | `3` | 無応答でクラッシュ判定する回数 |
| `*worker-startup-timeout*` | `30` | stdout JSON待ちタイムアウト(秒) |
