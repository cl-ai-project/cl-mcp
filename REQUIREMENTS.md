# cl-mcp REQUIREMENTS

本書は、AI Agent が Common Lisp 環境で REPL 駆動開発を行うための MCP (Model Context Protocol) サーバー実装要件をまとめたものです。ライセンスは MIT、配布は Quicklisp を前提とします。

## 1. ゴールとユースケース
- クライアント: AI Agent。
- ゴール: Agent が MCP 経由で Lisp サーバーと対話し、REPL 駆動でコード編集・ビルド・参照探索を行い、実プログラムを完成させる。
- MVP 成功条件: Agent がサーバーと通信し、(a) プロジェクト読込、(b) 参照探索（関数定義位置）、(c) AST 取得、(d) ビルド/ロード を一連で完了できること。

## 2. プロトコル
- 準拠: MCP 最新仕様（バージョンは別紙参照; 実装時点での最新版に追随）。
- JSON-RPC: 2.0 互換メッセージ・エラーフォーマット。
- 機能面: `tools` を中心に、最小限の `resources`/`prompts` を提供可能とする設計。

## 3. トランスポート/起動
- 既存 REPL から `(asdf:load-system :cl-mcp)`→`(cl-mcp:run &key transport ...)` で起動。
- 既定は `:stdio`（クライアントの標準入出力と接続される環境を想定）。REPL 併用時の衝突を避けるため、開発利便性として `:tcp`（127.0.0.1:port, JSON-RPC over length‑prefixed stream）も提供。後方互換のためトランスポートは抽象化する。

## 4. 対応環境
- OS: Linux x86_64 優先（macOS はベストエフォート）。
- 処理系: SBCL 2.x を第一優先。
- 依存: Quicklisp 経由で取得。JSON は `jzon` を想定。

## 5. セキュリティ
- 既定は最小権限。ファイルアクセスはプロジェクトルートの allow-list 配下に限定。
- MVP では `eval` を提供する。読み込み時の `#.` 等も許可する前提で、評価は指定パッケージ（既定 `CL-USER`）で逐次実行し、最後の値を返す。
- 将来の強化: フォームの allow-list、別パッケージ/スレッドでの隔離、CPU/時間制限、より厳密な I/O サンドボックス。

## 6. 提供機能（MVP）
- tools
  - `fs.list_dir(path, recursive?, page)`：隠し/巨大ファイル除外・ページング。
  - `fs.read_file(path, offset?, limit?)`：テキストのみ、サイズ上限あり。
  - `project.load_system(name)`：ASDF により依存も含めロード。
  - `project.build()`：`asdf:compile-op` 相当を実行。
  - `repl.eval(code, package?, print-level?, print-length?)`：`code` 文字列をパースし逐次評価。読み込み時も `eval` を許容し、最後の値と表示文字列を返す。
  - `code.find_definition(symbol, kind)`：関数/マクロ等の定義位置を返す（静的解析＋実装依存機能の併用、SBCL では `sb-introspect` を利用、他処理系は静的解析にフォールバック）。
  - `code.ast_at(path, form-range)`：安全な S 式パーサで AST を抽出（リーダーマクロを実行しない独自パース）。
- resources
  - プロジェクト内のドキュメント（README 等）を参照提供（将来拡張）。
- prompts
  - コード閲覧支援や検索の最小テンプレートを定義（将来拡張）。

## 7. 同時実行/制限
- 同時処理: 16 リクエストまで（キューイング）。
- タイムアウト: 既定 10s、最大 60s。
- ペイロード: リクエスト/レスポンス 1MB 目安。

## 8. ロギング/計測
- JSON Lines を stderr に出力。`request-id`・レベル（INFO/DEBUG/WARN/ERROR）。
- メトリクス（QPS/レイテンシ/失敗率）は将来拡張（簡易カウンタは実装）。

## 9. エラー方針
- ユーザー入力/環境/内部の 3 区分で条件型を定義し、再試行ヒントを付与。キャンセル/タイムアウトを尊重。

## 10. ビルド/配布
- ASDF システム: `cl-mcp` と `cl-mcp/tests`。
- Quicklisp での配布を目指し、依存は保守的に選定。

## 11. テスト/CI
- Rove によるユニット/統合テスト。ASDF `test-op` から実行可能。
- 主要経路: ハンドシェイク、`fs.*`、`project.*`、`code.find_definition`、`code.ast_at`。

## 12. 非機能
- 可読性/移植性優先。測定に基づく最小限の最適化（型宣言・`optimize` 等）。

## 13. 未確定事項（確認中）
- クライアントが利用可能なトランスポート（stdio のみか、ローカル TCP も可か）。
- `code.find_definition` の移植性要件（SBCL 以外でどの程度必要か）。
- AST 仕様（S 式のままか、フィールド名を正規化した構造体か）。

---
本要件に基づき、まず ASDF/パッケージ/テスト骨組みを追加し、順に `fs.*` → `project.*` → `code.*` の TDD 開発を進めます。
