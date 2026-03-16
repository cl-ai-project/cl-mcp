# lisp-read-file トークン節約設計

**日付**: 2026-03-16  
**ステータス**: 承認済み

## 問題

`lisp-read-file` を `collapsed=false`（raw モード）で使うと大量のテキストが返る。

- デフォルト行数が 2000 行と多すぎる
- truncate 時に `total_lines` / `truncated` は `meta` フィールドに入っているが、MCP クライアントは `content[].text` しかレンダリングしないため LLM に届いていない

## スコープ

`collapsed=false`（raw モード）のみ。`collapsed=true` はスコープ外。

## 設計

### 変更 1：デフォルト行数削減

`*default-line-limit*`: 2000 → 500

### 変更 2：truncate フッターの埋め込み

truncate 発生時、コンテンツ末尾に可視テキストを追記する。

フォーマット：
```
[Showing lines 1-500 of 3000. Use offset=500 to read more.]
```

offset 指定時：
```
[Showing lines 501-1000 of 3000. Use offset=1000 to read more.]
```

truncate がない場合（末尾まで読み切り）：フッターなし。

### 変更しないもの

- `meta["total_lines"]` / `meta["truncated"]` はそのまま保持（後方互換）
- `collapsed=true` モードの動作は変更なし

## 変更ファイル

`src/lisp-read-file.lisp` のみ

1. `*default-line-limit*` の値を 500 に変更
2. `%lisp-read-file-content` の `(not collapsed)` ブランチでフッターを追記
3. `define-tool` の `limit` パラメータ説明を更新

## テスト

`tests/fs-test.lisp` に追加：

- truncate 時にフッターが含まれること
- フッターの行番号・総行数が正確なこと
- offset 指定時のフッターが正しいこと
- truncate されない場合はフッターがないこと
