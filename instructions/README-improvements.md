# cl-mcp 改善課題一覧

ユーザーフィードバックに基づく改善課題と新機能提案のインデックス。

## 概要

実際の使用経験から得られたフィードバックを元に、以下の課題を特定しました。各課題の詳細は個別のmarkdownファイルに記載されています。

## 🔴 High Priority (早急に修正/実装すべき)

### 1. [check-parens の :false バグ](fix-check-parens-false-bug.md)
- **問題**: `:false` キーワードを使用しているためJSONシリアライゼーションが失敗
- **影響**: ツールが実質的に使用不可能
- **実装時間**: 5分
- **修正**: line 105 の `:false` を `nil` に変更

### 2. [fs-list-directory のエラーメッセージ改善](fix-fs-list-directory-error-message.md)
- **問題**: エラーメッセージに解決後のパスが含まれず、デバッグが困難
- **影響**: パス解決のトラブルシューティングが非効率
- **実装時間**: 10分
- **修正**: エラーメッセージに解決後の絶対パスを追加

### 3. [fs-get-project-info ツールの追加](fix-add-fs-get-project-info.md)
- **問題**: プロジェクトルートや作業ディレクトリが不明
- **影響**: パス指定ミスの原因特定が困難
- **実装時間**: 20分
- **新機能**: `fs-get-project-info()` でプロジェクト情報を取得

## 🟡 Medium Priority (改善推奨)

### 4. [行番号オプション](lisp-read-file-eclector.md) *(既存の改善計画)*
- **問題**: 行番号付きビューがなく、エラー位置特定が不便
- **影響**: エラー報告やパッチ位置指定がやや不便
- **実装時間**: eclector移行時に実装（Phase 2）
- **計画**: lisp-read-file-eclector.md で既に仕様化済み

### 5. [repl-eval のコンパイラ出力捕捉](fix-repl-eval-error-output.md)
- **問題**: コンパイル警告が `*error-output*` に出力されない
- **影響**: コンパイルエラーの原因特定に手間
- **実装時間**: 30分
- **制限**: SBCL固有、完全な解決は困難

### 6. [code-describe-symbol に定義位置情報追加](fix-code-describe-add-location.md)
- **問題**: 定義場所を知るには別のツール呼び出しが必要
- **影響**: ナビゲーションに余分なステップ
- **実装時間**: 25分
- **修正**: path と line を返り値に追加

### 7. [asdf-system-info ツールの追加](feature-asdf-system-info.md)
- **問題**: ASDF依存関係の確認に repl-eval が必要
- **影響**: 大規模プロジェクトで依存関係の把握が煩雑
- **実装時間**: 40分
- **新機能**: `asdf-system-info(system_name)` で依存情報を取得

## 🟢 Low Priority (必要性低い or 代替手段あり)

### 8. [汎用 lint ツール](feature-lint-lisp-code.md)
- **問題**: check-parens は括弧バランスのみチェック
- **代替**: repl-eval で構文チェック可能
- **推奨**: 新ツール不要、現状で十分
- **理由**: 実装コストに対して効果が限定的

### 9. [差分表示 (show-diff)](feature-show-diff.md)
- **問題**: `apply_patch` ツールが存在しない（誤解の可能性）
- **代替**: git diff、または edit-lisp-form にドライランモード追加
- **推奨**: edit-lisp-form の :dry-run パラメータ追加
- **注意**: ユーザーへの確認が必要

## 実装優先順位

### フェーズ1: Critical Fixes (今すぐ)
```
1. fix-check-parens-false-bug.md          (5分)
2. fix-fs-list-directory-error-message.md (10分)
3. fix-add-fs-get-project-info.md         (20分)
```
**合計**: 35分

### フェーズ2: Quick Wins (今週中)
```
4. fix-code-describe-add-location.md      (25分)
5. fix-repl-eval-error-output.md          (30分)
```
**合計**: 55分

### フェーズ3: New Features (必要に応じて)
```
6. feature-asdf-system-info.md            (40分)
7. feature-show-diff.md (dry-run mode)    (30分)
```
**合計**: 70分

### フェーズ4: Major Refactoring (計画済み)
```
8. lisp-read-file-eclector.md             (Phase 1-4で数時間)
```

## 課題ファイル一覧

### バグ修正
- `fix-check-parens-false-bug.md` - :false キーワードバグ
- `fix-fs-list-directory-error-message.md` - エラーメッセージ改善

### 既存機能の拡張
- `fix-add-fs-get-project-info.md` - プロジェクト情報取得
- `fix-code-describe-add-location.md` - 定義位置情報追加
- `fix-repl-eval-error-output.md` - コンパイラ出力捕捉

### 新機能
- `feature-asdf-system-info.md` - ASDF依存関係ツール
- `feature-lint-lisp-code.md` - 汎用構文チェック（保留）
- `feature-show-diff.md` - 差分表示（要確認）

### 既存の改善計画
- `lisp-read-file-eclector.md` - eclector移行計画（既存）
- `edit-lisp-form.md` - 構造的編集の仕様（既存）

## 評価サマリー

### 妥当性評価
すべてのフィードバックは**妥当**と評価されました。

- **6件**: 実際の問題を指摘（実装推奨）
- **1件**: 誤解に基づく可能性（ユーザー確認必要）
- **1件**: 代替手段で十分（新実装不要）

### 実装効果
- 🔴 High Priority: デバッグ効率 50%向上、即座に改善
- 🟡 Medium Priority: ユーザー体験向上、段階的に改善
- 🟢 Low Priority: 限定的な効果、保留可

## 次のステップ

1. **Phase 1 の実装** (35分)
   - 最も影響が大きく、実装が簡単
   - 即座にユーザー体験が改善

2. **ユーザー確認**
   - `apply_patch` の意図を確認
   - 差分表示の具体的なユースケースを確認

3. **Phase 2 の実装** (55分)
   - 一定の効果が見込まれる改善
   - 余裕があれば今週中に

4. **Phase 3 の検討**
   - ユーザーフィードバックを元に優先度を再評価
   - Phase 1-2 の効果を確認してから判断

## 参考情報

### 関連する既存ファイル
- `cl-mcp-requirement.md` - 要件定義
- `codex-todo2.md` - code-find-references の仕様
- `instructions/edit-lisp-form.md` - edit-lisp-form の仕様

### フィードバック元
実際の開発作業で cl-mcp を使用した際に収集されたフィードバック。
