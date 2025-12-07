# 改善課題: repl-eval のコンパイラ出力捕捉

## 優先度
🟡 **Medium** - 部分的改善は可能だが完全な解決は困難

## 課題の詳細

### 現在の問題

コンパイルエラー時に `repl-eval` では `*error-output*` が拾われず、バックトレースのみになる。標準出力と標準エラーの両方を自動で束縛するオプションがあると原因特定が楽になる。

### 具体例

```lisp
;; コンパイルエラーを含むコード
(repl-eval "(defun foo (x) (+ x undefined-var))")
```

**現在の動作**:
- `*error-output*` (stderr): 空文字列
- `*standard-output*` (stdout): 空文字列
- 返り値: バックトレースのみ

**期待される動作**:
- `*error-output*`: "WARNING: undefined variable: UNDEFINED-VAR"
- コンパイルの警告やノートも捕捉

### 根本原因

#### 1. SBCL のコンパイラ出力は特別なストリームを使用

SBCL のコンパイラは以下の専用ストリームに出力：
- `*compiler-note-stream*`: コンパイラノート
- `sb-c::*compiler-error-output*`: コンパイラエラー
- `sb-c::*compiler-trace-output*`: コンパイラトレース

これらは `*error-output*` とは独立している。

#### 2. Warning は error ではない

```lisp
;; src/repl.lisp:58-68
(handler-bind ((error (lambda (e) ...)))
  ...)
```

このコードは `error` condition のみキャッチするが、`warning` は素通しする。

#### 3. 実装依存

この動作は SBCL 固有であり、他の Common Lisp 実装では異なる可能性がある。

## 修正方針

### Option A: コンパイラストリームのリダイレクト（推奨）

SBCL 専用だが、コンパイラ出力を捕捉できる。

```lisp
#+sbcl
(let ((sb-c::*compiler-error-output* stderr)
      (sb-c::*compiler-note-stream* stderr))
  (eval form))
```

**利点**: コンパイラ出力を確実に捕捉
**欠点**: SBCL 固有、internal API 依存

### Option B: Warning をハンドル

```lisp
(handler-bind ((warning (lambda (w)
                          (format stderr "~&Warning: ~A~%" w)
                          (muffle-warning)))
               (error (lambda (e) ...)))
  (eval form))
```

**利点**: 移植性が高い
**欠点**: コンパイラノートは捕捉できない

### Option C: 新しいパラメータ `:capture-warnings`

```lisp
(repl-eval input :capture-warnings t)
```

**利点**: 後方互換性を保ちながら機能追加
**欠点**: 実装が複雑

### 推奨実装

**Hybrid approach**: Option A + Option B を組み合わせ

```lisp
;; SBCL では専用ストリームをリダイレクト
;; 他の実装では warning をハンドル
```

## 実装

### 修正対象ファイル
`src/repl.lisp`

### 修正前 (line 54-82)

```lisp
(do-eval ()
  (let ((last-value nil)
        (stdout (make-string-output-stream))
        (stderr (make-string-output-stream)))
    (handler-bind ((error (lambda (e) ...)))
      (let* ((pkg ...))
        ...
        (let ((*standard-output* stdout)
              (*error-output* stderr))
          (dolist (form forms)
            (setf last-value (eval form))))))))
```

### 修正後 (提案)

```lisp
(do-eval ()
  (let ((last-value nil)
        (stdout (make-string-output-stream))
        (stderr (make-string-output-stream)))
    (handler-bind
        ((warning (lambda (w)
                    (format stderr "~&Warning: ~A~%" w)
                    (muffle-warning)))
         (error (lambda (e) ...)))
      (let* ((pkg ...))
        ...
        (let ((*standard-output* stdout)
              (*error-output* stderr)
              ;; SBCL 専用: コンパイラストリームもリダイレクト
              #+sbcl (sb-c::*compiler-error-output* stderr)
              #+sbcl (sb-c::*compiler-note-stream* stderr))
          (dolist (form forms)
            (setf last-value (eval form))))))))
```

### 新しいパラメータの追加 (Optional)

```lisp
(defun repl-eval (input &key (package *default-eval-package*)
                             (print-level nil)
                             (print-length nil)
                             (timeout-seconds nil)
                             (max-output-length nil)
                             (safe-read nil)
                             (capture-warnings t))  ; 新パラメータ
  ...)
```

## 検証方法

### テストケース 1: 未定義変数の警告

```lisp
(repl-eval "(defun foo (x) (+ x undefined-var))")
```

**修正前**:
- stdout: ""
- stderr: ""
- result: バックトレースのみ

**修正後**:
- stdout: ""
- stderr: "Warning: undefined variable: UNDEFINED-VAR\n..."
- result: バックトレースまたは関数定義成功

### テストケース 2: 型宣言の警告

```lisp
(repl-eval "(let ((x 'foo)) (+ x 1))")
```

**修正後**:
- stderr: "Warning: Asserted type SYMBOL conflicts with derived type NUMBER.\n..."

### テストケース 3: 通常のエラー（変更なし）

```lisp
(repl-eval "(/ 1 0)")
```

**動作**: 従来通りエラーハンドリング

### テストケース 4: コンパイル成功（警告なし）

```lisp
(repl-eval "(defun bar (x) (+ x 1))")
```

**動作**: 従来通り

## 期待される効果

### 修正前
- ❌ コンパイラ警告が見えない
- ❌ 型エラーの原因が不明
- ❌ デバッグに追加の repl-eval 呼び出しが必要
- ❌ AIエージェントが問題を特定できない

### 修正後
- ✅ コンパイラ警告を stderr に出力
- ✅ 型エラーの詳細が分かる
- ✅ デバッグが効率化
- ⚠️ SBCL では完全、他の実装では部分的

## 制限事項

### SBCL 固有の問題
1. `sb-c::*compiler-error-output*` は internal API
2. 将来のバージョンで変更される可能性
3. 他の CL 実装では動作しない

### 代替策
- `compile` 関数を明示的に使う（より確実）
- `with-compilation-unit` でラップする
- ASDF を使ってファイルコンパイル

### 完全な解決が困難な理由
- コンパイラの内部実装に依存
- 実装間の移植性が低い
- すべての出力を捕捉するのは本質的に難しい

## 関連課題

- なし (独立した改善)

## 実装時間見積もり
**30分** (実装 20分 + SBCL/他実装でのテスト 10分)

## 将来的な改善案

### Phase 2: compile 関数の使用

```lisp
(defun repl-eval-with-compile (input &key ...)
  "Explicitly compile forms before evaluation for better error reporting."
  ...)
```

### Phase 3: 実装別の最適化

```lisp
#+sbcl (handle-sbcl-compiler-output ...)
#+ccl (handle-ccl-compiler-output ...)
#-(or sbcl ccl) (handle-generic-warnings ...)
```
