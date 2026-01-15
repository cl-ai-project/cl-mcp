---
description: トップレベルフォーム間に空行を追加（Google CL Style Guide準拠）
allowed-tools: Bash, Read, Edit, Glob, Grep
argument-hint: [path]
---

# トップレベルフォーム間の空行追加

Common Lispファイルのトップレベルフォーム間に空行を追加し、Google Common Lisp Style Guideに準拠させる。

## 対象

- パス: `$ARGUMENTS` (省略時は `src/` ディレクトリ)
- 拡張子: `.lisp`, `.asd`

## 検出ルール

以下のパターンを検出して空行を追加:
- 行末が `)` で終わり、次の行が `(` で始まる（カラム0）

## 例外（空行不要）

以下のケースは空行を追加しない:

1. **`declaim` の直後** - 型宣言は関連する定義と一緒に
   ```lisp
   (declaim (ftype ...))
   (defun foo ...)  ; OK: 空行不要
   ```

2. **関連するグローバル変数** - `defparameter`/`defvar` の連続
   ```lisp
   (defparameter *log-level* :debug)
   (defparameter *log-stream* *error-output*)  ; OK: 空行不要
   ```

3. **`defpackage` → `in-package`**
   ```lisp
   (defpackage #:my-pkg ...)
   (in-package #:my-pkg)  ; OK: 空行不要
   ```

## 注意事項（誤検知の可能性）

- `#+(or)` 等のリーダーマクロ直後 → 実際は無効化されたコード
- 文字列リテラル内のLispコード → テストデータ等

検出結果を確認し、これらのケースは修正をスキップすること。

## 手順

1. 指定パス内の `.lisp` ファイルを検索
2. 各ファイルで空行が必要な箇所を検出
3. 例外に該当しないケースのみ修正
4. 修正内容をサマリーとして報告

## 検出用コマンド

```bash
python3 << 'EOF'
import os
import sys
import re

path = sys.argv[1] if len(sys.argv) > 1 else 'src'

def find_form_start(lines, end_line_idx):
    """括弧のバランスを追跡して、フォームの開始行を見つける"""
    depth = 0
    for i in range(end_line_idx, -1, -1):
        line = lines[i]
        # 文字列とコメントを除外した簡易カウント
        in_string = False
        for j, ch in enumerate(line):
            if ch == '"' and (j == 0 or line[j-1] != '\\'):
                in_string = not in_string
            if in_string:
                continue
            if ch == ';':
                break
            if ch == '(':
                depth -= 1
            elif ch == ')':
                depth += 1
        if depth <= 0:
            return i
    return 0

def get_form_type(lines, start_idx):
    """フォームの種類を取得（defun, defpackage, declaim等）"""
    line = lines[start_idx].strip()
    match = re.match(r'\((\S+)', line)
    return match.group(1) if match else None

def check_file(filepath):
    issues = []
    with open(filepath, 'r') as f:
        lines = f.readlines()

    for i in range(len(lines) - 1):
        curr = lines[i].rstrip()
        next_line = lines[i + 1]

        if not curr or not curr.endswith(')'):
            continue
        if not next_line.startswith('('):
            continue

        # 現在のフォームの種類を特定
        form_start = find_form_start(lines, i)
        form_type = get_form_type(lines, form_start)

        # 次のフォームの種類を特定
        next_match = re.match(r'\((\S+)', next_line)
        next_form_type = next_match.group(1) if next_match else None

        # 例外チェック
        # 1. declaim直後
        if form_type == 'declaim':
            continue
        # 2. defparameter/defvar連続
        if form_type in ('defparameter', 'defvar') and \
           next_form_type in ('defparameter', 'defvar'):
            continue
        # 3. defpackage → in-package
        if form_type == 'defpackage' and next_form_type == 'in-package':
            continue

        issues.append((i + 1, i + 2, form_type or '?', next_form_type or '?',
                      curr[-40:], next_line.rstrip()[:40]))

    return issues

for root, dirs, files in os.walk(path):
    dirs[:] = [d for d in dirs if not d.startswith('.')]
    for f in files:
        if f.endswith('.lisp') or f.endswith('.asd'):
            filepath = os.path.join(root, f)
            issues = check_file(filepath)
            for line1, line2, form1, form2, end, start in issues:
                print(f'{filepath}:{line1}-{line2} ({form1} -> {form2})')
                print(f'  L{line1}: ...{end}')
                print(f'  L{line2}: {start}')
                print()
EOF
```

まず上記のコマンドで検出を行い、結果を確認してから修正を適用すること。
