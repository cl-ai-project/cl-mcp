# Worker debugger observe-and-abort — 設計

- 日付: 2026-09-21
- 対象: pooled SBCL worker の request 実行境界
- 状態: 提案。実装計画と production code はこの spec のレビュー後に作成する。

## 1. 目的と非目的

pooled worker は起動時に `SB-EXT:DISABLE-DEBUGGER` を呼ぶ。既存の
`ERROR` / `SERIOUS-CONDITION` boundary を抜けた condition が debugger path に
到達すると、disabled debugger hook が process を exit させ、TCP EOF と session
state の喪失になる。

今回の目的は、その **request 内** の経路だけを次へ変えることである。

```text
unhandled condition → INVOKE-DEBUGGER → diagnostic snapshot
  → private same-thread THROW → structured request failure
  → worker survives
```

これは対話 debugger、restart 選択、状態 rollback を実装する変更ではない。失敗した
処理の部分的な副作用は残り得る。`sb-ext:exit`、FFI crash、強制 kill、任意の
user-created thread、watchdog 等の request 外 thread は対象外のままにする。

SBCL 2.5.10 では `INVOKE-DEBUGGER` が ANSI `*DEBUGGER-HOOK*` より先に
`SB-EXT:*INVOKE-DEBUGGER-HOOK*` を呼び、`DISABLE-DEBUGGER` は後者を disabled
hook に設定する。したがって boundary は前者ではなく後者を request-local に
束縛する。[SBCL `debug.lisp`](https://github.com/sbcl/sbcl/blob/sbcl-2.5.10/src/code/debug.lisp#L1267-L1273)

## 2. 適用範囲と不変条件

- `%dispatch-request` が認証済み request の handler 実行を boundary の動的範囲に
  入れる。worker 起動時の global hook は変更しない。
- request 外では既存の disabled debugger 方針を保つ。
- `call-with-deadline-thread` は shared utility のため、全 call site に policy を
  強制しない。dispatch が request-boundary-active を明示的に束縛した場合だけ、
  utility がその値を lexical に捕捉して child thread 内へ持ち込む。
- child thread は親の catch tag や hook closure を共有しない。各実行 thread が
  自身の catch tag、hook、diagnostic snapshot を持つ。
- `SIGNAL`、muffle された warning、user handler/restart により解決済みの error は
  debugger path に来ないため介入しない。unhandled warning が debugger path に
  来た場合だけ request failure になる。
- 既存の `repl-eval` にある通常の `ERROR` capture とその error_context 経路は
  変えない。boundary はそこを抜けて debugger に至る場合だけ動く。
- deadline の private THROW、cancel、`UNWIND-PROTECT` cleanup、leaked-thread
  accounting は既存どおりである。boundary は cleanup が停止しない場合の即時安全な
  中断を保証しない。

## 3. same-thread boundary

小さな internal utility を追加する。これは対話 debugger 用の基盤ではなく、request
escape を outcome として返すためだけの SBCL-specific utility である。

```text
with request boundary on execution thread:
  establish fresh private catch tag
  dynamically bind SB-EXT:*INVOKE-DEBUGGER-HOOK*
  execute thunk

hook(condition, previous-hook):
  safely snapshot original condition, frames, restarts
  THROW the private tag with the snapshot

after catch (hook binding has unwound):
  return either normal multiple values or a private escape outcome
```

hook から `ERROR` を再 signal しない。catch は user code より外側に置き、hook 自身は
private tag への non-local exit だけを行う。そのため user の `ERROR` handler が
internal wrapper を捕捉して実行を成功へ変換することも、wrapper が同じ hook を再入して
無限再帰することもない。boundary は ABORT / CONTINUE を含む restart を選択しない。

catch の外側で、escape outcome を既存の error result へ接続するための private
`ERROR` subtype を materialize してよい。ただしそれは hook からの脱出とは別段階で
あり、元 condition、snapshot、safe type/message を保持する。通常の返り値は
multiple-value list として運び、escape と混同しない。

## 4. 診断 snapshot と応答変換

snapshot は hook 内、すなわち original dynamic context がまだ残る時点で採る。
`capture-error-context` / frame-inspector を既存の frame・print・preview 上限で使い、
少なくとも次を保持する。

- original condition type と安全に取得した message
- frames、取得できる source location と locals
- restart の name と description

restart は response 時点で live ではない診断 snapshot であり、invoke API は作らない。
condition の `:report`、printer、frame/restart inspection が二次 condition を起こしても
worker を落とさないよう、**診断処理だけ**を局所的な `(handler-case ... (condition ...))`
で保護する。失敗時は type と固定の縮退 message、空の frames/restarts に落とす。これは
user execution 全体を `(condition ...)` で捕捉するものではない。

変換先は既存 wire 形式に限る。

| 経路 | 変換先 |
|---|---|
| `worker/eval` / `repl-eval` deadline thread | 既存の error_context。snapshot の original type/message/frames/restarts を使う。 |
| `worker/run-tests` deadline thread | existing error path。成功や 0 件成功にはしない。 |
| deadline を使わない worker handler | existing `-32603` path。少なくとも original type/message を message に残す。 |

全 tool の schema を統一しない。新しい JSON-RPC code、MCP tool、debug policy option は
作らない。

## 5. 実装面

想定する最小変更面は次のとおりである。

1. request-local SBCL debugger boundary と snapshot/escape outcome を置く internal
   utility。
2. `src/worker/server.lisp` の authenticated handler dispatch。正常 result、existing
   serious-condition result、debugger escape result を区別して既存 response にする。
3. `src/utils/deadline.lisp`。request-boundary-active が明示されている時だけ child
   thread 内に独立 boundary を作り、escape を既存 `:error` outcome にする。
4. `src/repl-core.lisp`。deadline から返った private escape の pre-unwind snapshot を
   error_context として返す。
5. focused tests と必要最小限の user documentation。worker が生きても副作用を
   rollback しないこと、restart は snapshot で後から invoke できないことを記す。

`run-tests`、load、spec adapter 等の deadline 利用は request-active でない限り挙動を
変えない。長時間 timeout の原因調査や lifecycle log の拡張は別 work item とする。
SBCL 以外ではこの hook boundary を有効化せず、既存の挙動を保つ。

## 6. 検証基準

- fresh pooled worker で direct `CONDITION` と `SIMPLE-CONDITION` を `ERROR` に渡しても
  EOF/crash ではなく即時の structured failure になり、PID、事前 state、後続 eval が
  保たれる。
- fixture の original condition、user frame、user restart が repl error_context に
  snapshot として現れる。` :report` が失敗しても縮退 failure を返す。
- `signal`、warning + `muffle-warning`、user handler-case/handler-bind、user restart
  recovery、通常の `ERROR` response は退行しない。internal escape wrapper を user
  `ERROR` handler が成功へ変換できないことも確認する。
- dispatch thread、repl deadline thread、run-tests execution、explicit
  `INVOKE-DEBUGGER` を区別して検証する。Rove が通常 failure にするものと debugger
  に達するものも別 test にする。
- timeout/cancel/leaked-thread 既存 tests と、isolated subprocess での `sb-ext:exit`
  EOF → crash recovery → reaper を回帰確認する。
