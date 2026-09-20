# Worker debugger observe-and-abort — 設計

- 日付: 2026-09-21
- 対象: pooled SBCL worker の request 実行境界
- 状態: レビュー指摘を反映済み。実装計画と production code はこの改訂版の承認後に作成する。

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
  utility がその値を lexical に捕捉する。
- child thread は、捕捉した policy をその child の実行全体に**動的に再束縛**する。
  したがって cl-mcp がその child からさらに deadline thread を作っても policy は
  伝播する。共有するのは boolean policy だけであり、catch tag、hook closure、
  exit state、snapshot は各 thread が新たに作る。任意の user-created thread へは
  伝播しない。
- `SIGNAL`、muffle された warning、user handler/restart により解決済みの error は
  debugger path に来ないため介入しない。unhandled warning が debugger path に
  来た場合だけ request failure になる。
- 既存の `repl-eval` にある通常の `ERROR` capture とその error_context 経路は
  変えない。boundary はそこを抜けて debugger に至る場合だけ動く。
- deadline の private THROW、cancel、`UNWIND-PROTECT` cleanup、leaked-thread
  accounting は既存どおりである。boundary は cleanup が停止しない場合の即時安全な
  中断を保証しない。
- deadline/cancel による controlled unwind が既に始まった後は、その outcome が
  cleanup 中の debugger 到達より優先する。通常実行中の debugger escape と、その
  escape の cleanup 中の二次 debugger は error outcome に留め、成功へ戻さない。

## 3. same-thread boundary

小さな internal utility を追加する。これは対話 debugger 用の基盤ではなく、request
escape を outcome として返すためだけの SBCL-specific utility である。

deadline を持つ execution thread の dynamic layout は、内側 tag への再脱出を避ける
ため次に固定する。deadline を持たない dispatch thread では deadline tag だけを省く。

```text
terminal tag                         ; hook binding の外側
  request-local *INVOKE-DEBUGGER-HOOK*
    debugger-escape tag              ; normal execution の escape 先
      deadline/cancel tag             ; existing controlled unwind の escape 先
        user thunk
```

thread-local exit state は `:running`、`:debugger-unwinding`、
`:deadline-unwinding` のいずれかである。deadline/cancel interrupt は THROW の直前に
後者を設定し、primary hook は debugger tag へ THROW する直前に前者を設定する。

```text
primary hook(condition, previous-hook), when state = :running:
  make a non-printing minimal record with a fixed fallback message
  install a secondary hook before rendering or inspecting the condition
  snapshot original condition, frames, and restarts
  set state to :debugger-unwinding
  THROW debugger-escape tag with a private escape outcome

secondary hook(diagnostic-condition, ignored), during capture only:
  do not capture, print, or invoke a restart
  mark the original snapshot degraded
  set state to :debugger-unwinding
  THROW debugger-escape tag with the minimal original record

primary hook(condition, ignored), when state is no longer :running:
  do not capture again
  THROW terminal tag with a cleanup-reentry marker
```

SBCL temporarily binds the hook being called to `NIL`; the secondary hook is
therefore separately bound only while diagnostics run. It handles direct
`INVOKE-DEBUGGER` from a condition report or inspector without calling either
the disabled hook or the primary hook recursively. `previous-hook` is ignored:
for this call it is the hook value just before SBCL's temporary `NIL` binding,
not a handle for the disabled debugger policy.

hook から `ERROR` を re-signal しない。catch は user code より外側に置き、hook 自身は
private tag への non-local exit だけを行う。そのため user の `ERROR` handler が
internal wrapper を捕捉して実行を成功へ変換することも、wrapper が同じ hook を再入して
無限再帰することもない。boundary は ABORT / CONTINUE を含む restart を選択しない。

deadline/cancel の unwind 中、または debugger escape が cleanup を unwind 中に
debugger が再到達した場合、debugger tag には THROW しない。どちらもまだ外側で有効な
terminal tag へ escape する。前者は既存 deadline/cancel outcome、後者は最初に採った
debugger error outcome を保つ。cleanup の残りを完遂できるとは保証しないが、無効化中の
inner tag へ戻る undefined transfer や worker exit にはしない。
[CLHS `UNWIND-PROTECT`](https://www.lispworks.com/documentation/HyperSpec/Body/s_unwind.htm)
が示す、unwind 中に既に通過対象となった inner catch への再脱出は採用しない。

`call-with-deadline-thread` 自身の cooperative deadline/cancel が inner tag を
unwind し始める前には、既存の completion-wins 判定を済ませた上で、thread-local な
pending deadline outcome を既存の interrupt 制御下に記録する。terminal catch はその
state を見て、cleanup reentry を deadline outcome に縮退させる。通常の debugger escape
では pending debugger record を同様に記録する。したがって outcome の優先順位は次である。

1. 既に開始済みの `call-with-deadline-thread` の deadline/cancel は既存の
   timeout/cancel outcome を保つ。
2. 通常実行中の debugger 到達は snapshot 付き `:error` になる。
3. その debugger escape の cleanup 中の再到達は、最初の `:error` snapshot を保つ。

既存 utility が normal result を返す controlled deadline に限り、この outcome を既存の
result publication と同じ interrupt 制御下で公開する。外側からの任意の non-local exit を
新しい debugger error response に変換することはしない。いずれの local escape も結果格納を
飛び越えて "thread exited without a result" にはならない。通常の返り値は multiple-value
list として運び、`(values)`、`(values nil)`、複数値を escape と混同しない。

escape outcome を既存の error result へ接続するための private `ERROR` subtype は、
hook/catch を抜けた後に materialize してよい。元 condition と snapshot を内部保持するが、
その `:report` は保存済みの safe type/message だけを使い、元 condition を再び `~A` や
printer に渡さない。

## 4. 診断 snapshot と応答変換

snapshot は hook 内、すなわち original dynamic context がまだ残る時点で採る。
`capture-error-context` / frame-inspector を既存の frame・print・preview 上限で使い、
少なくとも次を保持する。

- original condition type と安全に取得した message
- frames、取得できる source location と locals
- restart の name と description

restart は response 時点で live ではない診断 snapshot であり、invoke API は作らない。

採取開始前に、元 condition の non-printing type name（安全に得られる場合）と固定の
fallback message から minimal original record を作る。message の render、frame/restart
inspection、locals preview はその後に行う。これにより、report/printer が壊れても
secondary hook が返す縮退結果には元 condition を指す安全な record が残る。

診断採取だけには二段の保護を置く。

1. 通常に signal された二次 condition は、採取関数を囲む局所的な
   `(handler-case ... (condition ...))` で縮退させる。
2. `:report`、printer、frame/restart inspection が**直接** `INVOKE-DEBUGGER` した場合は
   condition handler search を通らないため、採取中だけ bound する secondary
   `SB-EXT:*INVOKE-DEBUGGER-HOOK*` が受ける。secondary hook は snapshot の再試行、
   printing、restart invocation を一切せず、minimal original record と degraded marker を
   private debugger escape に渡す。

これは `INVOKE-DEBUGGER` が debugger hook を直接呼ぶためであり、局所的な
`handler-case` だけでは代替できない。
[CLHS `INVOKE-DEBUGGER`](https://www.lispworks.com/documentation/HyperSpec/Body/f_invoke.htm)

この保護は user execution 全体を `(condition ...)` で捕捉するものではない。診断失敗後も
元 condition の type/message を再度 `~A`、`PRINC-TO-STRING`、condition printer に渡さない。
private error object を既存経路に materialize する場合も、その `:report` は保存済みの safe
type/message だけで完結する。

変換先は既存 wire 形式に限る。

| 経路 | 変換先 |
|---|---|
| `worker/eval` / `repl-eval` deadline thread | 既存 five-value failure と `error_context`。private escape を先に識別し、pre-unwind snapshot の original type/message/frames/restarts を使う。 |
| `worker/run-tests` deadline thread | 既存 test-runner error path。成功や 0 件成功にはしない。 |
| system loader の `mcp-load-system` deadline | 既存 load failure payload。safe presentation が original record を示す。 |
| spec entry の `mcp-spec-read` deadline | 既存 `:internal-error` response。safe presentation だけを使う。 |
| spec adapter report の `mcp-spec-check` deadline | 既存 condition/internal-error result。safe presentation だけを使う。 |
| deadline を使わない worker handler | 既存 `-32603` path。少なくとも original type/message を message に残す。 |

deadline/cancel cleanup 中の reentry は、上記の deadline/cancel outcome が優先するため、
debugger diagnostic を新しい response field に載せない。通常実行中または debugger escape
cleanup 中の reentry だけが debugger failure record を返す。全 tool の schema を統一しない。
新しい JSON-RPC code、MCP tool、debug policy option は作らない。

## 5. 実装面

想定する最小変更面は次のとおりである。

1. `src/utils/deadline.lisp` と必要最小限の private helper に、request-active policy、
   per-thread boundary、snapshot、private execution outcome を置く。policy が true の時だけ
   child thread に独立した terminal/hook/debugger/deadline boundary を作り、child 内で policy
   を動的に再束縛する。policy が false の inline path と request 外 deadline call site の
   挙動は変えない。
2. `src/worker/server.lisp` の authenticated handler dispatch で request-active policy を
   動的に束縛し、deadline を使わない handler 用にも same-thread boundary を置く。normal
   result、existing serious-condition result、private debugger escape を既存 response に
   分岐接続する。
3. `src/utils/deadline.lisp` の deadline interrupt/result-publication を、pending deadline
   outcome と private execution outcome を区別できるようにする。completion-wins、
   `UNWIND-PROTECT` cleanup、leaked-thread accounting、timeout/cancel の公開契約は維持する。
4. `src/repl-core.lisp` で deadline の private debugger escape を `%thunk-error-result` より
   前に認識し、saved pre-unwind snapshot を既存 `error_context` へ接続する。
5. `src/system-loader-core.lisp`、`src/tools/spec-entry.lisp`、
   `src/spec-adapter-report.lisp`、必要なら test runner の既存 `:error` consumer を確認し、
   private diagnostic object を表示する箇所が original condition を再 print しないよう
   safe presentation に接続する。各 tool の schema は広げない。
6. focused regression tests と必要最小限の user documentation を追加する。worker が生きても
   副作用を rollback しないこと、restart は snapshot であり後から invoke できないことを
   記す。

`run-tests`、load、spec adapter 等の deadline 利用は request-active でない限り挙動を
変えない。長時間 timeout の原因調査や lifecycle log の拡張は別 work item とする。
SBCL 以外ではこの hook boundary を有効化せず、既存の挙動を保つ。

## 6. 検証基準

### A. pooled worker の最小再現

- fresh pooled worker で direct `CONDITION` と direct `SIMPLE-CONDITION` を別の型名で
  `ERROR` に渡す。EOF/crash や deadline 待ちではなく即時 structured failure になり、
  request 前後の PID、事前 state、後続 eval が保たれることを確認する。
- 失敗は worker crash と分類されず、TCP EOF を伴わないことを確認する。

### B. unwind 前の診断と診断失敗

- identifier を持つ user function、original condition、user restart を fixture に置き、
  repl error_context に original type/message、ユーザ frame、restart snapshot が残ることを
  確認する。取得できない locals は推測しない。
- `:report` が通常の二次 `ERROR` を signal する fixture と、直接
  `INVOKE-DEBUGGER` する fixture を別々に置く。いずれも worker を終了させず、元 condition
  の safe type/message を保った degraded failure になることを確認する。
- private diagnostic object の表示が保存済み文字列だけで済み、元 condition の壊れた
  printer を response builder が再実行しないことを確認する。

### C. condition と正常 return の非回帰

- `signal`、warning + `muffle-warning`、user `handler-case` / `handler-bind` が処理する
  `ERROR`、user restart recovery、既存の通常 `ERROR` response を確認する。
- original condition に一致しない内側の `ERROR` handler が、private escape を捕捉して成功へ
  変換できないことを確認する。
- `(values)`、`(values nil)`、複数値が normal result のままで、escape outcome と混同
  されないことを確認する。

### D. 実行経路と policy 伝播

- deadline を使わない dispatch thread、repl-eval deadline thread、run-tests execution、
  explicit `INVOKE-DEBUGGER` を別々に確認する。Rove が通常 test failure にする例と、そこを
  抜けて debugger に達する例も分ける。
- dispatch → deadline child → nested deadline child で request-active policy が伝播し、
  tag/hook/snapshot は thread ごとに異なることを確認する。request-active が false の
  deadline 利用には boundary を追加しないことも確認する。

### E. cleanup、deadline、実プロセス終了

- normal debugger escape により user `UNWIND-PROTECT` cleanup が走ることを確認する。
  その cleanup が再び debugger に到達しても、最初の debugger error snapshot を保ち、
  無限再入・無効 tag への transfer・worker exit を起こさないことを確認する。
- deadline/cancel の controlled unwind 中に cleanup が debugger に到達する fixture では、
  定義済みの deadline/cancel outcome を保ち、worker を終了させないことを確認する。
- 既存 timeout/cancel/leaked-thread tests を回帰し、debugger failure に誤分類しないことを
  確認する。`sb-ext:exit` は isolated subprocess で EOF → crash recovery → reaper の従来経路を
  維持することを確認する。
