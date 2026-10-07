# カバレッジサマリ

`scripts/coverage.ros` が生成する。手で編集しないこと。

- SBCL: 2.5.0.roswell
- 計測コミット: a37a4fc
- 計測ファイル数: 84
- 式カバレッジ合計: 47144/54157 (87.1%)
- 分岐カバレッジ合計: 5144/6312 (81.5%)

ワーカー子プロセス内の実行は計測できない。この表に出ない、0% の、あるいは低い数値のファイルが未テストとは限らない。特に `src/lisp-read-file.lisp` はテスト中盤の生 LOAD再ロードで計装が外れる既知の計測アーティファクトがあり、実際のテストは厚い。判断の前に `scripts/coverage-scope.md` を読むこと。

| ファイル | 式 | 式% | 分岐 | 分岐% |
|---|---|---|---|---|
| src/tools/all.lisp | 0/2 | 0.0 | 0/0 | N/A |
| src/project-scaffold-templates.lisp | 0/11 | 0.0 | 0/0 | N/A |
| src/yason-compat.lisp | 0/17 | 0.0 | 0/2 | 0.0 |
| src/system-loader.lisp | 1/38 | 2.6 | 0/4 | 0.0 |
| src/lisp-read-file.lisp | 55/1188 | 4.6 | 10/166 | 6.0 |
| src/worker/main.lisp | 44/267 | 16.5 | 2/16 | 12.5 |
| src/state.lisp | 2/6 | 33.3 | 0/0 | N/A |
| src/core.lisp | 2/6 | 33.3 | 0/0 | N/A |
| src/repl.lisp | 49/98 | 50.0 | 3/12 | 25.0 |
| src/utils/request-debugger-boundary-protocol.lisp | 19/34 | 55.9 | 0/0 | N/A |
| src/server-instructions.lisp | 6/10 | 60.0 | 0/0 | N/A |
| src/run.lisp | 111/184 | 60.3 | 12/18 | 66.7 |
| src/utils/system.lisp | 5/8 | 62.5 | 0/0 | N/A |
| src/clos.lisp | 31/49 | 63.3 | 5/6 | 83.3 |
| src/test-runner.lisp | 27/42 | 64.3 | 0/0 | N/A |
| src/tcp.lisp | 319/470 | 67.9 | 17/32 | 53.1 |
| src/utils/printing.lisp | 14/20 | 70.0 | 0/0 | N/A |
| src/http.lisp | 594/831 | 71.5 | 69/98 | 70.4 |
| src/request-lifecycle.lisp | 86/119 | 72.3 | 8/8 | 100.0 |
| src/asdf-tools.lisp | 76/104 | 73.1 | 1/2 | 50.0 |
| src/utils/lenient-read.lisp | 225/307 | 73.3 | 24/46 | 52.2 |
| src/log.lisp | 129/174 | 74.1 | 5/16 | 31.3 |
| src/reset-events.lisp | 107/139 | 77.0 | 10/10 | 100.0 |
| src/protocol.lisp | 542/704 | 77.0 | 61/84 | 72.6 |
| src/project-root.lisp | 89/114 | 78.1 | 6/8 | 75.0 |
| src/tools/spec-tools.lisp | 72/92 | 78.3 | 0/0 | N/A |
| src/worker/handlers.lisp | 361/451 | 80.0 | 9/26 | 34.6 |
| src/test-runner-core.lisp | 2684/3339 | 80.4 | 267/382 | 69.9 |
| src/debugger-preview.lisp | 322/400 | 80.5 | 31/44 | 70.5 |
| src/spec-adapter-core.lisp | 648/777 | 83.4 | 50/68 | 73.5 |
| src/worker-client.lisp | 1107/1317 | 84.1 | 111/146 | 76.0 |
| src/lisp-patch-form.lisp | 514/609 | 84.4 | 41/54 | 75.9 |
| src/clhs.lisp | 414/490 | 84.5 | 42/60 | 70.0 |
| src/project-scaffold.lisp | 377/445 | 84.7 | 18/34 | 52.9 |
| src/worker/server.lisp | 361/425 | 84.9 | 28/36 | 77.8 |
| src/utils/hash.lisp | 23/27 | 85.2 | 2/2 | 100.0 |
| src/utils/strings.lisp | 18/21 | 85.7 | 4/4 | 100.0 |
| src/pool.lisp | 1746/2031 | 86.0 | 211/244 | 86.5 |
| src/repl-core.lisp | 448/520 | 86.2 | 19/26 | 73.1 |
| src/clos-verify-core.lisp | 907/1045 | 86.8 | 122/154 | 79.2 |
| src/tools/helpers.lisp | 133/153 | 86.9 | 8/8 | 100.0 |
| src/tools/registry.lisp | 141/162 | 87.0 | 22/24 | 91.7 |
| src/inspect.lisp | 1012/1150 | 88.0 | 106/138 | 76.8 |
| src/tools/pool-kill-worker.lisp | 110/124 | 88.7 | 8/8 | 100.0 |
| src/worker/init-hook.lisp | 236/265 | 89.1 | 11/18 | 61.1 |
| src/spec-adapter-report.lisp | 3388/3803 | 89.1 | 298/326 | 91.4 |
| src/system-loader-core.lisp | 616/688 | 89.5 | 52/60 | 86.7 |
| src/utils/random.lisp | 18/20 | 90.0 | 0/0 | N/A |
| src/frame-inspector.lisp | 799/886 | 90.2 | 80/92 | 87.0 |
| src/spec-core-record.lisp | 725/800 | 90.6 | 133/138 | 96.4 |
| src/code-core.lisp | 1640/1806 | 90.8 | 234/300 | 78.0 |
| src/tools/spec-entry.lisp | 287/315 | 91.1 | 23/24 | 95.8 |
| src/validate.lisp | 690/757 | 91.1 | 80/92 | 87.0 |
| src/proxy.lisp | 555/608 | 91.3 | 51/58 | 87.9 |
| src/source-snapshot.lisp | 119/130 | 91.5 | 5/6 | 83.3 |
| src/cst.lisp | 615/668 | 92.1 | 73/90 | 81.1 |
| src/parinfer.lisp | 413/448 | 92.2 | 136/148 | 91.9 |
| src/fs.lisp | 902/976 | 92.4 | 79/98 | 80.6 |
| src/package-context.lisp | 488/526 | 92.8 | 50/66 | 75.8 |
| src/utils/paths.lisp | 528/568 | 93.0 | 47/50 | 94.0 |
| src/utils/clgrep.lisp | 1096/1179 | 93.0 | 171/194 | 88.1 |
| src/utils/request-debugger-boundary.lisp | 179/192 | 93.2 | 3/8 | 37.5 |
| src/utils/deadline.lisp | 200/214 | 93.5 | 20/20 | 100.0 |
| src/clos-core.lisp | 1173/1254 | 93.5 | 125/154 | 81.2 |
| src/object-registry.lisp | 174/186 | 93.5 | 17/18 | 94.4 |
| src/macroexpand-core.lisp | 437/467 | 93.6 | 43/48 | 89.6 |
| src/lisp-edit-form.lisp | 1979/2111 | 93.7 | 232/274 | 84.7 |
| src/lisp-macroexpand.lisp | 558/595 | 93.8 | 56/64 | 87.5 |
| src/lisp-edit-form-core.lisp | 1788/1906 | 93.8 | 273/314 | 86.9 |
| src/tools/spec-response-builders.lisp | 3675/3912 | 93.9 | 228/246 | 92.7 |
| src/tools/clos-response-builders.lisp | 1074/1142 | 94.0 | 91/100 | 91.0 |
| src/tools/response-builders.lisp | 1736/1835 | 94.6 | 186/206 | 90.3 |
| src/code-refs-scan.lisp | 1743/1832 | 95.1 | 242/294 | 82.3 |
| src/project-scaffold-core.lisp | 340/357 | 95.2 | 27/28 | 96.4 |
| src/code.lisp | 194/203 | 95.6 | 11/14 | 78.6 |
| src/tools/pool-status.lisp | 59/61 | 96.7 | 8/8 | 100.0 |
| src/code-refs-core.lisp | 1198/1237 | 96.8 | 135/146 | 92.5 |
| src/clgrep.lisp | 232/239 | 97.1 | 15/18 | 83.3 |
| src/paren-diagnostics.lisp | 2269/2337 | 97.1 | 363/384 | 94.5 |
| src/utils/sanitize.lisp | 284/292 | 97.3 | 76/78 | 97.4 |
| src/utils/bounded-stream.lisp | 196/201 | 97.5 | 22/22 | 100.0 |
| src/utils/nesting.lisp | 308/315 | 97.8 | 93/96 | 96.9 |
| src/utils/fasls.lisp | 147/149 | 98.7 | 17/20 | 85.0 |
| src/tools/define-tool.lisp | 155/157 | 98.7 | 6/6 | 100.0 |

## 実行済み式カウントが 0 のファイル

式ok が 0 は「一度も実行されなかった」ことを意味しない。defpackage・in-package 等の定義系トップレベルフォームは、そのファイルが計測プロセス内でコンパイルされる限り実行済みとして計上されない仕様であり、以下のファイルはロード時に実際には実行されている（詳細は `scripts/coverage-scope.md`）。

- `src/tools/all.lisp` (式 0/2)
- `src/project-scaffold-templates.lisp` (式 0/11)
- `src/yason-compat.lisp` (式 0/17)
