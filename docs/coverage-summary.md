# カバレッジサマリ

`scripts/coverage.ros` が生成する。手で編集しないこと。

- SBCL: 2.5.8.roswell
- 計測コミット: 24f0b61
- 計測ファイル数: 59
- 式カバレッジ合計: 18370/23915 (76.8%)
- 分岐カバレッジ合計: 1764/2592 (68.1%)

ワーカー子プロセス内の実行は計測できない。この表に出ない、0% の、あるいは低い数値のファイルが未テストとは限らない。特に `src/lisp-read-file.lisp` はテスト中盤の生 LOAD再ロードで計装が外れる既知の計測アーティファクトがあり、実際のテストは厚い。判断の前に `scripts/coverage-scope.md` を読むこと。

| ファイル | 式 | 式% | 分岐 | 分岐% |
|---|---|---|---|---|
| src/tools/all.lisp | 0/2 | 0.0 | 0/0 | N/A |
| src/project-scaffold-templates.lisp | 0/11 | 0.0 | 0/0 | N/A |
| src/system-loader.lisp | 1/38 | 2.6 | 0/4 | 0.0 |
| src/lisp-read-file.lisp | 55/1078 | 5.1 | 10/148 | 6.8 |
| src/worker/main.lisp | 46/264 | 17.4 | 2/16 | 12.5 |
| src/state.lisp | 2/6 | 33.3 | 0/0 | N/A |
| src/core.lisp | 2/6 | 33.3 | 0/0 | N/A |
| src/test-runner.lisp | 19/55 | 34.5 | 0/0 | N/A |
| src/run.lisp | 47/114 | 41.2 | 3/8 | 37.5 |
| src/utils/printing.lisp | 9/20 | 45.0 | 0/0 | N/A |
| src/repl.lisp | 41/72 | 56.9 | 0/0 | N/A |
| src/http.lisp | 411/664 | 61.9 | 35/62 | 56.5 |
| src/utils/system.lisp | 5/8 | 62.5 | 0/0 | N/A |
| src/tcp.lisp | 302/453 | 66.7 | 16/32 | 50.0 |
| src/proxy.lisp | 326/484 | 67.4 | 38/54 | 70.4 |
| src/test-runner-core.lisp | 1587/2258 | 70.3 | 130/218 | 59.6 |
| src/code.lisp | 46/65 | 70.8 | 0/0 | N/A |
| src/utils/lenient-read.lisp | 226/308 | 73.4 | 24/46 | 52.2 |
| src/tools/registry.lisp | 28/38 | 73.7 | 0/2 | 0.0 |
| src/project-root.lisp | 47/63 | 74.6 | 4/6 | 66.7 |
| src/log.lisp | 134/179 | 74.9 | 4/16 | 25.0 |
| src/protocol.lisp | 469/626 | 74.9 | 55/78 | 70.5 |
| src/worker-client.lisp | 762/1017 | 74.9 | 54/90 | 60.0 |
| src/worker/server.lisp | 263/349 | 75.4 | 18/28 | 64.3 |
| src/asdf-tools.lisp | 86/114 | 75.4 | 1/2 | 50.0 |
| src/tools/pool-status.lisp | 46/60 | 76.7 | 7/8 | 87.5 |
| src/pool.lisp | 1363/1736 | 78.5 | 144/198 | 72.7 |
| src/lisp-edit-form.lisp | 709/902 | 78.6 | 62/86 | 72.1 |
| src/cst.lisp | 283/357 | 79.3 | 29/42 | 69.0 |
| src/worker/handlers.lisp | 301/377 | 79.8 | 7/12 | 58.3 |
| src/lisp-edit-form-core.lisp | 396/490 | 80.8 | 62/84 | 73.8 |
| src/code-core.lisp | 657/803 | 81.8 | 88/144 | 61.1 |
| src/system-loader-core.lisp | 382/464 | 82.3 | 26/34 | 76.5 |
| src/tools/helpers.lisp | 128/153 | 83.7 | 8/8 | 100.0 |
| src/project-scaffold.lisp | 371/440 | 84.3 | 18/34 | 52.9 |
| src/clhs.lisp | 417/493 | 84.6 | 42/60 | 70.0 |
| src/frame-inspector.lisp | 382/451 | 84.7 | 32/38 | 84.2 |
| src/lisp-patch-form.lisp | 338/397 | 85.1 | 16/26 | 61.5 |
| src/validate.lisp | 659/771 | 85.5 | 73/98 | 74.5 |
| src/utils/strings.lisp | 18/21 | 85.7 | 4/4 | 100.0 |
| src/repl-core.lisp | 410/478 | 85.8 | 19/26 | 73.1 |
| src/parinfer.lisp | 236/274 | 86.1 | 65/74 | 87.8 |
| src/utils/hash.lisp | 25/29 | 86.2 | 2/2 | 100.0 |
| src/inspect.lisp | 881/1016 | 86.7 | 86/116 | 74.1 |
| src/tools/response-builders.lisp | 784/894 | 87.7 | 69/96 | 71.9 |
| src/tools/pool-kill-worker.lisp | 90/102 | 88.2 | 8/8 | 100.0 |
| src/utils/random.lisp | 18/20 | 90.0 | 0/0 | N/A |
| src/object-registry.lisp | 100/111 | 90.1 | 7/8 | 87.5 |
| src/fs.lisp | 560/621 | 90.2 | 40/52 | 76.9 |
| src/worker/init-hook.lisp | 202/223 | 90.6 | 8/14 | 57.1 |
| src/utils/paths.lisp | 249/273 | 91.2 | 28/30 | 93.3 |
| src/utils/clgrep.lisp | 1059/1142 | 92.7 | 165/188 | 87.8 |
| src/package-context.lisp | 463/496 | 93.3 | 45/60 | 75.0 |
| src/macroexpand-core.lisp | 439/469 | 93.6 | 43/48 | 89.6 |
| src/utils/sanitize.lisp | 247/263 | 93.9 | 69/74 | 93.2 |
| src/lisp-macroexpand.lisp | 559/587 | 95.2 | 56/64 | 87.5 |
| src/project-scaffold-core.lisp | 340/357 | 95.2 | 27/28 | 96.4 |
| src/clgrep.lisp | 189/196 | 96.4 | 9/12 | 75.0 |
| src/tools/define-tool.lisp | 155/157 | 98.7 | 6/6 | 100.0 |

## 実行済み式カウントが 0 のファイル

式ok が 0 は「一度も実行されなかった」ことを意味しない。defpackage・in-package 等の定義系トップレベルフォームは、そのファイルが計測プロセス内でコンパイルされる限り実行済みとして計上されない仕様であり、以下のファイルはロード時に実際には実行されている（詳細は `scripts/coverage-scope.md`）。

- `src/tools/all.lisp` (式 0/2)
- `src/project-scaffold-templates.lisp` (式 0/11)
