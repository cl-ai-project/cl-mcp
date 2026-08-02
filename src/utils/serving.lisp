;;;; src/utils/serving.lisp
;;;;
;;;; サーバが要求を処理しているスレッドをデバッガに入れないための土台。
;;;;
;;;; 親プロセスはワーカーと違い (sb-ext:disable-debugger) を呼べない。
;;;; cl-mcp:run は本番の入口であると同時にテストが呼ぶ関数でもあり、
;;;; グローバルに無効化するとテストと REPL のデバッガ体験まで壊れる。
;;;; 危険なのは「サーバが処理する要求の中でデバッガに入ること」だけなので、
;;;; 束縛をその範囲に閉じる。

(defpackage #:cl-mcp/src/utils/serving
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:export #:call-without-debugger))

(in-package #:cl-mcp/src/utils/serving)

(defun call-without-debugger (label thunk)
  "THUNK を呼ぶ。デバッガに入る事態になったらログを出して THUNK から脱出する。

デバッガに入ったスレッドはフォアグラウンドを永久に待つ。サーバのスレッドでは
それは応答しない接続とスレッドリークを意味するので、代わりにここまで巻き戻す。

**脱出が要である。** *invoke-debugger-hook* に束縛した関数が非局所脱出せずに
戻ると、戻った先で結局デバッガに入る。CATCH がその脱出先である。"
  (let ((tag (gensym "SERVING")))
    (catch tag
      (let ((sb-ext:*invoke-debugger-hook*
              (lambda (condition hook)
                (declare (ignore hook))
                (log-event :error "serving.debugger-suppressed"
                           "label" label
                           "condition_type" (string (type-of condition))
                           "message" (ignore-errors
                                      (princ-to-string condition)))
                (throw tag :debugger-suppressed))))
        (funcall thunk)))))
