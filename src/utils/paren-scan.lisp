;;;; src/utils/paren-scan.lisp
;;;;
;;;; Lisp テキストを文字単位で走査し、括弧の釣り合いと、標準リーダーが再帰的に
;;;; 処理する構文全体のネスト深さを調べる。
;;;;
;;;; 括弧 ( [ { } ] ) に加えて、クォート系のプレフィックス構文 ' ` , ,@ #' も
;;;; 深さに数える。標準リーダーはこれらに出会うたびに次のフォームを読むために
;;;; 自身を再帰呼び出しするため、括弧だけを数えると実際の再帰深度を過小評価
;;;; してしまう。プレフィックス構文には対応する閉じ文字がなく、次のフォーム
;;;; （アトム・文字列・文字リテラル、あるいは開き括弧なら対応する閉じ括弧）を
;;;; 読み終えた時点で再帰が戻るため、その時点でまとめて深さを戻す。
;;;;
;;;; 文字列リテラル・文字リテラル・行コメント・ネストしたブロックコメントの
;;;; 中の括弧・プレフィックス文字は数えない。素朴な括弧カウントでは "((((("
;;;; を含む正当なファイルを誤って弾くため、この区別に意味がある。
;;;;
;;;; src/validate.lisp から移設。validate は fs と tools/* に依存するツール層の
;;;; モジュールで、低レベルの src/cst.lisp から参照させたくないため、双方の共通
;;;; 土台としてここに置く。

(defpackage #:cl-mcp/src/utils/paren-scan
  (:use #:cl)
  (:export #:scan-parens
           #:*max-nesting-depth*))

(in-package #:cl-mcp/src/utils/paren-scan)

(defparameter *max-nesting-depth* 300
  "1 つのフォームに許すネストの深さの上限。

これを超える入力は、リーダーに渡す前に拒否する。深いネストは Eclector CST 経路でも
標準 CL リーダー経路でも再帰で処理され、到達すれば SBCL の制御スタックを枯渇させる。
枯渇は捕捉に頼れない（src/macroexpand-core.lisp の *max-walk-expansions* に同じ実測が
記録されている）ので、届かせないことだけが効く。

この値は Task 1 で実測した破綻深度（Eclector CST 経路で深度 1750 は生存、1875 で
制御スタック枯渇により死亡と確認、境界はこの間のどこか）のはるか下、このリポジトリの
実コードの最大ネスト深さ 20（src/proxy.lisp、src/pool.lisp）のはるか上に置いてある。")

(defun %closing (opener)
  (ecase opener
    (#\( #\))
    (#\[ #\])
    (#\{ #\})))

(defun %scan-parens-push-open (stack line col base-offset ch idx)
  (cons (list ch line col (+ base-offset idx)) stack))

(defun %scan-parens-pop-open (stack line col base-offset ch idx)
  (if (null stack)
      (values stack
              (list :ok nil
                    :kind "extra-close"
                    :expected nil
                    :found (string ch)
                    :offset (+ base-offset idx)
                    :line line
                    :column col))
      (destructuring-bind (top-ch top-line top-col top-off) (car stack)
        (declare (ignore top-line top-col top-off))
        (let ((expected (%closing top-ch)))
          (if (char= expected ch)
              (values (cdr stack) nil)
              (values stack
                      (list :ok nil
                            :kind "mismatch"
                            :expected (string expected)
                            :found (string ch)
                            :offset (+ base-offset idx)
                            :line line
                            :column col)))))))

(defstruct scan-state
  (line 1 :type fixnum)
  (col 1 :type fixnum)
  (stack '() :type list)
  (in-string nil :type boolean)
  (escape nil :type boolean)
  (line-comment nil :type boolean)
  (block-depth 0 :type fixnum)
  (block-open-pos 0 :type fixnum)
  (depth 0 :type fixnum)
  (max-depth 0 :type fixnum))

(defun %scan-handle-line-comment (state ch)
  (when (char= ch #\Newline)
    (setf (scan-state-line-comment state) nil)))

(defun %scan-handle-string (state ch)
  (cond
    ((scan-state-escape state)
     (setf (scan-state-escape state) nil))
    ((char= ch #\\)
     (setf (scan-state-escape state) t))
    ((char= ch #\")
     (setf (scan-state-in-string state) nil))))

(defun %scan-handle-block-comment (state ch next)
  (when (and (char= ch #\|) next (char= next #\#))
    (decf (scan-state-block-depth state))
    t))

(defun %scan-whitespace-char-p (ch)
  "Return T if CH is whitespace under the standard CL reader."
  (member ch '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page) :test #'char=))

(defun %scan-push-prefix (state)
  "Record one level of a prefix reader macro (', `, ,, ,@, or #') on
STATE's stack. These have no closing delimiter of their own -- the depth
they add is unwound by %SCAN-RESOLVE-PENDING-PREFIXES once the form they
prefix has been read."
  (push :prefix (scan-state-stack state))
  (incf (scan-state-depth state))
  (setf (scan-state-max-depth state)
          (max (scan-state-max-depth state) (scan-state-depth state))))

(defun %scan-resolve-pending-prefixes (state)
  "Pop any prefix-macro markers now on top of STATE's stack, decrementing
DEPTH for each. A prefix macro's reader recursion returns as soon as the
form it prefixes has been read, so its extra depth must unwind at that
point instead of persisting into sibling forms -- otherwise depth would
drift upward across ordinary files and never recover."
  (loop while (eq :prefix (car (scan-state-stack state)))
        do (pop (scan-state-stack state))
           (decf (scan-state-depth state))))

(defun %scan-handle-normal (state ch next idx base-offset text)
  "Handle a character in normal (non-string, non-comment) context.
Returns (VALUES err consumed) where CONSUMED is NIL or a positive integer
indicating how many additional characters past CH were consumed."
  (cond
   ((char= ch #\;) (setf (scan-state-line-comment state) t) (values nil nil))
   ((char= ch #\")
    (%scan-resolve-pending-prefixes state)
    (setf (scan-state-in-string state) t)
    (values nil nil))
   ;; Character literal: #\x or #\Space etc.  Skip past entirely so that
   ;; delimiter characters like #\( are not treated as open-parens.
   ((and (char= ch #\#) next (char= next #\\))
    (%scan-resolve-pending-prefixes state)
    (let ((skip 1))  ; at minimum skip the backslash
      (let ((char-pos (+ idx 2)))
        (when (< char-pos (length text))
          (incf skip)  ; skip the character after backslash
          ;; Named character literals: consume remaining alpha chars
          (when (alpha-char-p (char text char-pos))
            (loop for k from (1+ char-pos) below (length text)
                  while (alpha-char-p (char text k))
                  do (incf skip)))))
      (values nil skip)))
   ((and (char= ch #\#) next (char= next #\|))
    (when (zerop (scan-state-block-depth state))
      (setf (scan-state-block-open-pos state) (+ base-offset idx)))
    (incf (scan-state-block-depth state))
    (values nil 1))
   ;; #' is the FUNCTION reader macro (CLHS 2.4.6): like ', it makes the
   ;; reader recurse once for its operand and has no closing delimiter.
   ((and (char= ch #\#) next (char= next #\'))
    (%scan-push-prefix state)
    (values nil 1))
   ;; Quote and quasiquote (CLHS 2.4.5, 2.4.6): single-character prefix
   ;; macros.  Both make the standard reader call itself for the next
   ;; form; neither has a closing delimiter, so the depth they add is
   ;; unwound by %SCAN-RESOLVE-PENDING-PREFIXES once that form (or, for a
   ;; list, its closing delimiter) is reached.
   ((or (char= ch #\') (char= ch #\`))
    (%scan-push-prefix state)
    (values nil nil))
   ;; Unquote and unquote-splicing (CLHS 2.4.6): ",@" is a single reader
   ;; construct, so the optional "@" is consumed without adding a second
   ;; level.
   ((char= ch #\,)
    (%scan-push-prefix state)
    (if (and next (char= next #\@))
        (values nil 1)
        (values nil nil)))
   ((or (char= ch #\() (char= ch #\[) (char= ch #\{))
    (setf (scan-state-stack state)
            (%scan-parens-push-open (scan-state-stack state)
             (scan-state-line state) (scan-state-col state) base-offset ch
             idx))
    (incf (scan-state-depth state))
    (setf (scan-state-max-depth state)
            (max (scan-state-max-depth state) (scan-state-depth state)))
    (values nil nil))
   ((or (char= ch #\)) (char= ch #\]) (char= ch #\}))
    ;; A lone prefix directly before a closer (e.g. "')") has no operand
    ;; of its own; resolve it first so %SCAN-PARENS-POP-OPEN never sees a
    ;; prefix marker where it expects a bracket frame.
    (%scan-resolve-pending-prefixes state)
    (multiple-value-bind (new-stack err)
        (%scan-parens-pop-open (scan-state-stack state) (scan-state-line state)
         (scan-state-col state) base-offset ch idx)
      (setf (scan-state-stack state) new-stack)
      (unless err
        (decf (scan-state-depth state))
        ;; Closing this bracket also resolves any prefixes that preceded
        ;; it (e.g. the quote in "'(a)"): their recursive reads return
        ;; together with the list's own.
        (%scan-resolve-pending-prefixes state))
      (values err nil)))
   (t
    (unless (%scan-whitespace-char-p ch)
      (%scan-resolve-pending-prefixes state))
    (values nil nil))))

(defun %scan-advance-position (state ch)
  (cond
    ((char= ch #\Newline)
     (incf (scan-state-line state))
     (setf (scan-state-col state) 1))
    (t
     (incf (scan-state-col state)))))

(defun scan-parens (text &key (base-offset 0))
  "Return a plist describing balance of delimiters in TEXT.
Keys: :ok (boolean), :kind (string|nil), :expected, :found, :offset, :line,
:column, :max-depth (fixnum, the deepest the standard reader's recursion
would reach scanning TEXT -- brackets plus the prefix macros ', `, ,, ,@,
and #', which have no closing delimiter of their own -- even along
early-return error paths)."
  (let ((state (make-scan-state))
        (len (length text))
        (idx 0))
    (loop while (< idx len)
          for ch = (char text idx)
          for next = (and (< (1+ idx) len) (char text (1+ idx)))
          do
            (cond
              ((scan-state-line-comment state)
               (%scan-handle-line-comment state ch))
              ((scan-state-in-string state)
               (%scan-handle-string state ch))
              ((plusp (scan-state-block-depth state))
               (when (%scan-handle-block-comment state ch next)
                 (incf idx)
                 (incf (scan-state-col state))))
              (t
               (multiple-value-bind (err consumed)
                   (%scan-handle-normal state ch next idx base-offset text)
                 (when err
                   (return-from scan-parens
                     (append err (list :max-depth (scan-state-max-depth state)))))
                 (when consumed
                   (let ((n (if (integerp consumed) consumed 1)))
                     (incf idx n)
                     (incf (scan-state-col state) n))))))
            (%scan-advance-position state ch)
            (incf idx))
    (when (plusp (scan-state-block-depth state))
      (let* ((open-pos  (scan-state-block-open-pos state))
             (local-pos (- open-pos base-offset))
             (pre       (subseq text 0 (min local-pos (length text))))
             (r-line    (1+ (count #\Newline pre)))
             (col-start (or (position #\Newline pre :from-end t) -1))
             (r-col     (- local-pos col-start)))
        (return-from scan-parens
          (list :ok nil
                :kind "unclosed-block-comment"
                :expected nil
                :found nil
                :offset open-pos
                :line r-line
                :column r-col
                :max-depth (scan-state-max-depth state)))))
    ;; A trailing prefix marker (e.g. an unterminated "'" at end of input)
    ;; has no bracket to match, so it is not an "unclosed" error in the
    ;; sense the check below reports; drop it. The reader would raise
    ;; END-OF-FILE trying to read its operand, which %TRY-READER-CHECK
    ;; already handles.
    (%scan-resolve-pending-prefixes state)
    (when (scan-state-stack state)
      (destructuring-bind (ch l c off) (pop (scan-state-stack state))
        (return-from scan-parens
          (list :ok nil
                :kind "unclosed"
                :expected (string (%closing ch))
                :found nil
                :offset off
                :line l
                :column c
                :max-depth (scan-state-max-depth state)))))
    (list :ok t :max-depth (scan-state-max-depth state))))
