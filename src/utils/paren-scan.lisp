;;;; src/utils/paren-scan.lisp
;;;;
;;;; Lisp テキストを文字単位で走査し、括弧の釣り合いと、標準リーダーが再帰的に
;;;; 処理する構文のネスト深さを調べる。
;;;;
;;;; 括弧 ( [ { } ] ) に加えて、クォート系のプレフィックス構文 ' ` , ,@ #'、
;;;; ラベル定義 #n=、機能条件分岐 #+ #- も深さに数える。標準リーダーはこれら
;;;; に出会うたびに次のフォームを読むために自身を再帰呼び出しするため、括弧
;;;; だけを数えると実際の再帰深度を過小評価してしまう。これらには対応する
;;;; 閉じ文字がなく、次のフォーム（アトム・文字列・文字リテラル、あるいは
;;;; 開き括弧なら対応する閉じ括弧）を読み終えた時点で再帰が戻るため、その
;;;; 時点でまとめて深さを戻す。#+ / #- だけは機能式と被ガード対象の 2 回の
;;;; 独立した再帰読みを行うため、機能式を読み終えても深さを戻さず、被ガード
;;;; 対象の読みへとマーカーを転用する（詳細は %SCAN-RESOLVE-PENDING-PREFIXES
;;;; の docstring）。#n# は既読オブジェクトへの参照のみで再帰読みを伴わない
;;;; ため、扱う必要がない。
;;;;
;;;; この列挙は網羅ではなく、網羅を目指してもいない。*MAX-NESTING-DEPTH* の
;;;; docstring を参照。
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
実コードの最大ネスト深さ 20（src/proxy.lisp、src/pool.lisp）のはるか上に置いてある。

この上限を検査する SCAN-PARENS（src/utils/paren-scan.lisp）は安価な一次防御であり、
網羅的な防御ではない。3 回の監査を経て、標準リーダーを再帰させ、かつ対応する閉じ文字を
持たない構文が 3 系統見つかり、その都度組み込んできた: 括弧・角括弧・波括弧、クォート系
プレフィックス構文（' ` , ,@ #'、および #n= ラベル定義）、機能条件分岐（#+ #-）である。
標準リーダーが再帰し得る構文の列挙は原理的に終わらない――次に見つかる構文が最後である
保証はどこにもなく、この一覧を増やし続けること自体を目標にしてはならない。したがって、
この検査には構文の種類に依存しない保証はない。その保証は別の層が担う：後続タスクが
導入するコネクションごとのデバッガフックであり、たとえ未知の構文がこの一次防御を
すり抜けて制御スタックを枯渇させても、そのフックが捕捉して 1 リクエストの失敗として
処理し、接続スレッドがデバッガに永久に居座ることを防ぐ。SCAN-PARENS はその防御を
安価に発火させずに済ませるための最初の網に過ぎず、最後の網ではない。")

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

(defun %scan-push-feature-expr (state)
  "Record the first (feature-expression) read of a pending #+/#- on
STATE's stack, using a marker distinct from an ordinary prefix so
%SCAN-RESOLVE-PENDING-PREFIXES can convert it into one, rather than
popping it, once the feature-expression's target is reached -- see that
function's docstring for why."
  (push :feature-expr (scan-state-stack state))
  (incf (scan-state-depth state))
  (setf (scan-state-max-depth state)
          (max (scan-state-max-depth state) (scan-state-depth state))))

(defun %scan-resolve-pending-prefixes (state)
  "Pop or convert pending prefix markers now on top of STATE's stack.

An ordinary :PREFIX marker (quote, backquote, unquote, unquote-splicing,
function-quote, or a #n= label) is popped and DEPTH decremented: its one
recursive read has returned, and nothing further is pending.

A :FEATURE-EXPR marker (the first, feature-expression read of a pending
#+/#-) is instead CONVERTED to :PREFIX in place, and the loop stops
there. #+/#- issue a second, independent read for the guarded form
immediately after the first read returns, so depth must stay elevated
across that transition rather than dropping to baseline -- otherwise a
#+/#- chained into the guarded-form position would scan as shallow no
matter how deep the chain runs, reopening the same false-negative class
this scanner exists to close.

Use %SCAN-DRAIN-LEADING-MARKERS instead where a delimiter arrives with no
operand for a pending marker to attach to at all (e.g. \"')\" or a
malformed \"#+)\"): there, a :FEATURE-EXPR has no target to transition
into and must be popped like any other marker, not converted."
  (loop
    (let ((top (car (scan-state-stack state))))
      (cond
        ((eq top :prefix)
         (pop (scan-state-stack state))
         (decf (scan-state-depth state)))
        ((eq top :feature-expr)
         (setf (car (scan-state-stack state)) :prefix)
         (return))
        (t (return))))))

(defun %scan-drain-leading-markers (state)
  "Unconditionally pop every marker (:PREFIX or :FEATURE-EXPR) now on top
of STATE's stack, decrementing DEPTH once per marker popped.

Used only where a delimiter or end-of-input arrives with no operand for a
pending marker to attach to (a stray \"')\", a malformed \"#+)\", or a
dangling prefix at end of file): there is no target left to read, so a
:FEATURE-EXPR has nothing to transition into and is popped like any other
marker instead of being converted the way %SCAN-RESOLVE-PENDING-PREFIXES
would. Also guards %SCAN-PARENS-POP-OPEN and SCAN-PARENS's own trailing
\"unclosed\" check, both of which DESTRUCTURING-BIND the stack's top
assuming a bracket frame -- a marker left in place would signal a type
error there instead of scanning cleanly."
  (loop while (member (car (scan-state-stack state)) '(:prefix :feature-expr))
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
   ;; #+/#- (CLHS 1.5.2, 2.4.8.16/17): a feature conditional issues TWO
   ;; independent recursive reads -- first the feature-expression, then
   ;; (regardless of the test's outcome, since even a failing test must
   ;; skip over the guarded form) the guarded form itself. The first read
   ;; is tracked with a distinct marker so %SCAN-RESOLVE-PENDING-PREFIXES
   ;; can convert it into an ordinary pending prefix -- rather than
   ;; popping it -- once the feature-expression's target is reached: that
   ;; keeps depth from dropping to baseline between the two reads, so a
   ;; #+/#- chained into the guarded-form position (its own #+/#- as the
   ;; "form") still accumulates depth instead of resetting every block.
   ((and (char= ch #\#) next (or (char= next #\+) (char= next #\-)))
    (%scan-push-feature-expr state)
    (values nil 1))
   ;; #n=object (CLHS 2.4.8.14) reads OBJECT via one recursive READ, the
   ;; same shape as quote -- and, like quote, #n= can chain (#1=#2=...=x
   ;; is valid) to accumulate real recursion depth. #n# (CLHS 2.4.8.15),
   ;; by contrast, is a leaf back-reference to an already-read object: it
   ;; consumes only its own digits and has no operand to recurse on, so it
   ;; needs no marker and is left to fall through to the default case
   ;; below unchanged.
   ((and (char= ch #\#) next (digit-char-p next))
    (let ((k (1+ idx)))
      (loop while (and (< k (length text)) (digit-char-p (char text k)))
            do (incf k))
      (if (and (< k (length text)) (char= (char text k) #\=))
          (progn
            (%scan-push-prefix state)
            (values nil (- k idx)))
          (values nil nil))))
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
    ;; A lone marker directly before a closer (e.g. "')" or a malformed
    ;; "#+)") has no operand of its own; drain it first so
    ;; %SCAN-PARENS-POP-OPEN never sees a marker where it expects a
    ;; bracket frame.
    (%scan-drain-leading-markers state)
    (multiple-value-bind (new-stack err)
        (%scan-parens-pop-open (scan-state-stack state) (scan-state-line state)
         (scan-state-col state) base-offset ch idx)
      (setf (scan-state-stack state) new-stack)
      (unless err
        (decf (scan-state-depth state))
        ;; Closing this bracket also resolves any prefixes that preceded
        ;; it (e.g. the quote in "'(a)", or the feature-expression of a
        ;; "#+(a b) form" whose expression was itself the list just
        ;; closed): their recursive reads return together with the
        ;; list's own.
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
is known to reach scanning TEXT -- brackets, the prefix macros ', `, ,,
,@, and #', the #n= label syntax, and #+/#- feature conditionals, none of
which have a closing delimiter of their own -- even along early-return
error paths. See *MAX-NESTING-DEPTH*'s docstring: this list is not, and
cannot be, exhaustive)."
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
    ;; A trailing marker (e.g. an unterminated "'" or a dangling "#+" at
    ;; end of input) has no bracket to match, so it is not an "unclosed"
    ;; error in the sense the check below reports; drain it -- a
    ;; :FEATURE-EXPR here has no target left to transition into either, so
    ;; this uses the same unconditional drain as a marker directly before
    ;; a closer, not the convert-and-stop of %SCAN-RESOLVE-PENDING-
    ;; PREFIXES. The reader would raise END-OF-FILE trying to read the
    ;; missing operand, which %TRY-READER-CHECK already handles.
    (%scan-drain-leading-markers state)
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
