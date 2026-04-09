(:mallet-config
 (:extends :default)
 (:disable :no-eval)
 (:disable :no-ignore-errors)
 (:for-paths ("src/utils/lenient-read.lisp")
   (:disable :double-colon-access)))
