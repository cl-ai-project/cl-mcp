(:mallet-config
 (:extends :default)
 (:disable :eval-usage)
 (:disable :ignore-errors-usage)
 (:for-paths ("src/utils/lenient-read.lisp")
   (:disable :double-colon-access)))
