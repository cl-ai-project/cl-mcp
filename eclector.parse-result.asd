;;;; eclector.parse-result.asd
;;;; Compatibility shim: route eclector.parse-result to eclector

(asdf:defsystem "eclector.parse-result"
  :description "Compatibility shim: route eclector.parse-result to eclector"
  :depends-on ("eclector"))
