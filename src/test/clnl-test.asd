; For why this is the way it is, see src/main/clnl.asd
(asdf:defsystem clnl-test
  :name "Experiment Tests"
  :version "0.0.1"
  :maintainer "Frank Duncan (frank@kank.com)"
  :author "Frank Duncan (frank@kank.com)"
  :serial t
  :depends-on (:ironclad :clnl clnl-test.internal))
