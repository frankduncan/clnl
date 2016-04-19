(asdf:defsystem clnl-test
 :name "Experiment Tests"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :serial t
 :components ((:file "package")
              (:file "main")
              (:file "simpletests")
              (:file "viewtests"))
 :depends-on (#-travis :ironclad :clnl))
