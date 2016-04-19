(asdf:defsystem clnl
 :name "Experiment"
 :version "0.0.0"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :components ((:file "package")
              (:file "model")
              (:file "lex")
              (:file "parse")
              (:file "nvm")
              (:file "transpile")
              (:file "random")
              (:file "interface")
              (:file "cli")
              (:file "main"))
 :depends-on #-travis (:cl-ppcre :mt19937 :cl-opengl :cl-glut :cl-charms :strictmath) #+travis nil)
