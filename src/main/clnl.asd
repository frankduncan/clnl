(asdf:defsystem clnl
 :name "Experiment"
 :version "0.0.0"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :components ((:file "package")
              (:file "base")
              (:file "extensions")
              (:file "model")
              (:file "lex")
              (:file "parse")
              (:file "code-parse")
              (:file "nvm/base")
              (:file "nvm/utils")
              (:file "nvm/agent")
              (:file "nvm/nvm")
              (:file "nvm/topology")
              (:file "transpile")
              (:file "random")
              (:file "interface")
              (:file "cli")
              (:file "main"))
 :depends-on #-travis(:cl-ppcre :mt19937 :cl-opengl :cl-glut :cl-charms :ieee-floats :strictmath) #+travis nil)
