(asdf:defsystem clnl
 :name "Experiment"
 :version "0.1.0"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :components ((:file "package")
              (:file "nvm/package")
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
              (:file "nvm/agentset")
              (:file "nvm/controlflow")
              (:file "nvm/inout")
              (:file "nvm/math")
              (:file "nvm/turtles")
              (:file "nvm/world")
              (:file "nvm/topology")
              (:file "transpile")
              (:file "random")
              (:file "interface")
              (:file "main"))
 :depends-on #-travis(:cl-ppcre :mt19937 :cl-opengl :cl-glu :cl-glut :ieee-floats :strictmath :clnl-gltk) #+travis nil)
