; The systems are split up into two because travis will run more smoothly
; if we use a custom built sbcl that has all the deps pre-loaded since
; we are sure those will work just fine :)
;
; You should link to this file in your systems directory, or however you
; handle your asdf configurations.  Then just (asdf:load-system :clnl)
;
; There's probably a better way, but I don't know it

(asdf:defsystem clnl
  :name "Experiment"
  :version "0.0"
  :maintainer "Frank Duncan (frank@kank.com)"
  :author "Frank Duncan (frank@kank.com)"
  :depends-on (:cl-ppcre :mt19937 :cl-opengl :cl-glut :cl-charms :clnl.internal))
