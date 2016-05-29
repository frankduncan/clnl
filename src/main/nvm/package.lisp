(defpackage #:clnl-nvm
 (:use :common-lisp)
 (:shadow #:random #:count)
 (:export
  ; API as used by transpiled NetLogo programs

  ; base
  #:with-stop-handler

  ; nvm
  #:agent-value #:create-world #:current-state #:lookup-color

  ; turtles
  #:create-turtles #:die #:hatch #:forward #:random-xcor #:random-ycor #:set-default-shape #:setxy
  #:turtles-here #:turn-right #:turn-left

  ; agentset
  #:count #:of #:one-of #:patches #:turtles #:with

  ; controlflow
  #:ask #:stop

  ; world
  #:clear-all #:display #:reset-ticks #:tick #:ticks

  ; inout
  #:export-world #:show

  ; math
  #:random #:random-float)
 (:documentation
  "CLNL NVM

NetLogo Virtual Machine: the simulation engine."))
