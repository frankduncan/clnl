(in-package #:clnl-transpiler)

(defparameter *prims* nil)
(defparameter *prim-aliases* nil)

(defvar *local-variables* nil)

(defun prim-name (prim) (getf prim :name))
(defun prim-type (prim) (getf prim :type))
(defun prim-func (prim) (getf prim :func))
(defun is-reporter (prim) (eql :reporter (getf prim :type)))
(defun is-command (prim) (eql :command (getf prim :type)))

(defun find-prim (symb)
 (when symb
  (or
   (find symb *prims* :key #'prim-name)
   (find-prim (getf (find symb *prim-aliases* :key #'prim-name) :real-symb)))))

; Let this grow, slowly but surely, eventually taking on calling context, etc.
; For now, it's just a
(defun transpile-commands (parsed-ast)
 "TRANSPILE-COMMANDS PARSED-AST => AST

ARGUMENTS AND VALUES:

  PARSED-AST: An ast as returned by the parser
  AST: An common lisp AST that can be actually run in a common lisp instance

DESCRIPTION:

  TRANSPILE-COMMANDS takes a unambigious PARSED-AST and converts it to
  Common Lisp code.

  Calling eval on that code should work correctly as long as you have a
  running engine.  This is the entry point for commands, so it does
  extra checking to ensure that commands are actually in the PARSED-AST."
 `(progn
   ,@(transpile-commands-inner parsed-ast)))

(defun transpile-commands-inner (parsed-ast)
 (cond
  ((not parsed-ast) nil)
  ((and (listp (car parsed-ast)) (eql :let (caar parsed-ast))) (list (handle-let parsed-ast)))
  (t
   (cons
    (transpile-command (car parsed-ast))
    (transpile-commands-inner (cdr parsed-ast))))))

(defun handle-let (parsed-ast &optional vars)
 (if
  (and (listp (car parsed-ast)) (eql :let (caar parsed-ast)))
  (let
   ((*local-variables* (cons (second (car parsed-ast)) *local-variables*)))
   (handle-let
    (cdr parsed-ast)
    (cons
     (list
      (transpile-reporter (second (car parsed-ast)))
      (transpile-reporter (third (car parsed-ast))))
     vars)))
  `(let*
    ,vars
    ,@(transpile-commands-inner parsed-ast))))

(defun transpile-command (command)
 (cond
  ((not (listp command)) (error "Expected a statement of some sort"))
  ((not (find-prim (car command))) (error "Couldn't find the command for ~S" (car command)))
  ((not (is-command (find-prim (car command)))) (error "Expected command, got ~S" (car command)))
  (t (apply (prim-func (find-prim (car command))) (mapcar #'transpile-reporter (cdr command))))))

(defun transpile-reporter (reporter)
 "TRANSPILE-REPORTER REPORTER => AST

ARGUMENTS AND VALUES:

  REPORTER: An ast returned from the parser.
  AST: An common lisp AST that can be actually run in a common lisp instance

DESCRIPTION:

  TRANSPILE-REPORTER takes a unambigious PARSED-AST and converts it to
  Common Lisp code.

  Calling eval on that code should work correctly as long as you have a
  running engine.  This is the entry point for reporters, so it does
  extra checking to ensure that the reporter is actually in the REPORTER.

  The Common lisp code that is returned, when run, will return some value."
 (cond
  ((numberp reporter) reporter) ; The parser converts to double for us
  ; The parser should have checked that having a symbol here is ok
  ((symbolp reporter) (intern (symbol-name reporter) clnl:*model-package*))
  ((not (listp reporter)) (error "Expected a statement of some sort"))
  ((eql :command-block (car reporter)) (transpile-command-block reporter))
  ((eql :reporter-block (car reporter)) (transpile-reporter-block reporter))
  ((and (symbolp (car reporter)) (find (car reporter) *local-variables*))
   (intern (symbol-name (car reporter)) clnl:*model-package*))
  ((not (find-prim (car reporter))) (error "Couldn't find the reporter for ~S" (car reporter)))
  ((not (is-reporter (find-prim (car reporter)))) (error "Expected reporter, got ~S" (car reporter)))
  (t (apply (prim-func (find-prim (car reporter))) (mapcar #'transpile-reporter (cdr reporter))))))

(defun transpile-command-block (block)
 `(lambda () ,@(transpile-commands-inner (cdr block))))

(defun transpile-reporter-block (block)
 (when (/= (length block) 2) (error "Reporter block invalid ~S" block))
 `(lambda ()
   ,(transpile-reporter (cadr block))))

; Undoes the previous function :)
(defun make-command-block-inline (block)
 (cddr block))

(defmacro defprim (name type func)
 `(push (list :name ,name :type ,type :func ,func) *prims*))

(defmacro defsimpleprim (name type simple-func)
 `(defprim ,name ,type (lambda (&rest args) `(,',simple-func ,@args))))

(defmacro defkeywordprim (name)
 `(defprim ,name :reporter (lambda () ',name)))

(defmacro defprim-alias (name real-symb)
 `(push (list :name ,name :real-symb ,real-symb) *prim-aliases*))

(defmacro defagentvalueprim (name)
 `(defprim ,name :reporter (lambda () `(clnl-nvm:agent-value ,,name))))

; We count on the parser to handle arguemnts for us, when collating things.

(defsimpleprim := :reporter cl:equalp)
(defprim :!= :reporter (lambda (a b) `(not (equalp ,a ,b))))
(defsimpleprim :<= :reporter cl:<=)
(defsimpleprim :< :reporter cl:<)
(defsimpleprim :- :reporter cl:-)
(defsimpleprim :+ :reporter cl:+)
(defsimpleprim :* :reporter cl:*)
(defsimpleprim :/ :reporter cl:/)
(defprim :any? :reporter (lambda (agentset) `(> (length ,agentset) 0)))
(defsimpleprim :ask :command clnl-nvm:ask)
(defsimpleprim :crt :command clnl-nvm:create-turtles)
(defsimpleprim :die :command clnl-nvm:die)
(defsimpleprim :fd :command clnl-nvm:forward)
(defprim :if :command (lambda (pred a) `(when ,pred ,@(make-command-block-inline a))))
(defprim :ifelse :command (lambda (pred a b)
                           `(if ,pred
                             ,@(make-command-block-inline a)
                             ,@(make-command-block-inline b))))

(defprim-alias :if-else :ifelse)
(defsimpleprim :lt :command clnl-nvm:turn-left)
(defkeywordprim :nobody)
(defsimpleprim :one-of :reporter clnl-nvm:one-of)
(defsimpleprim :of :reporter clnl-nvm:of)
(defsimpleprim :patches :reporter clnl-nvm:patches)
(defagentvalueprim :pcolor)
(defsimpleprim :reset-ticks :command clnl-nvm:reset-ticks)
(defsimpleprim :random-float :reporter clnl-nvm:random-float)
(defsimpleprim :rt :command clnl-nvm:turn-right)
(defsimpleprim :show :command clnl-nvm:show)
(defsimpleprim :set :command cl:setf)
(defsimpleprim :tick :command clnl-nvm:tick)
(defsimpleprim :ticks :reporter clnl-nvm:ticks)
(defsimpleprim :turtles :reporter clnl-nvm:turtles)
(defagentvalueprim :who)

; Colors
(defmacro defcolorprim (color) `(defprim ,color :reporter (lambda () `(clnl-nvm:lookup-color ,,color))))
(defcolorprim :black)
(defcolorprim :blue)
(defcolorprim :brown)
(defcolorprim :green)
(defcolorprim :white)
