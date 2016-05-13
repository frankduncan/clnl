(in-package #:clnl-transpiler)

(defparameter *prims* nil)

(defvar *local-variables* nil)
(defvar *dynamic-prims* nil)

(defun prim-name (prim) (getf prim :name))
(defun prim-type (prim) (getf prim :type))
(defun prim-func (prim) (getf prim :func))
(defun prim-reporter-p (prim) (eql :reporter (getf prim :type)))
(defun prim-command-p (prim) (eql :command (getf prim :type)))

(defun find-prim (symb)
 (when symb
  (find-if
   (lambda (prim-name) (or (eql symb prim-name) (and (listp prim-name) (find symb prim-name))))
   (append *prims* *dynamic-prims*)
   :key #'prim-name)))

(defun transpile (parsed-ast &optional dynamic-prims)
 "TRANSPILE PARSED-AST &optional DYNAMIC-PRIMS => AST

  DYNAMIC-PRIMS: DYNAMIC-PRIM*
  DYNAMIC-PRIM: (:name NAME :type TYPE :macro MACRO :func FUNC)
  TYPE: :reporter | :command

ARGUMENTS AND VALUES:

  PARSED-AST: An ast as returned by the parser
  AST: An common lisp AST that can be actually run in a common lisp instance
  NAME: A symbol in the keyword package
  MACRO: A macro that will be called with the arguments ast
  FUNC: A function that will be called with the transpiled arguments

DESCRIPTION:

  TRANSPILE takes a unambigious PARSED-AST and converts it to
  Common Lisp code.  The PARSED-AST must be either a list of commands,
  or a single reporter.

  When a set of DYNAMIC-PRIMS is included, external language constructs
  can be also transpiled.  The provided functions will be inserted into
  the returned AST with a call to FUNCALL.  If :macro is included, instead
  of having a call to FUNCALL provided, the macro will be run at netlogo
  transpile time, with the arguments it should have specified to the
  parser.  The result of that function call will then be dropped into
  the ast.

  Calling eval on that code should work correctly as long as you have a
  running engine."

 (let
  ((*dynamic-prims*
    (mapcar
     (lambda (prim)
      (if (getf prim :macro)
       (append (list :func (getf prim :macro)) prim)
       (append (list :func (lambda (&rest args) `(funcall ,(getf prim :func) ,@args))) prim)))
     dynamic-prims)))
  (cond
   ((command-list-p parsed-ast) (transpile-commands parsed-ast))
   ((and (listp parsed-ast) (= 1 (length parsed-ast)) (reporter-p (car parsed-ast)))
    (transpile-reporter (car parsed-ast)))
   (t (error "Is neither a list of commands nor a reporter: ~S" parsed-ast)))))

(defun command-list-p (parsed-ast)
 "COMMAND-LIST-P PARSED-AST => RESULT

ARGUMENTS AND VALUES:

  PARSED-AST: An ast as returned by the parser
  RESULT: A boolean

DESCRIPTION:

  COMMAND-LIST-P returns whether the parsed-ast is a valid list
  of commands."
 (and
  (every #'listp parsed-ast)
  (every #'prim-command-p (mapcar #'find-prim (mapcar #'car parsed-ast)))))

(defun reporter-p (parsed-ast)
 "REPORTER-P PARSED-AST => RESULT

ARGUMENTS AND VALUES:

  PARSED-AST: An ast as returned by the parser
  RESULT: A boolean

DESCRIPTION:

  REPORTER-P returns whether the parsed-ast is a valid reporter."
 (and
  (symbolp (car parsed-ast))
  (prim-reporter-p (find-prim (car parsed-ast)))))

; Let this grow, slowly but surely, eventually taking on calling context, etc.
; For now, it's just a
(defun transpile-commands (parsed-ast)
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
  ((not (prim-command-p (find-prim (car command)))) (error "Expected command, got ~S" (car command)))
  (t (apply (prim-func (find-prim (car command))) (mapcar #'transpile-reporter (cdr command))))))

(defun transpile-reporter (reporter)
 (cond
  ((numberp reporter) reporter) ; The parser converts to double for us
  ((stringp reporter) reporter)
  ; The parser should have checked that having a symbol here is ok
  ((symbolp reporter) (intern (symbol-name reporter) clnl:*model-package*))
  ((not (listp reporter)) (error "Expected a statement of some sort"))
  ((eql :command-block (car reporter)) (transpile-command-block reporter))
  ((eql :list-literal (car reporter)) (cons 'list (mapcar #'transpile-reporter (cdr reporter))))
  ((eql :reporter-block (car reporter)) (transpile-reporter-block reporter))
  ((and (symbolp (car reporter)) (find (car reporter) *local-variables*))
   (intern (symbol-name (car reporter)) clnl:*model-package*))
  ((not (find-prim (car reporter))) (error "Couldn't find the reporter for ~S" (car reporter)))
  ((not (prim-reporter-p (find-prim (car reporter)))) (error "Expected reporter, got ~S" (car reporter)))
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
(defprim :any? :reporter (lambda (agentset) `(> (clnl-nvm:count ,agentset) 0)))
(defsimpleprim :ask :command clnl-nvm:ask)
(defagentvalueprim :color)
(defsimpleprim '(:clear-all :ca) :command clnl-nvm:clear-all)
(defsimpleprim :count :reporter clnl-nvm:count)
(defprim '(:crt :create-turtles) :command (lambda (num &optional fn) `(clnl-nvm:create-turtles ,num nil ,fn)))
(defsimpleprim :die :command clnl-nvm:die)
(defsimpleprim :display :command clnl-nvm:display)
(defsimpleprim :fd :command clnl-nvm:forward)
(defsimpleprim :hatch :command clnl-nvm:hatch)
(defprim :if :command (lambda (pred a) `(when ,pred ,@(make-command-block-inline a))))
(defprim '(:ifelse :if-else)
 :command (lambda (pred a b)
           `(if ,pred
             ,@(make-command-block-inline a)
             ,@(make-command-block-inline b))))

(defagentvalueprim :label)
(defagentvalueprim :label-color)
(defsimpleprim :let :command nil)
(defsimpleprim :lt :command clnl-nvm:turn-left)
(defsimpleprim :not :reporter cl:not)
(defkeywordprim :nobody)
(defsimpleprim :one-of :reporter clnl-nvm:one-of)
(defsimpleprim :of :reporter clnl-nvm:of)
(defsimpleprim :patches :reporter clnl-nvm:patches)
(defagentvalueprim :pcolor)
(defsimpleprim :reset-ticks :command clnl-nvm:reset-ticks)
(defsimpleprim :random :reporter clnl-nvm:random)
(defsimpleprim :random-float :reporter clnl-nvm:random-float)
(defsimpleprim :random-xcor :reporter clnl-nvm:random-xcor)
(defsimpleprim :random-ycor :reporter clnl-nvm:random-ycor)
(defprim :round :reporter (lambda (n) `(ffloor (+ ,n 0.5d0))))
(defsimpleprim :rt :command clnl-nvm:turn-right)
(defsimpleprim :set :command cl:setf)
(defsimpleprim :set-default-shape :command clnl-nvm:set-default-shape)
(defsimpleprim :setxy :command clnl-nvm:setxy)
(defsimpleprim :show :command clnl-nvm:show)
(defsimpleprim :stop :command clnl-nvm:stop)
(defagentvalueprim :size)
(defsimpleprim :tick :command clnl-nvm:tick)
(defsimpleprim :ticks :reporter clnl-nvm:ticks)
(defsimpleprim :turtles :reporter clnl-nvm:turtles)
(defsimpleprim :turtles-here :reporter clnl-nvm:turtles-here)
(defagentvalueprim :who)
(defsimpleprim :with :reporter clnl-nvm:with)

; Colors
(defmacro defcolorprim (color) `(defprim ,color :reporter (lambda () `(clnl-nvm:lookup-color ,,color))))
(defcolorprim :black)
(defcolorprim :blue)
(defcolorprim :brown)
(defcolorprim :green)
(defcolorprim :white)
