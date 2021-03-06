(in-package #:clnl)

(defun e (ast) ast)

(defun r (str)
 (let*
  ((lexed-ast (let ((ast (clnl-lexer:lex str)))
               (format t "Via lexing, AST for~%~S~% became~%~S~%~%" str ast) ast))
   (parsed-ast (let ((ast (clnl-parser:parse lexed-ast)))
                (format t "Via parsing, AST for~%~S~% became~%~S~%~%" lexed-ast ast) ast))
   (transpiled-ast (let ((ast (clnl-transpiler:transpile parsed-ast)))
                    (format t "Via transpiling, AST for~%~S~% became~%~S~%" parsed-ast ast) ast)))
  (eval transpiled-ast)))

(defun p (result) result)

(defun run (&optional file)
 "RUN &optional FILE => RESULT

ARGUMENTS AND VALUES:

  FILE: nlogo file with which to initialize
  RESULT: undefined, the system terminates at the end of the loop

DESCRIPTION:

  RUN starts up the CLNL system."
 (boot file)
 (clnl-interface:run))

(defvar *callback* nil)

(defun boot (&optional file headless-mode)
 "BOOT &optional FILE HEADLESS-MODE => RESULT

ARGUMENTS AND VALUES:

  FILE: nlogo file with which to initialize state
  HEADLESS-MODE: a boolean, defaults to nil
  RESULT: undefined

DESCRIPTION:

  BOOT does exactly that, boots the clnl system in a clean state.  The seed
  is set so that multiple runs will evaluate to the same.

  When FILE is not provided, a default model is used.

  When HEADLESS-MODE is set to nil, the opengl interface is initialized.
  Otherwise, the model will run headlessly, with no view."
 (let
  ((netlogoed-lisp
    (model->single-form-lisp
     (if file (with-open-file (str file) (clnl-model:read-from-nlogo str)) (clnl-model:default-model))
     :initialize-interface (not headless-mode)
     :netlogo-callback (lambda (f) (setf *callback* f))))
   (*package* *model-package*))
  (eval netlogoed-lisp)))

(defvar *commands-mutex* (sb-thread:make-mutex))

(defun run-commands (cmds)
 "RUN-COMMANDS CMDS => RESULT

ARGUMENTS AND VALUES:

  CMDS: A string that may have one more NetLogo commands
  RESULT: undefined

DESCRIPTION:

  RUN-COMMANDS will take NetLogo commands, put them through the various
  stages need to turn them into Common Lisp code, and run it."

 ; This mutex is a necessary because we haven't yet moved to a job thread
 (sb-thread:with-mutex (*commands-mutex*)
  (clnl-nvm:with-stop-handler
   (funcall *callback* cmds))))

(defun run-reporter (reporter)
 "RUN-REPORTER REPORTER => RESULT

ARGUMENTS AND VALUES:

  REPORTER: A string that should have only one reporter
  RESULT: The value reported by the NVM

DESCRIPTION:

  RUN-REPORTER will take a NetLogo REPORTER, put it through the various
  stages need to turn them into Common Lisp code, run it, and return the RESULT."
 (eval (clnl-transpiler:transpile (clnl-parser:parse (clnl-lexer:lex reporter)))))

; Because prims are used both at generation time and later at runtime, certain things in
; them must be escaped a little bit more, such as wrapping the whole thing in a list
; primitive.  This way, the output of these things looks like halfway decent lisp,
; and everything works nicely.  We don't want any <FUNC #> showing up or anything
(defun munge-prim (prim)
 (let
  ((copied (copy-list prim)))
  (when (getf copied :args) (setf (getf copied :args) `(quote ,(getf copied :args))))
  `(list ,@copied)))

(defun netlogo-callback-body (prims)
 `(eval
   (clnl-transpiler:transpile
    (clnl-parser:parse
     (clnl-lexer:lex ,(intern "NETLOGO-CODE" *model-package*))
     (list ,@(mapcar #'munge-prim prims)))
    (list ,@(mapcar #'munge-prim prims)))))

(defun create-world-call (model globals code-ast)
 `(clnl-nvm:create-world
   :dims ',(clnl-model:world-dimensions model)
   :globals (list
             ,@(mapcar
                (lambda (pair)
                 `(list ,(car pair) (lambda () ,(intern (string-upcase (car pair)) *model-package*))))
                globals))
   :turtles-own-vars ',(clnl-code-parser:turtles-own-vars code-ast)
   :patches-own-vars ',(clnl-code-parser:patches-own-vars code-ast)
   :breeds ',(clnl-code-parser:breeds code-ast)))

(defun create-proc-body (proc prims)
 `(,(intern (string-upcase (car proc)) *model-package*) ()
   (clnl-nvm:with-stop-handler
    ,@(cdr ; remove the progn, cuz it looks nicer
       (clnl-transpiler:transpile (cadr proc)
        (mapcar
         (lambda (prim)
          (if (getf prim :macro) ; The reason we do this is because with macros, we want to evaluate them in
                                 ; this scope while preserving them for the generational purposes below
           (append (list :macro (eval (getf prim :macro))) prim)
           prim)) prims)))
    :undefined)))

(defun nlogo->lisp (str pkg-symb boot-fn &key (seed 15) initialize-interface netlogo-callback-fn)
 (let*
  ((model (clnl-model:read-from-nlogo str))
   (shadow-symbs
    (remove nil
     (mapcar
      (lambda (proc-symb)
       (multiple-value-bind (found external) (find-symbol (symbol-name proc-symb) :cl)
        (when (and found (eql :external external)) proc-symb)))
      (mapcar #'car
       (clnl-code-parser:procedures
        (clnl-code-parser:parse
         (clnl-lexer:lex (clnl-model:code model))
         (clnl-model:widget-globals model))))))))
  (eval
   `(progn
     (defpackage ,pkg-symb (:use :common-lisp) (:shadow ,@shadow-symbs))
     (,(intern "IN-PACKAGE" :cl) ,pkg-symb) ; intern because of style check
     (cons
      `(defpackage ,,pkg-symb (:use :common-lisp) (:shadow ,,@shadow-symbs))
      (let
       ((clnl:*model-package* (find-package ,pkg-symb)))
       (clnl:model->multi-form-lisp
        ,model
        (intern (symbol-name ',boot-fn) ,pkg-symb)
        :seed ,seed
        :initialize-interface ,initialize-interface
        :netlogo-callback-fn ,netlogo-callback-fn)))))))

(setf (documentation 'nlogo->lisp 'function)
 "NLOGO->LISP STR PKG-SYMB BOOT-FN &key SEED INITIALIZE-INTERFACE NETLOGO-CALLBACK-FN => FORMS

ARGUMENTS AND VALUES:

  STR: A stream holding an nlogo file
  PKG-SYMB: A symbol for the generated package
  BOOT-FN: A function name
  SEED: An integer, defaults to 15
  INITIALIZE-INTERFACE: A boolean
  NETLOGO-CALLBACK-FN: a symbol
  FORMS: A list of common lisp form

DESCRIPTION:

  NLOGO->LISP takes a stream STR and returns a multi form lisp program,
  that when executed, sets up the model.  See MODEL->MULTI-FORM-LISP for
  more information.

  NLOGO->LISP does extra work of setting up the package to be named by
  PKG-SYMB in order to correctly shadow common lisp functions.

  It will also change the current package to the one created for the model
  named by PKG-SYMB.

EXAMPLES:

  (with-open-file (str \"Wolf Sheep Predation.nlogo\") (nlogo->lisp str :wolfsheep 'boot)) => (forms)")

(defun model->single-form-lisp (model &key (seed 15) initialize-interface netlogo-callback)
 (multiple-value-bind
  (code-ast prims)
  (clnl-code-parser:parse (clnl-lexer:lex (clnl-model:code model)) (clnl-model:widget-globals model))
  (let
   ((globals
     (append
      (clnl-code-parser:globals code-ast)
      (clnl-model:widget-globals model))))
   `(progn
     ,@(mapcar
        (lambda (pair) `(defparameter ,(intern (string-upcase (car pair)) *model-package*) ,(cadr pair)))
        globals)
     (labels
      ,(mapcar
        (lambda (proc) (create-proc-body proc prims))
        (clnl-code-parser:procedures code-ast))
      (clnl-random:set-seed ,seed)
      (clnl-model:set-current-interface ',(clnl-model:interface model))
      ,@(when netlogo-callback
         `((clnl-model:set-callback
            (lambda (,(intern "NETLOGO-CODE" *model-package*)) ,(netlogo-callback-body prims)))))
      ,(create-world-call model globals code-ast)
      ,@(when netlogo-callback
         `((funcall ,netlogo-callback
            (lambda (,(intern "NETLOGO-CODE" *model-package*))
             ,(netlogo-callback-body prims)))))
      ,@(when initialize-interface
         `((clnl-interface:initialize
            :dims ',(clnl-model:world-dimensions model)
            :view ',(clnl-model:view model)
            :buttons ',(clnl-model:buttons model)))))))))

(setf (documentation 'model->single-form-lisp 'function)
 "MODEL->SINGLE-FORM-LISP MODEL &key SEED INITIALIZE-INTERFACE NETLOGO-CALLBACK => FORM

ARGUMENTS AND VALUES:

  MODEL: A valid model
  SEED: An integer, defaults to 15
  INITIALIZE-INTERFACE: A boolean
  NETLOGO-CALLBACK: A function of one argument, or a symbol
  FORM: A common lisp form

DESCRIPTION:

  MODEL->SINGLE-FORM-LISP takes a model and returns a lisp program as a single form,
  that when executed runs the model.  The SEED passed in is used to start the
  clnl-random RNG.

  INITIALIZE-INTERFACE, when non nil, leads to initialization code for the
  opengl interface being included.

  NETLOGO-CALLBACK is a function that when called with a single argument,
  a function that when called with netlogo code, will compile and run that
  code in the environment of the model.

  Of note, all globals defined either in the model code or via the widgets
  are declared special in order to remain in the lexical environment for EVAL.")

(defun model->multi-form-lisp (model boot-fn &key (seed 15) initialize-interface netlogo-callback-fn)
 (multiple-value-bind
  (code-ast prims)
  (clnl-code-parser:parse (clnl-lexer:lex (clnl-model:code model)) (clnl-model:widget-globals model))
  (let
   ((globals
     (append
      (clnl-model:widget-globals model)
      (clnl-code-parser:globals code-ast))))
   `((in-package ,(intern (package-name *model-package*) :keyword))
     ,@(mapcar
        (lambda (pair)
         `(defvar ,(intern (string-upcase (car pair)) *model-package*) ,(cadr pair)))
        globals)
     ,@(mapcar
        (lambda (proc) `(defun ,@(create-proc-body proc prims)))
        (clnl-code-parser:procedures code-ast))
     (defun ,boot-fn ()
      (clnl-random:set-seed ,seed)
      (clnl-model:set-current-interface ',(clnl-model:interface model))
      (clnl-model:set-callback (symbol-function ',netlogo-callback-fn))
      ,(create-world-call model globals code-ast)
      ,@(when initialize-interface
         `((clnl-interface:initialize
            :dims ',(clnl-model:world-dimensions model)
            :view ',(clnl-model:view model)
            :buttons ',(clnl-model:buttons model)))))
     ,@(when netlogo-callback-fn
        `((defun ,netlogo-callback-fn (,(intern "NETLOGO-CODE" *model-package*))
           ,(netlogo-callback-body prims))))))))

(setf (documentation 'model->multi-form-lisp 'function)
 "MODEL->MULTI-FORM-LISP MODEL BOOT-FN &key SEED INITIALIZE-INTERFACE NETLOGO-CALLBACK-FN => FORMS

ARGUMENTS AND VALUES:

  MODEL: A valid model
  BOOT-FN: A function name
  SEED: An integer, defaults to 15
  INITIALIZE-INTERFACE: A boolean
  NETLOGO-CALLBACK-FN: a symbol
  FORMS: A list of common lisp form

DESCRIPTION:

  MODEL->MULTI-FORM-LISP takes a model and returns a multi form lisp program,
  that when executed, sets up the model.  Procedures map to defuns, globals
  to defvars, etc.  This can be output to load up quickly later.  A function
  named by BOOT-FN will be set for booting the program.

  The SEED passed in is used to start the clnl-random RNG.

  INITIALIZE-INTERFACE, when non nil, leads to initialization code for the
  opengl interface being included.

  NETLOGO-CALLBACK-FN is a symbol that will be defined as a function
  to be called to execute code in the running netlogo instance.")
