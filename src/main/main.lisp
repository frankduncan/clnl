(in-package #:clnl)

(defvar *model-package* (find-package :cl-user)
 "*MODEL-PACKAGE*

VALUE TYPE:

  a package

INITIAL VALUE:

  The common-lisp-user package

DESCRIPTION:

  *MODEL-PACKAGE* is used for interning symbols as a NetLogo code
  gets compiled.

  Any local symbols are interned in this package, for use either
  by other code, or in order to have all symbols interned in the
  same placakge.  This is also the package in which a model should
  be run, whether by clnl code or independently.")

(defun e (ast) ast)

(defun r (str)
 (let*
  ((lexed-ast (let ((ast (clnl-lexer:lex str)))
               (format t "Via lexing, AST for~%~S~% became~%~S~%~%" str ast) ast))
   (parsed-ast (let ((ast (clnl-parser:parse lexed-ast)))
                (format t "Via parsing, AST for~%~S~% became~%~S~%~%" lexed-ast ast) ast))
   (transpiled-ast (let ((ast (clnl-transpiler:transpile-commands parsed-ast)))
                    (format t "Via transpiling, AST for~%~S~% became~%~S~%" parsed-ast ast) ast)))
  (eval transpiled-ast)))

(defun p (result) result)

(defun run ()
 "RUN => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined, the system terminates at the end of the loop

DESCRIPTION:

  RUN starts up the CLNL system."

 (boot)
 (sb-thread:make-thread #'clnl-cli:run)
 (clnl-interface:run))

(defun boot (&optional file)
 "BOOT &optional FILE => RESULT

ARGUMENTS AND VALUES:

  FILE: nlogo file with which to initialize state
  RESULT: undefined

DESCRIPTION:

  BOOT does exactly that, boots the clnl system in a clean state.  The seed
  is set so that multiple runs will evaluate to the same.

  When FILE is not provided, a default model is used."
 (let
  ((netlogoed-lisp
    (model->lisp
     (if file (with-open-file (str file) (clnl-model:read-from-nlogo str)) (clnl-model:default-model))))
   (*package* *model-package*))
  (eval netlogoed-lisp)))

(defun run-commands (cmds)
 "RUN-COMMANDS CMDS => RESULT

ARGUMENTS AND VALUES:

  CMDS: A string that may have one more NetLogo commands
  RESULT: undefined

DESCRIPTION:

  RUN-COMMANDS will take NetLogo commands, put them through the various
  stages need to turn them into Common Lisp code, and run it."
 (eval (clnl-transpiler:transpile-commands (clnl-parser:parse (clnl-lexer:lex cmds)))))

(defun run-reporter (reporter)
 "RUN-REPORTER REPORTER => RESULT

ARGUMENTS AND VALUES:

  REPORTER: A string that should have only one reporter
  RESULT: The value reported by the NVM

DESCRIPTION:

  RUN-REPORTER will take a NetLogo REPORTER, put it through the various
  stages need to turn them into Common Lisp code, run it, and return the RESULT."
 (eval (clnl-transpiler:transpile-reporter (car (clnl-parser:parse (clnl-lexer:lex reporter))))))

; Everything gets tied together here
; The intention of this method is to generate the common lisp equivalent of a model file,
; such that if you decided to no longer use nlogo, you could use the engine without it.
(defun model->lisp (model)
 `(let
   ,(clnl-model:globals model)
   (clnl-random:set-seed 15) ; should the seed always be 15?
   (clnl-nvm:create-world :dims ',(clnl-model:world-dimensions model))
   (clnl-interface:initialize :dims ',(clnl-model:world-dimensions model))))
