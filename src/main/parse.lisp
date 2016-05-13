(in-package #:clnl-parser)

; Ok, after thinking about this a little, the parser is completely contextual
; based on what has come before.  We can't do a contextless parsing, like we
; could in other languages, due to amiguity about reporters vs reporter tasks
;
; So, for instance, we could have:
;   x + y => (+ x y)
;   x + y => (x (task +) y)
; So the definition of "+" is completely dependent on the nature of x
;
; The goal of this parser should be to turn in the amiguous lexed ast representing
; NetLogo into an unambigious S-expression, and nothing more, so things like
; expectation of commands being the first symbol is not be necessary until later
;
; In general, the parser will:
;  * Parse the structure of the lexed output first
;  * Parse the structure of the individual expressions (finding ('s and ['s and doing the right thing)
;  * Coalate things into an unambigious expressions
;  * Then we're done, let someone else make it evaluatable
;    - We don't really care if things are commands or reporters right now

(defparameter *prims* nil)
; prims that are created when compiling the netlogo file
; usually via procedures or top level things like breed declarations
(defparameter *dynamic-prims* nil)

(defun prim-name (prim) (getf prim :name))
(defun prim-precedence (prim) (getf prim :precedence))
(defun prim-args (prim) (getf prim :args))
(defun prim-structure-prim (prim) (getf prim :structure-prim))
(defun prim-is-infix (prim) (getf prim :infix))

(defun find-prim (symb)
 (or
  (find symb *prims* :key #'prim-name)
  (find symb *dynamic-prims* :key #'prim-name)))

; Make this only as complicated as it needs to be, letting it grow
; as we take on more and more of the language
(defun parse (lexed-ast &optional dynamic-prims)
 "PARSE LEXED-AST &optional DYNAMIC-PRIMS => AST

  DYNAMIC-PRIMS: DYNAMIC-PRIM*
  DYNAMIC-PRIM: (:name NAME :args ARGS :infix INFIX :precedence PRECEDENCE)
  ARGS: ARG*

ARGUMENTS AND VALUES:

  LEXED-AST: An ambigious ast
  AST: An unambigious ast that can be transpiled
  NAME: A symbol in the keyword package
  INFIX: Boolean denoting whether the prim is infix, defaulting to NIL
  PRECEDENCE: A number, usually 10 for reporters, and 0 for commands
  ARG: A list of symbols denoting the type of argument

DESCRIPTION:

  PARSE takes a ambigious LEXED-AST and converts it to an unambigious one.

  DYNAMIC-PRIMS that are passed in are used to avoid compilation errors on
  things not statically defined by the NetLogo language, be they user defined
  procedures or generated primitives from breed declarations.  NAME and PRECEDENCE
  are required for all dynamic prims.

  PRECEDENCE is a number used to calculate the order of operations.  Higher numbers
  have more precedence than lower ones.  Generally all commands should have the
  lowest precedence, and all reporters should have 10 as the precedence.

  The possible values for ARG are :agentset, :boolean, :number, :command-block,
  or t for wildcard.

  The need for a parser between the lexer and the transpiler is because NetLogo
  needs two passes to turn into something that can be used.  This is the only entry
  point into this module, and should probably remain that way.

  There's also a lot of error checking that the LEXED-AST even makes sense, even
  though the lexer obviously thought it did.

  Examples are too numerous and varied, but by inserting an output between
  the lexer and this code, a good idea of what goes on can be gotten."
 (when (find nil dynamic-prims :key #'prim-name)
  (error "All passed in prims must have a name: ~S" (find nil dynamic-prims :key #'prim-name)))
 (when (find nil dynamic-prims :key #'prim-precedence)
  (error "All passed in prims must have a precedence: ~S" (find nil dynamic-prims :key #'prim-precedence)))
 (let
  ; could have defined this using the special variable, but didn't to make the
  ; function definition simpler, as well as the documentation.
  ((*dynamic-prims* dynamic-prims))
  (remove-parened-forms (parse-internal lexed-ast))))

; This is needed to clean up where we had to note parenthesis wrapped
; things for the purpose of precedence
(defun remove-parened-forms (parsed-ast)
 (cond
  ((not parsed-ast) nil)
  ((and (listp parsed-ast) (eql :parened (car parsed-ast))) (remove-parened-forms (cadr parsed-ast)))
  ((listp parsed-ast) (mapcar #'remove-parened-forms parsed-ast))
  (t parsed-ast)))

(defun parse-internal (lexed-ast &key prev-item prev-remaining-arg remaining-args)
 (let
  ((prim (and lexed-ast (symbolp (car lexed-ast)) (find-prim (car lexed-ast)))))
  (cond
   ((and remaining-args (eql (car remaining-args) :done-with-args))
    (append (when prev-item (list (help-arg prev-item prev-remaining-arg))) lexed-ast))
   ((and prim (prim-is-infix prim))
    (parse-prim prim lexed-ast prev-item prev-remaining-arg remaining-args)) ; Special casing infix prims is cleaner
   (t
    (append
     (when prev-item (list (help-arg prev-item prev-remaining-arg)))
     (cond
      ((not lexed-ast) nil)
      ((stringp (car lexed-ast))
       (parse-internal (cdr lexed-ast)
        :prev-item (car lexed-ast)
        :prev-remaining-arg (car remaining-args)
        :remaining-args (cdr remaining-args)))
      ((numberp (car lexed-ast))
       (parse-internal (cdr lexed-ast)
        :prev-item (coerce (car lexed-ast) 'double-float)
        :prev-remaining-arg (car remaining-args)
        :remaining-args (cdr remaining-args)))
      ((eql (intern "(" :keyword) (car lexed-ast)) (parse-parened-expr (cdr lexed-ast) remaining-args))
      ((eql (intern ")" :keyword) (car lexed-ast)) (error "Closing parens has no opening parens"))
      ((eql :let (car lexed-ast)) (parse-let (cdr lexed-ast) remaining-args))
      ((eql :[ (car lexed-ast)) (parse-block (cdr lexed-ast) remaining-args))
      (prim
       (when (prim-structure-prim prim)
        (error "This doesn't make sense here"))
       (parse-prim prim lexed-ast nil prev-remaining-arg remaining-args))
      (t (error "Couldn't parse ~S" lexed-ast))))))))

(defun parse-let (lexed-ast remaining-args)
 (when (not (keywordp (car lexed-ast))) (error "Needed a keyword for let"))
 (let*
  ((half-parsed-remainder (parse-internal (cdr lexed-ast) :remaining-args (list t :done-with-args))))
  (let
   ((*dynamic-prims* (cons (list :name (car lexed-ast)) *dynamic-prims*)))
   (parse-internal
    (cdr half-parsed-remainder)
    :remaining-args (cdr remaining-args)
    :prev-remaining-arg (car remaining-args)
    :prev-item (list :let (car lexed-ast) (cadr (car half-parsed-remainder)))))))

(defun reconfigure-due-to-precedence (prev-item prim following-args)
 (flet
  ((calculate-precedence (x)
    (or
     (and
      (listp x)
      (< 1 (length prev-item))
      (keywordp (car x))
      (find-prim (car x))
      (prim-precedence (find-prim (car x))))
     20)))
  (cond
   ((<= (prim-precedence prim) (calculate-precedence prev-item))
    (cons
     (prim-name prim)
     (cons
      (second (help-arg prev-item (car (prim-args prim))))
      following-args)))
   (t (append
       (butlast prev-item)
       (list
        (reconfigure-due-to-precedence
         (car (last prev-item))
         prim
         following-args)))))))

(defun parse-prim (prim lexed-ast prev-item prev-remaining-arg remaining-args)
 (let*
  ((args (if (prim-is-infix prim) (cdr (prim-args prim)) (prim-args prim)))
   (half-parsed-remainder (parse-internal (cdr lexed-ast) :remaining-args (append args (list :done-with-args))))
   (breakpoint (or
                (position-if (lambda (form) (or (not (listp form)) (not (eql :arg (car form))))) half-parsed-remainder)
                (length half-parsed-remainder)))
   (already-parsed-limbo-forms
    (subseq half-parsed-remainder breakpoint (min (length args) (length half-parsed-remainder))))
   (middle-forms
    (cons
     (if
      (prim-is-infix prim)
      (reconfigure-due-to-precedence prev-item prim (mapcar #'cadr (subseq half-parsed-remainder 0 breakpoint)))
      (cons
       (prim-name prim)
       (mapcar #'cadr (subseq half-parsed-remainder 0 breakpoint))))
     already-parsed-limbo-forms)))
  (append
   (butlast middle-forms)
   (parse-internal
    (nthcdr (length args) half-parsed-remainder)
    :remaining-args (if (prim-is-infix prim) remaining-args (cdr remaining-args))
    :prev-remaining-arg (if (prim-is-infix prim) prev-remaining-arg (car remaining-args))
    :prev-item (car (last middle-forms))))))

(defun help-arg (arg arg-type)
 (cond
  ((not arg-type) arg)
  ((eql arg-type :command-block)
   (if (not (and (consp arg) (eql 'block (car arg))))
    (error "Required a block, but found a ~A" arg)
    (list :arg (cons :command-block (cdr arg)))))
  ((eql arg-type :reporter-block)
   (if (not (and (consp arg) (eql 'block (car arg))))
    (error "Required a block, but found a ~A" arg)
    (list :arg (cons :reporter-block (cdr arg)))))
  ((or
    (eql arg-type :list)
    (and (listp arg-type) (find :list arg-type)))
   (list
    :arg
    (if (and (consp arg) (eql 'block (car arg)))
     (cons :list-literal (cdr arg))
     arg)))
  ((and
    (listp arg-type)
    (find :command-block arg-type)
    (consp arg)
    (eql 'block (car arg)))
   (list :arg (cons :command-block (cdr arg))))
  ((and (listp arg-type) (find :optional arg-type)) arg)
  (t (list :arg arg))))

(defun parse-block (tokens remaining-args)
 (multiple-value-bind (in-block after-block) (find-closing-bracket tokens)
  (parse-internal after-block
   :prev-item (cons 'block (parse-internal in-block))
   :prev-remaining-arg (car remaining-args)
   :remaining-args (cdr remaining-args))))

(defun find-closing-bracket (tokens &optional (depth 0))
 (cond
  ((not tokens) (error "Failed to find a matching closing bracket"))
  ((and (eql :] (car tokens)) (= depth 0)) (values nil (cdr tokens)))
  (t (multiple-value-bind
      (in-block after-block)
      (find-closing-bracket (cdr tokens) (case (car tokens) (:[ (1+ depth)) (:] (1- depth)) (t depth)))
      (values (cons (car tokens) in-block) after-block)))))

(defun parse-parened-expr (tokens remaining-args)
 (multiple-value-bind (in-block after-block) (find-closing-paren tokens)
  (parse-internal after-block
   :prev-item
   (let
    ((parsed-in-block (parse-internal in-block)))
    (when (/= 1 (length parsed-in-block)) (error "Expected ) here"))
    (list :parened (car parsed-in-block)))
   :prev-remaining-arg (car remaining-args)
   :remaining-args (cdr remaining-args))))

(defun find-closing-paren (tokens &optional (depth 0))
 (cond
  ((not tokens) (error "Failed to find a matching closing bracket"))
  ((and (eql (intern ")" :keyword) (car tokens)) (= depth 0)) (values nil (cdr tokens)))
  (t (multiple-value-bind
      (in-block after-block)
      (find-closing-paren
       (cdr tokens)
       (cond
        ((eql (intern "(" :keyword) (car tokens)) (1+ depth))
        ((eql (intern ")" :keyword) (car tokens)) (1- depth)) (t depth)))
      (values (cons (car tokens) in-block) after-block)))))

(defmacro defprim (name args precedence &rest options)
 `(push
   (list :name ,name :args ',args :infix ,(find :infix options) :precedence ,precedence)
   *prims*))

(defmacro defstructureprim (name)
 `(push
   (list :name ,name :structure-prim t)
   *prims*))

; This list of prims will get combined with the mapping to actual code later
; Current list of argument types we accept:
; - :number
; - :agentset
; - :command-block
; - :boolean
; - t - any type
;
; After the arguments, :infix denotes that it's an :infix operator
;  - Note: Later we should move it to have a list of optional attributes of the primitive
(defprim := (t t) 5 :infix)
(defprim :!= (t t) 5 :infix)
(defprim :- (:number :number) 7 :infix)
(defprim :* (:number :number) 8 :infix)
(defprim :+ (:number :number) 7 :infix)
(defprim :/ (:number :number) 8 :infix)
(defprim :< (:number :number) 6 :infix)
(defprim :<= (:number :number) 6 :infix)
(defprim :any? (:agentset) 10)
(defprim :ask (:agentset :command-block) 0)
(defprim :ca () 0)
(defprim :clear-all () 0)
(defprim :crt (:number (:command-block :optional)) 0)
(defprim :create-turtles (:number (:command-block :optional)) 0)
(defprim :color () 10)
(defprim :count (:agentset) 10)
(defprim :die () 0)
(defprim :display () 0)
(defprim :with (:agentset :reporter-block) 12 :infix)
(defprim :fd (:number) 0)
(defprim :hatch (:number (:command-block :optional)) 0)
(defprim :let (t t) 0) ; while this has special processing, we need a prim for meta information
(defprim :if (:boolean :command-block) 0)
(defprim :if-else (:boolean :command-block :command-block) 0)
(defprim :ifelse (:boolean :command-block :command-block) 0)
(defprim :label () 10)
(defprim :label-color () 10)
(defprim :not (:boolean) 10)
(defprim :nobody () 10)
(defprim :one-of ((:agentset :list)) 10)
(defprim :of (:reporter-block :agentset) 11 :infix)
(defprim :patches () 10)
(defprim :pcolor () 10)
(defprim :random (:number) 10)
(defprim :random-float (:number) 10)
(defprim :random-xcor () 10)
(defprim :random-ycor () 10)
(defprim :round (t) 10)
(defprim :reset-ticks () 0)
(defprim :lt (:number) 0)
(defprim :rt (:number) 0)
(defprim :set (t t) 0)
(defprim :set-default-shape (t t) 0)
(defprim :setxy (:number :number) 0)
(defprim :show (t) 0)
(defprim :size () 10)
(defprim :stop () 0)
(defprim :tick () 0)
(defprim :ticks () 10)
(defprim :turtles () 10)
(defprim :turtles-here () 10)
(defprim :who () 10)

; colors
(defprim :black () 10)
(defprim :blue () 10)
(defprim :brown () 10)
(defprim :green () 10)
(defprim :white () 10)

(defstructureprim :globals)
(defstructureprim :breed)
(defstructureprim :turtles-own)
(defstructureprim :patches-own)
(defstructureprim :to)
(defstructureprim :to-report)
