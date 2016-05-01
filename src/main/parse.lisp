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
  DYNAMIC-PRIM: (:name NAME :args ARGS :infix INFIX)
  ARGS: ARG*

ARGUMENTS AND VALUES:

  LEXED-AST: An ambigious ast
  AST: An unambigious ast that can be transpiled
  NAME: A symbol in the keyword package
  INFIX: Boolean denoting whether the prim is infix
  ARG: A list of symbols denoting the type of argument

DESCRIPTION:

  PARSE takes a ambigious LEXED-AST and converts it to an unambigious one.

  DYNAMIC-PRIMS that are passed in are used to avoid compilation errors on
  things not statically defined by the NetLogo language, be they user defined
  procedures or generated primitives from breed declarations.

  The possible values for ARG are :agentset, :boolean, :number, :command-block,
  or t for wildcard.

  The need for a parser between the lexer and the transpiler is because NetLogo
  needs two passes to turn into something that can be used.  This is the only entry
  point into this module, and should probably remain that way.

  There's also a lot of error checking that the LEXED-AST even makes sense, even
  though the lexer obviously thought it did.

  Examples are too numerous and varied, but by inserting an output between
  the lexer and this code, a good idea of what goes on can be gotten."
 (let
  ; could have defined this using the special variable, but didn't to make the
  ; function definition simpler, as well as the documentation.
  ((*dynamic-prims* dynamic-prims))
  (parse-internal lexed-ast)))

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
     (cons
      (prim-name prim)
      (append
       (when (prim-is-infix prim) (list (second (help-arg prev-item (car (prim-args prim))))))
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
    (car parsed-in-block))
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

(defmacro defprim (name args &optional infix)
 `(push
   (list :name ,name :args ',args :infix ,infix)
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
(defprim := (t t) :infix)
(defprim :!= (t t) :infix)
(defprim :- (:number :number) :infix)
(defprim :* (:number :number) :infix)
(defprim :+ (:number :number) :infix)
(defprim :/ (:number :number) :infix)
(defprim :< (:number :number) :infix)
(defprim :<= (:number :number) :infix)
(defprim :any? (:agentset))
(defprim :ask (:agentset :command-block))
(defprim :clear-all ())
(defprim :crt (:number (:command-block :optional)))
(defprim :color ())
(defprim :count (:agentset))
(defprim :die ())
(defprim :display ())
(defprim :with (:agentset :reporter-block) :infix)
(defprim :fd (:number))
(defprim :hatch (:number :command-block))
; (defprim :let (t t)) ; keeping this here, commented out, to note that it has special processing
(defprim :if (:boolean :command-block))
(defprim :if-else (:boolean :command-block :command-block))
(defprim :ifelse (:boolean :command-block :command-block))
(defprim :label ())
(defprim :label-color ())
(defprim :not (:boolean))
(defprim :nobody ())
(defprim :one-of ((:agentset :list)))
(defprim :of (:reporter-block :agentset) :infix)
(defprim :patches ())
(defprim :pcolor ())
(defprim :random (:number))
(defprim :random-float (:number))
(defprim :random-xcor ())
(defprim :random-ycor ())
(defprim :round ())
(defprim :reset-ticks ())
(defprim :lt (:number))
(defprim :rt (:number))
(defprim :set (t t))
(defprim :set-default-shape (t t))
(defprim :setxy (:number :number))
(defprim :show (t))
(defprim :size ())
(defprim :stop ())
(defprim :tick ())
(defprim :ticks ())
(defprim :turtles ())
(defprim :who ())

; colors
(defprim :black ())
(defprim :blue ())
(defprim :brown ())
(defprim :green ())
(defprim :white ())

(defstructureprim :globals)
(defstructureprim :breed)
(defstructureprim :turtles-own)
(defstructureprim :patches-own)
(defstructureprim :to)
(defstructureprim :to-report)
