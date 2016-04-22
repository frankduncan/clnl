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
(defvar *in-structure* nil)

(defun prim-name (prim) (getf prim :name))
(defun prim-num-args (prim) (length (getf prim :args)))
(defun prim-args (prim) (getf prim :args))
(defun prim-in-structure (prim) (getf prim :in-structure))

(defun find-prim (symb) (find symb *prims* :key #'prim-name))

; Make this only as complicated as it needs to be, letting it grow
; as we take on more and more of the language
(defun parse (lexed-ast &optional structure)
 "PARSE LEXED-AST &optional STRUCTURE => AST

ARGUMENTS AND VALUES:

  LEXED-AST: An ambigious ast
  STRUCTURE: A boolean
  AST: An unambigious ast that can be transpiled

DESCRIPTION:

  PARSE takes a ambigious LEXED-AST and converts it to an unambigious one.

  When STRUCTURE is true, parse is done with the expanded indentifier set used
  in NetLogo files, as well as pulling out procedure definitions.

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
  ((*in-structure* structure))
  (parse-internal lexed-ast)))

(defun parse-internal (lexed-ast)
 (cond
  ((not lexed-ast) nil)
  ((numberp (car lexed-ast)) (cons (coerce (car lexed-ast) 'double-float) (parse-internal (cdr lexed-ast))))
  ((eql :[ (car lexed-ast)) (parse-block (cdr lexed-ast)))
  ((eql :to (car lexed-ast)) (parse-procedure lexed-ast))
  ((and (symbolp (car lexed-ast)) (find-prim (car lexed-ast)))
   (let
    ((prim (find-prim (car lexed-ast))))
    (when (and (not *in-structure*) (prim-in-structure prim))
     (error "This doesn't make sense here"))
    (if
     (and (= (prim-num-args prim) 1) (eql :unevaluated-list (car (prim-args prim))))
     (parse-prim-with-unevaluated-list prim lexed-ast)
     (parse-prim-normally prim lexed-ast))))
  (t (error "Couldn't parse ~S" lexed-ast))))

; This is a special case but left with a little wiggle room for future
; enhancements, like code blocks
(defun parse-prim-with-unevaluated-list (prim lexed-ast)
 (when (not (eql :[ (cadr lexed-ast)))
  (error "Expected list literal here"))
 (multiple-value-bind (in-list after-list) (find-closing-bracket (cddr lexed-ast))
  (cons
   (list (prim-name prim) (cons :list-literal in-list))
   (parse-internal after-list))))

(defun parse-prim-normally (prim lexed-ast)
 (let
  ((num-args (prim-num-args prim))
   (parsed-remainder (parse-internal (cdr lexed-ast))))
  (cons
   (cons
    (prim-name prim)
    (mapcar
     #'help-arg
     (prim-args prim)
     (butlast parsed-remainder (- (length parsed-remainder) num-args))))
   (nthcdr num-args parsed-remainder))))

(defun help-arg (arg-type arg)
 (case arg-type
  (:command-block
   (if (not (and (consp arg) (eql 'block (car arg))))
    (error "Required a block, but found a ~A" arg)
    (cons :command-block (cdr arg))))
  (:list
   (if (and (consp arg) (eql 'block (car arg)))
    (cons :list-literal (cdr arg))
    arg))
  (t arg)))

(defun parse-block (tokens)
 (multiple-value-bind (in-block after-block) (find-closing-bracket tokens)
  (cons
   (cons
    'block
    (parse-internal in-block))
   (parse-internal after-block))))

(defun find-closing-bracket (tokens &optional (depth 0))
 (cond
  ((not tokens) (error "Failed to find a matching closing bracket"))
  ((and (eql :] (car tokens)) (= depth 0)) (values nil (cdr tokens)))
  (t (multiple-value-bind
      (in-block after-block)
      (find-closing-bracket (cdr tokens) (case (car tokens) (:[ (1+ depth)) (:] (1- depth)) (t depth)))
      (values (cons (car tokens) in-block) after-block)))))

; Due to the non expression style syntax of procedures, this must be special cased
(defun parse-procedure (tokens)
 (when (not *in-structure*) (error "This doesn't make sense here"))
 (multiple-value-bind (in-block after-block) (find-end tokens)
  (declare (ignore in-block))
  (cons
   (cons
    (car tokens)
    nil) ; Update this to parsing the internal of the inblock)
   (parse-internal after-block))))

(defun find-end (tokens)
 (cond
  ((not tokens) (error "Failed to find end"))
  ((eql :end (car tokens)) (values nil (cdr tokens)))
  (t (multiple-value-bind (in-block after-block) (find-end (cdr tokens))
      (values (cons (car tokens) in-block) after-block)))))

; Used to populate dynamic-prims
(defun determine-procedure-definition (tokens)
 (declare (ignore tokens)))

(defmacro defprim (name args)
 `(push
   (list :name ,name :args ',args)
   *prims*))

(defmacro defstructureprim (name args)
 `(push
   (list :name ,name :args ',args :in-structure t)
   *prims*))

; This list of prims will get combined with the mapping to actual code later
; Current list of argument types we accept:
; - :number
; - :agentset
; - :command-block
; - t - any type
(defprim :ask (:agentset :command-block))
(defprim :crt (:number))
(defprim :fd (:number))
(defprim :random-float (:number))
(defprim :show (t))
(defprim :turtles ())

(defstructureprim :globals (:unevaluated-list))
(defstructureprim :breed (:unevaluated-list))
(defstructureprim :turtles-own (:unevaluated-list))
(defstructureprim :patches-own (:unevaluated-list))
