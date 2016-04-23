(in-package #:clnl-code-parser)

; This is different from the general parser (in clnl-parser) in that
; it's made for parsing the code section of nlogo files, and so works
; outside of the constraints.  In NetLogo, I believe this is analagous
; to the StructureParser, but I'm guessing there's weird overlap with
; other things

(defvar *dynamic-prims* nil)
(defun global->prim (global) (list :name global))

(defun parse (lexed-ast &optional external-globals)
 "PARSE LEXED-AST &optional EXTERNAL-GLOBALS => AST

ARGUMENTS AND VALUES:

  LEXED-AST: An ambigious ast
  EXTERNAL-GLOBALS: A list of symbols in keyword package
  AST: An unambigious ast that represents the code block of a model

DESCRIPTION:

  PARSE takes a ambigious LEXED-AST and converts it to an unambigious one.

  EXTERNAL-GLOBALS is a list of symbols representing global variables that
  are not defined within the code.  Normally these come from widgets defined
  in the model file, but could arguably come from elsewhere.

  This parser, unlike CLNL-PARSE:PARSE, should not be fed into the transpiler.

  Rather, the ast that's returned can be queried with other functions included
  in the CLNL-CODE-PARSER package to tease out necessary information.  Some of
  those things will involve code blocks that can then be transpiled."
 (let
  ((*dynamic-prims* (mapcar #'global->prim external-globals)))
  (parse-internal lexed-ast)))

(defun parse-internal (lexed-ast)
 (cond
  ((not lexed-ast) nil)
  ((eql :to (car lexed-ast)) (parse-procedure lexed-ast))
  ((find (car lexed-ast) '(:globals :turtles-own :patches-own))
   (parse-with-unevaluated-list lexed-ast))
  ((eql (car lexed-ast) :breed) (parse-breed lexed-ast))))

; Due to the non expression style syntax of procedures, this must be special cased
(defun parse-procedure (tokens)
 (multiple-value-bind (in-block after-block) (find-end tokens)
  (cons
   (list
    (first in-block)
    (second in-block)
    (clnl-parser:parse (cddr in-block) *dynamic-prims*))
   (parse-internal after-block))))

(defun find-end (tokens)
 (cond
  ((not tokens) (error "Failed to find end"))
  ((eql :end (car tokens)) (values nil (cdr tokens)))
  (t (multiple-value-bind (in-block after-block) (find-end (cdr tokens))
      (values (cons (car tokens) in-block) after-block)))))

; This is a special case but left with a little wiggle room for future
; enhancements, like code blocks
(defun parse-with-unevaluated-list (lexed-ast)
 (when (not (eql :[ (cadr lexed-ast))) (error "Expected list literal here"))
 (multiple-value-bind (in-list after-list) (find-closing-bracket (cddr lexed-ast))
  (cons
   (list (car lexed-ast) (cons :list-literal in-list))
   (let
    ((*dynamic-prims* (append (mapcar #'global->prim in-list) *dynamic-prims*)))
    (parse-internal after-list)))))

(defun parse-breed (lexed-ast)
 (when (not (eql :[ (cadr lexed-ast))) (error "Expected list literal here"))
 (multiple-value-bind (in-list after-list) (find-closing-bracket (cddr lexed-ast))
  (cons
   (list (car lexed-ast) (cons :list-literal in-list))
   (parse-internal after-list))))

(defun find-closing-bracket (tokens)
 (cond
  ((not tokens) (error "Failed to find a matching closing bracket"))
  ((eql :] (car tokens)) (values nil (cdr tokens)))
  ((eql :[ (car tokens)) (error "Expected name or ]"))
  (t (multiple-value-bind (in-block after-block) (find-closing-bracket (cdr tokens))
      (values (cons (car tokens) in-block) after-block)))))

(defun globals (code-parsed-ast)
 "GLOBALS MODEL => GLOBALS

  GLOBALS: GLOBAL*

ARGUMENTS AND VALUES:

  MODEL: An ast as created by clnl-code-parse:parse
  GLOBAL: A symbol interned in clnl:*model-package*

DESCRIPTION:

  Returns the globals that get declared in the code."
 (mapcar
  (lambda (global) (list (symbol-name global) 0d0))
  (cdr (second (find :globals code-parsed-ast :key #'car)))))