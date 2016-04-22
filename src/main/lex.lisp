(in-package #:clnl-lexer)

; I played around with using #'read for netlogo code, which would have been neat.
; However, it provides too many instances where people could inject CL code
; and I wanted to prevent that.  In the end, writing my own lexer became kind of fun.

(defvar *state* :initial)
(defvar *states* :extension-literal)
(defparameter *lexes* nil)

(defun as-symbol (text) (intern (string-upcase text) :keyword))

(defmacro deflex (state match &optional func)
 (let
  ((scanner (gensym)))
  `(let
    ((,scanner (when (stringp ,match) (cl-ppcre:create-scanner ,match))))
    (pushnew
     (list
      (lambda (state text)
       (and
        (eql ,state state)
        (or
         (and (symbolp text) (eql text ,match))
         (and
          ,scanner
          (stringp text)
          (multiple-value-bind (start end) (cl-ppcre:scan ,scanner text)
           (and start end (= 0 start) (/= 0 end)))))))
      (lambda (text) (second (multiple-value-list (cl-ppcre:scan ,scanner text))))
      ,(or func #'as-symbol))
     *lexes*))))

(defun lex (text)
 "LEX TEXT => AST

ARGUMENTS AND VALUES:

  TEXT: Some NetLogo code
  AST: An ambigious AST that can later be parsed

DESCRIPTION:

  LEX lexes NetLogo code.

  LEX checks for some things, in as much as it can without knowing anything
  about some of the backgrounds of NetLogo.  However, it does the first pass
  with as much as it can."
 (if (string= "" text)
  (let
   ((lex (find-if (lambda (f) (funcall f *state* :eof)) *lexes* :from-end t :key #'car)))
   (when lex (list (funcall (third lex) :eof))))
  (let
   ((lex (find-if (lambda (f) (funcall f *state* text)) *lexes* :from-end t :key #'car)))
   (when (not lex) (error "Can't lex this: ~S" text))
   (let
    ((val (funcall (third lex) (subseq text 0 (funcall (cadr lex) text)))))
    (if val
     (cons val (lex (subseq text (funcall (cadr lex) text))))
     (lex (subseq text (funcall (cadr lex) text))))))))

(defun set-state (new-state)
 (setf *state* new-state))

; This part is the actual netlogo spec

(defvar *string-text* "(\\\\\"|\\\\n|\\\\r|\\\\t|\\\\|[^\\r\\n\\\"])")
(defvar *nonnewline_white_space_char* "[ \\t\\b\\012]")
(defvar *letter* "\\w")
(defvar *digit* "\\d")
(defvar *identifier-char* "[\\w\\d_.?=\*!<>:#\+/%$\^'&-]")

;(defvar *extension-literal-depth* 0)
;(defstruct extension-literal text)

;(deflex :initial "{{"
; (lambda (text)
;  (set-state :extension-literal)
;  (as-symbol text)
;  ))

;(deflex :extension-literal "}}"
; (lambda (text)
;  (if (= 0 *extension-literal-depth*)
;      (progn (set-state :initial) text)
;      (progn (decf *extension-literal-depth*) (as-symbol text)))))

;(deflex :extension-literal "{{"
; (lambda (text) (incf *extension-literal-depth*) text))

;(deflex :extension-literal "\n|\r" (lambda () (error "End of line reached unexpectedly")))
;(deflex :extension-literal :eof (lambda () (error "end of file reached unexpectedly")))
;(deflex :extension-literal ".")

(deflex :initial "[,\\{\\}\\[\\]\\(\\)]" #'as-symbol)
(deflex :initial *nonnewline_white_space_char* (constantly nil))
(deflex :initial "\\n|\\r" (constantly nil))
(deflex :initial ";.*[\n\r]?" (constantly nil))
(deflex :initial (format nil "-?\\.?[0-9]~A*" *identifier-char*)
 (lambda (text)
  (let
   ((num?
     (let
      ((*readtable* (copy-readtable nil))
       (*read-eval* nil))
      (read-from-string (format nil "~Ad0" text)))))
   (if (numberp num?) num? (error "Invalid number")))))

(deflex :initial (format nil "~A*" *identifier-char*) #'as-symbol)
(deflex :initial (format nil "\"~A*\"" *string-text*) #'identity)
(deflex :initial (format nil "\"~A*" *string-text*)
 (lambda (text) (declare (ignore text)) (error "Closing double quote is missing")))
;(deflex :initial "." (lambda (text) (error "Non standard character is not allowed")))
