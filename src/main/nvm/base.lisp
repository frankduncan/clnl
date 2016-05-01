(in-package #:clnl-nvm)

(defvar *current-id* 0)

(defvar *turtles* nil)
(defvar *patches* nil)
(defvar *myself* nil)
(defvar *self* nil)
(defvar *dimensions* nil)
(defvar *topology* :torus)
(defvar *ticks* nil)
(defvar *breeds* nil)

(define-condition stop nil nil)

(defmacro with-stop-handler (&rest forms)
 "MACRO WITH-STOP-HANDLER &rest FORMS => HANDLED-FORM

ARGUMENTS AND VALUES:

  FORMS: body to be handled
  HANDLED-FORM: body with handling

DESCRIPTION:

  WITH-STOP-HANDLER is a convenience macro to handle when
  programs issue a stop condition.  When one does, a simple
  :stop is returned."
 `(handler-case (progn ,@forms) (stop (s) :stop)))

(defstruct turtle who color heading xcor ycor (label "") (label-color 9.9d0) (size 1d0) shape)
(defstruct patch color xcor ycor)

(defun agentset-list (agentset)
 (cond
  ((eql agentset :turtles) *turtles*)
  ((eql agentset :patches) *patches*)
  ((and (listp agentset) (eql :agentset (car agentset))) (cddr agentset))
  (t (error "Doesn't seem to be an agentset: ~A" agentset))))

(defun agentset-breed (agentset)
 (cond
  ((eql agentset :turtles) :turtles)
  ((eql agentset :patches) :patches)
  ((and (listp agentset) (eql :agentset (car agentset))) (second agentset))
  (t (error "Doesn't seem to be an agentset: ~A" agentset))))

(defun list->agentset (list breed)
 (append (list :agentset breed) list))

(defun agentset-p (o)
 (or
  (eql o :turtles)
  (eql o :patches)
  (and (listp o) (eql :agentset (car o)))))

(defun agent-p (o)
 (or (turtle-p o) (patch-p o)))

(defun breed-p (breed)
 (find breed *breeds* :key #'car))

(defun breed-default-shape (breed)
 (second (find breed *breeds* :key #'car)))

(defsetf breed-default-shape (breed) (shape)
 `(setf (second (find ,breed *breeds* :key #'car)) ,shape))
