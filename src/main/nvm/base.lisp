(in-package #:clnl-nvm)

(defvar *current-id* 0)

(defvar *turtles* nil)
(defvar *turtles-own-vars* nil)
(defvar *patches-own-vars* nil)
(defvar *patches* nil)
(defvar *myself* nil)
(defvar *self* nil)
(defvar *dimensions* nil)
(defvar *globals* nil)
(defvar *topology* :torus)
(defvar *ticks* nil)
(defvar *breeds* nil)

(define-condition stop nil nil)
(define-condition death nil nil)

(defmacro with-stop-handler (&rest forms)
 "MACRO WITH-STOP-HANDLER &rest FORMS => HANDLED-FORM

ARGUMENTS AND VALUES:

  FORMS: body to be handled
  HANDLED-FORM: body with handling

DESCRIPTION:

  WITH-STOP-HANDLER is a convenience macro to handle when
  programs issue a stop condition.  When one does, a simple
  :stop is returned."
 `(handler-case (progn ,@forms) (stop (s) (declare (ignore s)) :stop)))

(defmacro with-stop-and-death-handler (&rest forms)
 `(handler-case
   (progn ,@forms)
   (stop (s) (declare (ignore s)) :stop)
   (death (d) (declare (ignore d)) :death)))

(defmacro defcommand (name args docstring &rest body)
 `(defun ,name ,args ,docstring ,@body :undefined))

(defstruct turtle who breed color heading xcor ycor (label "") label-color size shape own-vars)
(defstruct patch color xcor ycor own-vars turtles)

(defun agentset-list (agentset)
 (cond
  ((eql agentset :turtles) *turtles*)
  ((eql agentset :patches) *patches*)
  ((and (listp agentset) (eql :agentset (car agentset))) (cddr agentset))
  ((find agentset *breeds* :key #'car)
   (remove agentset *turtles* :key #'turtle-breed :test-not #'eql))
  (t (error "Doesn't seem to be an agentset: ~A" agentset))))

(defun agentset-breed (agentset)
 (cond
  ((eql agentset :turtles) :turtles)
  ((eql agentset :patches) :patches)
  ((find agentset *breeds* :key #'car) agentset)
  ((and (listp agentset) (eql :agentset (car agentset))) (second agentset))
  (t (error "Doesn't seem to be an agentset: ~A" agentset))))

(defun list->agentset (list breed)
 (append (list :agentset breed) list))

(defun agentset-p (o)
 (or
  (eql o :turtles)
  (eql o :patches)
  (find o *breeds* :key #'car)
  (and (listp o) (eql :agentset (car o)))))

(defun agent-p (o)
 (or (turtle-p o) (patch-p o)))

(defun breed-p (breed)
 (find breed *breeds* :key #'car))

(defun breed-default-shape (breed)
 (second (find breed *breeds* :key #'car)))

(defsetf breed-default-shape (breed) (shape)
 `(setf (second (find ,breed *breeds* :key #'car)) ,shape))
