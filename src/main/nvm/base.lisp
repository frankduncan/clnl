(in-package #:clnl-nvm)

(defvar *current-id* 0)

(defvar *turtles* nil)
(defvar *patches* nil)
(defvar *myself* nil)
(defvar *self* nil)
(defvar *dimensions* nil)
(defvar *topology* :torus)
(defvar *ticks* nil)

(defstruct turtle who color heading xcor ycor (label "") (label-color 9.9d0) (size 1d0))
(defstruct patch color xcor ycor)

(defun agentset-list (agentset)
 (cond
  ((eql agentset :turtles) *turtles*)
  ((eql agentset :patches) *patches*)
  ((and (listp agentset) (eql :agentset (car agentset))) (cdr agentset))
  (t (error "Doesn't seem to be an agentset: ~A" agentset))))

(defun list->agentset (list)
 (cons :agentset list))

(defun agentset-p (o)
 (or
  (eql o :turtles)
  (eql o :patches)
  (and (listp o) (eql :agentset (car o)))))

(defun agent-p (o)
 (or (turtle-p o) (patch-p o)))
