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

(defun agent-set-list (agent-set)
 agent-set)
