(in-package #:clnl-nvm)

(defvar *current-id* 0)

(defvar *turtles* nil)
(defvar *patches* nil)
(defvar *myself* nil)
(defvar *self* nil)
(defvar *dimensions* nil)
(defvar *topology* :torus)

(defstruct turtle who color heading xcor ycor)
(defstruct patch color xcor ycor)
