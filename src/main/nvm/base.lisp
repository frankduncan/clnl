(in-package #:clnl-nvm)

(defvar *current-id* 0)

(defvar *turtles* nil)
(defvar *myself* nil)
(defvar *self* nil)
(defvar *dimensions* nil)
(defvar *topology* :torus)

(defstruct turtle who color heading xcor ycor)
