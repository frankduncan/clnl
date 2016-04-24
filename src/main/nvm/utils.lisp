(in-package #:clnl-nvm)

(defun min-pxcor () (getf *dimensions* :xmin))
(defun max-pxcor () (getf *dimensions* :xmax))
(defun min-pycor () (getf *dimensions* :ymin))
(defun max-pycor () (getf *dimensions* :ymax))

(defvar *cached-sins*
 (loop
  :for i :from 0 :to 360
  :collect
  (let
   ((potential-sin (strictmath:sin (strictmath:to-radians i))))
   (if (< (abs potential-sin) 3.2d-15) 0d0 potential-sin))))

(defun using-cached-sin (n)
 (if (= (floor n) n) (nth (floor n) *cached-sins*) (strictmath:sin (strictmath:to-radians n))))

(defvar *cached-coses*
 (loop
  :for i :from 0 :to 360
  :collect
  (let
   ((potential-cos (strictmath:cos (strictmath:to-radians i))))
   (if (< (abs potential-cos) 3.2d-15) 0d0 potential-cos))))

(defun using-cached-cos (n)
 (if (= (floor n) n) (nth (floor n) *cached-coses*) (strictmath:cos (strictmath:to-radians n))))
