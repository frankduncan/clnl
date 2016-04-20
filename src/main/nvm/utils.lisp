(in-package #:clnl-nvm)

(defun min-pxcor () (getf *dimensions* :xmin))
(defun max-pxcor () (getf *dimensions* :xmax))
(defun min-pycor () (getf *dimensions* :ymin))
(defun max-pycor () (getf *dimensions* :ymax))
