(in-package #:clnl-nvm)

(defun min-pxcor () (getf (clnl-model:world-dimensions *model*) :xmin))
(defun max-pxcor () (getf (clnl-model:world-dimensions *model*) :xmax))
(defun min-pycor () (getf (clnl-model:world-dimensions *model*) :ymin))
(defun max-pycor () (getf (clnl-model:world-dimensions *model*) :ymax))
