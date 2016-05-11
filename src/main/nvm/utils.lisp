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

(defun patch-at (xcor ycor)
 (flet
  ((rnd (d) (truncate (if (< d 0) (- d 0.5d0) (+ d 0.5d0)))))
  (or
   (find-if
    (lambda (patch)
     (and (equalp (patch-xcor patch) (rnd xcor)) (equalp (patch-ycor patch) (rnd ycor))))
    *patches*)
   (error "This shouldn't be possible: ~S ~S ~S" (rnd xcor) (rnd ycor) *patches*))))

(defmacro with-patch-update (turtle &rest forms)
 (let
  ((patch (gensym)) (new-patch (gensym)))
  `(let
    ((,patch (patch-at (turtle-xcor ,turtle) (turtle-ycor ,turtle)))
     (retn (progn ,@forms)))
    (let
     ((,new-patch (patch-at (turtle-xcor ,turtle) (turtle-ycor ,turtle))))
     (when (not (eql ,patch ,new-patch))
      (setf (patch-turtles ,patch) (remove ,turtle (patch-turtles ,patch)))
      (setf (patch-turtles ,new-patch) (nconc (patch-turtles ,new-patch) (list ,turtle))))))))
