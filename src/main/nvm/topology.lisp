(in-package #:clnl-nvm)

(defun wrap (pos min max)
 (cond
  ((>= pos max) (+ min (mod (- pos max) (- max min))))
  ((< pos min)
   (let
    ((res (- max (mod (- min pos) (- max min)))))
    (if (< res max) res min))) ; If d is infinitesimal, may return max, which would be bad :(
  (t pos)))

(defgeneric wrap-x (topology x))
(defgeneric wrap-y (topology y))

; Torus implementations
(defmethod wrap-x ((topology (eql :torus)) x)
 (wrap x (- (min-pxcor) 0.5d0) (+ (max-pxcor) 0.5d0)))

(defmethod wrap-y ((topology (eql :torus)) y)
 (wrap y (- (min-pycor) 0.5d0) (+ (max-pycor) 0.5d0)))
