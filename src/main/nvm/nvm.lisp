(in-package #:clnl-nvm)

(defun lookup-color (color)
 "LOOKUP-COLOR COLOR => COLOR-NUMBER

ARGUMENTS AND VALUES:

  COLOR: a symbol representing a color
  COLOR-NUMBER: the NetLogo color integer

DESCRIPTION:

  Returns the number used to represent colors in NetLogo.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#Constants"
 (case color
  (:black 0d0)
  (:gray 5d0)
  (:white 9.9d0)
  (:red 15d0)
  (:orange 25d0)
  (:brown 35d0)
  (:yellow 45d0)
  (:green 55d0)
  (:lime 65d0)
  (:turquoise 75d0)
  (:cyan 85d0)
  (:sky 95d0)
  (:blue 105d0)
  (:violet 115d0)
  (:magenta 125d0)
  (:pink 135d0)))

(defun create-turtle (breed &optional base-turtle)
 (let*
  ((breed (or breed (and base-turtle (turtle-breed base-turtle)) :turtles))
   (new-turtle (make-turtle
                :who (coerce *current-id* 'double-float)
                :color (if base-turtle
                        (turtle-color base-turtle)
                        (coerce (+ 5 (* 10 (clnl-random:next-int 14))) 'double-float))
                :heading (if base-turtle
                          (turtle-heading base-turtle)
                          (coerce (clnl-random:next-int 360) 'double-float))
                :label-color (if base-turtle (turtle-label-color base-turtle) 9.9d0)
                :size (if base-turtle (turtle-size base-turtle) 1d0)
                :breed breed
                :shape (breed-default-shape breed)
                :xcor (if base-turtle (turtle-xcor base-turtle) 0d0)
                :ycor (if base-turtle (turtle-ycor base-turtle) 0d0)
                :own-vars (when base-turtle (copy-list (turtle-own-vars base-turtle))))))
  (let
   ((patch (patch-at (turtle-xcor new-turtle) (turtle-ycor new-turtle))))
   (setf (patch-turtles patch) (nconc (patch-turtles patch) (list new-turtle))))
  (setf *turtles* (nconc *turtles* (list new-turtle)))
  (incf *current-id*)
  new-turtle))

(defun shufflerator (agentset-list)
 (let
  ((copy (copy-list agentset-list))
   (i 0)
   (agent nil))
  (labels
   ((fetch ()
     (let
      ((idx (when (< i (1- (length copy))) (+ i (clnl-random:next-int (- (length copy) i))))))
      (when idx (setf agent (nth idx copy)))
      (when idx (setf (nth idx copy) (nth i copy)))
      (incf i)
      (when (and (<= i (length copy)) (turtle-p agent) (= -1 (turtle-who agent))) (fetch)))))
   (fetch) ; we pre-fetch because netlogo does, rng sync hype!
   (lambda ()
    (cond
     ((> i (length copy)) nil)
     ((= i (length copy)) (incf i) (car (last copy)))
     (t (let ((result agent)) (fetch) result)))))))

(defun create-world (&key dims globals turtles-own-vars patches-own-vars breeds)
 "CREATE-WORLD &key DIMS GLOBALS TURTLES-OWN-VARS PATCHES-OWN-VARS BREEDS => RESULT

  DIMS: (:xmin XMIN :xmax XMAX :ymin YMIN :ymax YMAX)
  GLOBALS: GLOBAL*
  TURTLES-OWN-VARS: TURTLES-OWN-VAR*
  PATCHES-OWN-VARS: PATCHES-OWN-VAR*
  BREEDS: BREED*
  GLOBAL: (GLOBAL-NAME GLOBAL-ACCESS-FUNC)

ARGUMENTS AND VALUES:

  RESULT: undefined
  XMIN: An integer representing the minimum patch coord in X
  XMAX: An integer representing the maximum patch coord in X
  YMIN: An integer representing the minimum patch coord in Y
  YMAX: An integer representing the maximum patch coord in Y
  TURTLES-OWN-VAR: Symbol for the turtles own variable in the keyword package
  PATCHES-OWN-VAR: Symbol for the patches own variable in the keyword package
  BREED: A list of symbols representing the possible preeds
  GLOBAL-NAME: Symbol for the global in the keyword package
  GLOBAL-ACCESS-FUNC: Function to get the value of the global

DESCRIPTION:

  Initializes the world in the NVM.

  This should be called before using the engine in any real capacity.  If
  called when an engine is already running, it may do somethign weird."
 (setf *turtles-own-vars* turtles-own-vars)
 (setf *patches-own-vars* patches-own-vars)
 (setf *dimensions* dims)
 (setf *globals* globals)
 (setf *breeds*
  (append
   (list (list :turtles "default"))
   (mapcar (lambda (breed) (list breed "default")) breeds)))
 (clear-ticks)
 (clear-patches)
 (clear-turtles))

(defun current-state ()
 "CURRENT-STATE => WORLD-STATE

ARGUMENTS AND VALUES:

  WORLD-STATE: A list, the current state of the whole world

DESCRIPTION:

  Dumps out the state of the world.

  This is useful for visualizations and also storing in a common lisp
  data structure for easy usage in a common lisp instance.  It's preferable
  to use this when working with the nvm than the output done by export-world.

  Currently this only dumps out turtle and patch information.

  This is called CURRENT-STATE because export-world is an actual primitive
  used by NetLogo."
 (list
  (mapcar
   (lambda (turtle)
    (list
     :color (turtle-color turtle)
     :xcor (turtle-xcor turtle)
     :ycor (turtle-ycor turtle)
     :heading (turtle-heading turtle)
     :shape (turtle-shape turtle)
     :size (turtle-size turtle)))
   *turtles*)
  (mapcar
   (lambda (patch)
    (list
     :color (patch-color patch)
     :xcor (patch-xcor patch)
     :ycor (patch-ycor patch)))
   *patches*)))

; These match netlogo's dump
(defgeneric dump-object (o))

(defmethod dump-object ((n double-float))
 (multiple-value-bind (int rem) (floor n)
  (if (eql 0d0 rem)
   (format nil "~A" int)
   (let
    ((output (format nil "~D" n)))
    ; Someday we'll have d<posint>, but this is not that day!
    (cl-ppcre:regex-replace "d-" (cl-ppcre:regex-replace "d0" output "") "E-")))))

(defmethod dump-object ((o string)) (format nil "~A" (cl-ppcre:regex-replace-all "\"" (format nil "~S" o) "\"\"")))

(defmethod dump-object ((o (eql t))) "true")
(defmethod dump-object ((o (eql nil))) "false")

(defmethod dump-object ((o list))
 (cond
  ((agentset-p o) (format nil "(agentset, ~A ~A)" (dump-object (count o)) (string-downcase (agentset-breed o))))
  (t (format nil "[~{~A~^ ~}]" (mapcar #'dump-object o)))))

(defmethod dump-object ((o patch))
 (format nil "(patch ~A ~A)" (dump-object (patch-xcor o)) (dump-object (patch-ycor o))))

(defmethod dump-object ((o turtle)) (format nil "(turtle ~A)" (dump-object (turtle-who o))))
(defmethod dump-object ((o (eql :nobody))) (format nil "nobody"))
(defmethod dump-object ((o (eql :turtles))) (format nil "{all-turtles}"))
(defmethod dump-object ((o symbol))
 (cond
  ((find o *breeds* :key #'car) (format nil "{breed ~(~A~)}" o))
  (t (error "Keyword unrecognized by dump object: ~A" o))))
