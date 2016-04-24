(in-package #:clnl-nvm)

; Implementations of all the things the nvm can do.

(defun show (value)
 "SHOW VALUE => RESULT

ARGUMENTS AND VALUES:

  VALUE: a NetLogo value
  RESULT: undefined

DESCRIPTION:

  A command that prints the given NetLogo value to the command center.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#show"
 (format t "Showing: ~A~%" (dump-object value)))

(defun create-turtle ()
 (setf
  *turtles*
  (nconc
   *turtles*
   (list
    (make-turtle
     :who *current-id*
     :color (coerce (+ 5 (* 10 (clnl-random:next-int 14))) 'double-float)
     :heading (coerce (clnl-random:next-int 360) 'double-float)
     :xcor 0d0
     :ycor 0d0))))
 (incf *current-id*))

(defun die ()
 "DIE => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined, commands don't return

DESCRIPTION:

  The turtle or link dies

  A dead agent ceases to exist. The effects of this include:
  - The agent will not execute any further code.
  - The agent will disappear from any agentsets it was in, reducing the size of those agentsets by one.
  - Any variable that was storing the agent will now instead have nobody in it.
  - If the dead agent was a turtle, every link connected to it also dies.
  - If the observer was watching or following the agent, the observer's perspective resets.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#die"
 (when (not (turtle-p *self*)) (error "Gotta call die in turtle scope, dude (~A)" *self*))
 (setf (turtle-who *self*) -1)
 (setf *turtles* (remove *self* *turtles*)))

(defun turtles ()
 "TURTLES => ALL-TURTLES

ARGUMENTS AND VALUES:

  ALL-TURTLES: a NetLogo agentset, all turtles

DESCRIPTION:

  Reports the agentset consisting of all the turtles.

  This agentset is special in that it represents the living turtles
  each time it's used, so changes depending on the state of the engine.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles"
 *turtles*)

(defun ask (agent-set fn)
 "ASK AGENT-SET FN => RESULT

ARGUMENTS AND VALUES:

  AGENT-SET: a NetLogo agentset
  FN: a function, run on each agent
  RESULT: undefined, commands don't return

DESCRIPTION:

  ASK is equivalent to ask in NetLogo.

  The specified AGENT-SET runs the given FN.  The order in which the agents
  are run is random each time, and only agents that are in the set at the
  beginning of the call.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#ask"
 (let
  ((iter (shufflerator agent-set)))
  (loop
   :for agent := (funcall iter)
   :while agent
   :do (let ((*myself* *self*) (*self* agent)) (funcall fn)))))

(defun shufflerator (agent-set)
 (let
  ((copy (copy-list agent-set))
   (i 0)
   (agent nil))
  (flet
   ((fetch ()
     (let
      ((idx (when (< i (1- (length copy))) (+ i (clnl-random:next-int (- (length copy) i))))))
      (when idx (setf agent (nth idx copy)))
      (when idx (setf (nth idx copy) (nth i copy)))
      (incf i))))
   (fetch) ; we pre-fetch because netlogo does, rng sync hype!
   (lambda ()
    (cond
     ((> i (length copy)) nil)
     ((= i (length copy)) (incf i) (car (last copy)))
     (t (let ((result agent)) (fetch) result)))))))

(defun random-float (n)
 "RANDOM-FLOAT N => RANDOM-NUMBER

ARGUMENTS AND VALUES:

  N: a double, the upper bound of the random float
  RANDOM-NUMBER: a double, the random result

DESCRIPTION:

  Returns a random number strictly closer to zero than N.

  If number is positive, returns a random floating point number greater than
  or equal to 0 but strictly less than number.

  If number is negative, returns a random floating point number less than or equal
  to 0, but strictly greater than number.

  If number is zero, the result is always 0.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-float"
 (clnl-random:next-double n))

(defun jump (n)
 (when (not (turtle-p *self*)) (error "Gotta call fd in turtle scope, dude (~A)" *self*))
 (setf
  (turtle-xcor *self*)
  (wrap-x *topology*
   (+ (turtle-xcor *self*) (* n (strictmath:sin (strictmath:to-radians (turtle-heading *self*)))))))
 (setf
  (turtle-ycor *self*)
  (wrap-y *topology*
   (+ (turtle-ycor *self*) (* n (strictmath:cos (strictmath:to-radians (turtle-heading *self*))))))))

(defun forward (n)
 "FORWARD N => RESULT

ARGUMENTS AND VALUES:

  N: a double, the amount the turtle moves forward
  RESULT: undefined

DESCRIPTION:

  Moves the current turtle forward N steps, one step at a time.

  This moves forward one at a time in order to make the view updates look
  good in the case of a purposefully slow running instance.  If the number
  is negative, the turtle moves backward.

  If the current agent is not a turtle, it raises an error.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#forward"
 (when (not (turtle-p *self*)) (error "Gotta call fd in turtle scope, dude (~A)" *self*))
 (labels
  ((internal (i)
    (cond
     ((< (abs i) 3.2e-15) nil)
     ((< (abs i) 1d0) (jump i))
     (t (jump (if (> i 0d0) 1d0 -1d0)) (internal (- i (if (> i 0d0) 1d0 -1d0)))))))
  (internal n)))

(defun create-turtles (n)
 "CREATE-TURTLES N => RESULT

ARGUMENTS AND VALUES:

  N: an integer, the numbers of turtles to create
  RESULT: undefined

DESCRIPTION:

  Creates number new turtles at the origin.

  New turtles have random integer headings and the color is randomly selected
  from the 14 primary colors.  If commands are supplied, the new turtles
  immediately run them (unimplemented).

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#create-turtles"
 (loop :for i :from 1 :to n :do (create-turtle)))

(defun create-world (&key dims)
 "CREATE-WORLD &key DIMS => RESULT

  DIMS: (:xmin XMIN :xmax XMAX :ymin YMIN :ymax YMAX)

ARGUMENTS AND VALUES:

  RESULT: undefined
  XMIN: An integer representing the minimum patch coord in X
  XMAX: An integer representing the maximum patch coord in X
  YMIN: An integer representing the minimum patch coord in Y
  YMAX: An integer representing the maximum patch coord in Y

DESCRIPTION:

  Initializes the world in the NVM.

  This should be called before using the engine in any real capacity.  If
  called when an engine is already running, it may do somethign weird."
 (setf *dimensions* dims)
 (setf *turtles* nil)
 (setf *current-id* 0))

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

(defmethod dump-object ((o string)) o)

(defmethod dump-object ((o (eql t))) "true")
(defmethod dump-object ((o (eql nil))) "false")

(defun current-state ()
 "CURRENT-STATE => WORLD-STATE

ARGUMENTS AND VALUES:

  WORLD-STATE: A list, the current state of the whole world

DESCRIPTION:

  Dumps out the state of the world.

  This is useful for visualizations and also storing in a common lisp
  data structure for easy usage in a common lisp instance.  It's preferable
  to use this when working with the nvm than the output done by export-world.

  Currently this only dumps out turtle information.

  This is called CURRENT-STATE because export-world is an actual primitive
  used by NetLogo."
 (mapcar
  (lambda (turtle)
   (list
    :color (turtle-color turtle)
    :xcor (turtle-xcor turtle)
    :ycor (turtle-ycor turtle)
    :heading (turtle-heading turtle)))
  *turtles*))

(defun export-patches ()
 (list
  "\"pxcor\",\"pycor\",\"pcolor\",\"plabel\",\"plabel-color\""
  "\"-1\",\"1\",\"0\",\"\"\"\"\"\",\"9.9\""
  "\"0\",\"1\",\"0\",\"\"\"\"\"\",\"9.9\""
  "\"1\",\"1\",\"0\",\"\"\"\"\"\",\"9.9\""
  "\"-1\",\"0\",\"0\",\"\"\"\"\"\",\"9.9\""
  "\"0\",\"0\",\"0\",\"\"\"\"\"\",\"9.9\""
  "\"1\",\"0\",\"0\",\"\"\"\"\"\",\"9.9\""
  "\"-1\",\"-1\",\"0\",\"\"\"\"\"\",\"9.9\""
  "\"0\",\"-1\",\"0\",\"\"\"\"\"\",\"9.9\""
  "\"1\",\"-1\",\"0\",\"\"\"\"\"\",\"9.9\""))

(defun export-world ()
 "EXPORT-WORLD => WORLD-CSV

ARGUMENTS AND VALUES:

  WORLD-CSV: A string, the csv of the world

DESCRIPTION:

  Dumps out a csv matching NetLogo's export world.

  This is useful for serializing the current state of the engine in order
  to compare against NetLogo or to reimport later.  Contains everything needed
  to boot up a NetLogo instance in the exact same state."
 (format nil "~{~A~%~}"
  (list
   (format nil "~S" "RANDOM STATE")
   (format nil "~S" (clnl-random:export))
   ""
   (format nil "~S" "GLOBALS")
   (format nil "~A~A"
    "\"min-pxcor\",\"max-pxcor\",\"min-pycor\",\"max-pycor\",\"perspective\",\"subject\","
    "\"nextIndex\",\"directed-links\",\"ticks\",")
   (format nil "\"~A\",\"~A\",\"~A\",\"~A\",\"0\",\"nobody\",\"~A\",\"\"\"NEITHER\"\"\",\"-1\""
    (min-pxcor) (max-pxcor) (min-pycor) (max-pycor) *current-id*)
   ""
   (format nil "~S" "TURTLES")
   (format nil "~A~A"
    "\"who\",\"color\",\"heading\",\"xcor\",\"ycor\",\"shape\",\"label\",\"label-color\","
    "\"breed\",\"hidden?\",\"size\",\"pen-size\",\"pen-mode\"")
   (format nil "~{~A~%~}"
    (mapcar
     (lambda (turtle)
      (format nil
       "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",~A"
       (turtle-who turtle)
       (dump-object (turtle-color turtle))
       (dump-object (turtle-heading turtle))
       (dump-object (turtle-xcor turtle))
       (dump-object (turtle-ycor turtle))
       "\"\"\"default\"\"\",\"\"\"\"\"\",\"9.9\",\"{all-turtles}\",\"false\",\"1\",\"1\",\"\"\"up\"\"\""))
     *turtles*))
   (format nil "~S" "PATCHES")
   (format nil "~{~A~^~%~}" (export-patches))
   ""
   (format nil "~S" "LINKS")
   "\"end1\",\"end2\",\"color\",\"label\",\"label-color\",\"hidden?\",\"breed\",\"thickness\",\"shape\",\"tie-mode\""
   "")))
