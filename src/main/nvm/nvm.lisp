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

(defun create-turtle ()
 (setf
  *turtles*
  (nconc
   *turtles*
   (list
    (make-turtle
     :who (coerce *current-id* 'double-float)
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

(defun patches ()
 "PATCHES => ALL-PATCHES

ARGUMENTS AND VALUES:

  ALL-PATCHES: a NetLogo agentset, all patches

DESCRIPTION:

  Reports the agentset consisting of all the patches.

  This agentset is special in that it represents the living patches
  each time it's used, so changes depending on the state of the engine.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#patches"
 :patches)

(defun turtles ()
 "TURTLES => ALL-TURTLES

ARGUMENTS AND VALUES:

  ALL-TURTLES: a NetLogo agentset, all turtles

DESCRIPTION:

  Reports the agentset consisting of all the turtles.

  This agentset is special in that it represents the living turtles
  each time it's used, so changes depending on the state of the engine.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles"
 :turtles)

(defun ask (agent-or-agent-set fn)
 "ASK AGENT-OR-AGENT-SET FN => RESULT

  AGENT-OR-AGENT-SET: AGENT | AGENT-SET

ARGUMENTS AND VALUES:

  FN: a function, run on each agent
  RESULT: undefined, commands don't return
  AGENT: a NetLogo agent
  AGENT-SET: a NetLogo agentset

DESCRIPTION:

  ASK is equivalent to ask in NetLogo.

  The specified AGENT-SET or AGENT runs the given FN.  In the case of an
  AGENT-SET, the order in which the agents are run is random each time,
  and only agents that are in the set at the beginning of the call.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#ask"
 (cond
  ((agent-set-p agent-or-agent-set)
   (let
    ((iter (shufflerator (agent-set-list agent-or-agent-set))))
    (loop
     :for agent := (funcall iter)
     :while agent
     :do (let ((*myself* *self*) (*self* agent)) (funcall fn)))))
  ((agent-p agent-or-agent-set)
   (let ((*myself* *self*) (*self* agent-or-agent-set)) (funcall fn)))
  (t
   (error "Ask requires an agent-set or agent but got: ~A" agent-or-agent-set))))

(defun count (agent-set)
 "COUNT AGENT-SET => N

ARGUMENTS AND VALUES:

  AGENT-SET: a NetLogo agentset
  N: a number

DESCRIPTION:

  COUNT is equivalent to count in NetLogo.  Returns N, the number of
  agents in AGENT-SET.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#count"
 (coerce (length (agent-set-list agent-set)) 'double-float))

(defun of (fn agent-or-agent-set)
 "OF FN AGENT-OR-AGENT-SET => RESULT

  AGENT-OR-AGENT-SET: AGENT | AGENT-SET
  RESULT: RESULT-LIST | RESULT-VALUE

ARGUMENTS AND VALUES:

  FN: a function, run on each agent
  AGENT: a NetLogo agent
  AGENT-SET: a NetLogo agentset
  RESULT-LIST: a list
  RESULT-VALUE: a single value

DESCRIPTION:

  OF is equivalent to of in NetLogo.

  The specified AGENT-SET or AGENT runs the given FN.  In the case of an
  AGENT-SET, the order in which the agents are run is random each time,
  and only agents that are in the set at the beginning of the call.

  RESULT-LIST is returned when the input is an AGENT-SET, but RESULT-VALUE
  is returned when only passed an AGENT.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#of"
 (cond
  ((agent-set-p agent-or-agent-set)
   (let
    ((iter (shufflerator (agent-set-list agent-or-agent-set))))
    (loop
     :for agent := (funcall iter)
     :while agent
     :collect (let ((*myself* *self*) (*self* agent)) (funcall fn)))))
  ((agent-p agent-or-agent-set)
   (let ((*myself* *self*) (*self* agent-or-agent-set)) (funcall fn)))
  (t
   (error "Of requires an agent-set or agent but got: ~A" agent-or-agent-set))))

(defun shufflerator (agent-set-list)
 (let
  ((copy (copy-list agent-set-list))
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

(defun random (n)
 "RANDOM N => RANDOM-NUMBER

ARGUMENTS AND VALUES:

  N: an integer, the upper bound of the random
  RANDOM-NUMBER: an integer, the random result

DESCRIPTION:

  Returns a random number strictly closer to zero than N.

  If number is positive, returns a random integer greater than or equal to 0,
  but strictly less than number.

  If number is negative, returns a random integer less than or equal to 0,
  but strictly greater than number.

  If number is zero, the result is always 0.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#random"
 (coerce (clnl-random:next-long (truncate n)) 'double-float))

(defun random-xcor ()
 "RANDOM-XCOR => RANDOM-NUMBER

ARGUMENTS AND VALUES:

  RANDOM-NUMBER: a float, the random result

DESCRIPTION:

  Returns a random floating point number in the allowable range of turtle
  coordinates along the x axis.

  These range from min-pxcor - 0.5 (inclusive) to max-pxcor + 0.5 (exclusive)

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-cor"
 (let
  ((min (- (min-pxcor) 0.5d0))
   (max (+ (max-pxcor) 0.5d0)))
  (+ min (clnl-random:next-double (- max min)))))

(defun random-ycor ()
 "RANDOM-YCOR => RANDOM-NUMBER

ARGUMENTS AND VALUES:

  RANDOM-NUMBER: a float, the random result

DESCRIPTION:

  Returns a random floating point number in the allowable range of turtle
  coordinates along the y axis.

  These range from min-pycor - 0.5 (inclusive) to max-pycor + 0.5 (exclusive)

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-cor"
 (let
  ((min (- (min-pycor) 0.5d0))
   (max (+ (max-pycor) 0.5d0)))
  (+ min (clnl-random:next-double (- max min)))))

(defun one-of (agent-set)
 "ONE-OF AGENT-SET => RESULT

  RESULT: RANDOM-AGENT | :nobody

ARGUMENTS AND VALUES:

  AGENT-SET: An agent set
  RANDOM-AGENT: an agent if AGENT-SET is non empty

DESCRIPTION:

  From an agentset, returns a random agent. If the agentset is empty, returns nobody.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#one-of"
 (let*
  ((agent-set-list (agent-set-list agent-set))
   (length (length agent-set-list)))
  (if (zerop length) :nobody (nth (clnl-random:next-int length) agent-set-list))))

(defun jump (n)
 (when (not (turtle-p *self*)) (error "Gotta call jump in turtle scope, dude (~A)" *self*))
 (setf
  (turtle-xcor *self*)
  (wrap-x *topology*
   (+ (turtle-xcor *self*) (* n (using-cached-sin (turtle-heading *self*))))))
 (setf
  (turtle-ycor *self*)
  (wrap-y *topology*
   (+ (turtle-ycor *self*) (* n (using-cached-cos (turtle-heading *self*)))))))

(defun setxy (x y)
 "SETXY X Y => RESULT

ARGUMENTS AND VALUES:

  X: a double
  Y: a double
  RESULT: undefined

DESCRIPTION:

  Sets the x-coordinate and y-coordinate for the turle.  Equivalent to
  set xcor x set ycor y, except it happens in one step inside of two.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#setxy"
 (when (not (turtle-p *self*)) (error "Gotta call setxy in turtle scope, dude (~A)" *self*))
 (setf (turtle-xcor *self*) (wrap-x *topology* x))
 (setf (turtle-ycor *self*) (wrap-y *topology* y)))

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

(defun turn-right (n)
 "TURN-RIGHT N => RESULT

ARGUMENTS AND VALUES:

  N: a double, the amount the turtle turns
  RESULT: undefined

DESCRIPTION:

  The turtle turns right by number degrees. (If number is negative, it turns left.)

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#right"
 (when (not (turtle-p *self*)) (error "Gotta call fd in turtle scope, dude (~A)" *self*))
 (let
  ((new-heading (+ (turtle-heading *self*) n)))
  (setf (turtle-heading *self*)
   (cond
    ((< new-heading 0) (+ (mod new-heading -360) 360))
    ((>= new-heading 360) (mod new-heading 360))
    (t new-heading)))))

(defun turn-left (n)
 "TURN-LEFT N => RESULT

ARGUMENTS AND VALUES:

  N: a double, the amount the turtle turns
  RESULT: undefined

DESCRIPTION:

  The turtle turns left by number degrees. (If number is negative, it turns right.)

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#right"
 (turn-right (- n)))

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

(defun reset-ticks ()
 "RESET-TICKS => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined

DESCRIPTION:

  Resets the tick counter to zero, sets up all plots, then updates all plots.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#reset-ticks"
 (setf *ticks* 0d0))

(defun tick ()
 "RESET-TICKS => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined

DESCRIPTION:

  Advances the tick counter by one and updates all plots.

  If the tick counter has not been started yet with reset-ticks, an error results.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#tick"

 (when (not *ticks*) (error "reset-ticks must be called"))
 (incf *ticks*))

(defun ticks ()
 "TICKS => CURRENT-TICKS

ARGUMENTS AND VALUES:

  CURRENT-TICKS: A positiv double, representing the current number of ticks

DESCRIPTION:

  Reports the current value of the tick counter. The result is always a number and never negative.

  If the tick counter has not been started yet with reset-ticks, an error results.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#ticks"
 (when (not *ticks*) (error "reset-ticks must be called"))
 *ticks*)

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
 (setf
  *patches*
  (loop
   :for y :from (max-pycor) :downto (min-pycor)
   :append (loop
            :for x :from (min-pxcor) :to (max-pxcor)
            :collect (make-patch
                      :xcor (coerce x 'double-float)
                      :ycor (coerce y 'double-float)
                      :color 0d0))))
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

(defmethod dump-object ((o string)) (format nil "~A" (cl-ppcre:regex-replace-all "\"" (format nil "~S" o) "\"\"")))

(defmethod dump-object ((o (eql t))) "true")
(defmethod dump-object ((o (eql nil))) "false")

(defmethod dump-object ((o list)) (format nil "[~{~A~^ ~}]" (mapcar #'dump-object o)))

(defmethod dump-object ((o patch))
 (format nil "(patch ~A ~A)" (dump-object (patch-xcor o)) (dump-object (patch-ycor o))))

(defmethod dump-object ((o turtle)) (format nil "(turtle ~A)" (dump-object (turtle-who o))))
(defmethod dump-object ((o (eql :nobody))) (format nil "nobody"))

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
     :size (turtle-size turtle)))
   *turtles*)
  (mapcar
   (lambda (patch)
    (list
     :color (patch-color patch)
     :xcor (patch-xcor patch)
     :ycor (patch-ycor patch)))
   *patches*)))

(defun export-turtles ()
 (append
  (list
   "\"TURTLES\""
   (format nil "~A~A"
    "\"who\",\"color\",\"heading\",\"xcor\",\"ycor\",\"shape\",\"label\",\"label-color\","
    "\"breed\",\"hidden?\",\"size\",\"pen-size\",\"pen-mode\""))
  (mapcar
   (lambda (turtle)
    (format nil
     "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"\"\"default\"\"\",\"~A\",\"~A\",\"{all-turtles}\",\"false\",\"~A\",~A"
     (dump-object (turtle-who turtle))
     (dump-object (turtle-color turtle))
     (dump-object (turtle-heading turtle))
     (dump-object (turtle-xcor turtle))
     (dump-object (turtle-ycor turtle))
     (dump-object (turtle-label turtle))
     (dump-object (turtle-label-color turtle))
     (dump-object (turtle-size turtle))
     "\"1\",\"\"\"up\"\"\""))
   *turtles*)))

(defun export-patches ()
 (append
  (list
   "\"PATCHES\""
   "\"pxcor\",\"pycor\",\"pcolor\",\"plabel\",\"plabel-color\"")
  (mapcar
   (lambda (patch)
    (format nil
     "\"~A\",\"~A\",\"~A\",\"\"\"\"\"\",\"9.9\""
     (dump-object (patch-xcor patch))
     (dump-object (patch-ycor patch))
     (dump-object (patch-color patch))))
   *patches*)))

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
   (format nil "~{~A~^~%~}" (export-turtles))
   ""
   (format nil "~{~A~^~%~}" (export-patches))
   ""
   (format nil "~S" "LINKS")
   "\"end1\",\"end2\",\"color\",\"label\",\"label-color\",\"hidden?\",\"breed\",\"thickness\",\"shape\",\"tie-mode\""
   "")))
