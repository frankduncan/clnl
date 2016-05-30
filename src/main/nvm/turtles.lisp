(in-package #:clnl-nvm)

(defcommand create-turtles (n &optional breed fn)
 "CREATE-TURTLES N &optional BREED FN => RESULT

  RESULT: :undefined

ARGUMENTS AND VALUES:

  N: an integer, the numbers of turtles to create
  BREED: a breed
  FN: A function, applied to each turtle after creation

DESCRIPTION:

  Creates N new turtles at the origin.

  New turtles have random integer headings and the color is randomly selected
  from the 14 primary colors.  If FN is supplied, the new turtles immediately
  run it.  If a BREED is supplied, that is the breed the new turtles are set
  to.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#create-turtles"
 (let
  ((new-turtles (loop :repeat n :collect (create-turtle breed))))
  (when fn (ask (list->agentset new-turtles :turtles) fn))))

(defcommand die ()
 "DIE => RESULT

  RESULT: :undefined

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
 (setf *turtles* (remove *self* *turtles*))
 (let
  ((patch (patch-at (turtle-xcor *self*) (turtle-ycor *self*))))
  (setf (patch-turtles patch) (remove *self* (patch-turtles patch))))
 (error (make-condition 'death)))

(defcommand hatch (n &optional fn)
 "HATCH N &optional FN => RESULT

  RESULT: :undefined

ARGUMENTS AND VALUES:

  N: an integer, the numbers of turtles to hatch
  FN: A function, applied to each turtle after creation

DESCRIPTION:

  The turtle in *self* creates N new turtles. Each new turtle inherits of all its
  variables, including its location, from self.

  If FN is supplied, the new turtles immediately run it.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#hatch"
 (when (not (turtle-p *self*)) (error "Can only hatch from turtle scope"))
 (let
  ((new-turtles (loop :repeat n :collect (create-turtle nil *self*))))
  (when fn (ask (list->agentset new-turtles :turtles) fn))))

(defcommand forward (n)
 "FORWARD N => RESULT

  RESULT: :undefined

ARGUMENTS AND VALUES:

  N: a double, the amount the turtle moves forward

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

(defun jump (n)
 (when (not (turtle-p *self*)) (error "Gotta call jump in turtle scope, dude (~A)" *self*))
 (with-patch-update *self*
  (setf
   (turtle-xcor *self*)
   (wrap-x *topology*
    (+ (turtle-xcor *self*) (* n (using-cached-sin (turtle-heading *self*))))))
  (setf
   (turtle-ycor *self*)
   (wrap-y *topology*
    (+ (turtle-ycor *self*) (* n (using-cached-cos (turtle-heading *self*))))))))

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

(defcommand set-default-shape (breed shape)
 "SET-DEFAULT-SHAPE BREED SHAPE => RESULT

  RESULT: :undefined

ARGUMENTS AND VALUES:

  BREED: a valid breed
  SHAPE: a string

DESCRIPTION:

  Specifies a default initial shape for a BREED. When a turtle, or it changes breeds,
  its shape is set to the given shape.

  SET-DEFAULT-SHAPE doesn't affect existing agents, only agents you create afterwards.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#set-default-shape"
 (when (not (breed-p breed)) (error "Need a valid breed"))
 (setf (breed-default-shape breed) shape))

(defcommand setxy (x y)
 "SETXY X Y => RESULT

  RESULT: :undefined

ARGUMENTS AND VALUES:

  X: a double
  Y: a double

DESCRIPTION:

  Sets the x-coordinate and y-coordinate for the turle.  Equivalent to
  set xcor x set ycor y, except it happens in one step inside of two.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#setxy"
 (when (not (turtle-p *self*)) (error "Gotta call setxy in turtle scope, dude (~A)" *self*))
 (setf (turtle-xcor *self*) (wrap-x *topology* x))
 (setf (turtle-ycor *self*) (wrap-y *topology* y)))

(defun turtles-here (&optional breed)
 "TURTLES-HERE => TURTLES

ARGUMENTS AND VALUES:

  TURTLES: an agentset

DESCRIPTION:

  Returns the agentset consisting of all the turtles sharing the patch
  with the agent in by *self*

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles-here"
 (when (not (turtle-p *self*)) (error "Gotta call turtles-here with a turtle"))
 (let
  ((patch-turtles (patch-turtles (patch-at (turtle-xcor *self*) (turtle-ycor *self*)))))
  (list->agentset
   (if breed (remove breed patch-turtles :key #'turtle-breed :test-not #'eql) patch-turtles)
   (or breed :turtles))))

(defcommand turn-right (n)
 "TURN-RIGHT N => RESULT

  RESULT: :undefined

ARGUMENTS AND VALUES:

  N: a double, the amount the turtle turns

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

(defcommand turn-left (n)
 "TURN-LEFT N => RESULT

  RESULT: :undefined

ARGUMENTS AND VALUES:

  N: a double, the amount the turtle turns

DESCRIPTION:

  The turtle turns left by number degrees. (If number is negative, it turns right.)

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#right"
 (turn-right (- n)))
