(in-package #:clnl-nvm)

(defun clear-patches ()
 (setf
  *patches*
  (loop
   :for y :from (max-pycor) :downto (min-pycor)
   :append (loop
            :for x :from (min-pxcor) :to (max-pxcor)
            :collect (make-patch
                      :xcor (coerce x 'double-float)
                      :ycor (coerce y 'double-float)
                      :color 0d0)))))

(defun clear-turtles ()
 (setf *turtles* nil)
 (setf *current-id* 0))

(defun clear-ticks ()
 (setf *ticks* nil))

(defcommand clear-all ()
 "CLEAR-ALL => RESULT

  RESULT: :undefined

DESCRIPTION:

  Clears ticks, turtles, patches, globals (unimplemented).

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#clear-all"
 (clear-turtles)
 (clear-patches)
 (clear-ticks))

(defcommand display ()
 "DISPLAY => RESULT

  RESULT: :undefined

DESCRIPTION:

  As of yet, this does nothing.  A placeholder method for forced dipslay
  updates from the engine.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#display"
 nil)

(defcommand reset-ticks ()
 "RESET-TICKS => RESULT

  RESULT: :undefined

DESCRIPTION:

  Resets the tick counter to zero, sets up all plots, then updates all plots.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#reset-ticks"
 (setf *ticks* 0d0))

(defcommand tick ()
 "RESET-TICKS => RESULT

  RESULT: :undefined

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
