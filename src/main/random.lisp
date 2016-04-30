(in-package #:clnl-random)

(defun set-seed (n)
 "SET-SEED => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined

DESCRIPTION:

  SET-SEED sets the seed on the RNG."
 (setf mt19937:*random-state* (funcall
                               (symbol-function (intern "MAKE-RANDOM-OBJECT" :mt19937))
                               :state (mt19937:init-random-state n))))

(defun next-int (n)
 "NEXT-INT N => INT

ARGUMENTS AND VALUES:

  N: An integer representing the upper bound
  INT: An integer

DESCRIPTION:

  NEXT-INT returns the next randomly generated integer.

  It does so in a way that's in accordance with java.util.Random and
  the MerseinneTwisterFast that's in NetLogo.  It also advances the
  RNG and is bounded by N."
 (if
  (= n (logand n (- n)))
  (ash (* n (ash (mt19937:random-chunk mt19937:*random-state*) -1)) -31)
  (rem (ash (mt19937:random-chunk mt19937:*random-state*) -1) n)))

(defun next-long (n)
 "NEXT-LONG N => LONG

ARGUMENTS AND VALUES:

  N: A long representing the upper bound
  LONG: A long

DESCRIPTION:

  NEXT-LONG returns the next randomly generated long.

  It does so in a way that's in accordance with java.util.Random and
  the MerseinneTwisterFast that's in NetLogo.  It also advances the
  RNG and is bounded by N."
 (flet
  ((unsigned-to-signed (value size) ; We need this because MersenneTwisterFast
    (if (logbitp (1- size) value) (dpb value (byte size 0) -1) value))
   (signed-to-unsigned (value size) (ldb (byte size 0) value)))
  (let
   ((y (unsigned-to-signed (mt19937:random-chunk mt19937:*random-state*) 32))
    (z (unsigned-to-signed (mt19937:random-chunk mt19937:*random-state*) 32)))
   ;(mod (+ (ash y 32) z) n)))
   (mod (signed-to-unsigned (ash (+ (ash y 32) z) -1) 63) n))))

(defun next-double (&optional (n 1d0))
 "NEXT-DOUBLE &optional N => DOUBLE

ARGUMENTS AND VALUES:

  N: A double representing the upper bound
  DOUBLE: A double

DESCRIPTION:

  NEXT-DOUBLE returns the next randomly generated double.

  It does so in a way that's in accordance with java.util.Random and
  the MerseinneTwisterFast that's in NetLogo.  It also advances the
  RNG and is bounded by N."
 (let
  ((y (mt19937:random-chunk mt19937:*random-state*))
   (z (mt19937:random-chunk mt19937:*random-state*)))
  (*
   (/
    (+ (ash (ash y -6) 27) (ash z -5))
    (coerce (ash 1 53) 'double-float))
   n)))

; Oh, export world, you WILL be mine
(defun export ()
 "EXPORT => RANDOM-STATE

ARGUMENTS AND VALUES:

  RANDOM-STATE: A dump of the current random state

DESCRIPTION:

  EXPORT dumps out the random state to be export world ready.

  When NetLogo dumps out the current state of the engine, the state of the
  RNG also gets dumped out so that it can be reinitialized later.  This
  accomplishes that.

  This isn't really useful for regular use."
 (let
  ((state
    (map
     'list
     (lambda (x) (if (logbitp (1- 32) x) (dpb x (byte 32 0) -1) x))
     (funcall (symbol-function (intern "RANDOM-STATE-STATE" :mt19937)) mt19937:*random-state*))))
  (format nil "0 ~A ~A ~A 0.0 false ~{~A~^ ~}"
   (first state) (second state) (third state)
   (nthcdr 3 state))))
