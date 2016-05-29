(in-package #:clnl-nvm)

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
