(in-package #:clnl-nvm)

(defun export-turtles ()
 (append
  (list
   "\"TURTLES\""
   (format nil "~A~A~{,\"~A\"~}"
    "\"who\",\"color\",\"heading\",\"xcor\",\"ycor\",\"shape\",\"label\",\"label-color\","
    "\"breed\",\"hidden?\",\"size\",\"pen-size\",\"pen-mode\""
    (mapcar #'string-downcase *turtles-own-vars*)))
  (mapcar
   (lambda (turtle)
    (format nil
     "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"false\",\"~A\",~A~{,\"~A\"~}"
     (dump-object (turtle-who turtle))
     (dump-object (turtle-color turtle))
     (dump-object (turtle-heading turtle))
     (dump-object (turtle-xcor turtle))
     (dump-object (turtle-ycor turtle))
     (dump-object (turtle-shape turtle))
     (dump-object (turtle-label turtle))
     (dump-object (turtle-label-color turtle))
     (dump-object (turtle-breed turtle))
     (dump-object (turtle-size turtle))
     "\"1\",\"\"\"up\"\"\""
     (mapcar #'dump-object (mapcar (lambda (var) (agent-value-inner turtle var)) *turtles-own-vars*))))
   *turtles*)))

(defun export-patches ()
 (append
  (list
   "\"PATCHES\""
   (format nil "\"pxcor\",\"pycor\",\"pcolor\",\"plabel\",\"plabel-color\"~{,\"~A\"~}"
    (mapcar #'string-downcase *patches-own-vars*)))
  (mapcar
   (lambda (patch)
    (format nil
     "\"~A\",\"~A\",\"~A\",\"\"\"\"\"\",\"9.9\"~{,\"~A\"~}"
     (dump-object (patch-xcor patch))
     (dump-object (patch-ycor patch))
     (dump-object (patch-color patch))
     (mapcar #'dump-object (mapcar (lambda (var) (agent-value-inner patch var)) *patches-own-vars*))))
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
 (let
  ((ordered-globals (sort (copy-list *globals*) #'string< :key (lambda (global) (symbol-name (car global))))))
  (format nil "~{~A~%~}"
   (list
    (format nil "~S" "RANDOM STATE")
    (format nil "~S" (clnl-random:export))
    ""
    (format nil "~S" "GLOBALS")
    (format nil "~A~A~{\"~A\"~^,~}"
     "\"min-pxcor\",\"max-pxcor\",\"min-pycor\",\"max-pycor\",\"perspective\",\"subject\","
     "\"nextIndex\",\"directed-links\",\"ticks\","
     (mapcar #'string-downcase (mapcar #'car ordered-globals)))
    (format nil "\"~A\",\"~A\",\"~A\",\"~A\",\"0\",\"nobody\",\"~A\",\"\"\"NEITHER\"\"\",\"~A\"~{,\"~A\"~}"
     (min-pxcor) (max-pxcor) (min-pycor) (max-pycor) *current-id* (dump-object (or *ticks* -1d0))
     (mapcar #'dump-object (mapcar #'funcall (mapcar #'cadr ordered-globals))))
    ""
    (format nil "~{~A~^~%~}" (export-turtles))
    ""
    (format nil "~{~A~^~%~}" (export-patches))
    ""
    (format nil "~S" "LINKS")
    "\"end1\",\"end2\",\"color\",\"label\",\"label-color\",\"hidden?\",\"breed\",\"thickness\",\"shape\",\"tie-mode\""
    ""))))

(defun show (value)
 "SHOW VALUE => RESULT

ARGUMENTS AND VALUES:

  VALUE: a NetLogo value
  RESULT: undefined

DESCRIPTION:

  A command that prints the given NetLogo value to the command center.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#show"
 (format t "Showing: ~A~%" (dump-object value)))
