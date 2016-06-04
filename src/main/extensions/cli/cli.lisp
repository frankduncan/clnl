(in-package #:clnl-extension-cli)

(defun prims ()
 "PRIMS => PRIMS

ARGUMENTS AND VALUES:

  PRIMS: Primitives defined for this extension

DESCRIPTION:

  PRIMS returns the primitives used in the CLI extension."
 (list
  (list :name :q :type :command :func #'shut-down)
  (list :name :load :type :command :args '(t) :func #'load-file)
  (list :name :help :type :command :args '((:token :optional)) :precedence 20 :func #'help)))

(defun shut-down ()
 (cl-charms/low-level:endwin)
 (sb-ext:exit :abort t))

(defun load-file (file)
 (clnl:boot file))

(defun help (&optional token)
 (format t
  (if (not token)
   "Placeholder help facility, try <:help :q> or <:help :load> for information about the commands we accept"
   (case token
    (:|:Q| ":q quits out of clnl")
    (:|:LOAD|
     (concatenate 'string
      ":load <filename> loads up a model into the current clnl instance."
      " Try :load \"Wolf Sheep Predation.nlogo\""))
    (t (format nil "Don't have help for ~S" token))))))
