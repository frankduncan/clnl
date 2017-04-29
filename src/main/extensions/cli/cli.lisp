(in-package #:clnl-extension-cli)

(defmethod clnl-extensions:prims ((extension (eql :cli)))
 (list
  (list :name :q :type :command :func #'shut-down)
  (list :name :load :type :command :args '(t) :func #'load-file)
  (list :name :help :type :command :args '((:token :optional)) :precedence 20 :func #'help)
  (list
   :name :button :type :command :args '(:string (:number :optional))
   :precedence 20 :func #'clnl-model:execute-button)))

(defun shut-down ()
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
