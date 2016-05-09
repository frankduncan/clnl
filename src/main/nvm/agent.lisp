(in-package #:clnl-nvm)

(defun agent-value (var &optional (agent *self*))
 "AGENT-VALUE VAR &optional AGENT => RESULT

ARGUMENTS AND VALUES:

  VAR: A variable name
  AGENT: an agent, defaulting to *self*
  RESULT: the value of VAR

DESCRIPTION:

  AGENT-VALUE is the general agent variable access function.  For many
  NetLogo reporters, the compilation results is AGENT-VALUE.  The list of
  valid values are any builtin variable in the NetLogo dictionary, as well
  as any *-own variable.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html for builtins"
 (agent-value-inner agent (intern (string-upcase var) :keyword)))

(defsetf agent-value (var &optional (agent '*self*)) (new-value)
 `(set-agent-value-inner ,agent ,var ,new-value))

(defgeneric set-agent-value-inner (agent var new-value))
(defgeneric agent-value-inner (agent var))

(defmacro defagent-value (type symb &optional accessor)
 (let
  ((accessor (or accessor (intern (format nil "~A-~A" type symb))))
   (agent (gensym))
   (var (gensym))
   (new-val (gensym)))
  `(progn
    (defmethod agent-value-inner ((,agent ,type) (,var (eql ,symb))) (,accessor ,agent))
    (defmethod set-agent-value-inner ((,agent ,type) (,var (eql ,symb)) ,new-val) (setf (,accessor ,agent) ,new-val)))))

; Don't want the setter for :who
(defmethod agent-value-inner ((turtle turtle) (var (eql :who))) (turtle-who turtle))

(defagent-value patch :pcolor patch-color)

(defagent-value turtle :color)
(defagent-value turtle :label)
(defagent-value turtle :label-color)
(defagent-value turtle :size)

(defmethod agent-value-inner ((turtle turtle) var)
 (when (not (find var *turtles-own-vars*)) (error "~S is not a turtle variable" var))
 (or (getf (turtle-own-vars turtle) var) 0d0))

(defmethod set-agent-value-inner ((turtle turtle) var new-val)
 (when (not (find var *turtles-own-vars*)) (error "~S is not a turtle variable" var))
 (if (getf (turtle-own-vars turtle) var)
  (setf (getf (turtle-own-vars turtle) var) new-val)
  (setf (turtle-own-vars turtle) (append (list var new-val) (turtle-own-vars turtle)))))

(defmethod agent-value-inner ((patch patch) var)
 (when (not (find var *patches-own-vars*)) (error "~S is not a patch variable" var))
 (or (getf (patch-own-vars patch) var) 0d0))

(defmethod set-agent-value-inner ((patch patch) var new-val)
 (when (not (find var *patches-own-vars*)) (error "~S is not a patch variable" var))
 (if (getf (patch-own-vars patch) var)
  (setf (getf (patch-own-vars patch) var) new-val)
  (setf (patch-own-vars patch) (append (list var new-val) (patch-own-vars patch)))))
