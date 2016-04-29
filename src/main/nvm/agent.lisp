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

(defgeneric agent-value-inner (agent var))

(defmethod agent-value-inner ((agent turtle) (var (eql :who))) (turtle-who agent))
