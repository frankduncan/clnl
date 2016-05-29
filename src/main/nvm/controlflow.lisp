(in-package #:clnl-nvm)

(defun ask (agent-or-agentset fn)
 "ASK AGENT-OR-AGENTSET FN => RESULT

  AGENT-OR-AGENTSET: AGENT | AGENTSET

ARGUMENTS AND VALUES:

  FN: a function, run on each agent
  RESULT: undefined, commands don't return
  AGENT: a NetLogo agent
  AGENTSET: a NetLogo agentset

DESCRIPTION:

  ASK is equivalent to ask in NetLogo.

  The specified AGENTSET or AGENT runs the given FN.  In the case of an
  AGENTSET, the order in which the agents are run is random each time,
  and only agents that are in the set at the beginning of the call.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#ask"
 (cond
  ((agentset-p agent-or-agentset)
   (let
    ((iter (shufflerator (agentset-list agent-or-agentset))))
    (loop
     :for agent := (funcall iter)
     :while agent
     :do (when (not (and (turtle-p agent) (= -1 (turtle-who agent))))
          (let ((*myself* *self*) (*self* agent)) (with-stop-and-death-handler (funcall fn)))))))
  ((agent-p agent-or-agentset)
   (let ((*myself* *self*) (*self* agent-or-agentset)) (with-stop-and-death-handler (funcall fn))))
  (t
   (error "Ask requires an agentset or agent but got: ~A" agent-or-agentset))))

(defun stop ()
 "STOP => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined

DESCRIPTION:

  Returns from the current stop block, which will halt the currently running
  thing, be that the program, current ask block, or procedure.  Stop has odd
  semantics that are best gleaned from the actual NetLogo manual.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#stop"
 (error (make-condition 'stop)))
