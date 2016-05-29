(in-package #:clnl-nvm)

(defun count (agentset)
 "COUNT AGENTSET => N

ARGUMENTS AND VALUES:

  AGENTSET: a NetLogo agentset
  N: a number

DESCRIPTION:

  COUNT is equivalent to count in NetLogo.  Returns N, the number of
  agents in AGENTSET.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#count"
 (coerce (length (agentset-list agentset)) 'double-float))

(defun of (fn agent-or-agentset)
 "OF FN AGENT-OR-AGENTSET => RESULT

  AGENT-OR-AGENTSET: AGENT | AGENTSET
  RESULT: RESULT-LIST | RESULT-VALUE

ARGUMENTS AND VALUES:

  FN: a function, run on each agent
  AGENT: a NetLogo agent
  AGENTSET: a NetLogo agentset
  RESULT-LIST: a list
  RESULT-VALUE: a single value

DESCRIPTION:

  OF is equivalent to of in NetLogo.

  The specified AGENTSET or AGENT runs the given FN.  In the case of an
  AGENTSET, the order in which the agents are run is random each time,
  and only agents that are in the set at the beginning of the call.

  RESULT-LIST is returned when the input is an AGENTSET, but RESULT-VALUE
  is returned when only passed an AGENT.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#of"
 (cond
  ((agentset-p agent-or-agentset)
   (let
    ((iter (shufflerator (agentset-list agent-or-agentset))))
    (loop
     :for agent := (funcall iter)
     :while agent
     :collect (let ((*myself* *self*) (*self* agent)) (funcall fn)))))
  ((agent-p agent-or-agentset)
   (let ((*myself* *self*) (*self* agent-or-agentset)) (funcall fn)))
  (t
   (error "Of requires an agentset or agent but got: ~A" agent-or-agentset))))

(defun one-of (list-or-agentset)
 "ONE-OF LIST-OR-AGENTSET => RESULT

  LIST-OR-AGENTSET: LIST | AGENTSET
  RESULT: RANDOM-VALUE | RANDOM-AGENT | :nobody

ARGUMENTS AND VALUES:

  LIST: A list
  AGENTSET: An agent set
  RANDOM-VALUE: a value in LIST
  RANDOM-AGENT: an agent if AGENTSET is non empty

DESCRIPTION:

  From an AGENTSET, returns a RANDOM-AGENT. If the agentset is empty, returns :nobody.
  From a list, returns a RANDOM-VALUE.  If the list is empty, an error occurs.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#one-of"
 (cond
  ((agentset-p list-or-agentset)
   (let*
    ((agentset-list (agentset-list list-or-agentset))
     (length (length agentset-list)))
    (if (zerop length) :nobody (nth (clnl-random:next-int length) agentset-list))))
  ((listp list-or-agentset)
   (let*
    ((length (length list-or-agentset)))
    (if (zerop length)
     (error "one-of requires a nonempty list")
     (nth (clnl-random:next-int length) list-or-agentset))))
  (t (error "one-of requires a list or agentset"))))

(defun patches ()
 "PATCHES => ALL-PATCHES

ARGUMENTS AND VALUES:

  ALL-PATCHES: a NetLogo agentset, all patches

DESCRIPTION:

  Reports the agentset consisting of all the patches.

  This agentset is special in that it represents the living patches
  each time it's used, so changes depending on the state of the engine.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#patches"
 :patches)

(defun turtles ()
 "TURTLES => ALL-TURTLES

ARGUMENTS AND VALUES:

  ALL-TURTLES: a NetLogo agentset, all turtles

DESCRIPTION:

  Reports the agentset consisting of all the turtles.

  This agentset is special in that it represents the living turtles
  each time it's used, so changes depending on the state of the engine.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles"
 :turtles)

(defun with (agentset fn)
 "WITH AGENTSET FN => RESULT-AGENTSET

ARGUMENTS AND VALUES:

  AGENTSET: a NetLogo agentset
  FN: a boolean function, run on each agent to determine if included
  RESULT-AGENTSET: an agentset of valid agents

DESCRIPTION:

  WITH is equivalent to with in NetLogo.

  Returns a new agentset containing only those agents that reported true
  when FN is called.

  See http://ccl.northwestern.edu/netlogo/docs/dictionary.html#with"
 (list->agentset
  (remove-if-not
   (lambda (agent)
    (let ((*myself* *self*) (*self* agent)) (funcall fn)))
   (agentset-list agentset))
  (agentset-breed agentset)))
