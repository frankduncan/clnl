(in-package #:clnl-extension-cli)

(defun prims ()
 "PRIMS => PRIMS

ARGUMENTS AND VALUES:

  PRIMS: Primitives defined for this extension

DESCRIPTION:

  PRIMS returns the primitives used in the CLI extension."
 (list
  (list :name :q :type :command :func #'shut-down)))

(defun shut-down ()
 (cl-charms/low-level:endwin)
 (sb-ext:exit :abort t))
