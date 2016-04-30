(in-package #:clnl)

(defvar *model-package* (find-package :cl-user)
 "*MODEL-PACKAGE*

VALUE TYPE:

  a package

INITIAL VALUE:

  The common-lisp-user package

DESCRIPTION:

  *MODEL-PACKAGE* is used for interning symbols as a NetLogo code
  gets compiled.

  Any local symbols are interned in this package, for use either
  by other code, or in order to have all symbols interned in the
  same placakge.  This is also the package in which a model should
  be run, whether by clnl code or independently.")
