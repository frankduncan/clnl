(in-package #:clnl)

(defvar *model-package* (find-package :clnl-default-model-package)
 "*MODEL-PACKAGE*

VALUE TYPE:

  a package

INITIAL VALUE:

  The package named by :clnl-default-model-package

DESCRIPTION:

  *MODEL-PACKAGE* is used for interning symbols as a NetLogo code
  gets compiled.

  :clnl-default-model-package is used because it's set up to shadow
  common overlaps between the :cl package and netlogo programs, most
  notably GO.  When you set this to a package of your choosing, be
  aware of those overlaps in the case that use :use :common-lisp

  Any local symbols are interned in this package, for use either
  by other code, or in order to have all symbols interned in the
  same placakge.  This is also the package in which a model should
  be run, whether by clnl code or independently.")
