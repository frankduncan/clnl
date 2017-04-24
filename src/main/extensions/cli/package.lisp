(defpackage #:clnl-extension-cli
 (:use :common-lisp)
 (:documentation
  "CLI Extension

The CLI Extension provides prims for interacting with the interface from the
commandline.  It is a special extension in that it is the only one that without
a prefix being appended to the prims.

It uses the available functions made public through normal clnl packages to
offer command line operations to control the clnl program.  Because it is
an extension, all primitives are also available to any NetLogo programs
running in CLNL.  As there is no special case control mechanism in the original
NetLogo, the CLI extension represents a departure from classic NetLogo."))
