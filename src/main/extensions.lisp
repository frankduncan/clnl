(in-package #:clnl-extensions)

(defgeneric prims (extension)
 (:documentation
  "PRIMS EXTENSION => PRIMS

  PRIMS: PRIM*
  PRIM: (:name NAME :type TYPE :args ARGS :func FUNC)
  TYPE: :command | :reporter
  ARGS: ARG*

ARGUMENTS AND VALUES:

  EXTENSION: a symbol in the keyword package representing this extension
  NAME: a symbol in the keyword package
  FUNC: the function to call
  ARG: a list of symbols denoting the type of argument

DESCRIPTION:

  PRIMS returns the primitives used in the extension passed in.

  See CLNL-PARSER:PARSE for more information on the PRIM returned."))

(defun load-extension (extension)
 "LOAD-EXTENSION EXTENSION => PRIMS

ARGUMENTS AND VALUES:

  EXTENSION: A symbol
  PRIMS: Primitives that can be sent to the parser and transpiler

DESCRIPTION:

  LOAD-EXTENSION takes an EXTENSION and does the work to load the asdf package,
  as well as munge the prims from extension style prims to things to be used by
  the CLNL compiler stack.

  It returns those PRIMS after checking that all the pieces are there to not
  break the runtime."
 (let*
  ((name (intern (format nil "CLNL-EXTENSION-~A" (string-upcase extension)) :keyword)))
  (asdf:load-system name)
  (let
   ((pkg (find-package name)))
   (when (or (not pkg)) (error "Can't find package with extension name: ~A" name))
   (when (not (compute-applicable-methods #'prims (list extension)))
    (error "Can't find implemented PRIMS method within extension: ~A" name))
   (mapcar
    (lambda (prim)
     (when (not (getf prim :name)) (error "Prim requires a name: ~A ~A" name prim))
     (let
      ((type (getf prim :type)))
      (when (or (not type) (not (find type '(:reporter :command))))
       (error "Prim type invalid, needs to be :reporter or :command: ~A ~A ~A" name prim type)))
     (when (not (getf prim :func))
      (error "Prim needs a func: ~A ~A" name prim))
     (list
      :name (intern
             (format nil "~A:~A"
              (if (eql extension :cli) "" (string-upcase extension))
              (string-upcase (getf prim :name)))
             :keyword)
      :type (getf prim :type)
      :precedence (or (getf prim :precedence) (if (eql :reporter (getf prim :type)) 10 0))
      :args (getf prim :args)
      :func (getf prim :func)))
    (prims extension)))))
