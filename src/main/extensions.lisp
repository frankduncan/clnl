(in-package #:clnl-extensions)

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
   (multiple-value-bind (symb status) (find-symbol "PRIMS" pkg)
    (when (not symb) (error "Can't find PRIMS function within extension: ~A" name))
    (when (not (eql status :external)) (error "PRIMS function is not external in extension: ~A" name))
    (when (not (and (fboundp symb) (not (macro-function symb)) (not (special-operator-p symb))))
     (error "PRIMS is not a function in ~A" name))
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
     (funcall (symbol-function symb)))))))
