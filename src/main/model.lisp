(in-package #:clnl-model)

(defvar *separator* "@#$#@#$#@")

(defstruct model
 code
 interface
 info
 turtle-shapes
 version
 preview-commands
 system-dynamics
 behavior-space
 hub-net-client
 link-shapes
 model-settings
 delta-tick)

(defun default-model ()
 "DEFAULT-MODEL => MODEL

ARGUMENTS AND VALUES:

  MODEL: an object representing the model

DESCRIPTION:

  Returns the default startup model."
 (make-model
  :code ""
  :interface (list
              (make-view :min-pxcor -5 :max-pxcor 5 :min-pycor -5 :max-pycor 5 :patch-size 13d0))))

(defun read-from-nlogo (str)
 "READ-FROM-NLOGO STR => MODEL

ARGUMENTS AND VALUES:

  STR: a readable stream
  MODEL: an object representing the model

DESCRIPTION:

  Takes a stream STR, reads in a nlogo file, parses it, and then
  returns the model object."
 (let
  ((sections
    (labels
     ((read-sections (&optional section)
       (let
        ((line (read-line str nil)))
        (when line
         (if (string= *separator* line)
          (cons section (read-sections))
          (read-sections (append section (list line))))))))
     (read-sections))))
  (make-model
   :code (format nil "~{~A~^~%~}" (nth 0 sections))
   :interface (parse-interface (nth 1 sections))
   :info (nth 2 sections)
   :turtle-shapes (nth 3 sections)
   :version (nth 4 sections)
   :preview-commands (nth 5 sections)
   :system-dynamics (nth 6 sections)
   :behavior-space (nth 7 sections)
   :hub-net-client (nth 8 sections)
   :link-shapes (nth 9 sections)
   :model-settings (nth 10 sections)
   :delta-tick (nth 11 sections))))

;;; INTERFACE PARSER

(defparameter *widget-parsers* nil)

(defmacro defwidget-definition (type &rest definitions)
 (let
  ((lines (gensym)))
  `(progn
    (defstruct ,type
     ,@(remove nil
        (mapcar
         (lambda (def)
          (when (find (car def) (list :int :double :inverted-boolean :boolean :choice :string :option)) (second def)))
         definitions)))
    (push
     (list
      (lambda (,lines)
       (and
        ,@(remove nil
           (mapcar
            (lambda (def n)
             (let
              ((line `(nth ,n ,lines)))
              (case (car def)
               (:specified `(string= ,(second def) ,line))
               (:int `(parse-integer ,line :junk-allowed t))
               (:double `(ignore-errors (coerce (read-from-string ,line) 'double-float)))
               (:boolean `(or (string= "1" ,line) (string= "0" ,line)))
               (:inverted-boolean `(or (string= "0" ,line) (string= "1" ,line)))
               (:choice `(find ,line ',(mapcar #'car (third def)) :test #'string=)))))
            definitions
            (loop for i to (length definitions) collect i)))))
      (lambda (,lines)
       (,(read-from-string (format nil "make-~A" type))
        ,@(apply #'append
           (mapcar
            (lambda (def n)
             (let*
              ((line `(nth ,n ,lines))
               (val-getter
                (case (car def)
                 (:int `(parse-integer ,line))
                 (:double `(coerce (read-from-string ,line) 'double-float))
                 (:boolean `(string= "1" ,line))
                 (:inverted-boolean `(string= "0" ,line))
                 (:choice `(cadr (find ,line ',(third def) :key #'car :test #'string=)))
                 (:option `(when (string/= ,line ,(third def)) ,line))
                 (:string line))))
              (when val-getter (list (intern (symbol-name (cadr def)) :keyword) val-getter))))
            definitions
            (loop for i to (length definitions) collect i))))))
     *widget-parsers*))))

(defwidget-definition view
 (:specified "GRAPHICS-WINDOW")
 (:int left)
 (:int top)
 (:int right)
 (:int bottom)
 (:reserved "-1")
 (:reserved "-1")
 (:double patch-size)
 (:reserved)
 (:int font-size)
 (:reserved)
 (:reserved)
 (:reserved)
 (:reserved)
 (:boolean wrapping-allowed-in-x)
 (:boolean wrapping-allowed-in-y)
 (:reserved)
 (:int min-pxcor)
 (:int max-pxcor)
 (:int min-pycor)
 (:int max-pycor)
 (:choice update-mode (("0" :continuous) ("1" :tick-based)))
 (:dump update-mode)
 (:boolean show-tick-counter)
 (:string tick-counter-label)
 (:double frame-rate 30))

(defwidget-definition slider
 (:specified "SLIDER")
 (:int left)
 (:int top)
 (:int right)
 (:int bottom)
 (:string display)
 (:string varname)
 (:string min)
 (:string max)
 (:double default)
 (:string step)
 (:reserved)
 (:option units "NIL")
 (:choice direction (("HORIZONTAL" :horizontal) ("VERTICAL" :vertical))))

(defwidget-definition switch
 (:specified "SWITCH")
 (:int left)
 (:int top)
 (:int right)
 (:int bottom)
 (:string display)
 (:string varname)
 (:inverted-boolean on)
 (:reserved)
 (:reserved))

(defun parse-interface (interface-as-strings)
 (let
  ((widgets-as-strings
    (labels
     ((separate-widgets-as-strings (lines &optional widget-as-strings)
       (when lines
        (if (string= "" (car lines))
         (cons widget-as-strings (separate-widgets-as-strings (cdr lines)))
         (separate-widgets-as-strings (cdr lines) (append widget-as-strings (list (car lines))))))))
     (separate-widgets-as-strings interface-as-strings))))
  (remove
   nil
   (mapcar
    (lambda (widget-as-strings)
     (let
      ((parser (find-if (lambda (validator) (funcall validator widget-as-strings)) *widget-parsers* :key #'car)))
      (when parser (funcall (cadr parser) widget-as-strings))))
    widgets-as-strings))))

; With authoring, idx here needs to be looked at again.
(defun execute-button (name &optional (idx 0))
 "EXECUTE-BUTTON NAME &optional IDX => RESULT

ARGUMENTS AND VALUES:

  NAME: the name of the button
  IDX: the instance of the button, defaults to 0
  RESULT: undefined

DESCRIPTION:

  Executes the code in the button referenced by NAME and IDX.

  NAME refers to the display name for the button, which is usually
  set by the model, but sometimes defaults to the code inside.

  Because NAME is not guaranteed to be unique, IDX is available
  as a specifier.  The index is in the order that the buttons are
  loaded, and cannot be guaranteed to be stable from run to run."
 (declare (ignore name idx))
 nil)

;; INFORMATION ABOUT MODEL

(defun world-dimensions (model)
 "WORLD-DIMENSIONS MODEL => DIMS

  DIMS: (:xmin XMIN :xmax XMAX :ymin YMIN :ymax YMAX)

ARGUMENTS AND VALUES:

  MODEL: A valid model containing a view
  XMIN: An integer representing the minimum patch coord in X
  XMAX: An integer representing the maximum patch coord in X
  YMIN: An integer representing the minimum patch coord in Y
  YMAX: An integer representing the maximum patch coord in Y

DESCRIPTION:

  Returns the dimensions of MODEL.  MODEL must be a valid model
  as parsed by CLNL, and have a valid view in it."
 (let
  ((view (find-if #'view-p (model-interface model))))
  (list
   :xmin (view-min-pxcor view)
   :xmax (view-max-pxcor view)
   :ymin (view-min-pycor view)
   :ymax (view-max-pycor view)
   :patch-size (view-patch-size view))))

(defun widget-globals (model)
 "WIDGET-GLOBALS MODEL => GLOBALS

  GLOBALS: GLOBAL*
  GLOBAL: (NAME DEFAULT)

ARGUMENTS AND VALUES:

  MODEL: A valid model
  NAME: A symbol interned in the keyworkd package
  DEFAULT: The widget default value

DESCRIPTION:

  Returns the globals that get declared in the model from widgets.
  They are interned in the keyword package package set for clnl, so
  that they can later be used for multiple purposes."
 (remove nil
  (mapcar
   (lambda (widget)
    (typecase widget
     (slider (list (intern (string-upcase (slider-varname widget)) :keyword) (slider-default widget)))
     (switch (list (intern (string-upcase (switch-varname widget)) :keyword) (switch-on widget)))))
   (model-interface model))))

(defun code (model)
 "CODE MODEL => CODE

ARGUMENTS AND VALUES:

  MODEL: A valid model
  CODE: The string representing the netlogo code in this model

DESCRIPTION:

  Returns the code from the model."
 (model-code model))
