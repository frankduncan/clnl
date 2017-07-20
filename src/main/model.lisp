(in-package #:clnl-model)

(defvar *separator* "@#$#@#$#@")

(defvar *current-interface* nil)
(defvar *current-callback* nil)

; At this time, this is the only stateful part of the model.  If more get added,
; a more general concept can be introduced.
(defvar *enabled-forever-buttons* nil)

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

(defun set-callback (callback)
 "SET-CALLBACK CALLBACK => RESULT

ARGUMENTS AND VALUES:

  CALLBACK: a function that can take netlogo code
  RESULT: undefined

DESCRIPTION:

  Sets the means by which the interface can call arbitrary netlogo code."
 (setf *current-callback* callback))

(defun set-current-interface (interface)
 "SET-CURRENT-INTERFACE INTERFACE => RESULT

ARGUMENTS AND VALUES:

  INTERFACE: a list of widgets for display
  RESULT: undefined

DESCRIPTION:

  Sets the currently running model to INTERFACE.

  The widgets set here are comprised of the bare necessary
  to run the engine with or without an actual visual component."
 (setf *current-interface* interface))

(defun interface (model)
 "INTERFACE MODEL => INTERFACE

ARGUMENTS AND VALUES:

  MODEL: an object representing the model
  INTERFACE: a list of widgets for display

DESCRIPTION:

  INTERFACE returns the widgets in MODEL, used for display, or
  setting with SET-CURRENT-INTERFACE."
 (model-interface model))

(defun default-model ()
 "DEFAULT-MODEL => MODEL

ARGUMENTS AND VALUES:

  MODEL: an object representing the model

DESCRIPTION:

  Returns the default startup model."
 (make-model
  :code ""
  :interface (list
              (make-view :left 10 :top 10 :min-pxcor -5 :max-pxcor 5 :min-pycor -5 :max-pycor 5 :patch-size 13d0))))

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
          (when
           (find (car def) (list :int :double :tnil-boolean :inverted-boolean :boolean :choice :string :option :code))
           (second def)))
         definitions)))
    (push
     (list
      (lambda (,lines) ; Validator
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
               (:tnil-boolean `(or (string= "T" ,line) (string= "NIL" ,line)))
               (:choice `(find ,line ',(mapcar #'car (third def)) :test #'string=)))))
            definitions (loop for i to (length definitions) collect i)))))
      (lambda (,lines) ; Parser
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
                 (:tnil-boolean `(string/= "NIL" ,line))
                 (:choice `(cadr (find ,line ',(third def) :key #'car :test #'string=)))
                 (:option `(when (string/= ,line ,(third def)) ,line))
                 (:code `(unescape-code ,line))
                 (:string line))))
              (when val-getter (list (intern (symbol-name (cadr def)) :keyword) val-getter))))
            definitions (loop for i to (length definitions) collect i))))))
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

(defwidget-definition button
 (:specified "BUTTON")
 (:int left)
 (:int top)
 (:int right)
 (:int bottom)
 (:option display "NIL")
 (:code code)
 (:tnil-boolean forever)
 (:reserved)
 (:reserved)
 (:string button-type)
 (:reserved)
 (:string action-key)
 (:reserved)
 (:reserved)
 (:boolean go-time)) ; should it wait for ticks to be initialized

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

(defun find-button (name idx)
 (nth
  idx
  (remove-if-not
   (lambda (widget) (and (button-p widget) (string= (button-display-name widget) name)))
   *current-interface*)))

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
 (when *current-callback*
  (let
   ((button (find-button name (round idx))))
   (cond
    ((not button) (error "Couldn't find button with name ~A (idx: ~A)" name idx))
    ((and (button-forever button) (find button *enabled-forever-buttons* :test #'equal))
     (setf *enabled-forever-buttons* (remove button *enabled-forever-buttons* :test #'equal)))
    ((button-forever button)
     (setf *enabled-forever-buttons* (cons button *enabled-forever-buttons*))
     (sb-thread:make-thread
      (lambda ()
       (loop
        :while (find button *enabled-forever-buttons* :test #'equal)
        ; The sleep is necessary so that it gives other threads time
        :do (progn (clnl:run-commands (button-code button)) (sleep .001))))
      :name (format nil "Forever button: ~A" (button-display button))))
    (t (funcall *current-callback* (button-code button)))))))

(defun forever-button-on (name &optional (idx 0))
 "FOREVER-BUTTON-ON NAME &optional IDX => ON

ARGUMENTS AND VALUES:

  NAME: the name of the button
  IDX: the instance of the button, defaults to 0
  ON: a boolean

DESCRIPTION:

  Returns whether the button identified by NAME and IDX is currently on.

  NAME refers to the display name for the button, which is usually
  set by the model, but sometimes defaults to the code inside.

  Because NAME is not guaranteed to be unique, IDX is available
  as a specifier.  The index is in the order that the buttons are
  loaded, and cannot be guaranteed to be stable from run to run."
 (and (find (find-button name (round idx)) *enabled-forever-buttons* :test #'equal) t))

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

(defun buttons (model)
 "BUTTONS MODEL => BUTTON-DEFS

  BUTTON-DEFS: BUTTON-DEF*
  BUTTON-DEF: (:left LEFT :top TOP :height HEIGHT :width WIDTH :forever FOREVER :display DISPLAY)

ARGUMENTS AND VALUES:

  MODEL: A valid model
  LEFT: An integer representing the left position
  TOP: An integer representing the top position
  HEIGHT: An integer representing height
  WIDTH: An integer representing width
  FOREVER: A boolean representing whether this button is a forever button
  DISPLAY: A string representing display name

DESCRIPTION:

  Returns button definitions that get declared in the buttons of the
  MODEL.  This is used to initialize the interface."
 (remove nil
  (mapcar
   (lambda (widget)
    (typecase widget
     (button
      (list
       :left (button-left widget)
       :top (button-top widget)
       :width (- (button-right widget) (button-left widget))
       :height (- (button-bottom widget) (button-top widget))
       :forever (button-forever widget)
       :display (button-display-name widget)))))
   (model-interface model))))

(defun view (model)
 "VIEW MODEL => VIEW-DEF

  VIEW-DEF: (:left LEFT :top TOP)

ARGUMENTS AND VALUES:

  MODEL: A valid model
  LEFT: An integer representing the left position
  TOP: An integer representing the top position

DESCRIPTION:

  Returns the view definition that get declared in the view of the
  MODEL.  This is used to initialize the interface."
 (let
  ((view (find-if #'view-p (model-interface model))))
  (list :left (view-left view) :top (view-top view))))

(defun code (model)
 "CODE MODEL => CODE

ARGUMENTS AND VALUES:

  MODEL: A valid model
  CODE: The string representing the netlogo code in this model

DESCRIPTION:

  Returns the code from the model."
 (model-code model))

; This should get cached eventually, though maybe just cached via a display list is good enough
(defun button-display-name (button)
 (or
  (button-display button)
  (cl-ppcre:regex-replace-all "\\s+" (button-code button) " ")))

(defun unescape-code (code)
 (with-output-to-string (out)
  (with-input-from-string (in code)
   (loop
    :for c := (read-char in nil)
    :while c
    :for aux := (when (eql #\\ c)
                 (case (read-char in)
                  (#\n #\Newline)
                  (#\r #\Return)
                  (#\t #\Tab)
                  (#\\ #\\)
                  (#\" #\")
                  (t (error "Invalid escape sequence"))))
    :do (write-char (or aux c) out)))))

