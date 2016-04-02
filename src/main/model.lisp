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
   :code (nth 0 sections)
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
         (lambda (def) (when (find (car def) (list :int :double :boolean :choice :string)) (second def)))
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
                 (:choice `(cadr (find ,line ',(third def) :key #'car :test #'string=)))
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
