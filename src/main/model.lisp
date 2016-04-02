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
   :interface (nth 1 sections)
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
