(in-package #:clnl-cli)

(defvar *cli* nil)
(defvar *cli-dims* (list 0 0))

(defvar *info* nil)

(defun run ()
 "RUN => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined, should never get here

DESCRIPTION:

  RUN runs the command line interface in the running terminal.

  This should become the main REPL for a CLNL program.  If you want to use you're
  own REPL, you should use the rest of the functions in CLNL to recreate it."
 (initscr)
 (init-interface)
 (loop
  :for str := (cffi:with-foreign-pointer-as-string (str 255) (wgetnstr *cli* str 255))
  :while str
  :do (print-command-and-response str (execute str)))
 (endwin)
 (sb-ext:exit :abort t))

(defun execute (str)
 (handler-case
  (with-output-to-string (*standard-output*)
   (clnl:run-commands str))
  (error (e) (format nil "Ok, something went wrong: ~A~%Try :help" e))))

; for ui, we need to do at a minimum:
; - cli, first pass, read things in, bottom of the screen,
;     - just use getstr with a limit to be something like screen width - 10 for now
;   - print what the user inputted, dont' bomb on error messages, give target for show
;     - for overly long printing, go ahead and truncate with "..."
; - for info, this should put out a simple message about the program, maintainer, purpose, links, etc

(defun init-interface ()
 (let
  ((cli-height (min 10 (floor (* *lines* .3)))))
  (setup-info (- *lines* cli-height) *cols*)
  (setup-cli cli-height *cols* (- *lines* cli-height))))

(defun refresh-cli ()
 (wmove *cli* (1- (car *cli-dims*)) 0)
 (whline *cli* (char-code #\Space) (cadr *cli-dims*))
 (mvwprintw *cli* (1- (car *cli-dims*)) 0 ">")
 (wmove *cli* (1- (car *cli-dims*)) 2)
 (wrefresh *cli*))

(defun print-command-and-response (command response)
 (loop
  :for i :from 1 :to (- (car *cli-dims*) 3)
  :do (wmove *cli* i 0)
  :do (whline *cli* (char-code #\Space) (cadr *cli-dims*)))
 (mvwprintw *cli* 1 0 (format nil "> ~A" command))
 (mvwprintw *cli* 3 0 (format nil "~A" response))
 (refresh-cli))

(defun setup-cli (height width top)
 (setf *cli* (newwin height width top 0))
 (setf *cli-dims* (list height width))
 (whline *cli* 0 (cadr *cli-dims*))
 (wmove *cli* (- (car *cli-dims*) 2) 0)
 (whline *cli* (char-code #\.) (cadr *cli-dims*))
 (wmove *cli* (1- (car *cli-dims*)) 2)
 (wrefresh *cli*)
 (refresh-cli))

(defun setup-info (num-tall num-wide)
 (let*
  ((info "
       / \\
      /   \\     Welcome to CLNL version ~A!
     /     \\
    /_______\\
  
  CLNL is an experiment at creating an alternate
  implementation of NetLogo.
  
  You can enter in various netlogo commands below,
  or use :q to quit the program.
  
  See http://github.com/frankduncan/clnl for more
  information about CLNL and to keep apprised of
  any updates that may happen.")
   (info-height (length (cl-ppcre:split "\\n" info)))
   (info-width (apply #'max (mapcar #'length (cl-ppcre:split "\\n" info)))))
  (setf *info* (newwin
                (+ 3 info-height)
                (+ 2 info-width)
                (max 0 (floor (- num-tall info-height) 2))
                (max 0 (floor (- num-wide info-width) 2))))
  (mvwprintw *info* 1 0
   (format nil info
    (asdf:component-version (asdf:find-system :clnl))))
  (box *info* 0 0)
  (wrefresh *info*)))
