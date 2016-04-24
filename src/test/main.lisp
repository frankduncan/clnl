(in-package #:clnl-test)

(defparameter *tests* nil)

(defun run-and-print-test (test)
 (let
  ((green (format nil "~c[1;32m" #\Esc))
   (red (format nil "~c[1;31m" #\Esc))
   (result (funcall (cadr test))))
  (format t "~A- ~S ~A~c[0m~%" (if result green red) (car test) (if result "passed" "failed") #\Esc)
  result))

(defun run-tests (tests)
 (let
  ((final-result t))
  (loop
   :for test :in tests
   :for result := (run-and-print-test test)
   :do (setf final-result (and final-result result)))
  final-result))

(defun run-all-tests ()
 (run-tests (reverse *tests*)))

(defun run-tests-matching (match)
 (run-tests
  (remove-if-not (lambda (test-name) (cl-ppcre:scan (format nil "^~A$" match) test-name)) *tests* :key #'car)))

(defun find-test (name)
 (or
  (find name *tests* :test #'string= :key #'car)
  (error "Couldn't find test with name: ~A" name)))

(defun test-debug (name) (format t "----~%~A~%" (funcall (third (find-test name)))))
(defun test-scala-prog (name) (format t "----~%~A~%" (fourth (find-test name))))
(defun test-scala-input (name) (format t "----~%~A~%" (fifth (find-test name))))

(defmacro defsimpletest (name test-fn debug-fn scala-prog scala-input)
 `(progn
   ;(when (find-test ,name) (error "Test with name ~S already exists, abort, abort" ,name))
   (push
    (list ,name ,test-fn ,debug-fn ,scala-prog ,scala-input)
    *tests*)))

(defun checksum= (expected got)
 (if (stringp expected)
  (string= got expected)
  (find got expected :test #'string=)))

; To be used only with the simplest of tests, just a list of commands and a checksum of the
; world after they've been run.
(defmacro defsimplecommandtest (name commands checksum)
 `(defsimpletest
   (format nil "Simple Command - ~A" ,name)
   (lambda ()
    (clnl:boot "resources/empty.nlogo")
    (clnl:run-commands ,commands)
    (checksum= ,checksum (checksum-world)))
   (lambda ()
    (clnl:boot "resources/empty.nlogo")
    (clnl:run-commands ,commands)
    (format nil "~A~A"
     (clnl-nvm:export-world)
     (checksum-world)))
   "bin/runcmd.scala"
   (format nil "~A~%" ,commands)))

(defmacro defsimplereportertest (name reporter value checksum)
 `(defsimpletest
   (format nil "Simple Reporter - ~A" ,name)
   (lambda ()
    (clnl:boot "resources/empty.nlogo")
    (and
     (string= (funcall (intern "DUMP-OBJECT" :clnl-nvm) (clnl:run-reporter ,reporter)) ,value)
     (checksum= ,checksum (checksum-world))))
   (lambda ()
    (clnl:boot "resources/empty.nlogo")
    (format nil "~A~%~A~A"
     (funcall (intern "DUMP-OBJECT" :clnl-nvm) (clnl:run-reporter ,reporter))
     (clnl-nvm:export-world)
     (checksum-world)))
   "bin/runcmd.scala"
   (format nil "~%@#$#@#$#@~A~%" ,reporter)))

(defmacro defreportertestwithsetup (name setup reporter value checksum)
 `(defsimpletest
   (format nil "Reporter With Setup - ~A" ,name)
   (lambda ()
    (clnl:boot "resources/empty.nlogo")
    (clnl:run-commands ,setup)
    (and
     (string= (funcall (intern "DUMP-OBJECT" :clnl-nvm) (clnl:run-reporter ,reporter)) ,value)
     (checksum= ,checksum (checksum-world))))
   (lambda ()
    (clnl:boot "resources/empty.nlogo")
    (clnl:run-commands ,setup)
    (format nil "~A~%~A~A"
     (funcall (intern "DUMP-OBJECT" :clnl-nvm) (clnl:run-reporter ,reporter))
     (clnl-nvm:export-world)
     (checksum-world)))
   "bin/runcmd.scala"
   (format nil "~A~%@#$#@#$#@~A" ,setup ,reporter)))

(defmacro defviewtest (name commands checksum)
 `(defsimpletest
   (format nil "Simple View - ~A" ,name)
   (lambda ()
    (clnl:boot "resources/empty.nlogo")
    (clnl:run-commands ,commands)
    (let
     ((viewsum (checksum-view)))
     (when (not (checksum= ,checksum viewsum))
      (format t "~c[1;35m-- For ~A, got ~A but expected ~A~c[0m~%" #\Esc ,name viewsum ,checksum #\Esc))
     (checksum= ,checksum (checksum-view))))
   (lambda ()
    (clnl:boot "resources/empty.nlogo")
    (clnl:run-commands ,commands)
    (save-view-to-ppm)
    (format nil "~A" (checksum-view)))
   ""
   (format nil "~A~%" ,commands)))

(defun checksum-world ()
 (format nil "~{~2,'0X~}"
  (map 'list #'identity
   (ironclad:digest-sequence
    :sha1
    (map '(vector (unsigned-byte 8)) #'char-code (clnl-nvm:export-world))))))

(defun checksum-view ()
 (format nil "~{~2,'0X~}"
  (map 'list #'identity
   (ironclad:digest-sequence :sha1 (coerce (clnl-interface:export-view) '(vector (unsigned-byte 8)))))))

(defun save-view-to-ppm ()
 (let
  ((height 143) (width 143)) ; hardcoded in interface, hardcoded here, cry for me
  (with-open-file (str "cl.ppm"
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create
                   :element-type '(unsigned-byte 8))
   (write-sequence (map 'vector #'char-code (format nil "P6~%")) str)
   (write-sequence (map 'vector #'char-code (format nil "143 143~%")) str)
   (write-sequence (map 'vector #'char-code (format nil "255~%")) str)
   (let
    ((image-data (clnl-interface:export-view)))
    (dotimes (i width)
     (dotimes (j height)
      (write-byte (aref image-data (+ 0 (* 4 (+ (* (- (1- height) i) width) j)))) str)
      (write-byte (aref image-data (+ 1 (* 4 (+ (* (- (1- height) i) width) j)))) str)
      (write-byte (aref image-data (+ 2 (* 4 (+ (* (- (1- height) i) width) j)))) str)))))))

(defun run ()
 (loop
  :for str := (progn (format t "> ") (force-output) (read-line))
  :while str
  :do (progn (asdf:load-system :clnl-test) (run-tests-matching str))))
