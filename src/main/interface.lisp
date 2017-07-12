(in-package #:clnl-interface)

(defvar *turtle-lists* nil)
(defvar *patch-list* nil)

(defvar *glut-window-opened* nil)
(defvar *dimensions* nil) ; This is a useful placeholder for other view properties
(defvar *window-width* 1024)
(defvar *window-height* 768)

(defvar *default-shapes* nil)

(defvar *textbox* nil)
(defvar *inputbox* nil)

(defvar *widgets* nil) ; this is going to be pairs to save the original definition

; For now, shapes can live in here
; header is
; * name <like default>
; * rotatable (equal to "true" if yes)
;
; then after, the elements are like so:
;
; filled == filled in (always for now, ha)
; marked == use the turtle color instead of a color
; polygon -> Polygon <color> <filled> <marked> <alternating x y coords>
; circle -> Circle <color> <filled> <marked> <left> <top> <diameter> ; here, the left and top are NOT the center
; rectangle -> Rectangle <color> <filled> <marked> <left> <top> <right> <bottom>
;
; then ends with an empty string

(defun parse-circle (sections)
 (list :circle
  :color (parse-integer (car sections))
  :filled (string= (nth 1 sections) "true")
  :marked (string= (nth 2 sections) "true")
  :left (parse-integer (nth 3 sections))
  :top (parse-integer (nth 4 sections))
  :diameter (parse-integer (nth 5 sections))))

(defun parse-rectangle (sections)
 (list
  :rectangle
  :color (parse-integer (car sections))
  :filled (string= (nth 1 sections) "true")
  :marked (string= (nth 2 sections) "true")
  :left (parse-integer (nth 3 sections))
  :top (parse-integer (nth 4 sections))
  :right (parse-integer (nth 5 sections))
  :bottom (parse-integer (nth 6 sections))))

(defun parse-polygon (sections)
 (labels
  ((parse-points (sections)
    (when sections
     (cons
      (list (- 300 (parse-integer (car sections))) (parse-integer (cadr sections)))
      (parse-points (cddr sections))))))
  (list
   :polygon
   :color (parse-integer (car sections))
   :filled (string= (nth 1 sections) "true")
   :marked (string= (nth 2 sections) "true")
   :coords (parse-points (nthcdr 3 sections)))))

(defun parse-shape (str)
 (labels
  ((parse-element (line)
    (let
     ((sections (cl-ppcre:split " " line)))
     (cond
      ((string= (car sections) "Circle") (parse-circle (cdr sections)))
      ((string= (car sections) "Rectangle") (parse-rectangle (cdr sections)))
      ((string= (car sections) "Polygon") (parse-polygon (cdr sections))))))
   (parse-elements ()
    (let
     ((line (read-line str nil)))
     (when (and line (string/= line ""))
      (cons
       (parse-element line)
       (parse-elements))))))
  (let
   ((next-line (read-line str nil)))
   (when next-line
    (list
     :name next-line
     :rotatable (string= "true" (read-line str))
     :rgb (read-line str) ; this is ignored for now, I think
     :elements (parse-elements))))))

; Clipping ears algorithm.  This can be slow due to the fact that it will only be run once.
(defun triangulate (points &optional (ccw :unknown))
 (labels
  ((tri-is-ccw (x y z)
    (< 0 (- (* (- (car y) (car x)) (- (cadr z) (cadr x))) (* (- (car z) (car x)) (- (cadr y) (cadr x))))))
   (tri-is-concave (x y z) (if (tri-is-ccw x y z) (not ccw) ccw))
   (poly-is-ccw (points &optional cur-tri)
    (cond
     ((not cur-tri)
      (poly-is-ccw (append points (list (car points))) (list (car (last points)) (car points) (cadr points))))
     ((eql (length points) 2)
      (apply #'tri-is-ccw cur-tri))
     ((or
       (< (car (cadr points)) (car (cadr cur-tri)))
       (and
        (= (car (cadr points)) (car (cadr cur-tri)))
        (< (cadr (cadr points)) (cadr (cadr cur-tri)))))
      (poly-is-ccw (cdr points) (subseq points 0 3)))
     (t (poly-is-ccw (cdr points) cur-tri))))
   (point-in-tri (x y z p)
    ; Barycentric system test
    (let*
     ((denom (+ (* (- (cadr y) (cadr z)) (- (car x) (car z))) (* (- (car z) (car y)) (- (cadr x) (cadr z)))))
      (a (/ (+ (* (- (cadr y) (cadr z)) (- (car p) (car z))) (* (- (car z) (car y)) (- (cadr p) (cadr z)))) denom))
      (b (/ (+ (* (- (cadr z) (cadr x)) (- (car p) (car z))) (* (- (car x) (car z)) (- (cadr p) (cadr z)))) denom))
      (c (- 1 a b)))
     (and (<= 0 a 1) (<= 0 b 1) (<= 0 c 1))))
   (no-points-in-tri (tri points)
    (every (lambda (point) (not (point-in-tri (car tri) (cadr tri) (caddr tri) point))) points))
   (tri-is-actually-line (x y z)
    (zerop (+ (* (- (cadr y) (cadr z)) (- (car x) (car z))) (* (- (car z) (car y)) (- (cadr x) (cadr z)))))))
  (cond
   ((not (find :end points)) (triangulate (append points (list :end)) ccw))
   ((< (length points) 4) (error "Must have at least 3 points..."))
   ((= (length points) 4) (list (remove :end points)))
   ((eql ccw :unknown) (triangulate points (poly-is-ccw (remove :end points))))
   ((eql :end (car points)) (error "This polygon may not be triangulateable"))
   (t
    (let*
     ((endless (remove :end points))
      (tri (subseq endless 0 3)))
     (cond
      ((apply #'tri-is-actually-line tri)
       (triangulate (cons (car endless) (cddr endless)) ccw))
      ((apply #'tri-is-concave tri)
       (triangulate (append (cdr points) (list (car points))) ccw))
      ((no-points-in-tri tri (nthcdr 3 endless))
       (cons tri (triangulate (cons (car endless) (cddr endless)) ccw)))
      (t (triangulate (append (cdr points) (list (car points))) ccw))))))))

(defun element->gl-list (shape)
 (progn
  (when (not (getf (cdr shape) :marked))
   (gl:push-attrib :all-attrib-bits)
   (gl:color
    (/ (ash (ldb (byte 24 0) (getf (cdr shape) :color)) -16) 255)
    (/ (ash (ldb (byte 16 0) (getf (cdr shape) :color)) -8) 255)
    (/ (ldb (byte 8 0) (getf (cdr shape) :color)) 255)))
  (gl:begin :triangles)
  (case (car shape)
   (:polygon
    (mapcar
     (lambda (point) (gl:vertex (car point) (cadr point) 0))
     (apply #'append (triangulate (getf (cdr shape) :coords)))))
   (:rectangle
    (mapcar
     (lambda (point) (gl:vertex (car point) (cadr point) 0))
     (apply #'append
      (triangulate
       (list
        (list (- 300 (getf (cdr shape) :left)) (getf (cdr shape) :top))
        (list (- 300 (getf (cdr shape) :right)) (getf (cdr shape) :top))
        (list (- 300 (getf (cdr shape) :right)) (getf (cdr shape) :bottom))
        (list (- 300 (getf (cdr shape) :left)) (getf (cdr shape) :bottom)))))))
   (:circle
    (mapcar
     (lambda (point) (gl:vertex (car point) (cadr point) 0))
     (apply #'append
      (triangulate
       (loop
        :repeat 360
        :with c := (strictmath:cos (strictmath:to-radians 1))
        :with s := (strictmath:sin (strictmath:to-radians 1))
        :with r := (/ (getf (cdr shape) :diameter) 2)
        :with left := (getf (cdr shape) :left)
        :with top := (getf (cdr shape) :top)
        :for n := 0 :then x
        :for x := r :then (- (* c x) (* s y))
        :for y := 0 :then (+ (* s n) (* c y))
        :collect (list (- 300 (+ (+ x left) r)) (+ (+ y top) r))))))))
  (gl:end)
  (when (not (getf (cdr shape) :marked))
   (gl:pop-attrib))))

(defun parse-shapes (str)
 (let
  ((shape (parse-shape str)))
  (when shape (cons shape (parse-shapes str)))))

(defun default-shapes ()
 (with-open-file (str "resources/defaultshapes") (parse-shapes str)))

(eval-when (:load-toplevel)
 (when (probe-file "resources/defaultshapes")
  (setf *default-shapes* (default-shapes))))

(defvar *colors*
 '((140 140 140) ; gray       (5)
   (215 48 39) ; red       (15)
   (241 105 19) ; orange    (25)
   (156 109 70) ; brown     (35)
   (237 237 47) ; yellow    (45)
   (87 176 58) ; green     (55)
   (42 209 57) ; lime      (65)
   (27 158 119) ; turquoise (75)
   (82 196 196) ; cyan      (85)
   (43 140 190) ; sky       (95)
   (50 92 168) ; blue     (105)
   (123 78 163) ; violet   (115)
   (166 25 105) ; magenta  (125)
   (224 126 149) ; pink     (135)
   (0 0 0) ; black
   (255 255 255))) ; white

(defun nl-color->rgb (color)
 (let*
  ((step (+ (/ (- (mod (floor (* color 10)) 100) 50) 50.48) 0.012)))
  (mapcar
   (lambda (x) (/ (+ x (floor (* (if (< step 0d0) x (- 255 x)) step))) 255))
   (nth (floor color 10) *colors*))))

(defun render-scene ()
 (gl:matrix-mode :projection)
 (gl:with-pushed-matrix
  (gl:load-identity)
  (gl:ortho
   (floor (* (- (getf *dimensions* :xmin) 0.5) (patch-size)))
   (floor (* (+ (getf *dimensions* :xmax) 0.5) (patch-size)))
   (floor (* (- (getf *dimensions* :ymin) 0.5) (patch-size)))
   (floor (* (+ (getf *dimensions* :ymax) 0.5) (patch-size)))
   0 5000)
  (gl:matrix-mode :modelview)
  (gl:with-pushed-matrix
   (gl:load-identity)
   (destructuring-bind (turtles patches) (clnl-nvm:current-state)
    (mapcar
     (lambda (patch)
      (let
       ((color (nl-color->rgb (getf patch :color))))
       (gl:color (car color) (cadr color) (caddr color)))
      (gl:with-pushed-matrix
       (gl:translate (* (getf patch :xcor) (patch-size)) (* (getf patch :ycor) (patch-size)) 0)
       (gl:translate (floor (* -.5d0 (patch-size))) (floor (* -.5d0 (patch-size))) 0)
       (gl:scale (patch-size) (patch-size) 1)
       (gl:call-list *patch-list*)))
     patches)
    (mapcar
     (lambda (turtle)
      (let
       ((color (nl-color->rgb (getf turtle :color))))
       (gl:color (car color) (cadr color) (caddr color)))
      (mapcar
       (lambda (x-modification y-modification)
        (gl:with-pushed-matrix
         (gl:translate (* (getf turtle :xcor) (patch-size)) (* (getf turtle :ycor) (patch-size)) 0)
         (gl:translate x-modification y-modification 0)
         (let
          ((turtle-list (find (getf turtle :shape) *turtle-lists* :test #'string= :key #'car)))
          (when turtle-list
           (when (second turtle-list)
            (gl:rotate (getf turtle :heading) 0 0 -1))
           (gl:scale (patch-size) (patch-size) 1)
           (gl:scale (getf turtle :size) (getf turtle :size) 1)
           (gl:call-list (third turtle-list))))))
       (list 0 (1- (world-width-in-pixels)) (- (1- (world-width-in-pixels))) 0 0)
       (list 0 0 0 (1- (world-height-in-pixels)) (- (1- (world-height-in-pixels))))))
     turtles)))
  (gl:matrix-mode :projection))
 (gl:flush))

(defun render-widgets ()
 (clnl-gltk:render *textbox*)
 (clnl-gltk:render *inputbox*)
 (mapcar #'clnl-gltk:render (mapcar #'cadr *widgets*)))

(defun render ()
 (gl:clear :color-buffer-bit :depth-buffer-bit)
 (let
  ((width (world-width-in-pixels))
   (height (world-height-in-pixels)))
  (gl:viewport 0 0 *window-width* *window-height*)

  (gl:matrix-mode :projection)
  (let*
   ((left (getf *dimensions* :left))
    (top (getf *dimensions* :top))
    (view-x1 left)
    (view-x2 (+ view-x1 width))
    (view-y1 (- *window-height* height top))
    (view-y2 (- *window-height* top)))

   (gl:with-pushed-matrix
    (gl:load-identity)
    (gl:ortho 0 *window-width* 0 *window-height* 0 5000)
    (render-widgets)

    (gl:begin :lines)
    (gl:vertex view-x1 view-y1)
    (gl:vertex view-x1 (+ view-y2 1))

    (gl:vertex view-x1 (+ view-y2 1))
    (gl:vertex (+ view-x2 1) (+ view-y2 1))

    (gl:vertex (+ view-x2 1) (+ view-y2 1))
    (gl:vertex (+ view-x2 1) (- view-y1 1))

    (gl:vertex (+ view-x2 1) view-y1)
    (gl:vertex (- view-x1 1) view-y1)
    (gl:end))

   (gl:viewport view-x1 view-y1 width height)
   (render-scene))))

(defun display ()
 (render)
 (cl-glut:swap-buffers))

(defun idle ()
 (cl-glut:post-redisplay))

(defun close-func ()
 ;(glut:leave-main-loop)
 (sb-ext:exit :abort t))

(defun reshape (width height)
 (when (and (/= 0 width) (/= 0 height))
  (setf *window-width* width)
  (setf *window-height* height)
  (let
   ((box-width (truncate (- width 12) clnl-gltk:*font-width*)))
   (clnl-gltk:resize *textbox* box-width 12)
   (clnl-gltk:resize *inputbox* box-width 1))
  (mapcar
   (lambda (pair)
    (clnl-gltk:reposition (cadr pair)
     (getf (car pair) :left)
     (- *window-height* (getf (car pair) :height) (getf (car pair) :top))))
   *widgets*)))

(defun execute (str)
 (setf
  (clnl-gltk:textbox-text *textbox*)
  (format nil "> ~A~%~%~A" str
   (handler-case
    (with-output-to-string (*standard-output*)
     (clnl:run-commands str))
    (error (e) (format nil "Ok, something went wrong: ~A~%Try :help" e))))))

(defun key-pressed (key x y)
 (declare (ignore x y))
 (if (eql key 13)
  (progn
   (execute (clnl-gltk:value *inputbox*))
   (clnl-gltk:clear *inputbox*))
  (clnl-gltk:key-pressed *inputbox* key)))

(defun mouse (button state x y)
 (declare (ignore button))
 (mapcar
  (lambda (w)
   (when (eql state :down) (clnl-gltk:mousedown w x (- *window-height* y)))
   (when (eql state :up) (clnl-gltk:mouseup w x (- *window-height* y))))
  (mapcar #'cadr *widgets*)))

(defun motion (x y)
 (mapcar
  (lambda (w) (clnl-gltk:mousemove w x (- *window-height* y)))
  (mapcar #'cadr *widgets*)))

(cffi:defcallback display :void () (display))
(cffi:defcallback idle :void () (idle))
(cffi:defcallback close-func :void () (close-func))
(cffi:defcallback reshape :void ((width :int) (height :int)) (reshape width height))
(cffi:defcallback key-pressed :void ((key :uchar) (x :int) (y :int)) (key-pressed key x y))
(cffi:defcallback special-key-pressed :void ((key glut:special-keys) (x :int) (y :int)) (key-pressed key x y))
(cffi:defcallback mouse :void ((button cl-glut:mouse-button) (state cl-glut:mouse-button-state) (x :int) (y :int))
 (mouse button state x y))

(cffi:defcallback motion :void ((x :int) (y :int)) (motion x y))

(defun set-turtle-lists ()
 (setf
  *turtle-lists*
  (mapcar
   (lambda (shape)
    (let
     ((turtle-list
       (list
        (getf shape :name)
        (getf shape :rotatable)
        (gl:gen-lists 1))))
     (gl:with-new-list ((third turtle-list) :compile)
      (gl:rotate 180d0 0d0 0d0 -1d0)
      (gl:scale (/ 1d0 300d0) (/ 1d0 300d0) 1)
      (gl:translate -150d0 -150d0 -0.0d0)
      (mapcar #'element->gl-list (getf shape :elements)))
     turtle-list))
   (or *default-shapes* (default-shapes)))))

(defun set-patch-list ()
 (setf *patch-list* (gl:gen-lists 1))
 (gl:with-new-list (*patch-list* :compile)
  (gl:begin :polygon)
  (gl:vertex 0 0 0)
  (gl:vertex 0 1 0)
  (gl:vertex 1 1 0)
  (gl:vertex 1 0 0)
  (gl:end)))

(defvar *initial-banner*
 "
     / \\
    /   \\     Welcome to CLNL version ~A!
   /     \\
  /_______\\

CLNL is an experiment at creating an alternate implementation of NetLogo.

You can enter in various netlogo commands below, or use :q to quit the program.

See http://github.com/frankduncan/clnl for more information about CLNL and to
keep apprised of any updates that may happen.")

(defun initialize (&key dims view buttons)
 "INITIALIZE &key DIMS VIEW BUTTONS => RESULT

  DIMS: (:xmin XMIN :xmax XMAX :ymin YMIN :ymax YMAX :patch-size PATCH-SIZE)
  VIEW: (:left LEFT :top TOP)
  BUTTONS: BUTTON-DEF*
  BUTTON-DEF: (:left LEFT :top TOP :height HEIGHT :width WIDTH :display DISPLAY)

ARGUMENTS AND VALUES:

  RESULT: undefined
  XMIN: An integer representing the minimum patch coord in X
  XMAX: An integer representing the maximum patch coord in X
  YMIN: An integer representing the minimum patch coord in Y
  YMAX: An integer representing the maximum patch coord in Y
  PATCH-SIZE: A double representing the size of the patches in pixels
  LEFT: An integer representing the left position
  TOP: An integer representing the top position
  HEIGHT: An integer representing height
  WIDTH: An integer representing width
  DISPLAY: A string representing display name

DESCRIPTION:

  This is where the initialization of the interface that sits behind
  the interface lives.  From here, one can go into headless or running
  mode, but for certain things this interface will still need to act,
  and also allows for bringing up and taking down of visual elements."
 (setf *dimensions* (append dims view))
 (let
  ((known-button-names nil))
  (setf *widgets*
   (mapcar
    (lambda (button-def)
     (let
      ((idx (length (remove (getf button-def :display) known-button-names :test-not #'equal))))
      (push (getf button-def :display) known-button-names)
      (list
       button-def
       (clnl-gltk:button
        (getf button-def :left)
        (- *window-height* (getf button-def :height) (getf button-def :top))
        (getf button-def :width)
        (getf button-def :height)
        (getf button-def :display)
        (lambda ()
         (execute
          (format nil ":button \"~A\"~A"
           (getf button-def :display)
           (if (zerop idx) "" (format nil " ~A" idx)))))))))
    buttons))))

(defun run ()
 "RUN => RESULT

ARGUMENTS AND VALUES:

  RESULT: undefined, should never get here

DESCRIPTION:

  RUN runs the view in an external window.

  This should be run inside another thread as it starts the glut main-loop.
  Closing this window will then cause the entire program to terminate."
 ; I do this because I don't know who or what in the many layers
 ; is causing the floating point errors, but I definitely don't
 ; want to investigate until simply ignoring them becomes a problem.
 (sb-int:with-float-traps-masked (:invalid :inexact :divide-by-zero :overflow :underflow)
  (cl-glut:init)
  (cl-glut:init-window-size *window-width* *window-height*)
  (cl-glut:init-display-mode :double :rgba)
  (cl-glut:create-window "CLNL Test Window")
  (setf *glut-window-opened* t)
  (gl:clear-color 0 0 0 1)
  (cl-glut:display-func (cffi:get-callback 'display))
  (glut:reshape-func (cffi:callback reshape))
  (cl-glut:idle-func (cffi:get-callback 'idle))
  (cl-glut:close-func (cffi:get-callback 'close-func))
  (cl-glut:keyboard-func (cffi:get-callback 'key-pressed))
  (cl-glut:special-func (cffi:get-callback 'special-key-pressed))
  (cl-glut:motion-func (cffi:get-callback 'motion)) ; while mouse is down
  (cl-glut:passive-motion-func (cffi:get-callback 'motion)) ; while mouse is up
  (cl-glut:mouse-func (cffi:get-callback 'mouse)) ; state is up/down, button is button
  (gl:depth-func :lequal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :blend)
  (set-turtle-lists)
  (set-patch-list)
  (clnl-gltk:setup)
  (setf *textbox*
   (clnl-gltk:textbox
    5 (+ clnl-gltk:*font-height* 14)
    10 12
    (format nil *initial-banner* (asdf:component-version (asdf:find-system :clnl)))))
  (setf *inputbox* (clnl-gltk:inputbox 5 5 10))
  (cl-glut:main-loop)))

(defun patch-size () (getf *dimensions* :patch-size))

(defun world-width-in-pixels ()
 (floor (* (patch-size) (1+ (- (getf *dimensions* :xmax) (getf *dimensions* :xmin))))))

(defun world-height-in-pixels ()
 (floor (* (patch-size) (1+ (- (getf *dimensions* :ymax) (getf *dimensions* :ymin))))))

(defun export-view ()
 "EXPORT-VIEW => IMAGE-DATA

ARGUMENTS AND VALUES:

  IMAGE-DATA: A vector, pixel data as returned by opengls readPixels

DESCRIPTION:

  EXPORT-VIEW returns the current view in raw data of RGBA pixels.

  Each pixel is made up of 4 bytes of data, which an be walked over.  The number
  of pixels is the current width x height.  Converting to some other image format
  is a matter of pulling that information out and putting it into whatever format
  you like.

  This requires opengl to run, but can be used with xvfb in a headless mode."
 (sb-int:with-float-traps-masked (:invalid)
  (when (not *glut-window-opened*)
   (cl-glut:init)
   (cl-glut:init-window-size 1 1)
   (cl-glut:create-window "CLNL Test Window")
   (gl:clear-color 0 0 0 1)
   (set-turtle-lists)
   (set-patch-list)
   (setf *glut-window-opened* t))
  (let
   ((fbo (first (gl:gen-framebuffers 1)))
    (render-buf (first (gl:gen-renderbuffers 1)))
    (width (world-width-in-pixels))
    (height (world-height-in-pixels)))
   (gl:bind-framebuffer :framebuffer fbo)
   (gl:bind-renderbuffer :renderbuffer render-buf)
   (gl:renderbuffer-storage :renderbuffer :rgba8 width height)
   (gl:framebuffer-renderbuffer :draw-framebuffer :color-attachment0 :renderbuffer render-buf)
   (gl:viewport 0 0 width height)
   (render-scene)
   (gl:read-pixels 0 0 width height :rgba :unsigned-byte))))
