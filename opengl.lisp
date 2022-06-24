(in-package #:kons-9)

;;;; C interface ========================================================

(defmacro with-c-array-1 (vec &body forms)
  `(multiple-value-bind (v vp)
       (make-heap-ivector 1 'single-float)
       (setf (aref v 0) (aref ,vec 0))
     (unwind-protect
          (progn ,@forms)
       (dispose-heap-ivector v)
       (dispose-heap-ivector vp))))

(defmacro with-c-array-3 (vec &body forms)
  `(multiple-value-bind (v vp)
       (make-heap-ivector 3 'single-float)
       (setf (aref v 0) (aref ,vec 0))
       (setf (aref v 1) (aref ,vec 1))
       (setf (aref v 2) (aref ,vec 2))
     (unwind-protect
          (progn ,@forms)
       (dispose-heap-ivector v)
       (dispose-heap-ivector vp))))

(defmacro with-c-array-4 (vec &body forms)
  `(multiple-value-bind (v vp)
       (make-heap-ivector 4 'single-float)
       (setf (aref v 0) (aref ,vec 0))
       (setf (aref v 1) (aref ,vec 1))
       (setf (aref v 2) (aref ,vec 2))
       (setf (aref v 3) (aref ,vec 3))
     (unwind-protect
          (progn ,@forms)
       (dispose-heap-ivector v)
       (dispose-heap-ivector vp))))

;;;; graphics ===========================================================

(defun draw-axes (size)
  (#_glLineWidth 3.0)
  (#_glBegin #$GL_LINES)
  (#_glColor3f 1.0 0.0 0.0)
  (#_glVertex3f 0.0  0.0  0.0)
  (#_glVertex3f size 0.0  0.0)
  (#_glColor3f 0.0 1.0 0.0)
  (#_glVertex3f 0.0 0.0  0.0 )
  (#_glVertex3f 0.0 size 0.0 )
  (#_glColor3f 0.0 0.0 1.0)
  (#_glVertex3f 0.0  0.0  0.0)
  (#_glVertex3f 0.0  0.0 size)
  (#_glEnd))

(defun draw-ground-plane (size segments)
  (#_glColor3f 0.8 0.8 0.8)
  (#_glLineWidth 1.0)
  (#_glBegin #$GL_LINES)
  (dotimes (i (+ segments 1))
    (let* ((f (/ i segments))
           (coord (lerp f (- size) size)))
      (#_glVertex3f coord 0.0 (- size))
      (#_glVertex3f coord 0.0    size)
      (#_glVertex3f (- size) 0.0 coord)
      (#_glVertex3f    size  0.0 coord)))
  (#_glEnd))

(defparameter *cam-x-rot* 0.0)
(defparameter *cam-y-rot* 0.0)
(defparameter *cam-fwd-dist* 0.0)
(defparameter *cam-side-dist* 0.0)
(defparameter *cam-up-dist* 0.0)

(defparameter *do-lighting?* t)
(defparameter *display-filled?* t)
(defparameter *display-wireframe?* t)
(defparameter *display-points?* t)
(defparameter *do-backface-cull?* t)
(defparameter *do-smooth-shading?* nil)
(defparameter *display-ground-plane?* t)
(defparameter *display-axes?* t)
;; (defparameter *light-0-on?* t)
;; (defparameter *light-1-on?* t)

;(defparameter *shading-color* (c! 0.8 0.8 0.8))
(defparameter *shading-color* (c! 1 1 1))
(defparameter *light-color* (c! 2 2 2))
;; (defparameter *fg-color* (c! 1 1 1))
;; (defparameter *bg-color* (c! 0 0 0))
(defparameter *fg-color* (c! 0 0 0))
(defparameter *bg-color* (c! 1 1 1))
(defparameter *sel-color* (c! 1 0 0))

(defun set-theme-bright ()
  (setf *fg-color* (c! 0 0 0))
  (setf *bg-color* (c! 1 1 1)))

(defun set-theme-dark ()
  (setf *fg-color* (c! 1 1 1))
  (setf *bg-color* (c! 0 0 0)))

(defun init-view-camera ()
  (setf *cam-x-rot* 15.0)
  (setf *cam-y-rot* -35.0)
  (setf *cam-fwd-dist* -10.0)
  (setf *cam-side-dist* 0.0)
  (setf *cam-up-dist* -2.0))

(defun gl-set-fg-color ()
  (#_glColor3f (c-red *fg-color*) (c-green *fg-color*) (c-blue *fg-color*)))
  
(defun gl-set-sel-color ()
  (#_glColor3f (c-red *sel-color*) (c-green *sel-color*) (c-blue *sel-color*)))

(defun gl-enable-light (light-id dir &optional (color *light-color*))
  (#_glEnable light-id)

  (with-c-array-4 (vector (x dir) (y dir) (z dir) 0.0)
    (#_glLightfv light-id #$GL_POSITION vp))

  (with-c-array-4 (vector 0.25 0.25 0.25 1.0)
    (#_glLightfv light-id #$GL_AMBIENT vp))
  (with-c-array-4 (vector (c-red color) (c-green color) (c-blue color) 1.0)
    (#_glLightfv light-id #$GL_DIFFUSE vp))
  (with-c-array-4 (vector (c-red color) (c-green color) (c-blue color) 1.0)
    (#_glLightfv light-id #$GL_SPECULAR vp))
  )

(defun gl-disable-light (light-id)
  (#_glDisable light-id))

(defun gl-set-material (&optional (diff *shading-color*) (spec (c! 0 0 0)) (shine 0.0))
  (with-c-array-4 (vector (c-red diff) (c-green diff) (c-blue diff) 1.0)
    (#_glMaterialfv #$GL_FRONT_AND_BACK #$GL_DIFFUSE vp))
  (with-c-array-4 (vector (c-red spec) (c-green spec) (c-blue spec) 1.0)
    (#_glMaterialfv #$GL_FRONT_AND_BACK #$GL_SPECULAR vp))
  (with-c-array-1 (vector shine)
    (#_glMaterialfv #$GL_FRONT_AND_BACK #$GL_SHININESS vp)))
  
(defun new-pixel-format (&rest attributes)
  ;; take a list of opengl pixel format attributes (enums and other
  ;; small ints), make an array (character array?), and create and
  ;; return an NSOpenGLPixelFormat
  (let* ((attribute-size (ccl::foreign-size #>NSOpenGLPixelFormatAttribute :bytes))
         (nattributes (length attributes)))
    (ccl::%stack-block ((objc-attributes (* attribute-size (1+ nattributes))))
      (loop for i from 0 to nattributes
            for attribute in attributes do
            (setf (ccl:paref objc-attributes (:* #>NSOpenGLPixelFormatAttribute) i) 
                  attribute) ; <- autocoerced?
            finally (setf 
                     (ccl:paref objc-attributes 
                                (:* #>NSOpenGLPixelFormatAttribute) nattributes) 0))
      (make-instance ns:ns-opengl-pixel-format :with-attributes objc-attributes))))

(defun update-light-settings ()
  (if *do-backface-cull?*
      (progn
        (#_glEnable #$GL_CULL_FACE)
        (#_glLightModeli #$GL_LIGHT_MODEL_TWO_SIDE #$GL_FALSE))
      (progn
        (#_glDisable #$GL_CULL_FACE)
        (#_glLightModeli #$GL_LIGHT_MODEL_TWO_SIDE #$GL_TRUE)))

  (if *do-lighting?*
      (#_glEnable #$GL_LIGHTING)
      (#_glDisable #$GL_LIGHTING))
  (let ((mtx (matrix-multiply (make-x-rotation-matrix (- (radians *cam-x-rot*)))
                              (make-y-rotation-matrix (- (radians *cam-y-rot*))))))
    (gl-enable-light #$GL_LIGHT0 (transform-point (p! 0 0 1) mtx) (c! 0.7 0.7 0.7))))

;; (if *light-0-on?*
  ;;     (gl-enable-light #$GL_LIGHT0 (p! 0 1 0.8) (c! 0.7 0.7 0.7))
  ;;     (gl-disable-light #$GL_LIGHT0))
  ;; (if *light-1-on?*
  ;;     (gl-enable-light #$GL_LIGHT1 (p! 1 -0.5 0) (c! 0.3 0.3 0.3))
  ;;     (gl-disable-light #$GL_LIGHT1))
  ;; (when (and (not *light-0-on?*) (not *light-1-on?*)) ;use camera light
  ;;   (let ((mtx (matrix-multiply (make-x-rotation-matrix (- (radians *cam-x-rot*)))
  ;;                               (make-y-rotation-matrix (- (radians *cam-y-rot*))))))
  ;;     (gl-enable-light #$GL_LIGHT0 (transform-point (p! 0 0 1) mtx))))
;;  )


;;;; ui-view ===================================================================

(defclass ui-view (ns:ns-view)
  ((bg-color :accessor bg-color :initarg :bg-color :initform (c! 1 1 1 0)))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self ui-view) &rest initargs)
  (declare (ignore initargs))
  (#/setWantsLayer: self t)
  (#/setBorderWidth: (#/layer self) 1)
  (#/setCornerRadius: (#/layer self) 0))

(objc:defmethod (#/drawRect: :void) ((self ui-view) (rect :<NSR>ect))
  (with-accessors ((bg bg-color))
      self
    (#/setFill (#/colorWithCalibratedRed:green:blue:alpha:
                ns:ns-color (c-red bg) (c-green bg) (c-blue bg) (c-alpha bg))))
                                        ;  (#/setFill (#/whiteColor ns:ns-color))
  (#_NSRectFill (#/bounds self)))


;;;; ui-menu-item ==============================================================

(defclass ui-menu-item (ui-view)
  ((title :accessor title :initarg :title :initform "Menu Item")
   (action-fn :accessor action-fn :initarg :action-fn :initform nil))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/drawRect: :void) ((self ui-menu-item) (rect :<NSR>ect))
  (call-next-method rect)
  (let* ((str-width (ns:ns-size-width (#/sizeWithAttributes: (objc:make-nsstring (title self)) nil)))
         (rect-width (ns:ns-rect-width (#/frame self)))
         (x (/ (- rect-width str-width) 2)))
    (#/drawAtPoint:withAttributes: (objc:make-nsstring (title self))
                                   (ns:make-ns-point x 3)
                                   nil)))

;; (objc:defmethod (#/mouseEntered: :void) ((self ui-menu-item) event)
;;   (declare (ignore event))
;;   (print (title self)); xxx do highlight here
;;   (#/setNeedsDisplay: self t))


;;;; ui-popup-menu =============================================================

(defclass ui-popup-menu (ui-view)
  ((menu-items :accessor menu-items :initarg :menu-items :initform '()))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self ui-popup-menu) &rest initargs)
  (declare (ignore initargs))
  (setf (bg-color self) (c! 1 1 1 0.5))
  )

(defmethod update-layout ((self ui-popup-menu))
  (#/setSubviews: self (#/array ns:ns-array)) ;remove all subviews
  (let ((y 0))
    (dolist (menu-item (menu-items self))
      (#/setFrame: menu-item (ns:make-ns-rect 0 y 300 20))
      (incf y 20)
      (#/addSubview: self menu-item)
      (#/release menu-item))
    (let ((rect (ns:make-ns-rect 50 50 300 y)))
      (#/setFrame: self rect))))

(defmethod popup ((self ui-popup-menu) view)
  (let* ((menu-rect (#/frame self))
         (menu-width (ns:ns-rect-width menu-rect))
         (menu-height (ns:ns-rect-height menu-rect))
         (view-rect (#/frame view))
         (view-width (ns:ns-rect-width view-rect))
         (view-height (ns:ns-rect-height view-rect))
         (x (/ (- view-width menu-width) 2))
         (y (/ (- view-height menu-height) 2)))
    (#/setFrameOrigin: self (ns:make-ns-point x y))
    (#/addSubview: view self)))

(defmethod popdown ((self ui-popup-menu))
  (#/removeFromSuperview self))
;  (setf (menu-is-visible? self) nil)
;  (unhighlight-items self))

(defmethod unhighlight-items ((self ui-popup-menu))
  (let ((items (#/subviews self)))
    (dotimes (i (#/count items))
      (#/setBorderWidth: (#/layer (#/objectAtIndex: items i)) 1))))

;;;; ui-status-bar =============================================================

(defclass ui-status-bar (ui-view)
  ((info-1 :accessor info-1 :initarg :info-1 :initform "Mouse: orbit, [option] left/right up/down, [command] in/out")
   (info-2 :accessor info-2 :initarg :info-2 :initform "Information 2")
   (info-3 :accessor info-3 :initarg :info-3 :initform "Information 3")
   (info-4 :accessor info-4 :initarg :info-4 :initform "Information 4")
   (info-5 :accessor info-5 :initarg :info-5 :initform "Information 5"))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/drawRect: :void) ((self ui-status-bar) (rect :<NSR>ect))
  (call-next-method rect)
  ;; (let* ((str-width (ns:ns-size-width (#/sizeWithAttributes: (objc:make-nsstring (title self)) nil)))
  ;;        (rect-width (ns:ns-rect-width (#/frame self)))
  ;;        (x (/ (- rect-width str-width) 2)))
  (#/drawAtPoint:withAttributes: (objc:make-nsstring (info-1 self)) (ns:make-ns-point 20 3) nil)
  (#/drawAtPoint:withAttributes: (objc:make-nsstring (info-2 self)) (ns:make-ns-point 400 3) nil)
  (#/drawAtPoint:withAttributes: (objc:make-nsstring (info-3 self)) (ns:make-ns-point 600 3) nil)
  (#/drawAtPoint:withAttributes: (objc:make-nsstring (info-4 self)) (ns:make-ns-point 900 3) nil)
  (#/drawAtPoint:withAttributes: (objc:make-nsstring (info-5 self)) (ns:make-ns-point 1200 3) nil)
  )

;;;; scene-view ================================================================

(defclass scene-view (ns:ns-opengl-view)
  ((scene :accessor scene :initarg :scene :initform nil)
   (status-bar :accessor status-bar :initarg :status-bar :initform nil)
   (popup-menu :accessor popup-menu :initarg :popup-menu :initform nil))
  (:metaclass ns:+ns-object))

(defun default-popup-menu ()
  (let ((menu (make-instance 'ui-popup-menu)))
    (dotimes (i 5)
      (push (make-instance 'ui-menu-item
                           :title (format nil "Menu Item ~a" i)
                           :action-fn #'(lambda (item)
                                          (format t "Menu item \"~a\" selected.~%" (title item))))
            (menu-items menu)))
    (update-layout menu)
    menu))

(defmethod initialize-instance :after ((self scene-view) &rest initargs)
  (declare (ignore initargs))
  (#/setPixelFormat: self (new-pixel-format ;#$NSOpenGLPFAOpenGLProfile 
                                            ;#$NSOpenGLProfileVersion3_2Core
                                            ;#$NSOpenGLPFADoubleBuffer
                                            #$NSOpenGLPFAColorSize 32
                                            #$NSOpenGLPFADepthSize 24))
  (#/setWantsLayer: self t)
  (#/setBorderWidth: (#/layer self) 1)
;  (#/setCornerRadius: (#/layer self) 10)
  (init-view-camera))

;;; display the view
(objc:defmethod (#/drawRect: :void) ((self scene-view) (rect :<NSR>ect))
  (#_glClearColor (c-red *bg-color*) (c-green *bg-color*) (c-blue *bg-color*) 0.0)

  (#_glClear (logior #$GL_COLOR_BUFFER_BIT #$GL_DEPTH_BUFFER_BIT))
  (#_glEnable #$GL_DEPTH_TEST)
  (#_glCullFace #$GL_BACK)

  (update-light-settings)
  
  (#_glMatrixMode #$GL_PROJECTION)
  (#_glLoadIdentity)
  (#_gluPerspective 45.0d0 (/ 16.0d0 9.0d0) 0.01d0 1000.0d0)
  (#_glMatrixMode #$GL_MODELVIEW)
  (#_glLoadIdentity)

;;  (#_gluLookAt 4.0d0 3.0d0 5.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 0.0d0)

  (#_glTranslatef *cam-side-dist* *cam-up-dist* *cam-fwd-dist*)
  (#_glRotatef *cam-x-rot* 1.0 0.0 0.0)
  (#_glRotatef *cam-y-rot* 0.0 1.0 0.0)

  (when (scene self)
    (draw (scene self)))

  (#_glDisable #$GL_LIGHTING)
  (when *display-ground-plane?*
    (draw-ground-plane 10.0 10))
  (when *display-axes?*
    (draw-axes 3.0))
  (gl-set-fg-color)
  (#_glFlush))

;;; respond to first click in window
(objc:defmethod (#/acceptsFirstMouse: :<BOOL>) ((self scene-view) event)
  (declare (ignore event))
  t)

(defmethod update-status-bar-mouse-loc ((self ui-status-bar) p)
  (setf (info-2 self)
        (format nil "Cursor: (~a, ~a)" (round (ns:ns-point-x p)) (round (ns:ns-point-y p))))
  (#/setNeedsDisplay: self t))

(objc:defmethod (#/mouseMoved: :void) ((self scene-view) event)
  (let ( ;(flags (#/modifierFlags event))
        (p (#/locationInWindow event)))
    (update-status-bar-mouse-loc (status-bar self) (#/convertPoint:fromView: self p nil))
    (when (popup-menu self)
      (unhighlight-items (popup-menu self))
      ;; highlight menu item under mouse
      (let ((widget (#/hitTest: self p)))
        (when (typep widget 'ui-menu-item)
          (#/setBorderWidth: (#/layer widget) 3)))
      (#/setNeedsDisplay: self t))))

(objc:defmethod (#/mouseDown: :void) ((self scene-view) event)
  (let ((flags (#/modifierFlags event))
        (p (#/locationInWindow event)))
;    (format t "~a, ~a, ~a~%" p flags #$NSControlKeyMask)
    ;; (when (and (= flags 262145) ;NSControlKeyMask
    ;;            (popup-menu self)
    ;;            (not (menu-is-visible? (popup-menu self))))
    ;;   (popup (popup-menu self) self))
    (let ((widget (#/hitTest: self p)))
      (when (typep widget 'ui-menu-item)
        (when (action-fn widget)
          (funcall (action-fn widget) widget)))))
  (#/setNeedsDisplay: self t))

;;; accept key events
(objc:defmethod (#/acceptsFirstResponder :<BOOL>) ((self scene-view))
   t)

(objc:defmethod (#/mouseDragged: :void) ((self scene-view) event)
  (let ((flags (#/modifierFlags event))
        (p (#/locationInWindow event))
        (dx (coerce (#/deltaX event) 'single-float))
        (dy (coerce (#/deltaY event) 'single-float)))
    ;;    (format t "~a, ~a~%" flags #$NSAlternateKeyMask)
    (update-status-bar-mouse-loc (status-bar self) (#/convertPoint:fromView: self p nil))
    (cond ((or (= flags 524320) (= flags 524352)) ; #$NSAlternateKeyMask -- this has been deprecated
           (if (>= (abs dx) (abs dy))
               (incf *cam-side-dist* (* 0.1 dx))
               (incf *cam-up-dist* (* -0.1 dy))))
          ((or (= flags 1048584) (= flags 1048592)) ; command
           (incf *cam-fwd-dist* (* 0.1 dx)))
          (t
           (incf *cam-x-rot* dy)
           (incf *cam-y-rot* dx))))
  (redraw))

(defun print-viewport-help ()
  (format t "Mouse drag: orbit, [option] track left/right and up/down, [command] track in/out~%~
`: toggle lighting~%~
1: toggle filled display~%~
2: toggle wireframe display~%~
3: toggle point display~%~
4: toggle backface culling~%~
5: toggle smooth shading~%~
6: toggle ground plane display~%~
7: toggle axes display~%~
z: reset camera~%~
a: init scene~%~
space: update scene (hold down for animation) ~%~
tab: show/hide contextual menu ~%~
h or ?: print this help message~%"))

;; x: call GENERATE-SCENE function ~%~
;; c: update scene for 100 frames ~%~
;; v: call GENERATE-SCENE and update scene for 100 frames ~%~

;;; update scene when key pressed
(objc:defmethod (#/keyDown: :void) ((self scene-view) event)
  (let* ((str (objc:lisp-string-from-nsstring (#/charactersIgnoringModifiers event)))
         (char (char str 0))
         (scene (scene self)))
    (case char
          ;;; #\tab #\^Y 
      (#\h (print-viewport-help))
      (#\? (print-viewport-help))
      (#\a (when scene (init-scene scene)))
      (#\` (setf *do-lighting?* (not *do-lighting?*)))
      ;; (#\1 (setf *light-0-on?* (not *light-0-on?*)))
      ;; (#\2 (setf *light-1-on?* (not *light-1-on?*)))
      (#\1 (setf *display-filled?* (not *display-filled?*)))
      (#\2 (setf *display-wireframe?* (not *display-wireframe?*)))
      (#\3 (setf *display-points?* (not *display-points?*)))
      (#\4 (setf *do-backface-cull?* (not *do-backface-cull?*)))
      (#\5 (setf *do-smooth-shading?* (not *do-smooth-shading?*)))
      (#\6 (setf *display-ground-plane?* (not *display-ground-plane?*)))
      (#\7 (setf *display-axes?* (not *display-axes?*)))
      (#\z (init-view-camera))
      ;; (#\x (when (scene self) (generate-scene (scene self))))
      ;; (#\c (when (scene self) (dotimes (i 100) (update-scene (scene self)))))
      ;; (#\v (when (scene self)
      ;;        (generate-scene (scene self))
      ;;        (dotimes (i 100) (update-scene (scene self)))))
;      (#\space (when (scene self) (update-scene (scene self))))))
      (#\space (dolist (v *scene-views*) (update-scene (scene v))))

      (#\tab (if (null (popup-menu self))
                 (menu-popup self)
                 (menu-popdown self)))))
  (redraw))

(defmethod menu-popup ((self scene-view))
  (setf (popup-menu self) (default-popup-menu))
  (popup (popup-menu self) self))

(defmethod menu-popdown ((self scene-view))
  (popdown (popup-menu self))
  (setf (popup-menu self) nil))

;;;; app-window ================================================================

(defclass app-window (ns:ns-window)
  ()
  (:metaclass ns:+ns-object))

(defparameter *window-x-size* 960)
(defparameter *window-y-size* 540)

;;; create and display a window containing an OpeGL view
(defun show-window (scene)
  (setf *scene-views* '())
  (ns:with-ns-rect ;(frame 0 0 640 360)
    (frame 0 0 1400 (+ *window-y-size* 20)) ;960 540)
    (let* ((w (make-instance 'app-window
                             :title "foo"
                             :with-content-rect frame
                             :style-mask (logior #$NSTitledWindowMask
                                                 #$NSClosableWindowMask
                                                 #$NSMiniaturizableWindowMask)
                             :backing #$NSBackingStoreBuffered
                             :defer t))
           (s (make-instance 'ui-status-bar :scene scene))
           (v (make-instance 'scene-view :scene scene :status-bar s)))
;      (#/setContentView: w v)

      (#/addSubview: (#/contentView w) v)
      (#/setFrameOrigin: v (ns:make-ns-point 0 20))
      (#/setFrameSize: v (ns:make-ns-point *window-x-size* *window-y-size*))
      (push v *scene-views*)

      (#/addSubview: (#/contentView w) s)
      (#/setFrameOrigin: s (ns:make-ns-point 0 0))
      (#/setFrameSize: s (ns:make-ns-point 1400 20))

      (#/setAcceptsMouseMovedEvents: w t) ;for mouseMoved events
      
      (redraw)
      (#/release v)
      (#/center w)
      (#/orderFront: w nil)
      w)))

;;; create and display a window containing a grid of OpeGL views
(defun show-grid-window (grid-size)
  (setf *scene-views* '())
  (ns:with-ns-rect
    (frame 0 0 *window-x-size* *window-y-size*)
    (let* ((w (make-instance 'ns:ns-window
                             :title "foo"
                             :with-content-rect frame
                             :style-mask (logior #$NSTitledWindowMask
                                                 #$NSClosableWindowMask
                                                 #$NSMiniaturizableWindowMask)
                             :backing #$NSBackingStoreBuffered
                             :defer t))
           (dx (floor (/ *window-x-size* grid-size)))
           (dy (floor (/ *window-y-size* grid-size))))

      (dotimes (y grid-size)
        (dotimes (x grid-size)
          (let ((v (make-instance 'scene-view :scene (make-instance 'scene))))
            (#/addSubview: (#/contentView w) v)
            (#/setFrameOrigin: v (ns:make-ns-point (* x dx) (* y dy)))
            (#/setFrameSize: v (ns:make-ns-point dx dy))
            (push v *scene-views*)
            (#/release v))))

      (setf *scene-views* (reverse *scene-views*))
      
      (redraw)
      (#/center w)
      (#/orderFront: w nil)
      w)))

(defmacro with-redraw (&body body)
  `(let ((result (progn ,@body)))
     (redraw)
     result))

;;; with-redraw macro: add variant that clears scene
(defmacro with-clear-and-redraw (&body body)
  `(progn
     (clear-scene *scene*)
     (setf (init-done? *scene*) nil)
     (setf (current-frame *scene*) 0)
     (let ((_result (progn ,@body)))
       (redraw)
       _result)))

(defmacro with-grid-clear-and-redraw (&body body)
  `(progn
     (dolist (v *scene-views*)
       (clear-scene (scene v)))
     (let ((_result (progn ,@body)))
       (redraw)
       _result)))

(defmacro with-gl-enable (flag &body body)
  `(progn
     (#_glEnable ,flag)
     (let ((result (progn ,@body)))
       (#_glDisable ,flag)
       result)))

(defmacro with-gl-disable (flag &body body)
  `(progn
     (#_glDisable ,flag)
     (let ((result (progn ,@body)))
       (#_glEnable ,flag)
       result)))


;(defun pick-in-window ()
;  (#_gluUnProject 
