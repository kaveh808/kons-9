(in-package #:kons-9)

;;;; user input ========================================================

(defun get-input (prompt)
  (format t "~&~a: " prompt)
;  (finish-output)
  (force-output t)
  (read-line nil))

(defun get-float-input (prompt default-value)
  (let ((str (get-input (format nil "~a (~a)" prompt default-value))))
    (if (= 0 (length str))
        default-value
        (coerce (read-from-string str) 'float))))

(defun get-integer-input (prompt default-value)
  (let ((str (get-input (format nil "~a (~a)" prompt default-value))))
    (if (= 0 (length str))
        default-value
        (coerce (read-from-string str) 'integer))))

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
  (setf *cam-up-dist* 0.0)) ;-2.0))

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

;;;; ui utils ==================================================================

(defun draw-centered-text (str rect-width y)
  (let* ((dict (make-instance 'ns:ns-mutable-dictionary :with-capacity 2)))
    (#/setObject:forKey: dict (#/fontWithName:size:
                               ns:ns-font
                               #@"Monaco" 12.0) #&NSFontAttributeName)
    (let* ((str-width (ns:ns-size-width (#/sizeWithAttributes: (objc:make-nsstring str) dict)))
           (x (/ (- rect-width str-width) 2)))
      (#/drawAtPoint:withAttributes: (objc:make-nsstring str)
                                     (ns:make-ns-point x y)
                                     dict))))

(defparameter *popup-menu-width* 500)
(defparameter *ui-button-item-height* 25)

(defparameter *ui-inactive-border-width* 1)
(defparameter *ui-inactive-border-color* (#/CGColor (#/blackColor ns:ns-color)))
(defparameter *ui-active-border-width* 3)
(defparameter *ui-active-border-color* (#/CGColor (#/blueColor ns:ns-color)))

(defun ui-set-border-color (view cg-color)
  (#/setBorderColor: (#/layer view) cg-color))

(defun ui-switch-active-view (new-view old-view)
  (setf (is-active-view? new-view) t)
  (setf (is-active-view? old-view) nil)
  ;; highlight new-view
  (#/setBorderWidth: (#/layer new-view) *ui-active-border-width*)
  (#/setBorderColor: (#/layer new-view) *ui-active-border-color*)
  ;; unhighlight old-view
  (#/setBorderWidth: (#/layer old-view) *ui-inactive-border-width*)
  (#/setBorderColor: (#/layer old-view) *ui-inactive-border-color*)
  ;; redraw
  (#/setNeedsDisplay: new-view t)
  (#/setNeedsDisplay: old-view t))

;;;; ui-view ===================================================================

(defclass ui-view (ns:ns-view)
  ((bg-color :accessor bg-color :initarg :bg-color :initform (c! 1 1 1 0))
   (fg-color :accessor fg-color :initarg :fg-color :initform (c! 0 0 0 1)))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((view ui-view) &rest initargs)
  (declare (ignore initargs))
  (#/setWantsLayer: view t)
  (#/setBorderWidth: (#/layer view) 1)
  (#/setBorderColor: (#/layer view) (#/CGColor (#/blackColor ns:ns-color)))
  (#/setCornerRadius: (#/layer view) 0))

(objc:defmethod (#/drawRect: :void) ((view ui-view) (rect :<NSR>ect))
  (with-accessors ((bg bg-color) (fg fg-color)) view
    (#/setFill (#/colorWithCalibratedRed:green:blue:alpha:
                ns:ns-color (c-red bg) (c-green bg) (c-blue bg) (c-alpha bg)))
    (#_NSRectFill (#/bounds view))
    ;; (#/setStroke (#/colorWithCalibratedRed:green:blue:alpha:
    ;;             ns:ns-color (c-red fg) (c-green fg) (c-blue fg) (c-alpha fg)))
    ;; (#_NSFrameRectWithWidth: (#/bounds view) 3.0)))
    ))

;;;  (#/setFill (#/whiteColor ns:ns-color))

(defmethod unhighlight-items ((view ui-view))
  (let ((items (#/subviews view)))
    (dotimes (i (#/count items))
      (#/setBorderWidth: (#/layer (#/objectAtIndex: items i)) 1))))

;;;; ui-button-item ============================================================

(defclass ui-button-item (ui-view)
  ((title :accessor title :initarg :title :initform "Menu Item")
   (action-fn :accessor action-fn :initarg :action-fn :initform nil))
  (:metaclass ns:+ns-object))
  
(objc:defmethod (#/drawRect: :void) ((view ui-button-item) (rect :<NSR>ect))
  (call-next-method rect)
  (draw-ui-text view))

(defmethod draw-ui-text ((view ui-button-item))
  (draw-centered-text (title view) (ns:ns-rect-width (#/frame view)) 5))

;;;; ui-menu-item ==============================================================

(defclass ui-menu-item (ui-button-item)
  ()
  (:metaclass ns:+ns-object))

;;;; ui-popup-menu =============================================================

(defclass ui-popup-menu (ui-view)
  ((ui-items :accessor ui-items :initarg :ui-items :initform '()))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((view ui-popup-menu) &rest initargs)
  (declare (ignore initargs))
  (setf (bg-color view) (c! 1 1 1 0.9))
  )

(defmethod update-layout ((menu ui-popup-menu) &optional (view nil))
  (declare (ignore view))
  (#/setSubviews: menu (#/array ns:ns-array)) ;remove all subviews
  (let ((y 0))
    (dolist (menu-item (ui-items menu))
      (#/setFrame: menu-item (ns:make-ns-rect 0 y *popup-menu-width* *ui-button-item-height*))
      (incf y *ui-button-item-height*)
      (#/addSubview: menu menu-item)
      (#/release menu-item))
    (let ((rect (ns:make-ns-rect 50 50 *popup-menu-width* y)))
      (#/setFrame: menu rect)))
  menu)

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

;;;; ui-text-item ==============================================================

(defclass ui-text-item (ui-button-item)
  ((ui-value :accessor ui-value :initarg :ui-value :initform nil))
  (:metaclass ns:+ns-object))
  
(defmethod draw-ui-text ((view ui-text-item))
  (draw-centered-text (format nil "~a: ~a" (title view) (ui-value view))
                      (ns:ns-rect-width (#/frame view)) 5))

;;;; ui-dialog-box =============================================================

(defclass ui-dialog-box (ui-popup-menu)
  ((action-fn :accessor action-fn :initarg :action-fn :initform nil))
  (:metaclass ns:+ns-object))

;; (defmethod initialize-instance :after ((self ui-dialog-box) &rest initargs)
;;   (declare (ignore initargs))
;;   )

(defmethod update-layout ((self ui-dialog-box) &optional (view nil))
  (#/setSubviews: self (#/array ns:ns-array)) ;remove all subviews
  (let ((y 5))
    (dolist (menu-item (ui-items self))
      (#/setFrame: menu-item (ns:make-ns-rect 10 y (- *popup-menu-width* 20) *ui-button-item-height*))
      (incf y *ui-button-item-height*)
      (#/addSubview: self menu-item)
      (#/release menu-item))
    (let ((ok-button (make-instance
                      'ui-button-item
                      :title "OK"
                      :action-fn #'(lambda (item)
                                     (declare (ignore item))
                                     (when (action-fn self)
                                       (funcall (action-fn self)))
                                     (menu-popdown view)))))
      (#/setFrame: ok-button (ns:make-ns-rect 5 (+ y 5)
                                              (- (/ *popup-menu-width* 2) 10) *ui-button-item-height*))
      (#/addSubview: self ok-button)
      (#/release ok-button))
    (let ((cancel-button (make-instance
                          'ui-button-item
                          :title "Cancel"
                          :action-fn #'(lambda (item)
                                         (declare (ignore item))
                                         (menu-popdown view)))))
      (#/setFrame: cancel-button (ns:make-ns-rect (+ (/ *popup-menu-width* 2) 5) (+ y 5)
                                                  (- (/ *popup-menu-width* 2) 10) *ui-button-item-height*))
      (#/addSubview: self cancel-button)
      (#/release cancel-button))
    (incf y *ui-button-item-height*)
    (let ((rect (ns:make-ns-rect 50 50 *popup-menu-width* (+ y 10))))
      (#/setFrame: self rect)))
  self)

;;;; ui-status-bar =============================================================

(defclass ui-status-bar (ui-view)
  ((info-1 :accessor info-1 :initarg :info-1 :initform "Mouse: orbit, [option] left/right up/down, [command] in/out")
   (info-2 :accessor info-2 :initarg :info-2 :initform "Information 2")
   (info-3 :accessor info-3 :initarg :info-3 :initform "Information 3")
   (info-4 :accessor info-4 :initarg :info-4 :initform "Information 4")
   (info-5 :accessor info-5 :initarg :info-5 :initform "Information 5"))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/drawRect: :void) ((view ui-status-bar) (rect :<NSR>ect))
  (call-next-method rect)
  ;; (let* ((str-width (ns:ns-size-width (#/sizeWithAttributes: (objc:make-nsstring (title view)) nil)))
  ;;        (rect-width (ns:ns-rect-width (#/frame view)))
  ;;        (x (/ (- rect-width str-width) 2)))
  (#/drawAtPoint:withAttributes: (objc:make-nsstring (info-1 view)) (ns:make-ns-point 20 3) nil)
  (#/drawAtPoint:withAttributes: (objc:make-nsstring (info-2 view)) (ns:make-ns-point 400 3) nil)
  (#/drawAtPoint:withAttributes: (objc:make-nsstring (info-3 view)) (ns:make-ns-point 600 3) nil)
  (#/drawAtPoint:withAttributes: (objc:make-nsstring (info-4 view)) (ns:make-ns-point 900 3) nil)
  (#/drawAtPoint:withAttributes: (objc:make-nsstring (info-5 view)) (ns:make-ns-point 1200 3) nil)
  )

(defmethod update-status-bar-mouse-loc ((view ui-status-bar) p)
  (setf (info-2 view)
        (format nil "Cursor: (~a, ~a)" (round (ns:ns-point-x p)) (round (ns:ns-point-y p))))
  (#/setNeedsDisplay: view t))

;;;; ui-view-popup-mixin =======================================================

(defclass ui-view-popup-mixin ()
  ((popup-menu :accessor popup-menu :initarg :popup-menu :initform nil)))

(defmethod menu-popup ((view ui-view-popup-mixin) (menu ui-popup-menu))
  (setf (popup-menu view) menu)
  (popup (popup-menu view) view))

(defmethod menu-popdown ((view ui-view-popup-mixin))
  (popdown (popup-menu view))
  (setf (popup-menu view) nil))

;;;; ui-key-binding ============================================================

(defclass ui-key-binding ()
  ((key :accessor key :initarg :key :initform nil)
   (action-fn :accessor action-fn :initarg :action-fn :initform nil)))

(defmethod do-key-action ((binding ui-key-binding) event)
  (let* ((str (objc:lisp-string-from-nsstring (#/charactersIgnoringModifiers event)))
         (char (char str 0)))
    (when (eql (key binding) char)
      (when (action-fn binding)
        (funcall (action-fn binding))))))

;;;; ui-schematic-item =========================================================

(defclass ui-schematic-item (ui-button-item)
  ((shape :accessor shape :initarg :shape :initform nil))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/drawRect: :void) ((view ui-schematic-item) (rect :<NSR>ect))
  (call-next-method rect)
  )

(defmethod set-item-color ((view ui-schematic-item))
  (cond ((is-selected? (shape view))
         (setf (bg-color view) (c! 1 0 0 0.5)))
        ((typep (shape view) 'group)
         (setf (bg-color view) (c! 1 1 0 0.5)))
        ((typep (shape view) 'curve-shape)
         (setf (bg-color view) (c! 1 0 1 0.5)))
        ((typep (shape view) 'uv-mesh)
         (setf (bg-color view) (c! 0.5 1 0.5 0.5)))
        ((typep (shape view) 'particle-system)
         (setf (bg-color view) (c! 1 0.5 1 0.5)))
        ((typep (shape view) 'polyhedron)
         (setf (bg-color view) (c! 0 1 0 0.5)))
        ((typep (shape view) 'animator)
         (setf (bg-color view) (c! 0 0 1 0.5)))
        (t
         (setf (bg-color view) (c! 1 1 1 0.5)))))

(objc:defmethod (#/mouseDown: :void) ((view ui-schematic-item) event)
  (declare (ignore event))
  (when (action-fn view)
    (funcall (action-fn view) view)))

;;;; ui-schematic-view =========================================================

(defparameter *ui-schematic-item-width* 200)
(defparameter *ui-schematic-offset-x* 0.0)
(defparameter *ui-schematic-offset-y* 0.0)
(defparameter *ui-schematic-zoom-factor* 1.0)

(defun reset-schematic-view ()
  (setf *ui-schematic-offset-x* 0.0)
  (setf *ui-schematic-offset-y* 0.0)
  (setf *ui-schematic-zoom-factor* 1.0))

(defclass ui-schematic-view (ui-view ui-view-popup-mixin)
  ((scene :accessor scene :initarg :scene :initform nil)
   (is-active-view? :accessor is-active-view? :initarg :is-active-view? :initform nil)
   (schematic-items :accessor schematic-items :initarg :schematic-items :initform '())
   (scene-view :accessor scene-view :initarg :scene-view :initform nil)
   (status-bar :accessor status-bar :initarg :status-bar :initform nil))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((view ui-schematic-view) &rest initargs)
  (declare (ignore initargs))
  (update-view view)
  )

(defmethod update-view ((view ui-schematic-view))
  (setf (schematic-items view) '())
  (dolist (shape (shapes (scene view)))
    ;; (append (shapes (scene view)) (animators (scene view))))
    (push (make-instance 'ui-schematic-item :shape shape
                                            :title (string-name shape)
                                            :action-fn #'(lambda (item)
                                                           (toggle-selection (scene view) (shape item))
                                                           (set-item-color item)
                                                           (#/setNeedsDisplay: item t)
                                                           (#/setNeedsDisplay: (scene-view view) t)))
          (schematic-items view)))
    (update-layout view))

(defmethod update-layout ((self ui-schematic-view)  &optional (view nil))
  (declare (ignore view))
  (#/setSubviews: self (#/array ns:ns-array)) ;remove all subviews
  (when (popup-menu self)
    (#/addSubview: self (popup-menu self)))
  (let ((y 10))
    (dolist (item (schematic-items self))
      (#/setFrame: item (ns:make-ns-rect (* (+ 10 *ui-schematic-offset-x*) *ui-schematic-zoom-factor*)
                                         (* (+  y *ui-schematic-offset-y*) *ui-schematic-zoom-factor*)
                                         (* *ui-schematic-item-width* *ui-schematic-zoom-factor*)
                                         (* *ui-button-item-height* *ui-schematic-zoom-factor*)))
      (set-item-color item)
      (incf y (* (+ 10 *ui-button-item-height*) *ui-schematic-zoom-factor*))
      (#/addSubview: self item)
;;      (#/release item)   ; memory leak?
      ))
  self)

(defmethod update-status-bar ((self ui-schematic-view) &optional (p nil))
  (let ((sb (status-bar self))
        (scene (scene self)))
    (setf (info-1 sb) "Mouse drag: [option] pan, [command] zoom")
    (setf (info-2 sb) (if p
                          (format nil "Cursor: (~a, ~a)"
                                  (round (ns:ns-point-x p)) (round (ns:ns-point-y p)))
                          ""))
    (setf (info-3 sb) (format nil "Current Frame: ~a" (current-frame scene)))
    (setf (info-4 sb) (format nil "Num Shapes: ~a" (length (shapes scene))))
    (setf (info-5 sb) (format nil "Num Animators: ~a" (length (animators scene))))
    (#/setNeedsDisplay: sb t)))
    
;;; accept key events
(objc:defmethod (#/acceptsFirstResponder :<BOOL>) ((self ui-schematic-view))
   t)

(objc:defmethod (#/mouseMoved: :void) ((self ui-schematic-view) event)
  (let ((p (#/locationInWindow event)))
    (update-status-bar self (#/convertPoint:fromView: self p nil))
    (unhighlight-items self)
    (if (popup-menu self)
        (let ((widget (#/hitTest: self p)))
          (unhighlight-items (popup-menu self))
;;;          (when (typep widget 'ui-menu-item)
          (when (subtypep (type-of widget) 'ui-button-item)
            (#/setBorderWidth: (#/layer widget) 3)))   ;highlight menu item under mouse
        (let ((widget (#/hitTest: self p)))
          (when (typep widget 'ui-schematic-item)
            (#/setBorderWidth: (#/layer widget) 3)))) ;highlight schematic item under mouse
    (#/setNeedsDisplay: self t)))

(objc:defmethod (#/mouseDown: :void) ((self ui-schematic-view) event)
  (let ((p (#/locationInWindow event)))
    (if (not (is-active-view? self))
        (ui-switch-active-view self (scene-view self)))
    (when (popup-menu self)
      (let ((widget (#/hitTest: self p)))
        ;; (when (typep widget 'ui-menu-item)
        (when (subtypep (type-of widget) 'ui-button-item)
          (when (action-fn widget)
            (funcall (action-fn widget) widget)))))
    (#/setNeedsDisplay: (scene-view self) t)
    (update-view self)))                   ;need automatic way of updating

(objc:defmethod (#/mouseDragged: :void) ((self ui-schematic-view) event)
  (let ((flags (#/modifierFlags event))
        (p (#/locationInWindow event))
        (dx (coerce (#/deltaX event) 'single-float))
        (dy (coerce (#/deltaY event) 'single-float)))
    (update-status-bar self (#/convertPoint:fromView: self p nil))
;    (update-status-bar-mouse-loc (status-bar self) (#/convertPoint:fromView: self p nil))
    (cond ((or (= flags 524320) (= flags 524352)) ; #$NSAlternateKeyMask -- this has been deprecated
           (incf *ui-schematic-offset-x* (* 1.0 dx))
           (incf *ui-schematic-offset-y* (* -1.0 dy)))
          ((or (= flags 1048584) (= flags 1048592)) ; command
           (incf *ui-schematic-zoom-factor* (* 0.1 dx)))
          ;; (t
          ;;  drag without modifier ... xxx
          ))
  (update-layout self)
  (redraw))

(defun print-schematic-view-help ()
  (format t "Mouse Drag: [option] pan, [command] zoom~%~
z: reset view pan and zoom~%~
a: init scene~%~
n: clear scene~%~
space: update scene (hold down for animation) ~%~
delete: delete selected items ~%~
tab: show/hide contextual menu ~%~
h or ?: print this help message~%"))

(objc:defmethod (#/keyDown: :void) ((view ui-schematic-view) event)
  (let* ((str (objc:lisp-string-from-nsstring (#/charactersIgnoringModifiers event)))
         (char (char str 0)))
    (case char
      (#\h (print-schematic-view-help))
      (#\? (print-schematic-view-help))
      (#\z (reset-schematic-view))
      (#\a (dolist (v *scene-views*) (init-scene (scene v))))
      (#\n (dolist (v *scene-views*) (clear-scene (scene v))))
      (#\space (dolist (v *scene-views*) (update-scene (scene v))))
      (#\rubout (dolist (v *scene-views*) (remove-current-selection (scene v))))
      (#\tab (if (null (popup-menu view))
                 (menu-popup view (make-popup-menu view))
                 (menu-popdown view)))))
  (update-layout view)
  (redraw))

;;;; scene-view ================================================================

(defclass scene-view (ns:ns-opengl-view ui-view-popup-mixin)
  ((scene :accessor scene :initarg :scene :initform nil)
   (is-active-view? :accessor is-active-view? :initarg :is-active-view? :initform nil)
   (schematic-view :accessor schematic-view :initarg :schematic-view :initform nil)
   (status-bar :accessor status-bar :initarg :status-bar :initform nil))
  (:metaclass ns:+ns-object))

(defmacro menu-item-if-selection-type (required-selection-type menu-item)
  `(when (= 1 (length (selection scene)))
     (let ((shape (first (selection scene))))
       (when (subtypep (type-of shape) ,required-selection-type)
         (push ,menu-item items)))))

(defmacro menu-item-action-entry (title-expr action-expr)
  `(make-instance 'ui-menu-item
                  :title ,title-expr
                  :action-fn #'(lambda (item)
                                 (declare (ignore item))
                                 ,action-expr
                                 (menu-popdown view))))

(defmacro menu-item-submenu-entry (title-expr submenu-expr)
  `(make-instance 'ui-menu-item
                  :title ,title-expr
                  :action-fn #'(lambda (item)
                                 (declare (ignore item))
                                 (menu-popdown view)
                                 (menu-popup view ,submenu-expr))))

(defmacro menu-item-misc-entry (title-expr post-popdown-expr)
  `(make-instance 'ui-menu-item
                  :title ,title-expr
                  :action-fn #'(lambda (item)
                                 (declare (ignore item))
                                 (menu-popdown view)
                                 ,post-popdown-expr)))

(defun make-popup-menu (view)
  (let* ((menu (make-instance 'ui-popup-menu))
         (scene (scene view))
         (items (list (menu-item-action-entry "Bright Theme" (set-theme-bright))
                      (menu-item-action-entry "Dark Theme" (set-theme-dark))
                      (menu-item-submenu-entry "Create Shape..." (make-create-shape-popup-menu view)))))
    (when (selection scene)
      (push (menu-item-misc-entry "Inspect Selection" (inspect (selection scene)))
            items))
    ;;; edit circle shape
    (menu-item-if-selection-type 'circle-shape
                                 (menu-item-submenu-entry
                                  (format nil "Edit ~a..." (name shape))
                                  (edit-circle-shape-dialog view)))
    ;;; edit sine-curve shape
    (menu-item-if-selection-type 'sine-curve-shape
                                 (menu-item-submenu-entry
                                  (format nil "Edit ~a..." (name shape))
                                  (edit-sine-curve-shape-dialog view)))
    ;;; edit height-field shape
    (menu-item-if-selection-type 'superquadric
                                 (menu-item-submenu-entry
                                  (format nil "Edit ~a..." (name shape))
                                  (edit-superquadric-dialog view)))
    ;;; particle system from point-generator-mixin
    (menu-item-if-selection-type 'point-generator-mixin
                                 (menu-item-submenu-entry
                                  (format nil "Create PARTICLE-SYSTEM from ~a..." (name shape))
                                  (make-particle-system-dialog view)))
    ;;; dynamic particle system from point-generator-mixin
    (menu-item-if-selection-type 'point-generator-mixin
                                 (menu-item-submenu-entry
                                  (format nil "Create Dynamic PARTICLE-SYSTEM from ~a..." (name shape))
                                  (make-dynamic-particle-system-dialog view)))
    ;;; add field to particle system
    (menu-item-if-selection-type 'particle-system
                                 (menu-item-submenu-entry
                                  (format nil "Add Force Field to PARTICLE-SYSTEM ~a..." (name shape))
                                  (make-add-field-popup-menu view)))
    ;;; sweep-mesh-group from curve-generator-mixin
    (menu-item-if-selection-type 'curve-generator-mixin
                                 (menu-item-submenu-entry
                                  (format nil "Create SWEEP-MESH-GROUP from ~a..." (name shape))
                                  (make-sweep-mesh-group-dialog view)))
    ;;; set uv-mesh point colors
    (menu-item-if-selection-type 'uv-mesh
                                 (menu-item-submenu-entry
                                  (format nil "Set UV Point Colors for ~a..." (name shape))
                                  (make-uv-point-colors-dialog view)))
    ;;; set sweep-mesh-group point colors
    (menu-item-if-selection-type 'sweep-mesh-group
                                 (menu-item-submenu-entry
                                  (format nil "Set UV Point Colors for ~a..." (name shape))
                                  (make-uv-point-colors-dialog view)))
    ;;; menu setup
    (setf (ui-items menu) items)
    (update-layout menu)))

(defun make-create-shape-popup-menu (view)
  (let* ((items (list (menu-item-submenu-entry "Heightfield..." (make-heightfield-dialog view))
                      (menu-item-submenu-entry "Grid..." (make-grid-dialog view))
                      (menu-item-submenu-entry "Superquadric..." (create-superquadric-dialog view))
                      (menu-item-submenu-entry "Icosahedron..." (make-icosahedron-dialog view))
                      (menu-item-submenu-entry "Octahedron..." (make-octahedron-dialog view))
                      (menu-item-submenu-entry "Sine Curve..." (create-sine-curve-shape-dialog view))
                      (menu-item-submenu-entry "Circle..." (create-circle-shape-dialog view))))
         (menu (make-instance 'ui-popup-menu :ui-items items)))
    (update-layout menu)))

(defun make-dialog-item (name default &optional (input-fn #'get-float-input))
  (make-instance 'ui-text-item
                 :title name
                 :ui-value default
                 :action-fn #'(lambda (item)
                                (setf (ui-value item)
                                      (funcall input-fn (strcat "Enter " name) (ui-value item)))
                                (#/setNeedsDisplay: item t))))

(defmacro make-dialog (name action-expr &rest item-decls)
  `(defun ,name (view)
     (let* ((curr-selection (first (selection (scene view))))
            ,@(mapcar #'(lambda (decl)
                          (list (first decl) `(make-dialog-item ,(second decl) ,@(cddr decl))))
                      item-decls)
            (dialog (make-instance 'ui-dialog-box
                                   :ui-items ,(cons 'list (mapcar #'first item-decls))
                                   :action-fn #'(lambda () ,action-expr))))
       (update-layout dialog view))))

(make-dialog make-icosahedron-dialog
             (add-shape (scene view) (make-icosahedron (ui-value radius-item)))
             (radius-item "Radius" 1.0))

(make-dialog make-octahedron-dialog
             (add-shape (scene view) (make-octahedron (ui-value radius-item)))
             (radius-item "Radius" 1.0))

;; (make-dialog create-circle-dialog
;;              (add-shape (scene view) (make-circle-shape (ui-value diam-item)
;;                                                         (ui-value num-points-item)))
;;              (diam-item "Diameter" 3.0)
;;              (num-points-item "Num Points" 32 #'get-integer-input))

(defun find-class-slot (class-name slot-name)
  (find slot-name (class-slots (find-class class-name)) :key #'slot-definition-name))

(defmacro make-create-shape-dialog (class-name slot-decls)
  `(make-dialog ,(mashup-symbol 'create- class-name '-dialog)
                (add-shape (scene view)
                           (make-instance ',class-name
                                          ,@(flatten-list-1 (mapcar #'(lambda (slot-name)
                                                                        `(,(make-keyword slot-name) (ui-value ,(mashup-symbol slot-name '-item))))
                                                                    (mapcar #'first slot-decls)))))
                ,@(mapcar #'(lambda (slot-decl)
                              `(,(mashup-symbol (first slot-decl) '-item)
                                ,(format nil "~a" (first slot-decl))
                                (slot-definition-initform (find-class-slot ',class-name ',(first slot-decl)))
                                ,(cond ((eql 'float (second slot-decl)) '#'get-float-input)
                                       ((eql 'integer (second slot-decl)) '#'get-integer-input))))
                          slot-decls)))

;(make-create-shape-dialog circle-shape ((diameter float) (num-points integer)))

(defmacro make-edit-shape-dialog (class-name slot-decls)
  `(make-dialog ,(mashup-symbol 'edit- class-name '-dialog)
                (let* ((scene (scene view))
                       (shape (first (selection scene))))
                  ,@(mapcar #'(lambda (slot-name)
                                `(setf (,slot-name shape) (ui-value ,(mashup-symbol slot-name '-item))))
                            (mapcar #'first slot-decls)))
                ,@(mapcar #'(lambda (slot-decl)
                              `(,(mashup-symbol (first slot-decl) '-item)
                                ,(format nil "~a" (first slot-decl))
                                (slot-value curr-selection ',(first slot-decl))
                                ,(cond ((eql 'float (second slot-decl)) '#'get-float-input)
                                       ((eql 'integer (second slot-decl)) '#'get-integer-input))))
                          slot-decls)))

;(make-edit-shape-dialog circle-shape ((diameter float) (num-points integer)))

(defmacro make-create-and-edit-shape-dialogs (class-name slot-decls)
  `(progn
     (make-create-shape-dialog ,class-name ,slot-decls)
     (make-edit-shape-dialog ,class-name ,slot-decls)))

(make-create-and-edit-shape-dialogs circle-shape ((diameter float) (num-points integer)))
(make-create-and-edit-shape-dialogs sine-curve-shape ((y-scale float)
                                                      (x-scale float)
                                                      (frequency float)
                                                      (period float)
                                                      (num-points integer)))
(make-create-and-edit-shape-dialogs superquadric ((e2 float)
                                                  (e1 float)
                                                  (radius float)
                                                  (v-dim integer)
                                                  (u-dim integer)))

#|
(mapcar #'slot-definition-initform (class-slots (find-class 'circle-shape)))
|#

;; (make-dialog edit-circle-dialog
;;              (let* ((scene (scene view))
;;                     (shape (first (selection scene))))
;;                (setf (diameter shape) (ui-value diam-item))
;;                (setf (num-points shape) (ui-value num-points-item)))
;;              (diam-item "Diameter" 3.0)
;;              (num-points-item "Num Points" 32 #'get-integer-input))

(make-dialog make-grid-dialog
             (add-shape (scene view) (let ((n (ui-value num-points-item))
                                           (s (ui-value size-item)))
                                       (make-grid-uv-mesh n n (p! (- s) 0 (- s)) (p! s 0 s))))
             (size-item "Size" 4.0)
             (num-points-item "Num Points" 16 #'get-integer-input))

(defun height-fn-1 (x z)
  (* 4 (noise (p! x 0 z))))
(defun height-fn-2 (x z)
  (* 4 (turbulence (p! x 0 z) 3)))
(defun height-fn-3 (x z)
  (let* ((p (p! x 0 z))
         (mag (p-mag (p-scale p .25))))
    (if (= mag 0.0)
        10.0
        (/ 1.0 mag))))
(defun height-fn-4 (x z)
  (let* ((p (p! x 0 z))
         (mag (max 0.001 (p-mag (p-scale p 4)))))
    (* 3 (/ (sin mag) mag))))

(make-dialog make-heightfield-dialog
             (add-shape (scene view) (let ((n (ui-value num-points-item))
                                           (s (ui-value size-item))
                                           (f (ui-value func-item)))
                                       (make-height-field n n (p! (- s) 0 (- s)) (p! s 0 s)
                                                          (case f
                                                            (1 #'height-fn-1)
                                                            (2 #'height-fn-2)
                                                            (3 #'height-fn-3)
                                                            (4 #'height-fn-4)
                                                            (otherwise #'height-fn-4)))))
             (func-item "Function (1-4)" 4 #'get-integer-input)
             (size-item "Size" 4.0)
             (num-points-item "Num Points" 16 #'get-integer-input))

(make-dialog make-particle-system-dialog
             (let* ((scene (scene view))
                    (shape (first (selection scene)))
                    (n (ui-value number-item))
                    (s (ui-value speed-item))
                    (p-sys (make-particle-system shape
                                                 (p! s s s) n 4 'particle
                                                 :update-angle (range-float (/ pi 16) (/ pi 32)))))
               (add-shape scene p-sys)
               (add-animator scene p-sys))
             (number-item "Number" 1 #'get-integer-input)
             (speed-item "Speed" 0.2))

(make-dialog make-dynamic-particle-system-dialog
             (let* ((scene (scene view))
                    (n (ui-value number-item))
                    (s (ui-value speed-item))
                    (points (point-generator-points (first (selection scene))))
                    (dirs (point-generator-directions (first (selection scene)))) ;(point-generator-radial-directions shape))
                    (p-sys (make-particle-system-aux points dirs
                                                     (p! s s s) n 4 'dynamic-particle
                                                     :do-collisions? nil
                                                     :update-angle (range-float 0 0)
                                                     :spawn-angle (range-float 0 (/ pi 16))
                                                     :life-span (ui-value lifespan-item))))
               (add-shape scene p-sys)
               (add-animator scene p-sys))
             (number-item "Number" 2 #'get-integer-input)
             (speed-item "Speed" 0.2)
             (lifespan-item "Lifespan" 10 #'get-integer-input))

(make-dialog make-sweep-mesh-group-dialog
             (let* ((scene (scene view))
                    (r (ui-value radius-item))
                    (n (ui-value num-points-item))
                    (taper (ui-value taper-item))
                    (twist (ui-value twist-item)))
               (add-shape scene (make-sweep-mesh-group (make-circle-shape r n)
                                                       (first (selection scene))
                                                       :taper taper :twist twist)))
             (radius-item "Radius" 0.2)
             (num-points-item "Num Points" 6 #'get-integer-input)
             (taper-item "Taper" 0.0)
             (twist-item "Twist" 0.0))

(make-dialog make-uv-point-colors-dialog
             (let* ((scene (scene view))
                    (vf (ui-value v-factor-item)))
               (set-point-colors-by-uv (first (selection scene))
                                       #'(lambda (u v)
                                           (declare (ignore u))
                                           (c-rainbow (mod (* v vf) 1)))))
             (v-factor-item "V Factor" 1.0))


(defun make-add-field-popup-menu (view)
  (let* ((items (list (menu-item-submenu-entry "Gravity Force Field..."
                                               (make-gravity-force-field-dialog view))
                      (menu-item-submenu-entry "Attractor Force Field..."
                                               (make-attractor-force-field-dialog view))
                      (menu-item-submenu-entry "Noise Force Field..."
                                               (make-noise-force-field-dialog view))))
         (menu (make-instance 'ui-popup-menu :ui-items items)))
    (update-layout menu)))

(make-dialog make-gravity-force-field-dialog
             (add-force-field (first (selection (scene view)))
                              (make-instance 'constant-force-field
                                             :force-vector (p! 0 (ui-value force-item) 0)))
             (force-item "Force" -0.05))

(make-dialog make-attractor-force-field-dialog
             (add-force-field (first (selection (scene view)))
                              (make-instance 'attractor-force-field
                                             :magnitude (ui-value magnitude-item)))
             (magnitude-item "Magnitude" 0.05))

(make-dialog make-noise-force-field-dialog
             (add-force-field (first (selection (scene view)))
                              (make-instance 'noise-force-field
                                             :noise-frequency (ui-value frequency-item)
                                             :noise-amplitude (ui-value amplitide-item)))
             (frequency-item "Frequency" 1.0)
             (amplitide-item "Amplitude" 1.0))


(defmethod initialize-instance :after ((view scene-view) &rest initargs)
  (declare (ignore initargs))
  (#/setPixelFormat: view (new-pixel-format ;#$NSOpenGLPFAOpenGLProfile 
                                            ;#$NSOpenGLProfileVersion3_2Core
                                            ;#$NSOpenGLPFADoubleBuffer
                                            #$NSOpenGLPFAColorSize 32
                                            #$NSOpenGLPFADepthSize 24))
  (#/setWantsLayer: view t)
  (#/setBorderWidth: (#/layer view) 1)
;  (#/setCornerRadius: (#/layer view) 10)
  (init-view-camera))

;;; display the view
(objc:defmethod (#/drawRect: :void) ((view scene-view) (rect :<NSR>ect))
  (update-status-bar view)

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

  (when (scene view)
    (draw (scene view)))

  (#_glDisable #$GL_LIGHTING)
  (when *display-ground-plane?*
    (draw-ground-plane 10.0 10))
  (when *display-axes?*
    (draw-axes 3.0))
  (gl-set-fg-color)
  (#_glFlush))

(defmethod update-status-bar ((self scene-view) &optional (p nil))
  (let ((sb (status-bar self))
        (scene (scene self)))
    (setf (info-1 sb) "Mouse Drag: orbit, [option] left/right/up/down, [command] in/out")
    (setf (info-2 sb) (if p
                          (format nil "Cursor: (~a, ~a)"
                                  (round (ns:ns-point-x p)) (round (ns:ns-point-y p)))
                          ""))
    (setf (info-3 sb) (format nil "Current Frame: ~a" (current-frame scene)))
    (setf (info-4 sb) (format nil "Num Shapes: ~a" (length (shapes scene))))
    (setf (info-5 sb) (format nil "Num Animators: ~a" (length (animators scene))))
    (#/setNeedsDisplay: sb t)))

;;; respond to first click in window
(objc:defmethod (#/acceptsFirstMouse: :<BOOL>) ((self scene-view) event)
  (declare (ignore event))
  t)

(objc:defmethod (#/mouseMoved: :void) ((self scene-view) event)
  (let ((p (#/locationInWindow event)))
    (update-status-bar self (#/convertPoint:fromView: self p nil))
    (when (popup-menu self)
      (unhighlight-items (popup-menu self))
      ;; highlight menu item under mouse
      (let ((widget (#/hitTest: self p)))
;;;        (when (typep widget 'ui-menu-item)
        (when (subtypep (type-of widget) 'ui-button-item)
          (#/setBorderWidth: (#/layer widget) 3)))
      (#/setNeedsDisplay: self t))))

(objc:defmethod (#/mouseDown: :void) ((self scene-view) event)
  (if (not (is-active-view? self))
      (ui-switch-active-view self (schematic-view self))
      (let ( ;(flags (#/modifierFlags event))
            (p (#/locationInWindow event)))
;    (format t "~a, ~a, ~a~%" p flags #$NSControlKeyMask)
    ;; (when (and (= flags 262145) ;NSControlKeyMask
    ;;            (popup-menu self)
    ;;            (not (menu-is-visible? (popup-menu self))))
    ;;   (popup (popup-menu self) self))
        (let ((widget (#/hitTest: self p)))
          ;; (when (typep widget 'ui-menu-item)
          (when (subtypep (type-of widget) 'ui-button-item)
            (when (action-fn widget)
              (funcall (action-fn widget) widget))))))
  (redraw))

;;; accept key events
(objc:defmethod (#/acceptsFirstResponder :<BOOL>) ((self scene-view))
   t)

(objc:defmethod (#/mouseDragged: :void) ((self scene-view) event)
  (let ((flags (#/modifierFlags event))
        (p (#/locationInWindow event))
        (dx (coerce (#/deltaX event) 'single-float))
        (dy (coerce (#/deltaY event) 'single-float)))
    ;;    (format t "~a, ~a~%" flags #$NSAlternateKeyMask)
    (update-status-bar self (#/convertPoint:fromView: self p nil))
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

(defun print-scene-view-help ()
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
n: clear scene~%~
space: update scene (hold down for animation) ~%~
delete: delete selected items ~%~
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
      (#\h (print-scene-view-help))
      (#\? (print-scene-view-help))
      (#\a (when scene (init-scene scene)))
      (#\n (dolist (v *scene-views*) (clear-scene (scene v))))
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
      (#\rubout (dolist (v *scene-views*) (remove-current-selection (scene v))))
      (#\tab (if (null (popup-menu self))
                 (menu-popup self (make-popup-menu self))
                 (menu-popdown self)))
      (#\return (let ((popup (popup-menu self)))
                  (when (and popup (subtypep (type-of popup) 'ui-dialog-box))
                    (when (action-fn popup)
                      (funcall (action-fn popup)))
                    (menu-popdown self))))
      ))
  (redraw))

;; (defmethod menu-popup ((self scene-view) (menu ui-popup-menu))
;;   (setf (popup-menu self) menu)
;;   (popup (popup-menu self) self))

;; (defmethod menu-popdown ((self scene-view))
;;   (popdown (popup-menu self))
;;   (setf (popup-menu self) nil))

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
           (sb (make-instance 'ui-status-bar))
           (sv (make-instance 'ui-schematic-view :scene scene :status-bar sb))
           (v (make-instance 'scene-view :scene scene :status-bar sb)))
;      (#/setContentView: w v)

      (#/addSubview: (#/contentView w) sb)
      (#/setFrameOrigin: sb (ns:make-ns-point 0 0))
      (#/setFrameSize: sb (ns:make-ns-point 1400 20))

      (#/addSubview: (#/contentView w) sv)
      (#/setFrameOrigin: sv (ns:make-ns-point *window-x-size* 20))
      (#/setFrameSize: sv (ns:make-ns-point (- 1400 *window-x-size*) *window-y-size*))

      (#/addSubview: (#/contentView w) v)
      (#/setFrameOrigin: v (ns:make-ns-point 0 20))
      (#/setFrameSize: v (ns:make-ns-point *window-x-size* *window-y-size*))
      (push v *scene-views*)

      (setf (schematic-view v) sv)
      (setf (scene-view sv) v)
      
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

;(defun pick-in-window ()
;  (#_gluUnProject 
