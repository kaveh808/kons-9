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

;;;; macros ==============================================================

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

;;;; graphics ===========================================================

(defun draw-world-axes (size)
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

(defparameter *shading-color* (c! 1 1 1))
(defparameter *light-color* (c! 2 2 2))
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

(defun 3d-update-light-settings ()
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



(defun 3d-setup-buffer ()
  (#_glClearColor (c-red *bg-color*) (c-green *bg-color*) (c-blue *bg-color*) 0.0)
  (#_glClear (logior #$GL_COLOR_BUFFER_BIT #$GL_DEPTH_BUFFER_BIT))
  (#_glEnable #$GL_DEPTH_TEST)
  (#_glCullFace #$GL_BACK))

(defun 3d-setup-projection ()
  (#_glMatrixMode #$GL_PROJECTION)
  (#_glLoadIdentity)
  (#_gluPerspective 45.0d0 (/ 16.0d0 9.0d0) 0.01d0 1000.0d0)
  (#_glMatrixMode #$GL_MODELVIEW)
  (#_glLoadIdentity)
;;  (#_gluLookAt 4.0d0 3.0d0 5.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 0.0d0)
  (#_glTranslatef *cam-side-dist* *cam-up-dist* *cam-fwd-dist*)
  (#_glRotatef *cam-x-rot* 1.0 0.0 0.0)
  (#_glRotatef *cam-y-rot* 0.0 1.0 0.0))

(defun 3d-cleanup-render ()
  (#_glDisable #$GL_LIGHTING))

(defun 3d-flush-render ()
  (gl-set-fg-color)
  (#_glFlush))

;;; 3d display =================================================================

(defun 3d-push-matrix (translate rotate scale)
  (#_glPushMatrix)
  (#_glTranslatef (x translate) (y translate) (z translate))
  (#_glRotatef (x rotate) 1.0 0.0 0.0)
  (#_glRotatef (y rotate) 0.0 1.0 0.0)
  (#_glRotatef (z rotate) 0.0 0.0 1.0)
  (#_glScalef (x scale) (y scale) (z scale)))

(defun 3d-draw-marker (size)
  (#_glColor3f 1.0 1.0 0.0)
  (#_glLineWidth 5.0)
  (#_glBegin #$GL_LINES)
  (#_glVertex3f    size  0.0  0.0)
  (#_glVertex3f (- size) 0.0  0.0)
  (#_glVertex3f  0.0    size  0.0)
  (#_glVertex3f  0.0 (- size) 0.0)
  (#_glVertex3f  0.0  0.0    size )
  (#_glVertex3f  0.0  0.0 (- size))
  (#_glEnd))

(defun 3d-draw-axis (size)
  (with-gl-disable #$GL_LIGHTING
    (#_glLineWidth 3.0)
    (#_glBegin #$GL_LINES)
    ;; x axis (red)
    (#_glColor3f 1.0 0.0 0.0)
    (#_glVertex3f 0.0 0.0 0.0)
    (#_glVertex3f size 0.0 0.0)
    ;; y axis (green)
    (#_glColor3f 0.0 1.0 0.0)
    (#_glVertex3f 0.0 0.0 0.0)
    (#_glVertex3f 0.0 size 0.0)
    ;; z axis (blue)
    (#_glColor3f 0.0 0.0 1.0)
    (#_glVertex3f 0.0 0.0 0.0)
    (#_glVertex3f 0.0 0.0 size)
    (#_glEnd)))

(defun 3d-draw-bounds (lo hi color)
  (with-gl-disable #$GL_LIGHTING
    (#_glLineWidth 3.0)
    (#_glColor3f (c-red color) (c-green color) (c-blue color))
    (#_glBegin #$GL_LINES)
      (when (and lo hi)
        (let ((x0 (x lo))
              (y0 (y lo))
              (z0 (z lo))
              (x1 (x hi))
              (y1 (y hi))
              (z1 (z hi)))
          (#_glVertex3f x0 y0 z0) (#_glVertex3f x1 y0 z0)
          (#_glVertex3f x1 y0 z0) (#_glVertex3f x1 y0 z1)
          (#_glVertex3f x1 y0 z1) (#_glVertex3f x0 y0 z1)
          (#_glVertex3f x0 y0 z1) (#_glVertex3f x0 y0 z0)

          (#_glVertex3f x0 y1 z0) (#_glVertex3f x1 y1 z0)
          (#_glVertex3f x1 y1 z0) (#_glVertex3f x1 y1 z1)
          (#_glVertex3f x1 y1 z1) (#_glVertex3f x0 y1 z1)
          (#_glVertex3f x0 y1 z1) (#_glVertex3f x0 y1 z0)

          (#_glVertex3f x0 y0 z0) (#_glVertex3f x0 y1 z0)
          (#_glVertex3f x1 y0 z0) (#_glVertex3f x1 y1 z0)
          (#_glVertex3f x1 y0 z1) (#_glVertex3f x1 y1 z1)
          (#_glVertex3f x0 y0 z1) (#_glVertex3f x0 y1 z1)

          (#_glEnd)))))

(defun 3d-pop-matrix ()
  (#_glPopMatrix))

(defun 3d-draw-curve (points is-closed?)
  (with-gl-disable #$GL_LIGHTING
    (gl-set-fg-color)
    (#_glLineWidth 3.0)
    (if is-closed?
        (#_glBegin #$GL_LINE_LOOP)
        (#_glBegin #$GL_LINE_STRIP))
    (doarray (i p points)
      (#_glVertex3f (x p) (y p) (z p)))
    (#_glEnd)))

(defun 3d-draw-points (points)
  (with-gl-disable #$GL_LIGHTING
    (gl-set-fg-color)
    (#_glPointSize 9.0)
    (#_glBegin #$GL_POINTS)
    (doarray (i p points)
      (#_glVertex3f (x p) (y p) (z p)))
    (#_glEnd)))

(defun 3d-draw-lines (points)
  (with-gl-disable #$GL_LIGHTING
    (gl-set-fg-color)
    (#_glLineWidth 3.0)
    (#_glBegin #$GL_LINES)
    (dolist (p points)
      (#_glVertex3f (x p) (y p) (z p)))
    (#_glEnd)))

(defun 3d-setup-lighting ()
  (if *do-lighting?*
      (#_glEnable #$GL_LIGHTING)
      (#_glDisable #$GL_LIGHTING)))

(defun 3d-draw-filled-polygons (points faces face-normals point-normals point-colors)
  (if *do-smooth-shading?*
      (#_glShadeModel #$GL_SMOOTH)
      (#_glShadeModel #$GL_FLAT))
  (#_glPolygonMode #$GL_FRONT_AND_BACK #$GL_FILL)
  (with-gl-enable #$GL_POLYGON_OFFSET_FILL
    (#_glPolygonOffset 1.0 1.0)
    (3d-draw-filled-polygons-aux points faces face-normals point-normals point-colors)))

(defmethod 3d-draw-filled-polygons-aux (points faces face-normals point-normals point-colors)
  (gl-set-material *shading-color*)
  (with-gl-enable #$GL_COLOR_MATERIAL
    (#_glColorMaterial #$GL_FRONT_AND_BACK #$GL_DIFFUSE)
    (dotimes (f (length faces))
      (#_glBegin #$GL_POLYGON)
      (when (not *do-smooth-shading?*)
        (let ((n (aref face-normals f)))
          (#_glNormal3f (x n) (y n) (z n))))
      (dolist (pref (aref faces f))
        (if point-colors
            (let ((c (aref point-colors pref)))
              (#_glColor3f (c-red c) (c-green c) (c-blue c)))
            (#_glColor3f (c-red *shading-color*) (c-green *shading-color*) (c-blue *shading-color*))) ;inefficient...
        (when *do-smooth-shading?*
          (let ((n (aref point-normals pref)))
            (#_glNormal3f (x n) (y n) (z n))))
        (let ((p (aref points pref)))
          (#_glVertex3f (x p) (y p) (z p))))
      (#_glEnd))))

(defun 3d-draw-wireframe-polygons (points faces &key (closed? t))
  (#_glPolygonMode #$GL_FRONT_AND_BACK #$GL_LINE)
  (with-gl-disable #$GL_LIGHTING
    (gl-set-fg-color)
    (#_glLineWidth 1.0)
    (dotimes (f (length faces))
      (if closed?
          (#_glBegin #$GL_POLYGON)
          (#_glBegin #$GL_LINE_STRIP))
      (dolist (pref (aref faces f))
        (let ((p (aref points pref)))
          (#_glVertex3f (x p) (y p) (z p))))
      (#_glEnd))))
