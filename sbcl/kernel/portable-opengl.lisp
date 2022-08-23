(in-package #:kons-9)

;;;; macros ==============================================================

(defmacro with-gl-enable (flag &body body)
  `(progn
     (gl:enable ,flag)
     (let ((result (progn ,@body)))
       (gl:disable ,flag)
       result)))

(defmacro with-gl-disable (flag &body body)
  `(progn
     (gl:disable ,flag)
     (let ((result (progn ,@body)))
       (gl:enable ,flag)
       result)))

;;;; graphics ===========================================================

(defun draw-world-axes (size)
  (gl:line-width 3.0)
  (gl:begin :lines)
  (gl:color 1.0 0.0 0.0)
  (gl:vertex 0.0  0.0  0.0)
  (gl:vertex size 0.0  0.0)
  (gl:color 0.0 1.0 0.0)
  (gl:vertex 0.0 0.0  0.0 )
  (gl:vertex 0.0 size 0.0 )
  (gl:color 0.0 0.0 1.0)
  (gl:vertex 0.0  0.0  0.0)
  (gl:vertex 0.0  0.0 size)
  (gl:end))

(defun draw-ground-plane (size segments)
  (gl:color 0.8 0.8 0.8)
  (gl:line-width 1.0)
  (gl:begin :lines)
  (dotimes (i (+ segments 1))
    (let* ((f (/ i segments))
           (coord (lerp f (- size) size)))
      (gl:vertex coord 0.0 (- size))
      (gl:vertex coord 0.0    size)
      (gl:vertex (- size) 0.0 coord)
      (gl:vertex    size  0.0 coord)))
  (gl:end))

(defparameter *viewport-aspect-ratio* (/ 16.0 9.0))

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
  (gl:color (c-red *fg-color*) (c-green *fg-color*) (c-blue *fg-color*)))
  
(defun gl-set-sel-color ()
  (gl:color (c-red *sel-color*) (c-green *sel-color*) (c-blue *sel-color*)))

(defun gl-enable-light (light-id dir &optional (color *light-color*))
  (gl:enable light-id)

  ;; (with-c-array-4 (vector (x dir) (y dir) (z dir) 0.0)
  ;;   (gl:light light-id :position vp))
  (gl:light light-id :position (vector (x dir) (y dir) (z dir) 0.0))

  ;; (with-c-array-4 (vector 0.25 0.25 0.25 1.0)
  ;;   (gl:light light-id :ambient vp))
  (gl:light light-id :ambient (vector 0.25 0.25 0.25 1.0))

  ;; (with-c-array-4 (vector (c-red color) (c-green color) (c-blue color) 1.0)
  ;;   (gl:light light-id :diffuse vp))
  (gl:light light-id :diffuse (vector (c-red color) (c-green color) (c-blue color) 1.0))

  ;; (with-c-array-4 (vector (c-red color) (c-green color) (c-blue color) 1.0)
  ;;   (gl:light light-id :specular vp))
  (gl:light light-id :specular (vector (c-red color) (c-green color) (c-blue color) 1.0))
  )

(defun gl-disable-light (light-id)
  (gl:disable light-id))

(defun gl-set-material (&optional (diff *shading-color*) (spec (c! 0 0 0)) (shine 0.0))
  ;; (with-c-array-4 (vector (c-red diff) (c-green diff) (c-blue diff) 1.0)
  ;;   (gl:material :front-and-back :diffuse vp))
  (gl:material :front-and-back :diffuse (vector (c-red diff) (c-green diff) (c-blue diff) 1.0))
  ;; (with-c-array-4 (vector (c-red spec) (c-green spec) (c-blue spec) 1.0)
  ;;   (gl:material :front-and-back :specular vp))
  (gl:material :front-and-back :specular (vector (c-red spec) (c-green spec) (c-blue spec) 1.0))
  ;; (with-c-array-1 (vector shine)
  ;;   (gl:material :front-and-back :shininess vp))
  (gl:material :front-and-back :shininess shine))
  
;; (defun new-pixel-format (&rest attributes)
;;   ;; take a list of opengl pixel format attributes (enums and other
;;   ;; small ints), make an array (character array?), and create and
;;   ;; return an NSOpenGLPixelFormat
;;   (let* ((attribute-size (ccl::foreign-size #>NSOpenGLPixelFormatAttribute :bytes))
;;          (nattributes (length attributes)))
;;     (ccl::%stack-block ((objc-attributes (* attribute-size (1+ nattributes))))
;;       (loop for i from 0 to nattributes
;;             for attribute in attributes do
;;             (setf (ccl:paref objc-attributes (:* #>NSOpenGLPixelFormatAttribute) i)
;;                   attribute) ; <- autocoerced?
;;             finally (setf
;;                      (ccl:paref objc-attributes
;;                                 (:* #>NSOpenGLPixelFormatAttribute) nattributes) 0))
;;       (make-instance ns:ns-opengl-pixel-format :with-attributes objc-attributes))))

(defun 3d-update-light-settings ()
  (if *do-backface-cull?*
      (progn
        (gl:enable :cull-face)
        (gl:light-model :light-model-two-side :false))
      (progn
        (gl:disable :cull-face)
        (gl:light-model :light-model-two-side :true)))

  (if *do-lighting?*
      (gl:enable :lighting)
      (gl:disable :lighting))
  (let ((mtx (matrix-multiply (make-x-rotation-matrix (- (radians *cam-x-rot*)))
                              (make-y-rotation-matrix (- (radians *cam-y-rot*))))))
    (gl-enable-light :light0 (transform-point (p! 0 0 1) mtx) (c! 0.7 0.7 0.7))))

;; (if *light-0-on?*
  ;;     (gl-enable-light :light0 (p! 0 1 0.8) (c! 0.7 0.7 0.7))
  ;;     (gl-disable-light :light0))
  ;; (if *light-1-on?*
  ;;     (gl-enable-light :light1 (p! 1 -0.5 0) (c! 0.3 0.3 0.3))
  ;;     (gl-disable-light :light1))
  ;; (when (and (not *light-0-on?*) (not *light-1-on?*)) ;use camera light
  ;;   (let ((mtx (matrix-multiply (make-x-rotation-matrix (- (radians *cam-x-rot*)))
  ;;                               (make-y-rotation-matrix (- (radians *cam-y-rot*))))))
  ;;     (gl-enable-light :light0 (transform-point (p! 0 0 1) mtx))))
;;  )



(defun 3d-setup-buffer ()
  (gl:clear-color (c-red *bg-color*) (c-green *bg-color*) (c-blue *bg-color*) 0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:enable :depth-test)
  (gl:cull-face :back))

(defun 3d-setup-projection ()
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 45.0d0 (coerce *viewport-aspect-ratio* 'double-float) 0.01d0 1000.0d0)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
;;  (glu:look-at 4.0d0 3.0d0 5.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 0.0d0)
  (gl:translate *cam-side-dist* *cam-up-dist* *cam-fwd-dist*)
  (gl:rotate *cam-x-rot* 1.0 0.0 0.0)
  (gl:rotate *cam-y-rot* 0.0 1.0 0.0))

(defun 3d-cleanup-render ()
  (gl:disable :lighting))

(defun 3d-flush-render ()
  (gl-set-fg-color)
  (gl:flush))

;;; 3d display =================================================================

(defun 3d-push-matrix (translate rotate scale)
  (gl:push-matrix)
  (gl:translate (x translate) (y translate) (z translate))
  (gl:rotate (x rotate) 1.0 0.0 0.0)
  (gl:rotate (y rotate) 0.0 1.0 0.0)
  (gl:rotate (z rotate) 0.0 0.0 1.0)
  (gl:scale (x scale) (y scale) (z scale)))

(defun 3d-draw-marker (size)
  (gl:color 1.0 1.0 0.0)
  (gl:line-width 5.0)
  (gl:begin :lines)
  (gl:vertex    size  0.0  0.0)
  (gl:vertex (- size) 0.0  0.0)
  (gl:vertex  0.0    size  0.0)
  (gl:vertex  0.0 (- size) 0.0)
  (gl:vertex  0.0  0.0    size )
  (gl:vertex  0.0  0.0 (- size))
  (gl:end))

(defun 3d-draw-axis (size)
  (with-gl-disable :lighting
    (gl:line-width 3.0)
    (gl:begin :lines)
    ;; x axis (red)
    (gl:color 1.0 0.0 0.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex size 0.0 0.0)
    ;; y axis (green)
    (gl:color 0.0 1.0 0.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex 0.0 size 0.0)
    ;; z axis (blue)
    (gl:color 0.0 0.0 1.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex 0.0 0.0 size)
    (gl:end)))

(defun 3d-draw-bounds (lo hi color)
  (with-gl-disable :lighting
    (gl:line-width 3.0)
    (gl:color (c-red color) (c-green color) (c-blue color))
    (gl:begin :lines)
      (when (and lo hi)
        (let ((x0 (x lo))
              (y0 (y lo))
              (z0 (z lo))
              (x1 (x hi))
              (y1 (y hi))
              (z1 (z hi)))
          (gl:vertex x0 y0 z0) (gl:vertex x1 y0 z0)
          (gl:vertex x1 y0 z0) (gl:vertex x1 y0 z1)
          (gl:vertex x1 y0 z1) (gl:vertex x0 y0 z1)
          (gl:vertex x0 y0 z1) (gl:vertex x0 y0 z0)

          (gl:vertex x0 y1 z0) (gl:vertex x1 y1 z0)
          (gl:vertex x1 y1 z0) (gl:vertex x1 y1 z1)
          (gl:vertex x1 y1 z1) (gl:vertex x0 y1 z1)
          (gl:vertex x0 y1 z1) (gl:vertex x0 y1 z0)

          (gl:vertex x0 y0 z0) (gl:vertex x0 y1 z0)
          (gl:vertex x1 y0 z0) (gl:vertex x1 y1 z0)
          (gl:vertex x1 y0 z1) (gl:vertex x1 y1 z1)
          (gl:vertex x0 y0 z1) (gl:vertex x0 y1 z1)

          (gl:end)))))

(defun 3d-pop-matrix ()
  (gl:pop-matrix))

(defun 3d-draw-curve (points is-closed? &optional (line-width 3.0))
  (with-gl-disable :lighting
    (gl-set-fg-color)
    (gl:line-width line-width)
    (if is-closed?
        (gl:begin :line-loop)
        (gl:begin :line-strip))
    (doarray (i p points)
      (gl:vertex (x p) (y p) (z p)))
    (gl:end)))

(defun 3d-draw-points (points)
  (with-gl-disable :lighting
    (gl-set-fg-color)
    (gl:point-size 9.0)
    (gl:begin :points)
    (doarray (i p points)
      (gl:vertex (x p) (y p) (z p)))
    (gl:end)))

(defun 3d-draw-lines (points)
  (with-gl-disable :lighting
    (gl-set-fg-color)
    (gl:line-width 3.0)
    (gl:begin :lines)
    (dolist (p points)
      (gl:vertex (x p) (y p) (z p)))
    (gl:end)))

(defun 3d-setup-lighting ()
  (if *do-lighting?*
      (gl:enable :lighting)
      (gl:disable :lighting)))

(defun 3d-draw-filled-polygons (points faces face-normals point-normals point-colors)
  (if *do-smooth-shading?*
      (gl:shade-model :smooth)
      (gl:shade-model :flat))
  (gl:polygon-mode :front-and-back :fill)
  (with-gl-enable :rescale-normal
    (with-gl-enable :polygon-offset-fill
      (gl:polygon-offset 1.0 1.0)
      (3d-draw-filled-polygons-aux points faces face-normals point-normals point-colors))))

(defmethod 3d-draw-filled-polygons-aux (points faces face-normals point-normals point-colors)
  (gl-set-material *shading-color*)
  (with-gl-enable :color-material
    (gl:color-material :front-and-back :diffuse)
    (dotimes (f (length faces))
      (gl:begin :polygon)
      (when (not *do-smooth-shading?*)
        (let ((n (aref face-normals f)))
          (gl:normal (x n) (y n) (z n))))
      (dolist (pref (aref faces f))
        (if point-colors
            (let ((c (aref point-colors pref)))
              (gl:color (c-red c) (c-green c) (c-blue c)))
            (gl:color (c-red *shading-color*) (c-green *shading-color*) (c-blue *shading-color*))) ;inefficient...
        (when *do-smooth-shading?*
          (let ((n (aref point-normals pref)))
            (gl:normal (x n) (y n) (z n))))
        (let ((p (aref points pref)))
          (gl:vertex (x p) (y p) (z p))))
      (gl:end))))

(defun 3d-draw-wireframe-polygons (points faces &key (closed? t))
  (gl:polygon-mode :front-and-back :line)
  (with-gl-disable :lighting
    (gl-set-fg-color)
    (gl:line-width 1.0)
    (dotimes (f (length faces))
      (if closed?
          (gl:begin :polygon)
          (gl:begin :line-strip))
      (dolist (pref (aref faces f))
        (let ((p (aref points pref)))
          (gl:vertex (x p) (y p) (z p))))
      (gl:end))))
