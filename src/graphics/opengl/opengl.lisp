(in-package #:kons-9)

;;;; drawing-settings ==========================================================

(defclass drawing-settings ()
  ((monitor-scale :accessor monitor-scale :initarg :monitor-scale :initform 1.0)
   (default-font :accessor default-font :initarg :default-font :initform (asdf:system-relative-pathname "kons-9" "data/font/DejaVuSansMono.ttf"))
   (point-size :accessor point-size :initarg :point-size :initform 3.0)
   (line-thickness :accessor line-thickness :initarg :line-thickness :initform 1.0)
   (fg-color :accessor fg-color :initarg :fg-color :initform (c! 0 0 0))
   (bg-color :accessor bg-color :initarg :bg-color :initform (c! 1 1 1))
   (sel-color :accessor sel-color :initarg :sel-color :initform (c! 1 0 0))
   (shading-color :accessor shading-color :initarg :shading-color :initform (c! 1 1 1))
   (light-color :accessor light-color :initarg :light-color :initform (c! 2 2 2))
   (axes-size :accessor axes-size :initarg :axes-size :initform 3.0)
   (axes-thickness :accessor axes-thickness :initarg :axes-thickness :initform 3.0)
   (ground-plane-size :accessor ground-plane-size :initarg :ground-plane-size :initform 10.0)
   (ground-plane-segments :accessor ground-plane-segments :initarg :ground-plane-segments :initform 10)
   (ground-plane-thickness :accessor ground-plane-thickness :initarg :ground-plane-thickness :initform 1.0)
   (ground-plane-color :accessor ground-plane-color :initarg :ground-plane-color :initform (c! .8 .8 .8))
   (secondary-line-thickness :accessor secondary-line-thickness :initarg :secondary-line-thickness :initform 1.0)))

(defparameter *drawing-settings* (make-instance 'drawing-settings))

(defun set-lines-thin ()
  (setf (point-size *drawing-settings*) (* 3.0 (monitor-scale *drawing-settings*)))
  (setf (line-thickness *drawing-settings*) (* 1.0 (monitor-scale *drawing-settings*)))
  (setf (axes-thickness *drawing-settings*) (* 3.0 (monitor-scale *drawing-settings*)))
  (setf (secondary-line-thickness *drawing-settings*) (* 0.5 (monitor-scale *drawing-settings*))))

(defun set-lines-thick ()
  (setf (point-size *drawing-settings*) (* 6.0 (monitor-scale *drawing-settings*)))
  (setf (line-thickness *drawing-settings*) (* 2.0 (monitor-scale *drawing-settings*)))
  (setf (axes-thickness *drawing-settings*) (* 5.0 (monitor-scale *drawing-settings*)))
  (setf (secondary-line-thickness *drawing-settings*) (* 1.0 (monitor-scale *drawing-settings*))))

(defun set-theme-bright ()
  (setf (fg-color *drawing-settings*) (c! 0 0 0))
  (setf (bg-color *drawing-settings*) (c! 1 1 1))
  (set-ground-plane-bright))

(defun set-theme-dark ()
  (setf (fg-color *drawing-settings*) (c! 1 1 1))
  (setf (bg-color *drawing-settings*) (c! 0 0 0))
  (set-ground-plane-dark))

(defun set-ground-plane-bright ()
  (setf (ground-plane-color *drawing-settings*) (c! .8 .8 .8)))

(defun set-ground-plane-dark ()
  (setf (ground-plane-color *drawing-settings*) (c! .2 .2 .2)))

(defun set-ground-plane-sparse ()
  (setf (ground-plane-segments *drawing-settings*) 10))

(defun set-ground-plane-dense ()
  (setf (ground-plane-segments *drawing-settings*) 40))

#| test changing appearance

(set-lines-thin)
(set-lines-thick)
(set-theme-bright)
(set-theme-dark)
(set-ground-plane-bright)
(set-ground-plane-dark)
(set-ground-plane-sparse)
(set-ground-plane-dense)

|#

;;;; utils =====================================================================

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

(defun gl-set-color (col)
  (gl:color (c-red col) (c-green col) (c-blue col) (c-alpha col)))

(defun gl-set-fg-color ()
  (gl-set-color (fg-color *drawing-settings*)))
  
(defun gl-set-sel-color ()
  (gl-set-color (sel-color *drawing-settings*)))

;;;; graphics ==================================================================

(defun draw-world-axes ()
  (let ((size (axes-size *drawing-settings*)))
    (gl:line-width (axes-thickness *drawing-settings*))
    (gl:begin :lines)
    (gl:color 1.0 0.0 0.0)
    (gl:vertex 0.0  0.001  0.0)
    (gl:vertex size 0.001 0.0)
    (gl:color 0.0 1.0 0.0)
    (gl:vertex 0.0 0.0 0.0 )
    (gl:vertex 0.0 size 0.0 )
    (gl:color 0.0 0.0 1.0)
    (gl:vertex 0.0  0.001  0.0)
    (gl:vertex 0.0  0.001 size)
    (gl:end)))

(defun draw-ground-plane ()
  (let ((size (ground-plane-size *drawing-settings*))
        (segs (ground-plane-segments *drawing-settings*))
        (col (ground-plane-color *drawing-settings*))
        (thick (ground-plane-thickness *drawing-settings*)))
    (gl-set-color col)
    (gl:line-width thick)
    (gl:begin :lines)
    (dotimes (i (1+ segs))
      (let* ((f (/ i segs))
             (coord (lerp f (- size) size)))
        (gl:vertex coord 0.0 (- size))
        (gl:vertex coord 0.0    size)
        (gl:vertex (- size) 0.0 coord)
        (gl:vertex    size  0.0 coord)))
    (gl:end)))

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

(defun init-view-camera ()
  (setf *cam-x-rot* 15.0)
  (setf *cam-y-rot* -35.0)
  (setf *cam-fwd-dist* -10.0)
  (setf *cam-side-dist* 0.0)
  (setf *cam-up-dist* 0.0)) ;-2.0))

(defun gl-enable-light (light-id dir &optional (color (light-color *drawing-settings*)))
  (gl:enable light-id)
  (gl:light light-id :position (vector (p:x dir) (p:y dir) (p:z dir) 0.0))
  (gl:light light-id :ambient (vector 0.25 0.25 0.25 1.0))
  (gl:light light-id :diffuse color)
  (gl:light light-id :specular color))

(defun gl-disable-light (light-id)
  (gl:disable light-id))

(defun gl-set-material (&optional (diff (shading-color *drawing-settings*)) (spec (c! 0 0 0)) (shine 0.0))
  (gl:material :front-and-back :diffuse diff)
  (gl:material :front-and-back :specular spec)
  (gl:material :front-and-back :shininess shine))
  
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

(defun 3d-setup-buffer ()
  (let ((bg-color (bg-color *drawing-settings*)))
    (gl:clear-color (c-red bg-color) (c-green bg-color) (c-blue bg-color) 0.0)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:enable :depth-test)
    (gl:cull-face :back)
    ;; for sprite transparency
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    ;; (gl:blend-equation-separate :func-add :func-add)
    ;; (gl:blend-func-separate :src-alpha :one-minus-src-alpha :one :one-minus-src-alpha)
    (gl:enable :blend)
    ))

  ;;   (gl:blend-fun

  ;; (#_glBlendEquationSeparate #$GL_FUNC_ADD #$GL_FUNC_ADD)
  ;; (#_glBlendFuncSeparate #$GL_SRC_ALPHA #$GL_ONE_MINUS_SRC_ALPHA #$GL_ONE #$GL_ONE_MINUS_SRC_ALPHA)


(defun 3d-setup-projection ()
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 45.0d0 (coerce *viewport-aspect-ratio* 'double-float) 0.01d0 1000.0d0)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate *cam-side-dist* *cam-up-dist* *cam-fwd-dist*)
  (gl:rotate *cam-x-rot* 1.0 0.0 0.0)
  (gl:rotate *cam-y-rot* 0.0 1.0 0.0))

(defun 3d-cleanup-render ()
  (gl:disable :lighting))

(defun 3d-flush-render ()
  (gl-set-fg-color)
  (gl:flush))

(defun gl-get-float (param)
  (gl:get-float param))

;;; 3d display =================================================================

(defun 3d-translate (p)
  (gl:translate (p:x p) (p:y p) (p:z p)))

(defun 3d-push-matrix (&optional (matrix nil))
  (gl:push-matrix)
  (when matrix
    (gl:mult-matrix (matrix->vector matrix))))

(defun 3d-load-matrix (matrix)
  (gl:load-matrix (matrix->vector matrix)))

(defun 3d-pop-matrix ()
  (gl:pop-matrix))

(defun 3d-draw-marker (size)
  (gl:color 1.0 1.0 0.0)
  (gl:line-width (* 2 (line-thickness *drawing-settings*)))
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
    (gl:line-width (line-thickness *drawing-settings*))
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
    (gl:line-width (* 2 (line-thickness *drawing-settings*))) ;otherwise not visible for cubes etc.
    (gl-set-color color)
    (when (and lo hi)
      (let ((x0 (p:x lo))
            (y0 (p:y lo))
            (z0 (p:z lo))
            (x1 (p:x hi))
            (y1 (p:y hi))
            (z1 (p:z hi)))
        (gl:begin :lines)

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
        
        (gl:end)))
    (gl-set-color (shading-color *drawing-settings*))))    ;reset color

(defun 3d-draw-grid (dims lo hi color)
  (with-gl-disable :lighting
    (gl-set-color color)
    (when (and dims lo hi)
      (let ((nx (aref dims 0))
            (ny (aref dims 1))
            (nz (aref dims 2))
            (x0 (p:x lo))
            (y0 (p:y lo))
            (z0 (p:z lo))
            (x1 (p:x hi))
            (y1 (p:y hi))
            (z1 (p:z hi)))
        (gl:begin :lines)

        (dotimes (i nx)
          (let ((x (lerp (/ i (1- nx)) x0 x1)))
            (dotimes (j ny)
              (let ((y (lerp (/ j (1- ny)) y0 y1)))
                (gl:vertex x y z0) (gl:vertex x y z1)))))
                
        (dotimes (i nx)
          (let ((x (lerp (/ i (1- nx)) x0 x1)))
            (dotimes (k nz)
              (let ((z (lerp (/ k (1- nz)) z0 z1)))
                (gl:vertex x y0 z) (gl:vertex x y1 z)))))
                
        (dotimes (j ny)
          (let ((y (lerp (/ j (1- ny)) y0 y1)))
            (dotimes (k nz)
              (let ((z (lerp (/ k (1- nz)) z0 z1)))
                (gl:vertex x0 y z) (gl:vertex x1 y z)))))
                
        (gl:end)))
    (gl-set-color (shading-color *drawing-settings*))))    ;reset color

(defun 3d-draw-curve (points point-colors is-closed? &optional (line-width (line-thickness *drawing-settings*)))
  (with-gl-disable :lighting
    (gl-set-fg-color)
    (gl:line-width line-width)
    (if is-closed?
        (gl:begin :line-loop)
        (gl:begin :line-strip))
    (do-array (i p points)
      (when point-colors
        (gl-set-color (aref point-colors i)))
      (gl:vertex (p:x p) (p:y p) (p:z p)))
    (gl:end)
    (gl-set-fg-color)))

(defun 3d-draw-filled-curve (points)
  (with-gl-disable :lighting
    (gl:polygon-mode :front-and-back :fill)
    (gl:begin :polygon)
    (do-array (i p points)
      (gl:vertex (p:x p) (p:y p) (p:z p)))
    (gl:end)))

(defun 3d-draw-points (points point-colors &key (highlight? nil))
  (with-gl-disable :lighting
    (if highlight?
        (progn
          (gl-set-sel-color)
          (gl:point-size (* 2 (point-size *drawing-settings*))))
        (progn
          (gl-set-fg-color)
          (gl:point-size (point-size *drawing-settings*))))
    (gl:begin :points)
    (cond (point-colors
           (do-array (i p points)
             (let ((c (aref point-colors i)))
               (gl:color (c-red c) (c-green c) (c-blue c)))
             (gl:vertex (p:x p) (p:y p) (p:z p))))
          (t
           (do-array (i p points)
             (gl:vertex (p:x p) (p:y p) (p:z p)))))
    (gl:end)))

(defun 3d-draw-lines (points &key (highlight? nil))
  (with-gl-disable :lighting
    (gl-set-fg-color)
    (if highlight?
        (progn
          (gl-set-sel-color)
          (gl:line-width (* 2 (line-thickness *drawing-settings*))))
        (progn
          (gl-set-fg-color)
          (gl:line-width (line-thickness *drawing-settings*))))
    (gl:begin :lines)
    (dolist (p points)
      (gl:vertex (p:x p) (p:y p) (p:z p)))
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
  (with-gl-enable :normalize
    (with-gl-enable :polygon-offset-fill
      (gl:polygon-offset 1.0 1.0)
      (3d-draw-filled-polygons-aux points faces face-normals point-normals point-colors))))

(defmethod 3d-draw-filled-polygons-aux (points faces face-normals point-normals point-colors)
  (let* ((col (shading-color *drawing-settings*))
         (r (c-red col))
         (g (c-green col))
         (b (c-blue col)))
    (gl-set-material col)
    (gl:color-material :front-and-back :diffuse)
    (with-gl-enable :color-material
      (cond ((and (> (length point-colors) 0) *do-smooth-shading?*)
             (dotimes (f (length faces))
               (gl:begin :polygon)
               (dolist (pref (aref faces f))
                 (let ((p (aref points pref))
                       (n (aref point-normals pref))
                       (c (aref point-colors pref)))
                   (gl:color (c-red c) (c-green c) (c-blue c))
                   (gl:normal (p:x n) (p:y n) (p:z n))
                   (gl:vertex (p:x p) (p:y p) (p:z p))))
               (gl:end)))
            ((> (length point-colors) 0)
             (dotimes (f (length faces))
               (gl:begin :polygon)
               (let ((n (aref face-normals f)))
                 (gl:normal (p:x n) (p:y n) (p:z n)))
               (dolist (pref (aref faces f))
                 (let ((p (aref points pref))
                       (c (aref point-colors pref)))
                   (gl:color (c-red c) (c-green c) (c-blue c))
                   (gl:vertex (p:x p) (p:y p) (p:z p))))
               (gl:end)))
              (*do-smooth-shading?*
               (dotimes (f (length faces))
                 (gl:begin :polygon)
                 (dolist (pref (aref faces f))
                   (let ((p (aref points pref))
                         (n (aref point-normals pref)))
                     (gl:normal (p:x n) (p:y n) (p:z n))
                     (gl:color r g b)
                     (gl:vertex (p:x p) (p:y p) (p:z p))))
                 (gl:end)))
              (t
               (dotimes (f (length faces))
                 (gl:begin :polygon)
                 (let ((n (aref face-normals f)))
                   (gl:normal (p:x n) (p:y n) (p:z n)))
                 (dolist (pref (aref faces f))
                   (let ((p (aref points pref)))
                     (gl:color r g b)
                     (gl:vertex (p:x p) (p:y p) (p:z p))))
                 (gl:end)))))))

(defmethod 3d-draw-filled-polygons-aux-SAV (points faces face-normals point-normals point-colors)
  (let ((col (shading-color *drawing-settings*)))
    (gl-set-material col)
    (gl:color-material :front-and-back :diffuse)
    (with-gl-enable :color-material
      (dotimes (f (length faces))
        (gl:begin :polygon)
        (when (not *do-smooth-shading?*)
          (let ((n (aref face-normals f)))
            (gl:normal (p:x n) (p:y n) (p:z n))))
        (dolist (pref (aref faces f))
          (if (> (length point-colors) 0)
              (let ((c (aref point-colors pref)))
                (gl:color (c-red c) (c-green c) (c-blue c)))
              (gl:color (c-red col) (c-green col) (c-blue col))) ;inefficient...
          (when *do-smooth-shading?*
            (let ((n (aref point-normals pref)))
              (gl:normal (p:x n) (p:y n) (p:z n))))
          (let ((p (aref points pref)))
            (gl:vertex (p:x p) (p:y p) (p:z p))))
        (gl:end)))))

(defun 3d-draw-highlighted-polygons (points faces face-normals point-normals faces-highlighted)
  (if *do-lighting?*
      (gl:enable :lighting)
      (gl:disable :lighting))
  (if *do-smooth-shading?*
      (gl:shade-model :smooth)
      (gl:shade-model :flat))
  (gl:polygon-mode :front-and-back :fill)
  (with-gl-enable :rescale-normal
    (3d-draw-highlighted-polygons-aux points faces face-normals point-normals faces-highlighted)))

(defmethod 3d-draw-highlighted-polygons-aux (points faces face-normals point-normals faces-highlighted)
  (let ((sel-col (sel-color *drawing-settings*)))
    (gl:color-material :front-and-back :diffuse)
    (with-gl-enable :color-material
      (dotimes (f (length faces))
        (when (aref faces-highlighted f)
          (gl:begin :polygon)
          (when (not *do-smooth-shading?*)
            (let ((n (aref face-normals f)))
              (gl:normal (p:x n) (p:y n) (p:z n))))
          (dolist (pref (aref faces f))
            (gl:color (c-red sel-col) (c-green sel-col) (c-blue sel-col))
            (when *do-smooth-shading?*
              (let ((n (aref point-normals pref)))
                (gl:normal (p:x n) (p:y n) (p:z n))))
            (let ((p (aref points pref)))
              (gl:vertex (p:x p) (p:y p) (p:z p))))
          (gl:end))))))

(defun 3d-draw-wireframe-polygons (points faces &key (closed? t))
  (gl:polygon-mode :front-and-back :line)
  (with-gl-disable :lighting
    (gl-set-fg-color)
    (gl:line-width (secondary-line-thickness *drawing-settings*))
    (dotimes (f (length faces))
      (if closed?
          (gl:begin :polygon)
          (gl:begin :line-strip))
      (dolist (pref (aref faces f))
        (let ((p (aref points pref)))
          (gl:vertex (p:x p) (p:y p) (p:z p))))
      (gl:end))))

;;; 2d display =================================================================

(defun 2d-setup-projection (w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0.0 w h 0.0 -1.0 1.0) ; y=0 at top
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:disable :depth-test)
  (gl:disable :cull-face)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :blend)
)

;;; ray  =======================================================================

(defun gl-get-camera-position ()
  (let* ((inverse-matrix (origin.dmat4:invert (gl:get-double :modelview-matrix)))
         (position (origin.dmat4:get-translation inverse-matrix)))
    (p-vec position)))

(defun gl-unproject-to-far-plane (screen-x screen-y)
  (multiple-value-bind (x y z)
      (glu:un-project screen-x screen-y 1.d0)
    (p! x y z)))

(defun gl-get-picking-ray-coords (screen-x screen-y)
  (values (gl-get-camera-position)
          (gl-unproject-to-far-plane screen-x screen-y)))
