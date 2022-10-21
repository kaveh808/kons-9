(in-package #:kons-9)

;;;; USD format support ======================================================

(defun usd-safe-name (name)
  (remove #\_ (remove #\- (string name))))

(defmethod export-usd ((scene scene) filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-usd scene stream)))

(defmethod write-usd ((scene scene) &optional (stream t) (indent 0))
  (write-usd-header scene stream)
  (write-usd (shape-root scene) stream indent))

(defmethod write-usd-header ((scene scene) &optional (stream t))
  (format stream "#usda 1.0~%")
  (format stream "(~%")
  (format stream "    doc = \"kons-9 USD export -- https://github.com/kaveh808/kons-9\"~%")
  (format stream "    metersPerUnit = 1~%")
  (format stream "    upAxis = \"Y\"~%")
  (format stream ")~%~%"))

(defun format-pad (indent stream &rest args)
  (format stream (indent-padding indent))
  (apply #'format stream args))

(defmethod write-usd :before ((shape shape) &optional (stream t) (indent 0))
  (format-pad indent stream "def Xform \"~aXFORM\"~%" (usd-safe-name (name shape)))
  (format-pad indent stream "{~%")
  (let ((mtx (transform-matrix (transform shape))))
    (format-pad indent stream "    matrix4d xformOp:transform = ( (~A, ~A, ~A, ~A), (~A, ~A, ~A, ~A), (~A, ~A, ~A, ~A), (~A, ~A, ~A, ~A) )~%"
                (aref mtx 0 0) (aref mtx 0 1) (aref mtx 0 2) (aref mtx 0 3)
                (aref mtx 1 0) (aref mtx 1 1) (aref mtx 1 2) (aref mtx 1 3)
                (aref mtx 2 0) (aref mtx 2 1) (aref mtx 2 2) (aref mtx 2 3)
                (aref mtx 3 0) (aref mtx 3 1) (aref mtx 3 2) (aref mtx 3 3)))
  (format-pad indent stream "    uniform token[] xformOpOrder = [\"xformOp:transform\"]~%~%"))

(defmethod write-usd ((shape shape) &optional (stream t) (indent 0))
  (declare (ignore stream indent))
  ;; do nothing -- subclass responsibility
  )

(defmethod write-usd :after ((shape shape) &optional (stream t) (indent 0))
  (format-pad indent stream "}~%"))

(defmethod write-usd ((curve curve) &optional (stream t) (indent 0))
  (declare (ignore stream indent))
  ;; do nothing for now
  )

(defmethod write-usd ((group shape-group) &optional (stream t) (indent 0))
  (do-children (shape group)
    (write-usd shape stream (+ indent 4))))

(defmethod write-usd ((polyh polyhedron) &optional (stream t) (indent 0))
  (format-pad indent stream "    def Mesh \"~a\"~%" (usd-safe-name (name polyh)))
  (format-pad indent stream "    {~%")
  (format-pad indent stream "        int[] faceVertexCounts = [~{~a~^, ~}]~%" (usd-face-counts polyh))
  (format-pad indent stream "        int[] faceVertexIndices = [~{~a~^, ~}]~%" (usd-face-vertex-indices polyh))
  (format-pad indent stream "        point3f[] points = [~{~a~^, ~}]~%" (usd-points polyh))
  (format-pad indent stream "        uniform token subdivisionScheme = \"none\"~%")
  (format-pad indent stream "    }~%"))

(defmethod usd-face-counts ((polyh polyhedron))
  (map 'list #'length (faces polyh)))

(defmethod usd-face-vertex-indices ((polyh polyhedron))
  (flatten-list (coerce (faces polyh) 'list)))

(defmethod usd-points ((polyh polyhedron))
  (map 'list #'point->usd-string (points polyh)))

(defun point->usd-string (p)
  (format nil "(~a, ~a, ~a)" (p:x p) (p:y p) (p:z p)))



#|
(defmethod write-usd ((mesh uv-mesh) &optional (stream t) (indent 0))
  (format-pad indent stream "    def Mesh \"~a\"~%" (name mesh))
  (format-pad indent stream "    {~%")
  (format-pad indent stream "        int[] faceVertexCounts = [~{~a~^, ~}]~%" (usd-face-counts mesh))
  (format-pad indent stream "        int[] faceVertexIndices = [~{~a~^, ~}]~%" (usd-face-vertex-indices mesh))
  (format-pad indent stream "        point3f[] points = [~{~a~^, ~}]~%" (usd-points mesh))
  (format-pad indent stream "        uniform token subdivisionScheme = \"none\"~%")
  (format-pad indent stream "    }~%"))

(defmethod usd-face-counts ((mesh uv-mesh))
  (array->list (make-array (* (1- (wrapped-u-dim mesh)) (1- (wrapped-v-dim mesh)))
			   :initial-element 4)))

(defmethod usd-face-vertex-indices ((mesh uv-mesh))
  (let ((indices '())
	(u-dim (u-dim mesh))
	(v-dim (v-dim mesh)))
    (dotimes (u (1- (wrapped-u-dim mesh)))
      (dotimes (v (1- (wrapped-v-dim mesh)))
	(push (array-row-major-index (uv-point-array mesh)          u                  v)         indices)
	(push (array-row-major-index (uv-point-array mesh) (mod (1+ u) u-dim)          v)         indices)
	(push (array-row-major-index (uv-point-array mesh) (mod (1+ u) u-dim) (mod (1+ v) v-dim)) indices)
	(push (array-row-major-index (uv-point-array mesh)          u         (mod (1+ v) v-dim)) indices)))
    (reverse indices)))

(defun point->usd-string (p)
  (format nil "(~a, ~a, ~a)" (p:x p) (p:y p) (p:z p)))

(defmethod usd-points ((mesh uv-mesh))
  (let ((points '()))
    (dotimes (u (u-dim mesh))
      (dotimes (v (v-dim mesh))
	(push (point->usd-string (aref (uv-point-array mesh) u v)) points)))
    (reverse points)))
|#

;;;; export-animator ==================================================

(defclass export-animator (animator)
  ((base-filename :accessor base-filename :initarg :base-filename :initform "export")
   (num-frame-digits :accessor num-frame-digits :initarg :num-frame-digits :initform 4)
   (file-extension :accessor file-extension :initarg :file-extension :initform "txt")
   (scene :accessor scene :initarg :scene :initform nil)
   (export-fn :accessor export-fn :initarg :export-fn :initform nil)))

(defmethod update-animator ((anim export-animator))
  (let* ((format-string (strcat "~a_~" (write-to-string (num-frame-digits anim)) ",'0d.~a"))
	 (filename (format nil format-string
			   (base-filename anim) (current-frame (scene anim)) (file-extension anim))))
    (funcall (export-fn anim) (scene anim) filename)))
	 
(defun make-usd-export-animator (scene filename)
  (make-instance 'export-animator :base-filename filename :file-extension "usda"
				  :scene scene :export-fn #'export-usd))

(defun export-usd-frame (scene filename)
  (update-animator
   (make-instance 'export-animator :base-filename filename :file-extension "usda"
				   :scene scene :export-fn #'export-usd)))
