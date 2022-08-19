(in-package #:kons-9)

;;;; USD format support ======================================================

;;; TMP until we have unique shape names
(defparameter *usd-shape-num* 0)

(defun next-shape-name (name)
  (strcat name (write-to-string (incf *usd-shape-num*))))


(defmethod export-usd ((scene scene) filename)
  (with-open-file (stream filename :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (write-usd scene stream)))

(defmethod write-usd ((scene scene) &optional (stream t) (indent 0))
  (write-usd-header scene stream)
  (mapc #'(lambda (s) (write-usd s stream)) (shapes scene)))

(defmethod write-usd-header ((scene scene) &optional (stream t))
  (format stream "#usda 1.0~%")
  (format stream "(~%")
  (format stream "    doc = \"kons-9 pre-alpha\"~%")
  (format stream "    metersPerUnit = 1~%")
  (format stream "    upAxis = \"Y\"~%")
  (format stream ")~%~%"))

(defun format-pad (indent stream &rest args)
  (format stream (indent-padding indent))
  (apply #'format stream args))

(defun point->usd-string (p)
  (format nil "(~a, ~a, ~a)" (x p) (y p) (z p)))


;;;; shape =====================================================================

(defmethod write-usd :before ((shape shape) &optional (stream t) (indent 0))
  (let ((mtx (transform-matrix (transform shape))))
    (format-pad indent stream "def Xform \"~a\"~%" (next-shape-name "shape"))
    (format-pad indent stream "{~%")
    (format-pad indent stream "    matrix4d xformOp:transform = ( (~s, ~s, ~s, ~s), (~s, ~s, ~s, ~s), (~s, ~s, ~s, ~s), (~s, ~s, ~s, ~s) )~%"
                (aref mtx 0 0) (aref mtx 0 1) (aref mtx 0 2) (aref mtx 0 3)
                (aref mtx 1 0) (aref mtx 1 1) (aref mtx 1 2) (aref mtx 1 3)
                (aref mtx 2 0) (aref mtx 2 1) (aref mtx 2 2) (aref mtx 2 3)
                (aref mtx 3 0) (aref mtx 3 1) (aref mtx 3 2) (aref mtx 3 3))
    (format-pad indent stream "    uniform token[] xformOpOrder = [\"xformOp:transform\"]~%~%")))


(defmethod write-usd ((shape shape) &optional (stream t) (indent 0))
  (declare (ignore stream))
  ;; do nothing for now
  )

(defmethod write-usd :after ((shape shape) &optional (stream t) (indent 0))
  (format-pad indent stream "}~%"))

;;;; point-cloud ===============================================================

(defmethod usd-points ((p-cloud point-cloud))
  (map 'list #'point->usd-string (points p-cloud)))

;;;; polygon ===================================================================

(defmethod write-usd ((poly polygon) &optional (stream t) (indent 0))
  (format-pad indent stream "    def BasisCurves \"~a\"~%" (next-shape-name "polygon"))
  (format-pad indent stream "    {~%")
  (format-pad indent stream "        uniform token type = \"linear\"~%")

  (format-pad indent stream "        int[] curveVertexCounts = [~a]~%" (length (points poly)))
  (format-pad indent stream "        point3f[] points = [~{~a~^, ~}]~%" (usd-points poly))
  ;TODO -- width hardwired for now -- not working in Blender
  (format-pad indent stream "        float[] widths = [0.01] (interpolation = \"constant\")~%")
;;            color3f[] primvars:displayColor = [(1, 0, 0)]
  (format-pad indent stream "    }~%"))

;;;; polyhedron ================================================================

(defmethod write-usd ((polyh polyhedron) &optional (stream t) (indent 0))
  (format-pad indent stream "    def Mesh \"~a\"~%" (next-shape-name "polyhedron"))
  (format-pad indent stream "    {~%")
  (format-pad indent stream "        int[] faceVertexCounts = [~{~a~^, ~}]~%" (usd-face-counts polyh))
  (format-pad indent stream "        int[] faceVertexIndices = [~{~a~^, ~}]~%" (usd-face-vertex-indices polyh))
  (format-pad indent stream "        point3f[] points = [~{~a~^, ~}]~%" (usd-points polyh))
  (format-pad indent stream "        uniform token subdivisionScheme = \"none\"~%")
  (format-pad indent stream "    }~%"))

(defmethod usd-face-counts ((polyh polyhedron))
  (map 'list #'length (faces polyh)))

(defmethod usd-face-vertex-indices ((polyh polyhedron))
  (apply #'append (coerce (faces polyh) 'list)))

;;;; group =====================================================================

(defmethod write-usd ((group group) &optional (stream t) (indent 0))
  (dolist (shape (children group))
    (write-usd shape stream (+ indent 4))))

;;;; uv-mesh ===================================================================

(defmethod write-usd ((mesh uv-mesh) &optional (stream t) (indent 0))
  (format-pad indent stream "    def Mesh \"~a\"~%" (next-shape-name "uvmesh"))
  (format-pad indent stream "    {~%")
  (format-pad indent stream "        int[] faceVertexCounts = [~{~a~^, ~}]~%" (usd-face-counts mesh))
  (format-pad indent stream "        int[] faceVertexIndices = [~{~a~^, ~}]~%" (usd-face-vertex-indices mesh))
  (format-pad indent stream "        point3f[] points = [~{~a~^, ~}]~%" (usd-points mesh))
  (format-pad indent stream "        uniform token subdivisionScheme = \"none\"~%")
  (format-pad indent stream "    }~%"))

;;;; export-animator ==================================================

#| needs testing

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
|#
