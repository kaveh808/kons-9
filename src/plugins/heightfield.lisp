(in-package #:kons-9)

;;;; heightfield =======================================================

(defclass heightfield (uv-mesh)
  ((height-fn :accessor height-fn :initarg :height-fn :initform nil)))

(defmethod make-heightfield (x-segments z-segments bounds-lo bounds-hi &optional (height-fn nil))
  (let ((hfield (make-instance 'heightfield :u-dim (1+ z-segments)
                                            :v-dim (1+ x-segments)
                                            :u-wrap nil
                                            :v-wrap nil
                                            :height-fn height-fn)))
    (allocate-mesh-arrays hfield)
    (setf (uv-point-array hfield) (grid-point-array x-segments z-segments bounds-lo bounds-hi))
    (when height-fn
      (compute-heights hfield))
    (compute-polyhedron-data hfield)
    hfield))

(defun grid-point-array (x-segments z-segments &optional (bounds-lo (p! -1 0 -1)) (bounds-hi (p! 1 0 1)))
  (let* ((u-dim (1+ z-segments))
         (v-dim (1+ x-segments))
         (uv-point-array (make-array (list u-dim v-dim))))
    (dotimes (u u-dim)
      (let* ((fx (/ u (- u-dim 1.0)))
	     (x (lerp fx (p:x bounds-lo) (p:x bounds-hi))))
	(dotimes (v v-dim)
	  (let* ((fz (/ v (- v-dim 1.0)))
		 (z (lerp (- 1 fz) (p:z bounds-lo) (p:z bounds-hi)))) ;reverse order for correct backface culling: (- 1 fz)
	    (setf (aref uv-point-array u v) (p! x 0.0 z))))))
    uv-point-array))

(defmethod compute-heights ((hfield heightfield))
  (dotimes (u (u-dim hfield))
    (dotimes (v (v-dim hfield))
      (let ((p (aref (uv-point-array hfield) u v)))
        (setf (p:y p) (funcall (height-fn hfield) (p:x p) (p:z p)))))))

(defmethod update-heightfield ((hfield heightfield))
  (compute-heights hfield)
  (compute-face-normals hfield)
  (compute-point-normals hfield))

