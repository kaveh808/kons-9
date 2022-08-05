(in-package #:kons-9)

;;;; heightfield =======================================================

(defclass heightfield (uv-mesh)
  ((heightfn :accessor heightfn :initarg :heightfn :initform nil)))

(defmethod compute-heights ((heightfield heightfield))
  (dotimes (u (u-dim heightfield))
    (dotimes (v (v-dim heightfield))
      (let ((p (aref (uv-point-array heightfield) u v)))
        (setf (y p) (funcall (heightfn heightfield) (x p) (z p)))))))
  
(defmethod make-heightfield (u-dim v-dim bounds-lo bounds-hi &optional (heightfn nil))
  (let ((heightfield (make-instance 'heightfield :u-dim u-dim
                                                   :v-dim v-dim
                                                   :u-wrap nil
                                                   :v-wrap nil
                                                   :heightfn heightfn)))
    (allocate-mesh-arrays heightfield)
    (setf (uv-point-array heightfield) (grid-point-array u-dim v-dim bounds-lo bounds-hi))
    (when heightfn
      (compute-heights heightfield))
    (compute-polyhedron-data heightfield)
    heightfield))

