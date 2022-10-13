(in-package #:kons-9)

;;;; scalar-field ==============================================================

(defclass-kons-9 scalar-field (item)
  ((bounds-lo (p! -1 -1 -1))
   (bounds-hi (p!  1  1  1))
   (x-dim 0)
   (y-dim 0)
   (z-dim 0)
   (scalar-array nil)))

(defmethod printable-data ((self scalar-field))
  (strcat (call-next-method) (format nil ", dims (~a ~a ~a)" (x-dim self) (y-dim self) (z-dim self))))

(defmethod copy-instance-data ((dst scalar-field) (src scalar-field))
  (error "COPY-INSTANCE-DATA not implemented for SCALAR-FIELD"))

(defmethod make-scalar-field (x-dim y-dim z-dim &key (bounds-lo (p! -1 -1 -1)) (bounds-hi (p! 1 1 1)))
  (make-instance 'scalar-field :x-dim x-dim :y-dim y-dim :z-dim z-dim
                               :bounds-lo bounds-lo :bounds-hi bounds-hi
                               :scalar-array (make-array (list x-dim y-dim z-dim)
                                                         :element-type 'float
                                                         :initial-element 0.0)))

(defmethod apply-field-function ((field scalar-field) fn)
  (with-accessors ((x-dim x-dim) (y-dim y-dim) (z-dim z-dim)
                   (lo bounds-lo) (hi bounds-hi) (array scalar-array))
      field
    (let ((x-lo (p:x lo))
          (y-lo (p:y lo))
          (z-lo (p:z lo))
          (x-hi (p:x hi))
          (y-hi (p:y hi))
          (z-hi (p:z hi)))          
      (dotimes (x x-dim)
        (let* ((dx (/ x (1- (max x-dim 2))))
               (px (lerp dx x-lo x-hi)))
          (dotimes (y y-dim)
            (let* ((dy (/ y (1- (max y-dim 2))))
                   (py (lerp dy y-lo y-hi)))
              (dotimes (z z-dim)
                (let* ((dz (/ z (1- (max z-dim 2))))
                       (pz (lerp dz z-lo z-hi)))
                  (setf (aref array x y z) (funcall fn (p! px py pz)))))))))))
  field)

(defun point-source-field-fn (point-source &key (strength 1.0) (falloff 1.0) (epsilon 0.0001))
  (let ((points (source-points point-source)))
    (lambda (p)
      (let ((val 0))
        (do-array (i source-p points)
          (let* ((dist (p-dist source-p p))
                 (r (max dist epsilon))
                 (mag (/ strength (* falloff r r))))
            (incf val mag)))
        val))))

(defun curve-source-field-fn (curve-source &key (strength 1.0) (falloff 1.0) (epsilon 0.0001))
  (let ((curves (source-curves curve-source))
        (curves-closed (source-curves-closed curve-source)))
    (lambda (p)
      (let ((val 0))
        (loop for curve in curves
              for closed? in curves-closed
              do (let* ((dist (point-curve-dist p curve closed?))
                        (r (max dist epsilon))
                        (mag (/ strength (* falloff r r))))
                   (incf val mag)))
        val))))
