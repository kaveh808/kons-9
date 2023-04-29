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

(defmethod field-cell-size ((field scalar-field))
  (with-accessors ((x-dim x-dim) (y-dim y-dim) (z-dim z-dim) (lo bounds-lo) (hi bounds-hi))
      field
    (let* ((size (p! (1- x-dim) (1- y-dim) (1- z-dim)))
           (dim (p-from-to lo hi)))
      (p/ dim size))))

(defmethod point-cell-coords ((field scalar-field) p)
  (p/ (p:- p (bounds-lo field)) (field-cell-size field)))

;;; use trilinear interpolation
;;; NOTE -- array must have size > 1 in all dimensions
(defmethod field-value-at-point ((field scalar-field) p)
  (if (not (point-in-bounds? p (bounds-lo field) (bounds-hi field)))
      0
      (let* ((array (scalar-array field))
             (cell-coords (point-cell-coords field p))
             (dims (array-dimensions array)))
        (multiple-value-bind (x0 xd) (floor (p:x cell-coords))
          (multiple-value-bind (y0 yd) (floor (p:y cell-coords))
            (multiple-value-bind (z0 zd) (floor (p:z cell-coords))
              ;; test for when at positive edge of array bounds
              (when (= x0 (1- (nth 0 dims)))
                (setf x0 (1- x0))
                (setf xd 1.0))
              (when (= y0 (1- (nth 1 dims)))
                (setf y0 (1- y0))
                (setf yd 1.0))
              (when (= z0 (1- (nth 2 dims)))
                (setf z0 (1- z0))
                (setf zd 1.0))
              ;; do interpolation
              (let* ((x1 (1+ x0))
                     (y1 (1+ y0))
                     (z1 (1+ z0))
                     (v00 (+ (* (aref array x0 y0 z0) (- 1 xd)) (* (aref array x1 y0 z0) xd)))
                     (v01 (+ (* (aref array x0 y0 z1) (- 1 xd)) (* (aref array x1 y0 z1) xd)))
                     (v10 (+ (* (aref array x0 y1 z0) (- 1 xd)) (* (aref array x1 y1 z0) xd)))
                     (v11 (+ (* (aref array x0 y1 z1) (- 1 xd)) (* (aref array x1 y1 z1) xd)))
                     (v0  (+ (* v00 (- 1 yd)) (* v10 yd)))
                     (v1  (+ (* v01 (- 1 yd)) (* v11 yd)))
                     (v   (+ (* v0  (- 1 zd)) (* v1  zd))))
                v)))))))

(defmethod threshold-cell-points ((field scalar-field) threshold &optional (boundary-points? t))
  (let ((points (make-array 0 :adjustable t :fill-pointer 0)))
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
                    (when (>= (aref array x y z) threshold)
                      (if boundary-points? ;only return points on the threshold boundary
                          (when (or (= x 0) (= x (1- x-dim)) ;edge of field
                                    (= y 0) (= y (1- y-dim)) ;ditto
                                    (= z 0) (= z (1- z-dim)) ;ditto
                                    ;; at threshold border (one 6-neighbor is < threshold)
                                    (not (and (>= (aref array (1- x) y z) threshold)
                                              (>= (aref array (1+ x) y z) threshold)
                                              (>= (aref array x (1- y) z) threshold)
                                              (>= (aref array x (1+ y) z) threshold)
                                              (>= (aref array x y (1- z)) threshold)
                                              (>= (aref array x y (1+ z)) threshold))))
                            (vector-push-extend (p! px py pz) points))
                          (vector-push-extend (p! px py pz) points)))))))))))
    points))

(defmethod set-field-value-limits ((field scalar-field) &key (min-value nil) (max-value nil))
  (when (or min-value max-value)
    (with-accessors ((x-dim x-dim) (y-dim y-dim) (z-dim z-dim) (array scalar-array))
        field
      (dotimes (x x-dim)
        (dotimes (y y-dim)
          (dotimes (z z-dim)
            (let ((value (aref array x y z)))
              (setf (aref array x y z)
                    (bounded-value value min-value max-value))))))))
  field)

(defmethod set-field-value ((field scalar-field) i j k value &key (min-value nil) (max-value nil))
  (with-accessors ((array scalar-array))
      field
    (setf (aref array i j k) (bounded-value value min-value max-value))))

(defmethod set-field-constant-value ((field scalar-field) value)
  (with-accessors ((x-dim x-dim) (y-dim y-dim) (z-dim z-dim) (array scalar-array))
      field
    (dotimes (x x-dim)
      (dotimes (y y-dim)
        (dotimes (z z-dim)
          (setf (aref array x y z) value)))))
  field)

(defmethod apply-field-function ((field scalar-field) fn &key (operation :set) ;set, add
                                                           (min-value nil) (max-value nil))
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
                       (pz (lerp dz z-lo z-hi))
                       (fn-val (funcall fn (p! px py pz))))
                  (setf (aref array x y z)
                        (ecase operation
                          (:set
                           (bounded-value fn-val min-value max-value))
                          (:add
                           (bounded-value (+ (aref array x y z) fn-val)
                                          min-value max-value))))))))))))
                    
  field)

(defun one-over-r-squared-value-fn (strength falloff &optional (epsilon 0.0001))
  (lambda (dist)
    (let* ((r (max dist epsilon))
           (mag (/ strength (* falloff r r))))
      mag)))

(defun linear-value-fn (strength extent)
  (lambda (dist)
    (let* ((f (clamp (- 1.0 (/ dist extent)) 0.0 1.0))
           (mag (* strength f)))
      mag)))

(defun in-out-cubic-value-fn (strength extent)
  (lambda (dist)
    (let* ((f (clamp (- 1.0 (/ dist extent)) 0.0 1.0))
           (mag (* strength (in-out-cubic-fn f))))
      mag)))

(defun point-source-field-fn (point-source &optional (value-fn #'one-over-r-squared-value-fn))
  (let ((points (source-points point-source)))
    (lambda (p)
      (let ((val 0))
        (do-array (i source-p points)
          (let* ((dist (p-dist source-p p))
                 (mag (funcall value-fn dist)))
            (incf val mag)))
        val))))

(defun curve-source-field-fn (curve-source value-fn)
  (let ((curves (source-curves curve-source))
        (curves-closed (source-curves-closed curve-source)))
    (lambda (p)
      (let ((val 0))
        (loop for curve in curves
              for closed? in curves-closed
              do (let* ((dist (point-curve-dist p curve closed?))
                        (mag (funcall value-fn dist)))
                   (incf val mag)))
        val))))

;; (defun point-source-field-fn (point-source &key (strength 1.0) (falloff 1.0) (epsilon 0.0001))
;;   (let ((points (source-points point-source)))
;;     (lambda (p)
;;       (let ((val 0))
;;         (do-array (i source-p points)
;;           (let* ((dist (p-dist source-p p))
;;                  (r (max dist epsilon))
;;                  (mag (/ strength (* falloff r r))))
;;             (incf val mag)))
;;         val))))

;; (defun curve-source-field-fn (curve-source &key (strength 1.0) (falloff 1.0) (epsilon 0.0001))
;;   (let ((curves (source-curves curve-source))
;;         (curves-closed (source-curves-closed curve-source)))
;;     (lambda (p)
;;       (let ((val 0))
;;         (loop for curve in curves
;;               for closed? in curves-closed
;;               do (let* ((dist (point-curve-dist p curve closed?))
;;                         (r (max dist epsilon))
;;                         (mag (/ strength (* falloff r r))))
;;                    (incf val mag)))
;;         val))))

