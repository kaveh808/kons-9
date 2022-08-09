(in-package #:kons-9)

;;;; point-cloud ========================================================

(defclass point-cloud (shape)
  ((points :accessor points :initarg :points :initform (make-array 0 :adjustable t :fill-pointer t))))

(defmethod copy-instance-data :after ((dst point-cloud) (src point-cloud))
  (setf (points dst) (points src))) ;;; TODO - deep copy arrays

(defmethod draw ((p-cloud point-cloud))
  (when *display-points?*
    (draw-points p-cloud)))

(defmethod draw-points ((p-cloud point-cloud))
  (3d-draw-points (points p-cloud)))

(defmethod bounds-and-center ((p-cloud point-cloud))
  (when (= 0 (length (points p-cloud)))
    (return-from bounds-and-center (values nil nil nil)))
  (let* ((points (points p-cloud))
         (bounds-lo (p-copy (aref points 0)))
         (bounds-hi (p-copy (aref points 0))))
    (doarray (i p points)
       (setf bounds-lo (p-min bounds-lo p))
       (setf bounds-hi (p-max bounds-hi p)))
    (values bounds-lo bounds-hi (p-average bounds-lo bounds-hi))))

(defun make-point-cloud (points)
  (make-instance 'point-cloud :points points))

(defun make-rectangle-points (width height)
  (let ((x (/ width 2))
        (y (/ height 2)))
    (vector (p! x y 0) (p! (- x) y 0) (p! (- x) (- y) 0) (p! x (- y) 0))))

(defun make-circle-points (diameter num-points)
  (let ((points (make-array num-points))
        (radius (/ diameter 2.0))
        (angle-delta (/ 2pi num-points)))
    (dotimes (i num-points)
      (let ((angle (* i angle-delta)))
        (setf (aref points i) (p! (* (sin angle) radius) (* (cos angle) radius) 0))))
    (nreverse points)))                 ;return ccw points

(defun make-sine-curve-points (period frequency x-scale y-scale num-points)
  (let* ((points (make-array (1+ num-points)))
         (rad-period (radians period))
         (angle-delta (/ rad-period num-points)))
    (dotimes (i (1+ num-points))
      (let ((angle (* i angle-delta frequency)))
        (setf (aref points i) (p! (* x-scale (/ angle (* frequency rad-period))) (* y-scale (sin angle)) 0))))
    points))

(defun make-random-points (num bounds-lo bounds-hi)
  (let ((points (make-array num)))
    (dotimes (i num)
      (setf (aref points i) (p-rand2 bounds-lo bounds-hi)))
    points))

(defun make-grid-points (nx ny nz bounds-lo bounds-hi)
  (let ((points (make-array (* nx ny nz)))
        (i -1))
    (dotimes (ix nx)
      (let* ((fx (tween ix 0 (- nx 1.0)))
	     (x (lerp fx (x bounds-lo) (x bounds-hi))))
        (dotimes (iy ny)
          (let* ((fy (tween iy 0 (- ny 1.0)))
                 (y (lerp fy (y bounds-lo) (y bounds-hi))))
            (dotimes (iz nz)
              (let* ((fz (tween iz 0 (- nz 1.0)))
                     (z (lerp fz (z bounds-lo) (z bounds-hi))))
                (setf (aref points (incf i)) (p! x y z))))))))
    points))

;;; TODO - in-place array modification?
;;; randomize points
(defmethod randomize-points ((p-cloud point-cloud) (delta point))
  (setf (points p-cloud)
	(map 'vector #'(lambda (p)
                         (let ((offset (p! (rand1 (x delta)) (rand1 (y delta)) (rand1 (z delta)))))
                           (p+ p offset)))
             (points p-cloud))))

