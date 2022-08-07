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
  (3d-draw-points-array (points p-cloud)))

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

(defun make-point-cloud (&rest points)
  (make-instance 'point-cloud :points (make-array (length points)
                                                  :initial-contents points
                                                  :adjustable t
                                                  :fill-pointer t)))

(defun make-point-cloud-in-bounds (num bounds-lo bounds-hi)
  (let ((points '()))
    (dotimes (i num)
      (push (p-rand2 bounds-lo bounds-hi) points))
    (apply #'make-point-cloud points)))

