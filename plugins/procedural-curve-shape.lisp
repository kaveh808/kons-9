(in-package #:kons-9)

;;;; procedural-curve-shape ====================================================

(defclass procedural-curve-shape (curve-shape procedural-mixin)
  ((num-points :accessor num-points :initarg :num-points :initform 64)))

(def-procedural-input procedural-curve-shape num-points)
(def-procedural-output procedural-curve-shape points)

;;;; circle-shape ==============================================================

(defclass circle-shape (procedural-curve-shape)
  ((diameter :accessor diameter :initarg :diameter :initform 2.0)))

(def-procedural-input circle-shape diameter)

(defmethod compute-procedural-node ((shape circle-shape))
  (with-accessors ((d diameter) (n num-points))
    shape
    (let ((points '())
          (radius (/ d 2.0))
          (angle-delta (/ 2pi n)))
      (dotimes (i n)
        (let ((angle (* i angle-delta)))
          (push (p! (* (sin angle) radius) (* (cos angle) radius) 0)
                points)))
      (setf (points shape) points)
      shape)))

(defun make-circle-shape (diameter &optional (num-points 64))
  (make-instance 'circle-shape :diameter diameter :num-points num-points))

;;;; sine-curve-shape ==========================================================

(defclass sine-curve-shape (procedural-curve-shape)
  ((period :accessor period :initarg :period :initform 360.0)
   (frequency :accessor frequency :initarg :frequency :initform 1.0)
   (x-scale :accessor x-scale :initarg :x-scale :initform 1.0)
   (y-scale :accessor y-scale :initarg :y-scale :initform 1.0))
  (:default-initargs
   :is-closed-shape? nil))

(def-procedural-input sine-curve-shape period)
(def-procedural-input sine-curve-shape frequency)
(def-procedural-input sine-curve-shape x-scale)
(def-procedural-input sine-curve-shape y-scale)

(defmethod compute-procedural-node ((shape sine-curve-shape))
  (with-accessors ((period period) (frequency frequency) (x-scale x-scale) (y-scale y-scale) (n num-points))
      shape
    (let* ((points '())
           (rad-period (radians period))
           (angle-delta (/ rad-period n)))
      (dotimes (i (1+ n))
        (let ((angle (* i angle-delta frequency)))
          (push (p! (* x-scale (/ angle (* frequency rad-period))) (* y-scale (sin angle)) 0)
                points)))
      (setf (points shape) (nreverse points))
      shape)))

(defun make-sine-curve-shape (x-scale y-scale &optional (num-points 64))
  (make-instance 'sine-curve-shape :x-scale x-scale :y-scale y-scale :num-points num-points))
