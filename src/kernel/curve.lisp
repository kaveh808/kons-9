(in-package #:kons-9)

;;; curve class ====================================================

;;; this shape is defined by an array of points (vertices)
(defclass curve (point-cloud)
  ((is-closed-curve? :accessor is-closed-curve? :initarg :is-closed-curve? :initform t)))

(defun make-curve (points &optional (is-closed-curve? t))
  (make-instance 'curve :points points :is-closed-curve? is-closed-curve?))

(defun curve-point-tangent (i points &optional (is-closed? nil))
  (let ((len (length points))
	i1
	i2)
    (if (= len 2)
	(progn (setf i1 0)
	       (setf i2 1))
	(cond ((= i 0) (if is-closed?
			   (progn (setf i1 (1- len))
				  (setf i2 1))
			   (progn (setf i1 0)
				  (setf i2 1))))
	      ((= i (1- len)) (if is-closed?
				  (progn (setf i1 (- len 2))
					 (setf i2 0))
				  (progn (setf i1 (- len 3))
					 (setf i2 i))))
	      (t (progn (setf i1 (1- i))
			(setf i2 (1+ i))))))
    (p:normalize (p:- (aref points i2) (aref points i1)))))

(defun curve-tangents-aux (points &optional (is-closed? nil))
  (let ((tangents (make-array (length points))))
    (doarray (i p points)
      (declare (ignore p))
      (setf (aref tangents i) (curve-point-tangent i points is-closed?)))
    tangents))

(defmethod curve-tangents ((curve curve))
  (curve-tangents-aux (points curve) (is-closed-curve? curve)))

;;; curve shape functions ----------------------------------------------------

(defun make-line-curve (p1 p2 num-segments)
  (make-curve (make-line-points p1 p2 num-segments)
                nil))

(defun make-rectangle-curve (width height &optional (num-segments 1))
  (make-curve (make-rectangle-points width height num-segments)))

(defun make-square-curve (side &optional (num-segments 1))
  (make-curve (make-rectangle-points side side num-segments)))

(defun make-circle-curve (diameter num-segments)
  (make-curve (make-circle-points diameter num-segments)))

(defun make-arc-curve (diameter start-angle end-angle num-segments)
  (make-curve (make-arc-points diameter start-angle end-angle num-segments)
                nil))

(defun make-spiral-curve (start-diameter end-diameter length loops num-segments)
  (make-curve (make-spiral-points start-diameter end-diameter length loops num-segments)
                nil))

(defun make-sine-curve-curve (period frequency x-scale y-scale num-segments)
  (make-curve (make-sine-curve-points period frequency x-scale y-scale num-segments)
                nil))

#|
(defun make-3-point-arc (p0 p1 p2 num-segments)
  ...)
|#
