(in-package #:kons-9)

;;; curve-shape class ====================================================

;;; this shape is defined by a list of points (vertices)
(defclass curve-shape (shape)
  ((is-closed-shape? :accessor is-closed-shape? :initarg :is-closed-shape? :initform t)
   (points :accessor points :initarg :points :initform '())))

(defmethod copy-instance-data ((dst curve-shape) (src curve-shape))
  (error "COPY-INSTANCE-DATA not implemented for CURVE-SHAPE"))

(defmethod add-point ((self curve-shape) (p point))
  (push p (points self)))

(defmethod bounds-and-center ((self curve-shape))
  (when (= 0 (length (points self)))
    (return-from bounds-and-center (values nil nil nil)))
  (let* ((points (points self))
         (bounds-lo (p-copy (first points)))
         (bounds-hi (p-copy (first points))))
    (dolist (p points)
       (setf bounds-lo (p-min bounds-lo p))
       (setf bounds-hi (p-max bounds-hi p)))
    (values bounds-lo bounds-hi (p-average bounds-lo bounds-hi))))

(defmethod draw ((curve curve-shape))
  (when *display-wireframe?*
    (draw-wireframe curve))
  (when *display-points?*
    (draw-points curve)))

(defmethod draw-wireframe ((curve curve-shape))
  (3d-draw-curve (points curve) (is-closed-shape? curve)))

(defmethod draw-points ((curve curve-shape))
  (3d-draw-points (points curve)))

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
    (p-normalize (p- (nth i2 points) (nth i1 points)))))

(defun curve-tangents-aux (points &optional (is-closed? nil))
  (let ((tangents '()))
    (dotimes (i (length points))
      (push (curve-point-tangent i points is-closed?)
            tangents))
    (nreverse tangents)))

(defmethod curve-tangents ((curve curve-shape))
  (curve-tangents-aux (points curve) (is-closed-shape? curve)))

;;; randomize shape points
(defmethod randomize-points ((self curve-shape) (delta point))
  (setf (points self)
	(mapcar #'(lambda (p)
		    (let ((offset (p! (rand1 (x delta)) (rand1 (y delta)) (rand1 (z delta)))))
		      (p+ p offset)))
		(points self))))
