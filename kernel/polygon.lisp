(in-package #:kons-9)

;;; polygon class ====================================================

;;; this shape is defined by a list of points (vertices)
(defclass polygon (point-cloud)
  ((is-closed-polygon? :accessor is-closed-polygon? :initarg :is-closed-polygon? :initform t)))

(defmethod copy-instance-data ((dst polygon) (src polygon))
  (error "COPY-INSTANCE-DATA not implemented for POLYGON"))

(defun make-polygon (points &optional (is-closed-polygon? t))
  (make-instance 'polygon :points points :is-closed-polygon? is-closed-polygon?))

(defmethod draw ((poly polygon))
  (when *display-wireframe?*
    (draw-wireframe poly))
  (when *display-points?*
    (draw-points poly)))

(defmethod draw-wireframe ((poly polygon))
  (3d-draw-curve (points poly) (is-closed-polygon? poly)))

(defmethod draw-points ((poly polygon))
  (3d-draw-points (points poly)))

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
    (p-normalize (p- (aref points i2) (aref points i1)))))

(defun curve-tangents-aux (points &optional (is-closed? nil))
  (let ((tangents (make-array (length points))))
    (doarray (i p points)
      (setf (aref tangents i) (curve-point-tangent i points is-closed?)))
    tangents))

(defmethod curve-tangents ((poly polygon))
  (curve-tangents-aux (points poly) (is-closed-polygon? poly)))

;;; polygon shape functions ----------------------------------------------------

(defun make-line-polygon (p1 p2 num-segments)
  (make-polygon (make-line-points p1 p2 num-segments)
                nil))

(defun make-rectangle-polygon (width height &optional (num-segments 1))
  (make-polygon (make-rectangle-points width height num-segments)))

(defun make-square-polygon (side &optional (num-segments 1))
  (make-polygon (make-rectangle-points side side num-segments)))

(defun make-circle-polygon (diameter num-segments)
  (make-polygon (make-circle-points diameter num-segments)))

(defun make-arc-polygon (diameter num-segments start-angle end-angle)
  (make-polygon (make-arc-points diameter num-segments start-angle end-angle)
                nil))

(defun make-spiral-polygon (start-diameter end-diameter length loops num-segments)
  (make-polygon (make-spiral-points start-diameter end-diameter length loops num-segments)
                nil))

(defun make-sine-curve-polygon (period frequency x-scale y-scale num-segments)
  (make-polygon (make-sine-curve-points period frequency x-scale y-scale num-segments)
                nil))

#|
(defun make-arc-polygon (diameter num-segments start-angle end-angle)
  ...)

(defun make-3-point-arc (p0 p1 p2 num-segments)
  ...)


|#
