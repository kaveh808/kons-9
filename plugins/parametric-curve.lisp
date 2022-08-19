(in-package #:kons-9)

#|
- proof of concept for cubic parametric curves
- do equation in matrix form
- need to do the other classes as well
-- hermite-curve
-- catmull-clark-curve
-- b-spline-curve
-- nurbs-curve

NOTE: This won't work with the existing procedural mixin set up, because
 a component of a cv can be set and will not trigger the compute method.
|#

;;; cubic-parametric-curve class ===============================================

;;; this shape is defined by a list of points (vertices)
(defclass cubic-parametric-curve (polygon)
  ((cv0 :accessor cv0 :initarg :cv0 :initform (p! 0 0 0))
   (cv1 :accessor cv1 :initarg :cv1 :initform (p! 0 0 0))
   (cv2 :accessor cv2 :initarg :cv2 :initform (p! 0 0 0))
   (cv3 :accessor cv3 :initarg :cv3 :initform (p! 0 0 0))
   (num-segments :accessor num-segments :initarg :num-segments :initform 16)
   (show-control-hull? :accessor show-control-hull? :initarg :show-control-hull? :initform t))
  (:default-initargs
   :is-closed-polygon? nil))

(defmethod copy-instance-data ((dst cubic-parametric-curve) (src cubic-parametric-curve))
  (error "COPY-INSTANCE-DATA not implemented for PARAMETRIC-CURVE"))

(defmethod draw ((curve cubic-parametric-curve))
  (when *display-wireframe?*
    (draw-wireframe curve))
  (when (show-control-hull? curve)
    (let ((hull (vector (cv0 curve) (cv1 curve) (cv2 curve) (cv3 curve))))
      (when *display-wireframe?*
        (3d-draw-curve hull nil 1.0))
      (when *display-points?*
        (3d-draw-points hull)))))

;;; bezier-curve class =========================================================

(defclass bezier-curve (cubic-parametric-curve)
  ())

(defmethod compute-parametric-curve ((curve bezier-curve))
  (with-accessors ((num-segs num-segments) (cv0 cv0) (cv1 cv1) (cv2 cv2) (cv3 cv3))
      curve
    (setf (points curve) (make-array (1+ num-segs)))
    (dotimes (i (1+ num-segs))
      (let* ((u (tween i 0.0 num-segs))) ;0-1 range
        (setf (aref (points curve) i) (p! (+ (* (x cv0) (expt (- 1.0 u) 3))
                                             (* (x cv1) 3 u (expt (- 1.0 u) 2))
                                             (* (x cv2) 3 u u (- 1.0 u))
                                             (* (x cv3) (expt u 3)))
                                          (+ (* (y cv0) (expt (- 1.0 u) 3))
                                             (* (y cv1) 3 u (expt (- 1.0 u) 2))
                                             (* (y cv2) 3 u u (- 1.0 u))
                                             (* (y cv3) (expt u 3)))
                                          (+ (* (z cv0) (expt (- 1.0 u) 3))
                                             (* (z cv1) 3 u (expt (- 1.0 u) 2))
                                             (* (z cv2) 3 u u (- 1.0 u))
                                             (* (z cv3) (expt u 3)))))))
    curve))

(defun make-bezier-curve (cv0 cv1 cv2 cv3 &optional (num-segments 16))
  (compute-parametric-curve (make-instance 'bezier-curve
                                           :cv0 cv0 :cv1 cv1 :cv2 cv2 :cv3 cv3
                                           :num-segments num-segments)))

;;; parametric-curve shape functions ----------------------------------------------------

