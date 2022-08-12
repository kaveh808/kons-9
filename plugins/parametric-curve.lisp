(in-package #:kons-9)

#|
- proof of concept for parametric curves
- do equation in matrix form
- need to do the other classes as well
-- hermite-curve
-- catmull-clark-curve
-- b-spline-curve
-- nurbs-curve

NOTE: This won't work with the existing procedural mixin set up, because one control point
 or component of control point can be set and will not trigger the compute method
|#

;;; parametric-curve class =====================================================

;;; this shape is defined by a list of points (vertices)
(defclass parametric-curve (polygon)
  ((control-points :accessor control-points :initarg :control-points :initform nil) ;vec of 4 points
   (num-segments :accessor num-segments :initarg :num-segments :initform 16)
   (show-control-hull? :accessor show-control-hull? :initarg :show-control-hull? :initform t))
  (:default-initargs
   :is-closed-polygon? nil))

(defmethod copy-instance-data ((dst parametric-curve) (src parametric-curve))
  (error "COPY-INSTANCE-DATA not implemented for PARAMETRIC-CURVE"))


(defmethod draw ((curve parametric-curve))
  (when *display-wireframe?*
    (draw-wireframe curve))
  (when (show-control-hull? curve)
    (when *display-wireframe?*
      (3d-draw-curve (control-points curve) nil 1.0))
    (when *display-points?*
      (3d-draw-points (control-points curve)))))


;;; bezier-curve class =========================================================

;;; this shape is defined by a list of points (vertices)
(defclass bezier-curve (parametric-curve)
  ())

(defmethod compute-parametric-curve ((curve bezier-curve))
  (let ((num-segs (num-segments curve))
        (cv0 (aref (control-points curve) 0))
        (cv1 (aref (control-points curve) 1))
        (cv2 (aref (control-points curve) 2))
        (cv3 (aref (control-points curve) 3)))
    (setf (points curve) (make-array (1+ num-segs)))
    (dotimes (i (1+ num-segs))
      (let ((u (tween i 0.0 num-segs))) ;0-1 range
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
                                           :control-points (vector cv0 cv1 cv2 cv3)
                                           :num-segments num-segments)))

;;; parametric-curve shape functions ----------------------------------------------------

