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
(defclass cubic-parametric-curve (curve)
  ((cv0 :accessor cv0 :initarg :cv0 :initform (p! 0 0 0))
   (cv1 :accessor cv1 :initarg :cv1 :initform (p! 0 0 0))
   (cv2 :accessor cv2 :initarg :cv2 :initform (p! 0 0 0))
   (cv3 :accessor cv3 :initarg :cv3 :initform (p! 0 0 0))
   (num-segments :accessor num-segments :initarg :num-segments :initform 16)
   (show-control-hull? :accessor show-control-hull? :initarg :show-control-hull? :initform t))
  (:default-initargs
   :is-closed-curve? nil))

(defmethod copy-instance-data ((dst cubic-parametric-curve) (src cubic-parametric-curve))
  (error "COPY-INSTANCE-DATA not implemented for PARAMETRIC-CURVE"))

(defmethod draw ((curve cubic-parametric-curve))
  (when *display-wireframe?*
    (draw-wireframe curve))
  (when (show-control-hull? curve)
    (let ((hull (vector (cv0 curve) (cv1 curve) (cv2 curve) (cv3 curve))))
      (when *display-wireframe?*
        (3d-draw-curve hull nil (secondary-line-thickness *drawing-settings*)))
      (when *display-points?*
        (3d-draw-points hull nil)))))

;;; bezier-curve class =========================================================

(defclass bezier-curve (cubic-parametric-curve)
  ())

(defmethod compute-parametric-curve ((curve bezier-curve))
  (with-accessors ((num-segs num-segments) (cv0 cv0) (cv1 cv1) (cv2 cv2) (cv3 cv3))
      curve
    (setf (points curve) (make-array (1+ num-segs)))
    (dotimes (i (1+ num-segs))
      (let* ((u (tween i 0.0 num-segs))) ;0-1 range
        (setf (aref (points curve) i) (p! (+ (* (p:x cv0) (expt (- 1.0 u) 3))
                                             (* (p:x cv1) 3 u (expt (- 1.0 u) 2))
                                             (* (p:x cv2) 3 u u (- 1.0 u))
                                             (* (p:x cv3) (expt u 3)))
                                          (+ (* (p:y cv0) (expt (- 1.0 u) 3))
                                             (* (p:y cv1) 3 u (expt (- 1.0 u) 2))
                                             (* (p:y cv2) 3 u u (- 1.0 u))
                                             (* (p:y cv3) (expt u 3)))
                                          (+ (* (p:z cv0) (expt (- 1.0 u) 3))
                                             (* (p:z cv1) 3 u (expt (- 1.0 u) 2))
                                             (* (p:z cv2) 3 u u (- 1.0 u))
                                             (* (p:z cv3) (expt u 3)))))))
    curve))

(defun make-bezier-curve (cv0 cv1 cv2 cv3 &optional (num-segments 16))
  (compute-parametric-curve (make-instance 'bezier-curve
                                           :cv0 cv0 :cv1 cv1 :cv2 cv2 :cv3 cv3
                                           :num-segments num-segments)))

;;; parametric-curve shape functions ----------------------------------------------------

;;; just a fun mathematical curve -- https://en.wikipedia.org/wiki/Butterfly_curve_(transcendental)
(defun make-butterfly-curve (num-segments)
  (let ((points (make-array num-segments))
        (angle-delta (/ (* 12 pi) num-segments)))
    (dotimes (i num-segments)
      (let* ((angle (* i angle-delta))
             (e 2.71828)
             (radius (- (expt e (cos angle)) (* 2 (cos (* 4 angle))) (expt (sin (/ angle 12)) 5))))
        (setf (aref points i)
              (p! (* (sin angle) radius)
                  (* (cos angle) radius)
                  0))))
    (make-curve points)))

;;; the two functions below are inspired by spirograph curves

;;; hypotrochoid curve -- https://en.wikipedia.org/wiki/Hypotrochoid
;;; NOTE: fixed-r and rolling-r must be integer values due to lcm calculation
;;; resulting curve is scaled by 1/fixed-r
(defun make-hypotrochoid-curve (fixed-r rolling-r dist num-segments)
  (let* ((points (make-array num-segments))
         (total-sweep (* 2 pi (/ (lcm fixed-r rolling-r) fixed-r)))
         (theta-delta (/ total-sweep num-segments)))
    (dotimes (i num-segments)
      (let* ((theta (* i theta-delta))
             (angle (* (/ (- fixed-r rolling-r) rolling-r) theta)))
        (setf (aref points i)
              (p! (+ (* (- fixed-r rolling-r) (cos theta)) (* dist (cos angle)))
                  (- (* (- fixed-r rolling-r) (sin theta)) (* dist (sin angle)))
                  0))))
    (scale-by (make-curve points) (/ 1.0 fixed-r))))

;;; epitrochoid curve -- https://en.wikipedia.org/wiki/Epitrochoid
;;; NOTE: fixed-r and rolling-r must be integer values due to lcm calculation
;;; resulting curve is scaled by 1/fixed-r
(defun make-epitrochoid-curve (fixed-r rolling-r dist num-segments)
  (let* ((points (make-array num-segments))
         (total-sweep (* 2 pi (/ (lcm fixed-r rolling-r) fixed-r)))
         (theta-delta (/ total-sweep num-segments)))
    (dotimes (i num-segments)
      (let* ((theta (* i theta-delta))
             (angle (* (/ (+ fixed-r rolling-r) rolling-r) theta)))
        (setf (aref points i)
              (p! (- (* (+ fixed-r rolling-r) (cos theta)) (* dist (cos angle)))
                  (- (* (+ fixed-r rolling-r) (sin theta)) (* dist (sin angle)))
                  0))))
    (scale-by (make-curve points) (/ 1.0 fixed-r))))

;;; spirograph-like setup ------------------------------------------------------

(defun make-spirograph (scene ring-radius gear-radius arm-length gear-inside-ring?
                        rotation-offset rotation-increment &key (show-assembly? nil) (color nil))
  (let* ((gear-rotation-step (* (/ ring-radius gear-radius)
                                rotation-increment
                                (if gear-inside-ring? -1.0 1.0)))
         (gear-offset (if gear-inside-ring?
                          (- ring-radius gear-radius)
                          (+ ring-radius gear-radius)))
         (ring (make-circle (* 2 ring-radius) 64))
         (gear (make-circle (* 2 gear-radius) 16))
         (pen-location (p! 0 arm-length 0))
         (arm (make-line-curve (p! 0 0 0) pen-location 1))
         (gear-assembly (translate-by (make-shape-group (list gear arm))
                                      (p! 0 gear-offset 0)))
         (top-assembly (make-shape-group (list gear-assembly)))
         ;; generated curve with point colors
         (curve (allocate-point-colors (make-instance 'curve :is-closed-curve? nil)))
         (anim (make-instance
                'animator
                :setup-fn (lambda ()
                            (rotate-by top-assembly (p! 0 0 rotation-offset)))
                :update-fn (lambda ()
                             (rotate-by top-assembly (p! 0 0 rotation-increment))
                             (rotate-by gear-assembly (p! 0 0 gear-rotation-step))
                             (append-point curve
                                           (shape-global-point scene gear pen-location)
                                           color)))))
    (add-shape scene top-assembly)
    (add-shape scene ring)
    (setf (is-visible? top-assembly) show-assembly?)
    (setf (is-visible? ring) show-assembly?)
    (add-shape scene curve)
    (add-motion scene anim)
    curve))
