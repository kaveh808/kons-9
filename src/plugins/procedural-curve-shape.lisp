(in-package #:kons-9)

;;;; procedural-polygon ====================================================

(defclass procedural-polygon (polygon procedural-mixin)
  ((num-points :accessor num-points :initarg :num-points :initform 64)))

(def-procedural-input procedural-polygon num-points)
(def-procedural-output procedural-polygon points)

;;;; procedural-circle-polygon ==============================================================

(defclass procedural-circle-polygon (procedural-polygon)
  ((diameter :accessor diameter :initarg :diameter :initform 2.0)))

(def-procedural-input procedural-circle-polygon diameter)

(defmethod compute-procedural-node ((poly procedural-circle-polygon))
  (setf (points poly) (make-circle-points (diameter poly) (num-points poly))))

(defun make-procedural-circle-polygon (diameter &optional (num-points 64))
  (make-instance 'procedural-circle-polygon :diameter diameter :num-points num-points))

;;;; procedural-sine-curve-polygon ==========================================================

(defclass procedural-sine-curve-polygon (procedural-polygon)
  ((period :accessor period :initarg :period :initform 360.0)
   (frequency :accessor frequency :initarg :frequency :initform 1.0)
   (x-scale :accessor x-scale :initarg :x-scale :initform 1.0)
   (y-scale :accessor y-scale :initarg :y-scale :initform 1.0))
  (:default-initargs
   :is-closed-polygon? nil))

(def-procedural-input procedural-sine-curve-polygon period)
(def-procedural-input procedural-sine-curve-polygon frequency)
(def-procedural-input procedural-sine-curve-polygon x-scale)
(def-procedural-input procedural-sine-curve-polygon y-scale)

(defmethod compute-procedural-node ((poly procedural-sine-curve-polygon))
  (setf (points poly) (make-sine-curve-points (period poly) (frequency poly)
                                               (x-scale poly) (y-scale poly) (num-points poly))))

(defun make-procedural-sine-curve-polygon (period frequency x-scale y-scale &optional (num-points 64))
  (make-instance 'procedural-sine-curve-polygon :period period :frequency frequency
                                                :x-scale x-scale :y-scale y-scale :num-points num-points))
