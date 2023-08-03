(in-package #:kons-9)

;;;; force-field ===============================================================

(defclass force-field ()
  ())

(defmethod field-value ((field force-field) point time)
  ;; subclass responsibility
  (declare (ignore point time))
  (p! 0 0 0))

;;;; constant-force-field ======================================================

(defclass constant-force-field (force-field)
  ((force-vector :accessor force-vector :initarg :force-vector :initform (p! 0 0 0))))

(defmethod field-value ((field constant-force-field) point time)
  (declare (ignore point time))
  (force-vector field))

;;;; attractor-force-field =====================================================

(defclass attractor-force-field (force-field)
  ((location :accessor location :initarg :location :initform (p! 0 0 0))
   (magnitude :accessor magnitude :initarg :magnitude :initform 1.0)))

(defmethod field-value ((field attractor-force-field) point time)
  (declare (ignore time))
  (let ((dist (p-dist point (location field)))
        (dir (p:normalize (p-from-to point (location field)))))
    (if (= 0.0 dist)
        (p! 0 0 0)
        (p:scale dir (/ (magnitude field) (* dist dist))))))

;;;; time-varying-force-field ==================================================

(defclass time-varying-force-field (constant-force-field)
  ((noise-frequency :accessor noise-frequency :initarg :noise-frequency :initform 1.0)
   (noise-amplitude :accessor noise-amplitude :initarg :noise-amplitude :initform 1.0)))

(defmethod field-value ((field time-varying-force-field) point time)
  (declare (ignore point))
  (p+ (force-vector field)
      (p:scale (float-noise-gradient (* time (noise-frequency field)))
               (noise-amplitude field))))

;;;; 3d-noise-force-field =========================================================

(defclass 3d-noise-force-field (force-field)
  ((noise-frequency :accessor noise-frequency :initarg :noise-frequency :initform 1.0)
   (noise-amplitude :accessor noise-amplitude :initarg :noise-amplitude :initform 1.0)))

(defmethod field-value ((field 3d-noise-force-field) point time)
  (declare (ignore time))
  (p:scale (noise-gradient (p:scale point (noise-frequency field)))
           (noise-amplitude field)))
