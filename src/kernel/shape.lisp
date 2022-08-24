(in-package #:kons-9)

;;;; shape =====================================================================

(defclass shape (scene-item)
  ((transform :accessor transform :initarg :transform :initform (make-instance 'transform))
   (show-axis :accessor show-axis :initarg :show-axis :initform nil) ;nil or length
   (show-bounds? :accessor show-bounds? :initarg :show-bounds? :initform nil)))

(defmethod copy-instance-data :after ((dst shape) (src shape))
  ;; TODO - name not copied - always generate new name?
  (copy-instance-data (transform dst) (transform src))
  (setf (show-axis dst) (show-axis src))
  (setf (show-bounds? dst) (show-bounds? src)))

;;; utility methods for transforming shapes
(defmethod translate-by ((self shape) (p point))
  (translate-by (transform self) p)
  self)

(defmethod rotate-by ((self shape) (p point))
  (rotate-by (transform self) p)
  self)

(defmethod scale-by ((self shape) (p point))
  (scale-by (transform self) p)
  self)

(defmethod scale-by ((self shape) (s number))
  (scale-by (transform self) s)
  self)

(defmethod translate-to ((self shape) (p point))
  (translate-to (transform self) p)
  self)

(defmethod rotate-to ((self shape) (p point))
  (rotate-to (transform self) p)
  self)

(defmethod scale-to ((self shape) (p point))
  (scale-to (transform self) p)
  self)

(defmethod scale-to ((self shape) (s number))
  (scale-to (transform self) s)
  self)

(defmethod reset-transform ((self shape))
  (reset-transform (transform self))
  self)

(defmethod get-bounds ((self shape))
  (warn "Shape ~a does not have GET-BOUNDS defined. Using default bounds values." self)
  (values (p! -1 -1 -1) (p! 1 1 1)))

(defmethod center-at-origin ((self shape))
  (multiple-value-bind (bounds-lo bounds-hi center)
      (bounds-and-center self)
    (declare (ignore bounds-lo bounds-hi))
    (when center
      (translate-to self (p-negate (p* center (scale (transform self))))))))

(defmethod scale-to-size ((self shape) max-size)
  (multiple-value-bind (bounds-lo bounds-hi)
      (get-bounds self)
    (when (and bounds-lo bounds-hi)
      (let* ((size (max (abs (- (x bounds-hi) (x bounds-lo)))
                        (abs (- (y bounds-hi) (y bounds-lo)))
                        (abs (- (z bounds-hi) (z bounds-lo)))))
             (scale (if (= size 0)
                        1.0
                        (/ max-size size))))
        (scale-to self (p! scale scale scale))))))

