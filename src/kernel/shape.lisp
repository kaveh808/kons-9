(in-package #:kons-9)
(declaim (optimize debug))
;;;; shape =====================================================================

(defclass shape (scene-item)
  ((transform :accessor transform :initarg :transform :initform (make-instance 'euler-transform))
   (is-visible? :accessor is-visible? :initarg :is-visible? :initform t)
   (show-axis :accessor show-axis :initarg :show-axis :initform nil) ;nil or length
   (show-bounds? :accessor show-bounds? :initarg :show-bounds? :initform nil)))

;;; utility methods for transforming shapes

(defmethod translate-by ((self shape) p)
  (translate-by (transform self) p)
  self)

(defmethod rotate-by ((self shape) p)
  (rotate-by (transform self) p)
  self)

(defmethod scale-by ((self shape) p)
  (scale-by (transform self) p)
  self)

(defmethod translate-to ((self shape) p)
  (translate-to (transform self) p)
  self)

(defmethod rotate-to ((self shape) p)
  (rotate-to (transform self) p)
  self)

(defmethod scale-to ((self shape) p)
  (scale-to (transform self) p)
  self)

(defmethod scale-by ((self shape) (s number))
  (scale-by (transform self) s)
  self)

(defmethod reset-transform ((self shape))
  (reset-transform (transform self))
  self)

(defmethod get-bounds ((self shape))
  (warn "Shape ~a does not have GET-BOUNDS defined. Using default bounds values." self)
  (values (p! -1 -1 -1) (p! 1 1 1)))

(defmethod center-at-origin ((self shape))
  (multiple-value-bind (bounds-lo bounds-hi)
      (get-bounds self)
    (when (and bounds-lo bounds-hi)
      (let ((center (p-average bounds-lo bounds-hi)))
        (translate-to self (p:negate (p:* center (scaling (scale (transform self))))))))))

(defmethod scale-to-size ((self shape) max-size)
  (multiple-value-bind (bounds-lo bounds-hi)
      (get-bounds self)
    (when (and bounds-lo bounds-hi)
      (let* ((size (max (abs (- (p:x bounds-hi) (p:x bounds-lo)))
                        (abs (- (p:y bounds-hi) (p:y bounds-lo)))
                        (abs (- (p:z bounds-hi) (p:z bounds-lo)))))
             (scale (if (= size 0)
                        1.0
                        (/ max-size size))))
        (scale-to self (p! scale scale scale))))))

