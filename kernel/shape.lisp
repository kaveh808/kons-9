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

(defmethod translate-to ((self shape) (p point))
  (translate-to (transform self) p)
  self)

(defmethod rotate-to ((self shape) (p point))
  (rotate-to (transform self) p)
  self)

(defmethod scale-to ((self shape) (p point))
  (scale-to (transform self) p)
  self)

(defmethod reset-transform ((self shape))
  (reset-transform (transform self))
  self)

(defmethod bounds-and-center ((self shape))
  (warn "Object ~a does not have BOUNDS-AND-CENTER defined. Using default values." self)
  (values nil nil nil))

(defmethod center-at-origin ((self shape))
  (multiple-value-bind (bounds-lo bounds-hi center)
      (bounds-and-center self)
    (declare (ignore bounds-lo bounds-hi))
    (when center
      (translate-to self (p-negate (p* center (scale (transform self))))))))

(defmethod scale-to-size ((self shape) max-size)
  (multiple-value-bind (bounds-lo bounds-hi center)
      (bounds-and-center self)
    (declare (ignore center))
    (when (and bounds-lo bounds-hi)
      (let* ((size (max (abs (- (x bounds-hi) (x bounds-lo)))
                        (abs (- (y bounds-hi) (y bounds-lo)))
                        (abs (- (z bounds-hi) (z bounds-lo)))))
             (scale (if (= size 0)
                        1.0
                        (/ max-size size))))
        (scale-to self (p! scale scale scale))))))

;;; push matrix and do transform operations before drawing
(defmethod draw :before ((self shape))
  (let ((xform (transform self)))
    (3d-push-matrix (translate xform) (rotate xform) (scale xform))))

;;; draw a marker
(defmethod draw ((self shape))
  (3d-draw-marker 0.1))

(defmethod draw-axis ((self shape))
  (3d-draw-axis (show-axis self)))

(defmethod draw-bounds ((self shape) &optional (color (c! 0 1 1)))
  (multiple-value-bind (lo hi center)
      (bounds-and-center self)
    (declare (ignore center))
    (3d-draw-bounds lo hi color)))

(defmethod draw-selected ((self shape))
  (draw-bounds self (c! 1 0 0)))

;;; draw axis and pop matrix after drawing
(defmethod draw :after ((self shape))
  (when (show-axis self)
    (draw-axis self))
  (if (is-selected? self)
      (draw-selected self)
      (when (show-bounds? self)
        (draw-bounds self)))
  (3d-pop-matrix))
