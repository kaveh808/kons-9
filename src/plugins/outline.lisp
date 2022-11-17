(in-package #:kons-9)

;;;; outline ===================================================================

;;;; a shape consisting of multiple curves -- base class for such things as font
;;;; outlines and diagrams

(defclass outline (shape)
  ((curves :accessor curves :initarg :curves :initform (make-array 0 :adjustable t :fill-pointer t))))

(defmethod add-curve ((outline outline) (curve curve))
  (vector-push-extend curve (curves outline)))

(defmethod clear-curves ((outline outline))
  (setf (curves outline) (make-array 0 :adjustable t :fill-pointer t)))

(defmethod get-bounds ((outline outline))
  (when (= 0 (length (curves outline)))
    (warn "OUTLINE ~a does not have any curves. Using default bounds values." outline)
    (return-from get-bounds (values (p! -1 -1 -1) (p! 1 1 1))))
  (let ((lo-list '())
        (hi-list '()))
    (do-array (i curve (curves outline))
      (multiple-value-bind (lo hi)
          (get-bounds curve)
        (push lo lo-list)
        (push hi hi-list)))
    (values (reduce #'p:min lo-list)
            (reduce #'p:max hi-list))))

(defmethod draw ((outline outline))
  (when (is-visible? outline)
    (do-array (i curve (curves outline))
      (draw curve))))
