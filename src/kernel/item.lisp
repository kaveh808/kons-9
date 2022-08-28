(in-package #:kons-9)

;;;; item name generation util =================================================

(defparameter *item-name-table* (make-hash-table :test 'eq))

(defun next-item-type-number (type)
  (let ((num (gethash type *item-name-table*)))
    (if (null num)
        (progn
          (setf (gethash type *item-name-table*) 2)
          1)
        (progn
          (incf (gethash type *item-name-table*))
          num))))

;;;; item ======================================================================

;;; root class for scene entities
(defclass item ()
  ((name :accessor name :initarg :name :initform nil)))

(defparameter *global-item-counter* 0)

(defmethod initialize-instance :after ((item item) &rest initargs)
  (declare (ignore initargs))
  (when (null (name item))
    (let ((base-name (class-name (class-of item))))
      (setf (name item) (mashup-symbol base-name '- (next-item-type-number base-name))))))

(defmethod print-object ((self item) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream (printable-data self))))

(defmethod printable-data ((self item))
  (format nil "~a" (name self)))
