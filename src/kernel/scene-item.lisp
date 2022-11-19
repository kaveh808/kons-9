(in-package #:kons-9)

;;;; scene-item name generation util ===========================================

(defparameter *scene-item-name-table* (make-hash-table :test 'eq))

(defun next-scene-item-type-number (type)
  (let ((num (gethash type *scene-item-name-table*)))
    (if (null num)
        (progn
          (setf (gethash type *scene-item-name-table*) 2)
          1)
        (progn
          (incf (gethash type *scene-item-name-table*))
          num))))

(defun build-scene-item-name (base-name)
  (concat-syms base-name '- (next-scene-item-type-number base-name)))

;;;; scene-item ================================================================

(defclass scene-item (item)
  ((name :accessor name :initarg :name :initform nil)
   (scene :accessor scene :initarg :scene :initform nil)
   (parents :accessor parents :initarg :parents :initform '())
   (is-selected? :accessor is-selected? :initarg :is-selected? :initform nil)
   (editable-slots :accessor editable-slots :initarg :editable-slots :initform '() :allocation :class)))

(defmethod initialize-instance :after ((item scene-item) &rest initargs)
  (declare (ignore initargs))
  (when (null (name item))
    (let ((base-name (class-name (class-of item))))
      (setf (name item) (build-scene-item-name base-name)))))

(defmethod printable-data ((self scene-item))
  (format nil "~a" (name self)))

