(in-package #:kons-9)

;;;; scene-item ================================================================

(defparameter *scene-item-counter* 0)

(defclass scene-item ()
  ((name :accessor name :initarg :name :initform nil)
   (scene :accessor scene :initarg :scene :initform nil)
   (is-selected? :accessor is-selected? :initarg :is-selected? :initform nil)))

(defmethod initialize-instance :after ((item scene-item) &rest initargs)
  (declare (ignore initargs))
  (when (null (name item))
    (setf (name item) (mashup-symbol (class-name (class-of item)) '- (incf *scene-item-counter*)))))

(defmethod print-object ((self scene-item) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a" (name self))))

(defmethod copy-instance-data ((dst scene-item) (src scene-item))
  ;; TODO - name not copied - always generate new name?
  )

(defmethod string-name ((item scene-item))
  (format nil "~a" (if (name item) (name item) "- no name -")))

(defmethod select ((item scene-item))
  (setf (is-selected? item) t))
  ;; (when (scene item)
  ;;   (add-selection (scene item) item)))

(defmethod unselect ((item scene-item))
  (setf (is-selected? item) nil))
  ;; (when (scene item)
  ;;   (remove-selection (scene item) item)))

(defmethod toggle-select ((item scene-item))
  (if (is-selected? item)
      (unselect item)
      (select item)))
