(in-package #:kons-9)

;;;; scene-item ================================================================

(defclass scene-item (item)
  ((scene :accessor scene :initarg :scene :initform nil)
   (is-selected? :accessor is-selected? :initarg :is-selected? :initform nil)))

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
