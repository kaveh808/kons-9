(in-package #:kons-9)

;;;; group =====================================================================

;;; abstract class for managing hierarchies of shapes
(defclass-kons-9 group-mixin ()
  ((children '())))

(defmethod printable-data ((self group-mixin))
  (strcat (call-next-method) (format nil ", ~a children" (length (children self)))))

(defmethod add-child ((group group-mixin) (item scene-item))
  (push item (children group))
  (when (scene group)
    (set-item-scene (scene group) item))
  group)

(defmethod add-child-at-end ((group group-mixin) (item scene-item))
  (setf (children group) (append (children group) (list item)))
  (when (scene group)
    (set-item-scene (scene group) item))
  group)

(defmethod remove-child ((group group-mixin) (item scene-item))
  (setf (children group) (remove item (children group)))
  group)

(defmethod set-children ((group group-mixin) scene-items)
  (setf (children group) scene-items)
  group)

(defmethod remove-all-children ((group group-mixin))
  (setf (children group) '())
  group)

