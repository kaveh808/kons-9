(in-package #:kons-9)

;;;; utils =====================================================================

(defmacro do-children ((obj group) &rest body)
  (let ((i (gensym))
        (array (gensym)))
    `(let ((,array (children ,group)))
       (dotimes (,i (length ,array))
         (let ((,obj (aref ,array ,i)))
           ,@body)))))

;(pprint (macroexpand-1 '(dochildren (item group) (print item))))

(defmacro map-children (group fn)
  `(map nil ,fn (children ,group)))

;(pprint (macroexpand-1 '(map-children group (lambda (child) (print child)))))

;;;; group-mixin ===============================================================

;;; abstract class for managing hierarchies of shapes
(defclass-kons-9 group-mixin ()
  ((children (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod printable-data ((self group-mixin))
  (strcat (call-next-method) (format nil ", ~a children" (length (children self)))))

(defmethod children-as-list ((group group-mixin))
  (coerce (children group) 'list))

(defmethod add-child ((group group-mixin) (item scene-item))
  (vector-push-extend item (children group))
  (when (scene group)
    (set-item-scene (scene group) item))
  group)

(defmethod remove-child ((group group-mixin) (item scene-item))
  (setf (children group) (delete item (children group)))
  group)

(defmethod set-children ((group group-mixin) scene-items)
  (remove-all-children group)
  (dolist (item scene-items)
    (add-child group item))
  group)

(defmethod remove-all-children ((group group-mixin))
  (setf (children group) (make-array 0 :adjustable t :fill-pointer 0))
  group)

