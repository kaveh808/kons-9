(in-package #:kons-9)

;;;; animator ==================================================================

(defclass animator (scene-item dependency-node-mixin)
  ((init-fn :accessor init-fn :initarg :init-fn :initform nil)
   (update-fn :accessor update-fn :initarg :update-fn :initform nil)
   (init-args :accessor init-args :initarg :init-args :initform '())
   (is-initialized? :accessor is-initialized? :initarg :is-initialized? :initform nil)
   (shape :accessor shape :initarg :shape :initform nil)
   (data :accessor data :initarg :data :initform '())))

(defmethod initialize-instance :after ((anim animator) &rest initargs)
  (declare (ignore initargs))
  (setf (is-dirty? anim) nil))          ;nil by default as called explicitly each frame

(defmethod copy-instance-data ((dst animator) (src animator))
  (setf (init-fn dst) (init-fn src))
  (setf (update-fn dst) (update-fn src))
  (setf (init-args dst) (init-args src))
  (setf (is-initialized? dst) (is-initialized? src))
;;   (setf (shape dst) (shape src)) -- do not copy shape
  (setf (data dst) (copy-list (data src)))
  dst)

(defmethod duplicate-animator ((anim animator))
  (let ((new-anim (make-instance (type-of anim))))
    (copy-instance-data new-anim anim)
    new-anim))

(defmethod init-animator ((anim animator))
  (when (init-fn anim)
    (funcall (init-fn anim) anim)))

(defmethod init-animator :after ((anim animator))
  (setf (is-initialized? anim) t))

(defmethod update-animator :before ((anim animator))
  (when (not (is-initialized? anim))
    (init-animator anim)))

(defmethod update-animator ((anim animator))
  (when (update-fn anim)
    (funcall (update-fn anim) anim)))

(defmethod update-animator :after ((anim animator))
  (setf (time-stamp anim) (get-internal-real-time)))

