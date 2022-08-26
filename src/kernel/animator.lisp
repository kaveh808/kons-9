(in-package #:kons-9)

;;;; animator ==================================================================

(defclass animator (motion dependency-node-mixin)
  ((setup-fn :accessor setup-fn :initarg :setup-fn :initform nil)
   (update-fn :accessor update-fn :initarg :update-fn :initform nil)
   (setup-done? :accessor setup-done? :initarg :setup-done? :initform nil)))

(defmethod initialize-instance :after ((anim animator) &rest initargs)
  (declare (ignore initargs))
  (setf (is-dirty? anim) nil))          ;nil by default as called explicitly each frame

(defmethod setup-motion ((anim animator))
  (when (setup-fn anim)
    (funcall (setup-fn anim))))

(defmethod setup-motion :after ((anim animator))
  (setf (setup-done? anim) t))

(defmethod update-motion ((anim animator) parent-absolute-timing)
  (when (in-time-interval? anim parent-absolute-timing)
    (when (not (setup-done? anim))
      (setup-motion anim))
    (when (update-fn anim)
      (funcall (update-fn anim)))))

(defmethod update-motion :after ((anim animator) parent-absolute-timing)
  (declare (ignore parent-absolute-timing))
  (setf (time-stamp anim) (get-internal-real-time)))

;;;; shape-animator ============================================================

(defun get-alist-value (key alist)
  (cdr (assoc key alist)))

(defun add-alist-value (key value alist)
  (acons key value alist))

(defun set-alist-value (key value alist)
  (let ((pair (assoc key alist)))
    (if pair
	(rplacd pair value)
	(error "Key ~a not found in alist ~a~%" key alist)))
  alist)

(defclass shape-animator (animator)
  ((shape :accessor shape :initarg :shape :initform nil)
   (data :accessor data :initarg :data :initform '())))

(defmethod anim-data ((anim shape-animator) key)
  (get-alist-value key (data anim)))

(defmethod setup-motion ((anim shape-animator))
  (when (setup-fn anim)
    (funcall (setup-fn anim) anim)))

(defmethod update-motion ((anim animator) parent-absolute-timing)
  (let ((timing (compute-motion-absolute-timing anim parent-absolute-timing)))
    (when (in-time-interval? anim timing)
      (when (not (setup-done? anim))
        (setup-motion anim))
      (when (update-fn anim)
        (funcall (update-fn anim) anim)))))

