(in-package #:kons-9)

;;;; interactor ====================================================================

(defclass interactor (item)
  ((setup-fn :accessor setup-fn :initarg :setup-fn :initform nil)
   (update-fn :accessor update-fn :initarg :update-fn :initform nil)
   (setup-done? :accessor setup-done? :initarg :setup-done? :initform nil)))

;; (defmethod printable-data ((self interactor))
;;   (strcat (call-next-method) (format nil ", timing = ~a ~a" (start-time self) (duration self))))

(defmethod setup-interactor ((interactor interactor))
  (when (setup-fn interactor)
    (funcall (setup-fn interactor)))
  (setf (setup-done? interactor) t))

(defmethod update-interactor ((interactor interactor) key key-mods)
  (when (not (setup-done? interactor))
    (setup-interactor interactor))
  (when (update-fn interactor)
    (funcall (update-fn interactor) key key-mods)))

