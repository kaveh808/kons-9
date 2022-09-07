(in-package #:kons-9)

;;;; manager-group =============================================================

(defclass manager-group (group dependency-node-mixin)
  ())

(def-procedural-output manager-group children)

;;;; instancer-group ===========================================================

(defclass instancer-group (manager-group)
  ((instance-shape :accessor instance-shape :initarg :instance-shape :initform nil)))

(defmethod initialize-instance :after ((self instancer-group) &rest initargs)
  (declare (ignore initargs))
  (push 'instance-shape (input-slots self)))

(def-procedural-input instancer-group instance-shape)

;;;; point-instancer ===========================================================

(defclass point-instancer (instancer-group)
  ((point-generator :accessor point-generator :initarg :point-generator :initform nil)))

(defmethod initialize-instance :after ((self point-instancer) &rest initargs)
  (declare (ignore initargs))
  (push 'point-generator (input-slots self)))

(def-procedural-input point-instancer point-generator)

;;; todo -- align with normals/directions (shape axis option)
(defmethod compute-procedural-node ((self point-instancer))
  (remove-all-children self)
  (let ((points (source-points (point-generator self))))
    (dotimes (i (length points))
      (add-child self (translate-to (make-group (list (instance-shape self))) (aref points i))))))

(defun make-point-instancer (p-gen instance-shape)
  (make-instance 'point-instancer :point-generator p-gen :instance-shape instance-shape))

;;;; transform-instancer =======================================================

(defclass transform-instancer (instancer-group)
  ((instance-transform :accessor instance-transform :initarg :instance-transform :initform nil)
   (num-steps :accessor num-steps :initarg :num-steps :initform 10)))

(def-procedural-input transform-instancer instance-transform)
(def-procedural-input transform-instancer num-steps)

(defmethod compute-procedural-node ((self transform-instancer))
  (remove-all-children self)
  (with-accessors ((steps num-steps))
      self
    (when (< steps 1)
      (return-from compute-procedural-node))
    (dotimes (i (num-steps self))
      (let* ((factor (if (= 1 steps)
                         1.0
                         (tween i 0.0 (1- steps))))
             (instance-group (make-group (list (instance-shape self)))))
        ;; make group transform same type as the instance-transform
        (setf (transform instance-group) (make-instance (type-of (instance-transform self))))
        (partial-copy (transform instance-group) (instance-transform self) factor)
        (add-child self instance-group)))))

(defun make-transform-instancer (shape transform steps)
  (make-instance 'transform-instancer :instance-shape shape :instance-transform transform :num-steps steps))

;;;; variant-manager-group =====================================================

(defclass variant-manager-group (manager-group)
  ((visible-index :accessor visible-index :initarg :visible-index :initform 0)))

(def-procedural-input variant-manager-group visible-index)

(defmethod compute-procedural-node ((self variant-manager-group))
  (dolist (child (children self))
    (setf (is-visible? child) nil))
  (setf (is-visible? (nth (visible-index self) (children self))) t))

