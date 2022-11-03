(in-package #:kons-9)

;;;; manager-group =============================================================

(defclass manager-group (shape-group dependency-node-mixin)
  ())

(def-procedural-output manager-group children)

;;;; instancer-group ===========================================================

(defclass instancer-group (manager-group)
  ((instance-shape :accessor instance-shape :initarg :instance-shape :initform nil)))

(defmethod initialize-instance :after ((self instancer-group) &rest initargs)
  (declare (ignore initargs))
  (push 'instance-shape (input-slots self)))

(def-procedural-input instancer-group instance-shape)

;;;; point-instancer-group =====================================================

(defclass point-instancer-group (instancer-group)
  ((point-source :accessor point-source :initarg :point-source :initform nil)))

(defmethod initialize-instance :after ((self point-instancer-group) &rest initargs)
  (declare (ignore initargs))
  (push 'point-source (input-slots self)))

(def-procedural-input point-instancer-group point-source)

;;; todo -- align with normals/directions (shape axis option)
(defmethod compute-procedural-node ((self point-instancer-group))
  (remove-all-children self)
  (let ((points (source-points (point-source self))))
    (dotimes (i (length points))
      (add-child self (translate-to (make-shape-group (list (instance-shape self))) (aref points i))))))

(defun make-point-instancer-group (p-source instance-shape)
  (make-instance 'point-instancer-group :point-source p-source :instance-shape instance-shape))

;;;; voxel-grid-group ==========================================================

(defclass voxel-grid-group (point-instancer-group)
  ((boundary-points? :accessor boundary-points? :initarg :boundary-points? :initform t)))

(defun make-voxel-grid-group (isosurface)
  (let* ((cell-size (iso-field-cell-size isosurface))
         (instance-shape (make-box (p:x cell-size) (p:y cell-size) (p:z cell-size))))
    (make-instance 'voxel-grid-group :point-source isosurface :instance-shape instance-shape)))

;;;; transform-instancer-group =================================================

(defclass transform-instancer-group (instancer-group)
  ((instance-transform :accessor instance-transform :initarg :instance-transform :initform nil)
   (num-steps :accessor num-steps :initarg :num-steps :initform 10)))

(def-procedural-input transform-instancer-group instance-transform)
(def-procedural-input transform-instancer-group num-steps)

(defmethod compute-procedural-node ((self transform-instancer-group))
  (remove-all-children self)
  (with-accessors ((steps num-steps))
      self
    (when (< steps 1)
      (return-from compute-procedural-node))
    (dotimes (i (num-steps self))
      (let* ((factor (if (= 1 steps)
                         1.0
                         (tween i 0.0 (1- steps))))
             (instance-group (make-shape-group (list (instance-shape self)))))
        ;; make group transform same type as the instance-transform
        (setf (transform instance-group) (make-instance (type-of (instance-transform self))))
        (partial-copy (transform instance-group) (instance-transform self) factor)
        (add-child self instance-group)))))

(defun make-transform-instancer-group (shape transform steps &key (name nil))
  (make-instance 'transform-instancer-group
                 :name name
                 :instance-shape shape :instance-transform transform :num-steps steps))

;;;; variant-manager-group =====================================================

(defclass variant-manager-group (manager-group)
  ((visible-index :accessor visible-index :initarg :visible-index :initform 0)))

(def-procedural-input variant-manager-group visible-index)

(defmethod compute-procedural-node ((self variant-manager-group))
  (do-children (child self)
    (setf (is-visible? child) nil))
  (setf (is-visible? (aref (children self) (visible-index self))) t))

