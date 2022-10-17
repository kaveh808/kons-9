(in-package #:kons-9)

;;;; instancer-shape ===========================================================

(defclass instancer-shape (shape)
  ((instance-shape :accessor instance-shape :initarg :instance-shape :initform nil)))

;;;; point-instancer-shape =====================================================

(defclass point-instancer-shape (instancer-shape)
  ((point-source :accessor point-source :initarg :point-source :initform nil)))

(defmethod draw ((self point-instancer-shape))
  (3d-push-matrix (make-id-matrix))
  (let ((points (source-points (point-source self))))
    (dotimes (i (length points))
      (3d-translate (aref points i))
      (draw (instance-shape self))
      (3d-translate (p:negate (aref points i)))))
  (3d-pop-matrix))

(defun make-point-instancer-shape (p-source instance-shape)
  (make-instance 'point-instancer-shape :point-source p-source :instance-shape instance-shape))

;;;; voxel-grid-shape ==========================================================

(defclass voxel-grid-shape (point-instancer-shape)
  ((boundary-points? :accessor boundary-points? :initarg :boundary-points? :initform t)))

(defun make-voxel-grid (isosurface)
  (let* ((cell-size (iso-field-cell-size isosurface))
         (instance-shape (make-box (p:x cell-size) (p:y cell-size) (p:z cell-size))))
    (make-instance 'voxel-grid-shape :point-source isosurface :instance-shape instance-shape)))
