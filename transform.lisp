;;;; transform ==========================================================

(defclass transform ()
  ((translate :accessor translate :initarg :translate :initform (p! 0.0 0.0 0.0))
   (rotate :accessor rotate :initarg :rotate :initform (p! 0.0 0.0 0.0))
   (scale :accessor scale :initarg :scale :initform (p! 1.0 1.0 1.0))))

(defmethod copy-instance-data ((dst transform) (src transform))
  (setf (translate dst) (p-copy (translate src)))
  (setf (rotate    dst) (p-copy (rotate    src)))
  (setf (scale     dst) (p-copy (scale     src))))

(defun make-transform (translate rotate scale)
  (make-instance 'transform :translate translate :rotate rotate :scale scale))

(defmethod duplicate-transform ((self transform))
  (let ((new-transform (make-instance 'transform)))
    (copy-instance-data new-transform self)
    new-transform))

(defmethod translate-by ((self transform) (p point))
  (setf (translate self) (p+ (translate self) p)))

(defmethod rotate-by ((self transform) (p point))
  (setf (rotate self) (p+ (rotate self) p)))

(defmethod scale-by ((self transform) (p point))
  (setf (scale self) (p* (scale self) p)))

(defmethod translate-to ((self transform) (p point))
  (setf (translate self) p))

(defmethod rotate-to ((self transform) (p point))
  (setf (rotate self) p))

(defmethod scale-to ((self transform) (p point))
  (setf (scale self) p))

(defmethod reset-transform ((self transform))
  (setf (translate self) (p! 0.0 0.0 0.0))
  (setf (rotate self) (p! 0.0 0.0 0.0))
  (setf (scale self) (p! 1.0 1.0 1.0)))

(defmethod print-object ((self transform) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":TRANSLATE ~a :ROTATE ~a :SCALE ~a"
	    (translate self) (rotate self) (scale self))))

(defmethod partial-translate ((self transform) factor)
  (p-scale (translate self) factor))

(defmethod partial-rotate ((self transform) factor)
  (p-scale (rotate self) factor))

(defmethod partial-scale ((self transform) factor)
  (p-lerp factor (p! 1.0 1.0 1.0) (scale self)))

(defmethod partial-copy ((dst transform) (src transform) factor)
  (setf (translate dst) (partial-translate src factor))
  (setf (rotate dst) (partial-rotate src factor))
  (setf (scale dst) (partial-scale src factor)))
  
;;; fixed scale/rotate/translate order for now - add options later
(defmethod transform-matrix ((self transform) &optional (factor 1.0))
  (let ((t-mtx (make-translation-matrix (partial-translate self factor)))
	(r-mtx (make-rotation-matrix (partial-rotate self factor)))
	(s-mtx (make-scale-matrix (partial-scale self factor))))
    (matrix-multiply-n s-mtx r-mtx t-mtx)))

