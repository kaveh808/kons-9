(in-package #:kons-9)

;;;; transform-operator classes ================================================

(defclass transform-operator (item)
  ())

(defclass translate-operator (transform-operator)
  ((offset :accessor offset :initarg :offset :initform (p! 0.0 0.0 0.0))))

(defmethod transform-matrix ((self translate-operator) &optional (factor 1.0))
  (make-translation-matrix (p:lerp (p! 0.0 0.0 0.0) (offset self) factor)))

(defclass euler-rotate-operator (transform-operator)
  ((angles :accessor angles :initarg :angles :initform (p! 0.0 0.0 0.0)) ;degrees
   ;; :xyz :xzy :yxz :yzx :zxy :zyx
   (rotate-order :accessor rotate-order :initarg :rotate-order :initform :xyz)
   (pivot :accessor pivot :initarg :pivot :initform (p! 0.0 0.0 0.0))))


(defmethod transform-matrix ((self euler-rotate-operator) &optional (factor 1.0))
  (make-rotation-matrix (p:radians (p:lerp (p! 0.0 0.0 0.0) (angles self) factor)) ;convert to radians
                        (rotate-order self)
                        (pivot self)))

(defclass angle-axis-rotate-operator (transform-operator)
  ((angle :accessor angle :initarg :angle :initform 0.0) ;degrees
   (axis :accessor axis :initarg :axis :initform (p! 0.0 0.0 1.0))
   (pivot :accessor pivot :initarg :pivot :initform (p! 0.0 0.0 0.0))))

(defmethod transform-matrix ((self angle-axis-rotate-operator) &optional (factor 1.0))
  (make-axis-rotation-matrix (radians (lerp factor 0.0 (angle self))) ;convert to radians
                             (axis self)
                             (pivot self)))

;; TODO: scale pivot
(defclass scale-operator (transform-operator)
  ((scaling :accessor scaling :initarg :scaling :initform (p! 1.0 1.0 1.0))
   (pivot :accessor pivot :initarg :pivot :initform (p! 0.0 0.0 0.0))))

(defmethod transform-matrix ((self scale-operator) &optional (factor 1.0))
  (make-scale-matrix (p:lerp (p! 1.0 1.0 1.0) (scaling self) factor)
                     (pivot self)))

;;; transform ==================================================================

;;; abstract superclass
(defclass transform (item)
  ;; :trs :tsr :rts :rst :str :srt
  ((operator-order :accessor operator-order :initarg :operator-order :initform :srt)))


;;; euler-transform ============================================================

(defclass euler-transform (transform)
  ((translate :accessor translate :initarg :translate :initform (make-instance 'translate-operator))
   (rotate :accessor rotate :initarg :rotate :initform (make-instance 'euler-rotate-operator))
   (scale :accessor scale :initarg :scale :initform (make-instance 'scale-operator))))

(defmethod transform-matrix ((self euler-transform) &optional (factor 1.0))
  (let ((t-mtx (transform-matrix (translate self) factor))
	(r-mtx (transform-matrix (rotate self) factor))
	(s-mtx (transform-matrix (scale self) factor)))
    (case (operator-order self)
      (:trs (matrix-multiply-n t-mtx r-mtx s-mtx))
      (:tsr (matrix-multiply-n t-mtx s-mtx r-mtx))
      (:rts (matrix-multiply-n r-mtx t-mtx s-mtx))
      (:rst (matrix-multiply-n r-mtx s-mtx t-mtx))
      (:str (matrix-multiply-n s-mtx t-mtx r-mtx))
      (:srt (matrix-multiply-n s-mtx r-mtx t-mtx))
      (otherwise (error "Unknown rotate order ~a in TRANSFORM-MATRIX" (operator-order self))))))

(defmethod translate-by ((self euler-transform) point-or-number)
  (setf (offset (translate self)) (p+ (offset (translate self)) point-or-number)))

(defmethod rotate-by ((self euler-transform) point-or-number)
  (setf (angles (rotate self)) (p+ (angles (rotate self)) point-or-number)))

(defmethod scale-by ((self euler-transform) point-or-number)
  (setf (scaling (scale self)) (p* (scaling (scale self)) point-or-number)))

(defmethod translate-to ((self euler-transform) p)
  (setf (offset (translate self)) p))

(defmethod rotate-to ((self euler-transform) p)
  (setf (angles (rotate self)) p))

(defmethod scale-to ((self euler-transform) p)
  (setf (scaling (scale self)) p)) 

(defmethod scale-to ((self euler-transform) (s number))
    (setf (scaling (scale self)) (p! s s s)))

(defun make-euler-transform (translate rotate scale)
  (let ((xform (make-instance 'euler-transform)))
    (translate-to xform translate)
    (rotate-to xform rotate)
    (scale-to xform scale)
    xform))

(defmethod reset-transform ((self euler-transform))
  (setf (offset (translate self)) (p! 0.0 0.0 0.0))
  (setf (angles (rotate self)) (p! 0.0 0.0 0.0))
  (setf (scaling (scale self)) (p! 1.0 1.0 1.0)))

(defmethod partial-translate ((self euler-transform) factor)
  (p:scale (offset (translate self)) factor))

(defmethod partial-rotate ((self euler-transform) factor)
  (p:scale (angles (rotate self)) factor))

(defmethod partial-scale ((self euler-transform) factor)
  (p:lerp (p! 1.0 1.0 1.0) (scaling (scale self)) factor))

(defmethod partial-copy ((dst euler-transform) (src euler-transform) factor)
  (setf (offset (translate dst)) (partial-translate src factor)
        (angles (rotate dst))    (partial-rotate src factor)
        (scaling (scale dst))     (partial-scale src factor)))

(defmethod print-object ((self euler-transform) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":TRANSLATE ~a :ROTATE ~a :SCALE ~a"
	    (offset (translate self)) (angles (rotate self)) (scaling (scale self)))))

;;; angle-axis-transform =======================================================

(defclass angle-axis-transform (transform)
  ((translate :accessor translate :initarg :translate :initform (make-instance 'translate-operator))
   (rotate :accessor rotate :initarg :rotate :initform (make-instance 'angle-axis-rotate-operator))
   (scale :accessor scale :initarg :scale :initform (make-instance 'scale-operator))))

(defmethod transform-matrix ((self angle-axis-transform) &optional (factor 1.0))
  (let ((t-mtx (transform-matrix (translate self) factor))
	(r-mtx (transform-matrix (rotate self) factor))
	(s-mtx (transform-matrix (scale self) factor)))
    (case (operator-order self)
      (:trs (matrix-multiply-n t-mtx r-mtx s-mtx))
      (:tsr (matrix-multiply-n t-mtx s-mtx r-mtx))
      (:rts (matrix-multiply-n r-mtx t-mtx s-mtx))
      (:rst (matrix-multiply-n r-mtx s-mtx t-mtx))
      (:str (matrix-multiply-n s-mtx t-mtx r-mtx))
      (:srt (matrix-multiply-n s-mtx r-mtx t-mtx))
      (otherwise (error "Unknown operator order ~a in TRANSFORM-MATRIX" (operator-order self))))))


(defmethod translate-by ((self angle-axis-transform) point-or-number)
  (setf (offset (translate self)) (p:+ (offset (translate self)) point-or-number)))

(defmethod rotate-by ((self angle-axis-transform) a)
  (setf (angle (rotate self)) (+ (angle (rotate self)) a)))

(defmethod scale-by ((self angle-axis-transform) p)
  (setf (scaling (scale self)) (p:* (scaling (scale self)) p)))

(defmethod scale-by ((self angle-axis-transform) (s number))
  (setf (scaling (scale self)) (p* (scaling (scale self)) s)))

(defmethod translate-to ((self angle-axis-transform) p)
  (setf (offset (translate self)) p))

(defmethod rotate-to ((self angle-axis-transform) a)
  (setf (angle (rotate self)) a))

(defmethod scale-to ((self angle-axis-transform) p)
  (setf (scaling (scale self)) p))

(defmethod scale-to ((self angle-axis-transform) (s number))
  (setf (scaling (scale self)) (p! s s s)))

(defun make-axis-angle-transform (translate angle axis scale)
  (let ((xform (make-instance 'angle-axis-transform)))
    (translate-to xform translate)
    (rotate-to xform angle)
    (setf (axis (rotate xform)) axis)
    (scale-to xform scale)
    xform))

(defmethod reset-transform ((self angle-axis-transform))
  (setf (offset (translate self)) (p! 0.0 0.0 0.0))
  (setf (angle (rotate self)) 0.0)
  (setf (axis (rotate self)) (p! 0.0 0.0 1.0))
  (setf (scaling (scale self)) (p! 1.0 1.0 1.0)))

(defmethod partial-translate ((self angle-axis-transform) factor)
  (p:scale    (offset (translate self)) factor))

(defmethod partial-rotate ((self angle-axis-transform) factor)
  (* (angle (rotate self)) factor))

(defmethod partial-scale ((self angle-axis-transform) factor)
  (p:lerp (p! 1.0 1.0 1.0) (scaling (scale self)) factor))

(defmethod partial-copy ((dst angle-axis-transform) (src angle-axis-transform) factor)
  (setf (offset (translate dst)) (partial-translate src factor))
  (setf (angle (rotate dst)) (partial-rotate src factor))
  (setf (axis (rotate dst)) (axis (rotate src)))
  (setf (scaling (scale dst)) (partial-scale src factor)))

(defmethod print-object ((self angle-axis-transform) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream ":TRANSLATE ~a :ROTATE ~a, ~a :SCALE ~a"
	    (offset (translate self)) (angle (rotate self)) (axis (rotate self)) (scaling (scale self)))))

;;; generalized-transform ======================================================

(defclass generalized-transform (transform)
  ((operators :accessor operators :initarg :operators :initform '())))

;; TODO: reverse mtx-list for correct transform order -- needs testing
(defmethod transform-matrix ((self generalized-transform) &optional (factor 1.0))
  (if (> (length (operators self)) 0)
      (let ((mtx-list (nreverse (mapcar (lambda (op) (transform-matrix op factor))
                                        (operators self)))))
        (apply #'matrix-multiply-n mtx-list))
      (make-id-matrix)))

