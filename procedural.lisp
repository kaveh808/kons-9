;;;; procedural-mixin ======================================================

(defgeneric compute-procedural-node (obj)
  )

(defgeneric is-dirty? (obj)
  )

(defmethod is-dirty? ((obj t))
  nil)

(defclass procedural-mixin ()
  ((is-dirty? :accessor is-dirty? :initarg :is-dirty? :initform t)
   (test-for-compute? :accessor test-for-compute? :initarg :test-for-compute? :initform t)
   (compute-fn :accessor compute-fn :initarg :compute-fn :initform #'compute-procedural-node)))

(defmethod compute-procedural-node ((self procedural-mixin))
  (error "COMPUTE-PROCEDURAL-NODE not implented for object ~a" self))

(defmethod needs-compute? ((self procedural-mixin)  &optional (verbose nil))
  (when verbose
    (format t  "NEEDS-COMPUTE? ~a ~a ~a: ~a~%" self (test-for-compute? self) (is-dirty? self)
            (and (test-for-compute? self) (is-dirty? self))))
  (and (test-for-compute? self) (is-dirty? self)))

(defmacro without-testing-for-compute (obj &rest body)
  `(progn
     (setf (test-for-compute? ,obj) nil)
     (let ((_result (progn ,@body)))
       (setf (test-for-compute? ,obj) t)
       _result)))

(defmacro def-procedural-input (class slot)
  `(defmethod (setf ,slot) :after (value (obj ,class))
     (declare (ignore value))
     (setf (is-dirty? obj) t)))

(defmacro def-procedural-output (class slot)
  `(defmethod ,slot :before ((obj ,class))
     ;; (format t "GET ~a ~a~%" ',slot obj)
     (when (needs-compute? obj)
       (without-testing-for-compute obj
;       (compute-procedural-node obj)
         (funcall (compute-fn obj) obj))
       (setf (is-dirty? obj) nil))))

;;;; dependency-node-mixin ===============================================

(defgeneric time-stamp (obj))
(defmethod time-stamp ((obj t)) 0)

(defclass dependency-node-mixin (procedural-mixin)
  ((input-slots :accessor input-slots :initarg :input-slots :initform '()) ;must be of type dependency-node-mixin
   (time-stamp :accessor time-stamp :initarg :time-stamp :initform 0)))

(defmacro def-dependency-output (class slot)
  `(defmethod ,slot :after ((obj ,class))
     (setf (time-stamp obj) (get-internal-real-time))))

(defmethod needs-compute? ((node dependency-node-mixin) &optional (verbose nil))
  (when verbose
    (format t  "NEEDS-COMPUTE? ~a dt: ~a, test: ~a, dirty: ~a, dirty inputs: ~a -- ~a~%"
            node (- (time-stamp node) (inputs-time-stamp node))
            (test-for-compute? node) (is-dirty? node) (has-dirty-input? node)
            (and (test-for-compute? node)
                 (or (is-dirty? node)
                     (= 0 (time-stamp node))
                     (> (inputs-time-stamp node) (time-stamp node))))))
  (and (test-for-compute? node)
       (or (is-dirty? node)             ;procedural-mixin input changed
           (has-dirty-input? node)
           (= 0 (time-stamp node))      ;node never computed
           (> (inputs-time-stamp node) (time-stamp node))))) ;dependency-node-mixin input changed

(defmethod inputs-time-stamp ((node dependency-node-mixin))
  (if (input-slots node)
      (apply #'max (mapcar #'(lambda (input) (time-stamp (slot-value node input))) (input-slots node)))
      0))

(defmethod has-dirty-input? ((node dependency-node-mixin))
  (dolist (input (input-slots node))
    (when (is-dirty? (slot-value node input))
      (return-from has-dirty-input? t)))
  nil)

;;;; procedural-curve-shape ================================================

(defclass procedural-curve-shape (curve-shape dependency-node-mixin) ;procedural-mixin)
  ((num-points :accessor num-points :initarg :num-points :initform 64)))

(def-procedural-input procedural-curve-shape num-points)
(def-procedural-output procedural-curve-shape points) ;can we unify this...
(def-dependency-output procedural-curve-shape points) ; ...and this?

;;;; circle-shape ==========================================================

(defclass circle-shape (procedural-curve-shape)
  ((diameter :accessor diameter :initarg :diameter :initform 2.0)))

(def-procedural-input circle-shape diameter)

(defmethod compute-procedural-node ((shape circle-shape))
  (with-accessors ((d diameter) (n num-points))
    shape
    (let ((points '())
          (radius (/ d 2.0))
          (angle-delta (/ 2pi n)))
      (dotimes (i n)
        (let ((angle (* i angle-delta)))
          (push (p! (* (sin angle) radius) (* (cos angle) radius) 0)
                points)))
      (setf (points shape) points)
      shape)))

(defun make-circle-shape (diameter &optional (num-points 64))
  (make-instance 'circle-shape :diameter diameter :num-points num-points))

;;;; sine-curve-shape =====================================================

(defclass sine-curve-shape (procedural-curve-shape)
  ((period :accessor period :initarg :period :initform 2pi)
   (frequency :accessor frequency :initarg :frequency :initform 1.0)
   (x-scale :accessor x-scale :initarg :x-scale :initform 1.0)
   (y-scale :accessor y-scale :initarg :y-scale :initform 1.0))
  (:default-initargs
   :is-closed-shape? nil))

(def-procedural-input sine-curve-shape period)
(def-procedural-input sine-curve-shape frequency)
(def-procedural-input sine-curve-shape x-scale)
(def-procedural-input sine-curve-shape y-scale)

(defmethod compute-procedural-node ((shape sine-curve-shape))
  (with-accessors ((period period) (frequency frequency) (x-scale x-scale) (y-scale y-scale) (n num-points))
      shape
    (let ((points '())
          (angle-delta (/ period n)))
      (dotimes (i (1+ n))
        (let ((angle (* i angle-delta frequency)))
          (push (p! (* x-scale (/ angle (* frequency period))) (* y-scale (sin angle)) 0)
                points)))
      (setf (points shape) (nreverse points))
      shape)))

(defun make-sine-curve-shape (x-scale y-scale &optional (num-points 64))
  (make-instance 'sine-curve-shape :x-scale x-scale :y-scale y-scale :num-points num-points))

;;; square-shape ==========================================================
;; (defun make-square-shape (length)
;;   (let ((v (/ length 2.0)))
;;     (make-instance 'curve-shape
;; 		   :points (list (p!    v     v  0)
;; 				 (p!    v  (- v) 0)
;; 				 (p! (- v) (- v) 0)
;; 				 (p! (- v)    v  0)))))



;;;; manager-group ======================================================

(defclass manager-group (group dependency-node-mixin)
  ())

;;; these must always be together?
(def-procedural-output manager-group children)
(def-dependency-output manager-group children)

;;;; instancer-group ====================================================

(defclass instancer-group (manager-group)
  ((instance-shape :accessor instance-shape :initarg :instance-shape :initform nil)))

(defmethod initialize-instance :after ((self instancer-group) &rest initargs)
  (declare (ignore initargs))
  (push 'instance-shape (input-slots self)))

(def-procedural-input instancer-group instance-shape)

;;;; point-instancer ====================================================

(defclass point-instancer (instancer-group)
  ((point-generator :accessor point-generator :initarg :point-generator :initform nil)))

(defmethod initialize-instance :after ((self point-instancer) &rest initargs)
  (declare (ignore initargs))
  (push 'point-generator (input-slots self)))

(def-procedural-input point-instancer point-generator)

;;; todo -- align with normals/directions (shape axis option)
(defmethod compute-procedural-node ((self point-instancer))
  (remove-all-children self)
  (let ((points (point-generator-points (point-generator self))))
    (dotimes (i (length points))
      (add-child self (translate-to (make-group (instance-shape self)) (aref points i))))))

(defun make-point-instancer (p-gen instance-shape)
  (make-instance 'point-instancer :point-generator p-gen :instance-shape instance-shape))

;;;; transform-instancer ================================================

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
             (instance-group (make-group (instance-shape self))))
        (partial-copy (transform instance-group) (instance-transform self) factor)
        (add-child self instance-group)))))

(defun make-transform-instancer (shape transform steps)
  (make-instance 'transform-instancer :instance-shape shape :instance-transform transform :num-steps steps))

