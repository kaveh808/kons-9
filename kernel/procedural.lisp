(in-package #:kons-9)

;;;; procedural-mixin ==========================================================

(defgeneric compute-procedural-node (obj)
  )

(defgeneric is-dirty? (obj))
(defmethod is-dirty? ((obj t)) nil)

(defgeneric time-stamp (obj))
(defmethod time-stamp ((obj t)) 0)

(defclass procedural-mixin ()
  ((is-dirty? :accessor is-dirty? :initarg :is-dirty? :initform t)
   (time-stamp :accessor time-stamp :initarg :time-stamp :initform 0)
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
         (funcall (compute-fn obj) obj)
         (setf (time-stamp obj) (get-internal-real-time)))
       (setf (is-dirty? obj) nil))))

;;;; dependency-node-mixin =====================================================

(defclass dependency-node-mixin (procedural-mixin)
  ((input-slots :accessor input-slots :initarg :input-slots :initform '()))) ;must be of type dependency-node-mixin

(defmethod needs-compute? ((node dependency-node-mixin) &optional (verbose nil))
  (when verbose
    (format t  "NEEDS-COMPUTE? ~a (~a) dt: ~a (~a - ~a), test: ~a, dirty: ~a, dirty inputs: ~a~%"
            node
            (and (test-for-compute? node)
                 (or (is-dirty? node)
                     (has-dirty-input? node)
                     (= 0 (time-stamp node))
                     (> (inputs-time-stamp node) (time-stamp node))))
            (- (time-stamp node) (inputs-time-stamp node))
            (time-stamp node) (inputs-time-stamp node)
            (test-for-compute? node) (is-dirty? node) (has-dirty-input? node)))
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
;;      (format t "  >> dirty input: ~a, value: ~%" input (slot-value node input))    
      (return-from has-dirty-input? t)))
  nil)

