(in-package #:kons-9)

;;;; clobber customizations ====================================================

;;; write POINT as {x y z}
(defun clobber::serialize-point (point stream)
  (format stream "{~a ~a ~a}" (aref point 0) (aref point 1) (aref point 2)))

;;; add special case for POINT, which is of type '(SIMPLE-ARRAY SINGLE-FLOAT (3))
(defmethod clobber::serialize ((object vector) transaction-log)
  (with-accessors ((object-id-table clobber::object-id-table)
                   (next-object-id clobber::next-object-id)
                   (log-stream clobber::log-stream))
      transaction-log
    (if (typep object '(simple-array single-float (3))) ;special output syntax for points
        (clobber::serialize-point object log-stream)
        (let ((id (gethash object object-id-table)))
          (if (null id)
              (progn (setf id (incf next-object-id))
                     (setf (gethash object object-id-table) id)
                     (format log-stream "#~d!#(" id)
                     (loop for element across object
                           do (clobber::serialize element transaction-log)
                              (format log-stream " "))
                     (format log-stream ")"))
              (format log-stream "#~d^" id))))))

;;; encode data in an optional third entry of SAVE-INFO
;;; - NIL: save slot without putting it in OBJECT-ID-TABLE
;;; - function: save slot using function (not currently used)
(defmethod clobber::serialize ((object standard-object) transaction-log)
  (with-accessors ((object-id-table clobber::object-id-table)
                   (next-object-id clobber::next-object-id)
                   (log-stream clobber::log-stream))
      transaction-log
    (let ((id (gethash object object-id-table)))
      (if (null id)
          (progn (setf id (incf next-object-id))
                 (setf (gethash object object-id-table) id)
                 (format log-stream "#~d![" id)
                 (clobber::serialize (class-name (class-of object)) transaction-log)
                 (loop for info in (clobber::save-info object)
                       do (format log-stream "~%")
                          (clobber::serialize (car info) transaction-log)
                          (format log-stream " ")
                          (cond ((= (length info) 2) ;standard clobber output
                                 (clobber::serialize (funcall (cadr info) object)
                                                     transaction-log))
                                ((and (> (length info) 2) (null (third info))) ;no id output
                                 (format log-stream "~a" (funcall (cadr info) object)))
                                ((and (> (length info) 2) (not (null (third info)))) ;custom output
                                 (funcall (third info) (funcall (cadr info) object) log-stream))))
                 (format log-stream "]"))
          (format log-stream "#~d^" id)))))

(defun clobber::set-syntax (readtable object-table)
  (let ((*readtable* readtable))
    (clobber::%set-syntax-left-bracket )
    (clobber::%set-syntax-right-bracket)
    (clobber::%set-syntax-left-brace )  ;read POINT as {x y z}
    (clobber::%set-syntax-right-brace)  ; -- ditto --
    (clobber::%set-syntax-hash-bang object-table)
    (clobber::%set-syntax-hash-caret object-table)))

(defun clobber::%set-syntax-left-brace ()
  (set-macro-character
   #\{
   (lambda (stream char)
     (declare (ignore char))
     (apply #'kons-9::p! (read-delimited-list #\} stream t)))))

(defun clobber::%set-syntax-right-brace ()
  (set-syntax-from-char #\} #\)))

;;;; kons-9 classes ============================================================

(clobber:define-save-info scene-item
  (:name name))

(clobber:define-save-info translate-operator
  (:offset offset))

(clobber:define-save-info euler-rotate-operator
  (:angles angles)
  (:rotate-order rotate-order)
  (:pivot pivot))

(clobber:define-save-info angle-axis-rotate-operator
  (:angle angle)
  (:axis axis)
  (:pivot pivot))

(clobber:define-save-info scale-operator
  (:scaling scaling)
  (:pivot pivot))

(clobber:define-save-info transform
  (:operator-order operator-order))

(clobber:define-save-info euler-transform
  (:translate translate)
  (:rotate rotate)
  (:scale scale))

(clobber:define-save-info angle-axis-transform
  (:translate translate)
  (:rotate rotate)
  (:scale scale))

(clobber:define-save-info generalized-transform
  (:operators operators))

(clobber:define-save-info shape
  (:transform transform))

(clobber:define-save-info point-cloud
  (:points points)) ; clobber::serialize-point-array))

(clobber:define-save-info curve
  (:is-closed-curve? is-closed-curve?))

(clobber:define-save-info polyhedron
  (:faces faces nil))

(clobber:define-save-info scene
  (:shapes shapes)
;;  (:motions motions nil) ; motion classes not implemented -- how to save function slots?
  (:start-frame start-frame)
  (:end-frame end-frame)
  (:current-frame current-frame)
  (:fps fps))

;;;; transaction functions =====================================================

(defparameter *transaction-log* nil)

;;; return value of transaction
(defun execute-transaction (transaction-function &rest arguments)
  (let ((result (apply transaction-function arguments)))
    (clobber:log-transaction (cons transaction-function arguments)
                             *transaction-log*)
    result))

(defmacro execute (expr)
  `(if *transaction-log*
     (execute-transaction ',(first expr) ,@(rest expr))
     (funcall ',(first expr) ,@(rest expr))))

(defun start-transaction-log (filename)
  (let ((result nil))
    (setf *transaction-log*
          (clobber:open-transaction-log
           filename
           (lambda (transaction)
             (setf result (apply (car transaction) (cdr transaction))))))
    result))

(defun stop-transaction-log ()
  (clobber:close-transaction-log *transaction-log*))

(defun save-scene (scene filename)
  (start-transaction-log filename)
  (execute (identity scene))
  (stop-transaction-log))

(defun load-scene (filename)
  (start-transaction-log filename))

;; (setf *scene* (start-transaction-log filename))
;;   (setf (scene *default-scene-view*) *scene*))

#|
testing

(defun do-test-1 ()
  (let ((scene (make-instance 'scene))
        (p-cloud (make-point-cloud (make-grid-points 4 2 5 (p! -2.0 -0.4 -1.0) (p! 2.0 0.4 1.0))))
        (polyh (make-cut-cube 2.0)))
    (execute (add-shape scene p-cloud))
    (execute (rotate-by p-cloud (p! 0 45 0)))
    (execute (add-shape scene polyh))
    (execute (translate-to polyh (p! 0 1 0)))
    (execute (identity scene))))

(defun clobber-test ()
  (let ((filename "/Users/kaveh/Development/tmp/transaction-test-1.lisp"))
    (start-transaction-log filename)
    (do-test-1)
    (stop-transaction-log)
    (load-scene filename)))

(clobber-test)
(load-scene "/Users/kaveh/Development/tmp/transaction-test-1.lisp")

(save-scene *scene* "/Users/kaveh/Development/tmp/transaction-scene-1.lisp")
(load-scene "/Users/kaveh/Development/tmp/transaction-scene-1.lisp")

defclass-kons-9 -- add :logging [:yes, :no, :simple, 'custom-fn] slot attrib
deftransaction -- ...? -- all transactions come from GUI

TODO
-- filenames
-- text-edit ?
-- split ui.lisp into ui-widgets.lisp and interface.lisp
-- clear up compile warnings
-- test clobber
|#
