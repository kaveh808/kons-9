;;;; scene-item validation machinery ===========================================
;;;
;;; This file defines the generic function VALIDATE for detecting errors in
;;; SCENE and SCENE-ITEM objects.
;;;
;;; VALIDATE methods serve two purposes:
;;; - As documentation to say what invariants apply to each kind of object.
;;; - As a tool for discovering broken invariants and recovering from them.
;;;
;;; VALIDATE can be called manually and/or hooked into SCENE-UPDATE via
;;; *AUTO-VALIDATE-ON-SCENE-UPDATE*.

(in-package #:kons-9)

(defvar *validation-subject* nil
  "The object being validated by VALIDATE.")

(defvar *validation-aspect* nil
  "Named aspect of *VALIDATION-SUBJECT* that is being validated.")

(defgeneric validate (scene-or-item)
  (:documentation "Signal VALIDATION-ERROR if SCENE-OR-TIME is not valid.")
  ;; Scenes are validated via their constituent items.
  (:method ((scene scene))
    (do-children (item (shape-root scene))  (validate item))
    (do-children (item (motion-root scene)) (validate item)))
  ;; Scene items are valid by default.
  (:method ((item scene-item)) t)
  ;; User-friendly restarts are available while validating scene items.
  (:method :around ((item scene-item))
    (with-simple-restart (pass-item "Skip validation of scene item ~a" (name item))
      (loop (with-simple-restart (revalidate "Retry validation of ~a" (name item))
              (let ((*validation-subject* item))
                (return (call-next-method))))))
    (values)))

(define-condition validation-error (error)
  ((item :initarg :item)
   (aspect :initarg :aspect)
   (format :initarg :format)
   (args :initarg :args))
  (:report (lambda (condition stream)
             (with-slots (item aspect format args) condition
               (format stream "[~a~@['s ~a~]] ~?" (name item) aspect format args))))
  (:documentation
   "Invalid scene-item detected."))

(defun count-validation-errors (scene-item)
  "Return the total number of errors detected by VALIDATE."
  (let ((errors 0))
    (handler-bind ((validation-error (lambda (c)
                                       (declare (ignore c))
                                       (incf errors)
                                       (invoke-restart 'ignore))))
      (validate scene-item)
      errors)))

(defun verror (format &rest format-args)
  "Signal a (continuable) validation error on *VALIDATION-SUBJECT*."
  (check-type format string)
  (with-simple-restart (ignore "Ignore this single error and continue.")
    (error (make-instance 'validation-error
                          :item *validation-subject* :aspect *validation-aspect*
                          :format format :args format-args))))

(defmacro validating (aspect-name &body body)
  "Validate ASPECT-NAME of *VALIDATION-SUBJECT* by executing BODY."
  `(let ((*validation-aspect* ,aspect-name))
     (with-simple-restart (pass "Skip validating ~a of ~a."
                                ,aspect-name (name *validation-subject*))
       ,@body)))

(defmacro vassert (expr &optional fmt &rest fmt-args)
  "Assert EXPR to validate *VALIDATION-SUBJECT*."
  `(if ,expr t (verror "Assert ~a failed~@[: ~?~]." ',expr ,fmt ,@fmt-args)))

;;;; point-cloud validation ====================================================

(defmethod validate ((scene scene))
  (do-children (item (shape-root scene))  (validate item))
  (do-children (item (motion-root scene)) (validate item)))

;;;; point-cloud validation ====================================================

(defmethod validate ((pc point-cloud))
  (declare (optimize (debug 3)))
  (call-next-method)
  (with-slots (points point-colors) pc
    (validating :points
      (when (vassert (vectorp points))
        (loop for p across points
              do (unless (point? p)
                   (verror "Invalid point: ~a" p)))))
    (when point-colors
      (validating :point-colors
        (when (vassert (vectorp point-colors))
          (vassert (alexandria:length= points point-colors))
          (loop for c across point-colors
                do (unless (color? c)
                     (verror "Invalid color: ~a" c))))))))

;;;; curve validation ==========================================================

(defmethod validate ((cv curve))
  (declare (optimize (debug 3)))
  (call-next-method)
  (with-slots (points) cv
    (validating :points
      (vassert (>= (length points) 2) "a curve must have at least two points."))))

;;;; polyhedron validation =====================================================

(defmethod validate ((ph polyhedron))
  (declare (optimize (debug 3)))
  (call-next-method)
  (with-slots (points faces face-normals point-normals) ph
    ;; XXX Should empty polyhedrons be allowed to pass validation too?
    (when (vassert (>= (length faces) 3))
      (validating :faces
        (when (vassert (vectorp faces))
          (do-array (f face faces)
            (dolist (ix face)
              (let ((max-index (1- (length points))))
                (unless (and (integerp ix) (<= 0 ix max-index))
                  (verror "Invalid vertex reference ~a in face ~a" ix f)))))))
      ;; IDEA: Verify that the sum of the face normals, weighted by the area
      ;; of that face, is zero. That seems like an interesting invariant.
      ;; Have to implement FACE-AREA to do this though.
      (validating :face-normals
        ;; Normal should be a unit vector.
        (do-array (fn face-normal face-normals)
          (~= 1.0 (p:length face-normal))))
      (validating :point-normals
        (do-array (pn point-normal point-normals)
          (~= 1.0 (p:length point-normal)))))))

;;;; Hook to automatically validate scene updates ==============================

(defvar *auto-validate-on-scene-update* nil
  "Automatically validate the scene before and after each SCENE-UPDATE.")

(defmethod update-scene :around ((scene scene) &optional (num-frames 1))
  (declare (ignore num-frames))
  (if *scene-auto-validate*
      (progn (validate scene) (call-next-method) (validate scene))
      (call-next-method)))
