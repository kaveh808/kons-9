(in-package #:kons-9)

;;;; motion ====================================================================

(defclass motion (scene-item)
  ((start-time :accessor start-time :initarg :start-time :initform 0.0)
   (duration :accessor duration :initarg :duration :initform 1.0) ;nil = infinite duration
   (is-active? :accessor is-active? :initarg :is-active? :initform t)
   ;; :absolute-time :relative-to-parent
   (timing-mode :accessor timing-mode :initarg :timing-mode :initform :relative-to-parent)
   (local-time :accessor local-time :initarg :local-time :initform 0.0)))

(defmethod printable-data ((self motion))
  (strcat (call-next-method) (format nil ", timing = ~a ~a" (start-time self) (duration self))))

(defmethod set-timing ((motion motion) start-time duration)
  (setf (start-time motion) start-time)
  (setf (duration motion) duration)
  motion)

(defmethod offset-start-time ((motion motion) delta)
  (incf (start-time motion) delta)
  motion)

(defmethod scale-duration ((motion motion) factor)
  (setf (duration motion) (* factor (duration motion)))
  motion)

(defmethod in-time-interval? ((motion motion) timing)
  (let* ((global-time (current-time (scene motion)))
         (timing-start-time (aref timing 0))
         (timing-duration (aref timing 1))
         (local-time (/ (- global-time timing-start-time) timing-duration))) ; 0-1
    (setf (local-time motion) local-time)
    (and (<= 0.0 local-time) (<= local-time 1.0))))

(defmethod setup-motion ((motion motion))
  ;; do nothing
  )

(defmethod update-motion ((motion motion) parent-absolute-timing)
  (declare (ignore parent-absolute-timing))
  ;; do nothing
  )

(defmethod compute-motion-absolute-timing ((motion motion) parent-absolute-timing)
  (let ((parent-start-time (aref parent-absolute-timing 0))
        (parent-duration (aref parent-absolute-timing 1)))
    (if (eq :relative-to-parent (timing-mode motion))
        (vector (+ parent-start-time (* (start-time motion) parent-duration))
                (* (duration motion) parent-duration))
        (vector (start-time motion) (duration motion)))))

;;;; motion-group ============================================================

(defclass motion-group (motion)
  ((children :accessor children :initarg :children :initform ())))

(defmethod printable-data ((self motion-group))
  (strcat (call-next-method) (format nil ", ~a children" (length (children self)))))

(defmethod add-child ((self motion-group) (m motion))
  (push m (children self))
  self)

(defmethod set-children ((self motion-group) motions)
  (setf (children self) motions)
  self)

(defmethod remove-all-children ((self motion-group))
  (setf (children self) '())
  self)

(defun make-motion-group (motions &key (name nil))
  (make-instance 'motion-group :name name :children motions))

(defmethod setup-motion ((motion motion-group))
  (when (is-active? motion)
    (mapc #'setup-motion (children motion))))

(defmethod update-motion ((motion motion-group) parent-absolute-timing)
  (when (is-active? motion)
    (let ((timing (compute-motion-absolute-timing motion parent-absolute-timing)))
      (when (in-time-interval? motion timing)
        (mapc (lambda (m) (update-motion m timing))
              (children motion))))))

(defmethod parallel-order ((motion motion-group) &optional (start-time 0.0) (duration 1.0))
  (dolist (child (children motion))
    (set-timing child start-time duration)))

(defmethod sequential-order ((motion motion-group) &optional (start-time 0.0) (duration 1.0))
  (when (children motion)
    (let ((child-duration (/ duration (length (children motion))))
          (child-start start-time))
      (dolist (child (children motion))
        (set-timing child child-start child-duration)
        (incf child-start child-duration)))))

(defmethod random-order ((motion motion-group) &optional (min-duration 0.0) (max-duration 1.0))
  (dolist (child (children motion))
    (let* ((child-duration (rand2 min-duration max-duration))
           (child-start (rand2 0 (- 1.0 child-duration))))
      (set-timing child child-start child-duration))))

