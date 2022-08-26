(in-package #:kons-9)

;;;; scene =====================================================================

(defclass scene ()
  ((shapes :accessor shapes :initarg :shapes :initform '())
   (motions :accessor motions :initarg :motions :initform '())
   (initialized? :accessor initialized? :initarg :initialized? :initform nil)
   (selection :accessor selection :initarg :selection :initform '())
   (start-frame :accessor start-frame :initarg :start-frame :initform 0)
   (end-frame :accessor end-frame :initarg :end-frame :initform 240)
   (current-frame :accessor current-frame :initarg :current-frame :initform 0)
   (fps :accessor fps :initarg :fps :initform 24)))

(defmethod print-hierarchy ((self scene) &optional (indent 0))
  (print-spaces indent)
  (format t "~a~%" self)
  (dolist (shape (shapes self))
    (print-hierarchy shape (+ indent 2))))

(defmethod current-time ((scene scene))
  (/ (coerce (current-frame scene) 'single-float) (fps scene)))

(defmethod add-selection ((scene scene) (item scene-item))
  (select item)
  (pushnew item (selection scene))
  item)

(defmethod remove-selection ((scene scene) (item scene-item))
  (unselect item)
  (setf (selection scene) (remove item (selection scene)))
  item)

(defmethod toggle-selection ((scene scene) (item scene-item))
  (if (member item (selection scene))
      (remove-selection scene item)
      (add-selection scene item)))

(defmethod clear-selection ((scene scene))
  (dolist (item (selection scene))
    (unselect item))
  (setf (selection scene) '()))

(defmethod add-shape ((scene scene) (shape shape))
  (push shape (shapes scene))
  (setf (scene shape) scene)
  shape)

(defmethod add-shapes ((scene scene) shapes)
  (mapcar #'(lambda (s) (add-shape scene s)) shapes))

;;; only works for top-level shapes, not children of groups
(defmethod remove-shape ((scene scene) (shape shape))
  (setf (shapes scene) (remove shape (shapes scene)))
  (setf (selection scene) (remove shape (selection scene)))
  (setf (scene shape) nil)
  shape)

(defmethod add-motion ((scene scene) (motion motion))
  (push motion (motions scene))
  (setf (scene motion) scene)
  motion)

(defmethod add-motion-at-end ((scene scene) (motion motion))
  (setf (motions scene) (append (motions scene) (list motion)))
  (setf (scene motion) scene)
  motion)

(defmethod add-motions ((scene scene) motions)
  (mapcar #'(lambda (a) (add-motion scene a)) (reverse motions)))

;;; TODO -- set shape scene to nil, remove shapes from scene selection
(defmethod clear-shapes ((scene scene))
  (setf (shapes scene) '()))

;;; TODO -- set motion scene to nil, remove motions from scene selection
(defmethod clear-motions ((scene scene))
  (setf (motions scene) '()))

(defmethod remove-current-selection ((scene scene))
  (dolist (shape (selection scene))
    (remove-shape scene shape)))

(defmethod clear-scene ((scene scene))
  (setf (selection scene) '())
  (clear-shapes scene)
  (clear-motions scene)
    (setf (current-frame scene) 0))

(defmethod init-scene ((scene scene))
  (setf (current-frame scene) (start-frame scene))
  (do-motion-hierarchy scene (lambda (m) (setf (scene m) scene)))
  (mapc #'setup-motion (motions scene))
  (setf (initialized? scene) t))

(defmethod update-scene ((scene scene) &optional (num-frames 1))
  (when (not (initialized? scene))
    (init-scene scene))
  (dotimes (i num-frames)
    (when (< (current-frame scene) (end-frame scene))
      (incf (current-frame scene))
      (mapc (lambda (m) (update-motion m (compute-motion-absolute-timing scene nil)))
            (motions scene)))))

(defmethod compute-motion-absolute-timing ((scene scene) parent-absolute-timing)
  (declare (ignore parent-absolute-timing))
  (vector (coerce (/ (start-frame scene) (fps scene)) 'float)
          (coerce (/ (- (end-frame scene) (start-frame scene)) (fps scene)) 'float)))

(defmethod do-motion-hierarchy ((scene scene) func &key (test nil))
  (dolist (child (motions scene))
    (do-hierarchy child func :test test))
  scene)

(defmethod do-shape-hierarchy ((scene scene) func &key (test nil))
  (dolist (child (shapes scene))
    (do-hierarchy child func :test test))
  scene)


