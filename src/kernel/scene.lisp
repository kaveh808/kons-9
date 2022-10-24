(in-package #:kons-9)

;;;; scene =====================================================================

(defclass-kons-9 scene (item)
  ((shape-root (make-instance 'shape-group :name 'shapes))
   (motion-root (make-instance 'motion-group :name 'motions))
   (initialized? nil)
   (selection '())
   (start-frame 0)
   (end-frame 240)
   (current-frame 0)
   (fps 24)))

(defmethod printable-data ((self scene))
  (strcat (call-next-method)
          (format nil ", frame bounds: ~a ~a, current: ~a "
                  (start-frame self) (end-frame self) (current-frame self))))

(defmethod initialize-instance :after ((scene scene)  &rest initargs)
  (declare (ignore initargs))
  ;; set scene for shapes and motions
  (map-shape-hierarchy scene (lambda (item) (setf (scene item) scene)))
  (map-motion-hierarchy scene (lambda (item) (setf (scene item) scene))))

(defmethod current-time ((scene scene))
  (/ (coerce (current-frame scene) 'single-float) (fps scene)))

(defmethod relative-current-time ((scene scene))
  (if (= (end-frame scene) (start-frame scene))
      0.0
      (coerce (/ (current-frame scene) (- (end-frame scene) (start-frame scene))) 'float)))

(defmethod selected-shapes ((scene scene))
  (remove-if-not (lambda (item) (subtypep (type-of item) 'shape)) (selection scene)))

(defmethod selected-shape ((scene scene))
  (let ((selected (selected-shapes scene)))
    (if (= 1 (length selected))
        (first selected)
        nil)))

(defmethod selected-motions ((scene scene))
  (remove-if-not (lambda (item) (subtypep (type-of item) 'motion)) (selection scene)))

(defmethod selected-motion ((scene scene))
  (let ((selected (selected-motions scene)))
    (if (= 1 (length selected))
        (first selected)
        nil)))

(defmethod add-to-selection ((scene scene) (item scene-item))
  (setf (is-selected? item) t)
  (pushnew item (selection scene))
  item)

(defmethod remove-from-selection ((scene scene) (item scene-item))
  (setf (is-selected? item) nil)
  (setf (selection scene) (delete item (selection scene))))

(defmethod toggle-selection ((scene scene) (item scene-item))
  (if (member item (selection scene))
      (remove-from-selection scene item)
      (add-to-selection scene item)))

(defmethod clear-selection ((scene scene))
  (dolist (item (selection scene))
    (setf (is-selected? item) nil))
  (setf (selection scene) '()))

(defmethod delete-selection ((scene scene))
  (dolist (item (selection scene))
    (setf (is-selected? item) nil)
    (remove-item scene item))
  (setf (selection scene) '()))

(defmethod remove-item ((scene scene) (item scene-item))
  (let ((parents (parents item)))
    (dolist (parent parents)
      (remove-child parent item)))
  item)

(defmethod add-shape ((scene scene) (shape shape))
  (add-child (shape-root scene) shape)
  shape)

(defmethod set-item-scene ((scene scene) (item scene-item))
  (setf (scene item) scene)
  item)

(defmethod set-item-scene :after ((scene scene) (group group-mixin))
  (do-children (child group)
    (set-item-scene scene child)))

(defmethod add-shapes ((scene scene) shapes)
  (mapcar #'(lambda (s) (add-shape scene s)) shapes))

;;; only works for top-level shapes, not children of groups
(defmethod remove-shape ((scene scene) (shape shape))
  (remove-child (shape-root scene) shape)
  (setf (selection scene) (remove shape (selection scene)))
  (setf (scene shape) nil)
  shape)

(defmethod add-motion ((scene scene) (motion motion))
  (add-child (motion-root scene) motion)
  motion)

(defmethod add-motions ((scene scene) motions)
  (mapcar #'(lambda (a) (add-motion scene a)) (reverse motions)))

(defmethod clear-shapes ((scene scene))
  (remove-all-children (shape-root scene)))

(defmethod clear-motions ((scene scene))
  (remove-all-children (motion-root scene)))

(defmethod clear-scene ((scene scene))
  (setf (selection scene) '())
  (clear-shapes scene)
  (clear-motions scene)
  (setf (current-frame scene) 0))

(defmethod init-scene ((scene scene))
  (setf (current-frame scene) (start-frame scene))
  (map-motion-hierarchy scene (lambda (m) (setf (scene m) scene)))
  (setup-motion (motion-root scene))
  (setf (initialized? scene) t))

(defmethod update-scene ((scene scene) &optional (num-frames 1))
  (when (not (initialized? scene))
    (init-scene scene))
  (dotimes (i num-frames)
    (when (< (current-frame scene) (end-frame scene))
      (incf (current-frame scene))
      (let ((timing (compute-motion-absolute-timing scene nil)))
        (do-children (child (motion-root scene))
          (update-motion child timing))))))

(defmethod compute-motion-absolute-timing ((scene scene) parent-absolute-timing)
  (declare (ignore parent-absolute-timing))
  (vector (coerce (/ (start-frame scene) (fps scene)) 'float)
          (coerce (/ (- (end-frame scene) (start-frame scene)) (fps scene)) 'float)))

(defmethod num-shapes ((scene scene))
  (length (find-shapes scene #'identity)))

(defmethod num-motions ((scene scene))
  (length (find-motions scene #'identity)))
