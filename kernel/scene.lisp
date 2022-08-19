(in-package #:kons-9)

;;;; scene =====================================================================

(defclass scene ()
  ((shapes :accessor shapes :initarg :shapes :initform '())
   (animators :accessor animators :initarg :animators :initform '())
   (selection :accessor selection :initarg :selection :initform '())
   (init-done? :accessor init-done? :initarg :init-done? :initform nil)
   (current-frame :accessor current-frame :initarg :current-frame :initform 0)
   (fps :accessor fps :initarg :fps :initform 24)
   (sel-color :accessor sel-color :initarg :sel-color :initform (c! 1 0 0 1))
   (scene-generation-fn :accessor scene-generation-fn :initarg :scene-generation-fn :initform nil)))

(defmethod generate-scene ((scene scene))
  (when (scene-generation-fn scene)
    (funcall (scene-generation-fn scene))))

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

(defmethod add-animator ((scene scene) (anim animator))
  (push anim (animators scene))
  (setf (scene anim) scene)
  anim)

(defmethod add-animator-at-end ((scene scene) (anim animator))
  (setf (animators scene) (append (animators scene) (list anim)))
  (setf (scene anim) scene)
  anim)

(defmethod add-animators ((scene scene) animators)
  (mapcar #'(lambda (a) (add-animator scene a)) (reverse animators)))

;;; TODO -- set shape scene to nil, remove shapes from scene selection
(defmethod clear-shapes ((scene scene))
  (setf (shapes scene) '()))

;;; TODO -- set anim scene to nil, remove shapes from scene selection
(defmethod clear-animators ((scene scene))
  (setf (animators scene) '()))

(defmethod remove-current-selection ((scene scene))
  (dolist (shape (selection scene))
    (remove-shape scene shape)))

(defmethod clear-scene ((scene scene))
  (setf (selection scene) '())
  (clear-shapes scene)
  (clear-animators scene))
  
(defmethod init-scene ((scene scene))
  (setf (current-frame scene) 0)
  (mapc #'init-animator (animators scene)))

(defmethod update-scene ((scene scene))
  (when (not (init-done? scene))
    (init-scene scene)
    (setf (init-done? scene) t))
  (incf (current-frame scene))
  (mapc #'update-animator (animators scene)))


