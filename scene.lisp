(in-package #:kons-9)

;;;; scene ==============================================================

(defclass scene ()
  ((shapes :accessor shapes :initarg :shapes :initform '())
   (animators :accessor animators :initarg :animators :initform '())
   (init-done? :accessor init-done? :initarg :init-done? :initform nil)
   (current-frame :accessor current-frame :initarg :current-frame :initform 0)
   (fps :accessor fps :initarg :fps :initform 24)
   (sel-color :accessor sel-color :initarg :sel-color :initform (c! 1 0 0 1))
   (scene-generation-fn :accessor scene-generation-fn :initarg :scene-generation-fn :initform nil)))

(defmethod print-hierarchy ((self scene) &optional (indent 0))
  (print-spaces indent)
  (format t "~a~%" self)
  (dolist (shape (shapes self))
    (print-hierarchy shape (+ indent 2))))

(defmethod generate-scene ((scene scene))
  (when (scene-generation-fn scene)
    (funcall (scene-generation-fn scene))))

(defmethod current-time ((scene scene))
  (/ (coerce (current-frame scene) 'single-float) (fps scene)))

(defmethod add-shape ((scene scene) (obj shape))
  (push obj (shapes scene))
  obj)

(defmethod add-shapes ((scene scene) shapes)
  (mapcar #'(lambda (s) (add-shape scene s)) shapes))

;;; only works for top-level shapes, not children of groups
(defmethod remove-shape ((scene scene) (obj shape))
  (setf (shapes scene) (remove obj (shapes scene)))
  obj)

(defmethod add-animator ((scene scene) (obj animator))
  (push obj (animators scene))
  obj)

(defmethod add-animator-at-end ((scene scene) (obj animator))
  (setf (animators scene) (append (animators scene) (list obj)))
  obj)

(defmethod add-animators ((scene scene) animators)
  (mapcar #'(lambda (a) (add-animator scene a)) (reverse animators)))

(defmethod clear-shapes ((scene scene))
  (setf (shapes scene) '()))

(defmethod clear-animators ((scene scene))
  (setf (animators scene) '()))

(defmethod clear-scene ((scene scene))
  (clear-shapes scene)
  (clear-animators scene))

(defmethod draw ((scene scene))
  ;; (ccl:with-metering
  ;;     (update-scene init-scene update-animator update-particle
  ;;      add-point add-particle update-position update-velocity
  ;;      p-rand p-cross p-normalize update-angle range-value
  ;;      make-axis-rotation-matrix transform-point
  ;;      p-scale rand1 rand2
  ;;      make-sweep-mesh-group
  ;;      compute-procedural-node profile-curve-generator path-curve-generator
  ;;      sweep-extrude curve-generator-curves curve-generator-curves-closed
  ;;      sweep-extrude-aux curve-remove-consecutive-duplicates
  ;;      copy-points allocate-mesh-arrays curve-tangent make-axis-rotation-matrix
  ;;      p-angle make-translation-matrix matrix-multiply-n transform-points!
  ;;      make-scale-matrix p-lerp compute-polyhedron-data
  ;;      compute-polyhedron-mesh compute-face-normals compute-point-normals allocate-point-colors
  ;;      compute-face-list 2d-array-to-list face-normals point-normals p-normalize face-points
  ;;      needs-compute? inputs-time-stamp has-dirty-input?
  ;;      quad-normal triangle-normal x y z c-red c-green c-blue faces points
  ;;      draw draw-faces draw-wireframe draw-points draw-normals)
      ;; (:exclusive 0.0)
    (mapc #'draw (shapes scene)))
  ;; )

(defmethod init-scene ((scene scene))
  (setf (current-frame scene) 0)
  (mapc #'(lambda (anim)
            (let ((shape (init-animator anim)))
              (when shape
                (add-shape scene shape))))
        (animators scene)))

(defmethod update-scene ((scene scene))
  (when (not (init-done? scene))
    (init-scene scene)
    (setf (init-done? scene) t))
  (incf (current-frame scene))
  (mapc #'update-animator (animators scene)))


