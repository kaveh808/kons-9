(in-package #:kons-9)

(defparameter *previous-selection-ray* nil)

(defclass ray ()
  ((from :initarg :from :reader from)
   (to :initarg :to :reader to)))

(defmethod print-object ((self ray) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~s - ~s" (from self) (to self))))

(defun intersect-aabb (ray point-min point-max)
  (origin.geometry:raycast-aabb
   (origin.geometry.ray:ray-from-points :from (from ray) :to (to ray))
   (origin.geometry.aabb:aabb-from-min/max
    :min point-min :max point-max)))

(defun intersect-triangle (ray p0 p1 p2)
  (intersect/triangle
   (origin.geometry.triangle:triangle p0 p1 p2)
   (origin.geometry.ray:ray-from-points :from (from ray) :to (to ray))))

(defun intersect-triangles (ray triangles)
  (let ((min-distance nil))
    (do-array (_ points triangles)
      (let ((distance (intersect-triangle ray
                                          (aref points 0)
                                          (aref points 1)
                                          (aref points 2))))
        (when distance
          (when (or (null min-distance) (< distance min-distance))
            (setf min-distance distance)))))
    min-distance))

;;;; display ray (useful for debugging) ========================================

(defun set-previous-ray (ray)
  (setf *previous-selection-ray* ray))

(defun draw-previous-ray ()
  (when *previous-selection-ray*
    (gl:line-width 1)
    (gl:color 1.0 0.8 0.0)
    (gl:shade-model :flat)
    (gl:disable :lighting)
    (flet ((v (vec3) (apply #'gl:vertex (coerce vec3 'list))))
      (gl:begin :lines)
      (v (from *previous-selection-ray*))
      (v (to *previous-selection-ray*))
      (gl:end))))


;;;; intersect routines ========================================================

;;; ignore shapes for which the method is not defined; do not throw error
(defmethod intersect ((self ray) (shape shape))
  nil)
;;  (error "INTERSECT not implemented"))

(defmethod intersect ((self ray) (polyh polyhedron))
  (multiple-value-bind (lo hi) (get-bounds-world polyh)
    (when (and lo hi)
      (when (intersect-aabb self lo hi)
        ;; before doing a more expensive operation of intersecting many
        ;; triangles we first do a quick intersect with the shapes aabb (axis
        ;; aligned bounding box). If the aabb does not intersect there is no use
        ;; of intersecting with triangles.
        (intersect-triangles self (triangles-world-array polyh))))))

