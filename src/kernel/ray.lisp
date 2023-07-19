(in-package #:kons-9)

(defclass ray ()
  ((from :initarg :from :reader from)
   (to :initarg :to :reader to)))

(defmethod print-object ((self ray) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~s - ~s" (from self) (to self))))

(defun intersect-aabb (ray point-min point-max)
  (origin.geometry:raycast-aabb
   (origin.geometry.ray:ray :origin (from ray) :direction (to ray))
   (origin.geometry.aabb:aabb-from-min/max
    :min point-min :max point-max)))

(defun intersect-triangle (ray p0 p1 p2)
  (intersect/triangle
   (origin.geometry.triangle:triangle p0 p1 p2)
   (origin.geometry.ray:ray :origin (from ray) :direction (to ray))))

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

(defmethod intersect ((self ray) (scene scene))
  (let ((xs-hit-distances '())
        (xs-miss '())
        (xs-all (find-shapes scene #'identity)))
    (mapc (lambda (shape)
            (let ((distance (intersect self shape)))
              (if distance
                  (push (cons distance shape) xs-hit-distances)
                  (push shape xs-miss))))
          xs-all)
    (setf xs-hit-distances (stable-sort xs-hit-distances #'< :key #'car))
    (let ((xs-hit (mapcar #'cdr xs-hit-distances)))
      (values xs-hit xs-miss))))
