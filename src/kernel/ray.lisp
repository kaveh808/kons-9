(in-package #:kons-9)

(defclass ray ()
  ((from :initarg :from :reader from)
   (to :initarg :to :reader to)))

(defmethod print-object ((self ray) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~s - ~s" (from self) (to self))))

(defmethod intersect-triangle ((self ray) triangle-points)
  (let ((p0 (aref triangle-points 0))
        (p1 (aref triangle-points 1))
        (p2 (aref triangle-points 2)))
    (intersect/triangle
     (origin.geometry.triangle:triangle p0 p1 p2)
     (origin.geometry.ray:ray :origin (from self) :direction (to self)))))

(defmethod intersect-aabb ((self ray) point-min point-max)
  (origin.geometry:raycast-aabb
   (origin.geometry.ray:ray :origin (from self) :direction (to self))
   (origin.geometry.aabb:aabb-from-min/max
    :min point-min :max point-max)))
