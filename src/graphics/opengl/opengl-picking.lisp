(in-package #:kons-9)

(defun opengl-pick (canvas-x canvas-y shape-group)
  (let ((ray (make-ray canvas-x canvas-y)))
    (do-children (child shape-group)
                 (let ((i (intersect-ray ray child)))
                   (if i
                       (setf (is-selected? child) t)
                       (setf (is-selected? child) nil))))))

(defun get-aabb (lo hi)
  (origin.geometry.aabb:aabb-from-min/max :min lo :max hi))

(defun intersect-ray (ray child)
  (multiple-value-bind (lo hi) (get-bounds-world child)
    (when (and lo hi)
      (origin.geometry:raycast-aabb ray (get-aabb lo hi)))))

(defun make-ray (canvas-x canvas-y)
  (multiple-value-bind
        (x0 y0 z0 x1 y1 z1) (gl-mouse-picking-ray canvas-x canvas-y)
    (origin.geometry.ray:ray-from-points
     :from (origin.geometry.point3d:point
            (coerce x0 'single-float)
            (coerce y0 'single-float)
            (coerce z0 'single-float))
     :to
     (origin.geometry.point3d:point
      (coerce x1 'single-float)
      (coerce y1 'single-float)
      (coerce z1 'single-float)))))

(defun demo-cube (cords-list)
  (let ((shape (make-octahedron 1))
        (x (car cords-list))
        (y (cadr cords-list))
        (z (caddr cords-list)))
    (translate-to shape (p! x y z))
    (add-shape (scene *scene-view*) shape)))

(defun demo-opengl-picking ()
  (mapcar
   #'demo-cube
   `(
     (0 0 0)
     (3 4 5)
     (5 5 -5)
     (-5 4 -4)
     (2 2 5)
     (-2 -5 5)
     (3 2 5)
     (-4 -5 5)
     (4 -3 5)
     (2 -5 -5)
     )))
