(in-package #:kons-9)

;;;; intersect =================================================================

(defun intersect-shape-triangles (ray shape)
  (let ((min nil))
    (do-array (_ triangle-pts (triangles-world-array shape))
      (let ((distance (intersect-triangle ray triangle-pts)))
        (when distance
          (when (or (null min) (< distance min))
            (setf min distance)))))
    min))

(defun intersect-shape-aabb (ray shape)
  (multiple-value-bind (lo hi) (get-bounds-world shape)
    (when (and lo hi)
      (intersect-aabb ray lo hi))))

(defun intersect-shape (ray shape)
    (and (intersect-shape-aabb ray shape)
         (intersect-shape-triangles ray shape)))

(defun intersect-scene (ray scene)
  (let ((xs-hit-distances '())
        (xs-miss '())
        (xs-all (find-shapes scene #'identity)))
    (mapc (lambda (shape)
            (let ((distance (intersect-shape ray shape)))
              (if distance
                  (push (cons distance shape) xs-hit-distances)
                  (push shape xs-miss))))
          xs-all)
    (stable-sort xs-hit-distances #'< :key #'car)
    (let ((xs-hit (mapcar #'cdr xs-hit-distances)))
      (values xs-hit xs-miss))))

;;;; pick ======================================================================

(defun pick (ray view)
  (flet ((select (shape) (setf (is-selected? shape) t))
         (unselect (shape) (setf (is-selected? shape) nil)))
    (multiple-value-bind (xs-hit xs-miss) (intersect-scene ray (scene view))
      (unless (null xs-hit)
        (select (car xs-hit))
        (mapc #'unselect (cdr xs-hit)))
      (mapc #'unselect xs-miss))))
