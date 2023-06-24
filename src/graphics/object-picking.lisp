(in-package #:kons-9)

(defparameter *object-pick-requested* nil)
(defparameter *object-picking-ray-visible* t)

(defmacro picking-ray-visible-p ()
  '*object-picking-ray-visible*)

(defmacro make-pick-request ()
  `(setf *object-pick-requested* t))

(defmacro when-pick-requested (&body body)
  `(when *object-pick-requested*
     (setf *object-pick-requested* nil)
     ,@body))

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

(defun get-hit-results (ray scene)
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

(defun handle-pick-request (ray view)
  (flet ((select (shape) (setf (is-selected? shape) t))
         (unselect (shape) (setf (is-selected? shape) nil)))
    (multiple-value-bind (xs-hit xs-miss) (get-hit-results ray (scene view))

      (unless (null xs-hit)
        (select (car xs-hit))
        (mapc #'unselect (cdr xs-hit)))
      (mapc #'unselect xs-miss))))

(defun demo-cube (cords-list)
  (let ((shape (make-octahedron 0.3))
        (x (car cords-list))
        (y (cadr cords-list))
        (z (caddr cords-list)))
    (translate-to shape (p! x y z))
    (add-shape (scene *scene-view*) shape)))

(defun add-demo-shapes-to-scene ()
  (dotimes (x 4)
    (dotimes (y 4)
      (dotimes (z 4)
        (demo-cube (list x y z))))))

;; (defun add-demo-shapes-to-scene ()
;;   (mapcar
;;    #'demo-cube
;;    `(
;;      (0 0 0)
;;      (3 4 5)
;;      (5 5 -5)
;;      (-5 4 -4)
;;      (2 2 5)
;;      (-2 -5 5)
;;      (3 2 5)
;;      (-4 -5 5)
;;      (4 -3 5)
;;      (2 -5 -5)
;;      )))
