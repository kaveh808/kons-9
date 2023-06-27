(in-package #:kons-9)

#|

These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

|#

;;;; add-demo-shapes-to-scene ==================================================

(progn
  (flet ((demo-cube (cords-list)
           (let ((shape (make-octahedron 0.3))
                 (x (car cords-list))
                 (y (cadr cords-list))
                 (z (caddr cords-list)))
             (translate-to shape (p! x y z))
             (add-shape (scene *scene-view*) shape))))
    (dotimes (x 4)
      (dotimes (y 4)
        (dotimes (z 4)
          (demo-cube (list x y z)))))))
