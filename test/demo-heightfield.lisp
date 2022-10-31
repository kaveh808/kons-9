(in-package #:kons-9)

#|
These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

A HEIGHTFIELD is a subclass of UV-MESH. It stores a height value (Y) for each
point on the XZ plane, representing the function Y=f(X,Z). It is useful for
visualizing mathematical functions of this sort, as well as terrain and
landscapes when combined with NOISE and TURBULENCE functions.

The demos below illustrate some examples of generating HEIGHTFIELD instances.
|#

#|
(Demo 01 heightfield) creating heightfields and setting point colors ===========

We use different formulas to set the height values of several heightfields. Then
we apply a variety of point colors.

Mix and match shapes and color schemes as desired.
|#

(format t "  heightfield 1...~%") (finish-output)

(with-clear-scene
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       :name 'hfield
                                       :height-fn (lambda (x z)
                                                    (* 4 (noise (p! x 0 z)))))))

(with-clear-scene
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       :name 'hfield
                                       :height-fn (lambda (x z)
                                                    (* 4 (turbulence (p! x 0 z) 4))))))

(with-clear-scene
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       :name 'hfield
                                       :height-fn (lambda (x z)
                                                    (let* ((p (p! x 0 z))
                                                           (mag (p:length (p:scale p .25))))
                                                      (if (= mag 0.0)
                                                          10.0
                                                          (/ 1.0 mag)))))))

(with-clear-scene
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       :name 'hfield
                                       :height-fn (lambda (x z)
                                                    (let* ((p (p! x 0 z))
                                                           (mag (max 0.001 (p:length (p:scale p 4.0)))))
                                                      (* 3 (/ (sin mag) mag)))))))

;;; rainbow color based on height
(let ((mesh (find-shape-by-name *scene* 'hfield)))
  (set-point-colors-by-xyz mesh (lambda (p) (c-rainbow (clamp (tween (p:y p) -.25 1.0) 0.0 1.0)))))

;;; rainbow color based on XZ distance from origin
(let ((mesh (find-shape-by-name *scene* 'hfield)))
  (set-point-colors-by-xyz mesh (lambda (p) (c-rainbow (clamp (tween (p:length (p! (p:x p) 0 (p:z p))) 0 8) 0.0 1.0)))))

;;; 3D color noise
(let ((mesh (find-shape-by-name *scene* 'hfield)))
  (set-point-colors-by-xyz mesh (lambda (p) (color-noise p))))

#|
(Demo 02 heightfield) animating a heightfield ==================================

We use an animator to vary the values of the heightfield formula over time.
|#

(format t "  heightfield 2...~%") (finish-output)

(with-clear-scene
  (let ((mesh (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5))))
    (set-point-colors-by-xyz mesh
                             (lambda (p)
                               (c-rainbow (clamp (tween (p:length (p! (p:x p) 0 (p:z p))) 0 8) 0.0 1.0))))
    (add-shape *scene* mesh)
    (macrolet ((my-height-fn (scale)
                 `(lambda (x z)
                    (let* ((p (p! x 0 z))
                           (mag (max 0.001 (p:length (p:scale p ,scale)))))
                      (* 3 (/ (sin mag) mag))))))
      (add-motion *scene*
                    (make-instance 'animator
                                   :setup-fn (lambda ()
                                              (setf (height-fn mesh) (my-height-fn 1.0))
                                              (update-heightfield mesh))
                                   :update-fn (lambda ()
                                                (setf (height-fn mesh) (my-height-fn (+ 1.0 (current-time *scene*))))
                                                (update-heightfield mesh)))))))

;;; Hold down space key to play animation. Press '[' key to go back to frame 0.
