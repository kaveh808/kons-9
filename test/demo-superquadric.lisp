(in-package #:kons-9)

#|
These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

A SUPERQUADRIC is a subclass of UV-MESH. Superquadrics are a parametric (e1, e2)
representation of various ellipsoidal shapes. When both e1 and e2 are 1.0, the
resulting shape is a sphere.

Our implementation inherits from PROCEDURAL-MIXIN, so modifying the instance
slots will regenerate the geometry automatically.

The demos below illustrate some examples of SUPERQUADRICs.
|#

#|
(Demo 01 superquadric) creating a superquadric and assigning point colors ======

We use different (e1, e2) parameters to generate a superquadric shape. We then
apply a variety of point colors in (u, v) space.

Mix and match shapes and color schemes as desired.
|#

(format t "  superquadric 1...~%") (finish-output)

(with-clear-scene
  (defparameter *superq* (make-superquadric 16 16 2.0 1 0.1))
  (add-shape *scene* *superq*))

;;; modify slots and shape will update due to PROCEDURAL-MIXIN superclass
(setf (e1 *superq*) 0.5)
(setf (e2 *superq*) 0.5)
(setf (diameter *superq*) 4.0)
(setf (u-dim *superq*) 32)
(setf (v-dim *superq*) 32)

;;; rainbow color based on height
(set-point-colors-by-xyz *superq*
                         (lambda (p)
                           (c-rainbow (clamp (tween (p:y p) -2 2) 0.0 1.0))))

;;; rainbow color based on UV parameters
(set-point-colors-by-uv *superq*
                        (lambda (u v)
                          (declare (ignore v))
                          (c-rainbow u)))

(set-point-colors-by-uv *superq*
                        (lambda (u v)
                          (declare (ignore u))
                          (c-rainbow v)))

;;; 3D color noise
(set-point-colors-by-xyz *superq* (lambda (p) (color-noise p)))

#|
(Demo 02 superquadric) animate superquadric parameters =========================

Animate (e1, e2) parameters randomly using smooth noise.
|#

(format t "  superquadric 2...~%") (finish-output)

(with-clear-scene
  (let ((mesh (make-superquadric 32 32 4.0 1.0 1.0)))
    (add-shape *scene* mesh)
    (add-motion *scene*
                  (make-instance 'animator
                                 :setup-fn (lambda ()
                                            (setf (e1 mesh) 1.0)
                                            (setf (e2 mesh) 1.0))
                                 :update-fn (lambda ()
                                              (let ((p (p:normalize
                                                        (noise-gradient
                                                         (p! (+ (current-time *scene*) 0.123)
                                                             (+ (current-time *scene*) 0.347)
                                                             (+ (current-time *scene*) 0.965))))))
                                                (setf (e1 mesh) (* (abs (p:x p)) 2.0))
                                                (setf (e2 mesh) (* (abs (p:y p)) 2.0))))))))

;;; Hold down space key to play animation. Press '[' key to go back to frame 0.

#|
(Demo 03 superquadric) create grid of superquadrics ============================

Interpolate (e1, e2) parameters from 0.01 to 3.0. e1 and e2 must be greater
than zero.
|#

(format t "  superquadric 3...~%") (finish-output)

(with-clear-scene
  (let ((grid-dim 7)
        (grid-size 8.0))
    (dotimes (i grid-dim)               ;along X
      (dotimes (j grid-dim)             ;along Z
        (let* ((dx (/ i (1- grid-dim)))
               (dz (/ j (1- grid-dim)))
               (loc (p- (p! (* grid-size dx) 0.0 (* grid-size dz))
                        (p! (/ grid-size 2) 0.0 (/ grid-size 2))))
               (e1 (lerp dx 0.01 3.0))
               (e2 (lerp dz 0.01 3.0)))
          (add-shape *scene* (translate-to (make-superquadric 16 16 1.0 e1 e2) loc)))))))
                                           

