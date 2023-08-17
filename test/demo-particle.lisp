(in-package #:kons-9)


#|
These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

The PARTICLE-SYSTEM class represents particles which are updated over time.
These can be used to simulate fireworks-type objects or trailing/branching
structures.

PARTICLE-SYSTEM inherits from POLYHEDRON and internally creates polygonal faces
from the trails of its particles. These trails/faces can be used as paths for
SWEEP-MESH creation.

The demos below demonstrate examples of using particle systems.
|#

#|
(Demo 01 particle) particle system from a point ================================

Create 10 particles from a point. The particles split after their life-span of
5 frames.
|#

(format t "  particle-system 01...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point (p! 0 0 0) 10 (p! -.1 .1 -.1) (p! .1 .2 .1)
                                                :particle-class 'particle
                                                :particle-initargs '(:life-span 5))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 02 particle) particle system from a point source (curve) =================

Create particles from a point source (points of a curve). The particles get
their velocities from the curve tangents. We provide function to modify particle
initial velocities by scaling them down.
|#

(format t "  particle-system 02...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-circle-curve 2.0 16))
         (p-sys (make-particle-system-from-point-source shape
                                                        :vel-fn (lambda (vel) (p:scale vel 0.1))
                                                        :particle-class 'particle
                                                        :particle-initargs '(:life-span 10))))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 03 particle) particle system from a point source (polyhedron) ============

Create particles from a point source (points of a polyhedron). The particles get
their velocities from the point normals. We provide function to modify particle
initial velocities by scaling them down.
|#

(format t "  particle-system 03...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-icosahedron 2.0))
         (p-sys (make-particle-system-from-point-source shape
                                                        :vel-fn (lambda (vel) (p:scale vel 0.1))
                                                        :particle-class 'particle
                                                        :particle-initargs '(:life-span 10))))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 04 particle) particle system from a point source (polyhedron) ============

Create particles from a point source (faces of a polyhedron). The particles get
their velocities from the face normals. We provide function to modify particle
initial velocities by scaling them down.
|#

(format t "  particle-system 04...~%") (finish-output)

(with-clear-scene
  (let ((shape (make-icosahedron 2.0)))
    (setf (point-source-use-face-centers? shape) t) ;use face centers instead of points
    (let ((p-sys (make-particle-system-from-point-source shape
                                                         :vel-fn (lambda (vel) (p:scale vel 0.1))
                                                         :particle-class 'particle
                                                         :particle-initargs '(:life-span 10))))
      (setf (use-point-colors? p-sys) nil) ;face colors are white by default, ignore them
      (add-shape *scene* shape)
      (add-shape *scene* p-sys)
      (add-motion *scene* p-sys))))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 05 particle) particle system with wriggle ================================

Randomized particle velocities' update-angle to give "wriggle" effect.
|#

(format t "  particle-system 05...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-icosahedron 2))
         (p-sys (make-particle-system-from-point-source
                 shape
                 :vel-fn (lambda (vel) (p:scale vel 0.1))
                 :particle-class 'particle
                 :particle-initargs `(:life-span 10
                                      :update-angle ,(range-float 20.0 10.0)))))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 06 particle) dynamic particle system with force field ====================

Create dynamic particles with constant force field simulating gravity. Do
collisions with ground plane.
|#

(format t "  particle-system 06...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point
                (p! 0 1 0) 10 (p! -.2 .1 -.2) (p! .2 .3 .2)
                :particle-class 'dynamic-particle
                :particle-initargs `(:life-span 20
                                     :do-collisions? t
                                     :elasticity 0.8
                                     :force-fields ,(list (make-instance 'constant-force-field
                                                                        :force-vector (p! 0 -.02 0)))))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 100)               ;do update for batch testing

#|
(Demo 07 particle) dynamic particle system with wriggle ========================

Create dynamic particles with wriggle effect.
|#

(format t "  particle-system 07...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point
                (p! 0 1 0) 10 (p! -.2 .1 -.2) (p! .2 .3 .2)
                :particle-class 'dynamic-particle
                :particle-initargs `(:life-span 20
                                     :update-angle ,(range-float 20.0 10.0)
                                     :spawn-angle ,(range-float 20.0 10.0)
                                     :do-collisions? t
                                     :elasticity 0.95
                                     :force-fields ,(list (make-instance 'constant-force-field
                                                                        :force-vector (p! 0 -.05 0)))))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 100)               ;do update for batch testing

#|
(Demo 08 particle) dynamic particle system with an attractor force field =======

Simulate particles in orbit.
|#

(format t "  particle-system 08...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point-source
                (make-circle-curve 4 16)
                :vel-fn (lambda (vel) (p:scale (p:normalize (p+ vel (p-rand))) 0.2))
                :particle-class 'dynamic-particle
                :particle-initargs `(:life-span -1 ;infinite life-span
                                     :do-collisions? nil
                                     :force-fields ,(list (make-instance 'attractor-force-field
                                                                         :location (p! 0 0 0)
                                                                         :magnitude 0.1))))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 100)               ;do update for batch testing

#|
(Demo 09 particle) dynamic particle system with a noise force field ============

Dynamic particles growing from a height field under the influence of a noise
force field.
|#

(format t "  particle-system 09...~%") (finish-output)

(with-clear-scene
  (let* ((shape (freeze-transform
                 (translate-by (make-heightfield 20 20 (p! -5 0 -5) (p! 5 0 5)
                                                 :height-fn (lambda (x z)
                                                              (* 4 (turbulence (p! x 0 z) 4))))
                               (p! 0 -1 0))))
         (p-sys (make-particle-system-from-point-source
                 shape
                 :vel-fn (lambda (vel) (p:scale vel 0.05))
                 :particle-class 'dynamic-particle
                 :particle-initargs `(:life-span -1 ;infinite life-span
                                      :do-collisions? nil
                                      :force-fields ,(list (make-instance '3d-noise-force-field
                                                                          :noise-frequency 0.2
                                                                          :noise-amplitude 0.2))))))
    (setf (use-point-colors? p-sys) nil) ;point colors are white by default, ignore them
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 60)               ;do update for batch testing

#|
(Demo 10 particle) climbing particle system ====================================

Climbing particles which follow the surface of a shape.
|#

(format t "  particle-system 10...~%") (finish-output)

(with-clear-scene
  (let* ((shape (triangulate-polyhedron (make-cube-sphere 6.0 3)))
         ;(shape (triangulate-polyhedron (make-cube 6)))
         ;(shape (triangulate-polyhedron (make-grid-uv-mesh 3 3 1 1)))
         (p-sys (make-particle-system-from-point
                 (p! 0 3.5 0) 20 (p! -.5 0 -.5) (p! .5 0 .5)
                 :particle-class 'climbing-particle
                 :particle-initargs `(:life-span -1 ;infinite lifespan
                                      :support-polyh ,shape
                                      :update-angle ,(range-float 45.0 22.5)))))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation -- gets slow, need to optimize
;;; suggestion: turn off filled display for a better view (TAB, D, 1)

(update-scene *scene* 30)               ;do update for batch testing

#|
(Demo 11 particle) climbing particle system ====================================

Climbing particles which follow the surface of a shape.
|#

(format t "  particle-system 11...~%") (finish-output)

(with-clear-scene
  (let* ((shape (import-obj (asdf:system-relative-pathname "kons-9" "test/data/cow.obj")))
         (p-sys (make-particle-system-from-point
                 (p! .5 1.8 0) 10 (p! -.5 0 -.5) (p! .5 0 .5)
                 :particle-class 'climbing-particle
                 :particle-initargs `(:life-span 10
                                      :support-polyh ,shape
                                      :update-angle ,(range-float 45.0 22.5)))))
;    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 30)               ;do update for batch testing

#|
(Demo 12 particle) sweep mesh group from particle system =======================

Generating a sweep mesh group from a particle system
|#

(format t "  particle-system 12...~%") (finish-output)

(with-clear-scene
  (let ((p-source (make-icosahedron 2.0)))
    (setf (point-source-use-face-centers? p-source) t) ;comment out to use poly points
    (let* ((p-sys (make-particle-system-from-point-source
                   p-source
                   :vel-fn (lambda (vel) (p:scale vel 0.1))
                   :particle-class 'particle
                   :particle-initargs `(:life-span 10
                                        :update-angle ,(range-float 10.0 5.0)
                                        :spawn-angle ,(range-float 45.0 22.5))))
           (sweep-mesh-group (make-sweep-mesh-group (make-circle 0.2 6) p-sys
                                                    :taper 0.0 :twist 0.0)))
      (add-shape *scene* p-source)
      ;; (add-shape *scene* p-sys)         ;no need to add p-sys shape to scene
      (add-shape *scene* sweep-mesh-group)
      (add-motion *scene* p-sys))))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 30)               ;do update for batch testing

#|
(Demo 13 particle) dynamic particle system =====================================

Dynamic particles growing from a superquadric with a gravity force field.
|#

(format t "  particle-system 13...~%") (finish-output)

(with-clear-scene
  (let ((p-source (freeze-transform (translate-by (make-superquadric 16 8 2.0 1.0 1.0)
                                                  (p! 0 2 0)))))
    (setf (point-source-use-face-centers? p-source) t) ;comment out to use poly points
    (let ((p-sys (make-particle-system-from-point-source
                  p-source
                  :vel-fn (lambda (v) (p:scale v 0.2))
                  :particle-class 'dynamic-particle
                  :particle-initargs `(:life-span -1 ;infinite life-span
                                       :do-collisions? t
                                       :force-fields ,(list (make-instance 'constant-force-field
                                                                           :force-vector (p! 0 -0.05 0)))))))
      (setf (use-point-colors? p-sys) nil) ;point colors are white by default, ignore them
      (add-shape *scene* p-source)
      (add-shape *scene* p-sys)
      (add-motion *scene* p-sys))))
;;; hold down space key in 3D view to run animation

;;; for automated testing
(update-scene *scene* 30)

#|
(Demo 14 particle) particle system attributes ==================================

Create a row particle systems with varrying attributes.
|#

(format t "  particle-system 14...~%") (finish-output)

(with-clear-scene
  (defparameter *p-sys-1* (make-particle-system-from-point
                           (p! -8 0 0) 1 (p! 0 .2 0) (p! 0 .2 0)
                           :particle-class 'particle
                           :particle-initargs `(:life-span 5)))
  (defparameter *p-sys-2* (make-particle-system-from-point
                           (p! -4 0 0) 1 (p! 0 .2 0) (p! 0 .2 0)
                           :particle-class 'particle
                           :particle-initargs `(:life-span 5
                                                ;; narrower branching
                                                :spawn-angle ,(range-float 10.0 5.0))))
  (defparameter *p-sys-3* (make-particle-system-from-point
                           (p! 0 0 0) 1 (p! 0 .2 0) (p! 0 .2 0)
                           :particle-class 'particle
                           :particle-initargs `(:life-span 5
                                                ;; shorter branching
                                                :spawn-velocity-factor ,(range-float .5 .2))))
  (defparameter *p-sys-4* (make-particle-system-from-point
                           (p! 4 0 0) 1 (p! 0 .2 0) (p! 0 .2 0)
                           :particle-class 'particle
                           :particle-initargs `(:life-span 5
                                                ;; twisty branches
                                                :update-angle ,(range-float 20.0 10.0))))
  (defparameter *p-sys-5* (make-particle-system-from-point
                           (p! 8 0 0) 1 (p! 0 .2 0) (p! 0 .2 0)
                           :particle-class 'particle
                           :particle-initargs `(:life-span 5
                                                ;; very narrow branching
                                                :spawn-angle ,(range-float 5.0 2.5)
                                                ;; twisty branches
                                                :update-angle ,(range-float 10.0 5.0))))
  (add-shapes *scene* (list *p-sys-1* *p-sys-2* *p-sys-3* *p-sys-4* *p-sys-5*))
  (add-motions *scene* (list *p-sys-1* *p-sys-2* *p-sys-3* *p-sys-4* *p-sys-5*)))

;;; hold down space key in 3D view to run animation

(update-scene *scene* 30)               ;do update for batch testing


#|
(Demo 15 particle) dynamic particle system =====================================

Dynamic particles growing from a superquadric with a time-varying force field.
|#

(format t "  particle-system 15...~%") (finish-output)

(with-clear-scene
  (let ((p-source (freeze-transform (make-superquadric 16 8 2.0 1.0 1.0))))
    (setf (point-source-use-face-centers? p-source) t) ;comment out to use poly points
    (let ((p-sys (make-particle-system-from-point-source
                  p-source
                  :vel-fn (lambda (v) (p:scale v 0.2))
                  :particle-class 'dynamic-particle
                  :particle-initargs `(:life-span -1 ;infinite life-span
                                       :do-collisions? nil
                                       :force-fields ,(list (make-instance 'time-varying-force-field
                                                                           :force-vector (p! 0 -0.0 0)
                                                                           :noise-frequency 100
                                                                           :noise-amplitude 4.0))))))
      (setf (use-point-colors? p-sys) nil) ;point colors are white by default, ignore them
      (add-shape *scene* p-source)
      (add-shape *scene* p-sys)
      (add-motion *scene* p-sys))))
;;; hold down space key in 3D view to run animation

;;; for automated testing
(update-scene *scene* 30)

#|
(Demo 16 particle) particle system from a point source =========================

Create particles from a point source (3d grid).
|#

(format t "  particle-system 16...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-point-cloud (make-grid-points 5 5 5 (p! -2 -2 -2) (p! 2 2 2))))
         (p-sys (make-particle-system-from-point-source
                 shape
                 :vel-fn (lambda (v)
                   (declare (ignore v))
                   (p! 0 0 0))
                 :particle-class 'dynamic-particle
                 :particle-initargs `(:life-span -1 ;infinite life-span
                                      :do-collisions? nil
                                      :force-fields ,(list (make-instance '3d-noise-force-field
                                                                          :noise-frequency 0.5
                                                                          :noise-amplitude 0.5))))))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 17 particle) particle system with varying colors =========================

Set particle colors randomly.
|#

(format t "  particle-system 17...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point
                (p! 0 1 0) 10 (p! -.2 .1 -.2) (p! .2 .3 .2)
                :particle-class 'dynamic-particle
                :particle-initargs `(:life-span 20
                                     :update-color-fn ,(particle-random-color-fn)
                                     :do-collisions? t
                                     :elasticity 0.8
                                     :force-fields ,(list (make-instance 'constant-force-field
                                                                         :force-vector (p! 0 -.02 0)))))))
    (setf (draw-live-points-only? p-sys) nil)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)
    ))

#|
(Demo 18 particle) particle system with varying colors =========================

Set particle colors based on velocity.
|#

(format t "  particle-system 18...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point
                (p! -3 3 0) 100 (p! .1 0 -.1) (p! .2 .02 .1)
                :particle-class 'dynamic-particle
                :particle-initargs `(:life-span -1 ;infinite life-span
                                     :update-color-fn ,(particle-velocity-color-fn 0.0 (c! 0 0 1) 0.25 (c! 1 1 1))
                                     :do-collisions? t
                                     :elasticity 0.8
                                     :force-fields ,(list (make-instance 'constant-force-field
                                                                         :force-vector (p! 0 -.02 0)))))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)
    ))

#|
(Demo 19 particle) particle system with varying colors =========================

Set particle colors based on velocity and draw as streaks.
|#

(format t "  particle-system 19...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point
                (p! -3 3 0) 100 (p! .1 0 -.1) (p! .2 .02 .1)
                :particle-class 'dynamic-particle
                :particle-initargs `(:life-span 20
                                     :update-color-fn ,(particle-velocity-color-fn 0.0 (c! 0 0 1) 0.5 (c! 1 1 1))
                                     :do-collisions? t
                                     :elasticity 0.8
                                     :force-fields ,(list (make-instance 'constant-force-field
                                                                         :force-vector (p! 0 -.02 0)))))))
    (setf (draw-as-streaks? p-sys) t)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)
    ))

#|
(Demo 20 particle) particle system from points on surface of a polyhedron  =====

|#

(format t "  particle-system 20...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-sphere-uv-mesh 2.0 8 16)) 
         (p-sys (make-particle-system-from-point-source
                 (generate-point-cloud shape 100.0)
                 :vel-fn (lambda (v) (p:scale v 0.1))
                 :particle-class 'particle
                 :particle-initargs `(:life-span -1
                                      :update-angle ,(range-float 10.0 5.0)))))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 21 particle) particle system with emitter ================================

|#

(format t "  particle-system 21...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-circle-curve 2 16))
         (p-sys (make-particle-system-with-emitter (lambda () shape)
                                                   :vel-fn (lambda (v) (p:scale v .1))
                                                   :particle-class 'particle
                                                   :particle-initargs `(:life-span -1
                                                                        :update-angle ,(range-float 10.0 5.0)))))
    (setf (draw-as-streaks? p-sys) t)
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 22 particle) particle system with emitter ================================

|#

(format t "  particle-system 22...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-sphere-uv-mesh 2.0 8 16))
         (p-sys (make-particle-system-with-emitter (lambda () shape)
                                                   :vel-fn (lambda (v) (p:scale v .1))
                                                   :particle-class 'particle
                                                   :particle-initargs `(:life-span -1
                                                                        :update-angle ,(range-float 10.0 5.0)))))
    (setf (draw-as-streaks? p-sys) t)
    (setf (use-point-colors? p-sys) nil) ;point colors are white by default, ignore them
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing


#|
(Demo 23 particle) particle system with emitter and colors =====================

|#

(format t "  particle-system 23...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-sphere-uv-mesh 2.0 8 16)))
    (setf (draw-colored-points? shape) t)
    (set-point-colors-by-xyz shape (lambda (p) (c-rainbow (clamp (tween (p:y p) -1.0 1.0) 0.0 1.0))))
    (let ((p-sys (make-particle-system-with-emitter (lambda () shape)
                                                    :vel-fn (lambda (v) (p:scale v .1))
                                                    :particle-class 'particle
                                                    :particle-initargs `(:life-span 10
                                                                         :do-spawn? nil
                                                                         :update-angle ,(range-float 10.0 5.0)))))
      (setf (draw-as-streaks? p-sys) t)
      (add-shape *scene* shape)
      (add-shape *scene* p-sys)
      (add-motion *scene* p-sys))))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing


#|
END ============================================================================
|#
