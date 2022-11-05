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
  (let ((p-sys (make-particle-system-from-point (p! 0 1 0) 10 (p! -.5 0 -.5) (p! .5 1 .5)
                                                'particle
                                                :life-span 5)))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 02 particle) particle system from a point source =========================

Create particles from a point source (points of a curve). The particles get
their velocities from the curve tangents.
|#

(format t "  particle-system 02...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-circle-curve 2 16))
         (p-sys (make-particle-system-from-point-source shape
                                                        nil
                                                        'particle
                                                        :life-span 5)))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 03 particle) particle system from a point source, custom velocities ======

Provide function to modify particle initial velocities.
|#

(format t "  particle-system 03...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point-source (make-circle-curve 2 16)
                                                       (lambda (v) (p:normalize (p+ v (p! 0 2 0))))
                                                       'particle
                                                       :life-span 5)))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 04 particle) particle system with wriggle ================================

Randomized particle velocities' update-angle to give "wriggle" effect.
|#

(format t "  particle-system 04...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-icosahedron 2))
         (p-sys (make-particle-system-from-point-source shape
                                                        (lambda (v) (p:scale v 0.2))
                                                        'particle
                                                        :life-span 10
                                                        :update-angle (range-float (/ pi 8) (/ pi 16)))))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 05 particle) dynamic particle system with force field ====================

Create dynamic particles with constant force field simulating gravity. Do
collisions with ground plane.
|#

(format t "  particle-system 05...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point (p! 0 1 0) 10 (p! -.2 0 -.2) (p! .2 .5 .2)
                                                'dynamic-particle
                                                :life-span 20
                                                :do-collisions? t
                                                :elasticity 0.8
                                                :force-fields (list (make-instance 'constant-force-field
                                                                                   :force-vector (p! 0 -.02 0))))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 100)               ;do update for batch testing

#|
(Demo 06 particle) dynamic particle system with wriggle ========================

Create dynamic particles with wriggle effect.
|#

(format t "  particle-system 06...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point (p! 0 1 0) 10 (p! -.2 0 -.2) (p! .2 .5 .2)
                                                'dynamic-particle
                                                :life-span 20
                                                :update-angle (range-float (/ pi 8) (/ pi 16))
                                                :do-collisions? t
                                                :elasticity 0.95
                                                :force-fields (list (make-instance 'constant-force-field
                                                                                   :force-vector (p! 0 -.05 0))))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 100)               ;do update for batch testing

#|
(Demo 07 particle) dynamic particle system with an attractor force field =======

Simulate particles in orbit.
|#

(format t "  particle-system 07...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point-source
                (make-circle-curve 4 16)
                (lambda (v) (p:scale (p:normalize (p+ v (p-rand))) 0.2))
                'dynamic-particle
                :life-span -1 ;infinite life-span
                :do-collisions? nil
                :force-fields (list (make-instance 'attractor-force-field
                                                   :location (p! 0 0 0)
                                                   :magnitude 0.1)))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 100)               ;do update for batch testing

#|
(Demo 08 particle) dynamic particle system with a noise force field ============

Dynamic particles growing from a height field under the influence of a noise
force field.
|#

(format t "  particle-system 08...~%") (finish-output)

(with-clear-scene
  (let* ((shape (freeze-transform
                 (translate-by (make-heightfield 20 20 (p! -5 0 -5) (p! 5 0 5)
                                                 :height-fn (lambda (x z)
                                                              (* 4 (turbulence (p! x 0 z) 4))))
                               (p! 0 -1 0))))
         (p-sys (make-particle-system-from-point-source
                 shape
                 (lambda (v) (p:scale v 0.05))
                 'dynamic-particle
                 :life-span -1 ;infinite life-span
                 :do-collisions? nil
                 :force-fields (list (make-instance 'noise-force-field
                                                    :noise-frequency 0.2
                                                    :noise-amplitude 0.2)))))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 60)               ;do update for batch testing

#|
(Demo 09 particle) climbing particle system ====================================

Climbing particles which follow the surface of a shape.
|#

(format t "  particle-system 09...~%") (finish-output)

(with-clear-scene
  (let* ((shape (triangulate-polyhedron (make-cube-sphere 6.0 3)))
         ;(shape (triangulate-polyhedron (make-cube 6)))
         ;(shape (triangulate-polyhedron (make-grid-uv-mesh 3 3 1 1)))
         (p-sys (make-particle-system-from-point (p! .5 3.5 0) 10 (p! -.5 0 -.5) (p! .5 0 .5)
                                                 'climbing-particle
                                                 :support-polyh shape
                                                 :update-angle (range-float (/ pi 8) (/ pi 16))
                                                 :life-span 10)))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation -- gets slow, need to optimize
;;; suggestion: turn off filled display for a better view (TAB, D, 1)

(update-scene *scene* 30)               ;do update for batch testing

#|
(Demo 10 particle) particle system from polyhedron faces =======================

Generating particles from polyhedron face centroids (as opposed to vertices).
|#

(format t "  particle-system 10...~%") (finish-output)

(with-clear-scene
  (let ((p-source (make-icosahedron 2.0)))
    (setf (point-source-use-face-centers? p-source) t) ;comment out to use poly points
    (let* ((p-sys (make-particle-system-from-point-source p-source
                                                          (lambda (v) (p:scale v 0.1))
                                                          'particle
                                        :life-span 10
                                        :update-angle (range-float (/ pi 16) (/ pi 32))
                                        :spawn-angle (range-float (/ pi 8) (/ pi 16))))
           (sweep-mesh-group (make-sweep-mesh-group (make-circle 0.2 6) p-sys
                                                    :taper 0.0 :twist 0.0)))
      (add-shape *scene* p-source)
      ;; (add-shape *scene* p-sys)         ;no need to add p-sys shape to scene
      (add-shape *scene* sweep-mesh-group)
      (add-motion *scene* p-sys))))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 30)               ;do update for batch testing

#|
(Demo 11 particle) climbing particle system ====================================

Climbing particles which follow the surface of a shape.
|#

(format t "  particle-system 11...~%") (finish-output)

(with-clear-scene
  (let* ((shape (import-obj (asdf:system-relative-pathname "kons-9" "test/data/cow.obj")))
         (p-sys (make-particle-system-from-point (p! .5 1.8 0) 10 (p! -.5 0 -.5) (p! .5 0 .5)
                                                 'climbing-particle
                                                 :support-polyh shape
                                                 :update-angle (range-float (/ pi 8) (/ pi 16))
                                                 :life-span 10)))
;    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 30)               ;do update for batch testing

#|
(Demo 12 particle) dynamic particle system =====================================

Dynamic particles growing from a superquadric with a gravity force field.
|#

(format t "  particle-system 12...~%") (finish-output)

(with-clear-scene
  (let ((p-source (freeze-transform (translate-by (make-superquadric 16 8 2.0 1.0 1.0)
                                                  (p! 0 2 0)))))
    (setf (point-source-use-face-centers? p-source) t) ;comment out to use poly points
    (let ((p-sys (make-particle-system-from-point-source
                  p-source
                  (lambda (v) (p:scale v 0.2))
                  'dynamic-particle
                  :life-span -1 ;infinite life-span
                  :do-collisions? t
                  :force-fields (list (make-instance 'constant-force-field
                                                     :force-vector (p! 0 -0.05 0))))))
      (add-shape *scene* p-source)
      (add-shape *scene* p-sys)
      (add-motion *scene* p-sys))))
;;; hold down space key in 3D view to run animation

;;; for automated testing
(update-scene *scene* 30)

#|
END ============================================================================
|#
