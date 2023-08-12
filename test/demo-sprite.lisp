(in-package #:kons-9)

#|
These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

A sprite is a shape which always faces the camera. Since sprites often
have transparency, a SPRITE-INSTANCER sorts its instances from back to front.
We do not currently handle the case of sorting between multiple
SPRITE-INSTANCERs. Sprites should be added to a scene after other shapes for
the z-buffer to work properly.

The demos below demonstrate examples of generating SPRITE instances.
|#

#|
(Demo 01 sprite) single sprite =================================================

|#

(format t "  sprite 01...~%") (finish-output)

(with-clear-scene
  (add-shape *scene* (make-instance
                      'sprite-instancer
                      :geometry (make-circle-curve 2.0 16)
                      :point-source (make-point-cloud (vector (p! 0 0 0))))))

;;; rotate the view and notice the sprite always faces the camera

#|
(Demo 02 sprite) 3D grid of transparent colored sprites ========================

|#

(format t "  sprite 02...~%") (finish-output)

(with-clear-scene
  (add-shape *scene* (make-instance
                      'sprite-instancer
                      :geometry (make-circle-curve 1.0 16)
                      :point-source (set-point-colors-by-xyz
                                     (make-point-cloud (make-grid-points 10 4 10
                                                                         (p! -3 -1 -3)
                                                                         (p! 3 1 3)))
                                     (lambda (p)
                                       (c-rainbow (clamp (tween (p:x p) -5 5) 0.0 1.0) 0.1))))))

#|
(Demo 03 sprite) 3D grid of transparent colored sprites and polyhedron =========

|#

(format t "  sprite 03...~%") (finish-output)

(with-clear-scene
  (add-shape *scene* (make-sphere-uv-mesh 4.0 8 16))

  ;;; add after solid shapes so z-buffer and transparency work properly
  (add-shape *scene* (make-instance
                      'sprite-instancer
                      :geometry (make-circle-curve 1.0 16)
                      :point-source (set-point-colors-by-xyz
                                     (make-point-cloud (make-grid-points 10 4 10
                                                                         (p! -3 -1 -3)
                                                                         (p! 3 1 3)))
                                     (lambda (p)
                                       (c-rainbow (clamp (tween (p:x p) -5 5) 0.0 1.0) 0.1))))))

#|
(Demo 04 sprite) sprites generated at polyhedron points ========================

|#

(format t "  sprite 04...~%") (finish-output)

(with-clear-scene
  (add-shape *scene* (make-instance
                      'sprite-instancer
                      :geometry (make-circle-curve .5 16)
                      :point-source (set-point-colors (make-torus-uv-mesh 2.0 4.0 32 128)
                                                      (c! 0 0 1 .03)))))

#|
(Demo 05 sprite) sprites generated from particle system ========================

Randomly varying particle colors. The sprites take their position and color from
the particles.
|#

(format t "  sprite 05...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point
                (p! 0 1 0) 10 (p! -.1 .1 -.1) (p! .1 .3 .1)
                'dynamic-particle
                :life-span 20
                :update-color-fn (particle-random-color-fn)
                :do-collisions? t
                :elasticity 0.8
                :force-fields (list (make-instance 'constant-force-field
                                                   :force-vector (p! 0 -.02 0))))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)

    (add-shape *scene* (make-instance
                        'sprite-instancer
                        :geometry (make-circle-curve .25 16)
                        :point-source p-sys))
    ))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 06 sprite) sprites generated from particle system ========================

Particle system with emitter.
|#

(format t "  sprite 06...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-circle-curve 2 64))
         (p-sys (make-particle-system-with-emitter (lambda () shape)
                                                   (lambda (v)
                                                     (declare (ignore v))
                                                     (p-rand 0.025))
                                                   'particle
                                                   :life-span -1
                                                   :update-color-fn (lambda (ptcl)
                                                                      (declare (ignore ptcl))
                                                                      (c! 0 1 0 0.05))
                                                   :update-angle (range-float 10.0 5.0))))
    (setf (draw-as-streaks? p-sys) t)
    (add-shape *scene* shape)
    (add-motion *scene* p-sys)

    (add-shape *scene* (make-instance
                        'sprite-instancer
                        :geometry (make-circle-curve .5 16)
                        :point-source p-sys))
    ))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 07 sprite) sprites generated from particle system ========================

Particle system with static (non-animated) emitter in world space and gravity.
Particle color animated based on age.
|#

(format t "  sprite 07...~%") (finish-output)

(with-clear-scene
  (let* ((shape (translate-to (make-circle-curve 2 64) (p! 0 3 0)))
         (p-sys (make-particle-system-with-emitter (lambda () (freeze-transform (duplicate shape)))
                                                   (lambda (v)
                                                     (declare (ignore v))
                                                     (p-rand 0.05))
                                                   'dynamic-particle
                                                   :life-span 50
                                                   :do-spawn? nil
                                                   :update-color-fn (particle-age-color-fn
                                                                     (c! 0 1 0 0.05)
                                                                     (c! 0 1 0 0.0))
                                                   :do-collisions? t
                                                   :elasticity 0.5
                                                   :force-fields (list (make-instance 'constant-force-field
                                                                                      :force-vector (p! 0 -.01 0))))))
    (setf (draw-as-streaks? p-sys) t)
    (add-shape *scene* shape)
    (add-motion *scene* p-sys)

    (add-shape *scene* (make-instance
                        'sprite-instancer
                        :geometry (make-circle-curve .5 16)
                        :point-source p-sys))
    ))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
(Demo 08 sprite) sprites generated from particle system ========================

Particle system with animated emitter in world space and gravity. Particle color
animated based on age.
|#

(format t "  sprite 07...~%") (finish-output)

(with-clear-scene
  (let* ((shape (translate-to (make-circle-curve 2 64) (p! 0 -2 0)))
         (p-sys (make-particle-system-with-emitter (lambda () (freeze-transform (duplicate shape)))
                                                   (lambda (v)
                                                     (declare (ignore v))
                                                     (p-rand 0.05))
                                                   'dynamic-particle
                                                   :life-span 30
                                                   :do-spawn? nil
                                                   :update-color-fn (particle-age-color-fn
                                                                     (c! 0 0 0 0.1)
                                                                     (c! 1 1 1 0.0))
                                                   :do-collisions? nil
                                                   :force-fields (list (make-instance 'constant-force-field
                                                                                      :force-vector (p! 0 .02 0))))))

    ;; animate emitter shape
    (add-motion *scene*
                (make-instance 'animator
                               :setup-fn (lambda ()
                                           (translate-to shape
                                                         (p! 0.0 -2 0)))
                               :update-fn (lambda ()
                                            (translate-to shape
                                                          (p! (* 4.0 (sin (* 3.0 (current-time *scene*)))) -2 0)))))

    (setf (draw-as-streaks? p-sys) t)
    (add-shape *scene* shape)
    (add-motion *scene* p-sys)

    (add-shape *scene* (make-instance
                        'sprite-instancer
                        :geometry (make-circle-curve .5 16)
                        :point-source p-sys))
    ))
;;; hold down space key in 3D view to run animation

(update-scene *scene* 20)               ;do update for batch testing

#|
END ============================================================================
|#
