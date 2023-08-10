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

(format t "  sprite 1...~%") (finish-output)

(with-clear-scene
  (add-shape *scene* (make-instance
                      'sprite-instancer
                      :geometry (make-circle-curve 2.0 16)
                      :point-source (make-point-cloud (vector (p! 0 0 0))))))

;;; rotate the view and notice the sprite always faces the camera

#|
(Demo 02 sprite) 3D grid of transparent colored sprites ========================

|#

(format t "  sprite 2...~%") (finish-output)

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

(format t "  sprite 3...~%") (finish-output)

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

(format t "  sprite 4...~%") (finish-output)

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

(format t "  sprite 5...~%") (finish-output)

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

#|
END ============================================================================
|#
