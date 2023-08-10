(in-package #:kons-9)

#|
These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

The SPRITE class is a shape which always faces the camera.

The demos below demonstrate examples of generating SPRITE instances.
|#

#|
(Demo 01 uv-mesh) basic shapes =================================================

Some functions which generate basic sprites.
|#

(format t "  sprite 1...~%") (finish-output)

(with-clear-scene
  (add-shape *scene* (translate-to (make-cone-uv-mesh 2 4 16 7) (p! 0 0 0)))

  ;;; add after solid shapes so z-buffer and transparency work properly
  (add-shape *scene* (make-instance
                      'sprite-instancer
                      :geometry (make-circle-curve 2.0 16)
                      :point-source (set-point-colors-by-xyz
                                     (make-point-cloud (make-grid-points 10 4 10
                                                                         (p! -5 -2 -5)
                                                                         (p! 5 2 5)))
                                     (lambda (p)
                                       (c-rainbow (clamp (tween (p:x p) -5 5) 0.0 1.0) 0.1)))))
  )


(with-clear-scene
  (add-shape *scene* (make-instance
                      'sprite-instancer
                      :geometry (make-circle-curve .25 8)
                      :point-source (set-point-colors (make-torus-uv-mesh 1.0 2.0 32 128)
                                                      (c! 0 0 1 .03))))
  )


(with-clear-scene
  (let ((p-sys (make-particle-system-from-point
                (p! 0 1 0) 10 (p! -.2 .1 -.2) (p! .2 .3 .2)
                'dynamic-particle
                :life-span 20
                :update-color-fn (particle-random-color-fn)
                :do-collisions? t
                :elasticity 0.8
                :force-fields (list (make-instance 'constant-force-field
                                                   :force-vector (p! 0 -.02 0))))))
    (setf (draw-live-points-only? p-sys) t)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)

    (add-shape *scene* (make-instance
                        'sprite-instancer
                        :geometry (make-circle-curve .25 16)
                        :point-source p-sys))
    ))

(with-clear-scene
  (let ((p-sys (make-particle-system-from-point
                (p! -3 3 0) 100 (p! .1 0 -.1) (p! .2 .02 .1)
                'dynamic-particle
                :life-span -1
                :update-color-fn (particle-velocity-color-fn 0.0 (c! 0 0 1) 0.5 (c! 1 1 1))
                :do-collisions? t
                :elasticity 0.8
                :force-fields (list (make-instance 'constant-force-field
                                                   :force-vector (p! 0 -.02 0))))))
    (setf (draw-live-points-only? p-sys) t)
    (setf (draw-as-streaks? p-sys) t)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)

    ;; (add-shape *scene* (make-instance
    ;;                     'sprite-instancer
    ;;                     :geometry (make-circle-curve .25 16)
    ;;                     :point-source p-sys))
    ))

#|
END ============================================================================
|#
