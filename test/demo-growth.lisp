(in-package #:kons-9)

#|
Greetings and welcome to kons-9!

These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

The demos below will walk you through some of the features of the software. You
will evaluate the expressions in each demo and the graphics in the window will
update accordingly.

We also assume that you are familiar with the basics of 3D graphics.

As of this writing, kons-9 runs on MacOS (Intel & M1), Linux. and Windows.

We hope you find the system enjoyable and useful.
|#


#|
(Demo 01 growth) grow and consume resource =====================================

This demo creates a growth-environment which contains resource values in a
spacial distribution, specified by the distance from a sine curve.

A number of growth-feature items are placed in the environment, and at each
frame, they grow in size and consume the nearby resource.

Press the space key to advance one frame at a time.
|#
(format t "  growth 1...~%") (finish-output)

(with-clear-scene
  (let ((env (make-growth-environment 40 2 40 ;don't need cells in Y, resource is only on "ground"
                                      :bounds-lo (p! -10 0 -10) :bounds-hi (p! 10 0 10)))
        (curve (make-sine-curve 360 1 20 8 64)))
    ;; place curve in world space
    (translate-by curve (p! -10 0 0))
    (rotate-by curve (p! 90 0 0))
    (freeze-transform curve)
    ;; add scene data
    (add-shapes *scene* (list env curve))
    (add-motion *scene* env)
    ;; set field values with limits
    (apply-field-function (field env)
                          (curve-source-field-fn curve (in-out-cubic-value-fn 1.0 10.0))
                          :min-value (field-min-value env) :max-value (field-max-value env))
    ;; generate features which grow and consume resources
    (dotimes (i 100)
      (let ((feature (make-instance 'growth-feature
                                    :resource-usage-amount 0.15 :resource-usage-extent 2.0)))
        (create-feature-at-point env feature (p-rand2 (p! -10 0 -10) (p! 10 0 10)))))
    ))

#|
(Demo 02 growth) growth-grass-clump ============================================

In this demo, we replace growth-feature with a subclass, growth-grass-clump,
which grow using a particle system. Here, :resource-usage-amount is turned off
(set to 0.0 below). The features grow where there is available resource near
the curve.

Press the space key to advance one frame at a time.
|#
(format t "  growth 2...~%") (finish-output)

(with-clear-scene
  (let ((env (make-growth-environment 40 5 40
                                      :bounds-lo (p! -10 0 -10) :bounds-hi (p! 10 5 10)))
        (curve (make-sine-curve 360 1 20 8 64)))
    ;; set to t to draw grid
    (setf (show-grid? env) nil)
    ;; place curve in world space
    (translate-by curve (p! -10 0 0))
    (rotate-by curve (p! 90 0 0))
    (freeze-transform curve)
    ;; add scene data
    (add-shapes *scene* (list env curve))
    (add-motion *scene* env)
    ;; set field values with limits
    (apply-field-function (field env)
                          (curve-source-field-fn curve (in-out-cubic-value-fn 1.0 2.0))
                          :min-value (field-min-value env) :max-value (field-max-value env))
    ;; generate features which DO NOT consume resources (:resource-usage-amount is 0.0)
    (dotimes (i 100)
      (let ((feature (make-instance 'growth-grass-clump
                                    :resource-usage-amount 0.0 :resource-usage-extent 1.0)))
        (create-feature-at-point env feature (p-rand2 (p! -10 0 -10) (p! 10 0 10)))))
    ))

#|
(Demo 03 growth) growth-grass-clump ============================================

In this demo, the features consume resources casting "shadows" from particle
positions in a cylidrical volume below the particle. This prevents other grass
stalks from growing under them.

The environment resource is initialized to a constant value and te features are
grown in a confined volume so the effect of resource usage can be better seen.

Press the space key to advance one frame at a time.
|#
(format t "  growth 3...~%") (finish-output)

(with-clear-scene
  (let ((env (make-growth-environment 40 20 40 ;"light" resource extends in Y
                                      :bounds-lo (p! -10 0 -10) :bounds-hi (p! 10 10 10))))
    (setf (show-grid? env) nil)
    ;; add scene shapes
    (add-shape *scene* env)
    (add-motion *scene* env)
    ;; set field values to constant
    (set-field-constant-value (field env) 1.0)
    ;; generate features
    (dotimes (i 10)
      (let ((feature (make-instance 'growth-grass-clump ;consume resource (cast "shadows")
                                    :resource-usage-amount 0.2 :resource-usage-extent 1.0)))
        (create-feature-at-point env feature (p-rand2 (p! -3 0 -3) (p! 3 0 3)))))
    ;; isosurface for visualization
    (when nil                           ;set to t to create animated isosurface
      (let ((iso (generate-isosurface (make-instance 'isosurface :field (field env) :threshold 0.01))))
        (add-shape *scene* iso)
        ;; turn off backface culling (isosurface is reversed)
        (setf *do-backface-cull?* nil)
        ;; add animator
        (add-motion *scene*
                    (make-instance 'animator
                                   :update-fn (lambda ()
                                                (generate-isosurface iso))))))
    ))

#|
(Demo 04 growth) grow on heightfield ===========================================

In this demo the features are grown on a heightfield. The number of features
makes each frame slow to update. Potential ofr code optimization at some later
date.

Press the space key to advance one frame at a time.
|#
(format t "  growth 4...~%") (finish-output)

(with-clear-scene
  (let ((env (make-growth-environment 40 20 40 ;"light" resource extends in Y
                                      :bounds-lo (p! -10 0 -10) :bounds-hi (p! 10 10 10)))
        (hfield (make-heightfield 40 40 (p! -10 0 -10) (p! 10 0 10)
                                  :height-fn (lambda (x z)
                                               (max 0.1 (- (* 20 (turbulence (p:scale (p! x 0 z) .25) 4)) 8))))))
    
    (setf (show-grid? env) nil)
    ;; add scene shapes
    (add-shapes *scene* (list env hfield))
    (add-motion *scene* env)
    ;; initialize field values
    (set-field-constant-value (field env) 1.0)
    ;; set heightfield color
    (set-point-colors-by-xyz hfield (lambda (p) (c-lerp (clamp (tween (p:y p) 0.0 5) 0.0 1.0)
                                                        (c! .5 .4 0)
                                                        (c! .9 .8 .2))))
    ;; generate features
    (dotimes (i 100)
      (let* ((feature (make-instance 'growth-grass-clump
                                     :resource-usage-amount 0.2 :resource-usage-extent 1.0))
             (xz-location (p-rand2 (p! -9 0 -9) (p! 9 0 9)))
             (height (height-at-point hfield xz-location))
             (location (p! (p:x xz-location) height (p:z xz-location))))
        (create-feature-at-point env feature location)))
    ))

