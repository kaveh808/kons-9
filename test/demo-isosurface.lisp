(in-package #:kons-9)

#|
The ISOSURFACE class is a polyhedral mesh class which generates a surface at
a given threshold of a SCALAR-FIELD.

ISOSURFACE inherits from POLYHEDRON and internally creates polygonal (triangle)
faces. In this way it renders itself as a POLYHEDRON.

The demos below demonstrate examples of generating ISOSURFACE instances.
|#

#|
(Demo 01 isosurface) basic field function ======================================

Use a custom field function with APPLY-FIELD-FUNCTION to set the values of
a SCALAR-FIELD. The field values fall off uniformly from the origin, resulting
in a spherical isosurface.
|#
(format t "  isosurface 1...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field 20 20 20)
                                      (lambda (p) (- 1.0 (p:length p)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 02 isosurface) point source protocol =====================================

Use a point source (a POINT-CLOUD) with APPLY-FIELD-FUNCTION to set the values of
a SCALAR-FIELD. The POINT-SOURCE-FIELD-FN has a 1/r^2 falloff from its points.
|#
(format t "  isosurface 2...~%") (finish-output)

(with-clear-scene
  (let* ((p-cloud (make-point-cloud (make-grid-points 2 2 2 (p! -0.5 -0.5 -0.5) (p! 0.5 0.5 0.5))))
         (field (apply-field-function (make-scalar-field 40 40 40)
                                      (point-source-field-fn p-cloud
                                                             :strength 1.0 :falloff 1.2)))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 9.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 03 isosurface) point source protocol =====================================

Similar to Demo 02, but using custom field bounds and a random point source.
|#
(format t "  isosurface 3...~%") (finish-output)

(with-clear-scene
  (let* ((p-cloud (make-point-cloud (make-random-points 30 (p! -3 0 -3) (p! 3 1 3))))
         (field (apply-field-function (make-scalar-field 40 10 40
                                                         :bounds-lo (p! -4 -2 -4)
                                                         :bounds-hi (p! 4 2 4))
                                      (point-source-field-fn p-cloud
                                                             :strength 1.0 :falloff 1.0)))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 8.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 04 isosurface) animating isosurface threshold ============================

Animate the threshold value of the ISOSURFACE.

Press 'space' to run the animation.
|#
(format t "  isosurface 4...~%") (finish-output)

(with-clear-scene
  (let* ((p-cloud (make-point-cloud (make-grid-points 2 2 2 (p! -0.5 -0.5 -0.5) (p! 0.5 0.5 0.5))))
         (field (apply-field-function (make-scalar-field 40 40 40)
                                      (point-source-field-fn p-cloud :strength 1.0 :falloff 1.2)))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 20.0))))
    (add-shape *scene* iso)
    ;; set end frame
    (setf (end-frame *scene*) 60)
    ;; add animator
    (add-motion *scene*
                (make-instance 'animator
                               :setup-fn (lambda ()
                                           (setf (threshold iso) 9.0)
                                           (generate-isosurface iso))
                               :update-fn (lambda ()
                                            (setf (threshold iso)
                                                  (lerp (tween (current-frame *scene*) 0 60)
                                                        20.0 0.0))
                                           (generate-isosurface iso)))))
  )

;; TODO -- BUG -- crashes because face-normals and point-normals arrays are empty at
;; some frame, which does not happen when frames are rendered by pressing `space`
;;(update-scene *scene* 60)               ;do update for batch testing

#|
(Demo 05 isosurface) animating point source locations ==========================

Animate the point locations of the POINT-CLOUD.

Press 'space' to run the animation.
|#
(format t "  isosurface 5...~%") (finish-output)

(with-clear-scene
  (let* ((p-cloud (make-point-cloud (make-random-points 30 (p! -3 0 -3) (p! 3 1 3))))
         (field (apply-field-function (make-scalar-field 40 10 40
                                                         :bounds-lo (p! -4 -2 -4)
                                                         :bounds-hi (p! 4 2 4))
                                      (point-source-field-fn p-cloud
                                                             :strength 1.0 :falloff 1.0)))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 8.0))))
    (add-shape *scene* iso)
    ;; add animator
    (add-motion *scene*
                (make-instance 'animator
                               :update-fn (lambda ()
                                            (randomize-points p-cloud (p! .1 0 .1))
                                            (apply-field-function field
                                                                  (point-source-field-fn p-cloud
                                                                                         :strength 1.0
                                                                                         :falloff 1.0))
                                            (generate-isosurface iso)))))
  )

;; TODO -- BUG -- crashes because face-normals and point-normals arrays are empty at
;; some frame, which does not happen when frames are rendered by pressing `space`
;;(update-scene *scene* 60)               ;do update for batch testing

#|
(Demo 06 isosurface) curve source protocol =====================================

Use a curve source (a CURVE) with APPLY-FIELD-FUNCTION to set the values of
a SCALAR-FIELD. The CURVE-SOURCE-FIELD-FN has a 1/r^2 falloff from its line
segments.
|#
(format t "  isosurface 6...~%") (finish-output)

(with-clear-scene
  (let* ((curve (make-line-curve (p! -0.5 -0.5 -0.5) (p! 0.5 0.5 0.5) 1))
         (field (apply-field-function (make-scalar-field 40 40 40)
                                      (curve-source-field-fn curve
                                                             :strength 1.0 :falloff 1.2)))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 9.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 07 isosurface) curve source protocol =====================================

Similar to Demo 06 but with a sine curve.
|#
(format t "  isosurface 7...~%") (finish-output)

(with-clear-scene
  (let* ((curve (make-sine-curve-curve 360 1 2 1 16))
         (field (apply-field-function (make-scalar-field 40 40 20
                                                         :bounds-lo (p! -1 -2 -1)
                                                         :bounds-hi (p!  3  2  1))
                                      (curve-source-field-fn curve
                                                             :strength 1.0 :falloff 1.2)))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 16.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 08 isosurface) curve source protocol =====================================

Use a polyhedron as a curve source.
|#
(format t "  isosurface 8...~%") (finish-output)

(with-clear-scene
  (let* ((polyh (make-dodecahedron 3.0))
         (field (apply-field-function (make-scalar-field 40 40 40
                                                         :bounds-lo (p! -2 -2 -2)
                                                         :bounds-hi (p!  2  2  2))
                                      (curve-source-field-fn polyh
                                                             :strength 1.0 :falloff 1.2)))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 80.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 09 isosurface) curve source protocol, animate iso threshold ==============

Use a polyhedron as a curve source.
|#
(format t "  isosurface 9...~%") (finish-output)

(with-clear-scene
  (let* ((polyh (make-dodecahedron 3.0))
         (field (apply-field-function (make-scalar-field 40 40 40
                                                         :bounds-lo (p! -2 -2 -2)
                                                         :bounds-hi (p!  2  2  2))
                                      (curve-source-field-fn polyh
                                                             :strength 1.0 :falloff 1.2)))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 10.0))))
    (add-shape *scene* iso)
    ;; set end frame
    (setf (end-frame *scene*) 60)
    ;; add animator
    (add-motion *scene*
                (make-instance 'animator
                               :setup-fn (lambda ()
                                           (setf (threshold iso) 10.0)
                                           (generate-isosurface iso))
                               :update-fn (lambda ()
                                            (setf (threshold iso)
                                                  (lerp (tween (current-frame *scene*) 0 60)
                                                        10.0 80.0))
                                           (generate-isosurface iso)))))
  )

#|
(Demo 10 isosurface) particle curve source protocol ============================

Use a particle system as a curve source.
|#
(format t "  isosurface 10...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system (make-point-cloud (vector (p! 0 0 0)))
                                     (p! 0 .2 0) 10 -1 'particle
                                     :update-angle (range-float (/ pi 8) (/ pi 16))
                                     :life-span 10)))
    ;; add particle system to scene as both a shape and a motion
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)
    ;;; run animation
    (update-scene *scene* 15)
    ;;; create field and isosurface from particle paths (curves)
    (let* ((field (apply-field-function (make-scalar-field 40 40 40
                                                           :bounds-lo (p! -2 0 -2)
                                                           :bounds-hi (p!  2 4  2))
                                        (curve-source-field-fn p-sys
                                                               :strength 1.0 :falloff 1.2)))
           (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 100.0))))
      (add-shape *scene* iso))))

#|
(Demo 11 isosurface) signed distance functions =================================

Use signed distance functions with APPLY-FIELD-FUNCTION to set the values of
a SCALAR-FIELD.

Comment in the desired function.
|#
(format t "  isosurface 11...~%") (finish-output)

(with-clear-scene
  ;; turn off backface culling as isosurfaces are reversed for some reason
  (setf *do-backface-cull?* nil)
  (let* ((field (apply-field-function (make-scalar-field 40 40 40)
                                      (lambda (p)
                                        ;; single shapes
                                        ;; (sd-sphere p 1.0))
                                        ;; (sd-box p (p! 0.2 0.4 0.8))
                                        ;; (sd-round-box p (p! 0.2 0.4 0.8) 0.15)
                                        ;; (sd-box-frame p (p! 0.5 0.3 0.5) .05)
                                        ;; (sd-torus p (p! .5 .2 0.0))
                                        ;; (sd-capped-torus p (p! .5 -.866 0.0) 0.4 0.1)
                                        ;; (sd-link p 0.13 0.2 0.09)
                                        ;; (sd-cylinder p (p! 0 0 0.5))
                                        ;; (sd-cone p (p! .5 .5 0) 1.5)
                                        ;; (sd-plane p (p! .707 .707 0) 0)
                                        ;; (sd-tri-prism p (p! 1 1 0))
                                        ;; (sd-vertical-capsule p .7 .2)
                                        ;; (sd-capped-cylinder p .7 .2)
                                        ;; (sd-cut-hollow-sphere p 0.8 .3 .1)
                                        ;; combination of functions
                                        (min (sd-cut-hollow-sphere p 0.7 .3 .1)
                                             (sd-box-frame p (p! 0.8 0.3 0.8) .05)
                                             (sd-capped-torus p (p! .5 -.866 0.0) 0.7 0.1))
                                        
                                      )))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 12 isosurface) voxel grid ================================================

Visualize an ISOSURFACE as a VOXEL-GRID-SHAPE. Note the isosurface does not
need to be generated.
|#
(format t "  isosurface 1...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field 20 20 20)
                                      (lambda (p) (- 1.0 (p:length p)))))
         (iso (make-instance 'isosurface :field field :threshold 0.0))
         (vox (make-voxel-grid iso)))
    (add-shape *scene* vox))
  )

#|
(Demo 13 isosurface) voxel grid, animating isosurface threshold ================

Animate the threshold value of the ISOSURFACE and visualize as a
VOXEL-GRID-SHAPE. Note the isosurface does not need to be generated.

Press 'space' to run the animation.
|#
(format t "  isosurface 4...~%") (finish-output)

(with-clear-scene
  (let* ((p-cloud (make-point-cloud (make-grid-points 2 2 2 (p! -0.5 -0.5 -0.5) (p! 0.5 0.5 0.5))))
         (field (apply-field-function (make-scalar-field 40 40 40)
                                      (point-source-field-fn p-cloud :strength 1.0 :falloff 1.2)))
         (iso (make-instance 'isosurface :field field :threshold 20.0))
         (vox (make-voxel-grid iso)))
    (add-shape *scene* vox)
    ;; set end frame
    (setf (end-frame *scene*) 60)
    ;; add animator
    (add-motion *scene*
                (make-instance 'animator
                               :setup-fn (lambda ()
                                           (setf (threshold iso) 20.0))
                               :update-fn (lambda ()
                                            (setf (threshold iso)
                                                  (lerp (tween (current-frame *scene*) 0 60)
                                                        20.0 0.0))))))
  )


#|
END ============================================================================
|#
