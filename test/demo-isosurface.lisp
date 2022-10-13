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
(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field 20 20 20)
                                      (lambda (p) (- 1.0 (p-mag p)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 02 isosurface) point source protocol =====================================

Use a point source (a POINT-CLOUD) with APPLY-FIELD-FUNCTION to set the values of
a SCALAR-FIELD. The POINT-SOURCE-FIELD-FN has a 1/r^2 falloff from its points.
|#
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
|#
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

#|
(Demo 05 isosurface) animating point source locations ==========================

Animate the point locations of the POINT-CLOUD.
|#
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
#|
END ============================================================================
|#
