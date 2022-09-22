(in-package #:kons-9)


#|
The UV-MESH class is a polyhedral mesh class which is made up of a 2D array of
points. The mesh is parametrized in U and V, allowing for generation of shapes
using a variety of procedural techniques such as sweeps.

UV-MESH inherits from POLYHEDRON and internally creates polygonal faces from
its 2D array of points. In this way it renders itself as a POLYHEDRON.

The demos below demonstrate examples of generating UV-MESH instances.
|#

#|
(Demo 01 uv-mesh) basic shapes =================================================

Some functions which generate basic shapes.
|#
(with-clear-scene
  (add-shape *scene* (translate-to (make-grid-uv-mesh 3 1.5 1 1) (p! 0 0 -6.0)))
  (add-shape *scene* (translate-to (make-cylinder-uv-mesh 1.5 3 16 4) (p! 0 0 -4.0)))
  (add-shape *scene* (translate-to (make-cone-uv-mesh 2 2 16 7) (p! 0 0 -2.0)))
  (add-shape *scene* (translate-to (make-rect-prism-uv-mesh 1.5 3 4 2) (p! 0 0 0.0)))
  (add-shape *scene* (translate-to (make-pyramid-uv-mesh 2 2 5 3) (p! 0 0 2.0)))
  (add-shape *scene* (translate-to (make-torus-uv-mesh 1.0 2.0 8 32) (p! 0 0 4.0)))
  (add-shape *scene* (translate-to (make-sphere-uv-mesh 1.5 8 16) (p! 0 0 6.0)))
  )

#|
(Demo 02 uv-mesh) transform-extrude-uv-mesh ====================================

A transform extrude transforms a profile POLYGON and sweeps out a UV-MESH.
|#
(with-clear-scene
  (add-shape *scene* (transform-extrude-uv-mesh (make-rectangle-curve 2 2 2)
                                                (make-euler-transform (p! 2 1 4) (p! 90 90 60) (p! 1 .5 .2))
                                                16)))

#|
(Demo 03 uv-mesh) transform-extrude-uv-mesh ====================================

Another example of a transform extrude.
|#
(with-clear-scene
  (add-shape *scene* (transform-extrude-uv-mesh (make-circle-curve 2.0 16)
                                                (make-euler-transform (p! 0 0 4) (p! 0 0 360) (p! 2 .2 1))
                                                40)))

#|
(Demo 04 uv-mesh) transform-extrude-uv-mesh rotate-pivot =======================

A transform extrude with a rotate pivot.
|#
(with-clear-scene
  (let ((xform (make-euler-transform (p! 0 0 4) (p! 0 0 360) (p! 2 .2 1))))
    (setf (pivot (rotate xform)) (p! 1 0 0))
;;    (setf (operator-order xform) :trs) ;comment out to test effect of operator order
    (add-shape *scene* (transform-extrude-uv-mesh (make-circle-curve 2.0 16)
                                                  xform
                                                  40))))

#|
(Demo 05 uv-mesh) transform-extrude-uv-mesh scale-pivot ========================

A transform extrude with a scale pivot.
|#
(with-clear-scene
  (let ((xform (make-euler-transform (p! 0 0 4) (p! 0 0 360) (p! 2 .2 1))))
    (setf (pivot (scale xform)) (p! 1 2 0))
    (add-shape *scene* (transform-extrude-uv-mesh (make-circle-curve 2.0 16)
                                                  xform
                                                  40))))

#|
(Demo 06 uv-mesh) transform-extrude-uv-mesh generalized-transform ==============

Using a GENERALIZED-TRANSFORM. Should exactly match (Demo 05 uv-mesh).
|#
(with-clear-scene
  (let ((xform (make-instance 'generalized-transform
                              :operators
                              (list (make-instance 'translate-operator :offset (p! 0 0 4))
                                    (make-instance 'euler-rotate-operator :angles (p! 0 0 360))
                                    (make-instance 'scale-operator
                                                   :scaling (p! 2 .2 1)
                                                   :pivot (p! 1 2 0))))))
    (add-shape *scene* (transform-extrude-uv-mesh (make-circle-curve 2.0 16)
                                                  xform
                                                  40))))

#|
(Demo 07 uv-mesh) sweep-extrude-uv-mesh ========================================

A sweep extrude operation creates a UV-MESH by sweeping a profile CURVE along
a path CURVE. The profile and path can be open or closed.
|#
(with-clear-scene 
  (let* ((path (make-sine-curve-curve 360 1 4 2 64))
         (prof (make-circle-curve 1.0 4))
         (mesh (sweep-extrude-uv-mesh prof path :twist (* 2 pi) :taper 0.0)))
    (add-shape *scene* mesh)))
#|
Assign point colors to the UV-MESH by uv.
|#
(set-point-colors-by-uv (first (shapes *scene*))
                        (lambda (u v) (declare (ignore u)) (c-rainbow v)))
#|
Assign point colors to the UV-MESH by xyz.
|#
(set-point-colors-by-xyz (first (shapes *scene*))
                         (lambda (p) (c-rainbow (clamp (tween (p:y p) -2 2) 0.0 1.0))))

#|
(Demo 08 uv-mesh) function-extrude-uv-mesh ====================================

A function extrude operation sweeps out a profile CURVE using a function to
allow for more general procedural modeling.
|#
(with-clear-scene
  (add-shape *scene* (function-extrude-uv-mesh
                      (make-circle-curve 2.0 16)
                      (lambda (points factor)
                       (map 'vector (lambda (p)
                                      (p+ (p-jitter p (* .2 factor)) (p! 0 0 (* 4 factor))))
                            points))
                      20)))

#|
(Demo 09 uv-mesh) function-extrude-uv-mesh ====================================

Another example of a function extrude.
|#
(with-clear-scene
  (add-shape *scene* (function-extrude-uv-mesh
                      (make-circle-curve 2.0 16)
                      (lambda (points factor)
                       (map 'vector (lambda (p)
                                      (p+ (p* p (sin (* pi factor)))
                                          (p! 0 0 (* 4 factor))))
                            points))
                      20)))

#|
END ============================================================================
|#
