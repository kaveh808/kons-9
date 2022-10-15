(in-package #:kons-9)

#|
The FLEX-ANIMATOR class animates a POLYHEDRON shape using simple mass-spring
dynamics. It is intended as a test platform for more complex behaviors.

The demos below demonstrate some examples of the dynamics.

Press 'space' after each demo to run the animation.
|#

#|
(Demo 01 flex-animator) gravity and collisions =================================

Create a FLEX-ANIMATOR with gravity.
|#
(with-clear-scene
  (let ((shape (make-tetrahedron 2.0)))
    (translate-to shape (p! 0 2 0))
    (rotate-to shape (p! 30 45 60))
    (freeze-transform shape)
    (add-shape *scene* shape)
    (setf (end-frame *scene*) 1000)     ;run for as long as desired
    (let ((anim (make-flex-animator shape)))
      (setf (force-fields anim) (list (make-instance 'constant-force-field
                                                     :force-vector (p! 0 -.05 0))))
      (add-motion *scene* anim))))

(update-scene *scene* 60)               ;do update for batch testing

#|
(Demo 02 flex-animator) pinned vertex ==========================================

Create a FLEX-ANIMATOR with gravity and pin one vertex.
|#
(with-clear-scene
  (let ((shape (make-tetrahedron 2.0)))
    (translate-to shape (p! 0 2 0))
    (rotate-to shape (p! 30 45 60))
    (freeze-transform shape)
    (add-shape *scene* shape)
    (setf (end-frame *scene*) 1000)     ;run for as long as desired
    (let ((anim (make-flex-animator shape)))
      (setf (force-fields anim) (list (make-instance 'constant-force-field
                                                     :force-vector (p! 0 -.05 0))))
      (setf (pinned? (aref (vertices anim) 0)) t) ;pin vertex
      (add-motion *scene* anim))))

(update-scene *scene* 60)               ;do update for batch testing

#|
(Demo 03 flex-animator) gravity and collisions =================================

Create a FLEX-ANIMATOR with sideways force, weak springs, and collision friction.
|#
(with-clear-scene
  (let ((shape (make-cube 2.0)))
    (translate-to shape (p! -2 2 0))
    (rotate-to shape (p! 30 45 60))
    (freeze-transform shape)
    (add-shape *scene* shape)
    (setf (end-frame *scene*) 1000)     ;run for as long as desired
    ;; animator
    (let ((anim (make-flex-animator shape)))
      (setf (force-fields anim) (list (make-instance 'constant-force-field
                                                     :force-vector (p! 0.04 -.05 0))))
      (set-flex-spring-attr anim 'stiffness 0.2)   ;weak springs
      (set-flex-vertex-attr anim 'friction 0.0)    ;sticky friction
      (add-motion *scene* anim))))

(update-scene *scene* 60)               ;do update for batch testing

#|
(Demo 04 flex-animator) poly-strand, square ====================================

Create a web-like POLY-STRAND and animate it using a FLEX-ANIMATOR. The animator
animates the strands shrinking, resulting in a web-looking structure.

The base of the POLY-STRAND shape is a square (CURVE).

Add a CONSTANT-FORCE-FIELD for gravity.
|#
(with-clear-scene
  (let* ((shape (freeze-transform (rotate-to (make-square-curve 4) (p! 90 0 0))))
         (poly (make-poly-strand shape)))
    (insert-strands-by-length poly 100)
    (add-shape *scene* poly)
    ;; animator
    (let ((anim (make-flex-animator poly)))
      ;; turn off collisions
      (set-flex-vertex-attr anim 'do-collisions? nil)
      ;; pin points which are vertices of shape
      (dotimes (i (length (points shape)))
        (setf (pinned? (aref (vertices anim) i)) t))
      ;; shrink springs
      (do-array (i spring (springs anim))
        (setf (rest-length spring) (* 0.5 (rest-length spring))))
      (set-flex-spring-attr anim 'stiffness 0.5)
      (set-flex-vertex-attr anim 'damping 0.5)
      (setf (force-fields anim) (list (make-instance 'constant-force-field
                                                     :force-vector (p! 0 -.01 0))))
      (add-motion *scene* anim))))

(update-scene *scene* 120)               ;do update for batch testing

#|
(Demo 05 flex-animator) poly-strand, cube ======================================

Similar to Demo 05 but using a cube (POLYHEDRON) as the base shape for the
POLY-STRAND.
|#
(with-clear-scene
  (let* ((shape (make-cube 4))
         (poly (make-poly-strand shape)))
    (insert-strands-by-length poly 100)
    (add-shape *scene* poly)
    ;; animator
    (let ((anim (make-flex-animator poly)))
      ;; turn off collisions
      (set-flex-vertex-attr anim 'do-collisions? nil)
      ;; pin points which are vertices of shape
      (dotimes (i (length (points shape)))
        (setf (pinned? (aref (vertices anim) i)) t))
      ;; shrink springs
      (do-array (i spring (springs anim))
        (setf (rest-length spring) (* 0.5 (rest-length spring))))
      (set-flex-spring-attr anim 'stiffness 0.5)
      (set-flex-vertex-attr anim 'damping 0.5)
      ;; (setf (force-fields anim) (list (make-instance 'constant-force-field
      ;;                                                :force-vector (p! 0 -.01 0))))
      (add-motion *scene* anim))))

(update-scene *scene* 120)               ;do update for batch testing

#|
(Demo 06 flex-animator) poly-strand, isosurface ================================

Generate an ISOSURFACE from the POLY-STRAND.
|#
(with-clear-scene
  (let* ((shape (make-cube 4))
         (poly (make-poly-strand shape)))
    (insert-strands-by-length poly 20)
    (add-shape *scene* poly)
    ;; animator
    (let ((anim (make-flex-animator poly)))
      ;; turn off collisions
      (set-flex-vertex-attr anim 'do-collisions? nil)
      ;; pin points which are vertices of shape
      (dotimes (i (length (points shape)))
        (setf (pinned? (aref (vertices anim) i)) t))
      ;; shrink springs
      (do-array (i spring (springs anim))
        (setf (rest-length spring) 0.0))
      (set-flex-spring-attr anim 'stiffness 0.5)
      (set-flex-vertex-attr anim 'damping 0.5)
      ;; (setf (force-fields anim) (list (make-instance 'constant-force-field
      ;;                                                :force-vector (p! 0 -.01 0))))
      (add-motion *scene* anim))
    ;; run simulation
    (update-scene *scene* 60)
    ;; create isosurface
    (let* ((field (apply-field-function (make-scalar-field 40 40 40
                                                           :bounds-lo (p! -2.2 -2.2 -2.2)
                                                           :bounds-hi (p!  2.2  2.2  2.2))
                                        (curve-source-field-fn poly
                                                               :strength 1.0 :falloff 1.2)))
           (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 100.0))))
      (add-shape *scene* iso)
      ;;; optional -- save as OBJ file
;;      (export-obj iso "~/isosurface.obj")
      )))

#|
END ============================================================================
|#
