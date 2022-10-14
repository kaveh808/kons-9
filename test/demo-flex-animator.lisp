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
    (let ((anim (make-flex-animator shape)))
      (setf (force-fields anim) (list (make-instance 'constant-force-field
                                                     :force-vector (p! 0.04 -.05 0))))
      (set-spring-stiffness anim 0.2)   ;weak springs
      (set-vertex-friction anim 0.0)    ;sticky friction
      (add-motion *scene* anim))))

(update-scene *scene* 60)               ;do update for batch testing
