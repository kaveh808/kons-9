(in-package #:kons-9)

#|
Greetings and welcome to kons-9!

These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

The demos below will walk you through some of the features of the software. You
will evaluate the expressions in each demo and the graphics in the window will
update accordingly.

We also assume that you are familiar with the basics of 3D graphics.

As of this writing, kons-9 runs on MacOS (Intel & M1) and Linux.

We hope you find the system enjoyable and useful.
|#

#|
(Demo 01 kernel) 3D navigation and display =====================================

The SCENE class manages shapes and motions for display and animation
respectively. For our tests, the global variable *SCENE* is bound to our scene.
|#
(with-clear-scene
  (add-shape *scene* (make-cut-cube 2.0)))
#|
Click and drag in the window to rotate the camera.

Press OPTION/ALT and track to move the camera sideways and up and down.

Press CONTROL and drag to move the camera in and out. 

Press the 'h' key to print available keyboard commands. Try them out.
|#

#|
(Demo 02 kernel) display customizations ========================================
|#
(with-clear-scene
  (add-shape *scene* (make-cut-cube 2.0)))
#|
The following commands customize some view settings

For a full list of available options see the DRAWING-SETTINGS class in
kons-9/src/graphics/opengl/opengl.lisp
|#

#|
(set-lines-thin)
(set-lines-thick)
(set-theme-bright)
(set-theme-dark)
(set-ground-plane-bright)
(set-ground-plane-dark)
(set-ground-plane-sparse)
(set-ground-plane-dense)
|#

#|
(Demo 03 kernel) point clouds ==================================================

The POINT-CLOUD class displays 3D points. It is the basis for many geometry
classes.
|#
(with-clear-scene
  (add-shape *scene* (make-point-cloud (make-grid-points 10 2 5 (p! -2.0 -0.4 -1.0) (p! 2.0 0.4 1.0)))))

(with-clear-scene
  (add-shape *scene* (make-point-cloud (make-random-points 500 (p! -3 -3 -3) (p! 3 3 3)))))

#|
(Demo 04 kernel) curves ========================================================

The CURVE class represents open or closed paths in 3D space.
|#
(with-clear-scene
  (add-shape *scene* (translate-to (make-line-curve (p! 0 0 0) (p! 2 2 2) 8) (p! 0 0 -6.0)))
  (add-shape *scene* (translate-to (make-rectangle-curve 2 1 4) (p! 0 0 -4.0)))
  (add-shape *scene* (translate-to (make-square-curve 1.5) (p! 0 0 -2.0)))
  (add-shape *scene* (translate-to (make-circle-curve 2.0 16) (p! 0 0 0.0)))
  (add-shape *scene* (translate-to (make-arc-curve 2.0 0 90 16) (p! 0 0 2.0)))
  (add-shape *scene* (translate-to (make-sine-curve-curve 360 1 2 1 16) (p! 0 0 4.0)))
  (add-shape *scene* (translate-to (make-spiral-curve .2 2.0 -1.0 4 64) (p! 0 0 6.0))))

(with-clear-scene
  (add-shape *scene* (translate-to (make-circle-curve 3.0  7) (p! 0 0 -4.0)))
  (add-shape *scene* (translate-to (make-circle-curve 3.0  6) (p! 0 0 -2.0)))
  (add-shape *scene* (translate-to (make-circle-curve 3.0  5) (p! 0 0  0.0)))
  (add-shape *scene* (translate-to (make-circle-curve 3.0  4) (p! 0 0  2.0)))
  (add-shape *scene* (translate-to (make-circle-curve 3.0  3) (p! 0 0  4.0))))

#|
(Demo 05 kernel) polyhedral objects ============================================

The POLYHEDRON class is the basic 3D shape representaion.
|#
(with-clear-scene
  (add-shapes *scene* (list
                       (translate-to (make-tetrahedron  2.0) (p! -5 0 0))
                       (translate-to (make-cube         2.0) (p! -2.5 0 0))
                       (translate-to (make-octahedron   2.0) (p! 0 0  0))
                       (translate-to (make-dodecahedron 2.0) (p!  2.5 0 0))
                       (translate-to (make-icosahedron  2.0) (p! 5 0 0)))))

#|
Refine (subdivide) a polyhedron.
|#
(with-clear-scene
  (add-shape *scene* (refine-mesh (make-cube 2.0) 3)))

#|
A cube turned into a sphere.
|#
(with-clear-scene
  (add-shape *scene* (make-cube-sphere 4.0 3)))

#|
Triangulate a polyhedron.
|#
(with-clear-scene
  (add-shape *scene* (triangulate-polyhedron (make-cube-sphere 4.0 3))))

#|
Generate a point cloud on the surface of a polyhedron.
|#
(with-clear-scene
  (add-shape *scene* (generate-point-cloud (triangulate-polyhedron (make-cube-sphere 4.0 3))
                                           40)))

#|
(Demo 06 kernel) transforms and hierarchies ====================================

Hierarchies are implemented by SHAPE and GROUP classes, both of which have
a TRANSFORM.

Create a hierarchy and turn on shape axis display.
|#
(with-clear-scene
  (defparameter *icosahedron* (make-icosahedron 1.0))
  (defparameter *cube* (make-cube 1.0))
  (defparameter *tetrahedron* (make-tetrahedron 1.0))
  (defparameter *group-1* (make-group (list *cube* *tetrahedron*)))
  (defparameter *group-2* (make-group (list *group-1* *icosahedron*)))
  (add-shape *scene* *group-2*)
  (map-shape-hierarchy *scene* (lambda (s) (setf (show-axis s) 1.0)))
  (translate-by *tetrahedron* (p! 0.0 1.5 0.0))
  (translate-by *cube* (p! 0.0 -1.5 0.0))
  (translate-by *group-1* (p! 1.5 0.0 0.0))
  (translate-by *icosahedron* (p! -2.0 0.0 0.0)))

#|
Print the hierarchy.
|#
(print-hierarchy *group-2*)

#|
Rotate the groups.
|#
(rotate-by *group-1* (p! 0.0 0.0 10.0))

(rotate-by *group-2* (p! 10.0 0.0 0.0))

#|
Add a shape to two different groups. It is now an instance, appearing in two places in the hierarchy.
|#
(progn
  (defparameter *octahedron* (make-octahedron 1.0))
  (setf (show-axis *octahedron*) 1.0)
  (add-child *group-1* *octahedron*)
  (add-child *group-2* *octahedron*))

#|
Moving the octahedron translates both instances.
|#
(translate-by *octahedron* (p! 0.0 0.0 -.5))

#|
(Demo 07 kernel) groups ========================================================

Create a group by creating shapes at specified points.
|#
(with-clear-scene
  (add-shape *scene* (scatter-shapes-in-group (lambda () (make-cube 0.5))
                                              (make-grid-points 3 3 3 (p! -2 -2 -2) (p! 2 2 2)))))

(with-clear-scene
  (add-shape *scene* (scatter-shapes-in-group (lambda () (make-octahedron 0.5))
                                              (make-circle-points 4.0 32))))

#|
Randomly scale leaf nodes of the hierarchy.
|#
(map-shape-hierarchy *scene*
  (lambda (shape) (scale-to shape (rand2 0.2 2.0)))
  :test #'is-leaf?)

#|
(Demo 08 kernel) animation =====================================================

The simplest way of doing animation is to add an ANIMATOR to the motions of a 
scene. The animator will call its setup function (if it has one) when the scene
is initialized, and will call its update function every frame.

To play animation, hold down space key. Notice the frame counter updating in the
window title bar. By default the scene starts at frame 0 and ends at frame 240.

Press 'a' key to reset the scene back to frame 0.

This animation translates the shape in the X axis direction.
|#
(with-clear-scene
  (let ((shape (add-shape *scene* (make-cut-cube 2.0))))
    (add-motion *scene*
                (make-instance 'animator
                               :setup-fn (lambda () (translate-to shape (p! 0.0 0 0)))
                               :update-fn (lambda () (translate-by shape (p! 0.1 0 0)))))))

#|
(Demo 09 kernel) multiple animators ============================================

Animate all the leaf shapes in a group by the same amount.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(with-clear-scene
  (let ((group (add-shape *scene* (scatter-shapes-in-group
                                   (lambda () (make-cube 0.5))
                                   (make-grid-points 3 3 3 (p! -2 -2 -2) (p! 2 2 2))))))
    (add-motion *scene*
                (make-instance 'animator
                               :setup-fn (lambda ()
                                           (map-hierarchy group
                                             (lambda (shape) (rotate-to shape (p! 0 0 0)))
                                             :test #'is-leaf?))
                               :update-fn (lambda ()
                                            (map-hierarchy group
                                              (lambda (shape) (rotate-by shape (p! 0 5 0)))
                                              :test #'is-leaf?))))))

#|
(Demo 10 kernel) shape-animator ===============================================

The previous example animated all the shapes by the same amount.
A SHAPE-ANIMATOR allows us to store and use per-animator data in an association
list. It also takes a SHAPE to animate.

Here each shape is rotated by a different random amount around the Y axis.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(with-clear-scene
  (let ((group (add-shape *scene* (scatter-shapes-in-group
                                   (lambda () (make-cube 0.5))
                                   (make-grid-points 3 3 3 (p! -2 -2 -2) (p! 2 2 2))))))
    (map-hierarchy group
      (lambda (shape)
        (add-motion *scene*
                      (make-instance 'shape-animator
                                     :shape shape
                                     :setup-fn (lambda (anim)
                                                (rotate-to (shape anim) (p! 0 0 0)))
                                     :update-fn (lambda (anim)
                                                  (rotate-by (shape anim) (anim-data anim :rotate)))
                                     :data `((:rotate . ,(p! 0 (rand1 10) 0))))))
      :test #'is-leaf?)))

#|
(Demo 11 kernel) animating hierarchical scene geometry ========================

Create a robot arm as a hierarchical structure.
|#
(with-clear-scene
  (defparameter *waist-shape* (make-cube-sphere 0.5 2))
  (defparameter *torso-shape* (make-box 0.6 0.8 0.2))
  (defparameter *shoulder-shape* (make-cube-sphere 1.0 2))
  (defparameter *upper-arm-shape* (make-box 1.05 0.5 0.2))
  (defparameter *elbow-shape* (make-cube-sphere 1.0 2))
  (defparameter *lower-arm-shape* (make-box 2.1 0.5 0.2))
  (defparameter *wrist-shape* (make-cube-sphere 1.0 2))
  (defparameter *hand-shape* (make-box 1.5 1.2 0.4))

  (defparameter *wrist* (make-group (list *wrist-shape* *hand-shape*)))
  (defparameter *elbow* (make-group (list *elbow-shape* *lower-arm-shape* *wrist*)))
  (defparameter *shoulder* (make-group (list *shoulder-shape* *upper-arm-shape* *elbow*)))
  (defparameter *torso* (make-group (list *shoulder-shape* *upper-arm-shape* *elbow*)))
  (defparameter *waist* (make-group (list *waist-shape* *torso-shape* *shoulder*)))

  (setf (name *waist-shape*) 'waist-shape)
  (setf (name *torso-shape*) 'torso-shape)
  (setf (name *shoulder-shape*) 'shoulder-shape)
  (setf (name *upper-arm-shape*) 'upper-arm-shape)
  (setf (name *elbow-shape*) 'elbow-shape)
  (setf (name *lower-arm-shape*) 'lower-arm-shape)
  (setf (name *wrist-shape*) 'wrist-shape)
  (setf (name *hand-shape*) 'hand-shape)

  (setf (name *wrist*) 'wrist-group)
  (setf (name *elbow*) 'elbow-group)
  (setf (name *shoulder*) 'shoulder-group)
  (setf (name *torso*) 'torso-group)
  (setf (name *waist*) 'waist-group)

  (translate-to *waist* (p! -0.6 -0.4 0.0))
  (scale-to *waist* 2.0)
  (translate-to *shoulder* (p! 0.4 0.8 0.0))
  (scale-to *shoulder* 0.3)
  (translate-to *elbow* (p! 1.8 0.0 0.0))
  (scale-to *elbow* 0.5)
  (translate-to *wrist* (p! 3.0 0.0 0.0))
  (scale-to *wrist* 0.6)

  (translate-to *torso-shape* (p! 0.0 0.4 0.0))
  (translate-to *upper-arm-shape* (p! 1.0 0.0 0.0))
  (translate-to *lower-arm-shape* (p! 1.6 0.0 0.0))
  (translate-to *hand-shape* (p! 1.2 0.0 0.0))

  (add-shape *scene* *waist*)
)

#|
Turn on shape axis display.

You can turn off shading to see axes better (press 1 key).
|#
(map-shape-hierarchy *scene* (lambda (s) (setf (show-axis s) 1.0)))

#|
Animate each joined by a specified number of degrees per frame.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(progn
  (add-motion *scene*
              (make-instance 'shape-animator
                             :name 'waist-animator
                             :shape *waist*
                             :setup-fn (lambda (anim) (rotate-to (shape anim) (p! 0 0 0)))
                             :update-fn (lambda (anim) (rotate-by (shape anim) (anim-data anim :rotate)))
                             :data `((:rotate . ,(p! 0 0 -2)))))
  (add-motion *scene*
              (make-instance 'shape-animator
                             :name 'shoulder-animator
                             :shape *shoulder*
                             :setup-fn (lambda (anim) (rotate-to (shape anim) (p! 0 0 0)))
                             :update-fn (lambda (anim) (rotate-by (shape anim) (anim-data anim :rotate)))
                             :data `((:rotate . ,(p! 0 0 -6)))))
  (add-motion *scene*
              (make-instance 'shape-animator
                             :name 'elbow-animator
                             :shape *elbow*
                             :setup-fn (lambda (anim) (rotate-to (shape anim) (p! 0 0 0)))
                             :update-fn (lambda (anim) (rotate-by (shape anim) (anim-data anim :rotate)))
                             :data `((:rotate . ,(p! 0 0 12)))))
  (add-motion *scene*
              (make-instance 'shape-animator
                             :name 'wrist-animator
                             :shape *wrist*
                             :setup-fn (lambda (anim) (rotate-to (shape anim) (p! 0 0 0)))
                             :update-fn (lambda (anim) (rotate-by (shape anim) (anim-data anim :rotate)))
                             :data `((:rotate . ,(p! 0 0 9)))))
  )

#|
(Demo 12 kernel) scene shape management =======================================

Continue from (Demo 11 kernel).

Print scene shape hierarchy.
|#
(print-shape-hierarchy *scene* :names-only t)
  
#|
Add a shape in two places in the hierarchy.
|#
(let ((tetra (translate-to (make-tetrahedron 1.0) (p! 0 1 0))))
  (setf (name tetra) 'tetra)
  (add-child *wrist* tetra)
  (add-child *shoulder* tetra))

#|
Find shapes by name.
|#
(pprint (find-shape-by-name *scene* 'shoulder-shape))

(pprint (find-shape-by-name *scene* 'tetra))

(pprint (find-shape-by-name *scene* 'foobar)) ;no such name -- return nil

#|
Find shapes using predicate.
|#
(pprint (find-shapes *scene* (lambda (s) (= 4 (length (points s)))) :groups nil))

(pprint (find-shapes *scene* (lambda (s) (= 8 (length (points s)))) :groups nil))

(pprint (find-shapes *scene* (lambda (s) (search "ELBOW" (string (name s)))) :groups t))

#|
Get shape scene paths.
|#
(pprint (get-scene-paths *scene* (find-shape-by-name *scene* 'wrist-group)))

(pprint (get-scene-paths *scene* (find-shape-by-name *scene* 'tetra))) ;two paths

#|
Compute shape global matrices of shape scene paths.

Animate the scene and evaluate the expression below again to see updated
matrices.
|#
(let ((paths (get-scene-paths *scene* (find-shape-by-name *scene* 'tetra))))
  (pprint (shape-global-matrix *scene* (first paths)))
  (pprint (shape-global-matrix *scene* (second paths))))

#|
Find motions by name.
|#
(pprint (find-motion-by-name *scene* 'elbow-animator))

#|
Find motions using predicate.
|#
(pprint (find-motions *scene* (lambda (m) (eq *wrist* (shape m))) :groups nil))

(pprint (find-motions *scene* (lambda (m) (search "ELBOW" (string (name m)))) :groups t))

#|
(Demo 13 kernel) hierarchical animation timing using motions ==================

We put our animators inside a MOTION-GROUP. The animators reference a local time
within their timing range between zero and one.

The shapes rotate 90 degrees over duration of scene timeline (240 frames).

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#

(with-clear-scene
  (let ((group (add-shape *scene* (scatter-shapes-in-group
                                   (lambda () (scale-to (make-cube 0.5) (p! 2 1 .2)))
                                   (make-grid-points 3 1 1 (p! -2 0 0) (p! 2 0 0)))))
        (counter -1))
    (add-motion
     *scene*
     (make-instance 'motion-group
                    :name 'top-motion-group
                    :scene *scene*
                    :children (mapcar (lambda (shape)
                                        (make-instance
                                         'shape-animator
                                         :name (concat-syms 'animator- (incf counter))
                                         :scene *scene*
                                         :shape shape
                                         :setup-fn (lambda (anim)
                                                     (rotate-to (shape anim) (p! 0 0 0)))
                                         :update-fn (lambda (anim)
                                                      (rotate-to (shape anim)
                                                                 (p! 0 (* 90 (local-time anim)) 0)))))
                                      (children group))))))

#|
Set the scene end frame. The shapes now rotate 90 degrees over 42 frames.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(setf (end-frame *scene*) 42)

#|
Set the motion group's duration to 0.5. The shapes now rotate 90 degrees over
half of scene duration (21 frames).

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(setf (duration (first (motions *scene*))) 0.5)

#|
Set the indidual animators' timings so they run sequentially.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(let* ((anims (children (first (motions *scene*))))
       (anim-0 (nth 0 anims))
       (anim-1 (nth 1 anims))
       (anim-2 (nth 2 anims)))
  (set-timing anim-0 0 1/3)
  (set-timing anim-1 1/3 1/3)
  (set-timing anim-2 2/3 1/3))

#|
Modify the animators' timings.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(let* ((anims (children (first (motions *scene*))))
       (anim-0 (nth 0 anims))
       (anim-1 (nth 1 anims))
       (anim-2 (nth 2 anims)))
  (scale-duration anim-0 3.0)           ;duration = 1
  (offset-start-time anim-1 -1/3)       ;start-time = 0
  (set-timing anim-2 0.5 0.5))

#|
Set the motion group's duration to be full scene duration.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(setf (duration (first (motions *scene*))) 1.0)

#|
Modify the animators' timings using the parent motion-group's ordering methods.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(let ((group (first (motions *scene*))))
  (sequential-order group))

(let ((group (first (motions *scene*))))
  (parallel-order group))

(let ((group (first (motions *scene*))))
  (random-order group 0.25 0.5))

(let* ((anims (children (first (motions *scene*))))
       (anim-0 (nth 0 anims))
       (anim-1 (nth 1 anims))
       (anim-2 (nth 2 anims)))
  (print anim-0)
  (print anim-1)
  (print anim-2))

#|
(Demo 14 kernel) scene motion management ======================================

Continue from (Demo 13 kernel).

Print scene motion hierarchy.
|#
(print-motion-hierarchy *scene* :names-only t)

#|
Find shapes by name.
|#
(pprint (find-motion-by-name *scene* 'animator-2))

(pprint (find-motion-by-name *scene* 'foobar)) ;no such name -- return nil

#|
Get animator motion paths.
|#
(pprint (get-motion-paths *scene* (find-motion-by-name *scene* 'animator-2)))

#|
(Demo 15 kernel) creating constraints with animators ==========================

The dodecahedron moves in Y as the negative of the Y the tetrahedron.
We use ADD-MOTION-AT-END so the second animator runs after the first one.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(with-clear-scene
  (let ((tetrahedron (translate-to (make-tetrahedron 2.0) (p! -1.5 0 0)))
        (dodecahedron (translate-to (make-dodecahedron 2.0) (p! 1.5 0 0))))
    (add-shapes *scene* (list tetrahedron dodecahedron))
    (add-motion *scene*
                (make-instance 'shape-animator
                               :shape tetrahedron
                               :setup-fn (lambda (anim) (translate-to (shape anim) (p! -1.5 0 0)))
                               :update-fn (lambda (anim)
                                            (translate-to (shape anim)
                                                          (p! -1.5 (sin (current-time (scene anim))) 0)))))
    (add-motion-at-end *scene*
                       (make-instance 'shape-animator
                                      :shape dodecahedron
                                      :setup-fn (lambda (anim) (translate-to (shape anim) (p! 1.5 0 0)))
                                      :update-fn (lambda (anim)
                                                   (let ((target-y (p:y (offset (translate (transform (anim-data anim :target)))))))
                                                     (translate-to (shape anim) (p! 1.5 (- target-y) 0))))
                                      :data `((:target . ,tetrahedron))))))

#|
(Demo 16 kernel) object duplication ===========================================
|#
(with-clear-scene
  (let* ((icosahedron (make-icosahedron 1.0 :name 'icosahedron))
         (cube (make-cube 1.0 :name 'cube))
         (tetrahedron (make-tetrahedron 1.0 :name 'tetrahedron))
         (group-1 (make-group (list cube tetrahedron) :name 'group-1))
         (group-2 (make-group (list group-1 icosahedron) :name 'group-2)))
    (translate-by tetrahedron (p! 0.0 1.5 0.0))
    (translate-by cube (p! 0.0 -1.5 0.0))
    (translate-by group-1 (p! 1.5 0.0 0.0))
    (translate-by icosahedron (p! -2.0 0.0 0.0))
    (rotate-by group-1 (p! 0.0 0.0 10.0))

    (add-shape *scene* group-2)
    (map-shape-hierarchy *scene* (lambda (s) (setf (show-axis s) 1.0)))

    (format t "~%~%Original scene hierarchy:~%")
    (print-shape-hierarchy *scene*)))

#|
Duplicate cube (and add to parent of original)
|#
(let ((dup (duplicate-shape *scene* '(group-2 group-1 cube))))
  (scale-to dup (p! 0.5 0.5 0.5))
  (translate-by dup (p! 2.0 0.0 0.0))
  (format t "~%~%After cube duplication:~%")
  (print-shape-hierarchy *scene*))

#|
Duplicate group-1 (and add to parent of original)
|#
(let ((dup (duplicate-shape *scene* '(group-2 group-1))))
  (rotate-by dup (p! 0.0 0.0 -30.0))
  (translate-by dup (p! 0.0 0.0 4.0))
  (format t "~%~%After cube duplication:~%")
  (print-shape-hierarchy *scene*))

#|
(Demo 17 kernel) object visibility ============================================
|#
(with-clear-scene
  (let* ((icosahedron (make-icosahedron 1.0 :name 'icosahedron))
         (cube (make-cube 1.0 :name 'cube))
         (tetrahedron (make-tetrahedron 1.0 :name 'tetrahedron))
         (group-1 (make-group (list cube tetrahedron) :name 'group-1))
         (group-2 (make-group (list group-1 icosahedron) :name 'group-2)))
    (translate-by tetrahedron (p! 0.0 1.5 0.0))
    (translate-by cube (p! 0.0 -1.5 0.0))
    (translate-by group-1 (p! 1.5 0.0 0.0))
    (translate-by icosahedron (p! -2.0 0.0 0.0))
    (rotate-by group-1 (p! 0.0 0.0 10.0))

    (add-shape *scene* group-2)
    (map-shape-hierarchy *scene* (lambda (s) (setf (show-axis s) 1.0)))))

#|
Toggle cube visibility.
|#
(setf (is-visible? (find-shape-by-name *scene* 'cube)) nil)

(setf (is-visible? (find-shape-by-name *scene* 'cube)) t)

#|
Toggle group-1 visibility.
|#
(setf (is-visible? (find-shape-by-name *scene* 'group-1)) nil)

(setf (is-visible? (find-shape-by-name *scene* 'group-1)) t)

#|
(Demo 18 kernel) motion is-active? flag =======================================
|#

(with-clear-scene
  (let ((group (add-shape *scene* (scatter-shapes-in-group
                                   (lambda () (scale-to (make-cube 0.5) (p! 2 1 .2)))
                                   (make-grid-points 3 1 1 (p! -2 0 0) (p! 2 0 0)))))
        (counter -1))
    (add-motion
     *scene*
     (make-instance 'motion-group
                    :name 'top-motion-group
                    :scene *scene*
                    :children (mapcar (lambda (shape)
                                        (make-instance
                                         'shape-animator
                                         :name (concat-syms 'animator- (incf counter))
                                         :scene *scene*
                                         :shape shape
                                         :setup-fn (lambda (anim)
                                                     (rotate-to (shape anim) (p! 0 0 0)))
                                         :update-fn (lambda (anim)
                                                      (rotate-to (shape anim)
                                                                 (p! 0 (* 90 (local-time anim)) 0)))))
                                      (children group)))))
  (setf (end-frame *scene*) 42))

#|
Toggle second animator.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(setf (is-active? (find-motion-by-name *scene* 'animator-1)) nil)

(setf (is-active? (find-motion-by-name *scene* 'animator-1)) t)

#|
Toggle parent motion.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(setf (is-active? (find-motion-by-name *scene* 'top-motion-group)) nil)

(setf (is-active? (find-motion-by-name *scene* 'top-motion-group)) t)

#|
(Demo 19 kernel) events using animators =======================================

Use an animator to stop rotations and scale shapes past a certain angle.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#
(with-clear-scene
  (let ((group (add-shape *scene* (scatter-shapes-in-group
                                   (lambda () (make-cube 0.5))
                                   (make-grid-points 3 3 3 (p! -2 -2 -2) (p! 2 2 2)))))
        (motion-group (add-motion *scene* (make-instance 'motion-group
                                                         :name 'top-motion-group
                                                         :scene *scene*))))
    (map-hierarchy group
                   (lambda (shape)
                     (add-child motion-group
                                (make-instance 'shape-animator
                                               :shape shape
                                               :scene *scene*
                                               :setup-fn (lambda (anim)
                                                           (rotate-to (shape anim) (p! 0 0 0)))
                                               :update-fn (lambda (anim)
                                                            (rotate-by (shape anim) (anim-data anim :rotate)))
                                               :data `((:rotate . ,(p! 0 (rand1 10) 0))))))
                   :test #'is-leaf?)
    ;; add event animator
    (add-motion *scene*
                (make-instance 'animator
                               :scene *scene*
                               :update-fn (lambda ()
                                            (dolist (anim (children motion-group))
                                              (let ((angle (p:y (angles (rotate (transform (shape anim)))))))
                                                (when (> (abs angle) 45)
                                                  (scale-to (shape anim) 0.5)
                                                  (setf (is-active? anim) nil)))))))))

#|
(Demo 20 kernel) shape display options =========================================

Display shape bounds, face-normals, and axes.
|#
(with-clear-scene
    (let ((circle (translate-to (make-circle-curve 3.0  7) (p! 0 0 -4.0)))
          (sphere (translate-by (make-cube-sphere 2.0 3) (p! 0 0 4.0)))
          (icos (make-icosahedron 2.0)))
      (setf (show-axis circle) 1.0)
      (setf (show-normals icos) 1.0)
      (setf (show-bounds? sphere) t)
      (add-shapes *scene* (list circle sphere icos))))

#|
(Demo 21 kernel) freezing shapes ===============================================

Freeze a shape by transforming its points.
|#
(progn
  (defparameter *cube* (make-cube 2.0))
  (with-clear-scene
    (add-shapes *scene* (list
                         (translate-by
                          (make-group
                           (list (rotate-by
                                  (make-group
                                   (list
                                    (translate-by
                                     *cube*
                                     (p! 2.5 0 0))))
                                  (p! 0 45 0))))
                          (p! 0 2 0))))
    (setf (show-axis *cube*) 3.0)))

#|
Hints:
- Press 1 to turn off shaded view so you can see cube axis better.
- Press 7 to turn off global axes display.

Local freezing: transforms a point-based shape's points by its transform matrix
and resets the transform.
|#
(progn
  (format t "~%~%Before freeze: ~a~%" (points *cube*))
  (freeze-transform *cube*)
  (format t "After freeze: ~a~%" (points *cube*)))

#|
(Demo 22 kernel) using animation class to instance motions =====================

[TODO -- work in progress.]

We package an animation in an ANIMATION class. This animation can then be
instanced in a scene by automatically creating new GROUPs and MOTION-GROUPs for
the shapes and animators.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#

(with-clear-scene
  (let* ((shape (scale-to (make-cube 0.5) (p! 2 1 .2)))
         (animator (make-instance
                    'shape-animator
                    :scene *scene*
                    :shape shape
                    :setup-fn (lambda (anim)
                                (rotate-to (shape anim) (p! 0 0 0)))
                    :update-fn (lambda (anim)
                                 (rotate-to (shape anim)
                                            (p! 0 (* 90 (local-time anim)) 0)))))
         (animation (make-instance 'animation :shape shape :shape-animator animator))
         (top-shape-group (make-instance 'group))
         (top-motion-group (make-instance 'motion-group))
         (points (make-grid-points 3 3 3 (p! -2 -2 -2) (p! 2 2 2))))
    (dotimes (i (length points))
      (add-animation-to-scene animation top-shape-group top-motion-group :mode :add-as-instance))
    (scatter-shapes (children top-shape-group) points)
    (add-shape *scene* top-shape-group)
    (add-motion *scene* top-motion-group)
    (setf (end-frame *scene*) 120)))

#|
Set the timings of the animation instances.

Hold down space key to play animation. Press 'a' key to go back to frame 0.
|#

(let ((group (first (motions *scene*))))
  (sequential-order group))

(let ((group (first (motions *scene*))))
  (parallel-order group))

(let ((group (first (motions *scene*))))
  (random-order group 0.25 0.5))

#|
(let ((motion-group-instances (children (first (motions *scene*)))))
  (dolist (motion motion-group-instances)
    (print motion)))
|#

#|
END ============================================================================
|#

