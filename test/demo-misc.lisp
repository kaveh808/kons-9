(in-package #:kons-9)

;;;; start plugins demos =======================================================

;;; procedural-mixin superquadric ----------------------------------------------

(format t "  superquadrics...~%") (finish-output)

(with-clear-scene
  (let ((mesh (make-superquadric 16 16 2.0 1 0.1 :name 'superquadric)))
    (add-shape *scene* mesh)
    (translate-by mesh (p! 0 1 0))))

;;; modify slots and shape will change due to prcedural-mixin setup
(setf (e1 (find-shape-by-name *scene* 'superquadric)) 0.5)

(setf (u-dim (find-shape-by-name *scene* 'superquadric)) 32)

;;; animated superquadric
(with-clear-scene
  (let ((mesh (make-superquadric 32 32 2.0 1.0 1.0)))
    (add-shape *scene* mesh)
    (translate-by mesh (p! 0 1 0))
    (add-motion *scene*
                  (make-instance 'animator
                                 :setup-fn (lambda ()
                                            (setf (e1 mesh) 1.0)
                                            (setf (e2 mesh) 1.0))
                                 :update-fn (lambda ()
                                              (let ((p (p:normalize
                                                        (noise-gradient
                                                         (p! (+ (current-time *scene*) 0.123)
                                                             (+ (current-time *scene*) 0.347)
                                                             (+ (current-time *scene*) 0.965))))))
                                              (setf (e1 mesh) (* (abs (p:x p)) 2.0))
                                              (setf (e2 mesh) (* (abs (p:y p)) 2.0))))))))

;;; parametric-curve -----------------------------------------------------------

(format t "  parametric-curve...~%") (finish-output)

(with-clear-scene
  (add-shape *scene* (make-bezier-curve (p! -2 0 0) (p! -1 2 0) (p! 1 1 0) (p! 2 0 0))))

(with-clear-scene
  (add-shape *scene* (make-butterfly-curve 1024)))

;;; poly-mesh ------------------------------------------------------------------
(with-clear-scene
  (add-shape *scene* (translate-by (make-cube 2.0 :name 'cube :mesh-type 'poly-mesh) (p! 0 1 0))))
;;; select vertices
(progn
  (select-vertex (find-shape-by-name *scene* 'cube) 7)
  (select-vertex (find-shape-by-name *scene* 'cube) 6))
;;; select edges
(progn
  (select-edge (find-shape-by-name *scene* 'cube) 11)
  (select-edge (find-shape-by-name *scene* 'cube) 10))
;;; select faces
(progn   ;;TODO --  selected faces shouldn't change due to lighting or shading
  (select-face (find-shape-by-name *scene* 'cube) 2)
  (select-face (find-shape-by-name *scene* 'cube) 5))


;;; l-system ------------------------------------------------------------------

(format t "  l-system...~%") (finish-output)

;;; uncomment an l-system to test
(with-clear-scene
  (let ((l-sys
          ;; (make-koch-curve-l-system)
          ;; (make-binary-tree-l-system)
          ;; (make-serpinski-triangle-l-system)
          ;; (make-serpinski-arrowhead-l-system)
          ;; (make-dragon-curve-l-system)
           (make-fractal-plant-l-system)
          ))
    (setf (name l-sys) 'l-system)
    (add-shape *scene* l-sys)
    (add-motion *scene* l-sys)
    (update-scene *scene* 5)
    ;; resize shape to convenient size and center shape at origin
    (scale-to-size (find-shape-by-name *scene* 'l-system) 5.0)
    (center-at-origin (find-shape-by-name *scene* 'l-system))))
;;; WARNING -- press space key in 3D view to generate new l-system levels hangs for some of these
;;; need to investigate

;;; dynamics-animator ----------------------------------------------------------

(format t "  dynamics-animator...~%") (finish-output)

(with-clear-scene
  (let ((shapes '()))
    (dotimes (i 100) (push (make-cube 0.2) shapes))
    (add-shape *scene* (make-shape-group shapes))
    (add-motions *scene*
                   (mapcar (lambda (s)
                               (translate-by s (p! (rand1 2.0) (rand2 2.0 4.0) (rand1 2.0)))
                               (make-instance 'dynamics-animator
                                              :shape s
                                              :velocity (p-rand 0.1)
                                              :do-collisions? t
                                              :collision-padding 0.1
                                              :elasticity 0.5
                                              :force-fields (list (make-instance 'constant-force-field
                                                                                 :force-vector (p! 0 -.02 0)))))
                           shapes))))
;;; hold down space key in 3D view to run animation

;;; obj import -----------------------------------------------------------------

(format t "  obj import...~%") (finish-output)

(defparameter *example-obj-filename* 
  (first (list (asdf:system-relative-pathname "kons-9" "test/data/cow.obj")
               (asdf:system-relative-pathname "kons-9" "test/data/teapot.obj")))
  "An example object filename used in demonstrations for the OBJ-IMPORT facility.

You can find obj files at

  https://people.sc.fsu.edu/~jburkardt/data/obj/obj.html

in this and demos below, update the *EXAMPLE-OBJ-FILENAME* for your setup.")

(with-clear-scene
  (add-shape *scene*
             (import-obj *example-obj-filename*)))

;;; point-instancer-group ------------------------------------------------------------

(format t "  point-instancer-group...~%") (finish-output)

(with-clear-scene
  (let ((shape (make-point-instancer-group (import-obj *example-obj-filename*)
                                     (make-octahedron .2))))
    (setf (name shape) 'point-instancer-group)
    (add-shape *scene* shape)))
;;; change inputs and shape regenerates
(setf (instance-shape (find-shape-by-name *scene* 'point-instancer-group))
      (make-icosahedron .2))

(setf (point-source (find-shape-by-name *scene* 'point-instancer-group))
      (make-sine-curve 360.0 1.0 4.0 4.0 64))

#|
;;; point-instancer-group particle-system --------------------------------------------

(format t "  point-instancer-group particle-system...~%") (finish-output)

(with-clear-scene
  (let* ((p-sys (make-particle-system (make-point-cloud (vector (p! 0 0.1 0)))
                                      (p! 0 .2 0) 10 -1 'particle
                                      :update-angle (range-float (/ pi 8) (/ pi 16))
                                      :life-span (rand1 5 10))))
                                        ;    (setf (draw-live-points-only? p-sys) nil)
    (setf (name p-sys) 'p-system)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation
;;; instance shapes along particle system points
(add-shape *scene* (make-point-instancer-group (find-shape-by-name *scene* 'p-system)
                                               (make-octahedron .2)))
;;; hold down space key in 3D view to run animation with point-instancer-group updating

;;; point-instancer-group particle-system dependency-node-mixin ----------------------

(format t "  point-instancer-group particle-system dependency-node-mixin...~%") (finish-output)

(with-clear-scene
  (let* ((p-sys (make-particle-system (make-point-cloud (vector (p! 0 0.1 0)))
                                      (p! 0 .2 0) 10 -1 'particle
                                      :update-angle (range-float (/ pi 8) (/ pi 16))
                                      :life-span (rand1 5 10)))
         (shape (make-point-instancer-group p-sys
                                            (make-octahedron .2))))
    ;;; uncomment to only instance at live position
;;;    (setf (point-source-use-live-positions-only p-sys) t)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)
    (add-shape *scene* shape)
    ))
;;; hold down space key in 3D view to run animation
|#

;;; transform-instancer-group euler-transform ----------------------------------------

(format t "  transform-instancer-group...~%") (finish-output)

(progn
  (defparameter *instancer*
    (make-transform-instancer-group (make-cube 1.0)
                                    (make-euler-transform (p! 0 7 0) (p! 0 90 0) (p! 1 1 0.2))
                                    8))
  (with-clear-scene
    (add-shape *scene* *instancer*)))

;;; change inputs and shape regenerates
(setf (instance-shape *instancer*) (make-superquadric 16 16 1 .2 .2))

(setf (num-steps *instancer*) 6)

;;; transform-instancer-group angle-axis-transform -----------------------------------
(progn
  (defparameter *instancer*
    (make-transform-instancer-group (make-cube 1.0)
                                    (make-axis-angle-transform (p! 0 7 0) 90 (p! 1 2 3) (p! 1 1 0.2))
                                    8))
  (with-clear-scene
    (add-shape *scene* *instancer*)))

;;; change inputs and shape regenerates
(setf (instance-shape *instancer*) (make-superquadric 16 16 1 .2 .2))

;;; requires call to compute because of limitations of dependency-node-mixin
(progn
  (rotate-to (instance-transform *instancer*) 150)
  (compute-procedural-node *instancer*))

;;; uv-mesh transform-instancer-group 1 ----------------------------------------------

(format t "  uv-mesh transform-instancer-group...~%") (finish-output)

(with-clear-scene
  (let* ((path (make-sine-curve 360 1 4 1 32))
         (prof (make-circle 0.6 4))
         (mesh (first (sweep-extrude prof path :twist (* 2 pi) :taper 0.0)))
         (transform (make-euler-transform (p! 0 0 0) (p! 0 (* 360 7/8) 0) (p! 1 1 1))))
    (add-shape *scene* (make-transform-instancer-group mesh transform 8 :name 'xform-instancer-group))))

;;; change inputs and shape regenerates
(setf (num-steps (find-shape-by-name *scene* 'xform-instancer-group)) 4)

;;; uv-mesh transform-instancer-group 2 ----------------------------------------------
(with-clear-scene 
  (let* ((path (make-sine-curve 360 1 4 1 32))
         (prof (make-circle 0.6 4))
         (mesh (first (sweep-extrude prof path :twist (* 2 pi) :taper 0.0))))
    (set-point-colors-by-uv mesh (lambda (u v)
                                   (declare (ignore u))
                                   (c-rainbow v)))
    (let* ((transform-1 (make-euler-transform (p! 0 0 0) (p! 0 (* 360 7/8) 0) (p! 1 1 1)))
           (group-1 (make-transform-instancer-group mesh transform-1 8))
           (transform-2 (make-euler-transform (p! 0 6 0) (p! 0 45 0) (p! .2 .2 .2)))
           (group-2 (make-transform-instancer-group group-1 transform-2 6)))
      (add-shape *scene* group-2))))


;;; polyhedron curve-generator-mixin -------------------------------------------

(format t "  polyhedron curve-generator-mixin...~%") (finish-output)

(with-clear-scene
  (let ((polyh (make-cut-cube 4.0 :name 'cube)))
    (add-shape *scene* polyh)))
;;; sweep-extrude circle along polyh faces
(add-shape *scene*
           (make-shape-group (sweep-extrude (make-circle 0.5 6)
                                            (find-shape-by-name *scene* 'cube))))

;;;; END ========================================================================
