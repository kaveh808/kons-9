(in-package #:kons-9)

;;;; start plugins demos =======================================================


(format t "  heightfields...~%") (finish-output)

;;; heightfield ---------------------------------------------------------------
;;; try using various height functions and color functions
(with-clear-scene
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       (lambda (x z)
                                         (* 4 (noise (p! x 0 z)))))))

(with-clear-scene
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       (lambda (x z)
                                         (* 4 (turbulence (p! x 0 z) 4))))))

(with-clear-scene
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       (lambda (x z)
                                         (let* ((p (p! x 0 z))
                                                (mag (p:length (p:scale p .25))))
                                           (if (= mag 0.0)
                                               10.0
                                               (/ 1.0 mag)))))))

(with-clear-scene
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       (lambda (x z)
                                         (let* ((p (p! x 0 z))
                                                (mag (max 0.001 (p:length (p:scale p 4.0)))))
                                           (* 3 (/ (sin mag) mag)))))))

;;; rainbow color based on height
(let ((mesh (first (shapes *scene*))))
  (set-point-colors-by-xyz mesh (lambda (p) (c-rainbow (clamp (tween (p:y p) -.25 1.0) 0.0 1.0)))))

;;; rainbow color based on XZ distance from origin
(let ((mesh (first (shapes *scene*))))
  (set-point-colors-by-xyz mesh (lambda (p) (c-rainbow (clamp (tween (p:length (p! (p:x p) 0 (p:z p))) 0 8) 0.0 1.0)))))

;;; 3D color noise
(let ((mesh (first (shapes *scene*))))
  (set-point-colors-by-xyz mesh (lambda (p) (color-noise p))))

;;; animated heightfield -------------------------------------------------------
(with-clear-scene
  (let ((mesh (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5) nil)))
    (set-point-colors-by-xyz mesh (lambda (p) (c-rainbow (clamp (tween (p:length (p! (p:x p) 0 (p:z p))) 0 8) 0.0 1.0))))
    (add-shape *scene* mesh)
    (macrolet ((my-height-fn (scale)
                 `(lambda (x z)
                    (let* ((p (p! x 0 z))
                           (mag (max 0.001 (p:length (p:scale p ,scale)))))
                      (* 3 (/ (sin mag) mag))))))
      (add-motion *scene*
                    (make-instance 'animator
                                   :setup-fn (lambda ()
                                              (setf (height-fn mesh) (my-height-fn 1.0))
                                              (update-heightfield mesh))
                                   :update-fn (lambda ()
                                                (setf (height-fn mesh) (my-height-fn (+ 1.0 (current-time *scene*))))
                                                (update-heightfield mesh)))))))

;;; procedural-mixin superquadric ----------------------------------------------

(format t "  superquadrics...~%") (finish-output)

(with-clear-scene
  (let ((mesh (make-superquadric 16 16 2.0 1 0.1)))
    (add-shape *scene* mesh)
    (translate-by mesh (p! 0 1 0))))

;;; modify slots and shape will change due to prcedural-mixin setup
(setf (e1 (first (shapes *scene*))) 0.5)

(setf (u-dim (first (shapes *scene*))) 32)

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
  (add-shape *scene* (translate-by (make-cube 2.0 :mesh-type 'poly-mesh) (p! 0 1 0))))
;;; select vertices
(progn
  (select-vertex (first (shapes *scene*)) 7)
  (select-vertex (first (shapes *scene*)) 6))
;;; select edges
(progn
  (select-edge (first (shapes *scene*)) 11)
  (select-edge (first (shapes *scene*)) 10))
;;; select faces
(progn   ;;Fixme?  selected faces shouldn't change due to lighting or shading
  (select-face (first (shapes *scene*)) 2)
  (select-face (first (shapes *scene*)) 5))


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
    (add-shape *scene* l-sys)
    (add-motion *scene* l-sys)
    (update-scene *scene* 5)
    ;; resize shape to convenient size and center shape at origin
    (scale-to-size (first (shapes *scene*)) 5.0)
    (center-at-origin (first (shapes *scene*)))))
;;; WARNING -- press space key in 3D view to generate new l-system levels hangs for some of these
;;; need to investigate

;;; particle-system ------------------------------------------------------------

(format t "  particle-system...~%") (finish-output)

(with-clear-scene
  (let ((p-sys (make-particle-system (make-point-cloud (vector (p! 0 0 0)))
                                     (p! 0 .2 0) 10 -1 'particle
                                     :update-angle (range-float (/ pi 8) (/ pi 16))
                                     :life-span 10)))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; particle-system force-field collisions
;;; TODO -- num of intial particles not working -- always 1
(with-clear-scene
  (let ((p-sys (make-particle-system (make-point-cloud (vector (p! 0 2 0)))
                                     (p-rand .2) 2 -1 'dynamic-particle
                                     :life-span 20
                                     :do-collisions? t
                                     :force-fields (list (make-instance 'constant-force-field
                                                                        :force-vector (p! 0 -.02 0))))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; NOT TESTED BELOW THIS LINE

;;; sweep-mesh dependency-node-mixin -------------------------------------------

(format t "  sweep-mesh...~%") (finish-output)

(with-clear-scene
  (defparameter *profile* (make-circle 0.8 4))
  (defparameter *path* (make-sine-curve 360 1 4 1 32))
  (defparameter *mesh* (make-sweep-mesh *profile* 0 *path* 0 :twist (* 2 pi) :taper 0.0))
  (add-shape *scene* *mesh*))

;;; modify slots and shape will change
(setf (num-segments *profile*) 6)
(setf (num-segments *path*) 8)
(setf (taper *mesh*) 1.0)

;;; sweep-mesh dependency-node-mixin animator ----------------------------------
(with-clear-scene
  (defparameter *profile* (make-circle 1.2 4))
  (defparameter *path* (make-sine-curve 360 1 4 1 32))
  (defparameter *mesh* (make-sweep-mesh *profile* 0 *path* 0 :twist (* 2 pi) :taper 0.0))
  (let ((anim (make-instance 'animator
                             :setup-fn (lambda () (setf (num-segments *profile*) 4) nil)
                             :update-fn (lambda () (incf (num-segments *profile*))))))
    (add-motion *scene* anim)
    (add-shape *scene* *mesh*)))
;;; hold down space key in 3D view to run animation

;;; dynamics-animator ----------------------------------------------------------

(format t "  dynamics-animator...~%") (finish-output)

(with-clear-scene
  (let ((shapes '()))
    (dotimes (i 100) (push (make-cube 0.2) shapes))
    (add-shape *scene* (make-group shapes))
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

;;; particle system growth along point-cloud -----------------------------------

(format t "  particle-system...~%") (finish-output)

(with-clear-scene
  (let* ((shape (generate-point-cloud (triangulate-polyhedron (import-obj *example-obj-filename*))
                                      100))
         (p-sys (make-particle-system (make-point-cloud (vector (p! 0 0 0)))
                                      (p! .2 .2 .2) 10 -1 'climbing-particle
                                      :support-point-cloud shape
                                      :update-angle (range-float (/ pi 8) (/ pi 16))
                                      :life-span (rand1 5 10))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation -- gets slow, need to profile code & optimize

;;; particle system growth along point-cloud & sweep-extrude -------------------

(format t "  particle-system...~%") (finish-output)

(with-clear-scene
  (let* ((shape
           (import-obj *example-obj-filename*))
         (cloud (generate-point-cloud shape 100))
         (p-sys (make-particle-system (make-point-cloud (vector (p! 0 0 0)))
                                      (p! .2 .2 .2) 10 -1 'climbing-particle
                                      :support-point-cloud cloud
                                      :update-angle (range-float (/ pi 8) (/ pi 16))
                                      :life-span (rand1 5 10))))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation -- gets slow, need to profile code & optimize
;;; sweep-extrude along particle system paths (first shape in scene)
;;; BUG -- the group contains at least one degenerate uv-mesh (16x16) with no points, causing
;;; a crash in set-point-colors-by-uv (added sanity check in that method)
(let ((group (make-group (sweep-extrude (make-circle 0.1 8)
                                        (first (shapes *scene*))
                                        :taper 1.0 :twist 0.0 :from-end? nil))))
  (set-point-colors-by-uv group (lambda (u v)
                                  (declare (ignore u v))
                                  (c! 0.1 0.5 0.1)))
  (add-shape *scene* group))

;;; point-instancer ------------------------------------------------------------

(format t "  point-instancer...~%") (finish-output)

(with-clear-scene
  (let ((shape (make-point-instancer (import-obj *example-obj-filename*)
                                     (make-octahedron .2))))
    (add-shape *scene* shape)))
;;; change inputs and shape regenerates
(setf (instance-shape (first (shapes *scene*))) (make-icosahedron .2))

(setf (point-generator (first (shapes *scene*))) (make-sine-curve 360.0 1.0 4.0 4.0 64))

;;; point-instancer particle-system --------------------------------------------

(format t "  point-instancer particle-system...~%") (finish-output)

(with-clear-scene
  (let* ((p-sys (make-particle-system (make-point-cloud (vector (p! 0 0 0)))
                                      (p! 0 .2 0) 10 -1 'particle
                                      :update-angle (range-float (/ pi 8) (/ pi 16))
                                      :life-span (rand1 5 10))))
;    (setf (draw-live-points-only? p-sys) nil)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation
;;; instance shapes along particle system points
(add-shape *scene* (make-point-instancer (first (shapes *scene*))
                                         (make-octahedron .2)))
;;; hold down space key in 3D view to run animation with point-instancer updating

;;; point-instancer particle-system dependency-node-mixin ----------------------

(format t "  point-instancer particle-system dependency-node-mixin...~%") (finish-output)

(with-clear-scene
  (let* ((p-sys (make-particle-system (make-point-cloud (vector (p! 0 0 0)))
                                      (p! 0 .2 0) 10 -1 'particle
                                      :update-angle (range-float (/ pi 8) (/ pi 16))
                                      :life-span (rand1 5 10)))
         (shape (make-point-instancer p-sys
                                      (make-octahedron .2))))
    ;;; uncomment to only instance at live position
;;;    (setf (point-generator-use-live-positions-only p-sys) t)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)
    (add-shape *scene* shape)
    ))
;;; hold down space key in 3D view to run animation

;;; transform-instancer euler-transform ----------------------------------------

(format t "  transform-instancer...~%") (finish-output)

(progn
  (defparameter *instancer*
    (make-transform-instancer (make-cube 1.0)
                              (make-euler-transform (p! 0 7 0) (p! 0 90 0) (p! 1 1 0.2))
                              8))
  (with-clear-scene
    (add-shape *scene* *instancer*)))

;;; change inputs and shape regenerates
(setf (instance-shape *instancer*) (make-superquadric 16 16 1 .2 .2))

(setf (num-steps *instancer*) 6)

;;; transform-instancer angle-axis-transform -----------------------------------
(progn
  (defparameter *instancer*
    (make-transform-instancer (make-cube 1.0)
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

;;; uv-mesh transform-instancer 1 ----------------------------------------------

(format t "  uv-mesh transform-instancer...~%") (finish-output)

(with-clear-scene
  (let* ((path (make-sine-curve 360 1 4 1 32))
         (prof (make-circle 0.6 4))
         (mesh (first (sweep-extrude prof path :twist (* 2 pi) :taper 0.0)))
         (transform (make-euler-transform (p! 0 0 0) (p! 0 (* 360 7/8) 0) (p! 1 1 1))))
    (add-shape *scene* (make-transform-instancer mesh transform 8))))

;;; change inputs and shape regenerates
(setf (num-steps (first (shapes *scene*))) 4)

;;; uv-mesh transform-instancer 2 ----------------------------------------------
(with-clear-scene 
  (let* ((path (make-sine-curve 360 1 4 1 32))
         (prof (make-circle 0.6 4))
         (mesh (first (sweep-extrude prof path :twist (* 2 pi) :taper 0.0))))
    (set-point-colors-by-uv mesh (lambda (u v)
                                   (declare (ignore u))
                                   (c-rainbow v)))
    (let* ((transform-1 (make-euler-transform (p! 0 0 0) (p! 0 (* 360 7/8) 0) (p! 1 1 1)))
           (group-1 (make-transform-instancer mesh transform-1 8))
           (transform-2 (make-euler-transform (p! 0 6 0) (p! 0 45 0) (p! .2 .2 .2)))
           (group-2 (make-transform-instancer group-1 transform-2 6)))
      (add-shape *scene* group-2))))

;;; particle-system curve-shape force-field ------------------------------------

(format t "  particle-system 2...~%") (finish-output)

(with-clear-scene
  (let* ((curve (make-circle 4.0 16))
         (p-sys (make-particle-system curve (p! .2 .2 .2) 1 4 'dynamic-particle
                                      :force-fields (list (make-instance 'constant-force-field
                                                                         :force-vector (p! 0 -.02 0))))))
    (add-shape *scene* curve)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; particle-system curve-shape sweep-extrude ----------------------------------

(format t "  particle-system 3...~%") (finish-output)

(with-clear-scene
  (let* ((p-gen (make-circle 4.0 16))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 4 4 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32)))))
    (add-shape *scene* p-gen)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation
;;; do sweep along paths
(let ((group (make-group (sweep-extrude (make-circle 0.5 6)
                                        (first (shapes *scene*))
                                        :taper 0.0))))
  (set-point-colors-by-uv group (lambda (u v) (declare (ignore u)) (c-rainbow v)))
  (add-shape *scene* group))

;;; particle-system point-generator-mixin uv-mesh ------------------------------

(format t "  particle-system 4...~%") (finish-output)

(with-clear-scene
  (let* ((p-gen (make-grid-uv-mesh 8 8 24 24))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32)))))
    (add-shape *scene* p-gen)       
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation
;;; do sweep along paths
(let ((group (make-group (sweep-extrude (make-circle 0.2 6)
                                        (first (shapes *scene*))
                                        :taper 0.0))))
  (set-point-colors-by-uv group (lambda (u v) (declare (ignore u)) (c-rainbow v)))
  (add-shape *scene* group))

;;; particle-system point-generator-mixin sweep-mesh-group ---------------------

(format t "  particle-system 5...~%") (finish-output)

(with-clear-scene
  (let* ((p-gen (make-grid-uv-mesh 8 8 24 24))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32))))
         (sweep-mesh-group (make-sweep-mesh-group (make-circle 0.2 6)
                                                  p-sys
                                                  :taper 0.0 :twist 2pi)))
;;    (add-shape *scene* p-gen)
;;    (add-shape *scene* p-sys)
    (add-shape *scene* sweep-mesh-group)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; particle-system point-generator-mixin sweep-mesh-group spawning ------------

(format t "  particle-system 6...~%") (finish-output)

(with-clear-scene
  (let* ((p-gen (make-grid-uv-mesh 4 4 1 1))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 8 'particle
                                      :life-span (round (rand2 5 10))
                                      :update-angle (range-float (/ pi 16) (/ pi 32))))
         (sweep-mesh-group (make-sweep-mesh-group (make-circle 0.2 6) p-sys
                                                  :taper 0.0 :twist 0.0)))
    (add-shape *scene* p-sys)
    (add-shape *scene* sweep-mesh-group)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; particle-system point-generator-mixin use polyh face centers ---------------

(format t "  particle-system 7...~%") (finish-output)

(with-clear-scene
  (let ((p-gen (make-icosahedron 2.0)))
    (setf (point-source-use-face-centers? p-gen) t)
    (let* ((p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'particle
                                        :life-span 10
                                        :update-angle (range-float (/ pi 16) (/ pi 32))
                                        :spawn-angle (range-float (/ pi 8) (/ pi 16))))
           (sweep-mesh-group (make-sweep-mesh-group (make-circle 0.2 6) p-sys
                                                    :taper 0.0 :twist 0.0)))
      (add-shape *scene* p-gen)
      (add-shape *scene* p-sys)
      (add-shape *scene* sweep-mesh-group)
      (add-motion *scene* p-sys))))
;;; hold down space key in 3D view to run animation

;;; particle-system point-generator-mixin polyhedron ---------------------------

(format t "  particle-system 8...~%") (finish-output)

(with-clear-scene
  (let* ((p-gen (import-obj *example-obj-filename*))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'dynamic-particle
                                       :force-fields (list (make-instance 'constant-force-field
                                                                          :force-vector (p! 0 -.05 0))))))
    (add-shape *scene* p-gen)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation -- slow, profile & optimize

;;; particle-system point-generator-mixin particle-system ----------------------

(format t "  particle-system 9...~%") (finish-output)

(with-clear-scene
  (let* ((p-gen (freeze-transform (translate-by (make-superquadric 8 5 2.0 1.0 1.0)
                                                (p! 0 2 0))))
         (p-sys (make-particle-system p-gen (p! .4 .4 .4) 1 1 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32)))))
    (add-shape *scene* p-gen)
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; for automated testing
(update-scene *scene* 10)

;;; make new particle-system generate from paths of existing particle-system
(progn
  (clear-motions *scene*)             ;remove exsting particle animator
  (let* ((p-gen (first (shapes *scene*)))
         (p-sys (make-particle-system p-gen (p! .4 .4 .4) 1 1 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32)))))
    (add-shape *scene* p-sys)
    (add-motion *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; for automated testing
(update-scene *scene* 5)

;;; do sweep-extrude  
(let ((group (make-group (sweep-extrude (make-circle 0.25 4)
                                        (first (shapes *scene*))
                                        :taper 0.0))))
  (set-point-colors-by-uv group (lambda (u v)
                                  (declare (ignore u))
                                  (c-rainbow v)))
    (add-shape *scene* group))

;;; polyhedron curve-generator-mixin -------------------------------------------

(format t "  polyhedron curve-generator-mixin...~%") (finish-output)

(with-clear-scene
  (let ((polyh (make-cut-cube 4.0)))
    (add-shape *scene* polyh)))
;;; sweep-extrude circle along polyh faces
(add-shape *scene*
           (make-group (sweep-extrude (make-circle 0.5 6)
                                      (first (shapes *scene*)))))

;;;; END ========================================================================
