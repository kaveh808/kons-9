(in-package #:kons-9)

;;; class hierarchy
#|
(print-class-hierarchy (class-of (make-instance 'shape)))
(print-class-hierarchy (class-of (make-instance 'point-generator-mixin)))
(print-class-hierarchy (class-of (make-instance 'standard-object)))

(load "~/Development/sample-profiler-master/profile.lisp")
(sample-profiler:start-profiling)
(sample-profiler:end-profiling)

|#

;;; shapes and transforms ------------------------------------------------------
;;; press 'h' in 3d view to see key bindings and navigation
(with-clear-and-redraw
  (add-shape *scene* (translate-to (make-circle-shape 3.0  7) (p! 0 0 -4.0)))
  (add-shape *scene* (translate-to (make-circle-shape 3.0  6) (p! 0 0 -2.0)))
  (add-shape *scene* (translate-to (make-circle-shape 3.0  5) (p! 0 0  0.0)))
  (add-shape *scene* (translate-to (make-circle-shape 3.0  4) (p! 0 0  2.0)))
  (add-shape *scene* (translate-to (make-circle-shape 3.0  3) (p! 0 0  4.0))))

;; polyhedrons -----------------------------------------------------------------
;;; press 'h' in 3d view to see key bindings and navigation
(with-clear-and-redraw
    (let ((circle (translate-to (make-circle-shape 3.0  7) (p! 0 0 -4.0)))
          (superq (translate-by (make-superquadric 32 16 1.0 0.2 0.5) (p! 0 0 4.0)))
          (icos (make-icosahedron 2.0)))
      (setf (show-axis circle) 1.0)
      (setf (show-normals icos) 1.0)
      (setf (show-bounds? superq) t)
      (add-shapes *scene* (list circle superq icos))))

;;; uv-mesh --------------------------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (make-grid-uv-mesh 5 5 (p! -2 0 -2) (p! 2 0 2) 0.0)))

;;; uv-mesh transform-extrude --------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (transform-extrude (make-circle-shape 1.0 3)
					(make-transform (p! 0 0 4) (p! 0 0 0) (p! 1 1 1))
					4)))

;;; uv-mesh transform-extrude --------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (transform-extrude (make-circle-shape 1.0 3)
					(make-transform (p! 0 0 4) (p! 0 0 (* 2 pi)) (p! 0 0 1))
					40)))

;;; curve-shape sine curve -----------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (make-sine-curve-shape 4 1 16)))

;;; uv-mesh sweep-extrude ------------------------------------------------------
(with-clear-and-redraw
  (let* ((path (make-sine-curve-shape 4 1 64))
         (prof (make-circle-shape 0.8 4))
         (meshes (sweep-extrude prof path :twist (* 2 pi) :taper 0.0)))
    (add-shapes *scene* meshes)))
;;; assign point colors
(with-redraw
  (set-point-colors-by-uv (first (shapes *scene*)) #'(lambda (u v) (c-rainbow v))))

;;; uv-mesh sweep-extrude u-wrap v-wrap ----------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (make-torus 1.5 8 4.0 16)))
;;; assign point colors
(with-clear-and-redraw
  (let ((mesh (make-torus 1.5 32 4.0 64)))
    (set-point-colors-by-xyz mesh #'(lambda (p) (c-rainbow (clamp (tween (y p) -3 3) 0.0 1.0))))
;    (set-point-colors-by-uv mesh #'(lambda (u v) (c-rainbow v)))
    (add-shape *scene* mesh)))

;;; procedural-mixin circle ----------------------------------------------------
(with-clear-and-redraw
  (let ((shape (make-instance 'circle-shape :diameter 4.0 :num-points 8)))
    (add-shape *scene* shape)))
;;; modify slots and shape will change
(with-redraw
  (setf (num-points (first (shapes *scene*))) 32))
(with-redraw
  (setf (diameter (first (shapes *scene*))) 1.0))

;;; procedural-mixin sine curve ------------------------------------------------
(with-clear-and-redraw
  (let ((shape (make-instance 'sine-curve-shape :num-points 64 :frequency 2 :period (* 4 pi)
                                                :x-scale 4 :y-scale 2)))
    (add-shape *scene* shape)))
;;; modify slots and shape will change
(with-redraw
  (setf (frequency (first (shapes *scene*))) 1.0))
(with-redraw
  (setf (num-points (first (shapes *scene*))) 16))

;;; procedural-mixin superquadric ----------------------------------------------
(with-clear-and-redraw
  (let ((mesh (make-superquadric 16 16 1.0 1 0.1))) ; 0.2 0.2)))
    (add-shape *scene* mesh)
    (translate-by mesh (p! 0 1 0))))
;;; modify slots and shape will change
(with-redraw
  (setf (e1 (first (shapes *scene*))) 0.5))
(with-redraw
  (setf (u-dim (first (shapes *scene*))) 32))

;;; sweep-mesh dependency-node-mixin -------------------------------------------
(progn
  (defparameter *profile* (make-circle-shape 0.8 4))
  (defparameter *path* (make-sine-curve-shape 4 1 32))
  (defparameter *mesh* (make-sweep-mesh *profile* 0 *path* 0 :twist (* 2 pi) :taper 0.0))
  (with-clear-and-redraw
    (add-shape *scene* *mesh*)))
;;; modify slots and shape will change
(with-redraw
  (setf (num-points *profile*) 6))
(with-redraw
  (setf (num-points *path*) 8))
(with-redraw
  (setf (taper *mesh*) 1.0))

;;; sweep-mesh dependency-node-mixin animator ----------------------------------
(progn
  (defparameter *profile* (make-circle-shape 1.2 4))
  (defparameter *path* (make-sine-curve-shape 4 1 32))
  (defparameter *mesh* (make-sweep-mesh *profile* 0 *path* 0 :twist (* 2 pi) :taper 0.0))
  (with-clear-and-redraw
    (let ((anim (make-instance 'animator :init-fn #'(lambda (anim) (setf (num-points *profile*) 4) nil)
                                         :update-fn #'(lambda (anim) (incf (num-points *profile*))))))
      (add-animator *scene* anim)
      (add-shape *scene* *mesh*))))
;;; hold down space key in 3D view to run animation

;;; dynamics-animator ----------------------------------------------------------
(with-clear-and-redraw
  (let ((shapes '()))
    (dotimes (i 100) (push (make-cube 0.2) shapes))
    (add-shape *scene* (apply #'make-group shapes))
    (add-animators *scene*
                   (mapcar #'(lambda (s)
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
;;; you can find obj files at https://people.sc.fsu.edu/~jburkardt/data/obj/obj.html
;;; in this and demos below, update the obj file pathname for your setup
(with-clear-and-redraw
  (add-shape *scene*
             (import-obj "~/Development/3D DCC Project/data/cow.obj")
;  (import-obj "~/Development/3D DCC Project/data/teapot.obj")
  ))

;;; polyhedron triangulation ---------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (triangulate-polyhedron (make-cut-cube-polyhedron 2.0))))

;;; generate-point-cloud -------------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (generate-point-cloud (triangulate-polyhedron (make-cut-cube-polyhedron 2.0))
                                           40)))

;;; particle system growth along point-cloud -----------------------------------
(with-clear-and-redraw
  (let* ((shape (generate-point-cloud (triangulate-polyhedron
;;;                                       (make-cut-cube-polyhedron 2.0) ; can use instead of obj file
                                       (import-obj "~/Development/3D DCC Project/data/cow.obj")
                                       )
                                      100))
         (p-sys (make-particle-system (make-point-cloud (p! 0 0 0)) (p! .2 .2 .2) 10 -1 'climbing-particle
                                      :support-point-cloud shape
                                      :update-angle (range-float (/ pi 8) (/ pi 16))
                                      :life-span (rand1 5 10))))
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation -- gets slow, need to profile code & optimize

;;; particle system growth along point-cloud & sweep-extrude -------------------
(with-clear-and-redraw
  (let* ((shape
;;;        (triangulate-polyhedron (make-cut-cube-polyhedron 2.0)) ; can use instead of obj file
           (import-obj "~/Development/3D DCC Project/data/teapot.obj"))
         (cloud (generate-point-cloud shape 100))
         (p-sys (make-particle-system (make-point-cloud (p! 0 0 0)) (p! .2 .2 .2) 10 -1 'climbing-particle
                                      :support-point-cloud cloud
                                      :update-angle (range-float (/ pi 8) (/ pi 16))
                                      :life-span (rand1 5 10))))
    (add-shape *scene* shape)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation -- gets slow, need to profile code & optimize
;;; sweep-extrude along particle system paths (first shape in scene)
(with-redraw
  (let ((group (apply #'make-group (sweep-extrude (make-circle-shape 0.1 8) (first (shapes *scene*))
                                                  :taper 1.0 :twist 0.0 :from-end? nil))))
    (set-point-colors-by-uv group #'(lambda (u v) (c! 0.1 0.5 0.1)))
    (add-shape *scene* group)))

;;; point-instancer ------------------------------------------------------------
(with-clear-and-redraw
  (let ((shape (make-point-instancer (import-obj "~/Development/3D DCC Project/data/teapot.obj")
                                     (make-octahedron .1))))
    (add-shape *scene* shape)))
;;; change inputs and shape regenerates
(with-redraw
  (setf (instance-shape (first (shapes *scene*))) (make-icosahedron .1)))
(with-redraw
  (setf (point-generator (first (shapes *scene*))) (import-obj "~/Development/3D DCC Project/data/cow.obj")))
(with-redraw
  (setf (point-generator (first (shapes *scene*))) (make-sine-curve-shape 4.0 4.0)))

;;; particle-system ------------------------------------------------------------
(with-clear-and-redraw
  (let ((p-sys (make-particle-system (make-point-cloud (p! 0 0 0)) (p! 0 .2 0) 10 -1 'particle
                                     :update-angle (range-float (/ pi 8) (/ pi 16))
                                     :life-span 10)))
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; particle-system force-field collisions -------------------------------------
(with-clear-and-redraw
  (let ((p-sys (make-particle-system (make-point-cloud (p! 0 2 0)) (p-rand .2) 2 -1 'dynamic-particle
                                     :life-span 20
                                     :do-collisions? t
                                     :force-fields (list (make-instance 'constant-force-field
                                                                        :force-vector (p! 0 -.02 0))))))
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; point-instancer particle-system --------------------------------------------
(with-clear-and-redraw
  (let* ((p-sys (make-particle-system (make-point-cloud (p! 0 0 0)) (p! 0 .2 0) 10 -1 'particle
                                      :update-angle (range-float (/ pi 8) (/ pi 16))
                                      :life-span (rand1 5 10))))
;    (setf (draw-live-points-only? p-sys) nil)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation
;;; instance shapes along particle system points
(with-redraw
    (add-shape *scene* (make-point-instancer (first (shapes *scene*))
                                             (make-octahedron .1))))

;;; point-instancer particle-system dependency-node-mixin ----------------------
(with-clear-and-redraw
  (let* ((p-sys (make-particle-system (make-point-cloud (p! 0 0 0)) (p! 0 .2 0) 10 -1 'particle
                                      :update-angle (range-float (/ pi 8) (/ pi 16))
                                      :life-span (rand1 5 10)))
         (shape (make-point-instancer p-sys
                                      (make-octahedron .1))))
    (setf (point-generator-use-live-positions-only p-sys) t)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)
    (add-shape *scene* shape)
    ))
;;; hold down space key in 3D view to run animation

;;; transform-instancer --------------------------------------------------------
(progn
  (defparameter *instancer*
    (make-transform-instancer (make-cube 1.0)
                              (make-transform (p! 0 7 0) (p! 0 (* 90 7/8) 0) (p! 1 1 0.2))
                              8))
  (with-clear-and-redraw
    (add-shape *scene* *instancer*)))
;;; change inputs and shape regenerates
(with-redraw
  (setf (instance-shape *instancer*) (make-icosahedron 0.5)))
(with-redraw
  (setf (num-steps *instancer*) 6))

;;; uv-mesh transform-instancer 1 ----------------------------------------------
(with-clear-and-redraw
  (let* ((path (make-sine-curve-shape 4 1 32))
         (prof (make-circle-shape 0.6 4))
         (mesh (first (sweep-extrude prof path :twist (* 2 pi) :taper 0.0)))
         (transform (make-instance 'transform
                                   :translate (p! 0 0 0) :rotate (p! 0 (* 360 7/8) 0) :scale (p! 1 1 1))))
    (add-shape *scene* (make-transform-instancer mesh transform 8))))
(with-redraw
  (setf (num-steps (first (shapes *scene*))) 4))

;;; uv-mesh transform-instancer 2 ----------------------------------------------
(with-clear-and-redraw
  (let* ((path (make-sine-curve-shape 4 1 32))
         (prof (make-circle-shape 0.6 4))
         (mesh (first (sweep-extrude prof path :twist (* 2 pi) :taper 0.0))))
    (set-point-colors-by-uv mesh #'(lambda (u v) (c-rainbow v)))
    (let* ((transform-1 (make-instance 'transform :rotate (p! 0 (* 360 7/8) 0)))
           (group-1 (make-transform-instancer mesh transform-1 8))
           (transform-2 (make-instance 'transform :translate (p! 0 6 0) :rotate (p! 0 45 0)))
           (group-2 (make-transform-instancer group-1 transform-2 6)))
      (add-shape *scene* group-2))))

;;; particle-system curve-shape force-field ------------------------------------
(with-clear-and-redraw
  (let* ((curve (make-circle-shape 4.0 16))
         (p-sys (make-particle-system curve (p! .2 .2 .2) 1 4 'dynamic-particle
                                      :force-fields (list (make-instance 'constant-force-field
                                                                         :force-vector (p! 0 -.02 0))))))
    (add-shape *scene* curve)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; particle-system curve-shape sweep-extrude ----------------------------------
(with-clear-and-redraw
  (let* ((p-gen (make-circle-shape 4.0 16))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 4 4 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32)))))
    (add-shape *scene* p-gen)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation
;;; do sweep along paths
(with-redraw
  (let ((group (apply #'make-group (sweep-extrude (make-circle-shape 0.5 6) (first (shapes *scene*))
                                                  :taper 0.0))))
    (set-point-colors-by-uv group #'(lambda (u v) (c-rainbow v)))
    (add-shape *scene* group)))

;;; particle-system point-generator-mixin uv-mesh ------------------------------
(with-clear-and-redraw
  (let* ((p-gen (make-grid-uv-mesh 25 25 (p! -4 0 -4) (p! 4 0 4) 0.0))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32)))))
    (add-shape *scene* p-gen)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation
;;; do sweep along paths
(with-redraw
  (let ((group (apply #'make-group (sweep-extrude (make-circle-shape 0.2 6) (first (shapes *scene*))
                                                  :taper 0.0))))
    (set-point-colors-by-uv group #'(lambda (u v) (declare (ignore u)) (c-rainbow v)))
    (add-shape *scene* group)))

;;; particle-system point-generator-mixin sweep-mesh-group ---------------------
(with-clear-and-redraw
  (let* ((p-gen (make-grid-uv-mesh 25 25 (p! -4 0 -4) (p! 4 0 4) 0.0))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32))))
         (sweep-mesh-group (make-sweep-mesh-group (make-circle-shape 0.2 6)
                                                  p-sys
                                                  :taper 0.0 :twist 2pi)))
;;    (add-shape *scene* p-gen)
;;    (add-shape *scene* p-sys)
    (add-shape *scene* sweep-mesh-group)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; particle-system point-generator-mixin sweep-mesh-group spawning ------------
(with-clear-and-redraw
  (let* ((p-gen (make-grid-uv-mesh 2 2 (p! -2 0 -2) (p! 2 0 2) 0.0))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 8 'particle
                                      :life-span (round (rand2 5 10))
                                      :update-angle (range-float (/ pi 16) (/ pi 32))))
         (sweep-mesh-group (make-sweep-mesh-group (make-circle-shape 0.2 6) p-sys
                                                  :taper 0.0 :twist 0.0)))
    (add-shape *scene* p-sys)
    (add-shape *scene* sweep-mesh-group)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; particle-system point-generator-mixin use polyh face centers ---------------
(with-clear-and-redraw
  (let ((p-gen (make-icosahedron 2.0)))
    (setf (point-generator-use-face-centers? p-gen) t)
    (let* ((p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'particle
                                        :life-span 10
                                        :update-angle (range-float (/ pi 16) (/ pi 32))
                                        :spawn-angle (range-float (/ pi 8) (/ pi 16))))
           (sweep-mesh-group (make-sweep-mesh-group (make-circle-shape 0.2 6) p-sys
                                                    :taper 0.0 :twist 0.0)))
      (add-shape *scene* p-gen)
      (add-shape *scene* p-sys)
      (add-shape *scene* sweep-mesh-group)
      (add-animator *scene* p-sys))))
;;; hold down space key in 3D view to run animation

;;; particle-system point-generator-mixin polyhedron ---------------------------
(with-clear-and-redraw
  (let* ((p-gen (polyhedron-bake (import-obj "~/Development/3D DCC Project/data/teapot.obj")))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'dynamic-particle
                                       :force-fields (list (make-instance 'constant-force-field
                                                                          :force-vector (p! 0 -.05 0))))))
    (add-shape *scene* p-gen)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation -- slow, profile & optimize

;;; particle-system point-generator-mixin particle-system ----------------------
(with-clear-and-redraw
  (let* ((p-gen (polyhedron-bake (translate-by
                                  (make-superquadric 8 5 2.0 1.0 1.0)
                                 ;(make-cut-cube-polyhedron 2.0)
                                 (p! 0 2 0))))
         (p-sys (make-particle-system p-gen (p! .4 .4 .4) 1 1 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32)))))
    (add-shape *scene* p-gen)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation
;;; make new particle-system generate from paths of existing particle-system
(with-redraw
  (clear-animators *scene*)
  (let* ((p-gen (first (shapes *scene*)))
         (p-sys (make-particle-system p-gen (p! .4 .4 .4) 1 1 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32)))))
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation
;;; do sweep-extrude
(with-redraw
  (let ((group (apply #'make-group (sweep-extrude (make-circle-shape 0.25 4) (first (shapes *scene*))
                                                  :taper 0.0))))
;;    (set-point-colors-by-uv group #'(lambda (u v) (c-rainbow v)))
    (add-shape *scene* group)))

;;; polyhedron curve-generator-mixin -------------------------------------------
(with-clear-and-redraw
  (let ((polyh (make-cut-cube-polyhedron 4.0)))
    (add-shape *scene* polyh)))
;;; sweep-extrude circle along polyh faces
(with-redraw
  (add-shape *scene* (apply #'make-group (sweep-extrude (make-circle-shape 0.5 6) (first (shapes *scene*))))))

;;; polyhedron subdivision -- new vertices are not merged ----------------------
(with-clear-and-redraw
  (let ((polyh (make-cut-cube-polyhedron 2.0)))
    (add-shape *scene* (refine-mesh polyh 4))))

;;; polyhedron subdivision -- new vertices are not merged ----------------------
(with-clear-and-redraw
  (let ((polyh (import-obj "~/Development/3D DCC Project/data/teapot.obj")))
    (add-shape *scene* (refine-mesh polyh 1))))

;;; half-edge-mesh -------------------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (translate-by (make-cube 4.0 'he-mesh) (p! 0 2 0))))
;;; select vertices
(with-redraw
  (select-vertex (first (shapes *scene*)) 7)
  (select-vertex (first (shapes *scene*)) 6))
;;; select edges
(with-redraw
  (select-edge (first (shapes *scene*)) 11)
  (select-edge (first (shapes *scene*)) 10))
;;; select faces
(with-redraw
  (select-face (first (shapes *scene*)) 2)
  (select-face (first (shapes *scene*)) 5))

;;; height-field ---------------------------------------------------------------
;;; try using various height functions and color functions
(defun height-fn-1 (x z)
  (* 4 (noise (p! x 0 z))))
(defun height-fn-2 (x z)
  (* 4 (turbulence (p! x 0 z) 3)))
(defun height-fn-3 (x z)
  (let* ((p (p! x 0 z))
         (mag (p-mag (p-scale p .25))))
    (if (= mag 0.0)
        10.0
        (/ 1.0 mag))))
(defun height-fn-4 (x z)
  (let* ((p (p! x 0 z))
         (mag (max 0.001 (p-mag (p-scale p 4)))))
    (* 3 (/ (sin mag) mag))))
(with-clear-and-redraw
  (let ((mesh (make-height-field 41 41 (p! -5 0 -5) (p! 5 0 5) #'height-fn-4)))
    (add-shape *scene* mesh)
;    (set-point-colors-by-xyz mesh #'(lambda (p) (c-rainbow (clamp (tween (y p) 0 .25) 0.0 1.0))))
;    (set-point-colors-by-xyz mesh #'(lambda (p) (c-rainbow (clamp (tween (p-mag (p! (x p) 0 (z p))) 0 5) 0.0 1.0))))
    (set-point-colors-by-xyz mesh #'(lambda (p) (color-noise p)))
    (translate-by mesh (p! 0 1 0))))      ;adjust to height values

;;; height-field ---------------------------------------------------------------
;;; adjust color function as desired
(defun terrain-color (p n)
  (cond ;((< (y n) 0.5)
        ; (c! 0.5 0.5 0.5))
        ((> (y p) 4.5)
         (c! 1 1 1))
        ;; ((> (y p) 4)
        ;;  (c! 0.5 0.5 0.5))
        (t
         (c! 0.1 0.6 0.1))))
(with-clear-and-redraw
  (let* ((freq 0.5)
         (ampl 8.0)
         (res 81)
         (octaves 6)
         (mesh (make-height-field res res (p! -5 0 -5) (p! 5 0 5)
                                 #'(lambda (x z)
                                     (* ampl (turbulence (p-scale (p! x 0 z) freq) octaves))))))
    (set-point-colors-by-point-and-normal mesh #'terrain-color)
    (translate-by mesh (p! 0 -2 0))
    (add-shape *scene* mesh)))

;;; USD scene export (not recently tested) ------------------------------------
(export-usd *scene* "foo.usda")
(export-usd-frame *scene* "foo")

;;; l-system ------------------------------------------------------------------
;;; uncomment an l-system to test
(with-clear-and-redraw
  (let ((l-sys
          ;; (make-koch-curve-l-system)
          ;; (make-binary-tree-l-system)
          ;; (make-serpinski-triangle-l-system)
          ;; (make-serpinski-arrowhead-l-system)
          ;; (make-dragon-curve-l-system)
           (make-fractal-plant-l-system)
          ))
    (add-shape *scene* l-sys)
    (add-animator *scene* l-sys)))
;;; press space key in 3D view to generate new l-system levels
;;; resize shape to convenient size and center shape at origin
(with-redraw
  (scale-to-size (first (shapes *scene*)) 10.0)
  (center-at-origin (first (shapes *scene*))))

;;; l-system sweep-mesh-group --------------------------------------------------
(with-clear-and-redraw
  (let* ((l-sys
          ;; (make-koch-curve-l-system)
          ;; (make-binary-tree-l-system)
          ;; (make-serpinski-triangle-l-system)
          ;; (make-serpinski-arrowhead-l-system)
          ;; (make-dragon-curve-l-system)
           (make-fractal-plant-l-system)
          )
         (sweep-mesh-group (make-sweep-mesh-group (make-circle-shape 1.0 6) l-sys
                                                  :taper 0.0 :twist 0.0)))
;    (add-shape *scene* l-sys)
    (setf (show-bounds? sweep-mesh-group) t)
    (add-shape *scene* sweep-mesh-group)
    (add-animator *scene* l-sys)))
;;; press space key in 3D view to generate new l-system levels
;;; resize shape to convenient size and center shape at origin
(with-redraw
  (scale-to-size (first (shapes *scene*)) 10.0)
  (center-at-origin (first (shapes *scene*))))


;;; ============================================================================

;;; multi-view setup -----------------------------------------------------------
;;; close view window and run grid view
(run-grid 4)

;;; grid view ------------------------------------------------------------------
;;; navigate camera and all views will sync up
(with-grid-clear-and-redraw
  (let ((n 3))
    (dolist (v *scene-views*)
      (let ((scene (scene v)))
        (add-shape scene (make-circle-shape 3.0 (incf n)))))))

;;; grid view superquadric ----------------------------------------------------
(with-grid-clear-and-redraw
  (dolist (v *scene-views*)
    (let ((scene (scene v))
          (mesh (make-superquadric 32 32 2.0 (rand2 0 3) (rand2 0 3))))
      (translate-by mesh (p! 0 2 0))
      (add-shape scene mesh))))

;;; grid view particle-system --------------------------------------------------
(with-grid-clear-and-redraw
  (dolist (v *scene-views*)
    (let ((scene (scene v))
          (p-sys (make-particle-system (make-point-cloud (p! 0 0 0)) (p! 0 .2 0) 1 3 'particle
                                       :life-span (round (rand2 5 10))
                                       :mutate-spawns? t
                                       :update-angle (range-float (/ pi 16) (/ pi 32))
                                       :spawn-number-children (range-float 4 3)
                                       :spawn-angle (range-float (/ pi 4) (/ pi 6))
                                       :spawn-life-span-factor (range-float 1.0 0.5)
                                       :spawn-velocity-factor (range-float 1.0 0.5))))
      (add-shape scene p-sys)
      (add-animator scene p-sys))))
;;; hold down space key in 3D view to run animation

;;; grid view dynamic-particle -------------------------------------------------
(with-grid-clear-and-redraw
  (dolist (v *scene-views*)
    (let ((scene (scene v))
          (p-sys (make-particle-system (make-point-cloud (p! 0 2 0)) (p-rand 0.2) 1 4 'dynamic-particle
                                       :life-span 10 ;(round (rand2 5 10))
                                       :mutate-spawns? t
                                       :update-angle (range-float 0 0) ;(/ pi 16) (/ pi 32))
                                       :spawn-number-children (range-float 4 3)
                                       :spawn-angle (range-float (/ pi 4) (/ pi 6))
                                       :spawn-life-span-factor (range-float 1.0 0.5)
                                       :spawn-velocity-factor (range-float 1.0 0.5)
                                       :do-collisions? t
                                       :force-fields (list (make-instance 'constant-force-field
                                                                          :force-vector (p! 0 -.01 0))))))
      (add-shape scene p-sys)
      (add-animator scene p-sys))))
;;; hold down space key in 3D view to run animation -- slow, profile & optimize

;;;; END ========================================================================
