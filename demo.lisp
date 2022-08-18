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

;;; kernel tests
;;; press 'h' in 3d view to see key bindings and navigation

;;; point-cloud ----------------------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (make-point-cloud (make-grid-points 10 2 5 (p! -2.0 -0.4 -1.0) (p! 2.0 0.4 1.0)))))

(with-clear-and-redraw
  (add-shape *scene* (make-point-cloud (make-random-points 500 (p! -1 -1 -1) (p! 1 1 1)))))

;;; polygons -------------------------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (translate-to (make-line-polygon (p! 0 0 0) (p! 2 2 2) 8) (p! 0 0 -6.0)))
  (add-shape *scene* (translate-to (make-rectangle-polygon 2 1 4) (p! 0 0 -4.0)))
  (add-shape *scene* (translate-to (make-square-polygon 1.5) (p! 0 0 -2.0)))
  (add-shape *scene* (translate-to (make-circle-polygon 2.0 16) (p! 0 0 0.0)))
  (add-shape *scene* (translate-to (make-arc-polygon 2.0 16 0 pi) (p! 0 0 2.0)))
  (add-shape *scene* (translate-to (make-sine-curve-polygon 360 1 2 1 16) (p! 0 0 4.0)))
  (add-shape *scene* (translate-to (make-spiral-polygon .2 2.0 -1.0 4 64) (p! 0 0 6.0))))

(with-clear-and-redraw
  (add-shape *scene* (translate-to (make-circle-polygon 3.0  7) (p! 0 0 -4.0)))
  (add-shape *scene* (translate-to (make-circle-polygon 3.0  6) (p! 0 0 -2.0)))
  (add-shape *scene* (translate-to (make-circle-polygon 3.0  5) (p! 0 0  0.0)))
  (add-shape *scene* (translate-to (make-circle-polygon 3.0  4) (p! 0 0  2.0)))
  (add-shape *scene* (translate-to (make-circle-polygon 3.0  3) (p! 0 0  4.0))))

;; polyhedrons -----------------------------------------------------------------
(with-clear-and-redraw
  (add-shapes *scene* (list
                       (translate-to (make-tetrahedron  2.0) (p! -5 0 0))
                       (translate-to (make-cube         2.0) (p! -2.5 0 0))
                       (translate-to (make-octahedron   2.0) (p! 0 0  0))
                       (translate-to (make-dodecahedron 2.0) (p!  2.5 0 0))
                       (translate-to (make-icosahedron  2.0) (p! 5 0 0)))))

(with-clear-and-redraw
  (add-shape *scene* (translate-to (refine-mesh (make-cube 2.0) 3) (p! 0 1 0))))

(with-clear-and-redraw
  (add-shape *scene* (translate-to (make-cube-sphere 2.0 3) (p! 0 1 0))))

;;; transforms and hierarchies -------------------------------------------------

(with-clear-and-redraw
  (defparameter *icosahedron* (make-icosahedron 1.0))
  (defparameter *cube* (make-cube 1.0))
  (defparameter *tetrahedron* (make-tetrahedron 1.0))
  (defparameter *group-1* (make-group *cube* *tetrahedron*))
  (defparameter *group-2* (make-group *group-1* *icosahedron*))
  (add-shape *scene* *group-2*)
  (do-hierarchy *group-2* (lambda (s) (setf (show-axis s) 1.0)))

  (translate-by *tetrahedron* (p! 0.0 1.5 0.0))
  (translate-by *cube* (p! 0.0 -1.5 0.0))
  (translate-by *group-1* (p! 1.5 0.0 0.0))
  (translate-by *icosahedron* (p! -2.0 0.0 0.0))

  ;; show hierarchy
  (print-hierarchy *group-2*))

(with-redraw
  (rotate-by *group-1* (p! 0.0 0.0 10.0)))

(with-redraw
  (rotate-by *group-2* (p! 10.0 0.0 0.0)))

;;; parent shape to transformed group - inherits transformation
;;; placed at two spots in hierarchy (instancing)
(with-redraw
  (defparameter *octahedron* (make-octahedron 1.0))
  (setf (show-axis *octahedron*) 1.0)
  (add-child *group-1* *octahedron*)
  (add-child *group-2* *octahedron*))

(with-redraw
  (translate-by *octahedron* (p! 0.0 0.0 -.5)))

;;; make group of shapes placed at points
(with-clear-and-redraw
  (add-shape *scene* (scatter-shapes-in-group (lambda () (make-cube 0.5))
                                              (make-grid-points 3 3 3 (p! -2 -2 -2) (p! 2 2 2)))))

;;; make group of shapes placed at points
(with-clear-and-redraw
  (add-shape *scene* (scatter-shapes-in-group (lambda () (make-octahedron 0.5))
                                              (make-circle-points 4.0 32))))

;;; randomly scale (uniformly) leaf nodes
(with-redraw
  (do-hierarchy (first (shapes *scene*))
    (lambda (shape) (scale-to shape (rand2 0.5 1.5)))
    :test #'is-leaf?))

;;; make robot arm
(with-clear-and-redraw
  (defparameter *waist-shape* (make-icosahedron 0.5))
  (defparameter *torso-shape* (make-box 0.6 0.8 0.2))
  (defparameter *shoulder-shape* (make-icosahedron 1.0))
  (defparameter *upper-arm-shape* (make-box 1.05 0.5 0.2))
  (defparameter *elbow-shape* (make-icosahedron 1.0))
  (defparameter *lower-arm-shape* (make-box 2.1 0.5 0.2))
  (defparameter *wrist-shape* (make-icosahedron 1.0))
  (defparameter *hand-shape* (make-box 1.5 1.2 0.4))

  (defparameter *wrist* (make-group *wrist-shape* *hand-shape*))
  (defparameter *elbow* (make-group *elbow-shape* *lower-arm-shape* *wrist*))
  (defparameter *shoulder* (make-group *shoulder-shape* *upper-arm-shape* *elbow*))
  (defparameter *torso* (make-group *shoulder-shape* *upper-arm-shape* *elbow*))
  (defparameter *waist* (make-group *waist-shape* *torso-shape* *shoulder*))

  (print-hierarchy *waist*)

  (add-shape *scene* *waist*)

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
)

;;; scene generics test
(progn
  (defparameter *x* (make-box 0.2 0.2 0.2))
  (setf (name *x*) 'yoyo)
  (add-child *wrist* *x*)
  (add-child *shoulder* *x*)
  (print (get-scene-paths *scene* *x*))
  (print (find-scene-item-by-name *scene* 'yoyo))
  (print (eq *x* (find-scene-item-by-name *scene* 'yoyo)))
  )

;;; turn off shading to see axes better (press 1 key)
(with-redraw
  (do-hierarchy *waist* (lambda (s) (setf (show-axis s) 1.0))))

(progn
  (defun reset-pose ()
    (rotate-to *waist* (p! 0 0 0))
    (rotate-to *shoulder* (p! 0 0 0))
    (rotate-to *elbow* (p! 0 0 0))
    (rotate-to *wrist* (p! 0 0 0)))

  (defun flex ()
    (rotate-by *waist* (p! 0 0 -1))
    (rotate-by *shoulder* (p! 0 0 -5))
    (rotate-by *elbow* (p! 0 0 15))
    (rotate-by *wrist* (p! 0 0 10))))

(with-redraw
  (flex))

(with-redraw
  (reset-pose))

;;; animators --  hold down space key to update scene, press 'a' key to reset animation

;;; animate shape translation
(with-clear-and-redraw
  (let ((shape (add-shape *scene* (make-cut-cube-polyhedron 2.0))))
    (add-animator *scene*
                  (make-instance 'animator
                                 :init-fn   (lambda () (translate-to shape (p! 0.0 0 0)))
                                 :update-fn (lambda () (translate-by shape (p! 0.1 0 0)))))))

;;; animate a group
(with-clear-and-redraw
  (let ((group (add-shape *scene* (scatter-shapes-in-group
                                   (lambda () (make-cube 0.5))
                                   (make-grid-points 3 3 3 (p! -2 -2 -2) (p! 2 2 2))))))
    (add-animator *scene*
                  (make-instance 'animator
                                 :init-fn (lambda ()
                                            (do-hierarchy group
                                              (lambda (shape) (rotate-to shape (p! 0 0 0)))
                                              :test #'is-leaf?))
                                 :update-fn (lambda ()
                                              (do-hierarchy group
                                                (lambda (shape) (rotate-by shape (p! 0 5 0)))
                                                :test #'is-leaf?))))))

;;; shape animator -- store rotation data for each shape
(with-clear-and-redraw
  (let ((group (add-shape *scene* (scatter-shapes-in-group
                                   (lambda () (make-cube 0.5))
                                   (make-grid-points 3 3 3 (p! -2 -2 -2) (p! 2 2 2))))))
    (do-hierarchy group
      (lambda (shape)
        (add-animator *scene*
                      (make-instance 'shape-animator
                                     :shape shape
                                     :init-fn (lambda (anim)
                                                (rotate-to (shape anim) (p! 0 0 0)))
                                     :update-fn (lambda (anim)
                                                  (rotate-by (shape anim) (anim-data anim :rotate)))
                                     :data `((:rotate . ,(p! 0 (rand1 10) 0))))))
      :test #'is-leaf?)))

;;; evaluate robot arm and functions above
(with-clear-and-redraw
  (add-shape *scene* *waist*)
  (reset-pose)
  (add-animator *scene*
                (make-instance 'shape-animator
                               :shape *waist*
                               :init-fn (lambda (anim) (rotate-to (shape anim) (p! 0 0 0)))
                               :update-fn (lambda (anim) (rotate-by (shape anim) (anim-data anim :rotate)))
                               :data `((:rotate . ,(p! 0 0 -2)))))
  (add-animator *scene*
                (make-instance 'shape-animator
                               :shape *shoulder*
                               :init-fn (lambda (anim) (rotate-to (shape anim) (p! 0 0 0)))
                               :update-fn (lambda (anim) (rotate-by (shape anim) (anim-data anim :rotate)))
                               :data `((:rotate . ,(p! 0 0 -6)))))
  (add-animator *scene*
                (make-instance 'shape-animator
                               :shape *elbow*
                               :init-fn (lambda (anim) (rotate-to (shape anim) (p! 0 0 0)))
                               :update-fn (lambda (anim) (rotate-by (shape anim) (anim-data anim :rotate)))
                               :data `((:rotate . ,(p! 0 0 12)))))
  (add-animator *scene*
                (make-instance 'shape-animator
                               :shape *wrist*
                               :init-fn (lambda (anim) (rotate-to (shape anim) (p! 0 0 0)))
                               :update-fn (lambda (anim) (rotate-by (shape anim) (anim-data anim :rotate)))
                               :data `((:rotate . ,(p! 0 0 9)))))
)

;;; animator using scene time and establishing constraint -- use of add-animator-at-end
(with-clear-and-redraw
  (let ((tetrahedron (translate-to (make-tetrahedron 2.0) (p! -1.5 0 0)))
        (dodecahedron (translate-to (make-dodecahedron 2.0) (p! 1.5 0 0))))
    (add-shapes *scene* (list tetrahedron dodecahedron))
    (add-animator *scene*
                  (make-instance 'shape-animator
                                 :shape tetrahedron
                                 :init-fn (lambda (anim) (translate-to (shape anim) (p! -1.5 0 0)))
                                 :update-fn (lambda (anim)
                                              (translate-to (shape anim)
                                                            (p! -1.5 (sin (current-time (scene anim))) 0)))))
    (add-animator-at-end *scene*
                         (make-instance 'shape-animator
                                        :shape dodecahedron
                                        :init-fn (lambda (anim) (translate-to (shape anim) (p! 1.5 0 0)))
                                        :update-fn (lambda (anim)
                                                     (let ((target-y (y (translate (transform (anim-data anim :target))))))
                                                       (translate-to (shape anim) (p! 1.5 (- target-y) 0))))
                                        :data `((:target . ,tetrahedron))))))
    
;;;; end kernel demos ==========================================================


;;;; start plugins demos =======================================================

;;; uv-mesh --------------------------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene*
             (make-group (translate-to (make-grid-uv-mesh 3 1.5 1 1) (p! 0 0 -6.0))
                         (translate-to (make-cylinder-uv-mesh 1.5 3 16 4) (p! 0 0 -4.0))
                         (translate-to (make-cone-uv-mesh 2 2 16 7) (p! 0 0 -2.0))
                         (translate-to (make-rect-prism-uv-mesh 1.5 3 4 2) (p! 0 0 0.0))
                         (translate-to (make-pyramid-uv-mesh 2 2 5 3) (p! 0 0 2.0))
                         (translate-to (make-torus-uv-mesh 1.0 2.0 8 32) (p! 0 0 4.0))
                         (translate-to (make-sphere-uv-mesh 1.5 8 16) (p! 0 0 6.0)))))

;(export-usd *scene* "~/foo11.usda")


;;; transform-extrude-uv-mesh --------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (transform-extrude-uv-mesh (make-rectangle-polygon 2 2 2)
                                                (make-transform (p! 2 1 4) (p! 90 90 60) (p! 1 .5 .2))
                                                16)))

;;; transform-extrude-uv-mesh --------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (transform-extrude-uv-mesh (make-circle-polygon 2.0 16)
                                                (make-transform (p! 0 0 4) (p! 0 0 360) (p! 2 .2 1))
                                                40)))

;;; sweep-extrude-uv-mesh ------------------------------------------------------
(with-clear-and-redraw
  (let* ((path (make-sine-curve-polygon 360 1 4 2 64))
         (prof (make-circle-polygon 1.0 4))
         (mesh (sweep-extrude-uv-mesh prof path :twist (* 2 pi) :taper 0.0)))
    (add-shape *scene* mesh)))
;;; assign point colors by uv
(with-redraw
  (set-point-colors-by-uv (first (shapes *scene*)) (lambda (u v) (c-rainbow v))))
;;; assign point colors by xyz
(with-redraw
  (set-point-colors-by-xyz (first (shapes *scene*)) (lambda (p) (c-rainbow (clamp (tween (y p) -2 2) 0.0 1.0)))))

;;; function-extrude-uv-mesh --------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (function-extrude-uv-mesh
                      (make-circle-polygon 2.0 16)
                      (lambda (points f)
                       (map 'vector (lambda (p)
                                      (p+ (p-jitter p (* .1 f)) (p! 0 0 (* 4 f))))
                            points))
                      20)))

;;; function-extrude-uv-mesh --------------------------------------------------
(with-clear-and-redraw
  (add-shape *scene* (function-extrude-uv-mesh
                      (make-circle-polygon 2.0 16)
                      (lambda (points f)
                       (map 'vector (lambda (p)
                                      (p+ (p* p (sin (* pi f)))
                                          (p! 0 0 (* 4 f))))
                            points))
                      20)))

;;; heightfield ---------------------------------------------------------------
;;; try using various height functions and color functions
(with-clear-and-redraw
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       (lambda (x z)
                                         (* 4 (noise (p! x 0 z)))))))

(with-clear-and-redraw
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       (lambda (x z)
                                         (* 4 (turbulence (p! x 0 z) 4))))))

(with-clear-and-redraw
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       (lambda (x z)
                                         (let* ((p (p! x 0 z))
                                                (mag (p-mag (p-scale p .25))))
                                           (if (= mag 0.0)
                                               10.0
                                               (/ 1.0 mag)))))))

(with-clear-and-redraw
  (add-shape *scene* (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                       (lambda (x z)
                                         (let* ((p (p! x 0 z))
                                                (mag (max 0.001 (p-mag (p-scale p 4)))))
                                           (* 3 (/ (sin mag) mag)))))))

;;; rainbow color based on height
(with-redraw
  (let ((mesh (first (shapes *scene*))))
    (set-point-colors-by-xyz mesh (lambda (p) (c-rainbow (clamp (tween (y p) -.25 1.0) 0.0 1.0))))))

;;; rainbow color based on XZ distance from origin
(with-redraw
  (let ((mesh (first (shapes *scene*))))
    (set-point-colors-by-xyz mesh (lambda (p) (c-rainbow (clamp (tween (p-mag (p! (x p) 0 (z p))) 0 8) 0.0 1.0))))))

;;; 3D color noise
(with-redraw
  (let ((mesh (first (shapes *scene*))))
    (set-point-colors-by-xyz mesh (lambda (p) (color-noise p)))))

;;; animated heightfield -------------------------------------------------------
(with-clear-and-redraw
  (let ((mesh (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5) nil)))
    (set-point-colors-by-xyz mesh (lambda (p) (c-rainbow (clamp (tween (p-mag (p! (x p) 0 (z p))) 0 8) 0.0 1.0))))
    (add-shape *scene* mesh)
    (macrolet ((my-height-fn (scale)
                 `(lambda (x z)
                    (let* ((p (p! x 0 z))
                           (mag (max 0.001 (p-mag (p-scale p ,scale)))))
                      (* 3 (/ (sin mag) mag))))))
      (add-animator *scene*
                    (make-instance 'animator
                                   :init-fn (lambda ()
                                              (setf (height-fn mesh) (my-height-fn 1.0))
                                              (update-heightfield mesh))
                                   :update-fn (lambda ()
                                                (setf (height-fn mesh) (my-height-fn (+ 1.0 (current-time *scene*))))
                                                (update-heightfield mesh)))))))

;;; procedural-mixin superquadric ----------------------------------------------
(with-clear-and-redraw
  (let ((mesh (make-superquadric 16 16 2.0 1 0.1)))
    (add-shape *scene* mesh)
    (translate-by mesh (p! 0 1 0))))
;;; modify slots and shape will change due to prcedural-mixin setup
(with-redraw
  (setf (e1 (first (shapes *scene*))) 0.5))
(with-redraw
  (setf (u-dim (first (shapes *scene*))) 32))

;;; animated superquadric
(with-clear-and-redraw
  (let ((mesh (make-superquadric 32 32 2.0 1.0 1.0)))
    (add-shape *scene* mesh)
    (translate-by mesh (p! 0 1 0))
    (add-animator *scene*
                  (make-instance 'animator
                                 :init-fn (lambda ()
                                            (setf (e1 mesh) 1.0)
                                            (setf (e2 mesh) 1.0))
                                 :update-fn (lambda ()
                                              (let ((p (p-normalize (noise-gradient (p! (+ (current-time *scene*) 0.123)
                                                                                        (+ (current-time *scene*) 0.347)
                                                                                        (+ (current-time *scene*) 0.965))))))
                                              (setf (e1 mesh) (* (abs (x p)) 2.0))
                                              (setf (e2 mesh) (* (abs (y p)) 2.0))))))))


;;; parametric-curve

(with-clear-and-redraw
  (add-shape *scene* (make-bezier-curve (p! -2 0 0) (p! -1 2 0) (p! 1 1 0) (p! 2 0 0))))

(with-clear-and-redraw
  (add-shape *scene* (make-butterfly-curve-polygon 1024)))


;;; xxx -- updated to here...

(with-clear-and-redraw
  (add-shape *scene* (import-obj "~/Downloads/cessna.obj")))
(with-clear-and-redraw
  (add-shape *scene* (import-obj "~/Downloads/shuttle.obj")))
(with-clear-and-redraw
  (add-shape *scene* (import-obj "~/Downloads/minicooper.obj")))




(with-clear-and-redraw
    (let ((circle (translate-to (make-circle-polygon 3.0  7) (p! 0 0 -4.0)))
          (superq (translate-by (make-superquadric 32 16 1.0 0.2 0.5) (p! 0 0 4.0)))
          (icos (make-icosahedron 2.0)))
      (setf (show-axis circle) 1.0)
      (setf (show-normals icos) 1.0)
      (setf (show-bounds? superq) t)
      (add-shapes *scene* (list circle superq icos))))

;;; procedural-mixin circle ----------------------------------------------------
(with-clear-and-redraw
  (let ((shape (make-instance 'procedural-circle-polygon :diameter 4.0 :num-points 8)))
    (add-shape *scene* shape)))
;;; modify slots and shape will change
(with-redraw
  (setf (num-points (first (shapes *scene*))) 32))
(with-redraw
  (setf (diameter (first (shapes *scene*))) 1.0))

;;; procedural-mixin sine curve ------------------------------------------------
(with-clear-and-redraw
  (let ((shape (make-instance 'procedural-sine-curve-polygon :num-points 64 :frequency 2 :period 720
                                                             :x-scale 4 :y-scale 2)))
    (add-shape *scene* shape)))
;;; modify slots and shape will change
(with-redraw
  (setf (frequency (first (shapes *scene*))) 1.0))
(with-redraw
  (setf (num-points (first (shapes *scene*))) 16))

;;; sweep-mesh dependency-node-mixin -------------------------------------------
(progn
  (defparameter *profile* (make-procedural-circle-polygon 0.8 4))
  (defparameter *path* (make-procedural-sine-curve-polygon 360 1 4 1 32))
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
  (defparameter *profile* (make-procedural-circle-polygon 1.2 4))
  (defparameter *path* (make-procedural-sine-curve-polygon 360 1 4 1 32))
  (defparameter *mesh* (make-sweep-mesh *profile* 0 *path* 0 :twist (* 2 pi) :taper 0.0))
  (with-clear-and-redraw
    (let ((anim (make-instance 'animator :init-fn (lambda (anim) (setf (num-points *profile*) 4) nil)
                                         :update-fn (lambda (anim) (incf (num-points *profile*))))))
      (add-animator *scene* anim)
      (add-shape *scene* *mesh*))))
;;; hold down space key in 3D view to run animation

;;; dynamics-animator ----------------------------------------------------------
(with-clear-and-redraw
  (let ((shapes '()))
    (dotimes (i 100) (push (make-cube 0.2) shapes))
    (add-shape *scene* (apply #'make-group shapes))
    (add-animators *scene*
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


(defparameter *example-object-filename* 
  (first '("~/Development/3D DCC Project/data/cow.obj"
	   "~/Development/3D DCC Project/data/teapot.obj"))
  "An example object filename used in demonstrations for the OBJ-IMPORT facility.

You can find obj files at

  https://people.sc.fsu.edu/~jburkardt/data/obj/obj.html

in this and demos below, update the *EXAMPLE-OBJECT-FILENAME* for your setup.")

(with-clear-and-redraw
  (add-shape *scene*
             (import-obj *example-object-filename*)))

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
                                       (import-obj *example-object-filename*)
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
  (let ((group (apply #'make-group (sweep-extrude (make-procedural-circle-polygon 0.1 8) (first (shapes *scene*))
                                                  :taper 1.0 :twist 0.0 :from-end? nil))))
    (set-point-colors-by-uv group (lambda (u v) (c! 0.1 0.5 0.1)))
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
  (setf (point-generator (first (shapes *scene*))) (import-obj *example-object-filename*)))
(with-redraw
  (setf (point-generator (first (shapes *scene*))) (make-procedural-sine-curve-polygon 360.0 1.0 4.0 4.0)))

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
  (let* ((path (make-procedural-sine-curve-polygon 360 1 4 1 32))
         (prof (make-procedural-circle-polygon 0.6 4))
         (mesh (first (sweep-extrude prof path :twist (* 2 pi) :taper 0.0)))
         (transform (make-instance 'transform
                                   :translate (p! 0 0 0) :rotate (p! 0 (* 360 7/8) 0) :scale (p! 1 1 1))))
    (add-shape *scene* (make-transform-instancer mesh transform 8))))
(with-redraw
  (setf (num-steps (first (shapes *scene*))) 4))

;;; uv-mesh transform-instancer 2 ----------------------------------------------
(with-clear-and-redraw
  (let* ((path (make-procedural-sine-curve-polygon 360 1 4 1 32))
         (prof (make-procedural-circle-polygon 0.6 4))
         (mesh (first (sweep-extrude prof path :twist (* 2 pi) :taper 0.0))))
    (set-point-colors-by-uv mesh (lambda (u v) (c-rainbow v)))
    (let* ((transform-1 (make-instance 'transform :rotate (p! 0 (* 360 7/8) 0)))
           (group-1 (make-transform-instancer mesh transform-1 8))
           (transform-2 (make-instance 'transform :translate (p! 0 6 0) :rotate (p! 0 45 0)))
           (group-2 (make-transform-instancer group-1 transform-2 6)))
      (add-shape *scene* group-2))))

;;; particle-system curve-shape force-field ------------------------------------
(with-clear-and-redraw
  (let* ((curve (make-procedural-circle-polygon 4.0 16))
         (p-sys (make-particle-system curve (p! .2 .2 .2) 1 4 'dynamic-particle
                                      :force-fields (list (make-instance 'constant-force-field
                                                                         :force-vector (p! 0 -.02 0))))))
    (add-shape *scene* curve)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; particle-system curve-shape sweep-extrude ----------------------------------
(with-clear-and-redraw
  (let* ((p-gen (make-procedural-circle-polygon 4.0 16))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 4 4 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32)))))
    (add-shape *scene* p-gen)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation
;;; do sweep along paths
(with-redraw
  (let ((group (apply #'make-group (sweep-extrude (make-procedural-circle-polygon 0.5 6) (first (shapes *scene*))
                                                  :taper 0.0))))
    (set-point-colors-by-uv group (lambda (u v) (c-rainbow v)))
    (add-shape *scene* group)))

;;; particle-system point-generator-mixin uv-mesh ------------------------------
(with-clear-and-redraw
  (let* ((p-gen (make-grid-uv-mesh 8 8 24 24))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32)))))
    (add-shape *scene* p-gen)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation
;;; do sweep along paths
(with-redraw
  (let ((group (apply #'make-group (sweep-extrude (make-procedural-circle-polygon 0.2 6) (first (shapes *scene*))
                                                  :taper 0.0))))
    (set-point-colors-by-uv group (lambda (u v) (declare (ignore u)) (c-rainbow v)))
    (add-shape *scene* group)))

;;; particle-system point-generator-mixin sweep-mesh-group ---------------------
(with-clear-and-redraw
  (let* ((p-gen (make-grid-uv-mesh 8 8 24 24))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'particle
                                      :update-angle (range-float (/ pi 16) (/ pi 32))))
         (sweep-mesh-group (make-sweep-mesh-group (make-procedural-circle-polygon 0.2 6)
                                                  p-sys
                                                  :taper 0.0 :twist 2pi)))
;;    (add-shape *scene* p-gen)
;;    (add-shape *scene* p-sys)
    (add-shape *scene* sweep-mesh-group)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; particle-system point-generator-mixin sweep-mesh-group spawning ------------
(with-clear-and-redraw
  (let* ((p-gen (make-grid-uv-mesh 4 4 1 1))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 8 'particle
                                      :life-span (round (rand2 5 10))
                                      :update-angle (range-float (/ pi 16) (/ pi 32))))
         (sweep-mesh-group (make-sweep-mesh-group (make-procedural-circle-polygon 0.2 6) p-sys
                                                  :taper 0.0 :twist 0.0)))
    (add-shape *scene* p-sys)
    (add-shape *scene* sweep-mesh-group)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation

;;; particle-system point-generator-mixin use polyh face centers ---------------
(with-clear-and-redraw
  (let ((p-gen (make-icosahedron 2.0)))
    (setf (point-source-use-face-centers? p-gen) t)
    (let* ((p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'particle
                                        :life-span 10
                                        :update-angle (range-float (/ pi 16) (/ pi 32))
                                        :spawn-angle (range-float (/ pi 8) (/ pi 16))))
           (sweep-mesh-group (make-sweep-mesh-group (make-procedural-circle-polygon 0.2 6) p-sys
                                                    :taper 0.0 :twist 0.0)))
      (add-shape *scene* p-gen)
      (add-shape *scene* p-sys)
      (add-shape *scene* sweep-mesh-group)
      (add-animator *scene* p-sys))))
;;; hold down space key in 3D view to run animation

;;; particle-system point-generator-mixin polyhedron ---------------------------
(with-clear-and-redraw
  (let* ((p-gen (import-obj "~/Development/3D DCC Project/data/teapot.obj"))
         (p-sys (make-particle-system p-gen (p! .2 .2 .2) 1 4 'dynamic-particle
                                       :force-fields (list (make-instance 'constant-force-field
                                                                          :force-vector (p! 0 -.05 0))))))
    (add-shape *scene* p-gen)
    (add-shape *scene* p-sys)
    (add-animator *scene* p-sys)))
;;; hold down space key in 3D view to run animation -- slow, profile & optimize

;;; particle-system point-generator-mixin particle-system ----------------------
(with-clear-and-redraw
  (let* ((p-gen (polyhedron-bake (translate-by (make-superquadric 8 5 2.0 1.0 1.0)
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
  (let ((group (apply #'make-group (sweep-extrude (make-procedural-circle-polygon 0.25 4) (first (shapes *scene*))
                                                  :taper 0.0))))
;;    (set-point-colors-by-uv group (lambda (u v) (c-rainbow v)))
    (add-shape *scene* group)))

;;; polyhedron curve-generator-mixin -------------------------------------------
(with-clear-and-redraw
  (let ((polyh (make-cut-cube-polyhedron 4.0)))
    (add-shape *scene* polyh)))
;;; sweep-extrude circle along polyh faces
(with-redraw
  (add-shape *scene* (apply #'make-group (sweep-extrude (make-procedural-circle-polygon 0.5 6) (first (shapes *scene*))))))

;;; polyhedron subdivision -- new vertices are merged ----------------------
(with-clear-and-redraw
  (let ((polyh (make-cut-cube-polyhedron 2.0)))
    (add-shape *scene* (refine-mesh polyh 4))))

;;; polyhedron subdivision -- new vertices are merged ----------------------
(with-clear-and-redraw
  (let ((polyh (import-obj "~/Development/3D DCC Project/data/teapot.obj")))
    (add-shape *scene* (refine-mesh polyh 1))))

#| TODO -- half-edge-mesh commented out for now
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
|#


;;; USD scene export (not recently tested) ------------------------------------
(export-usd *scene* "~/foo1.usda")
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
         (sweep-mesh-group (make-sweep-mesh-group (make-procedural-circle-polygon 1.0 6) l-sys
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

#| TODO -- multi-view not tested for now

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

|#

;;;; END ========================================================================
