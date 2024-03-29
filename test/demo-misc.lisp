(in-package #:kons-9)

#|
These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)
|#

;;;; start misc demos ==========================================================

;;; parametric-curve -----------------------------------------------------------

(format t "  parametric-curve...~%") (finish-output)

(with-clear-scene
  (add-shape *scene* (make-bezier-curve (p! -2 0 0) (p! -1 2 0) (p! 1 1 0) (p! 2 0 0))))

(with-clear-scene
  (add-shape *scene* (make-butterfly-curve 1024)))

;;; poly-mesh select components ------------------------------------------------

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
(progn
  (select-face (find-shape-by-name *scene* 'cube) 2)
  (select-face (find-shape-by-name *scene* 'cube) 5))

;;; poly-mesh select vertex neighbors ------------------------------------------

;;; select vertex neighbor vertices
(with-clear-scene
  (let ((mesh (make-dodecahedron 4.0 :mesh-type 'poly-mesh)))
    (add-shape *scene* mesh)
    (select-vertex mesh 0)
    (let* ((vertex (aref (pm-vertices mesh) 0)))
      (dolist (v (pm-vertex-vertices vertex))
        (setf (selected? v) t)))))
  
;;; select vertex neighbor edges
(with-clear-scene
  (let ((mesh (make-dodecahedron 4.0 :mesh-type 'poly-mesh)))
    (add-shape *scene* mesh)
    (select-vertex mesh 0)
    (let* ((vertex (aref (pm-vertices mesh) 0)))
      (dolist (e (pm-vertex-edges vertex))
        (setf (selected? e) t)))))
  
;;; select vertex neighbor faces
(with-clear-scene
  (let ((mesh (make-dodecahedron 4.0 :mesh-type 'poly-mesh)))
    (add-shape *scene* mesh)
    (select-vertex mesh 0)
    (let* ((vertex (aref (pm-vertices mesh) 0)))
      (dolist (f (pm-vertex-faces vertex))
        (setf (selected? f) t)))))
  
;;; select edge neighbor faces
(with-clear-scene
  (let ((mesh (make-dodecahedron 4.0 :mesh-type 'poly-mesh)))
    (add-shape *scene* mesh)
    (select-edge mesh 0)
    (let* ((edge (aref (pm-edges mesh) 0)))
      (dolist (f (pm-edge-faces edge))
        (setf (selected? f) t)))))
  
;;; select face neighbor edges
(with-clear-scene
  (let ((mesh (make-dodecahedron 4.0 :mesh-type 'poly-mesh)))
    (add-shape *scene* mesh)
    (select-face mesh 0)
    (let* ((face (aref (pm-faces mesh) 0)))
      (dolist (e (pm-face-edges face))
        (setf (selected? e) t)))))
  
;;; select face neighbor faces
(with-clear-scene
  (let ((mesh (make-dodecahedron 4.0 :mesh-type 'poly-mesh)))
    (add-shape *scene* mesh)
    (select-face mesh 0)
    (let* ((face (aref (pm-faces mesh) 0)))
      (dolist (f (pm-face-faces face))
        (setf (selected? f) t)))))
  
;;; subdiv-mesh ----------------------------------------------------------------

;; refine-subdiv-mesh
(with-clear-scene
  (let ((mesh (translate-to (make-cube 2.0 :mesh-type 'refine-subdiv-mesh)
                            ;(make-square-polyhedron 2.0 :mesh-type 'refine-subdiv-mesh)
                            (p! 4 0 0))))
    (add-shape *scene* mesh)
    (let* ((subdiv (subdivide-mesh mesh)))
      (add-shape *scene* subdiv)
      (randomize-points subdiv (p! .25 .25 .25))
      (let ((subdiv2 (subdivide-mesh subdiv 4)))
        (add-shape *scene* subdiv2)
        (translate-to subdiv2 (p! -4 0 0))))))

;; smooth-subdiv-mesh
(with-clear-scene
  (let ((mesh (translate-to (make-cube 2.0 :mesh-type 'smooth-subdiv-mesh)
                            ;(make-square-polyhedron 2.0 :mesh-type 'smooth-subdiv-mesh)
                            (p! 4 0 0))))
    (add-shape *scene* mesh)
    (let* ((subdiv (subdivide-mesh mesh)))
      (add-shape *scene* subdiv)
      (randomize-points subdiv (p! .25 .25 .25))
      (let ((subdiv2 (subdivide-mesh subdiv 4)))
        (add-shape *scene* subdiv2)
        (translate-to subdiv2 (p! -4 0 0))))))

;; fractal-subdiv-mesh
(with-clear-scene
  (let ((mesh (translate-to (make-cube 2.0 :mesh-type 'fractal-subdiv-mesh)
                            ;(make-square-polyhedron 2.0 :mesh-type 'fractal-subdiv-mesh)
                            (p! 2 0 0))))
    (add-shape *scene* mesh)
    (setf (vertex-displacement mesh) 0.4)
    (let* ((subdiv (subdivide-mesh mesh 5)))
      (add-shape *scene* subdiv)
        (translate-to subdiv (p! -2 0 0)))))

;;; animated subdivide-mesh ----------------------------------------------------

(defun make-animated-subdiv-scene (mesh levels)
  (let ((group (make-instance 'variant-manager-group
                              :children (subdivide-mesh-into-array mesh levels))))
    (compute-procedural-node group)     ;need to manually trigger compute node after creation
    (add-shape *scene* group)
    (add-motion *scene* 
                (make-instance 'animator
                               :setup-fn (lambda () (setf (visible-index group) 0))
                               :update-fn (lambda () (setf (visible-index group)
                                                           (current-frame *scene*)))))
    (setf (end-frame *scene*) (1- (length (children group))))))

;; fractal-subdiv-mesh
(with-clear-scene
  (let* ((base-mesh (freeze-transform (rotate-by (make-square-polyhedron 6.0 :mesh-type 'fractal-subdiv-mesh) (p! -90 0 0)))))
    (make-animated-subdiv-scene base-mesh 7)))

;; smooth-subdiv-mesh
(with-clear-scene
  (let* ((base-mesh (make-cube 4.0 :mesh-type 'smooth-subdiv-mesh)))
    (randomize-points base-mesh (p! 1 1 1))
    (make-animated-subdiv-scene base-mesh 7)))

;; smooth-subdiv-mesh with edge sharpness
(with-clear-scene
  (let* ((base-mesh (make-cube 4.0 :mesh-type 'smooth-subdiv-mesh)))
    (select-edges base-mesh '(0 1 2 3 4 5 6 7))
    (map 'vector
         (lambda (e) (setf (sharpness e) 8.0))
         (selected-edges base-mesh))
    (make-animated-subdiv-scene base-mesh 7)))

;; smooth-subdiv-mesh with fractional edge sharpness -- cube
(with-clear-scene
  (let* ((base-mesh (make-cube 4.0 :mesh-type 'smooth-subdiv-mesh)))
    (select-edges base-mesh '(0 1 2 3)) ;4 5 6 7))
    (map 'vector
         (lambda (e) (setf (sharpness e) 1.2))
         (selected-edges base-mesh))
    (make-animated-subdiv-scene base-mesh 7)))

;; smooth-subdiv-mesh with fractional edge sharpness -- grid
(with-clear-scene
  (let ((uv-mesh (make-grid-uv-mesh 6.0 6.0 2 2)))
    (setf (aref (points uv-mesh) 4) (p! 0 2 0))
    (let ((base-mesh (make-instance 'smooth-subdiv-mesh :points (points uv-mesh) :faces (faces uv-mesh))))
      (unselect-all-edges base-mesh)
      (select-edges base-mesh '(1 4))
      (map 'vector (lambda (e) (setf (sharpness e) 3.5)) (selected-edges base-mesh))
      (unselect-all-edges base-mesh)
      (select-edges base-mesh '(2 9))
      (map 'vector (lambda (e) (setf (sharpness e) 0.5)) (selected-edges base-mesh))
      (make-animated-subdiv-scene base-mesh 7))))

;; smooth-subdiv-mesh with fractional edge sharpness -- octahedron
(with-clear-scene
  (let ((base-mesh (make-octahedron 6.0 :mesh-type 'smooth-subdiv-mesh)))
    ;; vertical ring 1
    (unselect-all-edges base-mesh)
    (select-edges base-mesh '(1 4 6 7))
    (map 'vector (lambda (e) (setf (sharpness e) 2.5)) (selected-edges base-mesh))
    ;; vertical ring 2
    (unselect-all-edges base-mesh)
    (select-edges base-mesh '(0 5 8 11))
    (map 'vector (lambda (e) (setf (sharpness e) 0.5)) (selected-edges base-mesh))
    ;; equator ring
    (unselect-all-edges base-mesh)
    (select-edges base-mesh '(2 3 9 10))
    (map 'vector (lambda (e) (setf (sharpness e) 8.0)) (selected-edges base-mesh))
    ;; create scene
    (make-animated-subdiv-scene base-mesh 7)))

;; refine-subdiv-mesh
(with-clear-scene
  (let* ((base-mesh (make-cube 4.0 :mesh-type 'refine-subdiv-mesh)))
    (randomize-points base-mesh (p! 1 1 1))
    (make-animated-subdiv-scene base-mesh 7)))

;; multi-shape scene
(with-clear-scene
  (let* ((base-mesh-1 (freeze-transform (translate-to (make-cube 4.0 :mesh-type 'smooth-subdiv-mesh)
                                                      (p! -5 0 0))))
         (base-mesh-2 (freeze-transform (translate-to (make-cube 4.0 :mesh-type 'refine-subdiv-mesh)
                                                      (p!  0 0 0))))
         (base-mesh-3 (freeze-transform (translate-to (make-cube 4.0 :mesh-type 'fractal-subdiv-mesh)
                                                      (p!  5 0 0)))))
    (setf (vertex-displacement base-mesh-3) 0.5)
    (make-animated-subdiv-scene base-mesh-1 7)
    (make-animated-subdiv-scene base-mesh-2 7)
    (make-animated-subdiv-scene base-mesh-3 7)
    ))

;; multi-shape scene
;; TODO -- slow to create, profile code (probably building half-edge structure)
#|
(with-clear-scene
  (let* ((filename (asdf:system-relative-pathname "kons-9" "test/data/cow.obj"))
         (polyh-1 (import-obj filename))
         (polyh-2 (import-obj filename))
         (polyh-3 (import-obj filename))
         (base-mesh-1 (freeze-transform (translate-to (make-instance 'smooth-subdiv-mesh
                                                                     :points (points polyh-1)
                                                                     :faces (faces polyh-1))
                                                      (p! 0 0 -5))))
         (base-mesh-2 (freeze-transform (translate-to (make-instance 'refine-subdiv-mesh
                                                                     :points (points polyh-2)
                                                                     :faces (faces polyh-2))
                                                      (p! 0 0  0))))
         (base-mesh-3 (freeze-transform (translate-to (make-instance 'fractal-subdiv-mesh
                                                                     :points (points polyh-3)
                                                                     :faces (faces polyh-3))
                                                      (p! 0 0  5)))))
    (setf (vertex-displacement base-mesh-3) 0.05)
    (make-animated-subdiv-scene base-mesh-1 3)
    (make-animated-subdiv-scene base-mesh-2 3)
    (make-animated-subdiv-scene base-mesh-3 3)
    ))
|#

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

;;; l-system ------------------------------------------------------------------

(format t "  l-system...~%") (finish-output)

;;; uncomment an l-system to test
(with-clear-scene
  (defparameter *l-sys*
    ;; (make-koch-curve-l-system)
    ;; (make-binary-tree-l-system)
    ;; (make-serpinski-triangle-l-system)
    ;; (make-serpinski-arrowhead-l-system)
     (make-dragon-curve-l-system)
    ;; (make-fractal-plant-l-system)
    )
  (add-shape *scene* *l-sys*)
  (add-motion *scene* *l-sys*)
    ;; resize shape to convenient size and center shape at origin
  (add-motion *scene*
              (make-instance 'animator
                             :update-fn (lambda ()
                                          (scale-to-size *l-sys* 5.0)
                                          (center-at-origin *l-sys*))))
  ;; grow 5 levels to have a representative shape
  (update-scene *scene* 5))

;;; WARNING -- pressing space key in 3D view to update l-system growth hangs for some of these
;;; past level 5 or so -- need to investigate

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

;;; xyz molecule import --------------------------------------------------------

(format t "  xyz import...~%") (finish-output)

(defparameter *example-xyz-filename* 
  (nth 1 (list (asdf:system-relative-pathname "kons-9" "test/data/benzoic-acid.xyz")
               (asdf:system-relative-pathname "kons-9" "test/data/caffeine.xyz")))
  "An example molecule structure filename used in demonstrations for the XYZ-IMPORT facility.
You can find xyz files at
  https://github.com/nutjunkie/IQmol3/tree/master/share/fragments/Molecules
in this and demos below, update the *EXAMPLE-XYZ-FILENAME* for your setup.")

(with-clear-scene
  (add-shape *scene*
             (import-xyz *example-xyz-filename*)))

;;; mol molecule import --------------------------------------------------------

(format t "  mol import...~%") (finish-output)

(defparameter *example-mol-filename* 
  (nth 0 (list (asdf:system-relative-pathname "kons-9" "test/data/caffeine.mol")
               (asdf:system-relative-pathname "kons-9" "test/data/cholesterol.mol")))
  "An example molecule structure filename used in demonstrations for the MOL-IMPORT facility.
You can find mol files at
  https://github.com/nutjunkie/IQmol3/tree/master/share/fragments/Molecules
in this and demos below, update the *EXAMPLE-MOL-FILENAME* for your setup.")

;;; show molecule atoms only (no bonds)
(with-clear-scene
  (add-shape *scene*
             (import-mol *example-mol-filename*)))

;;; show molecule bonds
(with-clear-scene
  (add-shape *scene*
             (import-mol *example-mol-filename* :show-bonds? t)))

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
                                      :update-angle (range-float 20.0 10.0)
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
                                      :update-angle (range-float 20.0 10.0)
                                      :life-span (rand1 5 10)))
         (shape (make-point-instancer-group p-sys
                                            (make-octahedron .2))))
    ;;; uncomment to only instance at live position -- SLOT REMOVED FOR NOW
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

;;; polyhedron fractalize and variant-manager-group ----------------------------

(defun make-animated-fractal-scene (polyh displacement levels)
  (let ((group (make-instance 'variant-manager-group
                              :children (fractalize-polyhedron-into-array polyh
                                                                          displacement
                                                                          levels))))
    (compute-procedural-node group)     ;need to manually trigger compute node after creation
    (add-shape *scene* group)
    (add-motion *scene* 
                (make-instance 'animator
                               :setup-fn (lambda () (setf (visible-index group) 0))
                               :update-fn (lambda () (setf (visible-index group)
                                                           (current-frame *scene*)))))
    (setf (end-frame *scene*) (1- (length (children group))))))

(with-clear-scene
  (let* ((base-polyh (freeze-transform (rotate-by (make-square-polyhedron 6.0) (p! -90 0 0)))))
    (make-animated-fractal-scene base-polyh 2.0 7)))

;;; press space key in 3D view to show next fractal level
;;; press [ to return to base shape (frame 0)

(with-clear-scene
  (let* ((base-polyh (make-cube 4.0)))
    (make-animated-fractal-scene base-polyh 1.0 7)))

;;; press space key in 3D view to show next fractal level
;;; press [ to return to base shape (frame 0)


(with-clear-scene
  (let* ((base-polyh (make-dodecahedron 6.0)))
    (make-animated-fractal-scene base-polyh 0.5 6)))

;;; press space key in 3D view to show next fractal level
;;; press [ to return to base shape (frame 0)

;;;; END ========================================================================
