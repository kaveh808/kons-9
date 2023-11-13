(in-package #:kons-9)

;;;; uv-mesh ============================================================

;;; class for parametrized quad meshes
(defclass uv-mesh (polyhedron)
  ((u-dim :accessor u-dim :initarg :u-dim :initform 0)
   (v-dim :accessor v-dim :initarg :v-dim :initform 0)
   (u-wrap :accessor u-wrap :initarg :u-wrap :initform nil)
   (v-wrap :accessor v-wrap :initarg :v-wrap :initform nil)
   (u-cap :accessor u-cap :initarg :u-cap :initform nil)
   (v-cap :accessor v-cap :initarg :v-cap :initform nil)
   (uv-point-array :accessor uv-point-array :initarg :uv-point-array :initform nil)
   ))

(defmethod printable-data ((self uv-mesh))
  (strcat (call-next-method) (format nil ", dims (~a ~a)" (u-dim self) (v-dim self))))

(defmethod copy-instance-data ((dst uv-mesh) (src uv-mesh))
  (error "COPY-INSTANCE-DATA not implemented for UV-MESH"))

(defmethod is-empty-mesh? ((mesh uv-mesh))
  (null (uv-point-array mesh)))

(defmethod wrapped-u-dim ((mesh uv-mesh))
  (if (u-wrap mesh)
      (1+ (u-dim mesh))
      (u-dim mesh)))

(defmethod wrapped-v-dim ((mesh uv-mesh))
  (if (v-wrap mesh)
      (1+ (v-dim mesh))
      (v-dim mesh)))

(defmethod allocate-mesh-arrays ((mesh uv-mesh))
  (setf (uv-point-array mesh) (make-array (list (u-dim mesh) (v-dim mesh)))))

(defmethod set-point-colors-by-uv ((mesh uv-mesh) color-fn)
  (when (> (length (points mesh)) 0)    ;need this because sweep-mesh seems to return
                                        ;some degenerate meshes with no points (see BUG in demo.lisp)
    (allocate-point-colors mesh)
    (dotimes (u (u-dim mesh))
      (let ((u0 (tween u 0.0 (1- (u-dim mesh)))))
        (dotimes (v (v-dim mesh))
          (let ((v0 (tween v 0.0 (1- (v-dim mesh)))))
            (setf (aref (point-colors mesh) (uv-mesh-1d-ref mesh  u v))
                  (funcall color-fn u0 v0))))))))

(defmethod set-point-colors-by-uv ((group shape-group) color-fn)
  (do-children (child group)
    (set-point-colors-by-uv child color-fn)))

(defun index+1 (index dim wrap?)
  (if wrap?
      (mod (1+ index) dim)
      (clamp (1+ index) 0 (1- dim))))

(defmethod u+1 ((mesh uv-mesh) index)
  (index+1 index (u-dim mesh) (u-wrap mesh)))

(defmethod v+1 ((mesh uv-mesh) index)
  (index+1 index (v-dim mesh) (v-wrap mesh)))

(defmethod compute-polyhedron-data ((mesh uv-mesh))
  (compute-polyhedron-mesh mesh)
  (compute-face-normals mesh)
  (compute-point-normals mesh)
  (allocate-point-colors mesh (c! 1 1 1))
  mesh)

;; (defun 2d-array-to-list (array)
;;   (let ((new-list '()))
;;     (loop for i below (array-dimension array 0)
;;           do (loop for j below (array-dimension array 1)
;;                    do (push (aref array i j) new-list)))
;;     (nreverse new-list)))

;;; NOTE: this does not copy the points, they will be shared between uv-point-array and polyhedron points
(defun flatten-array (array)
  "Return an n-dimensional array as a 1-dimensional vector."
  (let ((vector (make-array (array-total-size array))))
    (dotimes (i (array-total-size array))
      (setf (aref vector i) (row-major-aref array i)))
    vector))

(defmethod compute-polyhedron-mesh ((mesh uv-mesh))
  (setf (points mesh) (flatten-array (uv-point-array mesh)))
  ;; (setf (points mesh) (make-array (* (u-dim mesh) (v-dim mesh))
  ;;                                 :initial-contents (2d-array-to-list (uv-point-array mesh))))
  (setf (faces mesh) (make-array (+ (* (1- (wrapped-u-dim mesh)) (1- (wrapped-v-dim mesh)))
                                    (if (u-cap mesh) 2 0)
                                    (if (v-cap mesh) 2 0))
                                  :initial-contents (compute-face-list mesh))))

(defmethod uv-mesh-1d-ref ((mesh uv-mesh) i j)
  (+ j (* i (v-dim mesh))))

(defmethod compute-face-list ((mesh uv-mesh))
  (with-accessors ((u-dim u-dim) (v-dim v-dim))
      mesh
    (let ((faces '()))
      (dotimes (i (1- (wrapped-u-dim mesh)))
        (dotimes (j (1- (wrapped-v-dim mesh)))
          (push (list (uv-mesh-1d-ref mesh           i            j)
                      (uv-mesh-1d-ref mesh (u+1 mesh i)           j)
                      (uv-mesh-1d-ref mesh (u+1 mesh i) (v+1 mesh j))
                      (uv-mesh-1d-ref mesh           i  (v+1 mesh j)))
                faces)))
      (when (u-cap mesh)
        (let ((end-cap-1 '())
              (end-cap-2 '()))
          (dotimes (j (1- (wrapped-v-dim mesh)))
            (push (uv-mesh-1d-ref mesh 0                 j)
                  end-cap-1)
            (push (uv-mesh-1d-ref mesh (1- (u-dim mesh)) j)
                  end-cap-2))
          (push end-cap-1 faces)
          (push (nreverse end-cap-2) faces)))
      (when (v-cap mesh)
        (let ((end-cap-1 '())
              (end-cap-2 '()))
          (dotimes (i (1- (wrapped-u-dim mesh)))
            (push (uv-mesh-1d-ref mesh i                 0)
                  end-cap-1)
            (push (uv-mesh-1d-ref mesh i (1- (v-dim mesh)))
                  end-cap-2))
          (push end-cap-1 faces)
          (push (nreverse end-cap-2) faces)))
      (nreverse faces))))

;; (defun make-grid-uv-mesh (u-dim v-dim bounds-lo bounds-hi &optional (jitter nil))
;;   (let ((mesh (make-instance 'uv-mesh :u-dim u-dim :v-dim v-dim)))
;;     (allocate-mesh-arrays mesh)
;;     (setf (uv-point-array mesh) (grid-point-array u-dim v-dim bounds-lo bounds-hi jitter))
;;     (compute-polyhedron-data mesh)
;;     mesh))

(defun curve-tangent (i point-array &optional (is-closed? nil))
  (let ((len (length point-array))
	i1
	i2)
    (if (= len 2)
	(progn (setf i1 0)
	       (setf i2 1))
	(cond ((= i 0) (if is-closed?
			   (progn (setf i1 (1- len))
				  (setf i2 1))
			   (progn (setf i1 0)
				  (setf i2 1))))
	      ((= i (1- len)) (if is-closed?
				  (progn (setf i1 (- len 2))
					 (setf i2 0))
				  (progn (setf i1 (- len 3))
					 (setf i2 i))))
	      (t (progn (setf i1 (1- i))
			(setf i2 (1+ i))))))
    (p:normalize (p:- (aref point-array i2) (aref point-array i1)))))

(defun curve-remove-consecutive-duplicates (curve)
    (if (and curve (> (length curve) 1))
        (if (p:= (car curve) (cadr curve))
            (curve-remove-consecutive-duplicates (cons (car curve) (cddr curve)))
            (cons (car curve) (curve-remove-consecutive-duplicates (cdr curve))))
        curve))

;;; assumes profile curve has z-axis as normal
(defmethod sweep-extrude-aux ((mesh uv-mesh)
                              profile-point-array is-closed-profile?
                              path-point-array is-closed-path?
                              &key (twist 0.0) (taper 1.0) (from-end? nil))
  (when (or (< (length profile-point-array) 2)
            (< (length path-point-array) 2))
    (return-from sweep-extrude-aux (make-instance 'uv-mesh))) ;return empty mesh -- throw error?
  (setf (u-dim mesh) (length profile-point-array))
  (setf (v-dim mesh) (length path-point-array))
  (setf (u-wrap mesh) is-closed-profile?)
  (setf (v-wrap mesh) is-closed-path?)
  (setf (v-cap mesh) is-closed-profile?)
  (let* ((delta (/ 1.0 (1- (v-dim mesh))))
         (prev-tangent +z-axis+)
         (p0 +origin+)
         (point-array (copy-point-array profile-point-array))
         (path-points-2 (if from-end?
                            (reverse path-point-array)
                            path-point-array)))
    (allocate-mesh-arrays mesh)
    (do-array (v p1 path-points-2)
      (let* ((factor (tween v 0.0 (1- (v-dim mesh))))
             (tangent (curve-tangent v path-points-2 (v-wrap mesh)))
             (r1-mtx (make-axis-rotation-matrix (p-angle prev-tangent tangent)
                                                (p:cross prev-tangent tangent)
                                                p1))
             (r2-mtx (make-axis-rotation-matrix (* delta twist) tangent p1))
             (t-mtx (make-translation-matrix (p:- p1 p0)))
             (mtx (matrix-multiply-n t-mtx r1-mtx r2-mtx)))
        (transform-point-array! point-array mtx)
        (setf prev-tangent tangent)
        (setf p0 p1)
        (if (= 1.0 taper)               ;no need for scale transform
          (do-array (u p2 point-array)
            (setf (aref (uv-point-array mesh) u v) (p:copy p2)))
          (let ((scaled-point-array (copy-point-array point-array))
                (s-mtx (make-scale-matrix (p:lerp (p! 1 1 1) (p! taper taper taper) factor)
                                          p1)))
            (transform-point-array! scaled-point-array s-mtx)
            (do-array (u p2 scaled-point-array)
              (setf (aref (uv-point-array mesh) u v) (p:copy p2))))))))
  (compute-polyhedron-data mesh))

;;; sweep 0-th profile along all paths
(defmethod sweep-extrude (profiles paths &key (twist 0.0) (taper 1.0) (from-end? nil))
  (let ((meshes '())
        (profile-curve (elt (source-curves profiles) 0))
        (profile-closed (elt (source-curves-closed profiles) 0))
        (path-curves (source-curves paths))
        (path-closed (source-curves-closed paths)))
    (loop for curve in path-curves
          for is-closed? in path-closed
          do (push (sweep-extrude-aux (make-instance 'uv-mesh)
                                      profile-curve profile-closed
                                      curve is-closed?
                                      :twist twist :taper taper :from-end? from-end?)
                   meshes))
    (nreverse meshes)))

(defmethod sweep-extrude-uv-mesh (profile path &key (twist 0.0) (taper 1.0) (from-end? nil))
  (sweep-extrude-aux (make-instance 'uv-mesh)
                     (points profile) (is-closed-curve? profile)
                     (points path) (is-closed-curve? path)
                     :twist twist :taper taper :from-end? from-end?))

(defun transform-extrude-uv-mesh (profile transform num-steps &key (v-wrap nil) (u-cap nil) (v-cap t))
  (let ((mesh (make-instance 'uv-mesh :u-dim (length (points profile))
				      :v-dim (if v-wrap num-steps (1+ num-steps))
				      :u-wrap (is-closed-curve? profile)
				      :v-wrap v-wrap
                                      :u-cap u-cap
                                      :v-cap v-cap)))
    (allocate-mesh-arrays mesh)
    (dotimes (v (v-dim mesh))
      (let* ((factor (tween v 0.0 (1- (v-dim mesh))))
	     (points (transform-points (points profile) (transform-matrix transform factor))))
        (dotimes (u (u-dim mesh))
          (setf (aref (uv-point-array mesh) u v) (aref points u)))))
    (compute-polyhedron-data mesh)))

(defun function-extrude-uv-mesh (profile function num-steps &key (v-wrap nil) (u-cap nil) (v-cap t))
  (let ((mesh (make-instance 'uv-mesh :u-dim (length (points profile))
				      :v-dim (if v-wrap num-steps (1+ num-steps))
				      :u-wrap (is-closed-curve? profile)
				      :v-wrap v-wrap
                                      :u-cap u-cap
                                      :v-cap v-cap)))
    (allocate-mesh-arrays mesh)
    (dotimes (v (v-dim mesh))
      (let* ((factor (tween v 0.0 (1- (v-dim mesh))))
	     (points (funcall function (points profile) factor)))
        (dotimes (u (u-dim mesh))
          (setf (aref (uv-point-array mesh) u v) (aref points u)))))
    (compute-polyhedron-data mesh)))

;;; uv-mesh shape functions ----------------------------------------------------

(defun make-grid-uv-mesh (x-size z-size x-segments z-segments)
  (sweep-extrude-uv-mesh (make-line-curve (p! (/ x-size 2) 0 0) (p! (- (/ x-size 2)) 0 0) x-segments)
                         (make-line-curve (p! 0 0 (- (/ z-size 2))) (p! 0 0 (/ z-size 2)) z-segments)))

(defun make-cylinder-uv-mesh (diameter height radial-segments height-segments &key (taper 1.0))
  (sweep-extrude-uv-mesh (make-circle-curve diameter radial-segments)
                         (make-line-curve (p! 0 0 0) (p! 0 height 0) height-segments)
                         :taper taper))

(defun make-cylinder-uv-mesh-between-points (diameter p1 p2 radial-segments height-segments &key (taper 1.0))
  (sweep-extrude-uv-mesh (make-circle-curve diameter radial-segments)
                         (make-line-curve p1 p2 height-segments)
                         :taper taper))

(defun make-cone-uv-mesh (diameter height radial-segments height-segments)
  (make-cylinder-uv-mesh diameter height radial-segments height-segments :taper 0.0))

(defun make-rect-prism-uv-mesh (base-side height base-segments height-segments &key (taper 1.0))
  (sweep-extrude-uv-mesh (make-square-curve base-side base-segments)
                         (make-line-curve (p! 0 0 0) (p! 0 height 0) height-segments)
                         :taper taper))

(defun make-pyramid-uv-mesh (base-side height base-segments height-segments)
  (make-rect-prism-uv-mesh base-side height base-segments height-segments :taper 0.0))

(defun make-torus-uv-mesh (inner-diameter outer-diameter inner-segments outer-segments)
  (sweep-extrude-uv-mesh (make-circle-curve inner-diameter inner-segments)
                         (make-circle-curve outer-diameter outer-segments)))

(defun make-sphere-uv-mesh (diameter latitude-segments longitude-segments)
  (let ((xform (make-euler-transform (p! 0 0 0)
                                     (p! 0 (* 360 (/ (1- longitude-segments) longitude-segments)) 0)
                                     (p! 1 1 1))))
    (transform-extrude-uv-mesh (make-arc-curve diameter 0 -180 latitude-segments)
                               xform
                               longitude-segments
                               :v-wrap t
                               :v-cap nil)))

;;; version using function-extrude-uv-mesh
(defun make-sphere-uv-mesh-2 (diameter latitude-segments longitude-segments)
  (let ((xform (make-instance 'transform :rotate (p! 0 (* 360 (/ (1- longitude-segments) longitude-segments)) 0))))
    (function-extrude-uv-mesh (make-arc-curve diameter latitude-segments 0 (- pi))
                              (lambda (points f) (transform-points points (transform-matrix xform f)))
                              longitude-segments
                              :v-wrap t
                              :v-cap nil)))

;;;; gui =======================================================================

(defun uv-mesh-command-table ()
  (let ((table (make-instance `command-table :title "Create UV Mesh")))
    (ct-make-shape :C "Cone"     (make-cone-uv-mesh 2 2 16 7))
    (ct-make-shape :Y "Cylinder" (make-cylinder-uv-mesh 1.5 3 16 4))
    (ct-make-shape :G "Grid"     (make-grid-uv-mesh 3 1.5 1 1))
    (ct-make-shape :P "Prism"    (make-rect-prism-uv-mesh 1.5 3 4 2))
    (ct-make-shape :R "Pyramid"  (make-pyramid-uv-mesh 2 2 5 3))
    (ct-make-shape :S "Sphere"   (make-sphere-uv-mesh 1.5 8 16))
    (ct-make-shape :T "Torus"    (make-torus-uv-mesh 1.0 2.0 8 32))
    table))

(register-dynamic-command-table-entry
 "Create" :u "Create UV Mesh Menu"
 (lambda () (make-active-command-table (uv-mesh-command-table)))
 (lambda () t))
