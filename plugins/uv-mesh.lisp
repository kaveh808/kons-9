(in-package #:kons-9)

;;;; uv-mesh ============================================================

;;; class for parametrized quad meshes
(defclass uv-mesh (polyhedron)
  ((u-dim :accessor u-dim :initarg :u-dim :initform 16)
   (v-dim :accessor v-dim :initarg :v-dim :initform 16)
   (u-wrap :accessor u-wrap :initarg :u-wrap :initform nil)
   (v-wrap :accessor v-wrap :initarg :v-wrap :initform nil)
   (u-cap :accessor u-cap :initarg :u-cap :initform nil)
   (v-cap :accessor v-cap :initarg :v-cap :initform nil)
   (uv-point-array :accessor uv-point-array :initarg :uv-point-array :initform nil)
   ))

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
  (allocate-point-colors mesh)
  (dotimes (u (u-dim mesh))
    (let ((u0 (tween u 0.0 (1- (u-dim mesh)))))
      (dotimes (v (v-dim mesh))
        (let ((v0 (tween v 0.0 (1- (v-dim mesh)))))
          (setf (aref (point-colors mesh) (uv-mesh-1d-ref mesh  u v))
                (funcall color-fn u0 v0)))))))

(defmethod set-point-colors-by-uv ((group group) color-fn)
  (dolist (child (children group))
    (set-point-colors-by-uv child color-fn)))

(defun index+1 (index dim wrap?)
  (if wrap?
      (mod (1+ index) dim)
      (clamp (1+ index) 0 (1- dim))))

(defmethod u+1 ((mesh uv-mesh) index)
  (index+1 index (u-dim mesh) (u-wrap mesh)))

(defmethod v+1 ((mesh uv-mesh) index)
  (index+1 index (v-dim mesh) (v-wrap mesh)))

;; (defun grid-point-array (u-dim v-dim &optional (bounds-lo (p! -1 0 -1)) (bounds-hi (p! 1 0 1)) (jitter nil))
;;   (let ((uv-point-array (make-array (list u-dim v-dim))))
;;     (dotimes (u u-dim)
;;       (let* ((fx (/ u (- u-dim 1.0)))
;; 	     (x (lerp fx (x bounds-lo) (x bounds-hi))))
;; 	(dotimes (v v-dim)
;; 	  (let* ((fz (/ v (- v-dim 1.0)))
;; 		 (z (lerp fz (z bounds-lo) (z bounds-hi))))
;; 	    (setf (aref uv-point-array u v)
;; 		  (if jitter
;; 		      (p-jitter (p! z 0.0 x) jitter)
;; 		      (p! z 0.0 x)))))))
;;     uv-point-array))

(defmethod compute-polyhedron-data ((mesh uv-mesh))
  (compute-polyhedron-mesh mesh)
  (compute-face-normals mesh)
  (compute-point-normals mesh)
  (allocate-point-colors mesh)
  mesh)

;; (defun 2d-array-to-list (array)
;;   (let ((new-list '()))
;;     (loop for i below (array-dimension array 0)
;;           do (loop for j below (array-dimension array 1)
;;                    do (push (aref array i j) new-list)))
;;     (nreverse new-list)))

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

(defun curve-tangent (i points &optional (is-closed? nil))
  (let ((len (length points))
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
    (p-normalize (p- (nth i2 points) (nth i1 points)))))

(defun curve-remove-consecutive-duplicates (curve)
    (if (and curve (> (length curve) 1))
        (if (p= (car curve) (cadr curve))
            (curve-remove-consecutive-duplicates (cons (car curve) (cddr curve)))
            (cons (car curve) (curve-remove-consecutive-duplicates (cdr curve))))
        curve))

;;; assumes profile curve has z-axis as normal
(defmethod sweep-extrude-aux ((mesh uv-mesh) profile-points is-closed-profile? path-points is-closed-path?
                              &key (twist 0.0) (taper 1.0) (from-end? nil))
  (let ((unique-path-points (curve-remove-consecutive-duplicates (coerce path-points 'list)))); fix this
    (when (or (< (length profile-points) 2)
              (< (length unique-path-points) 2))
      (return-from sweep-extrude-aux (make-instance 'uv-mesh))) ;return empty mesh -- throw error?
    (setf (u-dim mesh) (length profile-points))
    (setf (v-dim mesh) (length unique-path-points))
    (setf (u-wrap mesh) is-closed-profile?)
    (setf (v-wrap mesh) is-closed-path?)
    (setf (v-cap mesh) t)
    (let* ((delta (/ 1.0 (1- (v-dim mesh))))
           (prev-tangent +z-axis+)
           (p0 +origin+)
           (points (copy-points profile-points))
           (path-points-2 (if from-end?
                              (reverse unique-path-points)
                              unique-path-points)))
      (allocate-mesh-arrays mesh)
      (loop :for p1 :in path-points-2
            :for v :from 0
            :do (let* ((factor (tween v 0.0 (- (v-dim mesh) 1)))
                       (tangent (curve-tangent v path-points-2 (v-wrap mesh))))
                  (when (p= tangent +origin+) ;heuristic to avoid null tangent in P0-P1-P0 case
                    (setf tangent prev-tangent))
                  (let* ((r1-mtx (make-axis-rotation-matrix (p-angle prev-tangent tangent)
                                                            (p-cross prev-tangent tangent)
                                                            p1))
                         (r2-mtx (make-axis-rotation-matrix (* delta twist) tangent p1))
                         (t-mtx (make-translation-matrix (p- p1 p0)))
                         (mtx (matrix-multiply-n t-mtx r1-mtx r2-mtx)))
                    (transform-points! points mtx)
                    (setf prev-tangent tangent)
                    (setf p0 p1)
                    (let ((scaled-points (copy-points points))
                          (s-mtx (make-scale-matrix (p-lerp factor (p! 1 1 1) (p! taper taper taper))
                                                    p1)))
                      (transform-points! scaled-points s-mtx)		      
                      (loop :for p2 :in scaled-points
                            :for u :from 0
                            :do (setf (aref (uv-point-array mesh) u v) (p-copy p2)))))))
      (compute-polyhedron-data mesh))))

;;; TODO -- cleanup
;;; for now sweep 0-th profile along all paths
(defmethod sweep-extrude (profiles paths &key (twist 0.0) (taper 1.0) (from-end? nil))
  (let ((meshes '())
        (profile-curve (coerce (elt (source-curves profiles) 0) 'list)) ;TODO -- fix this...
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

;;; TODO -- fix coerce to list
(defmethod sweep-extrude-uv-mesh (profile path &key (twist 0.0) (taper 1.0) (from-end? nil))
  (sweep-extrude-aux (make-instance 'uv-mesh)
                     (coerce (points profile) 'list) (is-closed-polygon? profile)
                     (points path) (is-closed-polygon? path)
                     :twist twist :taper taper :from-end? from-end?))

(defun transform-extrude-uv-mesh (profile transform num-steps &key (v-wrap nil) (u-cap nil) (v-cap t))
  (let ((mesh (make-instance 'uv-mesh :u-dim (length (points profile))
				      :v-dim (if v-wrap num-steps (1+ num-steps))
				      :u-wrap (is-closed-polygon? profile)
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
				      :u-wrap (is-closed-polygon? profile)
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
  (sweep-extrude-uv-mesh (make-line-polygon (p! (/ x-size 2) 0 0) (p! (- (/ x-size 2)) 0 0) x-segments)
                         (make-line-polygon (p! 0 0 (- (/ z-size 2))) (p! 0 0 (/ z-size 2)) z-segments)))

(defun make-cylinder-uv-mesh (diameter height radial-segments height-segments &key (taper 1.0))
  (sweep-extrude-uv-mesh (make-circle-polygon diameter radial-segments)
                         (make-line-polygon (p! 0 0 0) (p! 0 height 0) height-segments)
                         :taper taper))

(defun make-cone-uv-mesh (diameter height radial-segments height-segments)
  (make-cylinder-uv-mesh diameter height radial-segments height-segments :taper 0.0))

(defun make-rect-prism-uv-mesh (base-side height base-segments height-segments &key (taper 1.0))
  (sweep-extrude-uv-mesh (make-square-polygon base-side base-segments)
                         (make-line-polygon (p! 0 0 0) (p! 0 height 0) height-segments)
                         :taper taper))

(defun make-pyramid-uv-mesh (base-side height base-segments height-segments)
  (make-rect-prism-uv-mesh base-side height base-segments height-segments :taper 0.0))

(defun make-torus-uv-mesh (inner-diameter outer-diameter inner-segments outer-segments)
  (sweep-extrude-uv-mesh (make-circle-polygon inner-diameter inner-segments)
                         (make-circle-polygon outer-diameter outer-segments)))

(defun make-sphere-uv-mesh (diameter latitude-segments longitude-segments)
  (let ((xform (make-instance 'transform :rotate (p! 0 (* 360 (/ (1- longitude-segments) longitude-segments)) 0))))
    (transform-extrude-uv-mesh (make-arc-polygon diameter latitude-segments 0 (- pi))
                               xform
                               longitude-segments
                               :v-wrap t
                               :v-cap nil)))

;;; version using function-extrude-uv-mesh
(defun make-sphere-uv-mesh-2 (diameter latitude-segments longitude-segments)
  (let ((xform (make-instance 'transform :rotate (p! 0 (* 360 (/ (1- longitude-segments) longitude-segments)) 0))))
    (function-extrude-uv-mesh (make-arc-polygon diameter latitude-segments 0 (- pi))
                              (lambda (points f) (transform-points points (transform-matrix xform f)))
                              longitude-segments
                              :v-wrap t
                              :v-cap nil)))

#| TODO -- need to tesselate top and bottom faces, compute normals after sphericize
-- may be simpler to implement a general refine-polyhedron method?
(defun make-cube-sphere-uv-mesh (diameter segments)
  (let* ((mesh (make-rect-prism-uv-mesh diameter diameter segments segments))
         (uv-array (uv-point-array mesh))
         (radius (/ diameter 2))
         (center (p! 0 radius 0)))
    (dotimes (u (u-dim mesh))
      (dotimes (v (v-dim mesh))
        (setf (aref uv-array u v) (p-sphericize (aref uv-array u v) radius 1.0 center))))
    (setf (points mesh) (flatten-array (uv-point-array mesh)))
    mesh))
;;; test
(add-shape *scene* (translate-to (make-cube-sphere-uv-mesh 1.0 8) (p! 0 0 6.0)))
|#


#|
(defun every-nth (step list)
   (remove-if
    (let ((iterator 0))
      (lambda (x)
        (declare (ignore x))
        (not (= 0 (mod (incf iterator) step)))))
    list))
|#

