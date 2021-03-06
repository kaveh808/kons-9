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

(defun grid-point-array (u-dim v-dim &optional (bounds-lo (p! -1 0 -1)) (bounds-hi (p! 1 0 1)) (jitter nil))
  (let ((uv-point-array (make-array (list u-dim v-dim))))
    (dotimes (u u-dim)
      (let* ((fx (/ u (- u-dim 1.0)))
	     (x (lerp fx (x bounds-lo) (x bounds-hi))))
	(dotimes (v v-dim)
	  (let* ((fz (/ v (- v-dim 1.0)))
		 (z (lerp fz (z bounds-lo) (z bounds-hi))))
	    (setf (aref uv-point-array u v)
		  (if jitter
		      (p-jitter (p! z 0.0 x) jitter)
		      (p! z 0.0 x)))))))
    uv-point-array))

(defmethod compute-polyhedron-data ((mesh uv-mesh))
  (compute-polyhedron-mesh mesh)
  (compute-face-normals mesh)
  (compute-point-normals mesh)
  (allocate-point-colors mesh)
  mesh)

(defun 2d-array-to-list (array)
  (let ((new-list '()))
    (loop for i below (array-dimension array 0)
          do (loop for j below (array-dimension array 1)
                   do (push (aref array i j) new-list)))
    (nreverse new-list)))

(defmethod compute-polyhedron-mesh ((mesh uv-mesh))
  (setf (points mesh) (make-array (* (u-dim mesh) (v-dim mesh))
                                  :initial-contents (2d-array-to-list (uv-point-array mesh))
                                  :adjustable t
                                  :fill-pointer t))
  (setf (faces mesh) (make-array (+ (* (1- (wrapped-u-dim mesh)) (1- (wrapped-v-dim mesh)))
                                    (if (u-cap mesh) 2 0)
                                    (if (v-cap mesh) 2 0))
                                  :initial-contents (compute-face-list mesh)
                                  :adjustable t
                                  :fill-pointer t)))

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

(defun make-grid-uv-mesh (u-dim v-dim bounds-lo bounds-hi &optional (jitter nil))
  (let ((mesh (make-instance 'uv-mesh :u-dim u-dim :v-dim v-dim)))
    (allocate-mesh-arrays mesh)
    (setf (uv-point-array mesh) (grid-point-array u-dim v-dim bounds-lo bounds-hi jitter))
    (compute-polyhedron-data mesh)
    mesh))

(defun transform-extrude (profile transform steps)
  (let ((mesh (make-instance 'uv-mesh :u-dim (length (points profile))
				      :v-dim (+ steps 1)
				      :u-wrap (is-closed-shape? profile)
				      :v-wrap nil
                                      :v-cap t)))
    (allocate-mesh-arrays mesh)
    (dotimes (v (v-dim mesh))
      (let* ((factor (tween v 0.0 (- (v-dim mesh) 1)))
	     (mtx (transform-matrix transform factor))
	     (points (transform-points (points profile) mtx)))
	(loop :for p :in points
	      :for u :from 0
	      :do (setf (aref (uv-point-array mesh) u v) p))))
    (compute-polyhedron-data mesh)))

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
    (when (or (< (length profile-points) 3)
              (< (length unique-path-points) 2))
      (return-from sweep-extrude-aux (make-instance 'uv-mesh))) ;return empty mesh
    (setf (u-dim mesh) (length profile-points))
    (setf (v-dim mesh) (length unique-path-points))
    (setf (u-wrap mesh) is-closed-profile?)
    (setf (v-wrap mesh)is-closed-path?)
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

;;; for now sweep 0-th profile along all paths
(defmethod sweep-extrude ((profiles curve-generator-mixin) (paths curve-generator-mixin) &key (twist 0.0) (taper 1.0) (from-end? nil))
  (let ((meshes '())
        (profile-curve (coerce (elt (curve-generator-curves profiles) 0) 'list)) ;fix this...
        (profile-closed (elt (curve-generator-curves-closed profiles) 0))
        (path-curves (curve-generator-curves paths))
        (path-closed (curve-generator-curves-closed paths)))
    (loop for curve in path-curves
          for is-closed? in path-closed
          do (push (sweep-extrude-aux (make-instance 'uv-mesh)
                                      profile-curve profile-closed
                                      curve is-closed?
                                      :twist twist :taper taper :from-end? from-end?)
                   meshes))
    (reverse meshes)))

;;; make-torus
(defun make-torus (inner-radius inner-segments outer-radius outer-segments)
  (first (sweep-extrude (make-circle-shape inner-radius inner-segments)
                        (make-circle-shape outer-radius outer-segments))))

#|
(defun every-nth (step list)
   (remove-if
    (let ((iterator 0))
      (lambda (x)
        (declare (ignore x))
        (not (= 0 (mod (incf iterator) step)))))
    list))
|#

;;;; height-field =======================================================

(defclass height-field (uv-mesh)
  ((height-fn :accessor height-fn :initarg :height-fn :initform nil)))

(defmethod compute-heights ((height-field height-field))
  (dotimes (u (u-dim height-field))
    (dotimes (v (v-dim height-field))
      (let ((p (aref (uv-point-array height-field) u v)))
        (setf (y p) (funcall (height-fn height-field) (x p) (z p)))))))
  
(defmethod make-height-field (u-dim v-dim bounds-lo bounds-hi &optional (height-fn nil))
  (let ((height-field (make-instance 'height-field :u-dim u-dim
                                                   :v-dim v-dim
                                                   :u-wrap nil
                                                   :v-wrap nil
                                                   :height-fn height-fn)))
    (allocate-mesh-arrays height-field)
    (setf (uv-point-array height-field) (grid-point-array u-dim v-dim bounds-lo bounds-hi))
    (when height-fn
      (compute-heights height-field))
    (compute-polyhedron-data height-field)
    height-field))

;;;; superquadric =======================================================

(defclass superquadric (uv-mesh procedural-mixin)
  ((radius :accessor radius :initarg :radius :initform 1.0)
   (e1 :accessor e1 :initarg :e1 :initform 0.2)
   (e2 :accessor e2 :initarg :e2 :initform 0.2))
  (:default-initargs
   :u-wrap t))

(def-procedural-input superquadric u-dim)
(def-procedural-input superquadric v-dim)
(def-procedural-input superquadric radius)
(def-procedural-input superquadric e1)
(def-procedural-input superquadric e2)
(def-procedural-output superquadric points)
(def-procedural-output superquadric faces)

(defmethod compute-procedural-node ((mesh superquadric))
  (allocate-mesh-arrays mesh)
  (setf (uv-point-array mesh) (grid-point-array (u-dim mesh) (v-dim mesh)))
  (compute-superquadric-mesh mesh)
  (compute-polyhedron-data mesh)
  mesh)
  
(defmethod compute-superquadric-mesh ((mesh superquadric))
  (with-accessors ((u-dim u-dim) (v-dim v-dim) (r radius) (e1 e1) (e2 e2))
      mesh
    (let* ((u-pi (- (/ 2pi u-dim)))     ;negative so backface cull is correct
           (v-pi (/ pi (1- v-dim))))
      (dotimes (i u-dim)
        (let* ((u (* i u-pi))
               (cu1 (cos u))
               (su1 (sin u))
               (cu (* (expt (abs cu1) e1) (if (< cu1 0) -1 1)))
               (su (* (expt (abs su1) e1) (if (< su1 0) -1 1))))
          (dotimes (j v-dim)
            (let* ((v (- (* j v-pi) pi/2))
                   (cv1 (cos v))
                   (sv1 (sin v))
                   (cv (* (expt (abs cv1) e2) (if (< cv1 0) -1 1)))
                   (sv (* (expt (abs sv1) e2) (if (< sv1 0) -1 1))))
              (setf (aref (uv-point-array mesh) i j)
                    (if (or (= j 0) (= j (1- v-dim))) ;make sure first and last profile points are on y axis
                        (p! 0
                            (* sv r)
                            0)
                        (p! (* cv cu r)
                            (* sv r)
                            (* cv su r)))))))))))

(defmethod make-superquadric (u-dim v-dim radius e1 e2)
  (make-instance 'superquadric :u-dim u-dim
                               :v-dim v-dim
                               :u-wrap t
                               :v-wrap nil
                               :radius radius
                               :e1 e1
                               :e2 e2))
 
;;;; sweep-mesh ====================================================

(defclass sweep-mesh (uv-mesh dependency-node-mixin)
  ((profile-curve-generator :accessor profile-curve-generator :initarg :profile-curve-generator :initform nil)
   (profile-curve-index :accessor profile-curve-index :initarg :profile-curve-index :initform nil)
   (path-curve-generator :accessor path-curve-generator :initarg :path-curve-generator :initform nil)
   (path-curve-index :accessor path-curve-index :initarg :path-curve-index :initform nil)
   (twist :accessor twist :initarg :twist :initform 0.0)
   (taper :accessor taper :initarg :taper :initform 1.0)
   (from-end? :accessor from-end? :initarg :from-end? :initform nil)))

(defmethod profile-points ((mesh sweep-mesh))
  (coerce (elt (curve-generator-curves (profile-curve-generator mesh)) (profile-curve-index mesh))
          'list))

(defmethod is-profile-closed? ((mesh sweep-mesh))
  (elt (curve-generator-curves-closed (profile-curve-generator mesh)) (profile-curve-index mesh)))

(defmethod path-points ((mesh sweep-mesh))
  (coerce (elt (curve-generator-curves (path-curve-generator mesh)) (path-curve-index mesh))
          'list))

(defmethod is-path-closed? ((mesh sweep-mesh))
  (elt (curve-generator-curves-closed (path-curve-generator mesh)) (path-curve-index mesh)))

(defmethod initialize-instance :after ((mesh sweep-mesh) &rest initargs)
  (declare (ignore initargs))
  (push 'profile-curve-generator (input-slots mesh))
  (push 'path-curve-generator (input-slots mesh)))

(def-procedural-input sweep-mesh profile-curve-generator)
(def-procedural-input sweep-mesh profile-curve-index)
(def-procedural-input sweep-mesh path-curve-generator)
(def-procedural-input sweep-mesh path-curve-index)
(def-procedural-input sweep-mesh twist)
(def-procedural-input sweep-mesh taper)
(def-procedural-input sweep-mesh from-end?)

(def-procedural-output sweep-mesh uv-point-array)
(def-procedural-output sweep-mesh points)
(def-procedural-output sweep-mesh faces)

;;; assumes profile curve has z-axis as normal
(defmethod compute-procedural-node ((mesh sweep-mesh))
  (sweep-extrude-aux mesh
                     (profile-points mesh) (is-profile-closed? mesh)
                     (path-points mesh) (is-path-closed? mesh)
                     :twist (twist mesh) :taper (taper mesh) :from-end? (from-end? mesh)))

(defun make-sweep-mesh (profile-curve-generator profile-curve-index path-curve-generator path-curve-index
                        &rest initargs)
  (apply #'make-instance 'sweep-mesh :profile-curve-generator profile-curve-generator
                                       :profile-curve-index profile-curve-index
                                       :path-curve-generator path-curve-generator
                                       :path-curve-index path-curve-index
                                       initargs))

;;;; sweep-mesh-group ===========================================

(defclass sweep-mesh-group (manager-group)
  ((profile-curve-generator :accessor profile-curve-generator :initarg :profile-curve-generator :initform nil)
   (path-curve-generator :accessor path-curve-generator :initarg :path-curve-generator :initform nil)
   (twist :accessor twist :initarg :twist :initform 0.0)
   (taper :accessor taper :initarg :taper :initform 1.0)
   (from-end? :accessor from-end? :initarg :from-end? :initform nil)))

(defmethod initialize-instance :after ((group sweep-mesh-group) &rest initargs)
  (declare (ignore initargs))
  (push 'profile-curve-generator (input-slots group))
  (push 'path-curve-generator (input-slots group)))

;;; for now sweep 0-th profile along all path curves
(defmethod compute-procedural-node ((group sweep-mesh-group))
  (set-children group (sweep-extrude (profile-curve-generator group)
                                     (path-curve-generator group)
                                     :twist (twist group)
                                     :taper (taper group)
                                     :from-end? (from-end? group))))

(defun make-sweep-mesh-group (profile-curve-generator path-curve-generator &rest initargs)
  (apply #'make-instance 'sweep-mesh-group :profile-curve-generator profile-curve-generator
                                           :path-curve-generator path-curve-generator
                                           initargs))
