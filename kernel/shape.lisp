(in-package #:kons-9)

;;;; point-generator-mixin ==============================================

(defclass point-generator-mixin ()
  ())

(defmethod point-generator-points ((gen point-generator-mixin))
  (error "Method POINT-GENERATOR-POINTS not implemented for object ~a" gen))

(defmethod point-generator-directions ((gen point-generator-mixin))
  (error "Method POINT-GENERATOR-DIRECTIONS not implemented for object ~a" gen))

(defmethod point-generator-radial-directions ((gen point-generator-mixin))
  (map 'array #'p-normalize (point-generator-points gen)))

(defmethod point-generator-closest-point ((gen point-generator-mixin) point)
  (let* ((points (point-generator-points gen))
         (min-dist (p-dist point (aref points 0)))
         (closest-index 0))
    (doarray (i p points)
      (let ((dist (p-dist point p)))
        (when (< dist min-dist)
          (setf min-dist dist)
          (setf closest-index i))))
    (aref points closest-index)))

;;;; curve-generator-mixin =====================================================

(defclass curve-generator-mixin ()
  ())

(defmethod curve-generator-curves ((gen curve-generator-mixin))
  (error "Method CURVE-GENERATOR-CURVES not implemented for object ~a" gen))

(defmethod curve-generator-curves-closed ((gen curve-generator-mixin))
  (error "Method CURVE-GENERATOR-CLOSED not implemented for object ~a" gen))

;;;; scene-item ================================================================

(defparameter *scene-item-counter* 0)

(defclass scene-item ()
  ((name :accessor name :initarg :name :initform nil)
   (scene :accessor scene :initarg :scene :initform nil)
   (is-selected? :accessor is-selected? :initarg :is-selected? :initform nil)))

(defmethod initialize-instance :after ((item scene-item) &rest initargs)
  (declare (ignore initargs))
  (when (null (name item))
    (setf (name item) (mashup-symbol (class-name (class-of item)) '- (incf *scene-item-counter*)))))

(defmethod copy-instance-data ((dst scene-item) (src scene-item))
  ;; TODO - name not copied - always generate new name?
  )

(defmethod string-name ((item scene-item))
  (format nil "~a" (if (name item) (name item) "- no name -")))

(defmethod select ((item scene-item))
  (setf (is-selected? item) t))
  ;; (when (scene item)
  ;;   (add-selection (scene item) item)))

(defmethod unselect ((item scene-item))
  (setf (is-selected? item) nil))
  ;; (when (scene item)
  ;;   (remove-selection (scene item) item)))

(defmethod toggle-select ((item scene-item))
  (if (is-selected? item)
      (unselect item)
      (select item)))

;;;; shape =====================================================================

(defclass shape (scene-item)
  ((transform :accessor transform :initarg :transform :initform (make-instance 'transform))
   (show-axis :accessor show-axis :initarg :show-axis :initform nil) ;nil or length
   (show-bounds? :accessor show-bounds? :initarg :show-bounds? :initform nil)))

(defmethod copy-instance-data :after ((dst shape) (src shape))
  ;; TODO - name not copied - always generate new name?
  (copy-instance-data (transform dst) (transform src))
  (setf (show-axis dst) (show-axis src))
  (setf (show-bounds? dst) (show-bounds? src)))

;;; utility methods for transforming shapes
(defmethod translate-by ((self shape) (p point))
  (translate-by (transform self) p)
  self)

(defmethod rotate-by ((self shape) (p point))
  (rotate-by (transform self) p)
  self)

(defmethod scale-by ((self shape) (p point))
  (scale-by (transform self) p)
  self)

(defmethod translate-to ((self shape) (p point))
  (translate-to (transform self) p)
  self)

(defmethod rotate-to ((self shape) (p point))
  (rotate-to (transform self) p)
  self)

(defmethod scale-to ((self shape) (p point))
  (scale-to (transform self) p)
  self)

(defmethod reset-transform ((self shape))
  (reset-transform (transform self))
  self)

(defmethod bounds-and-center ((self shape))
  (warn "Object ~a does not have BOUNDS-AND-CENTER defined. Using default values." self)
  (values nil nil nil))

(defmethod center-at-origin ((self shape))
  (multiple-value-bind (bounds-lo bounds-hi center)
      (bounds-and-center self)
    (declare (ignore bounds-lo bounds-hi))
    (when center
      (translate-to self (p-negate (p* center (scale (transform self))))))))

(defmethod scale-to-size ((self shape) max-size)
  (multiple-value-bind (bounds-lo bounds-hi center)
      (bounds-and-center self)
    (declare (ignore center))
    (when (and bounds-lo bounds-hi)
      (let* ((size (max (abs (- (x bounds-hi) (x bounds-lo)))
                        (abs (- (y bounds-hi) (y bounds-lo)))
                        (abs (- (z bounds-hi) (z bounds-lo)))))
             (scale (if (= size 0)
                        1.0
                        (/ max-size size))))
        (scale-to self (p! scale scale scale))))))

;;; push matrix and do transform operations before drawing
(defmethod draw :before ((self shape))
  (let ((xform (transform self)))
    (3d-push-matrix (translate xform) (rotate xform) (scale xform))))

;;; draw a marker
(defmethod draw ((self shape))
  (3d-draw-marker 0.1))

(defmethod draw-axis ((self shape))
  (3d-draw-axis (show-axis self)))

(defmethod draw-bounds ((self shape) &optional (color (c! 0 1 1)))
  (multiple-value-bind (lo hi center)
      (bounds-and-center self)
    (declare (ignore center))
    (3d-draw-bounds lo hi color)))

(defmethod draw-selected ((self shape))
  (draw-bounds self (c! 1 0 0)))

;;; draw axis and pop matrix after drawing
(defmethod draw :after ((self shape))
  (when (show-axis self)
    (draw-axis self))
  (if (is-selected? self)
      (draw-selected self)
      (when (show-bounds? self)
        (draw-bounds self)))
  (3d-pop-matrix))

;;; curve-shape class ====================================================

;;; this shape is defined by a list of points (vertices)
(defclass curve-shape (shape point-generator-mixin curve-generator-mixin)
  ((is-closed-shape? :accessor is-closed-shape? :initarg :is-closed-shape? :initform t)
   (points :accessor points :initarg :points :initform '())))

(defmethod copy-instance-data ((dst curve-shape) (src curve-shape))
  (error "COPY-INSTANCE-DATA not implemented for CURVE-SHAPE"))

(defmethod add-point ((self curve-shape) (p point))
  (push p (points self)))

(defmethod bounds-and-center ((self curve-shape))
  (when (= 0 (length (points self)))
    (return-from bounds-and-center (values nil nil nil)))
  (let* ((points (points self))
         (bounds-lo (p-copy (first points)))
         (bounds-hi (p-copy (first points))))
    (dolist (p points)
       (setf bounds-lo (p-min bounds-lo p))
       (setf bounds-hi (p-max bounds-hi p)))
    (values bounds-lo bounds-hi (p-average bounds-lo bounds-hi))))

(defmethod draw ((curve curve-shape))
  (when *display-wireframe?*
    (draw-wireframe curve))
  (when *display-points?*
    (draw-points curve)))

(defmethod draw-wireframe ((curve curve-shape))
  (3d-draw-curve (points curve) (is-closed-shape? curve)))

(defmethod draw-points ((curve curve-shape))
  (3d-draw-points (points curve)))

(defun curve-point-tangent (i points &optional (is-closed? nil))
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

(defun curve-tangents-aux (points &optional (is-closed? nil))
  (let ((tangents '()))
    (dotimes (i (length points))
      (push (curve-point-tangent i points is-closed?)
            tangents))
    (nreverse tangents)))

(defmethod curve-tangents ((curve curve-shape))
  (curve-tangents-aux (points curve) (is-closed-shape? curve)))

(defmethod point-generator-points ((shape curve-shape))
  (coerce (points shape) 'array))

(defmethod point-generator-directions ((shape curve-shape))
  (coerce (curve-tangents shape) 'array))

(defmethod curve-generator-curves ((shape curve-shape))
  (list (coerce (points shape) 'array)))

(defmethod curve-generator-curves-closed ((shape curve-shape))
  (list (is-closed-shape? shape)))

;;; randomize shape points
(defmethod randomize-points ((self curve-shape) (delta point))
  (setf (points self)
	(mapcar #'(lambda (p)
		    (let ((offset (p! (rand1 (x delta)) (rand1 (y delta)) (rand1 (z delta)))))
		      (p+ p offset)))
		(points self))))

;;;; point-cloud ========================================================

(defclass point-cloud (shape point-generator-mixin)
  ((points :accessor points :initarg :points :initform (make-array 0 :adjustable t :fill-pointer t))))

(defmethod copy-instance-data :after ((dst point-cloud) (src point-cloud))
  (setf (points dst) (points src))) ;;; TODO - deep copy arrays

(defmethod point-generator-points ((p-cloud point-cloud))
  (points p-cloud))

;;; arbitrarily return (1 1 1) for use as velocity multiplier
(defmethod point-generator-directions ((p-cloud point-cloud))
  (make-array (length (points p-cloud))
              :initial-element (p! 1 1 1)
              :adjustable t
              :fill-pointer t))

(defmethod draw ((p-cloud point-cloud))
  (when *display-points?*
    (draw-points p-cloud)))

(defmethod draw-points ((p-cloud point-cloud))
  (3d-draw-points-array (points p-cloud)))

(defmethod bounds-and-center ((p-cloud point-cloud))
  (when (= 0 (length (points p-cloud)))
    (return-from bounds-and-center (values nil nil nil)))
  (let* ((points (points p-cloud))
         (bounds-lo (p-copy (aref points 0)))
         (bounds-hi (p-copy (aref points 0))))
    (doarray (i p points)
       (setf bounds-lo (p-min bounds-lo p))
       (setf bounds-hi (p-max bounds-hi p)))
    (values bounds-lo bounds-hi (p-average bounds-lo bounds-hi))))

(defun make-point-cloud (&rest points)
  (make-instance 'point-cloud :points (make-array (length points)
                                                  :initial-contents points
                                                  :adjustable t
                                                  :fill-pointer t)))

(defun make-point-cloud-in-bounds (num bounds-lo bounds-hi)
  (let ((points '()))
    (dotimes (i num)
      (push (p-rand2 bounds-lo bounds-hi) points))
    (apply #'make-point-cloud points)))

;;;; polyhedron =========================================================

(defclass polyhedron (point-cloud curve-generator-mixin)
  ((faces :accessor faces :initarg :faces :initform (make-array 0 :adjustable t :fill-pointer t))
   (face-normals :accessor face-normals :initarg :face-normals :initform (make-array 0 :adjustable t :fill-pointer t))
   (point-normals :accessor point-normals :initarg :point-normals :initform (make-array 0 :adjustable t :fill-pointer t))
   (point-colors :accessor point-colors :initarg :point-colors :initform nil)
   (show-normals :accessor show-normals :initarg :show-normals :initform nil)  ; length or nil
   (point-generator-use-face-centers? :accessor point-generator-use-face-centers? :initarg :point-generator-use-face-centers? :initform nil)))

(defmethod copy-instance-data :after ((dst polyhedron) (src polyhedron))
  (setf (faces dst) (faces src)) ;;; TODO - deep copy arrays
  (setf (face-normals dst) (face-normals src))
  (setf (point-normals dst) (point-normals src))
  (setf (point-colors dst) (point-colors src))
  (setf (show-normals dst) (show-normals src)))

(defmethod duplicate-shape ((polyh polyhedron))
  (let ((new-shape (make-instance 'polyhedron)))
    (copy-instance-data new-shape polyh)
    new-shape))

(defmethod empty-polyhedron ((polyh polyhedron))
  (setf (points polyh) (make-array 0 :adjustable t :fill-pointer t))
  (setf (faces polyh) (make-array 0 :adjustable t :fill-pointer t))
  (setf (face-normals polyh) (make-array 0 :adjustable t :fill-pointer t))
  (setf (point-normals polyh) (make-array 0 :adjustable t :fill-pointer t))
  polyh)

(defmethod set-face-point-lists ((polyh polyhedron) point-lists)
  (empty-polyhedron polyh)
  (let ((i -1))
    (dolist (point-list point-lists)
      (let ((p-refs '()))
        (dolist (p point-list)
          (vector-push-extend p (points polyh))
          (push (incf i) p-refs))
        (vector-push-extend (nreverse p-refs) (faces polyh))))))
  
(defmethod polyhedron-bake ((polyh polyhedron))
  (let ((mtx (transform-matrix (transform polyh))))
    (dotimes (i (length (points polyh)))
      (setf (aref (points polyh) i)
            (transform-point (aref (points polyh) i) mtx))))
  (reset-transform (transform polyh))
  polyh)

(defmethod face-center ((polyh polyhedron) face)
  (p-center (face-points polyh face)))

(defmethod face-centers ((polyh polyhedron))
  (map 'array #'(lambda (f) (face-center polyh f)) (faces polyh)))

(defun triangle-normal (p0 p1 p2)
  (p-normalize (p-cross (p-from-to p0 p1) (p-from-to p1 p2))))

(defun quad-normal (p0 p1 p2 p3)
  (p-normalize (p-cross (p-from-to p0 p2) (p-from-to p1 p3))))

;; no checking, asssumes well-formed faces
(defmethod face-normal ((polyh polyhedron) face)
  (cond ((< (length face) 3)
         (p! 0 0 0))
        ((= (length face) 3)
         (let* ((p0 (aref (points polyh) (elt face 0)))
                (p1 (aref (points polyh) (elt face 1)))
                (p2 (aref (points polyh) (elt face 2))))
           (triangle-normal p0 p1 p2)))
        ((> (length face) 3)
         (let* ((p0 (aref (points polyh) (elt face 0)))
                (p1 (aref (points polyh) (elt face 1)))
                (p2 (aref (points polyh) (elt face 2)))
                (p3 (aref (points polyh) (elt face 3))))
           (quad-normal p0 p1 p2 p3)))))

(defmethod compute-face-normals ((polyh polyhedron))
  (setf (face-normals polyh)
        (map 'array #'(lambda (f) (face-normal polyh f)) (faces polyh))))

(defmethod compute-point-normals ((polyh polyhedron))
  (setf (point-normals polyh) (make-array (length (points polyh))
                                               :initial-element (p! 0 0 0)
                                               :adjustable t
                                               :fill-pointer t))
  (let ((p-normals (point-normals polyh)))
    (dotimes (f (length (faces polyh)))
      (dolist (pref (aref (faces polyh) f))
        (setf (aref p-normals pref)
              (p+ (aref p-normals pref)
                  (aref (face-normals polyh) f)))))
    (dotimes (n (length p-normals))
      (setf (aref p-normals n)
            (p-normalize (aref p-normals n))))))

(defmethod compute-point-normals-SAV ((polyh polyhedron))
  (setf (point-normals polyh) (make-array (length (points polyh))
                                               :initial-element (p! 0 0 0)
                                               :adjustable t
                                               :fill-pointer t))
  (dotimes (f (length (faces polyh)))
    (dolist (pref (aref (faces polyh) f))
      (setf (aref (point-normals polyh) pref)
            (p+ (aref (point-normals polyh) pref)
                (aref (face-normals polyh) f)))))
  (dotimes (n (length (point-normals polyh)))
    (setf (aref (point-normals polyh) n)
          (p-normalize (aref (point-normals polyh) n)))))

(defmethod face-points ((polyh polyhedron) i)
  (mapcar #'(lambda (pref) (aref (points polyh) pref))
          (aref (faces polyh) i)))

(defmethod face-points ((polyh polyhedron) (face list))
  (mapcar #'(lambda (pref) (aref (points polyh) pref))
          face))

(defmethod reverse-face-normals ((polyh polyhedron))
  (dotimes (i (length (face-normals polyh)))
    (setf (aref (face-normals polyh) i) (p-negate (aref (face-normals polyh) i))))
  polyh)

(defmethod allocate-point-colors ((polyh polyhedron))
  (setf (point-colors polyh) (make-array (length (points polyh))
                                              :initial-element *shading-color*)))
  
(defmethod reset-point-colors ((polyh polyhedron))
  (allocate-point-colors polyh)
  polyh)

(defmethod set-point-colors-by-xyz ((polyh polyhedron) color-fn)
  (allocate-point-colors polyh)
  (doarray (i p (points polyh))
    (setf (aref (point-colors polyh) i) (funcall color-fn p))))

(defmethod set-point-colors-by-xyz ((group group) color-fn)
  (dolist (child (children group))
    (set-point-colors-by-xyz child color-fn)))

(defmethod set-point-colors-by-point-and-normal ((polyh polyhedron) color-fn)
  (allocate-point-colors polyh)
  (doarray (i p (points polyh))
    (let ((n (aref (point-normals polyh) i)))
      (setf (aref (point-colors polyh) i) (funcall color-fn p n)))))

(defmethod set-point-colors-by-point-and-normal ((group group) color-fn)
  (dolist (child (children group))
    (set-point-colors-by-point-and-normal child color-fn)))

(defmethod point-generator-points ((polyh polyhedron))
  (if (point-generator-use-face-centers? polyh)
      (face-centers polyh)
      (call-next-method)))

(defmethod point-generator-directions ((polyh polyhedron))
  (if (point-generator-use-face-centers? polyh)
      (face-normals polyh)
      (point-normals polyh)))

(defmethod curve-generator-curves ((polyh polyhedron))
  (let ((curves '()))
    (dotimes (f (length (faces polyh)))
      (push (face-points polyh f) curves))
    (nreverse curves)))

(defmethod curve-generator-curves-closed ((polyh polyhedron))
  (make-list (length (faces polyh)) :initial-element t)) ;always closed

(defun make-polyhedron (points faces &optional (mesh-type 'polyhedron))
  (let ((polyh (make-instance mesh-type :points (make-array (length points)
                                                            :initial-contents points
                                                            :adjustable t
                                                            :fill-pointer t)
                                             :faces (make-array (length faces)
                                                                :initial-contents faces
                                                                :adjustable t
                                                                :fill-pointer t))))
    (compute-face-normals polyh)
    (compute-point-normals polyh)
    polyh))

(defmethod draw-normals ((polyh polyhedron))
  (let ((lines ()))
    (dotimes (f (length (faces polyh)))
      (let* ((points (face-points polyh f))
             (p0 (p-center points))
             (p1 (p+ p0 (p-scale (aref (face-normals polyh) f) (show-normals polyh)))))
        (push p1 lines)
        (push p0 lines)))
    (3d-draw-lines lines)))

(defmethod draw ((polyh polyhedron))
  (when (or (= 0 (length (points polyh)))
          (= 0 (length (faces polyh))))
      (return-from draw))

  (3d-setup-lighting)

  (when *display-filled?*
    (3d-draw-filled-polygons (points polyh) (faces polyh)
                             (face-normals polyh) (point-normals polyh) (point-colors polyh)))

  (when *display-wireframe?*
    (3d-draw-wireframe-polygons (points polyh) (faces polyh)))

  (when *display-points?*
    (draw-points polyh))

  (when (show-normals polyh)
    (draw-normals polyh)))

(defmethod refine-face ((polyh polyhedron) face)
  (let* ((point-lists '())
         (points (face-points polyh face))
         (center (p-center points))
        (face-points (list->array points))
        (n (length points)))
    (dotimes (i n)
      (push (list (aref face-points i)
                  (p-average (aref face-points i) (aref face-points (mod (1+ i) n)))
                  center
                  (p-average (aref face-points i) (aref face-points (mod (1- i) n))))
            point-lists))
    point-lists))
                
(defmethod refine-mesh ((polyh polyhedron) &optional (levels 1))
  (if (<= levels 0)
      polyh
      (let ((points '())
            (faces '()))
        (dotimes (i (length (faces polyh)))
          (let ((pref (length points))               ;starting point index
                (point-lists (refine-face polyh i))) ;list of point-lists
            (dolist (point-list point-lists)
              (let ((face '()))
                (dolist (point point-list)
                  (push point points)
                  (push pref face)
                  (incf pref))
                (push face faces)))))
        (refine-mesh (make-polyhedron points faces) (1- levels)))))

(defun triangle-area (p0 p1 p2)
  (let ((e1 (p-from-to p0 p1))
        (e2 (p-from-to p1 p2)))
    (/ (* (p-mag e1) (p-mag e2) (p-angle-sine e1 e2)) 2)))

;;; only works for triangles
(defmethod face-area ((polyh polyhedron) face)
  (cond ((< (length face) 3)
         0.0)
        ((= (length face) 3)
         (let* ((p0 (aref (points polyh) (elt face 0)))
                (p1 (aref (points polyh) (elt face 1)))
                (p2 (aref (points polyh) (elt face 2))))
           (triangle-area p0 p1 p2)))
        (t
         (error "POLYHEDRON ~a FACE ~a IS NOT A TRIANGLE" polyh face))))

(defun barycentric-point (p0 p1 p2 a b)
  (p+ p0
      (p+ (p-scale (p-from-to p0 p1) a)
          (p-scale (p-from-to p0 p2) b))))

(defun generate-face-barycentric-points (p0 p1 p2 num)
  (let ((barycentric-points '()))
    (dotimes (i (round num))
      (let ((a (rand2 0.0 1.0))
            (b (rand2 0.0 1.0)))
        (do () ((<= (+ a b) 1.0))
          (setf a (rand2 0.0 1.0))
          (setf b (rand2 0.0 1.0)))
        (push (barycentric-point p0 p1 p2 a b)
              barycentric-points)))
    barycentric-points))

(defmethod generate-point-cloud ((polyh polyhedron) &optional (density 1.0))
    (when (not (is-triangulated-polyhedron? polyh))
      (error "POLYHEDRON ~a IS NOT TRIANGULATED" polyh))
  (let ((points '()))
    (dotimes (f (length (faces polyh)))
      (let* ((area (face-area polyh (aref (faces polyh) f)))
             (face-points (face-points polyh f))
             (p0 (elt face-points 0))
             (p1 (elt face-points 1))
             (p2 (elt face-points 2))
             (barycentric-points (generate-face-barycentric-points p0 p1 p2 (* area density))))
        (dolist (p barycentric-points)
                                        ;          (vector-push-extend p points))))
          (push p points))))
    (apply #'make-point-cloud points)))

(defun face-triangle-refs (prefs)
  (cond ((< (length prefs) 3)
         '())
        ((= (length prefs) 3)
         (list prefs))
        (t
         (let ((p0 (car prefs)))
           (loop for p1 in (cdr prefs)
                 for p2 in (cddr prefs)
                 collect (list p0 p1 p2))))))
      
(defmethod triangulate-polyhedron ((polyh polyhedron))
  (let ((tri-faces '()))
    (dotimes (f (length (faces polyh)))
      (dolist (tri (face-triangle-refs (aref (faces polyh) f)))
        (push tri tri-faces)))
    (make-polyhedron (points polyh) (coerce tri-faces 'array))))

(defmethod is-triangulated-polyhedron? ((polyh polyhedron))
  (dotimes (f (length (faces polyh)))
    (when (not (<= (length (aref (faces polyh) f)) 3))
      (return-from is-triangulated-polyhedron? nil)))
  t)

(defun make-cube (side &optional (mesh-type 'polyhedron))
  (let ((r (* side 0.5))
        (-r (* side -0.5)))
    (make-polyhedron (vector (p! -r -r -r)
                             (p!  r -r -r)
                             (p!  r -r  r)
                             (p! -r -r  r)
                             (p! -r  r -r)
                             (p!  r  r -r)
                             (p!  r  r  r)
                             (p! -r  r  r))
                     (vector '(0 1 2 3) '(0 4 5 1) '(1 5 6 2)
                             '(2 6 7 3) '(3 7 4 0) '(4 7 6 5))
                     mesh-type)))

(defun make-cut-cube-polyhedron (side)
  (let ((r (* side 0.5))
        (-r (* side -0.5))
        (b (* side 0.3)))
    (make-polyhedron (vector (p! -r -r -r)
                             (p!  r -r -r)
                             (p!  r -r  r)
                             (p! -r -r  r)
                             (p! -r  r -r)
                             (p!  r  r -r)
                             (p!  r  r  b)
                             (p!  b  r  r)
                             (p! -r  r  r)
                             (p!  r  b  r))
                     (vector '(1 2 3 0)
                             '(5 6 9 2 1)
                             '(9 7 8 3 2)
                             '(0 4 5 1)
                             '(8 4 0 3)
                             '(8 7 6 5 4)
                             '(6 7 9)))))

(defun make-octahedron (radius)
  (let* ((r (abs radius))
         (-r (- r)))
    (make-polyhedron (vector (p!  r  0  0) 
                             (p! -r  0  0)
                             (p!  0  r  0)
                             (p!  0 -r  0)
                             (p!  0  0  r) 
                             (p!  0  0 -r))
                     (vector '(0 2 4) '(2 0 5) '(3 0 4) '(0 3 5)
                             '(2 1 4) '(1 2 5) '(1 3 4) '(3 1 5)))))

(defun make-icosahedron (radius)
  (let* ((p1 (/ (abs radius) 1.902076))
         (p2 (* p1 1.618034))
         (-p1 (- p1))
         (-p2 (- p2)))
    (make-polyhedron (vector (p!  p2  p1   0)
                             (p! -p2  p1   0)
                             (p!  p2 -p1   0)
                             (p! -p2 -p1   0)
                             (p!  p1   0  p2)
                             (p!  p1   0 -p2)
                             (p! -p1   0  p2)
                             (p! -p1   0 -p2)
                             (p!   0  p2  p1)
                             (p!   0 -p2  p1)
                             (p!   0  p2 -p1)
                             (p!   0 -p2 -p1))
                     (vector '(0 8 4) '(0 5 10) '(2 4 9) '(2 11 5) '(1 6 8) '(1 10 7)
                             '(3 9 6) '(3 7 11) '(0 10 8) '(1 8 10) '(2 9 11)
                             '(3 11 9) '(4 2 0) '(5 0 2) '(6 1 3) '(7 3 1) '(8 6 4)
                             '(9 4 6) '(10 5 7) '(11 7 5)))))

