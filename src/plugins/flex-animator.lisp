(in-package #:kons-9)
      
;;;; flex-vertex ===============================================================

(defclass-kons-9 flex-vertex ()
  ((point nil)
   (springs (make-array 0 :adjustable t :fill-pointer t))
   (pinned? nil)
   (velocity (p! 0 0 0))
   (mass 1.0)
   (elasticity 0.75)
   (friction 0.75)
   (damping 0.95)
   (time-step 0.5)
   (do-collisions? t)
   (collision-padding 0.0)))

;; compute force on vertex
(defmethod compute-dynamics ((vertex flex-vertex) force-fields)
  (let* ((p0 (point vertex))
         (external-force (if force-fields
                             (reduce #'p:+ (mapcar #'(lambda (field)
                                                       (field-value field p0 ;TODO -- replace global
                                                                    (current-time *scene*)))
                                                   force-fields))
                             +origin+))
	 (internal-force (reduce #'p+
				 (map 'vector #'(lambda (s) (spring-force s vertex))
                                      (springs vertex))))
	 (force (p+ external-force internal-force))       ;compute force
         (acc (p/ force (mass vertex)))	                  ;compute acceleration
         (vel (p+ (velocity vertex) acc))                 ;compute velocity
         (pos (p:+ p0 (p:scale vel (time-step vertex))))) ;compute position
      (when (do-collisions? vertex)	; handle collision
	(let ((elast (elasticity vertex))
              (friction (friction vertex))
	      (lo (collision-padding vertex)))
	  (when (< (p:y pos) lo)
	    (setf (p:y pos) (+ lo (abs (- lo (p:y pos))))
	          (p:x vel) (* friction (p:x vel))
                  (p:y vel) (* elast (- (p:y vel)))
                  (p:z vel) (* friction (p:z vel))))))
    ;; update state 
    (setf (velocity vertex) (p* vel (damping vertex)))
    (setf (point vertex) pos)))

;;;; flex-spring ===============================================================

(defclass-kons-9 flex-spring ()
  ((vertex1 nil)
   (vertex2 nil)
   (stiffness 1.0)
   (rest-length 1.0)
   (current-length 1.0)))

(defmethod init-spring ((spring flex-spring))
  (setf (rest-length spring) (p-dist (point (vertex1 spring)) (point (vertex2 spring)))))

(defmethod compute-spring-length ((spring flex-spring))
  (setf (current-length spring)
        (p-dist (point (vertex1 spring)) (point (vertex2 spring)))))

(defmethod spring-force ((spring flex-spring) (vertex flex-vertex))
  (let ((dir (p:normalize (if (eq vertex (vertex1 spring))
                              (p- (point (vertex2 spring)) (point (vertex1 spring)))
                              (p- (point (vertex1 spring)) (point (vertex2 spring)))))))
    (p* dir (* (- (current-length spring) (rest-length spring)) (stiffness spring)))))

;;;; flex-animator =============================================================

(defclass-kons-9 flex-animator (shape-animator)
  ((vertices (make-array 0 :adjustable t :fill-pointer t))
   (springs (make-array 0 :adjustable t :fill-pointer t))
   (show-pinned? t)
   (show-springs? t)
   (do-collisions? t)
   (force-fields '())))


(defmethod add-spring ((anim flex-animator) (vertex1 flex-vertex) (vertex2 flex-vertex))
  (let* ((len (p-dist (point vertex1) (point vertex2)))
	 (spring (make-instance 'flex-spring :vertex1 vertex1 :vertex2 vertex2 :rest-length len)))
    (vector-push-extend spring (springs anim))
    (vector-push-extend spring (springs vertex1))
    (vector-push-extend spring (springs vertex2))
    spring))

;;; for now connect all vertices with springs
(defmethod create-flex-data ((anim flex-animator))
  (setf (vertices anim) (map 'vector #'(lambda (p) (make-instance 'flex-vertex :point p))
                             (points (shape anim))))
  (do-array (i v1 (vertices anim))
    (do-array (j v2 (vertices anim))
      (when (> i j)
	(add-spring anim v1 v2)))))

(defmethod make-flex-animator ((polyh polyhedron))
  (let ((anim (make-instance 'flex-animator :shape polyh)))
    (create-flex-data anim)
    anim))

(defmethod update-motion ((anim flex-animator) parent-absolute-timing)
  (declare (ignore parent-absolute-timing))
  (if (null (shape anim))
    (error "FLEX-ANIMATOR ~a HAS NO SHAPE.~%" anim)
    (let ((points (points (shape anim)))
          (vertices (vertices anim))
          (springs (springs anim))
          (force-fields (force-fields anim)))
      ;; get points
      (do-array (i v vertices)
        (setf (point v) (aref points i)))
      ;; compute dynamics
      (do-array (i s springs)
        (compute-spring-length s))
      (do-array (i v vertices)
        (when (not (pinned? v))
          (compute-dynamics v force-fields)))
      ;; set points
      (do-array (i v vertices)
        (setf (aref points i) (point v)))
      ;; update polyhedron
      (compute-face-normals (shape anim))
      (compute-point-normals (shape anim))))
  anim)

;;; some convenience setters

(defmethod set-spring-stiffness ((anim flex-animator) value)
  (do-array (i s (springs anim))
    (setf (stiffness s) value)))

(defmethod set-vertex-friction ((anim flex-animator) value)
  (do-array (i v (vertices anim))
    (setf (friction v) value)))

#|
(defmethod draw-animator ((anim flex-animator))
  (when (show-springs? anim)
    (#_glLineWidth 1.0)
    (#_glColor3f 1.0 0.0 0.0)
    (#_glBegin #$GL_LINES)
    (dolist (spring (springs anim))
      (let ((p1 (point (vertex1 spring)))
	    (p2 (point (vertex2 spring))))
	(#_glVertex3f (x p1) (y p1) (z p1))
	(#_glVertex3f (x p2) (y p2) (z p2))))
    (#_glEnd))
  (when (show-pinned? anim)
    (#_glPointSize 15.0)
    (#_glColor3f 0.0 1.0 1.0)
    (#_glBegin #$GL_POINTS)
    (dolist (vertex (vertices anim))
      (when (pinned? vertex)
      (let ((p (point vertex)))
	(#_glVertex3f (x p) (y p) (z p)))))
    (#_glEnd)))
|#
#|

;;; square

(defparameter *group*
  (with-clear-and-redraw
    (add-shape *scene* (make-flex (make-group (make-square-shape 1.0))))))

(setf *time-step* 0.0005)
(setf *flex-damping* 0.9)
(set-flex-vertex-attr *group* 'elasticity 0.8)

(set-flex-spring-attr *group* 'stiffness 100.0)

(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(0)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? nil '(0)))

;;; hexagon

(defparameter *group*
  (with-clear-and-redraw
    (add-shape *scene* (make-flex (make-group (make-circle-shape 1.0 6))))))

(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(5)))

(with-redraw (set-flex-vertex-attr *group* 'pinned? nil '(5)))

(setf *wind* (p! 5 0 0))
(setf *wind* (p! -10 0 0))
(setf *wind* (p! 0 0 0))

(set-flex-spring-attr *group* 'stiffness 100.0)

;;; circle

(setf *time-step* 0.0002)

(defparameter *group*
  (with-clear-and-redraw
    (add-shape *scene* (make-flex (make-group (make-circle-shape 1.0 16))))))
(set-flex-vertex-attr *group* 'elasticity 0.3)

(set-flex-spring-attr *group* 'stiffness 100.0)

(setf *wind* (p! 5 0 0))
(setf *wind* (p! -10 0 0))
(setf *wind* (p! 0 0 0))

(setf *gravity* (p! 0 -9.81 0))

>>> implement friction
(set-flex-vertex-attr *group* 'friction 0.0)


(set-flex-spring-attr *group* 'stiffness 500.0) ; numerical insability

(setf *time-step* 0.00005)		; reduce simulation timestep


;;; mesh -- cross-brace-1

(setf *time-step* 0.0002)
(setf *flex-damping* 0.9)

(defparameter *group*
  (with-clear-and-redraw
    (add-shape *scene* (make-group (make-mesh-anim-shape 7 7 (p! -.5 -.5 0) (p! .5 .5 0)
							 :cross-brace-1? t
							 :cross-brace-2? nil)))))

(set-flex-spring-attr *group* 'stiffness 200.0)
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(0 42)))

(setf *wind* (p! 5 0 0))
(setf *wind* (p! -10 0 0))
(setf *wind* (p! 0 0 0))

(with-redraw (set-flex-vertex-attr *group* 'pinned? nil '(42)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? nil '(0)))

(with-redraw (set-flex-vertex-attr *group* 'point (p! .8 .8 0) '(0)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(0)))

(with-redraw (set-flex-vertex-attr *group* 'point (p! -.8 .8 0) '(42)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(42)))

(set-flex-spring-attr *group* 'stiffness 500.0) ; induce numerical instability
(setf *time-step* 0.0001)

;;; mesh -- cross-brace-2

(setf *time-step* 0.0002)
(setf *flex-damping* 0.9)

(defparameter *group*
  (with-clear-and-redraw
    (add-shape *scene* (make-group (make-mesh-anim-shape 7 7 (p! -.5 -.5 0) (p! .5 .5 0)
							 :cross-brace-1? t
							 :cross-brace-2? t)))))
(set-flex-spring-attr *group* 'stiffness 200.0)
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(0 42)))

(setf *wind* (p! 5 0 0))
(setf *wind* (p! -10 0 0))
(setf *wind* (p! 0 0 0))

(with-redraw (set-flex-vertex-attr *group* 'point (p! .8 .8 0) '(0)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(0)))

(with-redraw (set-flex-vertex-attr *group* 'point (p! -.8 .8 0) '(42)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(42)))
       
(with-redraw (set-flex-vertex-attr *group* 'point (p! 0 .8 0) '(21)))
(with-redraw (set-flex-vertex-attr *group* 'pinned? t '(21)))

|#
