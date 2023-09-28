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
                             (reduce #'p+ (mapcar #'(lambda (field)
                                                       (field-value field p0 ;TODO -- replace global
                                                                    (current-time *scene*)))
                                                   force-fields))
                             +origin+))
	 (internal-force (if (> (length (springs vertex)) 0)
                             (reduce #'p+
                                     (map 'vector (lambda (s) (spring-vertex-force s vertex))
                                          (springs vertex)))
                             +origin+))
	 (force (p+ external-force internal-force))       ;compute force
         (acc (p/ force (mass vertex)))	                  ;compute acceleration
         (vel (p+ (velocity vertex) acc))                 ;compute velocity
         (pos (p+ p0 (p:scale vel (time-step vertex))))) ;compute position
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
   (current-length 1.0)
   (current-force (p! 0 0 0))))

(defmethod init-spring ((spring flex-spring))
  (setf (rest-length spring) (p-dist (point (vertex1 spring)) (point (vertex2 spring)))))

(defmethod update-spring ((spring flex-spring))
  (compute-spring-length spring)
  (compute-spring-force spring))

(defmethod compute-spring-length ((spring flex-spring))
  (setf (current-length spring)
        (p-dist (point (vertex1 spring)) (point (vertex2 spring)))))

(defmethod compute-spring-force ((spring flex-spring))
  (setf (current-force spring)
        (let ((dir (p:normalize (p- (point (vertex2 spring)) (point (vertex1 spring))))))
          (p* dir (* (- (current-length spring) (rest-length spring)) (stiffness spring))))))

(defmethod spring-vertex-force ((spring flex-spring) (vertex flex-vertex))
  (if (eq vertex (vertex1 spring))
      (current-force spring)
      (p:negate (current-force spring))))

;;;; flex-animator =============================================================

(defclass-kons-9 flex-animator (shape-animator)
  ((vertices (make-array 0 :adjustable t :fill-pointer t))
   (springs (make-array 0 :adjustable t :fill-pointer t))
   ;; (show-pinned? t)
   ;; (show-springs? t)
   ;; (do-collisions? t)
   (force-fields '())))


(defmethod add-spring ((anim flex-animator) (vertex1 flex-vertex) (vertex2 flex-vertex))
  (let* ((len (p-dist (point vertex1) (point vertex2)))
	 (spring (make-instance 'flex-spring :vertex1 vertex1 :vertex2 vertex2 :rest-length len)))
    (vector-push-extend spring (springs anim))
    (vector-push-extend spring (springs vertex1))
    (vector-push-extend spring (springs vertex2))
    spring))

(defmethod create-flex-data ((anim flex-animator) (shape t))
  ;; do nothing
  )

(defmethod update-motion-after ((anim flex-animator) (shape t))
  ;; do nothing
  )

;;; flex from curve ------------------------------------------------------------

(defmethod make-flex-animator ((curve curve))
  (let ((anim (make-instance 'flex-animator :shape curve)))
    (create-flex-data anim curve)
    anim))

(defmethod create-flex-data ((anim flex-animator) (curve curve))
  (setf (vertices anim) (map 'vector #'(lambda (p) (make-instance 'flex-vertex :point p))
                             (points curve)))
  (let ((vertices (vertices anim)))
    (dotimes (i (1- (length vertices)))
      (add-spring anim (aref vertices i) (aref vertices (1+ i))))
    (when (is-closed-curve? curve)
      (add-spring anim (aref vertices (1- (length vertices))) (aref vertices 0)))))

;;; flex from polyhedron -------------------------------------------------------

(defmethod make-flex-animator ((polyh polyhedron))
  (let ((anim (make-instance 'flex-animator :shape polyh)))
    (create-flex-data anim polyh)
    anim))

(defmethod create-flex-data ((anim flex-animator) (polyh polyhedron))
  (setf (vertices anim) (map 'vector #'(lambda (p) (make-instance 'flex-vertex :point p))
                             (points polyh)))
  ;; for now connect all vertices with springs
  (do-array (i v1 (vertices anim))
    (do-array (j v2 (vertices anim))
      (when (> i j)
	(add-spring anim v1 v2)))))

(defmethod update-motion-after ((anim flex-animator) (shape polyhedron))
  (compute-face-normals shape)
  (compute-point-normals shape))
  
;;; flex from poly-strand ------------------------------------------------------

(defmethod make-flex-animator ((poly poly-strand))
  (let ((anim (make-instance 'flex-animator :shape poly)))
    (create-flex-data anim poly)
    anim))

(defmethod create-flex-data ((anim flex-animator) (poly poly-strand))
  (setf (vertices anim) (map 'vector #'(lambda (p) (make-instance 'flex-vertex :point p))
                             (points poly)))
  (do-array (i strand (strands poly))
    (let ((v1 (aref (vertices anim) (aref strand 0)))
          (v2 (aref (vertices anim) (aref strand 1))))
      (add-spring anim v1 v2))))

;;; update ---------------------------------------------------------------------

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
        (update-spring s))
      (do-array (i v vertices)
        (when (not (pinned? v))
          (compute-dynamics v force-fields)))
      ;; set points
      (do-array (i v vertices)
        (setf (aref points i) (point v)))
      ;; update shape if necessary
      (update-motion-after anim (shape anim))))
  anim)

;;; convenience setters

(defmethod set-flex-vertex-attr ((anim flex-animator) attr value)
  (do-array (i v (vertices anim))
    (setf (slot-value v attr) value)))

(defmethod set-flex-spring-attr ((anim flex-animator) attr value)
  (do-array (i s (springs anim))
    (setf (slot-value s attr) value)))
