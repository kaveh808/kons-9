(in-package #:kons-9)

;;;; boid-animator ======================================================

(defclass boid-animator (shape-animator)
  ((boid-system :accessor boid-system :initarg :boid-system :initform nil)
   (expired? :accessor expired? :initarg :expired? :initform nil)
   (shape-fn :accessor shape-fn :initarg :shape-fn :initform nil
	     :allocation :class)))

(defmethod update-boid ((boid boid-animator))
  ;; subclass responsibility
  )

(defmethod boid-location ((boid boid-animator))
  (offset (translate (transform (shape boid)))))

(defmethod boid-dist ((boid1 boid-animator) (boid2 boid-animator))
  (p-dist (boid-location boid1) (boid-location boid2)))

(defmethod boid-heading ((boid1 boid-animator) (boid2 boid-animator))
  (let* ((delta-xz (p-from-to (boid-location boid1) (boid-location boid2))))
    (degrees (atan (p:x delta-xz) (p:z delta-xz)))))

;;;; vegetation =========================================================

(defclass-kons-9 vegetation (boid-animator)
  ((nutrition-value 1.0)))

(defmethod initialize-instance :after ((boid vegetation) &rest initargs)
  (declare (ignore initargs))
  (setf (shape-fn boid) (lambda ()
                          (let ((shape (make-sphere-uv-mesh 0.4 6 6)))
                            shape))))

(defmethod update-boid ((boid vegetation))
  ;; do nothing
  )

;;;; grazer ==============================================================

(defclass-kons-9 grazer (vegetation)
  ((energy 2.0)
   (energy-use 0.01)
   (heading 0.0)
   (idle-speed 0.1)
   (top-speed 0.2)
   (detection-dist 2.0)
   (food-type 'vegetation)))

(defmethod initialize-instance :after ((boid grazer) &rest initargs)
  (declare (ignore initargs))
  (setf (shape-fn boid) (lambda ()
                          (let* ((shape (make-shape-group (list (make-box .4 .4 .8)
                                                                (rotate-by (make-circle-curve (* 2 (detection-dist boid)) 32) (p! 90 0 0))))))
                            (setf (heading boid) (rand2 -180 180))
                            (rotate-to shape (p! 0 (heading boid) 0))
                            shape)))
  (setf (update-fn boid) #'update-boid))

(defmethod update-boid ((boid grazer))
  ;; find food
  (let* ((food (nearest-food-within-range boid))
         (angle (and food (boid-heading boid food))))
    (if angle
        (setf (heading boid) angle)
        (incf (heading boid) (rand2 -10 10)))
    ;; move
    (let ((dx (sin (radians (heading boid))))
          (dz (cos (radians (heading boid)))))
      (rotate-to (shape boid) (p! 0 (heading boid) 0))
      (translate-by (shape boid)
                    (p:scale (p! dx 0 dz) (if food (top-speed boid) (idle-speed boid)))))
    ;; eat
    (when (and food (< (boid-dist boid food) 0.2))
      (incf (energy boid) (nutrition-value food))
      (setf (expired? food) t))
    ;; use energy
    (decf (energy boid) (energy-use boid))
    (when (<= (energy boid) 0)
      (setf (expired? boid) t))))

(defmethod nearest-food-within-range ((boid grazer))
  (let ((nearest nil)
	(min-dist -1.0))
    (do-array (i other (remove boid (children (boid-system boid))))
      (when (eq (food-type boid) (type-of other))
	(let ((dist (boid-dist boid other)))
	  (when (and (< dist (detection-dist boid))
		     (or (= min-dist -1.0)
			 (< dist min-dist)))
	    (setf nearest other)
	    (setf min-dist dist)))))
    nearest))

;;;; predator ===========================================================

(defclass-kons-9 predator (grazer)
  ()
  (:default-initargs
   :energy-use 0.02
   :food-type 'grazer
   :detection-dist 3.0
   :idle-speed 0.15
   :top-speed 0.4))

(defmethod initialize-instance :after ((boid predator) &rest initargs)
  (declare (ignore initargs))
  (setf (shape-fn boid)
        (lambda ()
          (let ((shape (make-shape-group
                        (list (freeze-transform (rotate-by (make-pyramid-uv-mesh .6 1.2 1 1) (p! 90 0 0)))
                              (rotate-by (make-circle-curve (* 2 (detection-dist boid)) 32) (p! 90 0 0))))))
                            
;;                            (rotate-to shape (p! 90 0 0))
;;                            (freeze-transform shape)
;;                            (setf (show-axis shape) 1.0)
            (setf (heading boid) (rand2 -180 180))
            (rotate-to shape (p! 0 (heading boid) 0))
            shape))))

;;;; boid-system ==========================================================

(defclass-kons-9 boid-system (motion-group)
  ((shape (make-instance 'shape-group))
   (bounds-min (p! -10 0 -10))
   (bounds-max (p!  10 0  10))
   (spawn-new-boids-fn nil)))

(defmethod initialize-instance :after ((self boid-system) &rest initargs)
  (declare (ignore initargs))
  )

(defmethod update-motion :after ((b-sys boid-system) parent-absolute-timing)
  ;; clean up expired boids
  (let ((boids (copy-seq (children b-sys))))
    (do-array (i boid boids)
      (when (expired? boid)
        (remove-child b-sys boid)
        (remove-child (shape b-sys) (shape boid))))
    ;; update boid transforms
    (wrap-boid-transforms b-sys)
    ;; spawn new boids
    (when (spawn-new-boids-fn b-sys)
      (funcall (spawn-new-boids-fn b-sys) b-sys))))

(defmethod wrap-boid-transforms ((b-sys boid-system))
  (do-children (child (shape b-sys))
    (let* ((trans (offset (translate (transform child))))
           (lo (bounds-min b-sys))
           (hi (bounds-max b-sys))
           (size (p- hi lo)))
      (translate-to child (p! (cond ((< (p:x trans) (p:x lo)) (+ (p:x trans) (p:x size)))
                                    ((> (p:x trans) (p:x hi)) (- (p:x trans) (p:x size)))
                                    (t (p:x trans)))
                              0.0
                              (cond ((< (p:z trans) -10) (+ (p:z trans) (p:z size)))
                                    ((> (p:z trans)  10) (- (p:z trans) (p:z size)))
                                    (t (p:z trans))))))))

(defmethod spawn-boids ((self boid-system) class num
                        &optional (loc-fn (lambda (sys) (p-rand2 (bounds-min sys) (bounds-max sys)))))
  (dotimes (i num)
    (let* ((boid (make-instance class))
	   (shape (funcall (shape-fn boid))))
      (setf (boid-system boid) self)
      (setf (shape boid) shape)
      (translate-by shape (funcall loc-fn self))
      (add-child self boid)             ;add motion child
      (add-child (shape self) shape)))) ;add shape child

