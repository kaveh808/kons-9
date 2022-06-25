(in-package #:kons-9)

;;;; force-field ========================================================

(defclass force-field ()
  ())

(defmethod field-value ((field force-field) point time)
  ;; subclass responsibility
  (declare (ignore point time))
  (p! 0 0 0))

;;;; constant-force-field ===============================================

(defclass constant-force-field (force-field)
  ((force-vector :accessor force-vector :initarg :force-vector :initform (p! 0 -9.81 0))))

(defmethod field-value ((field constant-force-field) point time)
  (declare (ignore point time))
  (force-vector field))

;;;; attractor-force-field ==============================================

(defclass attractor-force-field (force-field)
  ((location :accessor location :initarg :location :initform (p! 0 0 0))
   (magnitude :accessor magnitude :initarg :magnitude :initform 1.0)))

(defmethod field-value ((field attractor-force-field) point time)
  (declare (ignore time))
  (let ((dist (p-dist point (location field)))
        (dir (p-normalize (p-from-to point (location field)))))
    (if (= 0.0 dist)
        (p! 0 0 0)
        (p-scale dir (/ (magnitude field) (* dist dist))))))

;;;; noise-force-field ==================================================

(defclass noise-force-field (force-field)
  ((noise-frequency :accessor noise-frequency :initarg :noise-frequency :initform 1.0)
   (noise-amplitude :accessor noise-amplitude :initarg :noise-amplitude :initform 1.0)))

(defmethod field-value ((field noise-force-field) point time)
  (declare (ignore time))
  (p-scale (noise-gradient (p-scale point (noise-frequency field)))
           (noise-amplitude field)))

;;;; animator ===========================================================

(defclass animator (dependency-node-mixin)
  ((init-fn :accessor init-fn :initarg :init-fn :initform nil)
   (update-fn :accessor update-fn :initarg :update-fn :initform nil)
   (init-args :accessor init-args :initarg :init-args :initform '())
   (is-initialized? :accessor is-initialized? :initarg :is-initialized? :initform nil)
   (is-selected? :accessor is-selected? :initarg :is-selected? :initform nil)
   (shape :accessor shape :initarg :shape :initform nil)
   (data :accessor data :initarg :data :initform '())))

(defmethod copy-instance-data ((dst animator) (src animator))
  (setf (init-fn dst) (init-fn src))
  (setf (update-fn dst) (update-fn src))
  (setf (init-args dst) (init-args src))
  (setf (is-initialized? dst) (is-initialized? src))
;;   (setf (shape dst) (shape src)) -- do not copy shape
  (setf (data dst) (copy-list (data src)))
  dst)

(defmethod duplicate-animator ((anim animator))
  (let ((new-anim (make-instance (type-of anim))))
    (copy-instance-data new-anim anim)
    new-anim))

(defmethod init-animator ((anim animator))
  (when (init-fn anim)
    (funcall (init-fn anim) anim)))

(defmethod init-animator :after ((anim animator))
  (setf (is-initialized? anim) t))

(defmethod update-animator :before ((anim animator))
  (when (not (is-initialized? anim))
    (init-animator anim)))

(defmethod update-animator ((anim animator))
  (when (update-fn anim)
    (funcall (update-fn anim) anim)))

(defmethod update-animator :after ((anim animator))
  (setf (time-stamp anim) (get-internal-real-time)))

;;;; dynamics-animator ==================================================

(defclass dynamics-animator (animator)
  ((velocity :accessor velocity :initarg :velocity :initform (p! 0 0 0))
   (mass :accessor mass :initarg :mass :initform 1.0)
   (elasticity :accessor elasticity :initarg :elasticity :initform 0.75)
   (friction :accessor friction :initarg :friction :initform 0.75)
   (time-step :accessor time-step :initarg :time-step :initform 1.0)
   (force-fields :accessor force-fields :initarg :force-fields :initform '())
   (do-collisions? :accessor do-collisions? :initarg :do-collisions? :initform t)
   (collision-padding :accessor collision-padding :initarg :collision-padding :initform 0.0)))

(defmethod copy-instance-data :after ((dst dynamics-animator) (src dynamics-animator))
  (setf (velocity dst) (p-copy (velocity src)))
  (setf (mass dst) (mass src))
  (setf (elasticity dst) (elasticity src))
  (setf (friction dst) (friction src))
  (setf (time-step dst) (time-step src))
  (setf (force-fields dst) (copy-list (force-fields src)))
  (setf (do-collisions? dst) (do-collisions? src))
  (setf (collision-padding dst) (collision-padding src))
  dst)

(defmethod update-animator ((anim dynamics-animator))
  (if (null (shape anim))
    (error "DYNAMICS-ANIMATOR ~a HAS NO SHAPE.~%" anim)
    (let* ((p0 (translate (transform (shape anim))))
           (force (if (force-fields anim) ;compute force
                      (reduce #'p+ (mapcar #'(lambda (field) (field-value field p0
                                                                          (current-time *scene*)))
                                          (force-fields anim)))
                      +origin+))
	   (acc (p/ force (mass anim)))	;compute acceleration
	   (vel (p+ (velocity anim) acc)) ;compute velocity
	   (pos (p+ p0 (p* vel (time-step anim))))) ;compute position

      (when (do-collisions? anim)	; handle collision
	(let ((elast (elasticity anim))
              (friction (friction anim))
	      (lo (collision-padding anim)))
	  (when (< (y pos) lo)
	    (setf (y pos) (+ lo (abs (- lo (y pos)))))
	    (setf (x vel) (* friction (x vel)))
            (setf (y vel) (* elast (- (y vel))))
            (setf (z vel) (* friction (z vel))))))
            

      ;; update state
      (setf (velocity anim) vel)
      (translate-to (shape anim) pos))))

