
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

(defmethod make-dynamic ((shape shape))
  (make-instance 'dynamics-animator :shape shape))

;;;; l-system ===========================================================

(defclass turtle ()
  ((location :accessor location :initarg :location :initform +origin+)
   (heading :accessor heading :initarg :heading :initform +y-axis+)
   (paths :accessor paths :initarg :paths :initform '(()))
   (stack :accessor stack :initarg :stack :initform '())))

(defmethod turtle-init ((self turtle) location heading)
  (setf (location self) location)
  (setf (heading self) heading)
  (setf (paths self) (list (list location)))
  (setf (stack self) '())
  self)

(defmethod turtle-forward ((turtle turtle) &optional (distance 1.0))
  (setf (location turtle) (p+ (location turtle) (p* (heading turtle) distance)))
  (push (location turtle) (first (paths turtle))))

(defmethod turtle-rotate ((turtle turtle) &optional (angle 90.0))
  (let ((matrix (make-z-rotation-matrix (radians angle))))
    (setf (heading turtle) (transform-point (heading turtle) matrix))))

(defmethod turtle-push ((turtle turtle))
  (push (list (location turtle) (heading turtle)) (stack turtle)))

(defmethod turtle-pop ((turtle turtle))
  (let ((data (pop (stack turtle))))
    (setf (location turtle) (first data))
    (setf (heading turtle) (second data)))
  (push '() (paths turtle))             ;new path
  (push (location turtle) (first (paths turtle))))

(defmethod turtle-move ((turtle turtle) command-list)
  (mapc #'(lambda (command) (funcall command turtle)) command-list))

(defclass l-system (animator)
  ((axiom :accessor axiom :initarg :axiom :initform '())
   (initial-location :accessor initial-location :initarg :initial-location :initform +origin+)
   (initial-heading :accessor initial-heading :initarg :initial-heading :initform +y-axis+)
   (production-rules :accessor production-rules :initarg :production-rules :initform '())
   (graphics-rules :accessor graphics-rules :initarg :graphics-rules :initform '())
   (genome :accessor genome :initarg :genome :initform '())
   (turtle :accessor turtle :initarg :turtle :initform nil)))

(defun apply-production-rules (genome rules)
  (let ((new-genome '()))
    (dolist (gene genome)
      (let ((rule (assoc gene rules)))
        (if rule
            (setf new-genome (append new-genome (cdr rule)))
            (setf new-genome (append new-genome (list gene))))))
    new-genome))
  
(defun apply-graphics-rules (genome rules)
  (let ((moves '()))
    (dolist (gene genome)
      (let ((rule (assoc gene rules)))
        (when rule
            (push (cadr rule) moves))))
    (reverse moves)))

(defmethod init-animator ((anim l-system))
  (setf (genome anim) '())
  (setf (turtle anim)
        (turtle-init (make-instance 'turtle) (initial-location anim) (initial-heading anim)))
  (empty-point-array (shape anim)))

(defmethod update-animator ((anim l-system))
  (if (null (genome anim))
      (setf (genome anim) (copy-list (axiom anim)))
      (setf (genome anim) (apply-production-rules (genome anim) (production-rules anim))))
  (if (null (shape anim))
    (format t "ERROR: animator ~a has no SHAPE.~%" anim)
    (progn
      (setf (turtle anim)
            (turtle-init (make-instance 'turtle) (initial-location anim) (initial-heading anim)))
      (empty-point-array (shape anim))
      (turtle-move (turtle anim) (apply-graphics-rules (genome anim) (graphics-rules anim)))
      (add-new-point-lists (shape anim) (paths (turtle anim))))))


(defun test-l-system (anim)
  (dotimes (i 4)
    (update-animator anim)
    (format t "~a: ~a~%" i (genome anim))))

(defun make-koch-curve-l-system (shape)
  (make-instance 'l-system :shape shape
                           :initial-heading +x-axis+
                           :axiom '(F)
                           :production-rules '((F F + F - F - F + F))
                           :graphics-rules `((F ,#'(lambda (tu) (turtle-forward tu)))
                                             (+ ,#'(lambda (tu) (turtle-rotate tu  90.0)))
                                             (- ,#'(lambda (tu) (turtle-rotate tu -90.0))))))

(defun make-binary-tree-l-system (shape)
  (make-instance 'l-system
                 :shape shape
                 :initial-heading +y-axis+
                 :axiom '(0)
                 :production-rules '((1 1 1) (0 1 [ 0 ] 0))
                 :graphics-rules `((0 ,#'(lambda (tu) (turtle-forward tu (rand1 0.2 1.0))))
                                   (1 ,#'(lambda (tu) (turtle-forward tu (rand1 0.2 1.0))))
                                   ([ ,#'(lambda (tu) (turtle-push tu) (turtle-rotate tu (rand1 20.0 45.0))))
                                   (] ,#'(lambda (tu) (turtle-pop tu) (turtle-rotate tu (rand1 20.0 -45.0)))))))

(defun make-serpinski-triangle-l-system (shape)
  (make-instance 'l-system
                 :shape shape
                 :initial-heading +y-axis+
                 :axiom '(F - G - G)
                 :production-rules '((F F - G + F + G - F) (G G G))
                 :graphics-rules `((F ,#'(lambda (tu) (turtle-forward tu)))
                                   (G ,#'(lambda (tu) (turtle-forward tu)))
                                   (+ ,#'(lambda (tu) (turtle-rotate tu  120.0)))
                                   (- ,#'(lambda (tu) (turtle-rotate tu -120.0))))))

(defun make-serpinski-arrowhead-l-system (shape)
  (make-instance 'l-system
                 :shape shape
                 :initial-heading +y-axis+
                 :axiom '(A)
                 :production-rules '((A B - A - B) (B A + B + A))
                 :graphics-rules `((A ,#'(lambda (tu) (turtle-forward tu)))
                                   (B ,#'(lambda (tu) (turtle-forward tu)))
                                   (+ ,#'(lambda (tu) (turtle-rotate tu  60.0)))
                                   (- ,#'(lambda (tu) (turtle-rotate tu -60.0))))))

(defun make-dragon-curve-l-system (shape)
  (make-instance 'l-system
                 :shape shape
                 :initial-heading +y-axis+
                 :axiom '(F)
                 :production-rules '((F F + G) (G F - G))
                 :graphics-rules `((F ,#'(lambda (tu) (turtle-forward tu)))
                                   (G ,#'(lambda (tu) (turtle-forward tu)))
                                   (+ ,#'(lambda (tu) (turtle-rotate tu  90.0)))
                                   (- ,#'(lambda (tu) (turtle-rotate tu -90.0))))))

(defun make-fractal-plant-l-system (shape)
  (make-instance 'l-system
                 :shape shape
                 :initial-heading +y-axis+
                 :axiom '(X)
                 :production-rules '((X F + [ [ X ] - X ] - F [ - F X ] + X) (F F F))
                 :graphics-rules `((F ,#'(lambda (tu) (turtle-forward tu (rand1 0.2 1.0))))
                                   (+ ,#'(lambda (tu) (turtle-rotate tu (rand1 5.0 25.0))))
                                   (- ,#'(lambda (tu) (turtle-rotate tu (rand1 5.0 -25.0))))
                                   ([ ,#'(lambda (tu) (turtle-push tu)))
                                   (] ,#'(lambda (tu) (turtle-pop tu))))))

#|
(test-l-system (make-instance 'l-system :axiom '(A)
                                        :production-rules '((A A B) (B A))))

|#


