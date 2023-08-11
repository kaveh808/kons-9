(in-package #:kons-9)
(declaim (optimize debug))
;;;; range attributes ====================================================

(defgeneric range-value (range)
  )

(defgeneric range-mutate (range factor)
  )

(defmethod range-value ((range number))
  range)

(defclass range-attr ()
  ((mean :accessor mean :initarg :mean :initform 0.0)
   (delta :accessor delta :initarg :delta :initform 1.0)))

(defmethod range-duplicate ((range range-attr))
  (make-instance (type-of range) :mean (mean range) :delta (delta range)))

(defclass range-float (range-attr)
  ()
  (:default-initargs
   :mean 0.0
   :delta 1.0))

(defun range-float (mean delta)
  (make-instance 'range-float :mean mean :delta delta))

(defmethod range-value ((range range-float))
  (rand1 (delta range) (mean range)))

(defmethod range-mutate ((range range-float) factor)
  (range-float (+ (mean range) (* factor (rand1 (delta range))))
               (+ (delta range) (* factor (rand1 (delta range))))))

(defclass range-point (range-attr)
  ()
  (:default-initargs
   :mean (p! 0 0 0)
   :delta (p! 1 1 1)))

(defun range-point (mean delta)
  (make-instance 'range-point :mean mean :delta delta))

(defmethod range-value ((range range-point))
  (p-rand1 (delta range) (mean range)))

(defmethod range-mutate ((range range-point) factor)
  (declare (ignore factor))
  (error "RANGE-MUTATE not implemented for class RANGE-POINT"))

;;;; particle ============================================================

(defclass-kons-9 particle ()
  ((pos (p! 0 0 0))
   (vel (p! 0 0 0))
   (col (c! 0 0 0 1))
   (is-alive? t)
   (generation 1)
   (life-span -1) ; -1 = immortal
   (age 0)

   ;; size, color, alpha
   
   (points (make-array 0 :adjustable t :fill-pointer t))
   (point-colors (make-array 0 :adjustable t :fill-pointer t))
;;   (behaviors (make-array 0 :adjustable t :fill-pointer t))

   (update-color-fn nil)
   
   (update-angle (range-float 0.0 0))

   (spawn-done? nil)
   (spawn-mutate? nil)
   (spawn-number-children (range-float 2 0))
   (spawn-angle (range-float 45.0 22.5))
   (spawn-life-span-factor (range-float 1.0 0))
   (spawn-velocity-factor (range-float 1.0 0))))

(defmethod copy-particle-data ((dst particle) (src particle))
  (setf (spawn-mutate? dst) (spawn-mutate? src))
  (setf (update-angle dst) (range-duplicate (update-angle src)))
  (setf (spawn-number-children dst) (range-duplicate (spawn-number-children src)))
  (setf (spawn-angle dst) (range-duplicate (spawn-angle src)))
  (setf (spawn-life-span-factor dst) (range-duplicate (spawn-life-span-factor src)))
  (setf (spawn-velocity-factor dst) (range-duplicate (spawn-velocity-factor src)))
  )

(defmethod mutate-particle ((ptcl particle) factor)
  (setf (update-angle ptcl) (range-mutate (update-angle ptcl) factor))
  (setf (spawn-number-children ptcl) (range-mutate (spawn-number-children ptcl) factor))
  (setf (spawn-angle ptcl) (range-mutate (spawn-angle ptcl) factor))
  (setf (spawn-life-span-factor ptcl) (range-mutate (spawn-life-span-factor ptcl) factor))
  (setf (spawn-velocity-factor ptcl) (range-mutate (spawn-velocity-factor ptcl) factor))
  ptcl)

(defun particle-random-color-fn ()
  (lambda (ptcl)
    (declare (ignore ptcl))
    (c-rand)))

(defun particle-velocity-color-fn (vel-1 col-1 vel-2 col-2)
  (lambda (ptcl)
    (c-lerp (clamp (tween (p:length (vel ptcl)) vel-1 vel-2) 0.0 1.0)
            col-1
            col-2)))

(defmethod update-color ((ptcl particle))
  (when (update-color-fn ptcl)
    (setf (col ptcl) (funcall (update-color-fn ptcl) ptcl))))

(defmethod update-velocity ((ptcl particle))
  (let* ((vel (vel ptcl))
         (rnd (p-rand))
         (axis (p:normalize (p:cross vel rnd)))
         (mtx (make-axis-rotation-matrix (radians (range-value (update-angle ptcl))) axis))
         (new-vel (transform-point vel mtx)))
    (setf (vel ptcl) new-vel)
    new-vel))

(defmethod update-position ((ptcl particle))
  (setf (pos ptcl)
        (p:+ (pos ptcl)
             (update-velocity ptcl))))

(defmethod update-particle ((ptcl particle))
  (if (or (= -1 (life-span ptcl))
          (< (age ptcl) (life-span ptcl)))
      (progn
        (update-position ptcl)
        (update-color ptcl)
        (incf (age ptcl)))
      (setf (is-alive? ptcl) nil)))

(defmethod spawn-velocity ((ptcl particle))
  (let* ((vel (vel ptcl))
         (rnd (p-rand))
         (axis (p:normalize (p:cross vel rnd)))
         (mtx (make-axis-rotation-matrix (radians (range-value (spawn-angle ptcl))) axis))
         (new-vel (transform-point vel mtx)))
    (p:scale new-vel (range-value (spawn-velocity-factor ptcl)))))

(defmethod spawn-particle ((ptcl particle))
  (let ((child (make-instance (type-of ptcl)
                              :pos (pos ptcl)
                              :vel (spawn-velocity ptcl)
                              :generation (1+ (generation ptcl))
                              :life-span (* (life-span ptcl)
                                            (range-value (spawn-life-span-factor ptcl)))
                              :update-color-fn (update-color-fn ptcl))))
    (copy-particle-data child ptcl)   ;transfer data
    (when (spawn-mutate? ptcl)
        (mutate-particle child 1.0))
    child))

(defmethod do-spawn ((ptcl particle))
  (if (and (not (= -1 (life-span ptcl)))
           (>= (age ptcl) (life-span ptcl))
           (not (spawn-done? ptcl)))
      (progn
        (setf (spawn-done? ptcl) t)
        ;; spawn offspring
        (let ((children '()))
          (dotimes (i (round (range-value (spawn-number-children ptcl))))
              (push (spawn-particle ptcl) children))
          children))
      nil))

;;;; climbing-particle ===================================================

(defclass climbing-particle (particle)
  ((support-polyh :accessor support-polyh :initarg :support-polyh :initform nil)))

(defmethod copy-particle-data ((dst climbing-particle) (src climbing-particle))
  (call-next-method)
  (setf (support-polyh dst) (support-polyh src)))

(defmethod update-position ((ptcl climbing-particle))
  (update-velocity ptcl)
  (let* ((new-pos (p:+ (pos ptcl) (vel ptcl)))
         (new-pos-on-polyh (polyhedron-closest-point (support-polyh ptcl) new-pos)))
    (when (not (p:= new-pos-on-polyh (pos ptcl))) ; avoid duplicate points
      (setf (vel ptcl) (p:scale (p:normalize (p-from-to (pos ptcl) new-pos-on-polyh))
                                (p:length (vel ptcl)))) ;redirect vel but keep magnitude
      (setf (pos ptcl) new-pos-on-polyh))))

;;;; scalar-field-particle ====================================================

(defclass scalar-field-particle (particle)
  ((field :accessor field :initarg :field :initform nil)))

(defmethod copy-particle-data ((dst scalar-field-particle) (src scalar-field-particle))
  (call-next-method)
  (setf (field dst) (field src)))

(defmethod update-position ((ptcl scalar-field-particle))
  (update-velocity ptcl)
  (let ((field-value (field-value-at-point (field ptcl) (pos ptcl))))
    (if (<= field-value 0.0)
        (setf (is-alive? ptcl) nil)
        (setf (pos ptcl) (p:+ (pos ptcl) (vel ptcl))))))

;;;; dynamic-particle ====================================================

(defclass dynamic-particle (particle)
  ((mass :accessor mass :initarg :mass :initform 1.0)
   (elasticity :accessor elasticity :initarg :elasticity :initform 0.75)
   (friction :accessor friction :initarg :friction :initform 0.75)
   (time-step :accessor time-step :initarg :time-step :initform 1.0) ;0.001)
   (force-fields :accessor force-fields :initarg :force-fields :initform '())
   (do-collisions? :accessor do-collisions? :initarg :do-collisions? :initform nil)
   (collision-padding :accessor collision-padding :initarg :collision-padding :initform 0.0)
   (spawn-mass-factor :accessor spawn-mass-factor :initarg :spawn-mass-factor :initform (range-float 1.0 0))))

(defmethod copy-particle-data ((dst dynamic-particle) (src dynamic-particle))
  (call-next-method)
  (setf (elasticity dst) (elasticity src))
  (setf (friction dst) (friction src))
  (setf (time-step dst) (time-step src))
  (setf (force-fields dst) (force-fields src))
  (setf (do-collisions? dst) (do-collisions? src))
  (setf (collision-padding dst) (collision-padding src))
  (setf (spawn-mass-factor dst) (range-duplicate (spawn-mass-factor src))))

(defmethod update-position ((ptcl dynamic-particle))
  (let* ((p0 (pos ptcl))
         (force (if (force-fields ptcl) ;compute force
                    (reduce #'p:+ (mapcar #'(lambda (field) (field-value field p0 (current-time *scene*)))
                                         (force-fields ptcl)))
                    +origin+))
         (acc (p/ force (mass ptcl)))  ;compute acceleration
         (vel (p+ (vel ptcl) acc))     ;compute velocity
         (pos (p:+ p0 (p:scale vel (time-step ptcl))))) ;compute position
    ;; handle collision
    (when (do-collisions? ptcl)
      (let ((elast (elasticity ptcl))
            (friction (friction ptcl))
            (lo (collision-padding ptcl)))
        (when (< (p:y pos) lo)
          (setf (p:y pos) (+ lo (abs (- lo (p:y pos))))
                (p:x vel) (* friction (p:x vel))
                (p:y vel) (* elast (- (p:y vel)))
                (p:z vel) (* friction (p:z vel))))))
    ;; update state
    (setf (vel ptcl) vel)
    (update-velocity ptcl)              ;do wiggle
    (setf (pos ptcl) pos)))

;;;; particle-system ====================================================

(defclass-kons-9 particle-system (shape animator)
  ((particles (make-array 0 :adjustable t :fill-pointer t))
   (max-generations -1) ; -1 = no maximum
   (use-point-colors? t)
   (draw-live-points-only? t)
   (draw-as-streaks? nil)
   (particle-class 'particle)
   (particle-initargs nil)
   (emitter-fn nil)
   (emitter-vel-fn nil)))

(defmethod print-object ((self particle-system) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a, timing = ~a ~a, ~a particles" (name self) (start-time self) (duration self)
            (length (particles self)))))

(defmethod add-force-field ((p-sys particle-system) (field force-field))
  (do-array (i ptcl (particles p-sys))
    (push field (force-fields ptcl)))
  p-sys)

(defmethod mutate-particle-system ((p-sys particle-system) factor)
  (do-array (i ptcl (particles p-sys))
    (mutate-particle ptcl factor))
  p-sys)

(defmethod draw ((p-sys particle-system))
  (when *display-wireframe?*
    (draw-wireframe p-sys))
  (when *display-points?*
    (draw-points p-sys)))

(defmethod draw-wireframe ((p-sys particle-system))
  (do-array (i ptcl (particles p-sys))
    (if (draw-as-streaks? p-sys)
        (let* ((i0 (1- (length (points ptcl))))
               (i1 (max 0 (1- i0))))
          (3d-draw-curve (vector (aref (points ptcl) i0) (aref (points ptcl) i1))
                         (if (use-point-colors? p-sys)
                             (vector (aref (point-colors ptcl) i0) (aref (point-colors ptcl) i1))
                             nil)
                         nil))
        (3d-draw-curve (points ptcl) (if (use-point-colors? p-sys) (point-colors ptcl) nil) nil))))

(defmethod draw-live-points ((p-sys particle-system))
  (let ((visible-points (make-array 0 :adjustable t :fill-pointer t))
        (visible-point-colors (make-array 0 :adjustable t :fill-pointer t)))
    (do-array-if (i ptcl #'is-alive? (particles p-sys))
      (vector-push-extend (pos ptcl) visible-points)
      (when (use-point-colors? p-sys)
        (vector-push-extend (col ptcl) visible-point-colors)))
    (3d-draw-points visible-points (if (use-point-colors? p-sys) visible-point-colors nil))))

(defmethod draw-points ((p-sys particle-system))
  (if (draw-live-points-only? p-sys)
      (draw-live-points p-sys)
      (do-array (i ptcl (particles p-sys))
        (3d-draw-points (points ptcl) (if (use-point-colors? p-sys) (point-colors ptcl) nil)))))

(defmethod draw-normals ((p-sys particle-system))
  ;; do nothing
  )

(defmethod add-particle ((p-sys particle-system) ptcl)
  (vector-push-extend ptcl (particles p-sys))
  (vector-push-extend (pos ptcl) (points ptcl))   ;store initial pos in points
  (vector-push-extend (col ptcl) (point-colors ptcl))   ;store initial pos in points
  p-sys)

(defmethod update-motion ((p-sys particle-system) parent-absolute-timing)
  (declare (ignore parent-absolute-timing))
  ;; emit any new poarticles
  (let* ((p-source (if (emitter-fn p-sys) (funcall (emitter-fn p-sys)) nil))
         (points (if p-source (source-points p-source) nil))
         (velocities (if p-source
                         (if (emitter-vel-fn p-sys)
                             (map 'vector (emitter-vel-fn p-sys) (source-directions p-source))
                             (source-directions p-source))
                         nil)))
    (when (and points velocities)
      (loop for p across points
            for v across velocities
            do (add-particle p-sys (apply #'make-instance
                                          (particle-class p-sys)
                                          :pos p
                                          :vel v
                                          (particle-initargs p-sys))))))
  ;; for each particle
  (do-array (i ptcl (particles p-sys))
    (when (or (= -1 (max-generations p-sys))
              (<= (generation ptcl) (max-generations p-sys)))
      ;; update live particles
      (when (is-alive? ptcl)
        (update-particle ptcl)
        (vector-push-extend (pos ptcl) (points ptcl))
        (vector-push-extend (col ptcl) (point-colors ptcl)))
      ;; spawn new particles
      (dolist (child (do-spawn ptcl))
        (add-particle p-sys child)))))

(defmethod points ((p-sys particle-system))
  (if (draw-live-points-only? p-sys)
      (map 'vector #'pos (particles p-sys))
      (apply #'concatenate 'vector (map 'list #'points (particles p-sys)))))

(defmethod point-colors ((p-sys particle-system))
  (if (draw-live-points-only? p-sys)
      (map 'vector #'col (particles p-sys))
      (apply #'concatenate 'vector (map 'list #'point-colors (particles p-sys)))))

(defmethod curves ((p-sys particle-system))
  (map 'list #'points (particles p-sys)))

(defmethod get-bounds ((p-sys particle-system))
  (when (= 0 (length (particles p-sys)))
    (warn "PARTICLE-SYSTEM ~a does not have any points. Using default bounds values." p-sys)
    (return-from get-bounds (values (p! -1 -1 -1) (p! 1 1 1))))
  (points-bounds (points p-sys)))

;;;; point-source-protocol =====================================================

(defmethod provides-point-source-protocol? ((p-sys particle-system))
  t)

(defmethod source-points ((p-sys particle-system))
  (points p-sys))

(defmethod source-point-colors ((p-sys particle-system))
  (point-colors p-sys))

(defmethod source-directions ((p-sys particle-system))
  (apply #'concatenate 'vector (map
                                'list
                                (lambda (ptcl) (curve-tangents-aux (points ptcl) nil))
                                (particles p-sys))))

;;;; curve-source-protocol =====================================================

(defmethod provides-curve-source-protocol? ((p-sys particle-system))
  t)

(defmethod source-curves ((p-sys particle-system))
  (curves p-sys))

(defmethod source-curves-closed ((p-sys particle-system))
  (make-list (length (particles p-sys)) :initial-element nil)) ;always open

;;;; create particle systems ===================================================

(defun make-particle-system-with-emitter (p-source-fn vel-fn particle-class &rest particle-initargs)
  (let* ((p-source (funcall p-source-fn))
         (points (source-points p-source))
         (velocities (if vel-fn
                         (map 'vector vel-fn (source-directions p-source))
                         (source-directions p-source)))
         (p-sys (apply #'make-particle-system-aux
                       points
                       velocities
                       particle-class
                       particle-initargs)))
    (setf (particle-class p-sys) particle-class)
    (setf (particle-initargs p-sys) particle-initargs)
    (setf (emitter-fn p-sys) p-source-fn)
    (setf (emitter-vel-fn p-sys) vel-fn)
    p-sys))

(defun make-particle-system-from-point-source (p-source vel-fn particle-class &rest particle-initargs)
  (apply #'make-particle-system-aux
         (source-points p-source)
         (if vel-fn
             (map 'vector vel-fn (source-directions p-source))
             (source-directions p-source))
         particle-class
         particle-initargs))

(defun make-particle-system-from-point (point num min-vel max-vel particle-class &rest particle-initargs)
  (let ((pnts (make-array num))
        (vels (make-array num)))
    (dotimes (i num)
      (setf (aref pnts i) (p:copy point))
      (setf (aref vels i) (p-rand2 min-vel max-vel)))
    (apply #'make-particle-system-aux
           pnts
           vels
           particle-class
           particle-initargs)))

(defun make-particle-system-aux (points velocities particle-class &rest initargs)
  (let ((p-sys (make-instance 'particle-system)))
    (loop for p across points
          for v across velocities
          do (add-particle p-sys (apply #'make-instance particle-class :pos p :vel v initargs)))
    p-sys))

;;;; gui =======================================================================

(defun single-point-source-selected? ()
  (let ((selected-shapes (selected-shapes (scene *scene-view*))))
    (and (= 1 (length selected-shapes))
         (provides-point-source-protocol? (first selected-shapes)))))

(defun make-dynamic-particle-system ()
  (let* ((scene (scene *scene-view*))
         (p-source (selected-shape scene))
         (p-sys (make-particle-system-from-point-source
                 p-source
                 (lambda (v) (p:scale v 0.2))
                 'dynamic-particle
                 :life-span -1
                 :force-fields (list (make-instance 'constant-force-field
                                                    :force-vector (p! 0 -.02 0))))))
     (add-motion scene p-sys)
    p-sys))

(defun make-wriggly-particle-system ()
  (let* ((scene (scene *scene-view*))
         (p-source (selected-shape scene))
         (p-sys (make-particle-system-from-point-source
                 p-source
                 (lambda (v) (p:scale v 0.2))
                 'particle
                 :life-span -1
                 :update-angle (range-float 20.0 10.0))))
    (add-motion scene p-sys)
    p-sys))

(defun particle-command-table ()
  (let ((table (make-instance `command-table :title "Create Particle System")))
    (ct-make-shape :D "Dynamic Particles" (when (single-point-source-selected?)
                                            (make-dynamic-particle-system)))
    (ct-make-shape :W "Wriggly Particles" (when (single-point-source-selected?)
                                            (make-wriggly-particle-system)))
    table))

(register-dynamic-command-table-entry
 "Context" :P "Create Particle System"
 (lambda () (make-active-command-table (particle-command-table)))
 #'single-point-source-selected?)
