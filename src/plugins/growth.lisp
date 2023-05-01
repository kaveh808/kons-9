(in-package #:kons-9)

;;;; growing objects in an environment =========================================

;;;; growth-environment ========================================================

;;; make subclass of shape-group and store features in children slot
(defclass-kons-9 growth-environment (shape-group motion-group)
  ((field (make-scalar-field 20 20 20))
   (field-min-value 0.0)
   (field-max-value 1.0)
   (show-grid? nil)))

(defmethod make-growth-environment (x-dim y-dim z-dim &key (bounds-lo (p! -1 -1 -1)) (bounds-hi (p! 1 1 1)))
  (make-instance 'growth-environment :field (make-scalar-field x-dim y-dim z-dim
                                                               :bounds-lo bounds-lo
                                                               :bounds-hi bounds-hi)))

(defmethod draw :after ((env growth-environment))
  (when (show-grid? env)
    (let ((field (field env)))
      (3d-draw-grid (vector (x-dim field) (y-dim field) (z-dim field))
                    (bounds-lo field)
                    (bounds-hi field)
                    (c! .8 .8 .8)))))

(defmethod get-resource-at-point ((env growth-environment) p)
  (field-value-at-point (field env) p))

(defmethod create-feature-at-point ((env growth-environment) feature p)
  (setf (environment feature) env)
  (add-child env (generate-shape-at-point feature p)))

;; (defmethod update-motion :after ((env growth-environment) parent-absolute-timing)
;;   (declare (ignore parent-absolute-timing))
;;   (print (list (field-min-value env) (field-max-value env)))
;;   (set-field-value-limits (field env) :min-value (field-min-value env) :max-value (field-max-value env)))

;;;; growth-feature ============================================================

(defclass-kons-9 growth-feature (shape-group motion)
  ((environment nil)
   (resource-usage-amount 1.0)
   (resource-usage-extent 1.0)))

(defmethod location ((feature growth-feature))
  (offset (translate (transform feature))))

(defmethod generate-shape-at-point ((feature growth-feature) p)
  ;; create cube -- specialize in subclass
  (add-child feature (make-cube 0.1))
  (translate-to feature p))

(defmethod update-motion ((feature growth-feature) parent-absolute-timing)
  (grow-using-resource feature))

(defmethod grow ((feature growth-feature) amount)
  ;; do scaling
  (scale-by feature (1+ amount)))

(defmethod grow-using-resource ((feature growth-feature))
  ;; grow feature based on available resource
  (grow feature (get-resource-at-point (environment feature) (location feature)))
  ;; reduce resource by feature usage
  (consume-resource feature))

(defmethod consume-resource ((feature growth-feature))
  (let ((env (environment feature)))
    (apply-field-function (field env)
                          (point-source-field-fn (make-point-cloud (vector (location feature)))
                                                 (in-out-cubic-value-fn (- (resource-usage-amount feature))
                                                                        (resource-usage-extent feature)))
                          :operation :add
                          :min-value (field-min-value env)
                          :max-value (field-max-value env))))

;;;; growth-grass-clump ========================================================

(defclass-kons-9 growth-grass-clump (growth-feature)
  ((p-system nil)
   (growth-steps-per-resource 10)))

(defmethod generate-shape-at-point ((grass growth-grass-clump) p)
  (setf (p-system grass)
        (make-particle-system-from-point p
                                         ;1 (p! 0 .5 0) (p! 0 .5 0)
                                         4 (p! -.2 .2 -.2) (p! .2 .4 .2)
                                         'scalar-field-particle ;grow particles in scalar-field
                                         :field (field (environment grass))
                                         :update-angle (range-float 10.0 5.0)
                                         ))
  (add-child grass
             (make-sweep-mesh-group (make-circle 0.5 4) (p-system grass) :taper 0.0 :twist 0.0)))

(defmethod grow ((grass growth-grass-clump) amount)
  (update-motion (p-system grass) 0.0)
  ;; set green ramp color
  (let ((group (aref (children grass) 0)))
    (set-point-colors-by-uv group (lambda (u v)
                                   (declare (ignore u))
                                   (c-lerp v (c! 0 .5 0) (c! .8 1 .8))))))

(defmethod consume-resource ((grass growth-grass-clump))
  (apply-field-function (field (environment grass))
                        (growth-shadow-field-fn (p-system grass)
                                                (- (resource-usage-amount grass))
                                                (resource-usage-extent grass))
                        :operation :add))

(defun growth-shadow-field-fn (point-source amount extent)
  (let ((points (source-points point-source)))
    (lambda (p)
      (let ((val 0))
        (do-array (i source-p points)
          (when (and (< (p:y p) (p:y source-p))
                   (< (p-dist (p! (p:x p) 0 (p:z p))
                              (p! (p:x source-p) 0 (p:z source-p))) extent))
              (incf val amount)))
        val))))
