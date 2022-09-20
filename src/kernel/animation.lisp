(in-package #:kons-9)

;;;; animation =================================================================

(defclass-kons-9 animation (scene-item)
  ((shape nil)
   (shape-animator nil)))

(defmethod setup-motion ((anim animation))
  (when (shape-animator anim)
    (setup-motion (shape-animator anim))))

(defmethod update-motion ((anim animation) parent-absolute-timing)
  (when (shape-animator anim)
    (update-motion (shape-animator anim) parent-absolute-timing)))

(defmethod add-animation-to-scene ((anim animation) group motion-group
                                   &key (mode :add-as-instance)) ; :add-as-duplicate
  (cond ((eq :add-as-instance mode)
         (let* ((instance-shape-group (make-group (list (shape anim))))
                (anim-dup (duplicate (shape-animator anim)))
                (instance-motion-group (make-motion-group (list anim-dup))))
           (setf (shape anim-dup) instance-shape-group)
           (setf (scene instance-motion-group) (scene anim-dup))
           (add-child group instance-shape-group)
           (add-child motion-group instance-motion-group)))
        ((eq :add-as-duplicate mode)
         (error "Not implemented"))
        (t
         (error "Unknown mode ~a" mode))))
