#|
These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

https://graphics.pixar.com/usd/release/tut_xforms.html
|#

(with-clear-scene
  (let ((top (add-shape *scene*
                        (freeze-transform
                                     (translate-to
                                      (rotate-to
                                       (make-pyramid-uv-mesh 2 2 3 3)
                                       (p! 180 0 0))
                                      (p! 0 2 0)))))
        (xform-op-precess (make-instance 'angle-axis-rotate-operator :axis (p! 0 1 0)))
        (xform-op-offset (make-instance 'translate-operator :offset (p! 0.2 0 0)))
        (xform-op-tilt (make-instance 'angle-axis-rotate-operator :angle 12 :axis (p! 1 0 0)))
        (xform-op-spin (make-instance 'angle-axis-rotate-operator :axis (p! 0 1 0))))
    (setf (end-frame *scene*) 192)
    (setf (transform top)
          (make-instance 'generalized-transform
                         :operators (list xform-op-precess xform-op-offset xform-op-tilt xform-op-spin)))
    (add-motion *scene*
                (set-children (make-instance 'motion-group :name 'top-motion-group)
                              (list
                               (make-instance
                                'shape-animator :scene *scene* :shape top
                                                :update-fn (lambda (anim)
                                                             (setf (angle xform-op-precess)
                                                                   (lerp (local-time anim) 0 360))))
                               (make-instance
                                'shape-animator :scene *scene* :shape top
                                                :update-fn (lambda (anim)
                                                             (setf (angle xform-op-spin)
                                                                   (lerp (local-time anim) 0 1440)))))))))
(setf (end-frame *scene*) 42)

(setf (duration (find-motion-by-name *scene* 'top-motion-group)) 0.5)

;;;; easing function visualizations ============================================

(defparameter *easing-fns*
  (list (list #'linear-fn)
        (list #'in-sine-fn #'out-sine-fn #'in-out-sine-fn)
        (list #'in-cubic-fn #'out-cubic-fn #'in-out-cubic-fn)
        (list #'in-quad-fn #'out-quad-fn #'in-out-quad-fn)
        (list #'in-quart-fn #'out-quart-fn #'in-out-quart-fn)
        (list #'in-quint-fn #'out-quint-fn #'in-out-quint-fn)
        (list #'in-exp-fn #'out-exp-fn #'in-out-exp-fn)
        (list #'in-circ-fn #'out-circ-fn #'in-out-circ-fn)
        (list #'in-elastic-fn #'out-elastic-fn #'in-out-elastic-fn)
        (list #'in-back-fn #'out-back-fn #'in-out-back-fn)
        (list #'in-bounce-fn #'out-bounce-fn #'in-out-bounce-fn)))

(defun make-ease-curve (fn &optional (num-segments 64))
  (let* ((curve (make-line-curve (p! 0 0 0) (p! 1 0 0) num-segments))
         (points (points curve)))
    (do-array (i p points)
      (setf (p:y p) (funcall fn (p:x p))))
    curve))

(set-lines-thin)

(with-clear-scene
  (let ((x -1.2)
        (y -1.5))
    (add-shape *scene*
               (make-shape-group (mapcar (lambda (list)
                                           (incf x 1.2)
                                           (setf y -1.5)
                                     (make-shape-group (mapcar (lambda (fn)
                                                                 (incf y 1.5)
                                                                 (translate-to (make-ease-curve fn) (p! x y 0)))
                                                               list)))
                                         *easing-fns*)))))

;;;; easing function test ======================================================

(progn
  (defparameter *cube* (translate-to (make-cube         1.5) (p!    0 0 0)))
  (defparameter *octa* (translate-to (make-octahedron   2.0) (p! -2.5 0  0)))
  (defparameter *icos* (translate-to (make-icosahedron  2.0) (p!  2.5 0 0)))
  (defparameter *channel* (make-instance 'procedural-channel :value-fn #'out-elastic-fn))
  (with-clear-scene
    (add-shapes *scene* (list *cube* *octa* *icos*))
    (add-motions *scene*
                 (list
                  (make-instance 'shape-animator
                                 :scene *scene* :shape *cube*
                                 :update-fn (lambda (anim)
                                              (let ((val (get-value *channel* (local-time anim))))
                                                (translate-to (shape anim) (p! 0 val 0)))))
                  (make-instance 'shape-animator
                                 :scene *scene* :shape *octa*
                                 :update-fn (lambda (anim)
                                              (let ((val (get-value *channel* (local-time anim))))
                                                (rotate-to (shape anim) (p! 0 (* 360 val) 0)))))
                  (make-instance 'shape-animator
                                 :scene *scene* :shape *icos*
                                 :update-fn (lambda (anim)
                                              (let ((val (get-value *channel* (local-time anim))))
                                                (scale-to (shape anim) (p! val val val))))))))
  (setf (end-frame *scene*) 60))

;;;; variant-manager-group test ================================================

;;; http://www.sci.utah.edu/~wald/animrep/hand/hand.obj.tgz
(progn
  (defparameter *obj-directory* "~/Downloads/hand")
  (defparameter *obj-filename* "hand_")
  (defparameter *obj-start-frame* 0)
  (defparameter *obj-end-frame* 43)
  (defparameter *obj-file-padding* 2))

;;; http://www.sci.utah.edu/~wald/animrep/wood-doll/wood-doll.obj.tgz
(progn
  (defparameter *obj-directory* "~/Downloads/wooddoll")
  (defparameter *obj-filename* "wooddoll_")
  (defparameter *obj-start-frame* 0)
  (defparameter *obj-end-frame* 28)
  (defparameter *obj-file-padding* 2))

(defun anim-obj-filename (path filename index padding)
  (format nil (format nil "~~a/~~a~a.obj" (format nil "~~~a,'0d" padding)) path filename index))

(with-clear-scene
  (flet ((anim-obj-filename (path filename index padding)
           (format nil (format nil "~~a/~~a~a.obj" (format nil "~~~a,'0d" padding)) path filename index)))
    (let ((shapes '()))
      (dotimes (i (1+ (- *obj-end-frame* *obj-start-frame*)))
        (push (import-obj (anim-obj-filename *obj-directory* *obj-filename*
                                             (+ i *obj-start-frame*) *obj-file-padding*))
              shapes))
      (let ((group (set-children (make-instance 'variant-manager-group) (reverse shapes))))
        (setf (visible-index group) 0)  ;only make first shape visible
        (add-shape *scene* group)
        (add-motion *scene* 
                    (make-instance 'animator
                                   :setup-fn (lambda () (setf (visible-index group) 0))
                                   :update-fn (lambda () (setf (visible-index group)
                                                               (current-frame *scene*))))))))
  (setf (current-frame *scene*) *obj-start-frame*)
  (setf (start-frame *scene*) *obj-start-frame*)
  (setf (end-frame *scene*) *obj-end-frame*))

