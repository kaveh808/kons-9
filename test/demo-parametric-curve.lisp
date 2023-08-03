(in-package #:kons-9)

#|
These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)
|#

;;;; start parametric-curve demos ==============================================

;;; parametric-curve -----------------------------------------------------------

(format t "  parametric-curve...~%") (finish-output)

(with-clear-scene
  (add-shape *scene* (make-bezier-curve (p! -2 0 0) (p! -1 2 0) (p! 1 1 0) (p! 2 0 0))))

(with-clear-scene
  (add-shape *scene* (make-butterfly-curve 1024)))

;;; assign point colors
(with-clear-scene
  (let ((curve (add-shape *scene* (make-butterfly-curve 1024))))
    (set-point-colors-by-xyz curve
                             (lambda (p)
                               (c-rainbow (clamp (tween (p:y p) -2 2) 0.0 1.0))))))


(with-clear-scene
  (add-shape *scene* (make-hypotrochoid-curve 5 3 5 128)))

;;; assign point colors
(with-clear-scene
  (let ((curve (add-shape *scene* (make-hypotrochoid-curve 5 3 5 128))))
    (set-point-colors-by-order curve
                               (lambda (f) (c-rainbow f)))))

;; special case -- ellipse
(with-clear-scene
  (add-shape *scene* (make-hypotrochoid-curve 10 5 1 32)))

(with-clear-scene
  (add-shape *scene* (make-epitrochoid-curve 3 1 .5 128)))

(with-clear-scene
  (add-shape *scene* (make-epitrochoid-curve 9 7 5 512)))

;;; spirograph -----------------------------------------------------------------

(with-clear-scene
  (setf (end-frame *scene*) 10000)      ;arbitrarily long scene duration
  (make-spirograph *scene* 2.5 0.7 0.6 t  0.0 5.0))

;;; hold down space key in 3D view to run animation

;;; run complete cycle
(update-scene *scene* 505)

(with-clear-scene
  (setf (end-frame *scene*) 10000)      ;arbitrarily long scene duration
  (let ((curve (make-spirograph *scene* 2.5 0.3 0.6 t  0.0 2.0)))
    (update-scene *scene* 541)             ;run complete cycle
    (set-point-colors-by-order curve
                               (lambda (f) (c-rainbow f)))))

(with-clear-scene
  (setf (end-frame *scene*) 10000)      ;arbitrarily long scene duration
  (make-spirograph *scene* 2.5 0.7 0.6 t  0.0 5.0 :color (c-rainbow 0.0))
  (make-spirograph *scene* 2.5 0.7 0.6 t  3.0 5.0 :color (c-rainbow 0.25))
  (make-spirograph *scene* 2.5 0.7 0.6 t  6.0 5.0 :color (c-rainbow 0.5))
  (make-spirograph *scene* 2.5 0.7 0.6 t  9.0 5.0 :color (c-rainbow 0.75))
  (make-spirograph *scene* 2.5 0.7 0.6 t 12.0 5.0 :color (c-rainbow 1.0))
  (make-spirograph *scene* 2.5 0.3 0.25 nil 0.0 2.5 :color (c! 1 0 0))
  (make-spirograph *scene* 2.75 0.35 0.2 nil 0.0 2.5 :color (c! 0 1 0))
  (make-spirograph *scene* 2.5 0.4 0.6 nil 0.0 2.5 :color (c! 0 0 1))
  )

;;; hold down space key in 3D view to run animation

;;; run complete cycle
(update-scene *scene* 1010)

(with-clear-scene
  (setf (end-frame *scene*) 10000)      ;arbitrarily long scene duration
  (let ((curve (make-spirograph *scene* 2.5 0.3 0.6 t  0.0 2.0)))
    (update-scene *scene* 542)             ;run complete cycle
    (set-point-colors-by-xyz (add-shape *scene* (make-sweep-mesh (make-circle .2 6) 0 curve 0))
                             (lambda (p)
                               (c-rainbow (clamp (tween (p:y p) -2 2) 0.0 1.0))))))

;;; "double" spirograph --------------------------------------------------------

(with-clear-scene
  (setf (end-frame *scene*) 10000)      ;arbitrarily long scene duration
  (make-spirograph-double *scene* 2.5 0.7 0.5 0.6 t nil  0.0 2.0 :show-assembly? t))

;;; hold down space key in 3D view to run animation

;;; run complete cycle
(update-scene *scene* 1264)

(with-clear-scene
  (setf (end-frame *scene*) 10000)      ;arbitrarily long scene duration
  (make-spirograph-double *scene* 2.5 0.7 0.2 0.6 t nil  0.0 1.0 :show-assembly? t))

;;; hold down space key in 3D view to run animation

;;; run complete cycle
(update-scene *scene* 5042)

(with-clear-scene
  (setf (end-frame *scene*) 10000)      ;arbitrarily long scene duration
  (make-spirograph-double *scene* 2.5 0.7 0.2 0.3 nil nil  0.0 1.0 :show-assembly? t))

;;; hold down space key in 3D view to run animation

;;; run complete cycle
(update-scene *scene* 5042)

(with-clear-scene
  (setf (end-frame *scene*) 10000)      ;arbitrarily long scene duration
  (make-spirograph-double *scene* 2.5 0.7 0.5 0.6 t nil  0.0 2.0 :color (c-rainbow 0.00))
  (make-spirograph-double *scene* 2.5 0.7 0.5 0.6 t nil  3.0 2.0 :color (c-rainbow 0.25))
  (make-spirograph-double *scene* 2.5 0.7 0.5 0.6 t nil  6.0 2.0 :color (c-rainbow 0.50))
  (make-spirograph-double *scene* 2.5 0.7 0.5 0.6 t nil  9.0 2.0 :color (c-rainbow 0.75))
  (make-spirograph-double *scene* 2.5 0.7 0.5 0.6 t nil 12.0 2.0 :color (c-rainbow 1.00))
  )

;;; hold down space key in 3D view to run animation

;;; run complete cycle
(update-scene *scene* 1264)

;;; spirograph with spring -----------------------------------------------------

;;; spring
(with-clear-scene
  (setf (end-frame *scene*) 10000)      ;arbitrarily long scene duration
  (make-spirograph-spring *scene* 2.5 0.7 0.6 t 0.0 2.0 2.0 1.0 0.5
                          :show-assembly? t :color (c! 0 0 1)
                          :do-collisions? nil
                          :force-fields '()))
;;; hold down space key in 3D view to run animation

;;; run simulation
(update-scene *scene* 500)

;;; spring, force-field
(with-clear-scene
  (setf (end-frame *scene*) 10000)      ;arbitrarily long scene duration
  (make-spirograph-spring *scene* 2.5 0.7 0.6 t 0.0 2.0 2.0 1.0 0.5
                          :show-assembly? t :color (c! 0 0 1)
                          :do-collisions? nil
                          :force-fields (list (make-instance 'time-varying-force-field
                                                             :force-vector (p! 0 0 0)
                                                             :noise-frequency 10.0
                                                             :noise-amplitude 8.0))))
;;; hold down space key in 3D view to run animation

;;; run simulation
(update-scene *scene* 500)

;;; spring, force-field, ground collision
(with-clear-scene
  (setf (end-frame *scene*) 10000)      ;arbitrarily long scene duration
  (make-spirograph-spring *scene* 2.5 0.7 0.6 t 0.0 2.0 2.0 1.0 0.5
                          :show-assembly? t :color (c! 0 0 1)
                          :do-collisions? t
                          :force-fields (list (make-instance 'time-varying-force-field
                                                             :force-vector (p! 0 0 0)
                                                             :noise-frequency 10.0
                                                             :noise-amplitude 8.0))))

;;; hold down space key in 3D view to run animation

;;; run simulation
(update-scene *scene* 500)

;;;; END ========================================================================

