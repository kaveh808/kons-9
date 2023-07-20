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

(with-clear-scene
  (add-shape *scene* (make-hypotrochoid-curve 5 3 5 128)))

;; special case -- ellipse
(with-clear-scene
  (add-shape *scene* (make-hypotrochoid-curve 10 5 1 32)))

(with-clear-scene
  (add-shape *scene* (make-epitrochoid-curve 3 1 .5 128)))

(with-clear-scene
  (add-shape *scene* (make-epitrochoid-curve 9 7 5 512)))

;;; spirograph-like setup ------------------------------------------------------

(with-clear-scene
  (let* ((ring-radius 2.5)
         (gear-radius 0.7)
         (arm-length 0.5)
         (gear-inside-ring? t)
         (rotation-increment 5)
         (gear-rotation-step (* (/ ring-radius gear-radius)
                                rotation-increment
                                (if gear-inside-ring? -1.0 1.0)))
         (gear-offset (if gear-inside-ring?
                          (- ring-radius gear-radius)
                          (+ ring-radius gear-radius)))
         (curve (make-instance 'curve :is-closed-curve? nil))
         (gear (make-circle (* 2 gear-radius) 16))
         (arm (make-line-curve (p! 0 0 0) (p! 0 arm-length 0) 1))
         (gear-assembly (translate-by (make-shape-group (list gear arm))
                                       (p! 0 gear-offset 0)))
         (top-assembly (make-shape-group (list gear-assembly)))
         (anim (make-instance 'animator
                              :update-fn (lambda ()
                                           (rotate-by top-assembly (p! 0 0 rotation-increment))
                                           (rotate-by gear-assembly (p! 0 0 gear-rotation-step))
                                           (append-point curve
                                                         (shape-global-point *scene*
                                                                             gear
                                                                             (p! 0 arm-length 0)))
                                           ))))
    (setf (end-frame *scene*) 10000)
    (add-shape *scene* top-assembly)
    (add-shape *scene* (make-circle (* 2 ring-radius) 64))
    (add-shape *scene* curve)
    (add-motion *scene* anim)))

;;;; END ========================================================================

