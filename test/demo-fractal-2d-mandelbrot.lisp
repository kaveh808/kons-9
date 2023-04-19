(in-package #:kons-9)

#|

These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

|#

#|

This is the first of a series of experiments with fractals. The
purpose is to explore kons-9, play with lisp and have fun along the
way. The code is inefficient and the outcome in this demo is pretty
crude but hopefully there will be improvements in the future, trying
to use shaders and finally attempting a 3D fractal such as the
MandelBulb.

To display a 2D fractal, here's what we do:

1. Start out with a 2D grid of points (in the xy plane)
2. Eliminate points from the grid which do not satisfy some conditions.
3. Display the remaining points, which is the fractal.

|#




;; == utility functions ==============================================

;; macro to clear the scene and add a point cloud to the scene
(defmacro with-redraw-points (points)
  `(with-clear-scene
     (add-shape *scene* (make-point-cloud ,points))))

;; function to make a 2D grid of points, based on `make-grid-points`
;; from :kons-9
(defun make-grid-points-xy (width height &optional (density 2))
  (let ((x (/ width 2))
        (y (/ height 2)))
    (make-grid-points
     (* width density)
     (* height density)
     1
     (p! (- x) (- y) 0)
     (p! x y 0))))




;; == display the grid without eliminating points ====================

;; just to get a feel for things, lets draw an xy grid without
;; eliminating any points
(with-redraw-points
    (make-grid-points-xy
     3   ;; width
     2   ;; height
     20  ;; density of points - increase this for better results
     ))




;; == more utility functions =========================================

;; we need a mechanism which takes a 2D grid of points and eliminates
;; those points which don't satisfy the said conditions/equation. The
;; remaining points will make up the fractal.

;; similar to the common lisp `remove-if`, this function is a helper
;; to work with points so that the calling code is simpler without
;; having to extract x, y coordinates from a point and subsequently
;; coerce the result back into a vector (1D array).
(defun points-remove-if (predicate points)
  (coerce (remove-if
           (lambda (p)
             (let ((x (aref p 0))
                   (y (aref p 1)))
               (apply predicate `(,x ,y))))
           points)
          'vector))

(defun sq (x)
  (expt x 2))




;; == Display a circle ===============================================

;; we are now ready to elimiate points from the grid to display a
;; fractal. However before that, lets do an exercise in using the
;; above functions. Elimiate points such that we are left with a
;; circle, for which the math is simple and this help us get a sense
;; of things before moving onto the fractal.

;; Given a 2D grid consisting of points in the xy plane, eliminate
;; points which lie outside a circle, i.e. eliminate a point (x,y) on
;; the grid if: (> (+ (sq x) (sq y)) (sq radius))

;; it is based of the equation of points which lie on a circle:
;; x^2 + y^2 = r^2
;; https://en.wikipedia.org/wiki/Circle#Equations

(with-redraw-points
    (points-remove-if
     (lambda (x y)
       (> (+ (sq x) (sq y)) 1)) ; sq of radius = 1
     (make-grid-points-xy
      2 2 20))) ;;; increase the last arg (density) for more points




;; == Display a mandelbrot fractal ===================================

;; The circle example above used a predicate function based on the
;; equation of a circle. Now lets do things on similar lines but use
;; a predicate function with different maths to generate a fractal.

;; this demo does not explain fractal maths.

;; This video helped me understand the math behind the 2D Mandelbrot
;; fractal. A big thank you to the author for putting it together.
;; https://www.youtube.com/watch?v=NJCiUVGiNyA&t=153s

(with-redraw-points
    (points-remove-if
     (lambda (x y)
       (let ((threshold 10)
             (iterations 100) ;;; for fun, explore lower number of
             ;;; iterations betweeen 2 and 20. also try high values
             ;;; upto 1000. maybe slow with better results
             (c (complex x y)))
         (do* ((i 0 (+ i 1))
               (z (+ (complex 0 0) c) (+ (sq z) c))
               (infinite nil (>= (abs (realpart z)) threshold)))
              ((or infinite (>= i iterations)) infinite))))
     (make-grid-points-xy
      3 2 100) ;;; increase the last arg (density) for more points
               ;;; (requires more time to process)
     ))

;; A mandelbrot remebles the following ascii art:

;;                                                **
;;                                             ****** *
;;                                             ********
;;                                              *****
;;                                        *************** *
;;                                   ***********************  ***
;;                               *   ****************************
;;                               ********************************
;;                               *********************************  *
;;                              ************************************
;;               ** ******     ************************************
;;              *************  ************************************
;;             *************** ***********************************
;;         ******************************************************
;;    *********************************************************
;;         ******************************************************
;;             *************** ***********************************
;;              *************  ************************************
;;               ** ******     ************************************
;;                              ************************************
;;                               *********************************  *
;;                               ********************************
;;                               *   ****************************
;;                                   ***********************  ***
;;                                        *************** *
;;                                              *****
;;                                             ********
;;                                             ****** *
;;                                                **




;; == Roadmap ========================================================

;; Keeping in mind the objectives (explore kons-9, play with lisp,
;; have fun exploring fractals) here is an ambitious roadmap.

;; 1. Use shaders for better visuals and in which case processing
;;    won't be an issue anymore. We can then try and keep zooming in
;;    to see recursive patterns.

;; 2. Attempt a 3D fractal such as the MandelBulb. This will be
;;    computational intesive, and maybe makes sense to implement in
;;    the shader code but only if there's a lispy way to do this
;;    because exploring lisp is one of the demo's objectives.

;; 3. As an exercise in parallel processing with lisp, use multiple
;;    CPUs to speed processing especially for a higher density of
;;    points and higher iterations (this is for the approach without
;;    shaders)
