(in-package #:kons-9/testsuite)

;; Scene items validate by default.
(define-testcase validate/valid ()
  (assert-t* (kons-9::validate (make-instance 'kons-9::scene-item)))
  (assert-condition (kons-9::validate 42) error))

;;; point cloud ----------------------------------------------------------------

(define-testcase validate/valid ()
  (loop for item in (valid-scene-items)
        do (assert-t (progn (kons-9::validate item) t))))

(defun valid-scene-items ()
  "List of scene items that are expected to validate successfully."
  ;; Just a few plucked from the demos.
  (list (make-instance 'kons-9::scene-item)
        ;; Point clouds (note: many other objects are point-clouds by inheritance too)
        (make-instance 'kons-9::point-cloud
                       :points (kons-9::make-line-points (kons-9::p! 0 0 0) (kons-9::p! 1 1 1) 100))
        (make-instance 'kons-9::point-cloud
                       :points (kons-9::make-rectangle-points 1 1 10))
        ;; Curves
        (kons-9::make-line-curve (kons-9::p! 0 0 0) (kons-9::p! 2 2 2) 8)
        (kons-9::make-rectangle-curve 2 1 4)
        (kons-9::make-square-curve 1.5)
        (kons-9::make-circle-curve 2.0 16)
        (kons-9::make-arc-curve 2.0 0 90 16)
        (kons-9::make-sine-curve-curve 360 1 2 1 16)
        (kons-9::make-spiral-curve .2 2.0 -1.0 4 64)
        ;; Polyhedrons
        (kons-9::make-tetrahedron  2.0)
        (kons-9::make-cube         2.0)
        (kons-9::make-octahedron   2.0)
        (kons-9::make-dodecahedron 2.0)
        (kons-9::make-icosahedron  2.0)))

(define-testcase validate/invalid ()
  (let ((origin (kons-9::p! 0 0 0))
        (black #(0.0f0 0.0f0 0.0f0 1.0f0)))
    (flet ((errors (item)
             (kons-9::count-validation-errors item)))
      ;; Safety low so that we can pass wrong-typed initargs to MAKE-INSTANCE.
      (declare (optimize (safety 0) (speed 0) (debug 3)))
      ;; point-clouds POINTS must be a vector
      (assert-eq 1 (errors (make-instance 'kons-9::point-cloud :points ())))
      ;; ... of points.
      (assert-eq 3 (errors (make-instance 'kons-9::point-cloud :points (vector black t :foo))))

      ;; point-cloud POINT-COLORS must be a vector
      (assert-eq 1 (errors (make-instance 'kons-9::point-cloud :points (vector origin) :point-colors :non-list)))
      ;; ... of the same length as POINTS
      (assert-eq 1 (errors (make-instance 'kons-9::point-cloud :points (vector origin) :point-colors #())))
      ;; .. containing colors.
      (assert-eq 2 (errors (make-instance 'kons-9::point-cloud :points (vector origin origin origin)
                                                               :point-colors (vector black origin :bar))))

      ;; Curves require at least two points.
      (assert-eq 1 (errors (make-instance 'kons-9::curve :points #())))
      (assert-eq 1 (errors (make-instance 'kons-9::curve :points (vector (kons-9::p! 0 0 0)))))

      ;; Polyhedrons aren't valid if empty.
      (assert-condition (kons-9::validate (make-instance 'kons-9::polyhedron)) kons-9::validation-error)

      ;; Validation doesn't work on objects that aren't scene-items.
      (assert-condition (validate :non-item) error))))

;;; point cloud ----------------------------------------------------------------
