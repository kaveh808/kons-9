(in-package #:kons-9/testsuite)

(define-testcase exercise-point-cloud/get-bounds/validate (point-cloud &key expected-bounds-lo expected-bounds-hi)
  "Validate that GET-BOUNDS on POINT-CLOUD returns the expected values."
  (assert-eq 2 (length (multiple-value-list (kons-9::get-bounds point-cloud))))
  (multiple-value-bind (bounds-lo bounds-hi)
      (kons-9::get-bounds point-cloud)
    (when expected-bounds-lo
      (assert-point-is-essentially-equal expected-bounds-lo bounds-lo))
    (when expected-bounds-hi
      (assert-point-is-essentially-equal expected-bounds-hi bounds-hi))))

(define-testcase exercise-point-cloud/get-bounds ()
  "Exercise the GET-BOUNDS method on various POINT-CLOUD values."
  (let* ((point1
	   (kons-9::p! 21 -1.41 100))
	 (point2
	   (kons-9::p! 100 -100 100))
	 (point3
	   (kons-9::p! -100 100 -100))
	 (expected-bounds-hi
	   (kons-9::p! 100 100 100))
	 (expected-bounds-lo
	   (kons-9::p! -100 -100 -100))
	 (empty-cloud
	   (kons-9::make-point-cloud nil))
	 (cloud-with-only-one-not-too-specific-point
	   (kons-9::make-point-cloud (make-array 1 :initial-contents (list point1))))
	 (cloud-with-several-points
	   (kons-9::make-point-cloud (make-array 3 :initial-contents (list point1 point2 point3)))))
    (assert-condition (kons-9::get-bounds empty-cloud) warning)
    (validate-point-cloud/get-bounds/validate cloud-with-only-one-not-too-specific-point
					      :expected-bounds-lo point1
					      :expected-bounds-hi point1)
    (validate-point-cloud/get-bounds/validate cloud-with-several-points
					      :expected-bounds-lo expected-bounds-lo
					      :expected-bounds-hi expected-bounds-hi)))

(define-testcase exercise-make-line-points/validate (p1 p2 num-segments)
  "Validate the output of MAKE-LINE-POINTS against expected invariants."
  (let ((points
	  (kons-9::make-line-points p1 p2 num-segments)))
    (assert-eq (1+ num-segments) (length points))
    (assert-point-is-essentially-equal p1 (aref points 0))
    (assert-point-is-essentially-equal p2 (aref points num-segments))))

(define-testcase exercise-make-line-points ()
  "Exercise MAKE-LINE-POINTS on various input parameters."
  (exercise-make-line-points/validate (kons-9::p! 0.0 0.0 0.0) (kons-9::p! 1.0 0.0 0.0) 1)
  (exercise-make-line-points/validate (kons-9::p! 0.0 1.0 0.0) (kons-9::p! 1.0 0.0 0.0) 10)
  (exercise-make-line-points/validate (kons-9::p! 0.0 0.0 -10.0) (kons-9::p! 1.0 0.0 0.0) 100))

(define-testcase exercise-make-rectangle-points/validate (p0 p1 p2 p3 num-segments)
  "Validate the output of MAKE-RECTANGLE-POINTS against expected invariants.

Points P0, P1, P2 and P3 must lie in the Z = 0 plane, the line from P0 to P1
must be parallel to the Y-axis and the line from P1 to P2 must be parallel
to the X-axis, and the corresponding rectangle must be centrered at 0."
  (let* ((width
	   (abs (- (p:x p0) (p:x p1))))
	 (height
	   (abs (- (p:y p0) (p:y p3))))
	 (points
	   (kons-9::make-rectangle-points width height num-segments)))
    (assert-eq (* 4 num-segments) (length points))
    (assert-point-is-essentially-equal p0 (aref points 0))
    (assert-point-is-essentially-equal p1 (aref points num-segments))
    (assert-point-is-essentially-equal p2 (aref points (* 2 num-segments)))
    (assert-point-is-essentially-equal p3 (aref points (* 3 num-segments)))))

(define-testcase exercise-make-rectangle-points ()
  "Exercise MAKE-RECTANGLE-POINTS on various input paramters."
  (exercise-make-rectangle-points/validate
   (kons-9::p!  1.0  2.0 0.0)
   (kons-9::p! -1.0  2.0 0.0)
   (kons-9::p! -1.0 -2.0 0.0)
   (kons-9::p!  1.0 -2.0 0.0)
   1)
  (exercise-make-rectangle-points/validate
   (kons-9::p!  1.0  2.0 0.0)
   (kons-9::p! -1.0  2.0 0.0)
   (kons-9::p! -1.0 -2.0 0.0)
   (kons-9::p!  1.0 -2.0 0.0)
   3)
  (exercise-make-rectangle-points/validate
   (kons-9::p!  1.0  2.0 0.0)
   (kons-9::p! -1.0  2.0 0.0)
   (kons-9::p! -1.0 -2.0 0.0)
   (kons-9::p!  1.0 -2.0 0.0)
   7))

(define-testcase testsuite-point-cloud ()
  (exercise-make-line-points)
  (exercise-make-rectangle-points))
