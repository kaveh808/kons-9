(in-package #:kons-9/testsuite)

(defun float-is-essentially-equal (float1 float2 &key (inaccuracy 0))
  (<= (abs (- float1 float2))
      (confidence::float-comparison-threshold 'single-float min inaccuracy float1 float2)))

(define-assertion assert-point-is-essentially-equal (point1 point2 &key (inaccuracy 0))
  "Asssert that POINT1 is essentially equal to POINT2.
This means that every coordinate of POINT1 is essentially equal to the corresponding
coordinate of POINT2.

Essential equality of floating numbers is a useful approximation for the equality
relation when computing with floating point numbers. Two floating point numbers are
essentially equal when they are in a neighbourhood whose size is based
on the magnitude orders of these floating point numbers and the inaccuracy."
  (:report
   (lambda (stream)
     (flet ((maybe-report (name float1 float2)
	      (unless (float-is-essentially-equal (p:y point1) (p:y point2) :inaccuracy inaccuracy)
		(format stream
			"~%~%The coordinate ~A for POINT1 and POINT2 are not essentially equal. The neighbourhood used
to compare essential equality of coordinate ~A of POINT1 and POINT2

  ~A  and  ~A

with an inaccuracy of ~A has size ~A."
            name name
	    float1 float2
	    inaccuracy (confidence::float-comparison-threshold 'single-float max inaccuracy float1 float2)))))
       (maybe-report "X" (p:x point1) (p:x point2))
       (maybe-report "Y" (p:y point1) (p:y point2))
       (maybe-report "Z" (p:z point1) (p:z point2)))))
  (and (float-is-essentially-equal (p:x point1) (p:x point2) :inaccuracy inaccuracy)
       (float-is-essentially-equal (p:y point1) (p:y point2) :inaccuracy inaccuracy)
       (float-is-essentially-equal (p:z point1) (p:z point2) :inaccuracy inaccuracy)))

