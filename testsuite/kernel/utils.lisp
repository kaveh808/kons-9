(in-package #:kons-9/testsuite)

(define-testcase exercise-strcat ()
  (assert-string= "" (kons-9::strcat))
  (assert-string= "KONS-9" (kons-9::strcat "KONS" "-" "9")))

(define-testcase exercise-concat-syms ()
  (assert-eq 'nil (kons-9::concat-syms 'n 'i 'l))
  (assert-eq (intern "KONS-9/TESTSUITE-X123")
	     (kons-9::concat-syms 'kons-9 '/ 'testsuite '- 'x123)))

(define-testcase exercise-make-keyword ()
  (assert-eq :kons-9 (kons-9::make-keyword "kons-9")))

(define-testcase exercise-flatten-list ()
  (assert-list-equal '(1 2 3 4 5 6 7 8)
		     (kons-9::flatten-list
		      '(1 (2 3) (4 (5 (6))) ((((7) 8))))))
  (assert-list-equal '() (kons-9::flatten-list '((((())))))))

(define-testcase exercise-angle-measurement-conversions ()
  (assert-float-is-essentially-equal
   pi
   (kons-9::radians (kons-9::degrees pi)))
  (assert-float-is-essentially-equal
   (coerce 180 'float)
   (coerce (kons-9::degrees (kons-9::radians 180)) 'float))
  (assert-float-is-essentially-equal
   kons-9::pi/2
   (kons-9::radians 90)))

(define-testcase exercise-lerp ()
  (assert-float-is-essentially-equal -1.0 (kons-9::lerp 0.0 -1.0 1.0))
  (assert-float-is-essentially-equal  0.0 (kons-9::lerp 0.5 -1.0 1.0))
  (assert-float-is-essentially-equal  1.0 (kons-9::lerp 1.0 -1.0 1.0)))

(define-testcase exercise-tween ()
  (assert-float-is-essentially-equal  0.0 (kons-9::tween -1.0 -1.0 1.0))
  (assert-float-is-essentially-equal  0.5 (kons-9::tween 0.0 -1.0 1.0))
  (assert-float-is-essentially-equal  1.0 (kons-9::tween 1.0 -1.0 1.0)))

(define-testcase exercise-clamp ()
  (assert-float-is-essentially-equal  0.0 (kons-9::clamp -1.0 0.0 1.0))
  (assert-float-is-essentially-equal  0.5 (kons-9::clamp 0.5 0.0 1.0))
  (assert-float-is-essentially-equal  1.0 (kons-9::clamp 2.0 0.0 1.0)))

(define-testcase testsuite-utils ()
  (exercise-strcat)
  (exercise-concat-syms)
  (exercise-make-keyword)
  (exercise-flatten-list)
  (exercise-angle-measurement-conversions)
  (exercise-lerp)
  (exercise-tween)
  (exercise-clamp))
