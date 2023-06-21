(in-package #:kons-9/testsuite)

(define-testcase exercise-ray-traingle-intersect ()
  (flet ((vec (a b c)
           (origin.vec3:vec (coerce a 'single-float)
                            (coerce b 'single-float)
                            (coerce c 'single-float)))
         (mk-ray (from to)
             (origin.geometry.ray:ray :origin from :direction to)))
    (let ((triangle (origin.geometry.triangle:triangle
                     (vec -3.5 0 -4.5)
                     (vec -4 1 -4.0)
                     (vec -4.5 0 -3.5))))
      (flet ((intersect (from to)
               (kons-9::intersect/triangle triangle (mk-ray from to))))

      ;;; Try intersections With default camera position
        ;; top vertex
        (assert-float-is-essentially-equal ;; inside triangle
         15.355117
         (intersect (vec 5.540323 2.5881906 7.912401)
                    (vec -0.6207694 -0.10930262 -0.7763364)))
        (assert-eq ;; outside triangle
         nil
         (intersect (vec 5.540323 2.5881906 7.912401)
                    (vec -0.619678 -0.10301772 -0.7780659)))
        ;; left vertex
        (assert-float-is-essentially-equal ;; inside triangle
         15.416862
         (intersect (vec 5.540323 2.5881906 7.912401)
                    (vec -0.6404545 -0.16041444 -0.7510561)))
        (assert-eq ;; outside triangle
         nil
         (intersect (vec 5.540323 2.5881906 7.912401)
                    (vec -0.6561073 -0.16276513 -0.73690623)))
        ;; right vertex
        (assert-float-is-essentially-equal ;; inside triangle
         15.5198345
         (intersect (vec 5.540323 2.5881906 7.912401)
                    (vec -0.59513503 -0.1619278 -0.7871427)))
        (assert-eq ;; outside triangle
         nil
         (intersect (vec 5.540323 2.5881906 7.912401)
                    (vec -0.5779269 -0.1710878 -0.79795325)))

      ;;; Retry intersections after zoomin in (closer to triangle)
        ;; top vertex
        (assert-float-is-essentially-equal ;; inside triangle
         1.7190325
         (intersect (vec -3.2982311 0.5 -2.5083206)
                    (vec -0.4080841 0.28324336 -0.86789435)))
        (assert-eq ;; outside triangle
         nil
         (intersect (vec -3.2982311 0.5 -2.5083206)
                    (vec -0.4061101 0.29344934 -0.8654259)))
        ;; left vertex
        (assert-float-is-essentially-equal ;; inside triangle
         1.6304653
         (intersect (vec -3.2982311 0.5 -2.5083206)
                    (vec -0.72773695 -0.29837447 -0.6175529)))
        (assert-eq ;; outside triangle
         nil
         (intersect (vec -3.2982311 0.5 -2.5083206)
                    (vec -0.73710036 -0.3065231 -0.6022679)))
        ;; right vertex
        (assert-float-is-essentially-equal ;; inside triangle
         2.0412412
         (intersect (vec -3.2982311 0.5 -2.5083206)
                    (vec -0.10965764 -0.23859437 -0.9649082)))
        (assert-eq ;; outside triangle
         nil
         (intersect (vec -3.2982311 0.5 -2.5083206)
                    (vec -0.09089148 -0.24464947 -0.9653421)))
        ))))


(define-testcase testsuite-ray-triangle ()
  (exercise-ray-traingle-intersect))
