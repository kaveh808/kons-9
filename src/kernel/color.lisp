(in-package #:kons-9)

;;;; color ==============================================================

(deftype color ()
  "Mutable vector of R,G,B,A single-float values between zero and one."
  '(vector * 4))

(defun color? (x)
  (and (typep x 'color)
       (every (lambda (x) (typep x '(single-float 0.0f0 1.0f0))) x)))

;;; we define colors as simple vectors
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun c! (r g b &optional (a 1.0))
    (vector (coerce r 'single-float)
	    (coerce g 'single-float)
	    (coerce b 'single-float)
	    (coerce a 'single-float)))
  )

(defun c-red   (c) (aref c 0))
(defun c-green (c) (aref c 1))
(defun c-blue  (c) (aref c 2))
(defun c-alpha (c) (aref c 3))
(defun c-set-rgb (c1 c2)
  (setf (aref c1 0) (aref c2 0))
  (setf (aref c1 1) (aref c2 1))
  (setf (aref c1 2) (aref c2 2)))
(defun c-set-alpha (c alpha)
  (setf (aref c 3) alpha))

#+nil
(defun c-lerp (f c1 c2)
  (map 'array #'(lambda (a b) (lerp f a b)) c1 c2))

(defun c-lerp (f c1 c2)
  (map 'vector #'(lambda (a b) (lerp f a b)) c1 c2))

(defun c-rand ()
  (c! (rand2 0.0 1.0) (rand2 0.0 1.0) (rand2 0.0 1.0)))

(defun c-rand-with-alpha ()
  (c! (rand2 0.0 1.0) (rand2 0.0 1.0) (rand2 0.0 1.0)  (rand2 0.0 1.0)))

(defun c-rand2 (c1 c2)
  (c-lerp (rand2 0 1) c1 c2))

#+nil
(defun c+ (c1 c2)
  (map 'array #'(lambda (a b) (+ a b)) c1 c2))

(defun c+ (c1 c2)
  (map 'vector #'(lambda (a b) (+ a b)) c1 c2))

#+nil
(defun c-scale (c1 x)
  (map 'array #'(lambda (a) (* a x)) c1))

(defun c-scale (c1 x)
  (map 'vector #'(lambda (a) (* a x)) c1))

#+nil
(defun c-jitter (c c-delta)
  (c+ c (map 'array #'(lambda (a) (rand1 a)) c-delta)))

(defun c-jitter (c c-delta)
  (c+ c (map 'vector #'(lambda (a) (rand1 a)) c-delta)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun c-255! (r g b &optional (a 255))
    (c! (/ r 255.0) (/ g 255.0) (/ b 255.0) (/ a 255.0)))

  #+nil ;;work around defconstant issues on sbcl
  (defconstant +rainbow+
    (vector (c-255! 255 0 0) (c-255! 255 127 0) (c-255! 255 255 0) (c-255! 0 255 0)
            (c-255! 0 0 255) (c-255! 75 0 130) (c-255! 148 0 211)))

  (defparameter +rainbow+
    (vector (c-255! 255 0 0) (c-255! 255 127 0) (c-255! 255 255 0) (c-255! 0 255 0)
            (c-255! 0 0 255) (c-255! 75 0 130) (c-255! 148 0 211))))

(defun c-rainbow (f)
  (let ((rainbow-value (* f 6.0)))
    (multiple-value-bind (i frac)
        (floor rainbow-value)
      (c-lerp frac (aref +rainbow+ i) (aref +rainbow+ (min (1+ i) 6))))))
