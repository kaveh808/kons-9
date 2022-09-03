(in-package #:kons-9)

;;; https://github.com/nikodemus/sb-cga/blob/master/types.lisp
;;; MATRIX TYPE
;; (deftype matrix ()
;;   "4x4 matrix of single floats, represented as a one-dimensional vector stored
;; in column-major order."
;;   `(simple-array single-float (16)))

#|
Tests

defclass
 (time (dotimes (i 10000000) (setf (x (p! 1 2 3)) 4)))
Evaluation took:
  0.352 seconds of real time
  0.308026 seconds of total run time (0.300775 user, 0.007251 system)
  [ Run times consist of 0.022 seconds GC time, and 0.287 seconds non-GC time. ]
  87.50% CPU
  21 lambdas converted
  1,167,201,276 processor cycles
  640,578,160 bytes consed
  
deftype simple-array
 (time (dotimes (i 10000000) (set-x! (p! 1 2 3) 4)))
Evaluation took:
  0.109 seconds of real time
  0.085133 seconds of total run time (0.082100 user, 0.003033 system)
  [ Run times consist of 0.011 seconds GC time, and 0.075 seconds non-GC time. ]
  77.98% CPU
  360,991,386 processor cycles
  319,979,296 bytes consed

declarations
(declaim (ftype (function (point number) single-float) set-x!))
(declaim (ftype (function (point number) single-float) set-y!))
(declaim (ftype (function (point number) single-float) set-z!))
(declaim (ftype (function (number number number) point) p!))
(declaim (ftype (function (point) list) point->list))
(declaim (inline x y z set-x! set-y! set-z! p!))
(declaim (optimize (speed 3) (safety 0)))


|#


;;;; point ==============================================================

;;; class for representing 3D points
(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype point ()
    `(simple-array single-float (4)))
)

(defun x (p) (aref p 0))
(defun y (p) (aref p 1))
(defun z (p) (aref p 2))

(defun set-x! (p val) (setf (aref p 0) (coerce val 'single-float)))
(defun set-y! (p val) (setf (aref p 1) (coerce val 'single-float)))
(defun set-z! (p val) (setf (aref p 2) (coerce val 'single-float)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun p! (x y z)
    (make-array 4 :element-type 'single-float :initial-contents (list (coerce x 'single-float)
                                                                      (coerce y 'single-float)
                                                                      (coerce z 'single-float)
                                                                      1.0f0)))
  )

(defun point->list (p)
  (list (x p) (y p) (z p)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +origin+ (p! 0 0 0))
  (defparameter +x-axis+ (p! 1 0 0))
  (defparameter +y-axis+ (p! 0 1 0))
  (defparameter +z-axis+ (p! 0 0 1)))

(defun p-set! (p x y z)
  (set-x! p x)
  (set-y! p y)
  (set-z! p z))

(defun p-copy (p)
  (p! (x p) (y p) (z p)))

(defun copy-points (points)
  (mapcar #'p-copy points))

(defmacro def-point-func-1 (op)
  `(defun ,(concat-syms 'p op) (p val)
     (ctypecase val
       (point (p! (,op (x p) (x val))
                  (,op (y p) (y val))
                  (,op (z p) (z val))))
       (number (p! (,op (x p) val)
                   (,op (y p) val)
                   (,op (z p) val))))))

;(macroexpand-1 '(def-point-func-1 +))

(def-point-func-1 +)
(def-point-func-1 -)
(def-point-func-1 *)
(def-point-func-1 /)

(defun p= (p1 p2 &optional (epsilon single-float-epsilon))
  (and (< (abs (- (x p1) (x p2))) epsilon)
       (< (abs (- (y p1) (y p2))) epsilon)
       (< (abs (- (z p1) (z p2))) epsilon)))

(defun p-negate (p)
  (p! (- (x p))
      (- (y p))
      (- (z p))))

(defun p-from-to (p1 p2)
  (p- p2 p1))

(defun p-lerp (f p1 p2)
  (p! (lerp f (x p1) (x p2))
      (lerp f (y p1) (y p2))
      (lerp f (z p1) (z p2))))

(defun p-smooth-lerp (f p1 p2)
  (labels ((cubic (x) (+ (* -2.0 (* x x x)) (* 3.0 (* x x)))))
    (p-lerp (cubic f) p1 p2)))

(defun p-min (p1 p2)
  (p! (min (x p1) (x p2))
      (min (y p1) (y p2))
      (min (z p1) (z p2))))

(defun p-max (p1 p2)
  (p! (max (x p1) (x p2))
      (max (y p1) (y p2))
      (max (z p1) (z p2))))

(defun p-rand-2 (val1 val2)
  (ctypecase val1
    (point (p! (rand2 (x val1) (x val2))
               (rand2 (y val1) (y val2))
               (rand2 (z val1) (z val2))))
    (number (p! (rand2 (x val1) val2)
                (rand2 (y val1) val2)
                (rand2 (z val1) val2)))))

(defun p-rand (&optional (mag 1.0))
  (p* (p-normalize (p! (rand1 1.0) (rand1 1.0) (rand1 1.0))) mag))

(defun p-rand1 (p &optional (pivot (p! 0 0 0)))
  (p+ pivot (p-rand2 (p-negate p) p)))

;;; magnitude (length) of point
(defun p-mag (p)
  (sqrt (+ (* (x p) (x p)) (* (y p) (y p)) (* (z p) (z p)))))

(defun p-mag-squared (p)
  (+ (* (x p) (x p)) (* (y p) (y p)) (* (z p) (z p))))

(defun p-normalize (p)
  (let ((mag (p-mag p)))
    (if (= mag 0.0)
	(p! 0.0 0.0 0.0)
	(p! (/ (x p) mag) (/ (y p) mag) (/ (z p) mag)))))

;;; distance between two points
(defun p-dist (p1 p2)
  (sqrt (+ (expt (- (x p2) (x p1)) 2)
	   (expt (- (y p2) (y p1)) 2)
	   (expt (- (z p2) (z p1)) 2))))

(defun p-dist-squared (p1 p2)
  (+ (expt (- (x p2) (x p1)) 2)
     (expt (- (y p2) (y p1)) 2)
     (expt (- (z p2) (z p1)) 2)))

(defun p-jitter (p amount)
  (p+ p (p! (rand1 amount) (rand1 amount) (rand1 amount))))

(defun p-midpoint (p1 p2)
  (p* (p+ p1 p2) 0.5))

(defun p-center (points)
  (p/ (reduce #'p+ points) (length points)))

(defun p-average (&rest points)
  (p-center points))

(defun p-dot (p1 p2)
  (+ (* (x p1) (x p2)) (* (y p1) (y p2)) (* (z p1) (z p2))))

(defun p-cross (p1 p2)
  (let ((x1 (x p1)) (y1 (y p1)) (z1 (z p1))
        (x2 (x p2)) (y2 (y p2)) (z2 (z p2)))
    (p! (- (* y1 z2) (* y2 z1))
	(- (* x2 z1) (* x1 z2))
	(- (* x1 y2) (* x2 y1)))))

(defun p-angle-cosine (p1 p2)
  (p-dot (p-normalize p1) (p-normalize p2)))

(defun p-angle-sine (p1 p2)
  (let ((c (p-angle-cosine p1 p2)))
    (sqrt (- 1 (* c c)))))

(defun p-angle (p1 p2)
  (acos (max -1.0 (min 1.0 (p-dot (p-normalize p1) (p-normalize p2))))))

(defun p-z-alignment-angles (point)
  (let* ((x (x point))
         (z (z point))
         (x-angle (- (/ pi 2) (p-angle point +y-axis+)))
         (y-angle (if (and (= x 0) (= z 0))
                    0.0
                    (acos (/ z (sqrt (+ (* x x) (* z z))))))))
    (values y-angle x-angle)))

(defun angle-2d (p1 p2)
  (let* ((theta1 (atan (y p1) (x p1)))
	 (theta2 (atan (y p2) (x p2)))
	 (dtheta (- theta2 theta1)))
    (loop while (> dtheta pi)
	  do (setf dtheta (- dtheta (* 2 pi))))
    (loop while (< dtheta (- pi))
	  do (setf dtheta (+ dtheta (* 2 pi))))
    dtheta))

(defun p-radians (p)
  (p! (radians (x p)) (radians (y p)) (radians (z p))))

(defun point-in-polygon? (p points)
  (let ((angle 0.0))
    (loop :for (p1 p2) :on (append points (list (first points))) :by #'cdr :while p2
	  :do (setf angle (+ angle (angle-2d (p- p1 p) (p- p2 p)))))
    (if (< (abs angle) pi)
	nil
	t)))

(defun points-bounds (points)
  (let ((bounds-lo (p-copy (aref points 0)))
	(bounds-hi (p-copy (aref points 0))))
    (dotimes (i (length points))
      (let ((p (aref points i)))
        (when (> (x p) (x bounds-hi)) (set-x! bounds-hi (x p)))
        (when (> (y p) (y bounds-hi)) (set-y! bounds-hi (y p)))
        (when (> (z p) (z bounds-hi)) (set-z! bounds-hi (z p)))
        (when (< (x p) (x bounds-lo)) (set-x! bounds-lo (x p)))
        (when (< (y p) (y bounds-lo)) (set-y! bounds-lo (y p)))
        (when (< (z p) (z bounds-lo)) (set-z! bounds-lo (z p)))))
    (values bounds-lo bounds-hi)))

(defun p-sphericize (p radius &optional (factor 1.0) (center +origin+))
  (p-lerp factor p (p* (p-normalize (p-from-to center p)) radius)))
