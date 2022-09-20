(in-package #:kons-9)

;;;; point ==============================================================

(declaim (inline p! set-x! set-y! set-z! p+ p- p/ p*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun p! (x y z)
    (p:vec (coerce x 'single-float) (coerce y 'single-float) (coerce z 'single-float))))

(defun point->list (p)
  (list (p:x p) (p:y p) (p:z p)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +origin+ (p! 0 0 0))
  (defparameter +x-axis+ (p! 1 0 0))
  (defparameter +y-axis+ (p! 0 1 0))
  (defparameter +z-axis+ (p! 0 0 1)))

(defun p-set! (p x y z)
  (setf (p:x p) (coerce x 'single-float) 
        (p:y p) (coerce y 'single-float)
        (p:z p) (coerce z 'single-float)))

(defun copy-points (points)
  (mapcar #'p:copy points))

(defmacro def-point-func-1 (op)
  `(defun ,(concat-syms 'p op) (p val)
     (ctypecase val
       (p:vec (,(find-symbol (symbol-name op) 'p) p val))
       (number (p! (,op (p:x p) val)
                   (,op (p:y p) val)
                   (,op (p:z p) val))))))

(def-point-func-1 +)
(def-point-func-1 -)
(def-point-func-1 *)
(def-point-func-1 /)


(defun p-from-to (p1 p2)
  (p:- p2 p1))

(defun p-smooth-lerp (f p1 p2)
  (labels ((cubic (x) (+ (* -2.0 (* x x x)) (* 3.0 (* x x)))))
    (p:lerp p1 p2 (cubic f))))


(defun p-rand2 (val1 val2)
  (ctypecase val2
    (p:vec  (p! (rand2 (p:x val1) (p:x val2))
                (rand2 (p:y val1) (p:y val2))
                (rand2 (p:z val1) (p:z val2))))
    (number (p! (rand2 (p:x val1) val2)
                (rand2 (p:y val1) val2)
                (rand2 (p:z val1) val2)))))

(defun p-rand (&optional (mag 1.0))
  (p:scale (p:normalize (p! (rand1 1.0) (rand1 1.0) (rand1 1.0))) mag))

(defun p-rand1 (p &optional (pivot (p! 0 0 0)))
  (p:+ pivot (p-rand2 (p:negate p) p)))

;;; distance between two points
(defun p-dist (p1 p2)
  (sqrt (+ (expt (- (p:x p2) (p:x p1)) 2)
	   (expt (- (p:y p2) (p:y p1)) 2)
	   (expt (- (p:z p2) (p:z p1)) 2))))

(defun p-dist-squared (p1 p2)
  (+ (expt (- (p:x p2) (p:x p1)) 2)
     (expt (- (p:y p2) (p:y p1)) 2)
     (expt (- (p:z p2) (p:z p1)) 2)))

(defun p-jitter (p amount)
  (p:+ p (p! (rand1 amount) (rand1 amount) (rand1 amount))))

(defun p-midpoint (p1 p2)
  (p:scale (p:+ p1 p2) 0.5))

(defun p-center (points)
  (p/ (reduce #'p+ points) (length points)))

(defun p-average (&rest points)
  (p-center points))


(defun p-angle-cosine (p1 p2)
  (p:dot (p:normalize p1) (p:normalize p2)))

(defun p-angle-sine (p1 p2)
  (let ((c (p-angle-cosine p1 p2)))
    (sqrt (- 1 (* c c)))))

(defun p-angle (p1 p2)
  (acos (max -1.0 (min 1.0 (p:dot (p:normalize p1) (p:normalize p2))))))

(defun p-z-alignment-angles (point)
  (let* ((x (p:x point))
         (z (p:z point))
         (x-angle (- (/ pi 2) (p:angle point +y-axis+)))
         (y-angle (if (and (= x 0) (= z 0))
                    0.0
                    (acos (/ z (sqrt (+ (* x x) (* z z))))))))
    (values y-angle x-angle)))

(defun angle-2d (p1 p2)
  (let* ((theta1 (atan (p:y p1) (p:x p1)))
	 (theta2 (atan (p:y p2) (p:x p2)))
	 (dtheta (- theta2 theta1)))
    (loop while (> dtheta pi)
	  do (setf dtheta (- dtheta (* 2 pi))))
    (loop while (< dtheta (- pi))
	  do (setf dtheta (+ dtheta (* 2 pi))))
    dtheta))


(defun point-in-polygon? (p points)
  (let ((angle 0.0))
    (loop :for (p1 p2) :on (append points (list (first points))) :by #'cdr :while p2
	  :do (setf angle (+ angle (angle-2d (p- p1 p) (p- p2 p)))))
    (if (< (abs angle) pi)
	nil
	t)))

(defun points-bounds (points) ;nobody uses this, why did I fix it?
  (let ((bounds-lo (p:copy (first points)))
	(bounds-hi (p:copy (first points))))
    (dolist (p points)
      (p:min! bounds-lo bounds-lo p)
      (p:max! bounds-hi bounds-hi p))
    (values bounds-lo bounds-hi)))

(defun p-sphericize (p radius &optional (factor 1.0) (center +origin+))
  (p:lerp p (p* (p:normalize (p-from-to center p)) radius) factor))
