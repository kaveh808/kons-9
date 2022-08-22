(in-package #:kons-9)

;;;; point ==============================================================

;;; class for representing 3D points
;;; change to more efficient representation later
(defclass point ()
  ((x :accessor x :initarg :x :initform 0.0)
   (y :accessor y :initarg :y :initform 0.0)
   (z :accessor z :initarg :z :initform 0.0)))

(defmethod (setf x) (val (self point))
  (setf (slot-value self 'x) (coerce val 'single-float)))

(defmethod (setf y) (val (self point))
  (setf (slot-value self 'y) (coerce val 'single-float)))

(defmethod (setf z) (val (self point))
  (setf (slot-value self 'z) (coerce val 'single-float)))

(defmethod print-object ((self point) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "[~a, ~a, ~a]" (x self) (y self) (z self))))

(defun p! (x y z)
  (make-instance 'point :x (coerce x 'single-float)
			:y (coerce y 'single-float)
			:z (coerce z 'single-float)))

(defun point->list (p)
  (list (x p) (y p) (z p)))

(defun point->array (p)
  (vector (x p) (y p) (z p)))

;;; required for defconstant to work...
(defmethod make-load-form ((p point) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots p))

(defconstant +origin+ (p! 0 0 0))
(defconstant +x-axis+ (p! 1 0 0))
(defconstant +y-axis+ (p! 0 1 0))
(defconstant +z-axis+ (p! 0 0 1))

(defmethod p-set! ((self point) x y z)
  (setf (x self) x)
  (setf (y self) y)
  (setf (z self) z))

(defun p-copy (p)
  (p! (x p) (y p) (z p)))

(defun copy-points (points)
  (mapcar #'p-copy points))

(defmethod p+ ((p1 point) (p2 point))
  (p! (+ (x p1) (x p2))
      (+ (y p1) (y p2))
      (+ (z p1) (z p2))))

(defmethod p- ((p1 point) (p2 point))
  (p! (- (x p1) (x p2))
      (- (y p1) (y p2))
      (- (z p1) (z p2))))

(defmethod p* ((p1 point) (n number))
  (p! (* (x p1) n)
      (* (y p1) n)
      (* (z p1) n)))

(defmethod p* ((p1 point) (p2 point))
  (p! (* (x p1) (x p2))
      (* (y p1) (y p2))
      (* (z p1) (z p2))))

(defmethod p/ ((p1 point) (n number))
  (p! (/ (x p1) n)
      (/ (y p1) n)
      (/ (z p1) n)))

(defmethod p/ ((p1 point) (p2 point))
  (p! (/ (x p1) (x p2))
      (/ (y p1) (y p2))
      (/ (z p1) (z p2))))

(defmethod p= ((p1 point) (p2 point) &optional (epsilon single-float-epsilon))
  (and (< (abs (- (x p1) (x p2))) epsilon)
       (< (abs (- (y p1) (y p2))) epsilon)
       (< (abs (- (z p1) (z p2))) epsilon)))

(defmethod p-scale ((p point) factor)
  (p! (* (x p) factor)
      (* (y p) factor)
      (* (z p) factor)))

(defmethod p-negate ((p point))
  (p! (- (x p))
      (- (y p))
      (- (z p))))

(defmethod p-from-to ((p1 point) (p2 point))
  (p- p2 p1))

(defmethod p-lerp (f (p1 point) (p2 point))
  (p! (lerp f (x p1) (x p2))
      (lerp f (y p1) (y p2))
      (lerp f (z p1) (z p2))))

(defmethod p-smooth-lerp (f (p1 point) (p2 point))
  (labels ((cubic (x) (+ (* -2.0 (* x x x)) (* 3.0 (* x x)))))
    (p-lerp (cubic f) p1 p2)))

(defmethod p-min ((p1 point) (p2 point))
  (p! (min (x p1) (x p2))
      (min (y p1) (y p2))
      (min (z p1) (z p2))))

(defmethod p-max ((p1 point) (p2 point))
  (p! (max (x p1) (x p2))
      (max (y p1) (y p2))
      (max (z p1) (z p2))))

(defmethod p-rand2 ((p1 point) (p2 point))
  (p! (rand2 (x p1) (x p2))
      (rand2 (y p1) (y p2))
      (rand2 (z p1) (z p2))))

(defmethod p-rand2 ((n1 number) (n2 number))
  (p! (rand2 n1 n2)
      (rand2 n1 n2)
      (rand2 n1 n2)))

(defmethod p-rand (&optional (mag 1.0))
  (p-scale (p-normalize (p! (rand1 1.0) (rand1 1.0) (rand1 1.0))) mag))

(defmethod p-rand1 ((p point) &optional (pivot (p! 0 0 0)))
  (p+ pivot (p-rand2 (p-negate p) p)))

;;; magnitude (length) of point
(defun p-mag (p)
  (sqrt (+ (* (x p) (x p)) (* (y p) (y p)) (* (z p) (z p)))))

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

(defun p-jitter (p amount)
  (p+ p (p! (rand1 amount) (rand1 amount) (rand1 amount))))

(defun p-midpoint (p1 p2)
  (p-scale (p+ p1 p2) 0.5))

(defun p-center (points)
  (let ((n (length points)))
    (p* (reduce #'p+ points) (p! (/ 1.0 n) (/ 1.0 n) (/ 1.0 n)))))

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
  "Angle of the rotation bringing the oriented line associated to P1 on the P2 one.
The result is a value expressed in radians, between -PI (exclusive) and PI (inclusive).

The computation is in the X-Y plane, the Z coordinate is completely omitted. (Orthogonal
projection.)

The oriented line associated to a point P is the line passing through the origin and P,
so that the abscissae of P is a positive real number."
  ;; Sketch of proof for the formula used:
  ;;  Let Q be the intersection point of the line OP2 with the line normal
  ;;  to OP1 going through P1. We need to compute (atan |P1Q| |OP1|) (using Lisp
  ;;  notation for Arctan). Solve Q using the determinant and use homogeneity for atan.
  (let ((x1 (x p1))
	(y1 (y p1))
	(x2 (x p2))
	(y2 (y p2)))
    (atan (- (* x1 y2) (* x2 y1)) (+ (* x1 x2) (* y1 y2)))))
	    

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
  (let ((bounds-lo (p-copy (first points)))
	(bounds-hi (p-copy (first points))))
    (dolist (p points)
      (when (> (x p) (x bounds-hi)) (setf (x bounds-hi) (x p)))
      (when (> (y p) (y bounds-hi)) (setf (y bounds-hi) (y p)))
      (when (> (z p) (z bounds-hi)) (setf (z bounds-hi) (z p)))
      (when (< (x p) (x bounds-lo)) (setf (x bounds-lo) (x p)))
      (when (< (y p) (y bounds-lo)) (setf (y bounds-lo) (y p)))
      (when (< (z p) (z bounds-lo)) (setf (z bounds-lo) (z p))))
    (values bounds-lo bounds-hi)))

(defun p-sphericize (p radius &optional (factor 1.0) (center +origin+))
  (p-lerp factor p (p* (p-normalize (p-from-to center p)) radius)))
