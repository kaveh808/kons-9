(in-package #:kons-9)

;;;; class hierarchy ====================================================

(defun print-class-hierarchy (class &optional (indent 0))
  (print-spaces indent)
  (format t "~a : ~a~%" (class-name class) (mapcar #'class-name (class-direct-superclasses class)))
  (let ((subclasses (class-direct-subclasses class)))
    (dolist (subclass subclasses)
      (print-class-hierarchy subclass (+ indent 2)))))

;;;; utils ==============================================================

(defun debug (x &optional (str ""))
  (format t "DEBUG: ~a~s~%" str x)
  x)

;;; concatenate strings
(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

;;; concatenate symbols
(defun concat-syms (&rest syms)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))

(defun print-spaces (num)
  (dotimes (i num)
    (princ " ")))

(defun indent-padding (num)
  (format nil "~v{~a~:*~}" num '(#\space)))

(defun array->list (array)
  (map 'list #'identity array))

(defun list->array (list)
  (map 'array #'identity list))

(defun wrap-list (list)
  (append list (list (first list))))

(defmacro doarray ((i obj array) &rest body)
  `(dotimes (,i (length ,array))
     (let ((,obj (aref ,array ,i)))
       ,@body)))

(defmacro doarray-if ((i obj test array) &rest body)
  `(dotimes (,i (length ,array))
     (let ((,obj (aref ,array ,i)))
       (when (funcall ,test ,obj)
         ,@body))))

;;;; math ===============================================================

(defvar 2pi (* 2 pi))                   ;should be defconsant?
(defvar pi/2 (/ pi 2))                  ;should be defconsant?

;;; linear interpolation
(defun lerp (f lo hi)
  (+ lo (* f (- hi lo))))

;;; compute [0-1] factor
(defun tween (val start end)
  (cond ((<= val start) 0.0)
	((>= val end) 1.0)
	(t (/ (- val start) (- end start)))))

;;; random float between a and b
(defun rand2 (a b)
  (if (= a b)				;doesn't like (random 0)
      a
      (let ((lo (min a b))
	    (hi (max a b))
	    (*random-state* (make-random-state t)))
	(+ lo (random (coerce (- hi lo) 'float))))))

;;; random float between -a and a
(defun rand1 (a &optional (pivot 0))
  (+ pivot (rand2 (- a) a)))

;;; from web
(defun normal-random (mean std-dev)
  "Normal random numbers, with the given mean & standard deviation."
  (do* ((rand-u (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-v (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-s (+ (* rand-u rand-u) (* rand-v rand-v))
                (+ (* rand-u rand-u) (* rand-v rand-v))))
    ((not (or (= 0 rand-s) (>= rand-s 1)))
     (+ mean
      (* std-dev
         (* rand-u (sqrt (/ (* -2.0 (log rand-s)) rand-s))))))))

(defun radians (angle)
  (* angle (/ pi 180)))

(defun degrees (angle)
  (* angle (/ 180 pi)))

(defun clamp (x lo hi)
  (max lo (min x hi)))

;;;; color ==============================================================

;;; we define colors as simple vectors
(defun c! (r g b &optional (a 1.0))
  (vector (coerce r 'single-float)
	  (coerce g 'single-float)
	  (coerce b 'single-float)
	  (coerce a 'single-float)))

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

(defun c-lerp (f c1 c2)
  (map 'array #'(lambda (a b) (lerp f a b)) c1 c2))

(defun c-rand ()
  (c! (rand2 0.0 1.0) (rand2 0.0 1.0) (rand2 0.0 1.0)))

(defun c-rand-with-alpha ()
  (c! (rand2 0.0 1.0) (rand2 0.0 1.0) (rand2 0.0 1.0)  (rand2 0.0 1.0)))

(defun c-rand2 (c1 c2)
  (c-lerp (rand2 0 1) c1 c2))

(defun c+ (c1 c2)
  (map 'array #'(lambda (a b) (+ a b)) c1 c2))

(defun c-scale (c1 x)
  (map 'array #'(lambda (a) (* a x)) c1))

(defun c-jitter (c c-delta)
  (c+ c (map 'array #'(lambda (a) (rand1 a)) c-delta)))

(defun c-255! (r g b &optional (a 255))
  (c! (/ r 255.0) (/ g 255.0) (/ b 255.0) (/ a 255.0)))

(defconstant +rainbow+
  (vector (c-255! 255 0 0) (c-255! 255 127 0) (c-255! 255 255 0) (c-255! 0 255 0)
          (c-255! 0 0 255) (c-255! 75 0 130) (c-255! 148 0 211)))

(defun c-rainbow (f)
  (let ((rainbow-value (* f 6.0)))
    (multiple-value-bind (i frac)
        (floor rainbow-value)
      (c-lerp frac (aref +rainbow+ i) (aref +rainbow+ (min (1+ i) 6))))))

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
  (let* ((theta1 (atan (y p1) (x p1)))
	 (theta2 (atan (y p2) (x p2)))
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

;;;; matrix =============================================================

;;;;
;;;; Kaveh Kardan -- 4 April 1992
;;;; Kaveh Kardan -- 20 December 1992
;;;;


(defun make-matrix ()
  (make-array '(4 4) :element-type 'number :initial-element 0.0))
  
(defun make-matrix-with (contents)
  (make-array '(4 4) :element-type 'number :initial-contents contents))

(defun make-id-matrix ()
  (make-matrix-with
   (list (list 1.0 0.0 0.0 0.0)
         (list 0.0 1.0 0.0 0.0)
         (list 0.0 0.0 1.0 0.0)
         (list 0.0 0.0 0.0 1.0))))

(defun matrix->list (matrix)
  (list (aref matrix 0 0) (aref matrix 0 1) (aref matrix 0 2) (aref matrix 0 3)
        (aref matrix 1 0) (aref matrix 1 1) (aref matrix 1 2) (aref matrix 1 3)
        (aref matrix 2 0) (aref matrix 2 1) (aref matrix 2 2) (aref matrix 2 3)
        (aref matrix 3 0) (aref matrix 3 1) (aref matrix 3 2) (aref matrix 3 3)))
        
(defun matrix->vector (matrix)
  (vector (aref matrix 0 0) (aref matrix 0 1) (aref matrix 0 2) (aref matrix 0 3)
          (aref matrix 1 0) (aref matrix 1 1) (aref matrix 1 2) (aref matrix 1 3)
          (aref matrix 2 0) (aref matrix 2 1) (aref matrix 2 2) (aref matrix 2 3)
          (aref matrix 3 0) (aref matrix 3 1) (aref matrix 3 2) (aref matrix 3 3)))

;;; transformation matrices

(defun make-translation-matrix (point)
  (make-matrix-with
   (list (list 1.0       0.0       0.0       0.0)
         (list 0.0       1.0       0.0       0.0)
         (list 0.0       0.0       1.0       0.0)
         (list (x point) (y point) (z point) 1.0))))
                          
(defun make-rotation-matrix (point &optional (center nil))
  (let ((rot-matrix
         (matrix-multiply-n (make-x-rotation-matrix (x point))
                            (make-y-rotation-matrix (y point))
                            (make-z-rotation-matrix (z point)))))
    (if center
      (matrix-multiply-n (make-translation-matrix (p-negate center))
                         rot-matrix
                         (make-translation-matrix center))
      rot-matrix)))

(defun make-x-rotation-matrix (angle)
  (let ((s (sin angle))
        (c (cos angle)))
    (make-matrix-with
     (list (list 1.0 0.0   0.0 0.0)
           (list 0.0 c     s   0.0)
           (list 0.0 (- s) c   0.0)
           (list 0.0 0.0   0.0 1.0)))))

(defun make-y-rotation-matrix (angle)
  (let ((s (sin angle))
        (c (cos angle)))
    (make-matrix-with
     (list (list c   0.0 (- s) 0.0)
           (list 0.0 1.0 0.0   0.0)
           (list s   0.0 c     0.0)
           (list 0.0 0.0 0.0   1.0)))))

(defun make-z-rotation-matrix (angle)
  (let ((s (sin angle))
        (c (cos angle)))
    (make-matrix-with
     (list (list c     s   0.0 0.0)
           (list (- s) c   0.0 0.0)
           (list 0.0   0.0 1.0 0.0)
           (list 0.0   0.0 0.0 1.0)))))

(defun make-axis-rotation-matrix (angle axis &optional (center nil))
  (let* ((s (sin angle))
         (c (cos angle))
         (c1 (- 1 c))
         (norm-axis (p-normalize axis))
         (x (x norm-axis))
         (y (y norm-axis))
         (z (z norm-axis))
         (x2 (* x x))
         (y2 (* y y))
         (z2 (* z z))
         (xyc1 (* x y c1))
         (yzc1 (* y z c1))
         (xzc1 (* x z c1))
         (sx (* s x))
         (sy (* s y))
         (sz (* s z))
         (cx2 (* c (- 1 x2)))
         (cy2 (* c (- 1 y2)))
         (cz2 (* c (- 1 z2)))
	 (rot-matrix (make-matrix-with
		      (list
		       (list (+ x2 cx2)    (+ xyc1 sz)   (- xzc1 sy)   0.0)
		       (list (- xyc1 sz)   (+ y2 cy2)    (+ yzc1 sx)   0.0)
		       (list (+ xzc1 sy)   (- yzc1 sx)   (+ z2 cz2)    0.0)
		       (list 0.0           0.0           0.0           1.0)))))
    (if center
	(matrix-multiply-n (make-translation-matrix (p-negate center))
			   rot-matrix
			   (make-translation-matrix center))
	rot-matrix)))


(defun make-scale-matrix (point &optional (center nil))
  (let ((mtx (make-matrix-with
	      (list (list (x point) 0.0       0.0       0.0)
		    (list 0.0       (y point) 0.0       0.0)
		    (list 0.0       0.0       (z point) 0.0)
		    (list 0.0       0.0       0.0       1.0)))))
    (if center
	(matrix-multiply-n (make-translation-matrix (p-negate center))
			   mtx
			   (make-translation-matrix center))
	mtx)))

(defun make-shear-matrix (point)
  (make-matrix-with
   (list (list 1.0       0.0       0.0      0.0)
         (list (x point) 1.0       0.0      0.0)
         (list (y point) (z point) 1.0      0.0)
         (list 0.0       0.0       0.0      1.0))))

(defun make-z-alignment-matrix (point)
  (multiple-value-bind (y-angle x-angle)
      (p-z-alignment-angles (p-normalize point))
    (let* ((sx (sin x-angle))
	   (cx (cos x-angle))
	   (sy (sin y-angle))
	   (cy (cos y-angle))
	   (ra (* sx sy))
	   (rb (* sx cy))
	   (rc (* cx sy))
	   (rd (* cx cy)))
      (make-matrix-with
       (list (list cy     ra  rc     0.0)
	     (list 0.0    cx  (- sx) 0.0)
	     (list (- sy) rb  rd     0.0)
	     (list 0.0    0.0 0.0    1.0))))))
                  
(defun make-look-at-from-matrix (from to &optional (up (p! 0 1 0)))
  (let* ((fwd (p-normalize (p- to from)))
	 (right (p-normalize (p-cross up fwd)))
	 (up2 (p-cross fwd right)))
    (make-matrix-with
     (list (list (x right) (y right) (z right) 0.0)
	   (list (x up2) (y up2) (z up2) 0.0)
	   (list (x fwd) (y fwd) (z fwd) 0.0)
	   (list (x from) (y from) (z from) 1.0)))))

(defun make-look-dir-from-matrix (from dir &optional (up (p! 0 1 0)))
  (let* ((fwd (p-normalize dir))
	 (right (p-cross (p-normalize up) fwd))
	 (up2 (p-cross fwd right)))
    (make-matrix-with
     (list (list (x right) (y right) (z right) 0.0)
	   (list (x up2) (y up2) (z up2) 0.0)
	   (list (x fwd) (y fwd) (z fwd) 0.0)
	   (list (x from) (y from) (z from) 1.0)))))

;;; matrix operations

(defun matrix-multiply (matrix-1 matrix-2)
  (let ((m (make-matrix))
        (e00 (aref matrix-1 0 0))
        (e01 (aref matrix-1 0 1))
        (e02 (aref matrix-1 0 2))
        (e03 (aref matrix-1 0 3))
        (e10 (aref matrix-1 1 0))
        (e11 (aref matrix-1 1 1))
        (e12 (aref matrix-1 1 2))
        (e13 (aref matrix-1 1 3))
        (e20 (aref matrix-1 2 0))
        (e21 (aref matrix-1 2 1))
        (e22 (aref matrix-1 2 2))
        (e23 (aref matrix-1 2 3))
        (e30 (aref matrix-1 3 0))
        (e31 (aref matrix-1 3 1))
        (e32 (aref matrix-1 3 2))
        (e33 (aref matrix-1 3 3))
        (f00 (aref matrix-2 0 0))
        (f01 (aref matrix-2 0 1))
        (f02 (aref matrix-2 0 2))
        (f03 (aref matrix-2 0 3))
        (f10 (aref matrix-2 1 0))
        (f11 (aref matrix-2 1 1))
        (f12 (aref matrix-2 1 2))
        (f13 (aref matrix-2 1 3))
        (f20 (aref matrix-2 2 0))
        (f21 (aref matrix-2 2 1))
        (f22 (aref matrix-2 2 2))
        (f23 (aref matrix-2 2 3))
        (f30 (aref matrix-2 3 0))
        (f31 (aref matrix-2 3 1))
        (f32 (aref matrix-2 3 2))
        (f33 (aref matrix-2 3 3)))
    (setf (aref m 0 0) (+ (* e00 f00) (* e01 f10) (* e02 f20) (* e03 f30)))
    (setf (aref m 0 1) (+ (* e00 f01) (* e01 f11) (* e02 f21) (* e03 f31)))
    (setf (aref m 0 2) (+ (* e00 f02) (* e01 f12) (* e02 f22) (* e03 f32)))
    (setf (aref m 0 3) (+ (* e00 f03) (* e01 f13) (* e02 f23) (* e03 f33)))
    (setf (aref m 1 0) (+ (* e10 f00) (* e11 f10) (* e12 f20) (* e13 f30)))
    (setf (aref m 1 1) (+ (* e10 f01) (* e11 f11) (* e12 f21) (* e13 f31)))
    (setf (aref m 1 2) (+ (* e10 f02) (* e11 f12) (* e12 f22) (* e13 f32)))
    (setf (aref m 1 3) (+ (* e10 f03) (* e11 f13) (* e12 f23) (* e13 f33)))
    (setf (aref m 2 0) (+ (* e20 f00) (* e21 f10) (* e22 f20) (* e23 f30)))
    (setf (aref m 2 1) (+ (* e20 f01) (* e21 f11) (* e22 f21) (* e23 f31)))
    (setf (aref m 2 2) (+ (* e20 f02) (* e21 f12) (* e22 f22) (* e23 f32)))
    (setf (aref m 2 3) (+ (* e20 f03) (* e21 f13) (* e22 f23) (* e23 f33)))
    (setf (aref m 3 0) (+ (* e30 f00) (* e31 f10) (* e32 f20) (* e33 f30)))
    (setf (aref m 3 1) (+ (* e30 f01) (* e31 f11) (* e32 f21) (* e33 f31)))
    (setf (aref m 3 2) (+ (* e30 f02) (* e31 f12) (* e32 f22) (* e33 f32)))
    (setf (aref m 3 3) (+ (* e30 f03) (* e31 f13) (* e32 f23) (* e33 f33)))
    m))

(defun matrix-multiply-n (&rest matrices)
  (when matrices
    (let ((m1 (first matrices)))
      (dolist (m2 (rest matrices))
        (setf m1 (matrix-multiply m1 m2)))
      m1)))

;;; 3dpoint matrix multiplication

(defun transform-point (point matrix)
  (let* ((x (x point))
         (y (y point))
         (z (z point))
         (w 1.0)
         (tx (+ (* x (aref matrix 0 0))
                (* y (aref matrix 1 0))
                (* z (aref matrix 2 0))
                (* w (aref matrix 3 0))))
         (ty (+ (* x (aref matrix 0 1))
                (* y (aref matrix 1 1))
                (* z (aref matrix 2 1))
                (* w (aref matrix 3 1))))
         (tz (+ (* x (aref matrix 0 2))
                (* y (aref matrix 1 2))
                (* z (aref matrix 2 2))
                (* w (aref matrix 3 2))))
         (tw (+ (* x (aref matrix 0 3))
                (* y (aref matrix 1 3))
                (* z (aref matrix 2 3))
                (* w (aref matrix 3 3)))))
    (if (/= tw 0.0)
      (p! (/ tx tw) (/ ty tw) (/ tz tw))
      (p! tx ty tz))))

(defun transform-points (points matrix)
  (mapcar #'(lambda (p) (transform-point p matrix)) points))

;;; destructive version
(defun transform-point! (point matrix)
  (let* ((x (x point))
         (y (y point))
         (z (z point))
         (w 1.0)
         (tx (+ (* x (aref matrix 0 0))
                (* y (aref matrix 1 0))
                (* z (aref matrix 2 0))
                (* w (aref matrix 3 0))))
         (ty (+ (* x (aref matrix 0 1))
                (* y (aref matrix 1 1))
                (* z (aref matrix 2 1))
                (* w (aref matrix 3 1))))
         (tz (+ (* x (aref matrix 0 2))
                (* y (aref matrix 1 2))
                (* z (aref matrix 2 2))
                (* w (aref matrix 3 2))))
         (tw (+ (* x (aref matrix 0 3))
                (* y (aref matrix 1 3))
                (* z (aref matrix 2 3))
                (* w (aref matrix 3 3)))))
    (if (/= tw 0.0)
      (p-set! point (/ tx tw) (/ ty tw) (/ tz tw))
      (p-set! point tx ty tz))))

(defun transform-points! (points matrix)
  (mapc #'(lambda (p) (transform-point! p matrix)) points)
  points)


;;;
;;; noise ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(defconstant noise-numpts 512)
(defconstant noise-p1 173.0)
(defconstant noise-p2 263.0)
(defconstant noise-p3 337.0)
(defconstant noise-phi 0.6180339)

(defvar *noise-pts* (make-array noise-numpts :element-type 'float))

(defun init-noise ()
  (dotimes (i noise-numpts)
    (setf (aref *noise-pts* i) (random 1.0))))

(init-noise)

(defun noise (p)
  (let* ((x (x p))
         (y (y p))
         (z (z p))

         (xi (floor x))
         (yi (floor y))
         (zi (floor z))

         (xa (floor (* noise-p1 (mod (*    xi    noise-phi) 1))))
         (xb (floor (* noise-p1 (mod (* (+ xi 1) noise-phi) 1))))
         (xc (floor (* noise-p1 (mod (* (+ xi 2) noise-phi) 1))))
         (ya (floor (* noise-p2 (mod (*    yi    noise-phi) 1))))
         (yb (floor (* noise-p2 (mod (* (+ yi 1) noise-phi) 1))))
         (yc (floor (* noise-p2 (mod (* (+ yi 2) noise-phi) 1))))
         (za (floor (* noise-p3 (mod (*    zi    noise-phi) 1))))
         (zb (floor (* noise-p3 (mod (* (+ zi 1) noise-phi) 1))))
         (zc (floor (* noise-p3 (mod (* (+ zi 2) noise-phi) 1))))

         (p000 (aref *noise-pts* (mod (+ xa ya za) noise-numpts)))
         (p100 (aref *noise-pts* (mod (+ xb ya za) noise-numpts)))
         (p200 (aref *noise-pts* (mod (+ xc ya za) noise-numpts)))
         (p010 (aref *noise-pts* (mod (+ xa yb za) noise-numpts)))
         (p110 (aref *noise-pts* (mod (+ xb yb za) noise-numpts)))
         (p210 (aref *noise-pts* (mod (+ xc yb za) noise-numpts)))
         (p020 (aref *noise-pts* (mod (+ xa yc za) noise-numpts)))
         (p120 (aref *noise-pts* (mod (+ xb yc za) noise-numpts)))
         (p220 (aref *noise-pts* (mod (+ xc yc za) noise-numpts)))
         (p001 (aref *noise-pts* (mod (+ xa ya zb) noise-numpts)))
         (p101 (aref *noise-pts* (mod (+ xb ya zb) noise-numpts)))
         (p201 (aref *noise-pts* (mod (+ xc ya zb) noise-numpts)))
         (p011 (aref *noise-pts* (mod (+ xa yb zb) noise-numpts)))
         (p111 (aref *noise-pts* (mod (+ xb yb zb) noise-numpts)))
         (p211 (aref *noise-pts* (mod (+ xc yb zb) noise-numpts)))
         (p021 (aref *noise-pts* (mod (+ xa yc zb) noise-numpts)))
         (p121 (aref *noise-pts* (mod (+ xb yc zb) noise-numpts)))
         (p221 (aref *noise-pts* (mod (+ xc yc zb) noise-numpts)))
         (p002 (aref *noise-pts* (mod (+ xa ya zc) noise-numpts)))
         (p102 (aref *noise-pts* (mod (+ xb ya zc) noise-numpts)))
         (p202 (aref *noise-pts* (mod (+ xc ya zc) noise-numpts)))
         (p012 (aref *noise-pts* (mod (+ xa yb zc) noise-numpts)))
         (p112 (aref *noise-pts* (mod (+ xb yb zc) noise-numpts)))
         (p212 (aref *noise-pts* (mod (+ xc yb zc) noise-numpts)))
         (p022 (aref *noise-pts* (mod (+ xa yc zc) noise-numpts)))
         (p122 (aref *noise-pts* (mod (+ xb yc zc) noise-numpts)))
         (p222 (aref *noise-pts* (mod (+ xc yc zc) noise-numpts)))

         (xf (- x xi))
         (xt (* xf xf))
         (x2 (* .5 xt))
         (x1 (- (+ .5 xf) xt))
         (x0 (- (+ .5 x2) xf))

         (yf (- y yi))
         (yt (* yf yf))
         (y2 (* .5 yt))
         (y1 (- (+ .5 yf) yt))
         (y0 (- (+ .5 y2) yf))

         (zf (- z zi))
         (zt (* zf zf))
         (z2 (* .5 zt))
         (z1 (- (+ .5 zf) zt))
         (z0 (- (+ .5 z2) zf)))

    (+ (* z0 (+ (* y0 (+ (* x0 p000) (* x1 p100) (* x2 p200)))
                (* y1 (+ (* x0 p010) (* x1 p110) (* x2 p210)))
                (* y2 (+ (* x0 p020) (* x1 p120) (* x2 p220)))))
       (* z1 (+ (* y0 (+ (* x0 p001) (* x1 p101) (* x2 p201)))
                (* y1 (+ (* x0 p011) (* x1 p111) (* x2 p211)))
                (* y2 (+ (* x0 p021) (* x1 p121) (* x2 p221)))))
       (* z2 (+ (* y0 (+ (* x0 p002) (* x1 p102) (* x2 p202)))
                (* y1 (+ (* x0 p012) (* x1 p112) (* x2 p212)))
                (* y2 (+ (* x0 p022) (* x1 p122) (* x2 p222))))))))

(defun turbulence (p n-octaves)
  (let ((sum 0.0)
        (scale 1.0))
    (dotimes (i n-octaves)
      (incf sum (/ (noise (p-scale p scale)) (* scale 2)))
      (setf scale (* scale 2)))
    sum))

(defun noise-gradient (p &optional (delta 0.01))
  (let* ((x (x p))
         (y (y p))
         (z (z p))
         (dx (- (noise (p! (+ x delta) y z)) (noise (p! (- x delta) y z))))
         (dy (- (noise (p! x (+ y delta) z)) (noise (p! x (- y delta) z))))
         (dz (- (noise (p! x y (+ z delta))) (noise (p! x y (- z delta))))))
    (p! dx dy dz)))

(defun color-noise (p &optional (delta 0.01))
  (let ((pn (p-normalize (noise-gradient p delta))))
    (c! (abs (x pn)) (abs (y pn)) (abs (z pn)))))

