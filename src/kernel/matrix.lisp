(in-package #:kons-9)

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

(defun matrix-copy (matrix)
  (make-matrix-with (list (list (aref matrix 0 0) (aref matrix 0 1) (aref matrix 0 2) (aref matrix 0 3))
                          (list (aref matrix 1 0) (aref matrix 1 1) (aref matrix 1 2) (aref matrix 1 3))
                          (list (aref matrix 2 0) (aref matrix 2 1) (aref matrix 2 2) (aref matrix 2 3))
                          (list (aref matrix 3 0) (aref matrix 3 1) (aref matrix 3 2) (aref matrix 3 3)))))

;;; transformation matrices

(defun make-translation-matrix (point)
  (make-matrix-with
   (list (list 1.0       0.0       0.0       0.0)
         (list 0.0       1.0       0.0       0.0)
         (list 0.0       0.0       1.0       0.0)
         (list (p:x point) (p:y point) (p:z point) 1.0))))
                          
(defun make-rotation-matrix (point order &optional (pivot nil))
  (let ((rot-matrix
          (case order
            (:xyz (matrix-multiply-n (make-x-rotation-matrix (p:x point))
                                     (make-y-rotation-matrix (p:y point))
                                     (make-z-rotation-matrix (p:z point))))
            (:xzy (matrix-multiply-n (make-x-rotation-matrix (p:x point))
                                     (make-z-rotation-matrix (p:z point))
                                     (make-y-rotation-matrix (p:y point))))
            (:yxz (matrix-multiply-n (make-y-rotation-matrix (p:y point))
                                     (make-x-rotation-matrix (p:x point))
                                     (make-z-rotation-matrix (p:z point))))
            (:yzx (matrix-multiply-n (make-y-rotation-matrix (p:y point))
                                     (make-z-rotation-matrix (p:z point))
                                     (make-x-rotation-matrix (p:x point))))
            (:zxy (matrix-multiply-n (make-z-rotation-matrix (p:y point))
                                     (make-x-rotation-matrix (p:x point))
                                     (make-z-rotation-matrix (p:z point))))
            (:zyx (matrix-multiply-n (make-z-rotation-matrix (p:z point))
                                     (make-y-rotation-matrix (p:y point))
                                     (make-x-rotation-matrix (p:x point))))
            (otherwise (error "Unknown rotate order ~a in MAKE-ROTATION-MATRIX" order)))))
    (if pivot
      (matrix-multiply-n (make-translation-matrix (p:negate pivot))
                         rot-matrix
                         (make-translation-matrix pivot))
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

(defun make-axis-rotation-matrix (angle axis &optional (pivot nil))
  (let* ((s (sin angle))
         (c (cos angle))
         (c1 (- 1 c))
         (norm-axis (p:normalize axis))
         (x (p:x norm-axis))
         (y (p:y norm-axis))
         (z (p:z norm-axis))
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
    
    (if pivot
	(matrix-multiply-n (make-translation-matrix (p:negate pivot))
			   rot-matrix
			   (make-translation-matrix pivot))
	rot-matrix)))


(defun make-scale-matrix (point &optional (pivot nil))
  (let ((mtx (make-matrix-with
	      (list (list (p:x point) 0.0       0.0       0.0)
		    (list 0.0       (p:y point) 0.0       0.0)
		    (list 0.0       0.0       (p:z point) 0.0)
		    (list 0.0       0.0       0.0       1.0)))))
    (if pivot
	(matrix-multiply-n (make-translation-matrix (p:negate pivot))
			   mtx
			   (make-translation-matrix pivot))
	mtx)))

(defun make-shear-matrix (point)
  (make-matrix-with
   (list (list 1.0       0.0       0.0      0.0)
         (list (p:x point) 1.0       0.0      0.0)
         (list (p:y point) (p:z point) 1.0      0.0)
         (list 0.0       0.0       0.0      1.0))))

(defun make-z-alignment-matrix (point)
  (multiple-value-bind (y-angle x-angle)
      (p-z-alignment-angles (p:normalize point))
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
  (let* ((fwd (p:normalize (p:- to from)))
	 (right (p:normalize (p:cross up fwd)))
	 (up2 (p:cross fwd right)))
    (make-matrix-with
     (list (list (p:x right) (p:y right) (p:z right) 0.0)
	   (list (p:x up2) (p:y up2) (p:z up2) 0.0)
	   (list (p:x fwd) (p:y fwd) (p:z fwd) 0.0)
	   (list (p:x from) (p:y from) (p:z from) 1.0)))))

(defun make-look-dir-from-matrix (from dir &optional (up (p! 0 1 0)))
  (let* ((fwd (p:normalize dir))
	 (right (p:cross (p:normalize up) fwd))
	 (up2 (p:cross fwd right)))
    (make-matrix-with
     (list (list (p:x right) (p:y right) (p:z right) 0.0)
	   (list (p:x up2) (p:y up2) (p:z up2) 0.0)
	   (list (p:x fwd) (p:y fwd) (p:z fwd) 0.0)
	   (list (p:x from) (p:y from) (p:z from) 1.0)))))

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
  (let* ((x (p:x point))
         (y (p:y point))
         (z (p:z point))
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
  (map 'vector #'(lambda (p) (transform-point p matrix)) points))

;;; destructive version
(defun transform-point! (point matrix)
  (let* ((x (p:x point))
         (y (p:y point))
         (z (p:z point))
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
      (p-set! point tx ty tz))
    point))


(defun transform-points! (points matrix)
  (loop for p across points
        do (transform-point! p matrix)))

;;; TODO -- remove this for efficiency?
(defun transform-point-list! (points matrix)
  (mapc #'(lambda (p) (transform-point! p matrix)) points)
  points)

