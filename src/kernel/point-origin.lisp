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

(defun p-vec (vec3)
  (p! (aref vec3 0) (aref vec3 1) (aref vec3 2)))

(defun copy-points (points)
  (mapcar #'p:copy points))

(defun copy-point-array (point-array)
  (map 'vector #'p:copy point-array))

(defmacro def-point-func-1 (op)
  `(defun ,(intern (concatenate 'string "P" (symbol-name op))) (p val)
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
    (sqrt (- 1.0 (* c c)))))

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

(defun triangle-normal (p0 p1 p2)
  (p:normalize (p:cross (p:- p1 p0) (p:- p2 p1))))

(defun quad-normal (p0 p1 p2 p3)
  (p:normalize (p:cross (p:- p2 p0) (p:- p3 p1))))

(defun triangle-area (p0 p1 p2)
  (let ((e1 (p-from-to p0 p1))
        (e2 (p-from-to p1 p2)))
    (/ (* (p:length e1) (p:length e2) (p-angle-sine e1 e2)) 2)))

(defun barycentric-point (p0 p1 p2 a b)
  (p:+ p0
       (p:+ (p:scale (p-from-to p0 p1) a)
            (p:scale (p-from-to p0 p2) b))))

(defun random-barycentric-point (p0 p1 p2)
  (let ((a (rand2 0.0 1.0))
        (b (rand2 0.0 1.0)))
    (do () ((<= (+ a b) 1.0))
      (setf a (rand2 0.0 1.0))
      (setf b (rand2 0.0 1.0)))
    (barycentric-point p0 p1 p2 a b)))

;;; TODO -- do these work in 3D?
;; (defun angle-2d (p1 p2)
;;   (let* ((theta1 (atan (p:y p1) (p:x p1)))
;; 	 (theta2 (atan (p:y p2) (p:x p2)))
;; 	 (dtheta (- theta2 theta1)))
;;     (loop while (> dtheta pi)
;; 	  do (setf dtheta (- dtheta (* 2 pi))))
;;     (loop while (< dtheta (- pi))
;; 	  do (setf dtheta (+ dtheta (* 2 pi))))
;;     dtheta))

;; (defun point-in-polygon? (p points)
;;   (let ((angle 0.0))
;;     (loop :for (p1 p2) :on (append points (list (first points))) :by #'cdr :while p2
;; 	  :do (setf angle (+ angle (angle-2d (p- p1 p) (p- p2 p)))))
;;     (if (< (abs angle) pi)
;; 	nil
;; 	t)))

(defun point-in-bounds? (p lo hi)
  (and (<= (p:x lo) (p:x p) (p:x hi))
       (<= (p:y lo) (p:y p) (p:y hi))
       (<= (p:z lo) (p:z p) (p:z hi))))
       
(defun points-bounds (point-array)
  (when (= 0 (length point-array))
    (return-from points-bounds (values (p! -1 -1 -1) (p! 1 1 1))))
  (let ((bounds-lo (p:copy (aref point-array 0)))
        (bounds-hi (p:copy (aref point-array 0))))
    (do-array (i p point-array)
      (p:min! bounds-lo bounds-lo p)
      (p:max! bounds-hi bounds-hi p))
    (values bounds-lo bounds-hi)))

(defun p-sphericize (p radius &optional (factor 1.0) (center +origin+))
  (p:lerp p (p* (p:normalize (p-from-to center p)) radius) factor))

(defun point-line-segment-dist (p a b)
  (let ((ab (p-from-to a b))
        (ap (p-from-to a p))
        (bp (p-from-to b p)))
    (cond ((<= (p:dot ap ab) 0.0)
           (p:length ap))               ;distance to endpoint A
          ((>= (p:dot bp ab) 0.0)
           (p:length bp))               ;distance to endpoint B
          (t
           (/ (p:length (p:cross ab ap)) (p:length ab)))))) ;perpendicular distance to line

(defun point-curve-dist (point points is-closed-curve?) ;points is an array of point
  (let* ((min-dist (p-dist point (aref points 0))))
    (dotimes (i (1- (length points)))
      (let ((dist (point-line-segment-dist point (aref points i) (aref points (1+ i)))))
        (when (< dist min-dist)
          (setf min-dist dist))))
    (when is-closed-curve?
      (let ((dist (point-line-segment-dist point (aref points (1- (length points))) (aref points 0))))
        (when (< dist min-dist)
          (setf min-dist dist))))
    min-dist))

;;; TODO - the following are for polyh-closest-point -- need to be optimized

(defun point-on-plane (point plane-point plane-normal)
  (let* ((v (p-from-to plane-point point))
         (d (p:dot v plane-normal))
         (p (p:- point (p:scale plane-normal d))))

;    (print (list 'point-on-plane point plane-point plane-normal v d p))
    
    p))

(defun point-on-triangle-plane (p a b c)
  (let ((n (triangle-normal a b c)))
    (point-on-plane p a n)))

(defun point-barycentric-coordinates (p a b c)
  (let* ((v0 (p-from-to a b))
         (v1 (p-from-to a c))
         (v2 (p-from-to a p))
         (d00 (p:dot v0 v0))
         (d01 (p:dot v0 v1))
         (d11 (p:dot v1 v1))
         (d20 (p:dot v2 v0))
         (d21 (p:dot v2 v1))
         (denom (- (* d00 d11) (* d01 d01)))
         (v (/ (- (* d11 d20) (* d01 d21)) denom))
         (w (/ (- (* d00 d21) (* d01 d20)) denom))
         (u (- 1.0 v w)))
    (p! u v w)))

(defun point-inside-triangle (p a b c)
  (let* ((p0 (point-on-triangle-plane p a b c))
         (bary (point-barycentric-coordinates p0 a b c)))

    ;; (print (and (<= 0.0 (p:x bary) 1.0)
    ;;             (<= 0.0 (p:y bary) 1.0)
    ;;             (<= 0.0 (p:z bary) 1.0)))

    (if (and (<= 0.0 (p:x bary) 1.0)
             (<= 0.0 (p:y bary) 1.0)
             (<= 0.0 (p:z bary) 1.0))
        (progn

;          (print (list 'inside p0))
          
          p0)
        nil)))

;;; TODO -- problem with points snapping to end points

(defun point-line-segment-closest-point (p a b)

;  (print "---------------------------------------------")
;  (print (list 'point-line-segment-closest-point p a b))

  (let ((ab (p:normalize (p-from-to a b)))
        (ap  (p-from-to a p))
        (bp  (p-from-to b p)))

;    (print (list 'ab ab))
;    (print (list 'ap ap))
;    (print (list 'bp bp))

    (cond ((<= (p:dot ap ab) 0.0)
;           (print (list 'a (p:dot ap ab) a))
           a)
          ((>= (p:dot bp ab) 0.0)
;           (print (list 'b (p:dot bp ab) b))
           b)
          (t
           (let* ((param-t (p:dot ab ap))
                  (p0 (p:+ a (p:scale ab param-t))))
           ;; d = ab
           ;; v = ap
           ;; t = v.d
           ;; p = a + td

           ;; (/ (p:length (p:cross ab ap)) (p:length ab)))))) ;perpendicular distance to line

           
           ;; (let* ((d (p:normalize ab))
           ;;       (p0 (p:scale d (p:dot ap d))))
;           (print (list 'p0 p0))
             p0)))))

;;; return closest point and distance to point
(defun point-triangle-closest-point (p a b c)

;  (print (list 'point-triangle-closest-point p))

  (let ((p0 (point-inside-triangle p a b c)))
    (if p0
        (values p0 (p-dist p p0))
        (let* ((p1 (point-line-segment-closest-point p a b))
               (p2 (point-line-segment-closest-point p b c))
               (p3 (point-line-segment-closest-point p c a))
               (d1 (p-dist p p1))
               (d2 (p-dist p p2))
               (d3 (p-dist p p3)))
          (cond ((and (<= d1 d2) (<= d1 d3))
                 (values p1 d1))
                ((and (<= d2 d1) (<= d2 d3))
                 (values p2 d2))
                (t
                 (values p3 d3)))))))
                
