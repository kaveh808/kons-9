(in-package #:kons-9)

;;;; signed distance functions =================================================

;;; based on: https://iquilezles.org/articles/distfunctions/

(defun sd-sphere (p r)
  (- (p:length p) r))

(defun sd-box (p b)
  (let ((q (p- (p:abs p) b)))
    (+ (p:length (p:max q +origin+))
       (min (max (p:x q) (max (p:y q) (p:z q))) 0.0))))

(defun sd-round-box (p b r)
  (let ((q (p- (p:abs p) b)))
    (- (+ (p:length (p:max q +origin+))
          (min (max (p:x q) (max (p:y q) (p:z q))) 0.0))
       r)))

(defun sd-box-frame (p b e)
  (let* ((p (p- (p:abs p) b))
         (q (p- (p:abs (p+ p e)) e)))
    (min
     (+ (p:length (p:max (p! (p:x p) (p:y q) (p:z q)) +origin+)) (min (max (p:x p) (p:y q) (p:z q)) 0.0))
     (+ (p:length (p:max (p! (p:x q) (p:y p) (p:z q)) +origin+)) (min (max (p:x q) (p:y p) (p:z q)) 0.0))
     (+ (p:length (p:max (p! (p:x q) (p:y q) (p:z p)) +origin+)) (min (max (p:x q) (p:y q) (p:z p)) 0.0)))))

(defun sd-torus (p r)
  (let ((q (p! (- (p:length (p! (p:x p) (p:z p) 0.0)) (p:x r)) (p:y p) 0.0)))
    (- (p:length q) (p:y r))))

(defun sd-capped-torus (p sc ra rb)
  (let* ((p (p! (abs (p:x p)) (p:y p) (p:z p)))
         (pxy (p! (p:x p) (p:y p) 0.0))
         (k (if (> (* (p:y sc) (p:x p)) (* (p:x sc) (p:y p)))
                (p:dot p sc)
                (p:length pxy))))
    (- (sqrt (- (+ (p:dot p p) (* ra ra)) (* 2 ra k))) rb)))

(defun sd-link (p le r1 r2)
  (let ((q (p! (p:x p) (max (- (abs (p:y p)) le) 0.0) (p:z p))))
    (- (p:length (p! (- (p:length (p! (p:x q) (p:y q) 0)) r1) (p:z q) 0)) r2)))

(defun sd-cylinder (p c)
  (- (p:length (p- (p! (p:x p) (p:z p) 0) (p! (p:x c) (p:y c) 0))) (p:z c)))

(defun sd-cone (p c h)
  (let* ((q (p* (p! (/ (p:x c) (p:y c)) -1.0 0) h))
         (pxz (p! (p:x p) (p:z p) 0.0))
         (w (p! (p:length pxz) (p:y p) 0))
         (a (p- w (p* q (clamp (/ (p:dot w q) (p:dot q q)) 0.0 1.0))))
         (b (p- w (p* q (p! (clamp (/ (p:x w) (p:x q)) 0.0 1.0) 1.0 0))))
         (k (signum (p:y q)))
         (d (min (p:dot a a) (p:dot b b)))
         (s (max (* k (- (* (p:x w) (p:y q)) (* (p:y w) (p:x q))))
                 (* k (- (p:y w) (p:y q))))))
    (* (sqrt d) (signum s))))

(defun sd-plane (p n h)                  ;n must be normalized
  (+ (p:dot p n) h))

(defun sd-tri-prism (p h)
  (let ((q (p:abs p)))
    (max (- (p:z q) (p:y h))
         (- (max (+ (* (p:x q) 0.866025) (* (p:y p) 0.5))
                 (- (p:y p)))
            (* (p:x h) 0.5)))))

(defun sd-vertical-capsule (p h r)
  (let* ((y (- (p:y p) (clamp (p:y p) 0.0 h)))
         (q (p! (p:x p) y (p:z p))))
    (- (p:length q) r)))
    
(defun sd-capped-cylinder (p h r)
  (let* ((pxz (p! (p:x p) (p:z p) 0))
         (d (p- (p:abs (p! (p:length pxz) (p:y p) 0))
                (p! r h 0))))
    (+ (min (max (p:x d) (p:y d)) 0.0)
       (p:length (p:max d (p! 0 0 0))))))

(defun sd-cut-hollow-sphere (p r h th)
  (let* ((w (sqrt (- (* r r) (* h h))))
         (pxz (p! (p:x p) (p:z p) 0))
         (q (p! (p:length pxz) (p:y p) 0)))
    (- (if (< (* h (p:x q)) (* w (p:y q)))
           (p:length (p- q (p! w h 0)))
           (abs (- (p:length q) r)))
       th)))

(defun sd-op-union (d1 d2)
  (min d1 d2))

(defun sd-op-subtraction (d1 d2)
  (max (- d1) d2))

(defun sd-op-intersection (d1 d2)
  (max d1 d2))

(defun sd-op-smooth-union (d1 d2 k)
  (let ((h (clamp (+ 0.5 (* 0.5 (/ (- d2 d1) k))) 0.0 1.0)))
    (- (lerp h d2 d1) (* k h (- 1.0 h)))))

(defun sd-op-smooth-subtraction (d1 d2 k)
  (let ((h (clamp (- 0.5 (* 0.5 (/ (+ d2 d1) k))) 0.0 1.0)))
    (+ (lerp h d2 (- d1)) (* k h (- 1.0 h)))))

(defun sd-op-smooth-intersection (d1 d2 k)
  (let ((h (clamp (- 0.5 (* 0.5 (/ (- d2 d1) k))) 0.0 1.0)))
    (+ (lerp h d2 d1) (* k h (- 1.0 h)))))

