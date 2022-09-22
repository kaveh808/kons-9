(in-package #:kons-9)

;;;; easing functions ==========================================================

#|
Copied from:
https://github.com/vydd/easing/blob/master/src/easing-single-float.lisp

linear-fn in-sine-fn out-sine-fn in-out-sine-fn in-cubic-fn out-cubic-fn
in-out-cubic-fn in-quad-fn out-quad-fn in-out-quad-fn in-quart-fn out-quart-fn
in-out-quart-fn in-quint-fn out-quint-fn in-out-quint-fn in-exp-fn out-exp-fn
in-out-exp-fn in-circ-fn out-circ-fn in-out-circ-fn in-elastic-fn out-elastic-fn
in-out-elastic-fn in-back-fn out-back-fn in-out-back-fn in-bounce-fn
out-bounce-fn in-out-bounce-fn
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant pi-sf 3.141592653589793s0))

(defmacro defeasing-f (name args &body body)
  (let ((x (or (car args) 'x))
	(arg-names (remove-if (lambda (x) (char= (aref (symbol-name x) 0) #\&))
			      (mapcar (lambda (x) (if (listp x) (first x) x))
				      args)))
	(declaim-args (mapcar (lambda (x)
				(if (and (symbolp x)
					 (char= (aref (symbol-name x) 0) #\&))
				    x
				    'single-float))
			      args))
	;; (ease-in (alexandria:symbolicate 'in- name))
	;; (ease-out (alexandria:symbolicate 'out- name))
	;; (ease-in-out (alexandria:symbolicate 'in-out- name)))
	(ease-in (concat-syms 'in- name '-fn))
	(ease-out (concat-syms 'out- name '-fn))
	(ease-in-out (concat-syms 'in-out- name '-fn)))
    `(progn
       ;; in
       (declaim (inline ,ease-in)
		(ftype (function ,declaim-args single-float) ,ease-in))
       (defun ,ease-in ,args
	 (declare (optimize (speed 3) (safety 0) (debug 0))
		  (single-float ,@arg-names)
		  (inline + - / *))
	 (cond ((>= 0s0 ,x) 0s0)
	       ((<= 1s0 ,x) 1s0)
	       (t ,@body)))
       ;; out
       (declaim (inline ,ease-out)
		(ftype (function ,declaim-args single-float) ,ease-out))
       (defun ,ease-out ,args
	 (declare (optimize (speed 3) (safety 0) (debug 0))
		  (single-float ,@arg-names)
		  (inline + - / *))
	 (cond ((>= 0s0 ,x) 0s0)
	       ((<= 1s0 ,x) 1s0)
	       (t (let ((,x (- 1s0 ,x)))
		    (the single-float
			 (+ 1s0 (- ,@body)))))))
       ;; in-out
       (declaim (inline ,ease-in-out)
		(ftype (function ,declaim-args single-float) ,ease-in-out))
       (defun ,ease-in-out ,args
	 (declare (optimize (speed 3) (safety 0) (debug 0))
		  (single-float ,@arg-names)
		  (inline + - / *))
	 (cond ((>= 0s0 ,x) 0s0)
	       ((<= 1s0 ,x) 1s0)
	       (t (the single-float
		       (if (<= ,x 0.5s0)
			   (let ((,x (* 2s0 ,x)))
			     (/ ,@body 2s0))
			   (let ((,x (- 1s0 (* 2s0 (- ,x 0.5s0)))))
			     (+ 0.5s0 (/ (+ 1s0 (- ,@body))
					 2s0)))))))))))

;; The float versions

(declaim (inline linear-fn)
         (ftype (function (single-float) single-float) linear-fn))
(defun linear-fn (x)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (single-float x))
  x)

(defeasing-f sine (x)
  (- 1s0 (cos (* x (/ pi-sf 2s0)))))

(defeasing-f quad (x)
  (* x x))

(defeasing-f cubic (x)
  (* x x x))

(defeasing-f quart (x)
  (expt x 4s0))

(defeasing-f quint (x)
  (expt x 5s0))

(defeasing-f exp (x)
  (expt 2s0 (* 10s0 (- x 1s0))))

(defeasing-f circ (x)
  (- (- (the single-float (sqrt (- 1s0 (* x x)))) 1s0)))

(defeasing-f elastic (x &optional (p 0.3s0) (s 0s0 set-s))
  (let ((s (if set-s s (* (asin 1s0) (* p #.(/ 1s0 (* 2s0 pi-sf)))))))
    (- (* (expt 2 (* 10 (- x 1s0))) (sin (/ (* (- (- x 1s0) s) (* 2 pi-sf)) p))))))

(defeasing-f back (x &optional (s 1.70158s0))
  (* x x (- (* (+ 1s0 s) x) s)))

(defeasing-f bounce (x &optional (c1 7.5625))
  (let ((x (- 1s0 x)))
    (- 1s0 (cond ((< x (/ 1s0 2.75)) (* c1 x x))
		 ((< x (/ 2s0 2.75s0)) (let ((x (- x (/ 1.5s0 2.75s0))))
					 (+ 0.75s0 (* c1 x x))))
		 ((< x (/ 2.5s0 2.75s0)) (let ((x (- x (/ 2.25 2.75))))
					   (+ 0.9375s0 (* c1 x x))))
		 (t (let ((x (- x (/ 2.625s0 2.75s0))))
		      (+ 0.984375s0 (* c1 x x))))))))

;;;; channel ===================================================================

(defclass-kons-9 channel (item)
  ((mode :frame)))                     ; :relative 0-1, :time time-value, :frame frame-number

(defclass-kons-9 procedural-channel (channel)
  ((value-fn nil)))

(defclass-kons-9 data-channel (channel)
  ((data #())))

(defclass-kons-9 time-value-channel (channel)
  ((keys #())
   (interpolation :linear)))            ; :linear, :step

(defun key-time (key) (first key))
(defun key-value (key) (second key))

(defmethod find-key-index-before ((channel time-value-channel) key-time)
  (let* ((keys (keys channel)))
    (cond ((= 0 (length keys))
           -1)
          ((< key-time (key-time (aref keys 0)))
           -1)
          ((> key-time (key-time (aref keys (1- (length keys)))))
           (1- (length keys)))
          (t
           (dotimes (i (length keys))
             (when (< key-time (key-time (aref keys i)))
               (return-from find-key-index-before (1- i))))))))

(defmethod add-key ((channel time-value-channel) key-time key-value)
  (let ((matching-key (find key-time (keys channel) :key 'first :test '=)))
    (if matching-key
        (setf (second matching-key) key-value)
        (add-key-aux channel key-time key-value)))
  channel)
        
(defmethod add-key-aux ((channel time-value-channel) key-time key-value)
  (setf (keys channel)
        (insert-at (keys channel)
                   (1+ (find-key-index-before channel key-time))
                   (list key-time key-value))))

(defgeneric get-value (channel param)

  (:method ((channel procedural-channel) param)
    (when (value-fn channel)
      (funcall (value-fn channel) param)))

  (:method ((channel data-channel) param)
    (let ((index (ecase (mode channel)
                   (:relative (floor (* param (length (data channel)))))
                   (:time (floor param))
                   (:frame param))))
      (aref (data channel) index)))
  
  (:method ((channel time-value-channel) param)
    (let* ((keys (keys channel))
           (effective-param (ecase (mode channel)
                              (:relative (lerp param
                                               (key-time (aref keys 0))
                                               (key-time (aref keys (1- (length keys))))))
                              (:time param)
                              (:frame param)))
           (index (find-key-index-before channel effective-param)))
      (cond ((= -1 index)
             (key-value (aref keys 0)))
            ((= (1- (length keys)) index)
             (key-value (aref keys (1- (length keys)))))
            (t
             (print (list param effective-param index))
             (if (eq :step (interpolation channel))
                 (key-value (aref keys index))
                 (lerp (tween effective-param
                              (key-time (aref keys index))
                              (key-time (aref keys (1+ index))))
                       (key-value (aref keys index))
                       (key-value (aref keys (1+ index)))))))))
  )

#| test procedural-channel
(defparameter my-channel (make-instance 'procedural-channel :value-fn #'in-out-cubic-fn))
(print (get-value my-channel .9))
|#

#| test
(defparameter my-channel (make-instance 'time-value-channel))
(print (keys my-channel))
(add-key my-channel 10 0)
(print (keys my-channel))
(add-key my-channel 20 100)
(print (keys my-channel))
(add-key my-channel 0 20)
(print (keys my-channel))
(add-key my-channel 30 50)
(print (keys my-channel))
(add-key my-channel 20 80)
(print (keys my-channel))
(add-key my-channel 25 100)
(print (keys my-channel))

(print (get-value my-channel -5))
(print (get-value my-channel 20))
(print (get-value my-channel 5))
(print (get-value my-channel 100))

(setf (mode my-channel) :relative)
(print (get-value my-channel -0.1))
(print (get-value my-channel 0.5))
(print (get-value my-channel .9))
(print (get-value my-channel 1.2))
|#

