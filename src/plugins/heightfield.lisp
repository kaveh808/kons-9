(in-package #:kons-9)

;;;; heightfield =======================================================

(defclass heightfield (uv-mesh)
  ((height-fn :accessor height-fn :initarg :height-fn :initform nil)))

(defmethod make-heightfield (x-segments z-segments bounds-lo bounds-hi &key (name nil) (height-fn nil))
  (let ((hfield (make-instance 'heightfield :name name
                                            :u-dim (1+ z-segments)
                                            :v-dim (1+ x-segments)
                                            :u-wrap nil
                                            :v-wrap nil
                                            :height-fn height-fn)))
    (allocate-mesh-arrays hfield)
    (setf (uv-point-array hfield) (grid-point-array x-segments z-segments bounds-lo bounds-hi))
    (when height-fn
      (compute-heights hfield))
    (compute-polyhedron-data hfield)
    hfield))

(defun grid-point-array (x-segments z-segments &optional (bounds-lo (p! -1 0 -1)) (bounds-hi (p! 1 0 1)))
  (let* ((u-dim (1+ z-segments))
         (v-dim (1+ x-segments))
         (uv-point-array (make-array (list u-dim v-dim))))
    (dotimes (u u-dim)
      (let* ((fx (/ u (- u-dim 1.0)))
	     (x (lerp fx (p:x bounds-lo) (p:x bounds-hi))))
	(dotimes (v v-dim)
	  (let* ((fz (/ v (- v-dim 1.0)))
		 (z (lerp (- 1 fz) (p:z bounds-lo) (p:z bounds-hi)))) ;reverse order for correct backface culling: (- 1 fz)
	    (setf (aref uv-point-array u v) (p! x 0.0 z))))))
    uv-point-array))

(defmethod compute-heights ((hfield heightfield))
  (dotimes (u (u-dim hfield))
    (dotimes (v (v-dim hfield))
      (let ((p (aref (uv-point-array hfield) u v)))
        (setf (p:y p) (funcall (height-fn hfield) (p:x p) (p:z p)))))))

(defmethod update-heightfield ((hfield heightfield))
  (compute-heights hfield)
  (compute-face-normals hfield)
  (compute-point-normals hfield))

;;; NOTE -- array must have size > 1 in all dimensions
(defmethod height-at-point ((hfield heightfield) p)
  (funcall (height-fn hfield) (p:x p) (p:z p)))

;;;; gui =======================================================================

(defun heightfield-command-table ()
  (let ((table (make-instance `command-table :title "Create Heightfield")))
    (ct-make-shape :A "Heightfield 1"
                   (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                     :height-fn (lambda (x z)
                                                  (* 4 (noise (p! x 0 z))))))
    (ct-make-shape :B "Heightfield 2"
                   (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                     :height-fn (lambda (x z)
                                                  (* 4 (turbulence (p! x 0 z) 4)))))
    (ct-make-shape :C "Heightfield 3"
                   (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                     :height-fn (lambda (x z)
                                                  (let* ((p (p! x 0 z))
                                                         (mag (p:length (p:scale p .25))))
                                                    (if (= mag 0.0)
                                                        10.0
                                                        (/ 1.0 mag))))))
    (ct-make-shape :D "Heightfield 4"
                   (make-heightfield 80 80 (p! -5 0 -5) (p! 5 0 5)
                                     :height-fn (lambda (x z)
                                                  (let* ((p (p! x 0 z))
                                                         (mag (max 0.001 (p:length (p:scale p 4.0)))))
                                                    (* 3 (/ (sin mag) mag))))))
    table))

(register-dynamic-command-table-entry "Create" :H "Create Heightfield Menu"
                                      (lambda () (make-active-command-table (heightfield-command-table)))
                                      (lambda () t))

