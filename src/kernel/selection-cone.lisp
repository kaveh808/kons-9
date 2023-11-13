(in-package #:kons-9)

(defparameter *previous-selection-cone* nil)
(defparameter *previous-selection-cone-intersect-list* nil)

(defclass selection-cone ()
  ((from :initarg :from :reader from)
   (to :initarg :to :reader to)
   (angle :initarg :angle :reader angle)
   (triangles :accessor triangles)))

(defmethod print-object ((self selection-cone) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~s - ~s, ~s" (from self) (to self) (angle self))))

(defmethod initialize-instance :after ((self selection-cone) &rest initargs)
  (declare (ignore initargs))
  (macrolet ((triangle-points (tri)
               `(vector
                 (origin.geometry.triangle::triangle-a ,tri)
                 (origin.geometry.triangle::triangle-b ,tri)
                 (origin.geometry.triangle::triangle-c ,tri))))
    (multiple-value-bind (faces points)
        (make-pyramid-faces (from self) (to self) (angle self))
      (setf (triangles self) 
            (map 'vector
                 (lambda (face)
                   (list
                    (aref points (elt face 0))
                    (aref points (elt face 1))
                    (aref points (elt face 2))))
                 (subseq faces 0 4))) ;;; the first 4 are lateral faces
      )))

(defun make-pyramid-faces (apex centroid angle)
  (flet ((arbitrary-perpendicular (vec)
           (dolist (a (list (p! 0 0 1) (p! 0 1 0)))
             (let ((vec-norm (p:normalize vec)))
               (when (not (p:parallel-p vec-norm a))
                 (return (p:normalize (p:cross vec-norm a))))))))
    (let* ((axis (p:- centroid apex))
           (p-0 (arbitrary-perpendicular axis))
           (p-1 (p:normalize (p:cross (p:normalize axis) p-0)))
           (len (/ (coerce (* (p:length axis) (sin angle)) 'single-float) 2))
           (-len (- len))
           (q0 (p:+ (p:lerp (p:zero) p-0 len) axis))
           (q1 (p:+ (p:lerp (p:zero) p-1 len) axis))
           (q2 (p:+ (p:lerp (p:zero) p-0 -len) axis))
           (q3 (p:+ (p:lerp (p:zero) p-1 -len) axis))
           (c-0 (p:+ centroid q0))
           (c-1 (p:+ centroid q1))
           (c-2 (p:+ centroid q2))
           (c-3 (p:+ centroid q3))
           (points (vector apex c-0 c-1 c-2 c-3))
           (faces (vector
                   '(0 1 2)
                   '(0 2 3)
                   '(0 3 4)
                   '(0 4 1)
                   '(1 2 3 4))))
      (values faces points))))

(defun intersect-line (selection-cone p0 p1)
  (let* ((line-start (p-vec p0))
         (line-end (p-vec p1))
         (line-vec (p:- line-end line-start))
         (line-length (p:length line-vec))
         (ray (make-instance 'ray :from line-start :to line-end))
         (triangles (triangles selection-cone)))
    (flet ((dist-from-cone-apex-to-pt (pt)
             (p:length (p:- pt (from selection-cone))))
           (positive? (a)
             (when (and a (> a 0))
               a)))
      (dotimes (i (length triangles))
        (destructuring-bind (p0 p1 p2) (aref triangles i)
          (let ((distance (positive? (intersect-triangle ray p0 p1 p2))))
            ;; successful intersection of a ray does not imply intersection of
            ;; the line because a ray is infinite while a line is finite.
            (when (and distance (<= distance line-length))
              (let* ((intersect-pt (p:lerp line-start line-end
                                           (/ distance line-length)))
		     (pick-point (if (< (/ distance line-length) 0.5) line-start line-end)))
                (push intersect-pt *previous-selection-cone-intersect-list*)
                (return-from intersect-line
                  (cons (dist-from-cone-apex-to-pt intersect-pt) pick-point))))))))))

(defun get-lines (curve)
  ;;; the resulting lines reverse in order of the curve's points
  (let* ((points (points curve))
         (count (length points))
         (lines '()))
    (when (< (length points) 2)
      (return-from get-lines))
    (flet ((add-line (index-0 index-1)
             (push (list (aref points index-0) (aref points index-1)) lines)))
      (dotimes (i (- count 1) lines)
        (add-line i (+ i 1)))
      (when (is-closed-curve? curve)
        (add-line (- count 1) 0)))
    lines))


;;;; display selection cone (useful for debugging) =============================

(defun set-previous-selection-cone-and-intersects (cone)
  (setf *previous-selection-cone* cone)
  (setf *previous-selection-cone-intersect-list* nil))

(defun draw-previous-selection-cone ()
  (flet ((v (vec3) (apply #'gl:vertex (coerce vec3 'list))))
    (when *previous-selection-cone*
      (let ((triangles (triangles *previous-selection-cone*)))
        (gl:shade-model :flat)
        (gl:disable :lighting)
        (dotimes (i (length triangles))
          (destructuring-bind (p0 p1 p2) (aref triangles i)
            (gl:color 1.0 1.0 0.0)
            (gl:polygon-mode :front :fill)
            (gl:begin :polygon)
            (v p0)
            (v p1)
            (v p2)
            (gl:end)

            (gl:color 0.0 1.1 0.0)
            (gl:polygon-mode :back :fill)
            (gl:begin :polygon)
            (v p0)
            (v p1)
            (v p2)
            (gl:end)
            
            (gl:line-width 1)
            (gl:color 0.0 0.0 1.0)
            (gl:begin :lines)
            (v p0)
            (v p1)
            (v p2)
            (gl:end)
            ))))

    (when *previous-selection-cone-intersect-list*
      (gl:shade-model :flat)
      (gl:disable :lighting)
      (gl:point-size 10.0)
      (gl:color 1.0 0.0 0.0 0.5)
      (gl:begin :points)
      (mapc #'v *previous-selection-cone-intersect-list*)
      (gl:end))))


;;;; intersect routines ========================================================

(defmethod intersect ((self selection-cone) (shape shape))
  nil)

(defmethod intersect ((self selection-cone) (curve curve))
  (setf *previous-selection-cone* self)
  (flet ((intersect-with-line (line) ))
    (let* ((distances-and-picked-points
	     (mapcar
	      (lambda (line) (intersect-line self (car line) (cadr line)))
              (get-lines curve)))
           (distances-non-null (remove-if #'null distances-and-picked-points)))
      (when distances-non-null
        (let ((min-dist-and-point (first distances-non-null)))
	  (loop for dist-and-point in distances-non-null
		do (when (< (car dist-and-point) (car min-dist-and-point) )
		     (setf min-dist-and-point dist-and-point)))
	  (values (car min-dist-and-point) (cdr min-dist-and-point)))))))
