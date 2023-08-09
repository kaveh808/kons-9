(in-package #:kons-9)

;;;; poly-strand ===============================================================

(defclass-kons-9 poly-strand (point-cloud)
  ((strands (make-array 0 :adjustable t :fill-pointer t)))) ;a strand is #(vref-1 vref-2)

(defmethod printable-data ((self poly-strand))
  (strcat (call-next-method) (format nil ", ~a strands" (length (strands self)))))

(defmethod strand-points ((poly poly-strand) s-ref)
  (let ((strand (aref (strands poly) s-ref)))
    (vector (aref (points poly) (aref strand 0))
            (aref (points poly) (aref strand 1)))))

(defmethod append-strand ((poly poly-strand) p-ref-1 p-ref-2)
  (vector-push-extend (vector p-ref-1 p-ref-2) (strands poly)))

(defmethod split-strand ((poly poly-strand) s-ref point)
  (let ((p-ref (vector-push-extend point (points poly)))
        (strand (aref (strands poly) s-ref)))
    (append-strand poly p-ref (aref strand 1))
    (setf (aref strand 1) p-ref)
    p-ref))

(defmethod insert-strand ((poly poly-strand) strand-1 t-1 strand-2 t-2)
  (let* ((strand-1-points (strand-points poly strand-1))
         (strand-2-points (strand-points poly strand-2))
         (p1 (p:lerp (elt strand-1-points 0) (elt strand-1-points 1) t-1))
         (p2 (p:lerp (elt strand-2-points 0) (elt strand-2-points 1) t-2))
         (p-ref-1 (split-strand poly strand-1 p1))
         (p-ref-2 (split-strand poly strand-2 p2)))
    (append-strand poly p-ref-1 p-ref-2)))

(defmethod insert-strands-random ((poly poly-strand) num &optional (t-min 0.2) (t-max 0.8))
  (let ((count 0))
    (loop while (< count num)
          do (let ((i (random (length (strands poly))))
                   (j (random (length (strands poly)))))
               (when (not (= i j))
                 (incf count)
                 (insert-strand poly
                                i (rand2 t-min t-max)
                                j (rand2 t-min t-max)))))))

(defmethod strand-length ((poly poly-strand) strand)
  (p-dist (aref (points poly) (aref strand 0))
          (aref (points poly) (aref strand 1))))

(defmethod insert-strands-by-length ((poly poly-strand) num &optional (t-min 0.2) (t-max 0.8))
  (dotimes (count num)
    (let* ((sorted-strands (sort (strands poly)
                                 (lambda (s1 s2)
                                   (> (strand-length poly s1) (strand-length poly s2)))))
           (strand-1 (aref sorted-strands 0))
           (strand-2 (aref sorted-strands 1)))
      (insert-strand poly
                     (position strand-1 (strands poly)) (rand2 t-min t-max)
                     (position strand-2 (strands poly)) (rand2 t-min t-max)))))

(defmethod contains-strand? ((poly poly-strand) p-ref-1 p-ref-2)
  (find-if (lambda (strand) (or (and (= (aref strand 0) p-ref-1) (= (aref strand 1) p-ref-2))
                                (and (= (aref strand 1) p-ref-1) (= (aref strand 0) p-ref-2))))
           (strands poly)))

(defmethod draw ((poly poly-strand))
  (when (is-visible? poly)
    (when *display-wireframe?*
      (draw-wireframe poly))
    (when *display-points?*
      (draw-points poly)))) ; nil))))         ;TODO -- maybe implement point-colors later

(defmethod draw-wireframe ((poly poly-strand))
  (let ((lines '())
        (points (points poly)))
    (do-array (i strand (strands poly))
      (push (aref points (aref strand 0)) lines)
      (push (aref points (aref strand 1)) lines))
    (3d-draw-lines lines)))

(defmethod make-poly-strand ((p-cloud point-cloud))
  (let* ((poly (make-instance 'poly-strand))
         (len (length (points p-cloud))))
    (do-array (i p (points p-cloud))
      (vector-push-extend p (points poly)))
    (dotimes (i len)
      (let ((j (random len)))
        (when (not (= i j))
          (append-strand poly i j))))
    poly))

(defmethod make-poly-strand ((curve curve))
  (let* ((poly (make-instance 'poly-strand))
         (len (length (points curve))))
    (do-array (i p (points curve))
      (vector-push-extend p (points poly)))
    (dotimes (i (1- len))
      (append-strand poly i (1+ i)))
    (when (is-closed-curve? curve)
      (append-strand poly (1- len) 0))
    poly))

(defmethod make-poly-strand ((polyh polyhedron))
  (let* ((poly (make-instance 'poly-strand)))
    (do-array (i p (points polyh))
      (vector-push-extend p (points poly)))
    (do-array (i f (faces polyh))
      (let ((face-array (coerce f 'vector)))
        (dotimes (j (1- (length face-array)))
          (let ((j0 (aref face-array j))
                (j1 (aref face-array (1+ j))))
            (when (not (contains-strand? poly j0 j1))
              (append-strand poly j0 j1))))
        (when (not (contains-strand? poly (vec-last face-array) (vec-first face-array)))
          (append-strand poly (vec-last face-array) (vec-first face-array)))))
        
    poly))

;;;; curve-source-protocol =====================================================

;;; return a list of "curves" where each curve is an array of points
(defmethod provides-curve-source-protocol? ((poly poly-strand))
  t)

(defmethod source-curves ((poly poly-strand))
  (let ((curves '()))
    (dotimes (s (length (strands poly)))
      (push (strand-points poly s) curves))
    (nreverse curves)))

(defmethod source-curves-closed ((poly poly-strand))
  (make-list (length (strands poly)) :initial-element nil)) ;always open
