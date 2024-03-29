(in-package #:kons-9)

;;;; point-source-protocol =====================================================

(defgeneric provides-point-source-protocol? (obj)
  (:method ((obj t)) nil)
  (:method ((p-cloud point-cloud)) t)
  )

(defgeneric point-source-data (obj)
  
  (:method ((obj t))
    (error "Method POINT-SOURCE-DATA not implemented for object ~a" obj))

  (:method ((p-cloud point-cloud))
    (let ((n (length (points p-cloud))))
      (values (points p-cloud)
              (or (point-colors p-cloud)
                  (make-array-with-fn n (lambda () (c! 0 0 0))))
              (make-array-with-fn n #'p-rand))))
    
  (:method ((curve curve))
    (let ((n (length (points curve))))
      (values (points curve)
              (or (point-colors curve)
                  (make-array-with-fn n (lambda () (c! 0 0 0))))
              (curve-tangents curve))))
    
  (:method ((polyh polyhedron))
    (let ((n (length (points polyh))))
      (if (point-source-use-face-centers? polyh)
          (values (face-centers polyh)
                  (face-colors polyh)
                  (face-normals polyh))
          (values (points polyh)
                  (or (point-colors polyh)
                      (make-array-with-fn n (lambda () (c! 0 0 0))))
                  (point-normals polyh))))))

;; (defgeneric source-random-directions (obj)
;;   (:method ((obj t))
;;     (let ((dir (make-array (length (source-points obj)))))
;;       (dotimes (i (length dir))
;;         (setf (aref dir i) (p-rand)))
;;       dir)))

;; (defgeneric source-radial-directions (obj)
;;   (:method ((obj t)) 
;;     (map 'vector #'p:normalize (source-points obj))))

;; (defgeneric source-closest-point (obj point)
;;   (:method ((obj t) point)
;;     (let* ((points (source-points obj))
;;            (min-dist (p-dist point (aref points 0)))
;;            (closest-index 0))
;;       (do-array (i p points)
;;         (let ((dist (p-dist point p)))
;;           (when (< dist min-dist)
;;             (setf min-dist dist)
;;             (setf closest-index i))))
;;       (aref points closest-index))))

;;;; curve-source-protocol =====================================================

;;; return a list of "curves" where each curve is an array of points
(defgeneric provides-curve-source-protocol? (obj)
  (:method ((obj t)) nil)
  (:method ((curve curve)) t)
  (:method ((polyh polyhedron)) t)
  )

(defgeneric source-curves (obj)
  (:method ((obj t)) 
    (error "Method SOURCE-CURVES not implemented for object ~a" obj))

  (:method ((curve curve))
    ;; (list (coerce (points curve) 'array))
    (list (points curve))
    )

  (:method ((polyh polyhedron))
    (let ((curves '()))
      (dotimes (f (length (faces polyh)))
        (push (face-points-array polyh f) curves))
      (nreverse curves)))
  )

(defgeneric source-curves-closed (obj)
  (:method ((obj t)) 
    (error "Method SOURCE-CURVES-CLOSED not implemented for object ~a" obj))

  (:method ((curve curve))
    (list (is-closed-curve? curve)))

  (:method ((polyh polyhedron))
    (make-list (length (faces polyh)) :initial-element t)) ;always closed
  )

;; (defmethod source-curves-closed ((l-sys l-system))
;;   (make-list (length (faces l-sys)) :initial-element nil))


