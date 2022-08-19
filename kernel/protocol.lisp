(in-package #:kons-9)

;;;; point-source-protocol =====================================================

(defgeneric provides-point-source-protocol? (obj)
  (:method ((obj t)) nil)
  (:method ((p-cloud point-cloud)) t)
  )

(defgeneric source-points (obj)
  
  (:method ((obj t)) 
    (error "Method SOURCE-POINTS not implemented for object ~a" obj))

  (:method ((p-cloud point-cloud))
    (points p-cloud))

  (:method ((polyh polyhedron))
    (if (point-source-use-face-centers? polyh)
        (face-centers polyh)
        (call-next-method)))
  )

(defgeneric source-directions (obj)

  (:method ((obj t)) 
    (error "Method SOURCE-DIRECTIONS not implemented for object ~a" obj))

  (:method ((poly polygon))
    (curve-tangents poly))

  (:method ((p-cloud point-cloud))
    ;; arbitrarily return (1 1 1) for use as velocity multiplier
    (make-array (length (points p-cloud))
                :initial-element (p! 1 1 1)))

  (:method ((polyh polyhedron))
    (if (point-source-use-face-centers? polyh)
        (face-normals polyh)
        (point-normals polyh)))
  )

(defgeneric source-radial-directions (obj)
  (:method ((obj t)) 
    (map 'vector #'p-normalize (source-points obj)))
  )

(defgeneric source-closest-point (obj point)
  (:method ((obj t) point)
    (let* ((points (source-points obj))
           (min-dist (p-dist point (aref points 0)))
           (closest-index 0))
      (doarray (i p points)
               (let ((dist (p-dist point p)))
                 (when (< dist min-dist)
                   (setf min-dist dist)
                   (setf closest-index i))))
      (aref points closest-index)))
  )

;;;; curve-source-protocol =====================================================

(defgeneric provides-curve-source-protocol? (obj)
  (:method ((obj t)) nil)
  (:method ((poly polygon)) t)
  (:method ((polyh polyhedron)) t)
  )

(defgeneric source-curves (obj)
  (:method ((obj t)) 
    (error "Method SOURCE-CURVES not implemented for object ~a" obj))

  (:method ((poly polygon))
    ;; (list (coerce (points poly) 'array))
    (list (points poly))
    )

  (:method ((polyh polyhedron))
    (let ((curves '()))
      (dotimes (f (length (faces polyh)))
        (push (face-points polyh f) curves))
      (nreverse curves)))
  )

(defgeneric source-curves-closed (obj)
  (:method ((obj t)) 
    (error "Method SOURCE-CURVES-CLOSED not implemented for object ~a" obj))

  (:method ((poly polygon))
    (list (is-closed-polygon? poly)))

  (:method ((polyh polyhedron))
    (make-list (length (faces polyh)) :initial-element t)) ;always closed
  )

;; (defmethod source-curves-closed ((l-sys l-system))
;;   (make-list (length (faces l-sys)) :initial-element nil))


;;;; scene view protocol

(defgeneric set-needs-redisplay (scene-view))
(defgeneric draw-scene-view (scene-view))
(defgeneric accepts-first-responder (scene-view))
(defgeneric accepts-first-mouse (scene-view event))
(defgeneric mouse-down (scene-view event))
(defgeneric mouse-dragged (scene-view event))
(defgeneric key-down (scene-view event))
