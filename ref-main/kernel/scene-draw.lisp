(in-package #:kons-9)

;;;; scene draw functions ======================================================

(defgeneric draw (obj)
  
  (:method ((scene scene))
    (mapc #'draw (shapes scene)))

  (:method ((group group))
    (mapc #'draw (children group)))

  ;; push matrix and do transform operations before drawing
  (:method :before ((shape shape))
    (let ((xform (transform shape)))
      (3d-push-matrix (translate xform) (rotate xform) (scale xform))))

  ;; draw a marker as a default
  (:method ((shape shape))
    (3d-draw-marker 0.1))

  ;; draw axis and pop matrix after drawing
  (:method :after ((shape shape))
    (when (show-axis shape)
      (draw-axis shape))
    (if (is-selected? shape)
        (draw-selected shape)
        (when (show-bounds? shape)
          (draw-bounds shape)))
    (3d-pop-matrix))

  (:method ((p-cloud point-cloud))
    (when *display-points?*
      (draw-points p-cloud)))

  (:method ((poly polygon))
    (when *display-wireframe?*
      (draw-wireframe poly))
    (when *display-points?*
      (draw-points poly)))

  (:method ((polyh polyhedron))
    (when (or (= 0 (length (points polyh)))
              (= 0 (length (faces polyh))))
      (return-from draw))
    (3d-setup-lighting)
    (when *display-filled?*
      (3d-draw-filled-polygons (points polyh) (faces polyh)
                               (face-normals polyh) (point-normals polyh) (point-colors polyh)))
    (when *display-wireframe?*
      (3d-draw-wireframe-polygons (points polyh) (faces polyh)))
    (when *display-points?*
      (draw-points polyh))
    (when (show-normals polyh)
      (draw-normals polyh)))
  )

;;; shape helper methods -------------------------------------------------------

(defmethod draw-axis ((shape shape))
  (3d-draw-axis (show-axis shape)))

(defmethod draw-bounds ((shape shape) &optional (color (c! 0 1 1)))
  (multiple-value-bind (lo hi center)
      (bounds-and-center shape)
    (declare (ignore center))
    (3d-draw-bounds lo hi color)))

(defmethod draw-selected ((shape shape))
  (draw-bounds shape (c! 1 0 0)))

;;; point-cloud helper methods -------------------------------------------------

(defmethod draw-points ((p-cloud point-cloud))
  (3d-draw-points (points p-cloud)))

;;; polygon helper methods -----------------------------------------------------
(defmethod draw-wireframe ((poly polygon))
  (3d-draw-curve (points poly) (is-closed-polygon? poly)))

(defmethod draw-points ((poly polygon))
  (3d-draw-points (points poly)))

;;; polyhedron helper methods --------------------------------------------------

(defmethod draw-normals ((polyh polyhedron))
  (let ((lines ()))
    (dotimes (f (length (faces polyh)))
      (let* ((points (face-points polyh f))
             (p0 (p-center points))
             (p1 (p+ p0 (p-scale (aref (face-normals polyh) f) (show-normals polyh)))))
        (push p1 lines)
        (push p0 lines)))
    (3d-draw-lines lines)))

