(in-package #:kons-9)

;;;; scene draw functions ======================================================

(defgeneric draw (obj)
  
  (:method ((scene scene))
    (mapc #'draw (shapes scene)))

  (:method ((group group))
    (when (is-visible? group)
      (mapc #'draw (children group))))

  ;; push matrix and do transform operations before drawing
  (:method :before ((shape shape))
    (when (is-visible? shape)
      (let ((xform (transform shape)))
        (3d-push-matrix (transform-matrix xform)))))

  ;; draw a marker as a default
  (:method ((shape shape))
    (when (is-visible? shape)
      (3d-draw-marker 0.1)))

  ;; draw axis and pop matrix after drawing
  (:method :after ((shape shape))
    (when (is-visible? shape)
      (when (show-axis shape)
        (draw-axis shape))
      (if (is-selected? shape)
          (draw-selected shape)
          (when (show-bounds? shape)
            (draw-bounds shape)))
      (3d-pop-matrix)))

  (:method ((anim animation))
    (when (shape anim)
      (draw (shape anim))))
  
  (:method ((p-cloud point-cloud))
    (when (is-visible? p-cloud)
      (when *display-points?*
        (draw-points p-cloud))))

  (:method ((curve curve))
    (when (is-visible? curve)
      (when *display-wireframe?*
        (draw-wireframe curve))
      (when *display-points?*
        (draw-points curve))))

  (:method ((polyh polyhedron))
    (when (is-visible? polyh)
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
        (draw-normals polyh))))
  )

;;; shape helper methods -------------------------------------------------------

(defmethod draw-axis ((shape shape))
  (3d-draw-axis (show-axis shape)))

(defmethod draw-bounds ((shape shape) &optional (color (c! 0 1 1)))
  (multiple-value-bind (lo hi)
      (get-bounds shape)
    (3d-draw-bounds lo hi color)))

(defmethod draw-selected ((shape shape))
  (draw-bounds shape (c! 1 0 0)))

;;; point-cloud helper methods -------------------------------------------------

(defmethod draw-points ((p-cloud point-cloud))
  (3d-draw-points (points p-cloud)))

;;; curve helper methods -----------------------------------------------------
(defmethod draw-wireframe ((curve curve))
  (3d-draw-curve (points curve) (is-closed-curve? curve)))

;; (defmethod draw-points ((curve curve))
;;   (3d-draw-points (points curve)))

;;; polyhedron helper methods --------------------------------------------------

(defmethod draw-normals ((polyh polyhedron))
  (let ((lines ()))
    (dotimes (f (length (faces polyh)))
      (let* ((points (face-points polyh f))
             (p0 (apply #'p-average points))
             (p1 (p:+ p0 (p:scale (aref (face-normals polyh) f) (show-normals polyh)))))
        (push p1 lines)
        (push p0 lines)))
    (3d-draw-lines lines)))

