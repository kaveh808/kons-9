(in-package #:kons-9)

;;;; smooth-subdiv-mesh ========================================================

;;; subclass of subdiv-mesh which does smooth mesh refinement
;;; smoothing based on https://onrendering.com/data/papers/catmark/HalfedgeCatmullClark.pdf
;;; edge sharpness based on https://graphics.pixar.com/library/Geri/paper.pdf

(defclass-kons-9 smooth-subdiv-mesh (subdiv-mesh)
  ())

(defmethod compute-subdiv-points ((mesh smooth-subdiv-mesh) (subdiv smooth-subdiv-mesh))
  (set-smooth-face-vertex-points mesh subdiv)
  (set-smooth-edge-vertex-points mesh subdiv)
  (set-smooth-vertex-vertex-points mesh subdiv)
  ;; TODO - sharp creases
  (set-smooth-edge-vertex-crease-points mesh subdiv)
  (set-smooth-vertex-vertex-crease-points mesh subdiv)
  (set-smooth-subdiv-edge-sharpness mesh subdiv)
  )

(defun set-smooth-face-vertex-points (mesh subdiv)
  (do-array (x h (sm-half-edges mesh))
    (let ((m (half-edge-cycle-length mesh h))
          (v (vertex h))
          (i (+ (length (sm-vertices mesh)) (face h))))
      (setf (point (sm-nth-vertex subdiv i))
            (p+ (point (sm-nth-vertex subdiv i))
                (p/ (point (sm-nth-vertex mesh v)) m))))))

(defun set-smooth-edge-vertex-points (mesh subdiv)
  (do-array (x h (sm-half-edges mesh))
    (if (is-boundary-edge? (sm-nth-edge mesh (edge h)))
        (let ((v0 (vertex h))
              (v1 (vertex (sm-nth-half-edge mesh (next-half-edge h))))
              (j (+ (length (sm-vertices mesh)) (length (sm-faces mesh)) (edge h))))
          (setf (point (sm-nth-vertex subdiv j))
                (p/ (p+ (point (sm-nth-vertex mesh v0))
                        (point (sm-nth-vertex mesh v1)))
                    2)))
        (let ((v (vertex h))
              (i (+ (length (sm-vertices mesh)) (face h)))
              (j (+ (length (sm-vertices mesh)) (length (sm-faces mesh)) (edge h))))
          (setf (point (sm-nth-vertex subdiv j))
                (p+ (point (sm-nth-vertex subdiv j))
                    (p/ (p+ (point (sm-nth-vertex mesh v))
                            (point (sm-nth-vertex subdiv i)))
                        4)))))))

;;; sharp creases
(defun smooth-edge-vertex-crease-point (mesh half-edge)
  (let ((v0 (vertex half-edge))
        (v1 (vertex (sm-nth-half-edge mesh (next-half-edge half-edge)))))
    (p/ (p+ (point (sm-nth-vertex mesh v0))
            (point (sm-nth-vertex mesh v1)))
        2)))
  
(defun set-smooth-edge-vertex-crease-points (mesh subdiv)
  (do-array (x h (sm-half-edges mesh))
    (when (not (is-boundary-edge? (sm-nth-edge mesh (edge h))))
      (let ((sharpness (sharpness (sm-nth-edge mesh (edge h))))
            (crease-point (smooth-edge-vertex-crease-point mesh h))
            (j (+ (length (sm-vertices mesh)) (length (sm-faces mesh)) (edge h))))
        (cond ((>= sharpness 1.0)         ;same as boundary case, set to crease point
               (setf (point (sm-nth-vertex subdiv j)) crease-point))
              ((> sharpness 0.0)          ;lerp smooth and crease points
               (setf (point (sm-nth-vertex subdiv j))
                     (p:lerp (point (sm-nth-vertex subdiv j))
                             crease-point
                             sharpness))))))))

(defun set-smooth-vertex-vertex-points (mesh subdiv)
  (do-array (x h (sm-half-edges mesh))
    (let ((v (vertex h)))
      (if (is-boundary-vertex? (sm-nth-vertex mesh v))
          (setf (point (sm-nth-vertex subdiv v))
                (p:copy (point (sm-nth-vertex mesh v))))
          (let ((n (half-edge-valence mesh h))
                (i (+ (length (sm-vertices mesh)) (face h)))
                (j (+ (length (sm-vertices mesh)) (length (sm-faces mesh)) (edge h))))
            (setf (point (sm-nth-vertex subdiv v))
                  (p+ (point (sm-nth-vertex subdiv v))
                      (p/ (p+ (p+ (p* (point (sm-nth-vertex subdiv j)) 4)
                                  (p:negate (point (sm-nth-vertex subdiv i))))
                              (p* (point (sm-nth-vertex mesh v)) (- n 3)))
                          (* n n)))))))))

;;; sharp creases
(defun smooth-vertex-vertex-crease-point (mesh half-edge)
  (let* ((v (vertex half-edge))
         (vtx (sm-nth-vertex mesh v))
         (crease-edges (sm-vertex-crease-edges mesh v)))
    (cond ((> (length crease-edges) 2) ;corner vertex
           (p:copy (point vtx)))
          ((= (length crease-edges) 2) ;vertex on crease
           (let ((edge-point-0 (point (sm-edge-other-vertex mesh (elt crease-edges 0) vtx)))
                 (edge-point-1 (point (sm-edge-other-vertex mesh (elt crease-edges 1) vtx))))
             ;; equation 9 in Pixar paper
             (p/ (p+ (p+ (p* (point vtx) 6) edge-point-0) edge-point-1) 8.0)))
          (t nil))))                 ;return nil and do nothing, use smoothed vertex
  
(defun set-smooth-vertex-vertex-crease-points (mesh subdiv)
  (do-array (x h (sm-half-edges mesh))
    (when (not (is-boundary-vertex? (sm-nth-vertex mesh (vertex h))))
      (let* ((sharpness (sharpness (sm-nth-edge mesh (edge h))))
             (crease-point (smooth-vertex-vertex-crease-point mesh h))
             (v (vertex h)))
        (when crease-point
          (cond ((>= sharpness 1.0)         ;same as boundary case, set to crease point
                 (setf (point (sm-nth-vertex subdiv v)) crease-point))
                ((> sharpness 0.0)          ;lerp smooth and crease points
                 (setf (point (sm-nth-vertex subdiv v))
                       (p:lerp (point (sm-nth-vertex subdiv v))
                               crease-point
                               sharpness)))))))))

(defun set-smooth-vertex-vertex-crease-points-SAV (mesh subdiv)
  (do-array (x h (sm-half-edges mesh))
    (let* ((v (vertex h))
           (vtx (sm-nth-vertex mesh v))
           (crease-edges (sm-vertex-crease-edges mesh v)))
      (cond ((> (length crease-edges) 2) ;corner vertex
             (setf (point (sm-nth-vertex subdiv v))
                   (p:copy (point vtx))))
            ((= (length crease-edges) 2) ;vertex on crease
             (let ((edge-point-0 (point (sm-edge-other-vertex mesh (elt crease-edges 0) vtx)))
                   (edge-point-1 (point (sm-edge-other-vertex mesh (elt crease-edges 1) vtx))))
               ;; equation 9 in Pixar paper
               (setf (point (sm-nth-vertex subdiv v))
                     (p/ (p+ (p+ (p* (point vtx) 6)
                                 edge-point-0)
                             edge-point-1)
                         8.0))))
            (t nil)))))
(defmethod sm-edge-other-vertex ((mesh subdiv-mesh) (edge sm-edge) (vertex sm-vertex))
  (let* ((vertices (sm-edge-vertices mesh edge)))
    (cond ((eq vertex (elt vertices 0))
           (elt vertices 1))
          ((eq vertex (elt vertices 1))
           (elt vertices 0))
          (t (error "Vertex ~a is not on edge ~a" vertex edge)))))

;;; TODO - sharp creases
(defun set-smooth-subdiv-edge-sharpness (mesh subdiv)
  (do-array (i e (sm-edges mesh))
    (setf (sharpness (sm-nth-edge subdiv (* 2 i))) (1- (sharpness e)))
    (setf (sharpness (sm-nth-edge subdiv (+ (* 2 i) 1))) (1- (sharpness e)))))

