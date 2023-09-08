(in-package #:kons-9)

;;;; refine-subdiv-mesh ===============================================================

;;; subclass of subdiv-mesh which does simple mesh refinement

(defclass-kons-9 refine-subdiv-mesh (subdiv-mesh)
  ())

(defmethod compute-subdiv-points ((mesh refine-subdiv-mesh) (subdiv refine-subdiv-mesh))
  (set-refine-face-vertex-points mesh subdiv)
  (set-refine-edge-vertex-points mesh subdiv)
  (set-refine-vertex-vertex-points mesh subdiv))

;;; the methods below are the same as for smooth-subdiv-mesh paper but simply assume
;;; all edges and vertices are boundary

(defun set-refine-face-vertex-points (mesh subdiv)
  (do-array (x h (sm-half-edges mesh))
    (let ((m (half-edge-cycle-length mesh h))
          (v (vertex h))
          (i (+ (length (sm-vertices mesh)) (face h))))
      (setf (point (sm-nth-vertex subdiv i))
            (p+ (point (sm-nth-vertex subdiv i))
                (p/ (point (sm-nth-vertex mesh v)) m))))))

(defun set-refine-edge-vertex-points (mesh subdiv)
  (do-array (x h (sm-half-edges mesh))
    (let ((v0 (vertex h))
          (v1 (vertex (sm-nth-half-edge mesh (next-half-edge h))))
          (j (+ (length (sm-vertices mesh)) (length (sm-faces mesh)) (edge h))))
      (setf (point (sm-nth-vertex subdiv j))
            (p/ (p+ (point (sm-nth-vertex mesh v0))
                    (point (sm-nth-vertex mesh v1)))
                2)))))

(defun set-refine-vertex-vertex-points (mesh subdiv)
  (do-array (x h (sm-half-edges mesh))
    (let ((v (vertex h)))
      (setf (point (sm-nth-vertex subdiv v))
            (p:copy (point (sm-nth-vertex mesh v)))))))

