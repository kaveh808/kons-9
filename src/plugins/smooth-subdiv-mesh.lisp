(in-package #:kons-9)

;;;; smooth-subdiv-mesh ========================================================

;;; subclass of subdiv-mesh which does smooth mesh refinement
;;; smoothing based on https://onrendering.com/data/papers/catmark/HalfedgeCatmullClark.pdf

(defclass-kons-9 smooth-subdiv-mesh (subdiv-mesh)
  ())

(defmethod compute-subdiv-points ((mesh smooth-subdiv-mesh) (subdiv smooth-subdiv-mesh))
  (set-smooth-face-vertex-points mesh subdiv)
  (set-smooth-edge-vertex-points mesh subdiv)
  (set-smooth-vertex-vertex-points mesh subdiv))
    ;; TODO - sharp creases
    ;; (set-subdiv-edge-vertex-crease-points mesh subdiv)
    ;; (set-subdiv-vertex-vertex-crease-points mesh subdiv)
    ;; (set-subdiv-edge-sharpness mesh subdiv)

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

;;; TODO - sharp creases
;; (defun set-smooth-edge-vertex-crease-points (mesh subdiv)
;;   (do-array (x h (sm-half-edges mesh))
;;     (when (> (sharpness (sm-nth-edge mesh (edge h))) 0)
;;       (let ((v0 (vertex h))
;;             (v1 (vertex (sm-nth-half-edge mesh (next-half-edge h))))
;;             (j (+ (length (sm-vertices mesh)) (length (sm-faces mesh)) (edge h))))
;;         (setf (point (sm-nth-vertex subdiv j))
;;               (p:lerp (point (sm-nth-vertex subdiv j))
;;                       (p/ (p+ (point (sm-nth-vertex mesh v0))
;;                               (point (sm-nth-vertex mesh v1)))
;;                           2)
;;                       (sharpness (sm-nth-edge mesh (edge h)))))))))

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

;;; TODO - sharp creases
;; (defun set-smooth-vertex-vertex-crease-points (mesh subdiv)
;;   (do-array (x h (sm-half-edges mesh))
;;     (let* ((v (vertex h))
;;            (sharpness (sm-vertex-sharpness mesh v)))
;;       (when (> sharpness 0)
;;         (setf (point (sm-nth-vertex subdiv v))
;;               (p:lerp (point (sm-nth-vertex subdiv v))
;;                       (point (sm-nth-vertex mesh v))
;;                       sharpness))))))

;;; TODO - sharp creases
;; (defun set-smooth-edge-sharpness (mesh subdiv)
;;   (do-array (i e (sm-edges mesh))
;;     (setf (sharpness (sm-nth-edge subdiv (* 2 i))) (sharpness e))
;;     (setf (sharpness (sm-nth-edge subdiv (+ (* 2 i) 1))) (sharpness e))))

