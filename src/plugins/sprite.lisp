(in-package #:kons-9)

;;;; sprite ====================================================================

;;;; A sprite is a shape which always faces the camera.

(defclass-kons-9 sprite-instancer (shape)
  ((geometry nil)
   (point-source nil)))

;; (defmethod printable-data ((self uv-mesh))
;;   (strcat (call-next-method) (format nil ", dims (~a ~a)" (u-dim self) (v-dim self))))

;; (defmethod copy-instance-data ((dst uv-mesh) (src uv-mesh))
;;   (error "COPY-INSTANCE-DATA not implemented for UV-MESH"))

;;; + problem displaying with other shapes in scene
;;; - optimize by reducing calls to gl:get-float
;;; + write wrapper for (gl:get-float :modelview-matrix)
;;; + need to keep colors in sync with points
;;; + instance sprites on particles
;;; + test update-color-fn
;;; - add colors to demo-particles
;;; - karl sims waterfall
;;;   - collider shapes
;;;   - particle-shape intersection

;;; from https://www.reddit.com/r/lisp/comments/15l4mvg/sorting_two_sequences_in_sync_in_cl/
(defun reorder (x ord)
  (dotimes (dst (length ord) x)
    (let ((src (aref ord dst)))
      (when (< src dst) ; Find shuffled index
        (loop do (setf src (aref ord src))
              until (<= dst src)))
      (rotatef (aref x src) (aref x dst)))))

;; (defun test (&key (p #(3.0 2.0 1.0 1.5 1.5 5.0))
;;                   (c #(:r  :g  :b  :y  :c  :m)))
;;   (let ((order (make-array (length p) :element-type '(unsigned-byte 32))))
;;     (dotimes (i (length order)) (setf (aref order i) i))
;;     (setf order (sort order #'< :key (lambda (x) (aref p x))))
;;     (values (reorder p order)
;;             (reorder c order))))

(defmethod draw ((instancer sprite-instancer))
  (when (and (geometry instancer) (point-source instancer))
    (let* ((view-mtx (gl-get-float :modelview-matrix)) ; array[16]
           (view-dir (p! (aref view-mtx 2) (aref view-mtx 6) (aref view-mtx 10))) ; view z direction
           (points (source-points (point-source instancer)))
           (colors (source-point-colors (point-source instancer)))
           (order (make-array (length points) :element-type '(unsigned-byte 32))))
      (dotimes (i (length order)) (setf (aref order i) i))
      (setf order (sort order #'< :key (lambda (i) (p:dot (aref points i) view-dir))))
      (let ((ordered-points (reorder points order))
            (ordered-colors (if colors (reorder colors order) nil)))
        (do-array (i p ordered-points)
          (3d-push-matrix)
          (3d-translate p)
          (let ((matrix (make-matrix-from-vector (gl-get-float :modelview-matrix))))
            (setf (aref matrix 0 0) 1.0)
            (setf (aref matrix 1 0) 0.0)
            (setf (aref matrix 2 0) 0.0)
            (setf (aref matrix 0 1) 0.0)
            (setf (aref matrix 1 1) 1.0)
            (setf (aref matrix 2 1) 0.0)
            (setf (aref matrix 0 2) 0.0)
            (setf (aref matrix 1 2) 0.0)
            (setf (aref matrix 2 2) 1.0)
            (3d-load-matrix matrix)
            (when ordered-colors
              (gl-set-color (aref ordered-colors i)))
            (3d-draw-filled-curve (points (geometry instancer)))
            (3d-pop-matrix)))))))

