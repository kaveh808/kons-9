(in-package #:kons-9)

;;;; fractal-subdiv-mesh ===============================================================

;;; subclass of subdiv-mesh which does fractal mesh subdivision

(defclass-kons-9 fractal-subdiv-mesh (refine-subdiv-mesh)
  ((vertex-displacement 1.0)))

(defmethod compute-subdiv-points ((mesh fractal-subdiv-mesh) (subdiv fractal-subdiv-mesh))
  (call-next-method)                    ;generate refined vertex positions
  (setf (points subdiv) (map 'vector #'point (sm-vertices subdiv))) ;update polyhedron points
  (compute-normals subdiv)                                          ;update normals
  (set-fractal-points mesh subdiv))

(defun set-fractal-points (mesh subdiv)
  (let ((points (points subdiv))
        (normals (point-normals subdiv))
        (vertices (sm-vertices subdiv))
        (displacement (vertex-displacement mesh)))
    (loop for i from (length (sm-vertices mesh)) below (length (sm-vertices subdiv))
          do (let ((p (aref points i))
                   (n (aref normals i)))
               (setf (point (aref vertices i))
                     (p+ p (p* n (rand1 displacement))))))
    (setf (vertex-displacement subdiv) (/ displacement 2.0))))
