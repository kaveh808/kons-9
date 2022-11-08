(in-package #:kons-9)

;;;; stl format export =========================================================

(defmethod export-stl ((polyh polyhedron) filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-stl polyh stream)))

(defmethod write-stl ((polyh polyhedron) &optional (stream t))
  (format stream "solid ~A -- kons-9 stl export -- https://github.com/kaveh808/kons-9~%" (name polyh))
  (multiple-value-bind (lo hi)
      (get-bounds polyh)
    (declare (ignore hi))
    ;; move polyh so no negative coords, and then triangulate it
    (let ((tri-polyh (freeze-transform (translate-by (triangulate-polyhedron polyh) (p:negate lo)))))
      ;; export data
      (do-array (i f (faces tri-polyh))
        (let ((n (aref (face-normals tri-polyh) i)))
          (when (not (p:= n +origin+))    ;skip degenerate triangles
            (format stream "  facet normal ~E ~E ~E~%" (p:x n) (p:y n) (p:z n)) ;scientific notation
            (format stream "    outer loop~%")
            (dolist (vref f)
              (let ((v (aref (points tri-polyh) vref)))
                (format stream "      vertex ~E ~E ~E~%" (p:x v) (p:y v) (p:z v)))) ;scientific notation
            (format stream "    endloop~%")
            (format stream "  endfacet~%")))))
    (format stream "endsolid ~A~%" (name polyh))))




;;; stl export test

#|
(export-stl (make-superquadric 16 16 20.0 0.2 0.2) "~/superq.stl")
(export-stl (make-cube 20) "~/cube.stl")
|#


  

