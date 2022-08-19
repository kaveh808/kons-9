(in-package #:kons-9)

;;;; poly-mesh =================================================================

;;; half edge data structures for topological operations on meshes

(defclass pm-vertex ()
  ((point :accessor point :initarg :point :initform (p! 0 0 0))
   (pm-edge :accessor pm-edge :initarg :pm-edge :initform nil)
   (selected? :accessor selected? :initarg :selected? :initform nil)))
  
(defclass pm-face ()
  ((pm-edge :accessor pm-edge :initarg :pm-edge :initform nil)
   (selected? :accessor selected? :initarg :selected? :initform nil)))
  
(defclass pm-edge ()
  ((vertex :accessor vertex :initarg :vertex :initform nil)
   (face :accessor face :initarg :face :initform nil)
   (next-edge :accessor next-edge :initarg :next-edge :initform nil)
   (prev-edge :accessor prev-edge :initarg :prev-edge :initform nil)
   (pair-edge :accessor pair-edge :initarg :pair-edge :initform nil)
   (selected? :accessor selected? :initarg :selected? :initform nil)))   
  
(defclass poly-mesh (polyhedron)
  ((pm-vertices :accessor pm-vertices :initarg :pm-vertices :initform (make-array 0 :adjustable t :fill-pointer t))
   (pm-faces :accessor pm-faces :initarg :pm-faces :initform (make-array 0 :adjustable t :fill-pointer t))
   (pm-edges :accessor pm-edges :initarg :pm-edges :initform (make-array 0 :adjustable t :fill-pointer t))))

(defmethod initialize-instance :after ((mesh poly-mesh) &rest initargs)
  (declare (ignore initargs))
  (initialize-topology mesh))

(defmethod add-vertex ((mesh poly-mesh) (vertex pm-vertex))
  (vector-push-extend vertex (pm-vertices mesh)))

(defmethod add-face ((mesh poly-mesh) (face pm-face))
  (vector-push-extend face (pm-faces mesh)))

(defmethod add-edge ((mesh poly-mesh) (edge pm-edge))
  (vector-push-extend edge (pm-edges mesh)))

(defmethod initialize-topology ((mesh poly-mesh))
  (when (and (> (length (points mesh)) 0) (> (length (faces mesh)) 0))
    ;; create vertices
    (dotimes (i (length (points mesh)))
      (add-vertex mesh (make-instance 'pm-vertex :point (aref (points mesh) i))))
    ;; create faces
    (dotimes (i (length (faces mesh)))
      (add-face mesh (make-instance 'pm-face)))
    ;; create edges
    (dotimes (i (length (faces mesh)))
      (let ((face-vertex-refs (aref (faces mesh) i))
            (first-edge nil)
            (prev-edge nil))
        (dotimes (j (length face-vertex-refs))
          (let* ((vref (elt face-vertex-refs j))
                 (edge (make-instance 'pm-edge
                                      :vertex (aref (pm-vertices mesh) vref)
                                      :face (aref (pm-faces mesh) i))))
            (add-edge mesh edge)
            (setf (pm-edge (vertex edge)) edge)         ;set vertex edge
            (when (= j 0)                              ;first edge
              (setf first-edge edge)
              (setf (pm-edge (face edge)) edge))        ;set face edge
            (when (not (null prev-edge))
              (setf (next-edge prev-edge) edge)
              (setf (prev-edge edge) prev-edge))
            (when (= j (1- (length face-vertex-refs))) ;last edge
              (setf (next-edge edge) first-edge)
              (setf (prev-edge first-edge) edge))
            (setf prev-edge edge)))))
    ;; set edge pairs
    (loop for i from 0 below (length (pm-edges mesh))
          do (let ((edge-1 (aref (pm-edges mesh) i)))
;               (format t "i: ~a~%" i)
               (loop for j from (1+ i) below (length (pm-edges mesh))
                     do (let ((edge-2 (aref (pm-edges mesh) j)))
;               (format t "  j: ~a~%" j)
                          (when (and (eq (vertex edge-1) (vertex (prev-edge edge-2)))
                                     (eq (vertex edge-2) (vertex (prev-edge edge-1))))
                            (setf (pair-edge edge-1) edge-2)
                            (setf (pair-edge edge-2) edge-1))))))))

(defmethod verify-topology ((mesh poly-mesh))
  ;; vertices
  (loop for vertex across (pm-vertices mesh)
        do (when (null (pm-edge vertex))
             (error "VERIFY-TOPOLOGY -- null edge in vertex ~a" vertex))
           (when (neq vertex (vertex (pm-edge vertex)))
             (error "VERIFY-TOPOLOGY -- vertex edge mismatch ~a ~a" vertex (pm-edge vertex))))
  ;; faces
  (loop for face across (pm-faces mesh)
        do (when (null (pm-edge face))
             (error "VERIFY-TOPOLOGY -- null edge in face ~a" face))
           (when (neq face (face (pm-edge face)))
             (error "VERIFY-TOPOLOGY -- face edge mismatch ~a ~a" face (pm-edge face))))
  ;; edges
  (loop for edge across (pm-edges mesh)
        do (when (null (vertex edge))
             (error "VERIFY-TOPOLOGY -- null vertex in edge ~a" edge))
           (when (null (face edge))
             (error "VERIFY-TOPOLOGY -- null face in edge ~a" edge))
           (when (null (next-edge edge))
             (error "VERIFY-TOPOLOGY -- null next-edge in edge ~a" edge))
           (when (null (prev-edge edge))
             (error "VERIFY-TOPOLOGY -- null prev-edge in edge ~a" edge))
           (when (null (pair-edge edge))
             (error "VERIFY-TOPOLOGY -- null pair-edge in edge ~a" edge))
           (when (neq edge (next-edge (prev-edge edge)))
             (error "VERIFY-TOPOLOGY -- next-edge mismatch ~a ~a" edge (prev-edge edge)))
           (when (neq edge (prev-edge (next-edge edge)))
             (error "VERIFY-TOPOLOGY -- prev-edge mismatch ~a ~a" edge (next-edge edge)))
           (when (neq edge (pair-edge (pair-edge edge)))
             (error "VERIFY-TOPOLOGY -- pair-edge mismatch ~a ~a" edge (pair-edge edge))))
  t)

(defmethod select-vertex ((mesh poly-mesh) i)
  (setf (selected? (aref (pm-vertices mesh) i)) t))

(defmethod select-face ((mesh poly-mesh) i)
  (setf (selected? (aref (pm-faces mesh) i)) t))

(defmethod select-edge ((mesh poly-mesh) i)
  (setf (selected? (aref (pm-edges mesh) i)) t))

(defmethod draw ((mesh poly-mesh))
  (call-next-method)
  (draw-selected-faces mesh)
  (draw-selected-edges mesh)
  (draw-selected-points mesh))

(defmethod draw-selected-faces ((mesh poly-mesh))
  (3d-draw-highlighted-polygons (points mesh) (faces mesh) (face-normals mesh) (point-normals mesh)
                                (map 'vector (lambda (f) (selected? f)) (pm-faces mesh))))

(defmethod draw-selected-edges ((mesh poly-mesh))
  (let ((selected-edges ()))
    (dotimes (i (length (pm-edges mesh)))
      (let ((edge (aref (pm-edges mesh) i)))
        (when (selected? edge)
          (let ((p0 (point (vertex edge)))
                (p1 (point (vertex (prev-edge edge)))))
            (push p0 selected-edges)
            (push p1 selected-edges)))))
    (3d-draw-lines selected-edges :highlight? t)))

(defmethod draw-selected-points ((mesh poly-mesh))
  (let ((selected-points ()))
    (dotimes (i (length (points mesh)))
      (when (selected? (aref (pm-vertices mesh) i))
        (push (aref (points mesh) i) selected-points)))
    (3d-draw-points (coerce selected-points 'vector) :highlight? t)))

