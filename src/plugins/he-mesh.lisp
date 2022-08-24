(in-package #:kons-9)

;;;; half edge mesh =====================================================

(defclass he-vertex ()
  ((point :accessor point :initarg :point :initform (p! 0 0 0))
   (h-edge :accessor h-edge :initarg :h-edge :initform nil)
   (selected? :accessor selected? :initarg :selected? :initform nil)))
  
(defclass he-face ()
  ((h-edge :accessor h-edge :initarg :h-edge :initform nil)
   (selected? :accessor selected? :initarg :selected? :initform nil)))
  
(defclass he-edge ()
  ((vertex :accessor vertex :initarg :vertex :initform nil)
   (face :accessor face :initarg :face :initform nil)
   (next-edge :accessor next-edge :initarg :next-edge :initform nil)
   (prev-edge :accessor prev-edge :initarg :prev-edge :initform nil)
   (pair-edge :accessor pair-edge :initarg :pair-edge :initform nil)
   (selected? :accessor selected? :initarg :selected? :initform nil)))   
  
(defclass he-mesh (polyhedron)
  ((h-vertices :accessor h-vertices :initarg :h-vertices :initform (make-array 0 :adjustable t :fill-pointer t))
   (h-faces :accessor h-faces :initarg :h-faces :initform (make-array 0 :adjustable t :fill-pointer t))
   (h-edges :accessor h-edges :initarg :h-edges :initform (make-array 0 :adjustable t :fill-pointer t))))

(defmethod initialize-instance :after ((mesh he-mesh) &rest initargs)
  (declare (ignore initargs))
  (initialize-topology mesh))

(defmethod add-vertex ((mesh he-mesh) (vertex he-vertex))
  (vector-push-extend vertex (h-vertices mesh)))

(defmethod add-face ((mesh he-mesh) (face he-face))
  (vector-push-extend face (h-faces mesh)))

(defmethod add-edge ((mesh he-mesh) (edge he-edge))
  (vector-push-extend edge (h-edges mesh)))

(defmethod initialize-topology ((mesh he-mesh))
  (when (and (> (length (points mesh)) 0) (> (length (faces mesh)) 0))
    ;; create vertices
    (dotimes (i (length (points mesh)))
      (add-vertex mesh (make-instance 'he-vertex :point (aref (points mesh) i))))
    ;; create faces
    (dotimes (i (length (faces mesh)))
      (add-face mesh (make-instance 'he-face)))
    ;; create edges
    (dotimes (i (length (faces mesh)))
      (let ((face-vertex-refs (aref (faces mesh) i))
            (first-edge nil)
            (prev-edge nil))
        (dotimes (j (length face-vertex-refs))
          (let* ((vref (elt face-vertex-refs j))
                 (edge (make-instance 'he-edge
                                      :vertex (aref (h-vertices mesh) vref)
                                      :face (aref (h-faces mesh) i))))
            (add-edge mesh edge)
            (setf (h-edge (vertex edge)) edge)         ;set vertex edge
            (when (= j 0)                              ;first edge
              (setf first-edge edge)
              (setf (h-edge (face edge)) edge))        ;set face edge
            (when (not (null prev-edge))
              (setf (next-edge prev-edge) edge)
              (setf (prev-edge edge) prev-edge))
            (when (= j (1- (length face-vertex-refs))) ;last edge
              (setf (next-edge edge) first-edge)
              (setf (prev-edge first-edge) edge))
            (setf prev-edge edge)))))
    ;; set edge pairs
    (loop for i from 0 below (length (h-edges mesh))
          do (let ((edge-1 (aref (h-edges mesh) i)))
;               (format t "i: ~a~%" i)
               (loop for j from (1+ i) below (length (h-edges mesh))
                     do (let ((edge-2 (aref (h-edges mesh) j)))
;               (format t "  j: ~a~%" j)
                          (when (and (eq (vertex edge-1) (vertex (prev-edge edge-2)))
                                     (eq (vertex edge-2) (vertex (prev-edge edge-1))))
                            (setf (pair-edge edge-1) edge-2)
                            (setf (pair-edge edge-2) edge-1))))))))

(defmethod verify-topology ((mesh he-mesh))
  ;; vertices
  (loop for vertex across (h-vertices mesh)
        do (when (null (h-edge vertex))
             (error "VERIFY-TOPOLOGY -- null edge in vertex ~a" vertex))
           (when (neq vertex (vertex (h-edge vertex)))
             (error "VERIFY-TOPOLOGY -- vertex edge mismatch ~a ~a" vertex (h-edge vertex))))
  ;; faces
  (loop for face across (h-faces mesh)
        do (when (null (h-edge face))
             (error "VERIFY-TOPOLOGY -- null edge in face ~a" face))
           (when (neq face (face (h-edge face)))
             (error "VERIFY-TOPOLOGY -- face edge mismatch ~a ~a" face (h-edge face))))
  ;; edges
  (loop for edge across (h-edges mesh)
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

(defmethod select-vertex ((mesh he-mesh) i)
  (setf (selected? (aref (h-vertices mesh) i)) t))

(defmethod select-face ((mesh he-mesh) i)
  (setf (selected? (aref (h-faces mesh) i)) t))

(defmethod select-edge ((mesh he-mesh) i)
  (setf (selected? (aref (h-edges mesh) i)) t))

(defmethod draw ((mesh he-mesh))
  (call-next-method)
  
  (if *do-lighting?*
      (#_glEnable #$GL_LIGHTING)
      (#_glDisable #$GL_LIGHTING))

  (when *display-filled?*
    (if *do-smooth-shading?*
        (#_glShadeModel #$GL_SMOOTH)
        (#_glShadeModel #$GL_FLAT))
    (#_glPolygonMode #$GL_FRONT_AND_BACK #$GL_FILL)
    (draw-selected-faces mesh))

  (draw-selected-edges mesh)
  (draw-selected-points mesh))

(defmethod draw-selected-faces ((mesh he-mesh))
  (with-gl-enable #$GL_COLOR_MATERIAL
    (#_glColorMaterial #$GL_FRONT_AND_BACK #$GL_DIFFUSE)

    (dotimes (f (length (faces mesh)))
      (when (selected? (aref (h-faces mesh) f))
        (#_glBegin #$GL_POLYGON)
        (when (not *do-smooth-shading?*)
          (let ((n (aref (face-normals mesh) f)))
            (#_glNormal3f (x n) (y n) (z n))))

        (dolist (pref (aref (faces mesh) f))
          (#_glColor3f (c-red *sel-color*) (c-green *sel-color*) (c-blue *sel-color*))
          (when *do-smooth-shading?*
            (let ((n (aref (point-normals mesh) pref)))
              (#_glNormal3f (x n) (y n) (z n))))
          (let ((p (aref (points mesh) pref)))
            (#_glVertex3f (x p) (y p) (z p))))
        (#_glEnd)))))

(defmethod draw-selected-edges ((mesh he-mesh))
  (with-gl-disable #$GL_LIGHTING
    (gl-set-sel-color)
    (#_glLineWidth 7.0)
    (#_glBegin #$GL_LINES)
    (dotimes (i (length (h-edges mesh)))
      (let ((edge (aref (h-edges mesh) i)))
        (when (selected? edge)
          (let ((p0 (point (vertex edge)))
                (p1 (point (vertex (prev-edge edge)))))
            (#_glVertex3f (x p0) (y p0) (z p0))
            (#_glVertex3f (x p1) (y p1) (z p1))))))
    (#_glEnd)))

(defmethod draw-selected-points ((mesh he-mesh))
  (with-gl-disable #$GL_LIGHTING
    (gl-set-sel-color)
    (#_glPointSize 17.0)
    (#_glBegin #$GL_POINTS)
    (dotimes (i (length (points mesh)))
      (when (selected? (aref (h-vertices mesh) i))
        (let ((p (aref (points mesh) i)))
          (#_glVertex3f (x p) (y p) (z p)))))
    (#_glEnd)))

