(in-package #:kons-9)

;;;; subdiv-mesh ===============================================================

;;; half edge data structures for subdivision operations on meshes
;;; https://onrendering.com/data/papers/catmark/HalfedgeCatmullClark.pdf

(defclass-kons-9 sm-vertex ()
  ((mesh nil)
   (point (p! 0 0 0))
   (sm-half-edge nil)
   (vertex-type nil)                    ; :vertex, :edge, :face
   (is-boundary-vertex? nil)
   (selected? nil)))
  
(defclass-kons-9 sm-face ()
  ((mesh nil)
   (sm-half-edge nil)
   (selected? nil)))
  
(defclass-kons-9 sm-edge ()
  ((sm-half-edge nil)
   (sharpness 0)
   (is-boundary-edge? nil)
   (selected? nil)))

(defclass-kons-9 sm-half-edge ()
  ((mesh nil)
   (vertex nil)
   (face nil)
   (edge nil)
   (next-half-edge nil)
   (prev-half-edge nil)
   (pair-half-edge -1)))                  ;no pair

(defclass-kons-9 subdiv-mesh (polyhedron)
  ((sm-vertices   (make-array 0 :adjustable t :fill-pointer t))
   (sm-faces      (make-array 0 :adjustable t :fill-pointer t))
   (sm-edges      (make-array 0 :adjustable t :fill-pointer t))
   (sm-half-edges (make-array 0 :adjustable t :fill-pointer t))))

(defmethod initialize-instance :after ((mesh subdiv-mesh) &rest initargs)
  (declare (ignore initargs))
  (initialize-topology mesh))

(defmethod compute-subdiv-points ((mesh subdiv-mesh) (subdiv subdiv-mesh))
  (error "SUBDIV-MESH ~a has no COMPUTE-SUBDIV-POINTS method defined" mesh))

(defmethod add-vertex ((mesh subdiv-mesh) (vertex sm-vertex))
  (vector-push-extend vertex (sm-vertices mesh)))

(defmethod add-face ((mesh subdiv-mesh) (face sm-face))
  (vector-push-extend face (sm-faces mesh)))

(defmethod add-edge ((mesh subdiv-mesh) (edge sm-edge))
  (vector-push-extend edge (sm-edges mesh)))

(defmethod add-half-edge ((mesh subdiv-mesh) (edge sm-half-edge))
  (vector-push-extend edge (sm-half-edges mesh)))

(defmethod sm-nth-vertex ((mesh subdiv-mesh) n)
  (aref (sm-vertices mesh) n))

(defmethod sm-nth-face ((mesh subdiv-mesh) n)
  (aref (sm-faces mesh) n))

(defmethod sm-nth-edge ((mesh subdiv-mesh) n)
  (aref (sm-edges mesh) n))

(defmethod sm-nth-half-edge ((mesh subdiv-mesh) n)
  (aref (sm-half-edges mesh) n))

(defmethod sm-vertex-half-edges ((mesh subdiv-mesh) v)
  (let* ((e0 (sm-half-edge (sm-nth-vertex mesh v)))
         (e e0))
    (loop :do (setf e (next-half-edge (sm-nth-half-edge mesh (pair-half-edge (sm-nth-half-edge mesh e)))))
;;    (loop :do (setf e (prev-half-edge (sm-nth-half-edge mesh (pair-half-edge (sm-nth-half-edge mesh e)))))
          :collect (sm-nth-half-edge mesh e)
          :while (not (= e e0)))))

(defmethod sm-vertex-half-edges-indices ((mesh subdiv-mesh) v)
  (let* ((e0 (sm-half-edge (sm-nth-vertex mesh v)))
         (e e0))
    (loop :do (setf e (next-half-edge (sm-nth-half-edge mesh (pair-half-edge (sm-nth-half-edge mesh e)))))
;;    (loop :do (setf e (prev-half-edge (sm-nth-half-edge mesh (pair-half-edge (sm-nth-half-edge mesh e)))))
          :collect e
          :while (not (= e e0)))))

(defmethod sm-vertex-edges ((mesh subdiv-mesh) v)
  (mapcar (lambda (e) (sm-nth-edge mesh (edge e)))
          (sm-vertex-half-edges mesh v)))

(defmethod sm-vertex-edges-indices ((mesh subdiv-mesh) v)
  (mapcar (lambda (e) (edge e))
          (sm-vertex-half-edges mesh v)))

;;; TODO - sharp creases
;; (defmethod sm-vertex-sharpness ((mesh subdiv-mesh) v)
;;   (let ((edge-sharpness-list (mapcar #'sharpness (sm-vertex-edges mesh v))))
;;     (/ (reduce #'+ edge-sharpness-list) (length edge-sharpness-list))))

(defmethod sm-vertex-crease-edges ((mesh subdiv-mesh) v)
  (remove-if (lambda (e) (<= (sharpness e) 0.0)) (sm-vertex-edges mesh v)))


(defmethod sm-vertex-vertices ((mesh subdiv-mesh) v)
  (mapcar (lambda (e) (sm-nth-vertex mesh (vertex e)))
          (sm-vertex-half-edges mesh v)))

  ;; (let* ((e0 (sm-half-edge (sm-nth-vertex mesh v)))
  ;;        (e e0))
  ;;   (loop :do (setf e (prev-half-edge (sm-nth-half-edge mesh (pair-half-edge (sm-nth-half-edge mesh e)))))
  ;;         :collect (vertex (sm-nth-half-edge mesh e))
  ;;         :while (not (eq e e0)))))

(defmethod sm-vertex-faces ((mesh subdiv-mesh) v)
  (mapcar (lambda (e) (face (sm-nth-half-edge mesh e))) (sm-vertex-half-edges mesh v)))

  ;; (let* ((e0 (sm-half-edge (sm-nth-vertex mesh v)))
  ;;        (e e0))
  ;;   (loop :do (setf e (prev-half-edge (sm-nth-half-edge mesh (pair-half-edge (sm-nth-half-edge mesh e)))))
  ;;         :collect (face (sm-nth-half-edge mesh e))
  ;;         :while (not (eq e e0)))))

(defmethod sm-edge-vertices ((mesh subdiv-mesh) e)
  (let* ((h0 (sm-half-edge e))
         (h1 (next-half-edge (sm-nth-half-edge mesh h0))))
;;    (print (list e h0 h1 (vertex (sm-nth-half-edge mesh h0)) (vertex (sm-nth-half-edge mesh h1))))
    (list (sm-nth-vertex mesh (vertex (sm-nth-half-edge mesh h0)))
          (sm-nth-vertex mesh (vertex (sm-nth-half-edge mesh h1))))))

(defmethod sm-edge-vertices-index ((mesh subdiv-mesh) e)
  (let* ((h0 (sm-half-edge e))
         (h1 (next-half-edge (sm-nth-half-edge mesh h0))))
    (list (vertex (sm-nth-half-edge mesh h0))
          (vertex (sm-nth-half-edge mesh h1)))))

;; (defmethod sm-half-edge-faces ((edge sm-half-edge))
;;   (list (face edge) (face (pair-half-edge edge))))

;; (defmethod sm-face-half-edges ((face sm-face))
;;   (let* ((edge-0 (sm-half-edge face))
;;          (edge edge-0))
;;     (loop :do (setf edge (next-half-edge edge))
;;           :collect edge
;;           :while (not (eq edge edge-0)))))

;; (defmethod sm-face-faces ((face sm-face))
;;   (let* ((edge-0 (sm-half-edge face))
;;          (edge edge-0))
;;     (loop :do (setf edge (next-half-edge edge))
;;           :collect (face (pair-half-edge edge))
;;           :while (not (eq edge edge-0)))))

(defmethod initialize-topology ((mesh subdiv-mesh))
  (when (and (> (length (points mesh)) 0) (> (length (faces mesh)) 0))
    ;; create vertices
    (dotimes (i (length (points mesh)))
      (add-vertex mesh (make-instance 'sm-vertex :mesh mesh :point (aref (points mesh) i))))
    ;; create faces
    (dotimes (i (length (faces mesh)))
      (add-face mesh (make-instance 'sm-face :mesh mesh)))
    (let ((half-edge-index -1))
      ;; create half-edges
      (dotimes (i (length (faces mesh)))
        (let ((face-vertex-refs (aref (faces mesh) i))
              (first-half-edge-index nil)
              (prev-half-edge-index nil))
          (dotimes (j (length face-vertex-refs))
            (let* ((vref (elt face-vertex-refs j))
                   (half-edge (make-instance 'sm-half-edge :mesh mesh :vertex vref :face i)))
              (add-half-edge mesh half-edge)
              (incf half-edge-index)                 ;index of current edge
              (setf (sm-half-edge (sm-nth-vertex mesh (vertex half-edge))) half-edge-index) ;set vertex edge
              (when (= j 0)                              ;first edge
                (setf first-half-edge-index half-edge-index)
                (setf (sm-half-edge (sm-nth-face mesh (face half-edge))) half-edge-index)) ;set face edge
              (when (not (null prev-half-edge-index))
                (setf (next-half-edge (sm-nth-half-edge mesh prev-half-edge-index)) half-edge-index)
                (setf (prev-half-edge (sm-nth-half-edge mesh half-edge-index)) prev-half-edge-index))
              (when (= j (1- (length face-vertex-refs))) ;last edge
                (setf (next-half-edge (sm-nth-half-edge mesh half-edge-index)) first-half-edge-index)
                (setf (prev-half-edge (sm-nth-half-edge mesh first-half-edge-index)) half-edge-index))
              (setf prev-half-edge-index half-edge-index))))))
    ;; set half-edge pairs
    (loop for i from 0 below (length (sm-half-edges mesh))
          do (let ((half-edge-1 (aref (sm-half-edges mesh) i)))
               (loop for j from (1+ i) below (length (sm-half-edges mesh))
                     do (let ((half-edge-2 (aref (sm-half-edges mesh) j)))
                          (when (and (eq (vertex half-edge-1)
                                         (vertex (sm-nth-half-edge mesh (next-half-edge half-edge-2))))
                                     (eq (vertex half-edge-2)
                                         (vertex (sm-nth-half-edge mesh (next-half-edge half-edge-1)))))
                            ;; set half-edge pairs
                            (setf (pair-half-edge half-edge-1) j)
                            (setf (pair-half-edge half-edge-2) i))))))
    ;; create edges
    (loop for i from 0 below (length (sm-half-edges mesh))
          do (let* ((half-edge-1 (aref (sm-half-edges mesh) i))
                    (new-edge (if (null (edge half-edge-1))
                                  (make-instance 'sm-edge :sm-half-edge i
                                                          :is-boundary-edge? t) ;unset boundary below
                                  nil)))
               (when new-edge
                 (add-edge mesh new-edge)
                 (setf (edge half-edge-1) (1- (length (sm-edges mesh))))
                 (let* ((pair-index (pair-half-edge half-edge-1))
                        (half-edge-2 (if (= -1 pair-index)
                                         nil
                                         (sm-nth-half-edge mesh pair-index))))
                   (when half-edge-2
                     ;; set half-edge-2 edge to same as half-edge-1
                     (setf (edge half-edge-2) (edge half-edge-1))
                     ;; set edge is-boundary-edge? to nil since we have a pair
                     (setf (is-boundary-edge? (sm-nth-edge mesh (edge half-edge-1))) nil))))))
    ;; set boundary vertices
    (loop for i from 0 below (length (sm-half-edges mesh))
          do (let* ((half-edge-1 (aref (sm-half-edges mesh) i)))
               (when (= -1 (pair-half-edge half-edge-1))
                 (setf (is-boundary-vertex? (sm-nth-vertex mesh (vertex half-edge-1))) t)))))
  mesh)

(defmethod print-topology-dims ((mesh subdiv-mesh))
  (format t "~%")
  (format t "Vertices: ~a~%" (length (sm-vertices mesh)))
  (format t "Faces: ~a~%" (length (sm-faces mesh)))
  (format t "Edges: ~a~%" (length (sm-edges mesh)))
  (format t "Half-edges: ~a~%" (length (sm-half-edges mesh))))

(defmethod print-topology ((mesh subdiv-mesh))
  (format t "~%")
  (format t "Vertices:~%")
  (do-array (i v (sm-vertices mesh))
    (format t "  ~d: ~a ~a ~a he ~a e ~a~%" i (sm-half-edge v) (vertex-type v) (point v) (sm-vertex-half-edges-indices mesh i) (sm-vertex-edges-indices mesh i)))
  (format t "Faces:~%")
  (do-array (i f (sm-faces mesh))
    (format t "  ~d: ~a~%" i (sm-half-edge f)))
  (format t "Edges:~%")
  (do-array (i e (sm-edges mesh))
    (format t "  ~d: ~a ~a ~a v ~a~%" i (sm-half-edge e) (is-boundary-edge? e) (selected? e) (sm-edge-vertices-index mesh e)))
  (format t "Half-edges:~%")
  (do-array (i e (sm-half-edges mesh))
    (format t "  ~d: v ~a f ~a e ~a n ~a p ~a t ~a~%" i (vertex e) (face e) (edge e) (next-half-edge e) (prev-half-edge e) (pair-half-edge e))))

(defmethod print-topology-diff ((mesh-1 subdiv-mesh) (mesh-2 subdiv-mesh))
  (format t "~%")
  (format t "Vertices: ~a~%" (- (length (sm-vertices mesh-1)) (length (sm-vertices mesh-2))))
  (dotimes (i (min (length (sm-vertices mesh-1)) (length (sm-vertices mesh-2))))
    (format t "  ~d: ~a ~a~%" i
            (= (sm-half-edge (aref (sm-vertices mesh-1) i))
               (sm-half-edge (aref (sm-vertices mesh-2) i)))
            (p- (point (aref (sm-vertices mesh-1) i))
                (point (aref (sm-vertices mesh-2) i)))))
  (format t "Faces: ~a~%" (- (length (sm-faces mesh-1)) (length (sm-faces mesh-2))))
  (dotimes (i (min (length (sm-faces mesh-1)) (length (sm-faces mesh-2))))
    (format t "  ~d: ~a~%" i (= (sm-half-edge (aref (sm-faces mesh-1) i))
                                (sm-half-edge (aref (sm-faces mesh-2) i)))))
  (format t "Edges: ~a ~a~%" (length (sm-edges mesh-1)) (length (sm-edges mesh-2)))
  (dotimes (i (min (length (sm-edges mesh-1)) (length (sm-edges mesh-2))))
    (format t "  ~d: ~a~%" i (eq (is-boundary-edge? (aref (sm-edges mesh-1) i))
                                 (is-boundary-edge? (aref (sm-edges mesh-2) i)))))
  (format t "Half-edges: ~a~%" (- (length (sm-half-edges mesh-1)) (length (sm-half-edges mesh-2))))
  (dotimes (i (min (length (sm-half-edges mesh-1)) (length (sm-half-edges mesh-2))))
    (let ((e1 (aref (sm-half-edges mesh-1) i))
          (e2 (aref (sm-half-edges mesh-2) i)))
      (format t "  ~d: v ~a f ~a e ~a [~a vs ~a] n ~a p ~a t ~a~%" i
              (= (vertex e1) (vertex e2))
              (= (face e1) (face e2))
              (= (edge e1) (edge e2)) (edge e1) (edge e2)
              (= (next-half-edge e1) (next-half-edge e2))
              (= (prev-half-edge e1) (prev-half-edge e2))
              (= (pair-half-edge e1) (pair-half-edge e2))
              ))))

;;; return array of all levels of a subdiv-mesh
(defmethod subdivide-mesh-into-array ((mesh subdiv-mesh) &optional (levels 1))
  (let* ((mesh-array (make-array levels))
         (curr-mesh mesh))
    (dotimes (i levels)
      (let ((level-mesh (if (= 0 i) mesh (subdivide-mesh curr-mesh 1))))
        (setf (aref mesh-array i) level-mesh)
        (setf curr-mesh level-mesh)))
    mesh-array))

(defmethod subdivide-mesh ((mesh subdiv-mesh) &optional (levels 1))
  (if (<= levels 0)
      mesh
      (subdivide-mesh (subdivide-mesh-1 mesh) (1- levels))))

(defmethod make-subdivided-mesh ((mesh subdiv-mesh))
  (let* ((nv  (+ (length (sm-vertices mesh)) (length (sm-edges mesh)) (length (sm-faces mesh))))
         (nf  (* 4 (length (sm-faces mesh))))
         (ne  (+ (* 2 (length (sm-edges mesh))) (faces-num-points-refs mesh)))
         (nhe (* 4 (length (sm-half-edges mesh)))))
    (make-instance (class-name (class-of mesh)) ;subdiv should be same class as mesh
                   :sm-vertices   (make-array-with-fn nv  (lambda () (make-instance 'sm-vertex)))
                   :sm-faces      (make-array-with-fn nf  (lambda () (make-instance 'sm-face)))
                   :sm-edges      (make-array-with-fn ne  (lambda () (make-instance 'sm-edge)))
                   :sm-half-edges (make-array-with-fn nhe (lambda () (make-instance 'sm-half-edge))))))

(defmethod subdivide-mesh-1 ((mesh subdiv-mesh))
  (let ((subdiv (make-subdivided-mesh mesh)))
    (compute-subdiv-topology mesh subdiv)
    ;; build faces before computing points as algorithm may require polyhedron normals (e.g. fractals)
    (build-polyhedron-faces subdiv)
    (compute-subdiv-points mesh subdiv)
    (set-polyhedron-points subdiv)
    subdiv))

;;; based on fig. 3 of above paper
(defmethod compute-subdiv-topology ((mesh subdiv-mesh) (subdiv subdiv-mesh))
  ;; edge subdivision references
  (do-array (h e (sm-half-edges mesh))
    (let ((e0 (sm-nth-half-edge subdiv (+ (* 4 h) 0)))
          (e1 (sm-nth-half-edge subdiv (+ (* 4 h) 1)))
          (e2 (sm-nth-half-edge subdiv (+ (* 4 h) 2)))
          (e3 (sm-nth-half-edge subdiv (+ (* 4 h) 3)))
          (vd (length (sm-vertices mesh)))
          (fd (length (sm-faces mesh)))
          (ed (length (sm-edges mesh))))
      ;; pair rule
      (setf (pair-half-edge e0)
            (if (= -1 (pair-half-edge e)) ;boundary
                -1
                (+ (* 4 (next-half-edge (sm-nth-half-edge mesh (pair-half-edge e)))) 3)))
      (setf (pair-half-edge e1) (+ (* 4 (next-half-edge e)) 2))
      (setf (pair-half-edge e2) (+ (* 4 (prev-half-edge e)) 1))
      (setf (pair-half-edge e3)
            (if (= -1 (pair-half-edge (sm-nth-half-edge mesh (prev-half-edge e)))) ;boundary
                -1
                (+ (* 4 (pair-half-edge (sm-nth-half-edge mesh (prev-half-edge e)))) 0)))
      ;; next rule
      (setf (next-half-edge e0) (+ (* 4 h) 1))
      (setf (next-half-edge e1) (+ (* 4 h) 2))
      (setf (next-half-edge e2) (+ (* 4 h) 3))
      (setf (next-half-edge e3) (+ (* 4 h) 0))
      ;; prev rule
      (setf (prev-half-edge e0) (+ (* 4 h) 3))
      (setf (prev-half-edge e1) (+ (* 4 h) 0))
      (setf (prev-half-edge e2) (+ (* 4 h) 1))
      (setf (prev-half-edge e3) (+ (* 4 h) 2))
      ;; vertex rule
      (setf (vertex e0) (vertex e))
      (setf (vertex e1) (+ vd fd (edge e)))
      (setf (vertex e2) (+ vd (face e)))
      (setf (vertex e3) (+ vd fd (edge (sm-nth-half-edge mesh (prev-half-edge e)))))
      ;; TODO -- are these necessary?
      (setf (sm-half-edge (sm-nth-vertex subdiv (vertex e0))) (+ (* 4 h) 0)) ;set vertex half-edge
      (setf (vertex-type  (sm-nth-vertex subdiv (vertex e0))) :vertex)
      (setf (sm-half-edge (sm-nth-vertex subdiv (vertex e1))) (+ (* 4 h) 1)) ;set vertex half-edge
      (setf (vertex-type  (sm-nth-vertex subdiv (vertex e1))) :edge)
      (setf (sm-half-edge (sm-nth-vertex subdiv (vertex e2))) (+ (* 4 h) 2)) ;set vertex half-edge
      (setf (vertex-type  (sm-nth-vertex subdiv (vertex e2))) :face)
      (setf (sm-half-edge (sm-nth-vertex subdiv (vertex e3))) (+ (* 4 h) 3)) ;set vertex half-edge
      (setf (vertex-type  (sm-nth-vertex subdiv (vertex e3))) :edge)
      ;; edge rule
      (let* ((hprime (prev-half-edge e))
             (eprime (sm-nth-half-edge mesh hprime)))
        (setf (edge e0) (if (> h (pair-half-edge e))
                            (* 2 (edge e))
                            (+ (* 2 (edge e)) 1)))
        (setf (edge e1) (+ (* 2 ed) h))
        (setf (edge e2) (+ (* 2 ed) hprime))
        (setf (edge e3) (if (> hprime (pair-half-edge eprime))
                            (+ (* 2 (edge eprime)) 1)
                            (* 2 (edge eprime)))))
      ;; face rule
      (setf (face e0) h)
      (setf (face e1) h)
      (setf (face e2) h)
      (setf (face e3) h)
      (setf (sm-half-edge (sm-nth-face subdiv h)) (* 4 h)) ;set face half-edge
      ))

  ;; set edge half-edge, and edge and vertex boundary flags
  (do-array (i e (sm-half-edges subdiv))
    (setf (sm-half-edge (sm-nth-edge subdiv (edge e))) i)
    (when (= -1 (pair-half-edge e))
      (setf (is-boundary-edge? (sm-nth-edge subdiv (edge e))) t)
      (setf (is-boundary-vertex? (sm-nth-vertex subdiv (vertex e))) t)))

  ;; set edge selection flags
  (do-array (i e (sm-edges mesh))
    (setf (selected? (sm-nth-edge subdiv (* 2 i))) (selected? e))
    (setf (selected? (sm-nth-edge subdiv (+ (* 2 i) 1))) (selected? e)))

  subdiv)

(defmethod build-polyhedron-faces ((mesh subdiv-mesh))
  (let* ((num-faces (length (sm-faces mesh)))
         (prefs (make-array num-faces)))
    (dotimes (i num-faces)
      (setf (aref prefs i)            ;faces are quads
            (list (vertex (sm-nth-half-edge mesh (+ (* 4 i) 0)))
                  (vertex (sm-nth-half-edge mesh (+ (* 4 i) 1)))
                  (vertex (sm-nth-half-edge mesh (+ (* 4 i) 2)))
                  (vertex (sm-nth-half-edge mesh (+ (* 4 i) 3))))))
    (setf (faces mesh) prefs))
  mesh)

(defmethod set-polyhedron-points ((mesh subdiv-mesh))
  (setf (points mesh) (map 'vector #'point (sm-vertices mesh)))
  (compute-normals mesh)
  mesh)

(defmethod half-edge-cycle-length ((mesh subdiv-mesh) h)
  (let ((m 1)
        (h0 (sm-nth-half-edge mesh (next-half-edge h))))
    (loop :do (incf m)
              (setf h0 (sm-nth-half-edge mesh (next-half-edge h0)))
          :while (not (eq h0 h)))
    m))

(defmethod half-edge-valence ((mesh subdiv-mesh) h)
  (let ((n 1)
        (h0 (sm-nth-half-edge mesh (next-half-edge (sm-nth-half-edge mesh (pair-half-edge h))))))
    (loop :do (incf n)
              (setf h0 (sm-nth-half-edge mesh (next-half-edge (sm-nth-half-edge mesh (pair-half-edge h0)))))
          :while (not (eq h0 h)))
    n))


(defmethod select-vertex ((mesh subdiv-mesh) i)
  (setf (selected? (aref (sm-vertices mesh) i)) t))

(defmethod select-face ((mesh subdiv-mesh) i)
  (setf (selected? (aref (sm-faces mesh) i)) t))

(defmethod select-edge ((mesh subdiv-mesh) i)
  (setf (selected? (aref (sm-edges mesh) i)) t))

(defmethod select-edges ((mesh subdiv-mesh) edges)
  (map 'vector (lambda (e) (select-edge mesh e)) edges))

(defmethod selected-edges ((mesh subdiv-mesh))
  (remove-if (lambda (e) (not (selected? e))) (sm-edges mesh)))


(defmethod draw ((mesh subdiv-mesh))
;  (print-topology mesh) 
  (call-next-method)
  (when (is-visible? mesh)
    (draw-selected-faces mesh)
    (draw-selected-edges mesh)
    (draw-selected-points mesh)))

(defmethod draw-selected-faces ((mesh subdiv-mesh))
  (3d-draw-highlighted-polygons (points mesh) (faces mesh) (face-normals mesh) (point-normals mesh)
                                (map 'vector (lambda (f) (selected? f)) (sm-faces mesh))))

(defmethod draw-selected-edges ((mesh subdiv-mesh))
  (let ((selected-edges ()))
    (do-array (i edge (sm-edges mesh))
      (when (selected? edge)
        (let ((vertices (sm-edge-vertices mesh edge)))
          (push (point (elt vertices 0)) selected-edges)
          (push (point (elt vertices 1)) selected-edges))))
    (3d-draw-lines selected-edges :highlight? t)))

(defmethod draw-selected-points ((mesh subdiv-mesh))
  (let ((selected-points ()))
    (dotimes (i (length (points mesh)))
      (when (selected? (aref (sm-vertices mesh) i))
        (push (aref (points mesh) i) selected-points)))
    (3d-draw-points (coerce selected-points 'vector) nil :highlight? t)))


#|
(defmethod verify-topology ((mesh subdiv-mesh))
  ;; vertices
  (loop for vertex across (sm-vertices mesh)
        do (when (null (sm-half-edge vertex))
             (error "VERIFY-TOPOLOGY -- null edge in vertex ~a" vertex))
           (when (not (eq vertex (vertex (sm-half-edge vertex))))
             (error "VERIFY-TOPOLOGY -- vertex edge mismatch ~a ~a" vertex (sm-half-edge vertex))))
  ;; faces
  (loop for face across (sm-faces mesh)
        do (when (null (sm-half-edge face))
             (error "VERIFY-TOPOLOGY -- null edge in face ~a" face))
           (when (not (eq face (face (sm-half-edge face))))
             (error "VERIFY-TOPOLOGY -- face edge mismatch ~a ~a" face (sm-half-edge face))))
  ;; edges
  (loop for edge across (sm-half-edges mesh)
        do (when (null (vertex edge))
             (error "VERIFY-TOPOLOGY -- null vertex in edge ~a" edge))
           (when (null (face edge))
             (error "VERIFY-TOPOLOGY -- null face in edge ~a" edge))
           (when (null (next-half-edge edge))
             (error "VERIFY-TOPOLOGY -- null next-half-edge in edge ~a" edge))
           (when (null (prev-half-edge edge))
             (error "VERIFY-TOPOLOGY -- null prev-half-edge in edge ~a" edge))
           (when (null (pair-half-edge edge))
             (error "VERIFY-TOPOLOGY -- null pair-half-edge in edge ~a" edge))
           (when (not (eq edge (next-half-edge (prev-half-edge edge))))
             (error "VERIFY-TOPOLOGY -- next-half-edge mismatch ~a ~a" edge (prev-half-edge edge)))
           (when (not (eq edge (prev-half-edge (next-half-edge edge))))
             (error "VERIFY-TOPOLOGY -- prev-half-edge mismatch ~a ~a" edge (next-half-edge edge)))
           (when (not (eq edge (pair-half-edge (pair-half-edge edge))))
             (error "VERIFY-TOPOLOGY -- pair-half-edge mismatch ~a ~a" edge (pair-half-edge edge))))
  t)
|#

;;; print topology
;;; add levels to subdiv-mesh
;;; do subdivision

