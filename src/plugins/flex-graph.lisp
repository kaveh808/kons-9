(in-package #:kons-9)

;;;; flex-graph ================================================================

(defclass-kons-9 flex-graph (graph animator)
  ((num-edge-springs 0)
   (node-spring-probability 1.0)
   (poly-strand nil)
   (flex-animator nil)))

(defmethod update-motion ((graph flex-graph) parent-absolute-timing)
  (when (and (poly-strand graph) (flex-animator graph))
    ;; update spring-mass system
    (update-motion (flex-animator graph) parent-absolute-timing)
    ;; update graph layout
    (update-graph-layout graph (points (poly-strand graph)))))

(defmethod setup-graph-dynamics ((graph flex-graph) &key
                                                      (layout-style :2d)
                                                      (node-spring-probability 1.0)
                                                      link-spring-length
                                                      link-spring-stiffness
                                                      spacing-spring-length
                                                      spacing-spring-stiffness)
  (setf (layout-style graph) layout-style)
  (setf (node-spring-probability graph) node-spring-probability)
  ;; randomize graph layout
  (initialize-graph-layout graph :size spacing-spring-length)
  ;; build poly-strand and flex-animator
  (setf (poly-strand graph) (make-poly-strand graph))
  (let ((anim (make-flex-animator (poly-strand graph))))
    (setf (flex-animator graph) anim)
    (set-flex-vertex-attr anim 'do-collisions? nil)
    ;; set spring attrs
;;    (print (list (num-edge-springs graph) (length (springs anim))))
    (do-array (i spring (springs anim))
      (if (< i (num-edge-springs graph))
          (progn                            ;attraction spring
            (setf (rest-length spring) link-spring-length)
            (setf (stiffness spring) link-spring-stiffness))
          (progn                        ;repulsion spring
            (setf (rest-length spring) spacing-spring-length)
            (setf (stiffness spring) spacing-spring-stiffness))))
    (set-flex-vertex-attr anim 'damping 0.5)
    (set-flex-vertex-attr anim 'time-step 0.2)
    ))

(defmethod initialize-graph-layout ((graph flex-graph) &key (size 10.0))
  (do-array (i n (graph-nodes graph))
    (translate-to n (ecase (layout-style graph)
                      (:tree (p! (rand1 size) 0.0          (rand1 size)))
                      (:2d   (p! (rand1 size) 0.0          (rand1 size)))
                      (:layered (p! (rand1 size) 0.0          (rand1 size)))
                      (:3d   (p! (rand1 size) (rand1 size) (rand1 size))))))
  graph)

(defmethod update-graph-layout ((graph flex-graph) points)
  (do-array (i n (graph-nodes graph))
    (let ((p (aref points i)))
      (translate-to n (ecase (layout-style graph)
                        (:tree (p! (p:x p) (layer-value n) 0.0))
                        (:2d   (p! (p:x p) 0.0             (p:z p)))
                        (:layered (p! (p:x p) (layer-value n) (p:z p)))
                        (:3d   (p! (p:x p) (p:y p)         (p:z p))))))))

(defmethod make-poly-strand ((graph flex-graph))
  (let* ((poly (make-instance 'poly-strand)))
    ;; build poly-strand points
    (setf (points poly) (map 'vector #'node-location (graph-nodes graph)))
    ;; build component dependency springs
    (do-array (i n1 (graph-nodes graph))
      (do-array (j n2 (graph-edges n1))
        (append-strand poly (graph-index n1) (graph-index n2))))
    ;; store number of dependencies (i.e. dependency link springs)
    (setf (num-edge-springs graph) (length (strands poly)))
    ;; build inter-node springs for x% of cases
    (if (or (eq :layered (layout-style graph)) (eq :tree (layout-style graph)))
        (do-array (i n1 (graph-nodes graph))
          (do-array (j n2 (graph-nodes graph))
            (when (and (> j i)
                       (= (layer-value n1) (layer-value n2)) ;nodes in same layer only
                       (< (random 1.0) (node-spring-probability graph))) ;only for some pairs
              (append-strand poly (graph-index n1) (graph-index n2)))))
        (do-array (i n1 (graph-nodes graph))
          (do-array (j n2 (graph-nodes graph))
            (when (and (> j i)
                       (< (random 1.0) (node-spring-probability graph))) ;only for some pairs
              (append-strand poly (graph-index n1) (graph-index n2))))))
    poly))

