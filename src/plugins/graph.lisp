(in-package #:kons-9)


;;;; graph-node ================================================================

(defclass-kons-9 graph-node (shape json-mixin)
  ((graph-ref nil)
   (graph-index 0)
   (geometry (rotate-to (make-circle-polyhedron 1.0 16) (p! -90 0 0)))
   (color (c! 1 1 1))
   (graph-edges (make-array 0 :adjustable t :fill-pointer t)) ;graph-nodes this node has links to
   (layer-value 0)
   ))

(defmethod (setf color) :after (col (node graph-node))
  (when (geometry node)
    (set-point-colors (geometry node) col)))
  
(defmethod (setf geometry) :after (geo (node graph-node))
  (when (color node)
    (set-point-colors geo (color node))))

(defmethod get-bounds ((node graph-node))
  (if (geometry node)
      (get-bounds (geometry node))
      (values (p! -1 -1 -1) (p! 1 1 1))))

(defmethod node-location ((node graph-node))
  (offset (translate (transform node))))

(defmethod draw ((node graph-node))
  (when (geometry node)
    (draw (geometry node))))

(defmethod node-edge-depth ((node graph-node))
  (if (= 0 (length (graph-edges node)))
      0
      (+ 1 (reduce #'max (map 'vector #'node-edge-depth (graph-edges node))))))

;;;; graph =====================================================================

(defclass-kons-9 graph (shape-group)
  ((hash-data nil)
   ;; graph nodes stored in group's children slot for rendering
   (graph-nodes (make-array 0 :adjustable t :fill-pointer t))
   (show-names? nil)
   (show-links? t)
   (layout-style :2d)
   ;; interactor (optional)
   (interactor nil)))

(defmethod find-node ((graph graph) ref)
  (do-array (i n (graph-nodes graph))
    (when (equal ref (graph-ref n))
      (return-from find-node n)))
  nil)

(defmethod draw ((graph graph))
  ;; draw children/components
  (call-next-method)
  ;; draw links
  (when (show-links? graph)
    (let ((lines '()))
      (do-array (i n1 (graph-nodes graph))
        (when (is-visible? n1)
          (do-array (j n2 (graph-edges n1))
            (when (is-visible? n2)
              (push (node-location n2) lines)
             (push (node-location n1) lines)))))
      (if (or (eq :layered (layout-style graph)) (eq :tree (layout-style graph)))
          (3d-draw-smooth-lines lines)
          (3d-draw-tapered-lines lines 1.0 0.1 2)))))

(defun standard-json-graph-node-fn (node-class
                                    nodes-attr node-ref-attr node-name-attr
                                    node-x-attr node-y-attr node-layer-attr)
  (lambda (g)
    (do-array (i node (gethash nodes-attr (hash-data g)))
      ;; add nodes to graph
      (let ((gnode (make-instance node-class
                                  :hash-data node
                                  :graph-ref (gethash node-ref-attr node)
                                  :layer-value (if node-layer-attr
                                                   (read-from-string (gethash node-layer-attr node))
                                                   0)
                                  :name (gethash node-name-attr node)
                                  :show-name? (show-names? g))))
        ;; put graph on XZ plane
        (when (and node-x-attr node-y-attr)
          (translate-to gnode (p! (gethash node-x-attr node) 0.0 (gethash node-y-attr node))))
        (vector-push-extend gnode (graph-nodes g))))))

(defun standard-json-graph-edge-fn (edges-attr edge-from-attr edge-to-attr edge-to-is-array)
  (lambda (g)
    (if (not edge-to-is-array)
        ;; single link
        (do-array (i edge (gethash edges-attr (hash-data g)))
          (let ((node1 (find-node g (gethash edge-from-attr edge)))
                (node2 (find-node g (gethash edge-to-attr edge))))
            (when (and node1 node2)
              (vector-push-extend node2 (graph-edges node1)))))
        ;; array of links
        (do-array (i edge (gethash edges-attr (hash-data g)))
          (let ((node1 (find-node g (gethash edge-from-attr edge)))
                (nodes-to (remove-if #'null (map 'vector (lambda (ref) (find-node g ref))
                                                 (gethash edge-to-attr edge)))))
            (when (and node1 (> (length nodes-to) 0))
              (do-array (j n nodes-to)
                (vector-push-extend n (graph-edges node1)))))))))

(defun make-json-graph (filename &key (graph-class 'graph) (node-class 'graph-node)
                                   (show-names? nil) (show-links? t)
                                   nodes-attr node-ref-attr node-name-attr
                                   node-x-attr node-y-attr node-layer-attr
                                   edges-attr edge-from-attr edge-to-attr
                                   (edge-to-is-array nil))
  (let ((graph (make-instance graph-class
                              :hash-data (load-json filename)
                              :show-links? show-links?
                              :show-names? show-names?)))
    (build-graph-from-json graph
     :node-fn (standard-json-graph-node-fn node-class
                                           nodes-attr node-ref-attr node-name-attr
                                           node-x-attr node-y-attr node-layer-attr)
     :edge-fn (standard-json-graph-edge-fn edges-attr edge-from-attr edge-to-attr edge-to-is-array))
    (set-graph-attributes graph)
    graph))

(defun make-json-graph-SAV (filename &key (graph-class 'graph) (node-class 'graph-node)
                                   (show-names? nil) (show-links? t)
                                   nodes-attr node-ref-attr node-name-attr
                                   node-x-attr node-y-attr node-layer-attr
                                   edges-attr edge-from-attr edge-to-attr
                                   (edge-to-is-array nil))
  (let ((graph (make-instance graph-class
                              :hash-data (load-json filename)
                              :show-links? show-links?
                              :show-names? show-names?)))
    (build-graph-from-json graph
     :node-fn (lambda (g)               ;build nodes
                (do-array (i node (gethash nodes-attr (hash-data g)))
                  ;; add nodes to graph
                  (let ((gnode (make-instance node-class
                                              :hash-data node
                                              :graph-ref (gethash node-ref-attr node)
                                              :layer-value (if node-layer-attr
                                                               (read-from-string (gethash node-layer-attr node))
                                                               0)
                                              :name (gethash node-name-attr node)
                                              :show-name? (show-names? g))))
                    ;; put graph on XZ plane
                    (when (and node-x-attr node-y-attr)
                      (translate-to gnode (p! (gethash node-x-attr node) 0.0 (gethash node-y-attr node))))
                    (vector-push-extend gnode (graph-nodes g)))))
     :edge-fn (lambda (g)               ;set node edges/links
                (if (not edge-to-is-array)
                    ;; single link
                    (do-array (i edge (gethash edges-attr (hash-data g)))
                      (let ((node1 (find-node g (gethash edge-from-attr edge)))
                            (node2 (find-node g (gethash edge-to-attr edge))))
                        (when (and node1 node2)
                          (vector-push-extend node2 (graph-edges node1)))))
                    ;; array of links
                    (do-array (i edge (gethash edges-attr (hash-data g)))
                      (let ((node1 (find-node g (gethash edge-from-attr edge)))
                            (nodes-to (remove-if #'null (map 'vector (lambda (ref) (find-node g ref))
                                                             (gethash edge-to-attr edge)))))
                        (when (and node1 (> (length nodes-to) 0))
                          (do-array (j n nodes-to)
                            (vector-push-extend n (graph-edges node1)))))))))
    (set-graph-attributes graph)
    graph))


(defun make-json-graph-fns (filename &key (graph-class 'graph)
                                       (show-names? nil) (show-links? t)
                                       (node-fn nil) (edge-fn nil) (layer-fn nil))
  (when (not (and node-fn edge-fn))
    (error "Missing NODE-FN and/or EDGE-FN arguments"))
  (let ((graph (make-instance graph-class
                              :hash-data (load-json filename)
                              :show-names? show-names?
                              :show-links? show-links?)))
    (build-graph-from-json graph
                           :node-fn node-fn
                           :edge-fn edge-fn
                           :layer-fn layer-fn)
    (set-graph-attributes graph)
    graph))

(defmethod set-graph-children ((graph graph))
  (remove-all-children graph)
  (do-array (i n (graph-nodes graph))
    (add-child graph n)             ;add child to shape-group
    (setf (graph-index n) i))       ;set graph-index value for nodes
  graph)

(defmethod build-graph-from-json ((graph graph) &key (node-fn nil) (edge-fn nil) (layer-fn nil))
  (when (not (and node-fn edge-fn))
    (error "Missing NODE-FN and/or EDGE-FN arguments"))
  ;; build nodes
  (funcall node-fn graph)
  ;; set node edges/links
  (funcall edge-fn graph)
  ;; set nodes layer value
  (when layer-fn
    (apply-nodes graph layer-fn))
  ;; add graph children
  (set-graph-children graph)
  graph)

(defmethod set-graph-attributes ((graph graph))
  ;; set component layer-value
  ;; set component sizes
  ;; set component vulnerability colors
  ;; set vulnerability sizes by max-severity-score
  ;; set vulnerability colors
  graph)

(defmethod filter-nodes ((graph graph) func)
  ;; remove non-matching nodes from graph
  (let ((nodes (remove-if-not func (graph-nodes graph))))
    (setf (graph-nodes graph) nodes)
    ;; remove filtered node from node edges
    (do-array (i node (graph-nodes graph))
      (setf (graph-edges node) (remove-if-not (lambda (n) (find n nodes))
                                              (graph-edges node)))))
  ;; update graph children
  (set-graph-children graph)
  graph)

(defmethod apply-nodes ((graph graph) func)
  (map nil func (graph-nodes graph))
  graph)



