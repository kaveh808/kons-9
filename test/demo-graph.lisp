(in-package #:kons-9)

(defparameter *demo-graph-filename*
  (asdf:system-relative-pathname "kons-9" "test/data/miserables.json"))

;;; Demo 01 -- graph with single link data =====================================

(with-clear-scene
  (let ((graph (make-json-graph *demo-graph-filename*
                                :show-names? t
                                :nodes-attr "nodes"
                                :node-ref-attr "_id"
                                :node-name-attr "name"
                                :node-x-attr "x"
                                :node-y-attr "y"
                                :node-layer-attr "group"
                                :edges-attr "edges"
                                :edge-from-attr "_from"
                                :edge-to-attr "_to")))
    ;; resize and move to origin
    (apply-nodes graph (lambda (n) (scale-to n 30.0)))
    (scale-to graph 0.1)
    (center-at-origin graph)
    (add-shape *scene* graph)))

;;; Demo 02 -- graph with single link data =====================================
;;; set node colors

(with-clear-scene
  (let ((graph (make-json-graph *demo-graph-filename*
                                :show-names? t
                                :nodes-attr "nodes"
                                :node-ref-attr "_id"
                                :node-name-attr "name"
                                :node-x-attr "x"
                                :node-y-attr "y"
                                :node-layer-attr "group"
                                :edges-attr "edges"
                                :edge-from-attr "_from"
                                :edge-to-attr "_to")))
    ;; resize and move to origin
    (apply-nodes graph (lambda (n) (scale-to n 30.0)))
    (scale-to graph 0.1)
    (center-at-origin graph)
    ;; color nodes by layer value
    (apply-nodes graph
                 (lambda (n)
                   (set-point-colors (geometry n)
                                     (case (read-from-string (get-json-attr n "group"))
                                       (1 (c! 1 0 0))
                                       (2 (c! 0 .8 0))
                                       (3 (c! .3 .3 1))
                                       (4 (c! 1 1 0))
                                       (5 (c! 0 1 1))
                                       (6 (c! 1 0 1))
                                       (7 (c! 1 .5 0))
                                       (8 (c! .5 1 .5))
                                       (9 (c! .5 .5 .5))
                                       (10 (c! .5 .5 0))
                                       (otherwise (c! 1 1 1))))))
    (add-shape *scene* graph)))

;;; Demo 03 -- graph with custom data funcs ====================================

(with-clear-scene
  (let ((graph (make-json-graph-fns
                *demo-graph-filename*
                :show-names? t
                :node-fn (lambda (g)
                           (do-array (i node (gethash "nodes" (hash-data g)))
                             (let ((gnode (make-instance 'graph-node
                                                         :hash-data node
                                                         :graph-ref (gethash "_id" node)
                                                         :layer-value (read-from-string (gethash "group" node))
                                                         :name (gethash "name" node)
                                                         :show-name? t)))
                               ;; put graph on XZ plane
                               (translate-to gnode (p! (gethash "x" node) 0.0 (gethash "y" node)))
                               (vector-push-extend gnode (graph-nodes g)))))
                :edge-fn (lambda (g)
                           (do-array (i edge (gethash "edges" (hash-data g)))
                             (let ((node1 (find-node g (gethash "_from" edge)))
                                   (node2 (find-node g (gethash "_to" edge))))
                               (when (and node1 node2)
                                 (vector-push-extend node2 (graph-edges node1)))))))))
    ;; resize and move to origin
    (apply-nodes graph (lambda (n) (scale-to n 30.0)))
    (scale-to graph 0.1)
    (center-at-origin graph)
    (add-shape *scene* graph)))

;;; Demo 04 -- graph with single link data =====================================
;;; set node colors
;;; add interactor

(with-clear-scene
  (let ((graph (make-json-graph *demo-graph-filename*
                                :show-names? t
                                :nodes-attr "nodes"
                                :node-ref-attr "_id"
                                :node-name-attr "name"
                                :node-x-attr "x"
                                :node-y-attr "y"
                                :node-layer-attr "group"
                                :edges-attr "edges"
                                :edge-from-attr "_from"
                                :edge-to-attr "_to")))
    ;; resize and move to origin
    (apply-nodes graph (lambda (n) (scale-to n 30.0)))
    (scale-to graph 0.1)
    (center-at-origin graph)
    ;; color nodes by layer value
    (apply-nodes graph
                 (lambda (n)
                   (set-point-colors (geometry n)
                                     (case (read-from-string (get-json-attr n "group"))
                                       (1 (c! 1 0 0))
                                       (2 (c! 0 .8 0))
                                       (3 (c! .3 .3 1))
                                       (4 (c! 1 1 0))
                                       (5 (c! 0 1 1))
                                       (6 (c! 1 0 1))
                                       (7 (c! 1 .5 0))
                                       (8 (c! .5 1 .5))
                                       (9 (c! .5 .5 .5))
                                       (10 (c! .5 .5 0))
                                       (otherwise (c! 1 1 1))))))
    ;; build interactor -- use '-' and '=' to decrease/increase visibility based on node layer
    (setf (interactor graph)
          (let ((visibility-threshold 0))
            (make-instance
             'interactor
             :update-fn (lambda (key key-mods)
                          (declare (ignore key-mods))
                          (cond ((eq :equal key)
                                 (incf visibility-threshold))
                                ((eq :minus key)
                                 (decf visibility-threshold)))
                          (apply-nodes graph
                                       (lambda (n)
                                         (setf (is-visible? n)
                                               (> (layer-value n) visibility-threshold))))))))
    (add-shape *scene* graph)
    (setf (interactor *scene*) (interactor graph))))

;;; Demo 05 -- graph with single link data =====================================

(with-clear-scene
  (let ((graph (make-json-graph *demo-graph-filename*
                                :graph-class 'flex-graph
                                :show-names? t
                                :nodes-attr "nodes"
                                :node-ref-attr "_id"
                                :node-name-attr "name"
                                :node-x-attr "x"
                                :node-y-attr "y"
                                :node-layer-attr "group"
                                :edges-attr "edges"
                                :edge-from-attr "_from"
                                :edge-to-attr "_to")))
    (setup-graph-dynamics graph :layout-style :3d
                                :link-spring-length 10.0
                                :link-spring-stiffness 0.25
                                :spacing-spring-length 40.0
                                :spacing-spring-stiffness 0.01)
    (apply-nodes graph (lambda (n) (scale-to n 5.0)))
    ;; color nodes by layer value
    (apply-nodes graph
                 (lambda (n)
                   (set-point-colors (geometry n)
                                     (case (read-from-string (get-json-attr n "group"))
                                       (1 (c! 1 0 0))
                                       (2 (c! 0 .8 0))
                                       (3 (c! .3 .3 1))
                                       (4 (c! 1 1 0))
                                       (5 (c! 0 1 1))
                                       (6 (c! 1 0 1))
                                       (7 (c! 1 .5 0))
                                       (8 (c! .5 1 .5))
                                       (9 (c! .5 .5 .5))
                                       (10 (c! .5 .5 0))
                                       (otherwise (c! 1 1 1))))))
    (add-shape *scene* graph)
    (add-motion *scene* graph)
    (setf (end-frame *scene*) 10000)
    (update-scene *scene* 1000)))

;;; xxx

;;; Demo 01 -- build graph with single link data ===============================

(with-clear-scene
  (let ((graph (make-json-force-directed-graph *demo-graph-filename-single*
                                               :style :3d ;:layered
                                               :dynamics '(20.0 4.0 0.25 0.1)
                                               :node-spring-probability 1.0
                                               :show-names? t
                                               :nodes-attr "nodes"
                                               :node-ref-attr "_id"
                                               :node-name-attr "name"
                                               :node-layer-attr "group"
                                               :edges-attr "edges"
                                               :edge-from-attr "_from"
                                               :edge-to-attr "_to")))
    (setf (end-frame *scene*) 10000)    ;run for as long as desired
    (add-shape *scene* graph)
    (add-motion *scene* graph)))

(update-scene *scene* 1000)

;;; Demo 02 -- build graph with single link data -- apply ======================

(with-clear-scene
  (let ((graph (make-json-force-directed-graph *demo-graph-filename-single*
                                               :style :2d ;;:3d ;:layered
                                               :dynamics '(30.0 5.0 0.25 0.1)
                                               :node-spring-probability 1.0
                                               :show-names? t
                                               :nodes-attr "nodes"
                                               :node-ref-attr "_id"
                                               :node-name-attr "name"
                                               :node-layer-attr "group"
                                               :edges-attr "edges"
                                               :edge-from-attr "_from"
                                               :edge-to-attr "_to")))
    (apply-nodes graph (lambda (n) (scale-to (geometry n) (p! 4 4 4))))
    (apply-nodes graph
                 (lambda (n)
                   (set-point-colors (geometry n)
                                     (case (read-from-string (get-json-attr n "group"))
                                       (1 (c! 1 0 0))
                                       (2 (c! 0 .8 0))
                                       (3 (c! .3 .3 1))
                                       (4 (c! 1 1 0))
                                       (5 (c! 0 1 1))
                                       (6 (c! 1 0 1))
                                       (7 (c! 1 .5 0))
                                       (8 (c! .5 1 .5))
                                       (9 (c! .5 .5 .5))
                                       (10 (c! .5 .5 0))
                                       (otherwise (c! 1 1 1))))))
    (setf (end-frame *scene*) 10000)    ;run for as long as desired
    (add-shape *scene* graph)
    (add-motion *scene* graph)))

(update-scene *scene* 1000)

;;; Demo 03 -- build graph with custom data funcs ==============================

(with-clear-scene
  (let ((graph (make-json-force-directed-graph-fns
                *demo-graph-filename-single*
                :style :3d ;:layered
                :dynamics '(20.0 4.0 0.25 0.1)
                :show-names? t
                :node-fn (lambda (g)
                           (do-array (i node (gethash "nodes" (hash-data g)))
                             (vector-push-extend 
                              (make-instance 'graph-node
                                             :hash-data node
                                             :graph-ref (gethash "_id" node)
                                             :layer-value (read-from-string (gethash "group" node))
                                             :name (gethash "name" node)
                                             :show-name? t)
                              (graph-nodes g))))
                :edge-fn (lambda (g)
                           (do-array (i edge (gethash "edges" (hash-data g)))
                             (let ((node1 (find-node g (gethash "_from" edge)))
                                   (node2 (find-node g (gethash "_to" edge))))
                               (when (and node1 node2)
                                 (vector-push-extend node2 (graph-edges node1)))))))))
    (setf (end-frame *scene*) 10000)    ;run for as long as desired
    (add-shape *scene* graph)
    (add-motion *scene* graph)))

(update-scene *scene* 1000)

;;; Demo 04 -- build graph with array link data

;;; how to set layers for graph below?

(with-clear-scene
  (let ((graph (make-json-force-directed-graph *demo-graph-filename-array*
                                               :style :3d ;:layered
                                               :dynamics '(20.0 0.0 0.25 0.01)
                                               :node-spring-probability 1.0
                                               :show-names? nil
                                               :nodes-attr "components"
                                               :node-ref-attr "bom-ref"
                                               :node-name-attr "name"
                                               :node-layer-attr nil
                                               :edges-attr "dependencies"
                                               :edge-from-attr "ref"
                                               :edge-to-attr "dependsOn"
                                               :edge-to-is-array t)))
    (setf (end-frame *scene*) 10000)    ;run for as long as desired
    (add-shape *scene* graph)
    (add-motion *scene* graph)))

(update-scene *scene* 100)

;;; Demo 05 -- build graph with array link data, funcs, and layers
;;; -- very large file sbom5.json

(with-clear-scene
  (let ((graph (make-json-force-directed-graph-fns
                *demo-graph-filename-array*
                :style :layered
                :dynamics '(20.0 0.0 0.1 0.01)
                :node-spring-probability 0.1
                :show-names? nil
                :node-fn (lambda (g)
                           (do-array (i node (gethash "components" (hash-data g)))
                             (vector-push-extend 
                              (make-instance 'graph-node
                                             :hash-data node
                                             :graph-ref (gethash "bom-ref" node)
                                             :name (gethash "name" node)
                                             :show-name? nil)
                              (graph-nodes g))))
                 :edge-fn (lambda (g)
                            (do-array (i edge (gethash "dependencies" (hash-data g)))
                              (let ((node1 (find-node g (gethash "ref" edge)))
                                    (nodes-to (remove-if #'null
                                                         (map 'vector
                                                              (lambda (ref) (find-node g ref))
                                                              (gethash "dependsOn" edge)))))
                                (when (and node1 (> (length nodes-to) 0))
                                  (do-array (j n nodes-to)
                                    (vector-push-extend n (graph-edges node1)))))))
                :layer-fn (lambda (n)
                            (setf (layer-value n) (node-edge-depth n)))
                )))
    ;; set node colors

    ;; create and connect vulnerability nodes
    
    (setf (end-frame *scene*) 10000)    ;run for as long as desired
    (add-shape *scene* graph)
    (add-motion *scene* graph)))

(update-scene *scene* 100)
