(in-package #:kons-9)

(defmacro def-procedural-curve (name slot-names-and-initforms class-options
                                  inputs-info &rest compute-expr)
  `(progn
     (defclass-kons-9 ,name (procedural-curve) ,slot-names-and-initforms ,@class-options)
     ,@(mapcar #'(lambda (input)
                   `(def-procedural-input ,name ,(first input)))
               inputs-info)
     (defmethod editable-slots ((poly ,name))
       (append (call-next-method) ',inputs-info))
     (defmethod compute-procedural-node ((poly ,name))
       (setf (points poly) ,@compute-expr))
     (defun ,(concat-syms 'make- name) ,(append (mapcar #'first slot-names-and-initforms)
                                         (list 'num-segments))
       (make-instance ',name ,@(flatten-list (mapcar #'(lambda (slot-info)
                                                         (let ((slot-name (first slot-info)))
                                                           (list (intern (symbol-name slot-name) "KEYWORD")
                                                                 slot-name)))
                                        (append slot-names-and-initforms '((num-segments ignore)))))))
     ))

;;;; procedural-curve ========================================================

(defclass-kons-9 procedural-curve (curve procedural-mixin)
  ((num-segments 64)))

(defmethod editable-slots ((self procedural-curve))
  (append (call-next-method) '((num-segments :number))))

(def-procedural-input procedural-curve num-segments)
(def-procedural-output procedural-curve points)

;;;; procedural curves =======================================================

(def-procedural-curve
    line
    ((p1 (p! 0 0 0))
     (p2 (p! 0 1 0)))
  ()
  ((p1 :point) (p2 :point))
  (make-line-points (p1 poly) (p2 poly) (num-segments poly)))

(def-procedural-curve
    rectangle
    ((width 2.0)
     (height 1.0))
  ()
  ((width :number) (height :number))
  (make-rectangle-points (width poly) (height poly) (num-segments poly)))

(def-procedural-curve
    square
    ((side 2.0))
  ()
  ((side :number))
  (make-rectangle-points (side poly) (side poly) (num-segments poly)))

(def-procedural-curve
    circle
    ((diameter 2.0))
  ()
  ((diameter :number))
  (make-circle-points (diameter poly) (num-segments poly)))

(def-procedural-curve
    arc
    ((diameter 2.0)
     (start-angle 0.0)
     (end-angle 90.0))
  ((:default-initargs
    :is-closed-curve? nil))
  ((diameter :number) (start-angle :number) (end-angle :number) (is-closed-curve? :boolean))
  (make-arc-points (diameter poly) (start-angle poly) (end-angle poly) (num-segments poly)))

(def-procedural-curve
    spiral
    ((start-diameter 0.0)
     (end-diameter 2.0)
     (axis-length 2.0)
     (num-loops 2.0))
  ((:default-initargs
    :is-closed-curve? nil))
  ((start-diameter :number) (end-diameter :number) (axis-length :number) (num-loops :number))
  (make-spiral-points (start-diameter poly) (end-diameter poly) (axis-length poly) (num-loops poly) (num-segments poly)))

(def-procedural-curve
    sine-curve
    ((period 360.0)
     (frequency 1.0)
     (x-scale 1.0)
     (y-scale 1.0))
  ((:default-initargs
    :is-closed-curve? nil))
  ((period :number) (frequency :number) (x-scale :number) (y-scale :number))
  (make-sine-curve-points (period poly) (frequency poly)
                          (x-scale poly) (y-scale poly) (num-segments poly)))

(def-procedural-curve
    star
    ((num-spikes 5)
     (outer-diameter 3.0)
     (inner-diameter 1.0))
  ()
  ((num-spikes :number) (outer-diameter :number) (inner-diameter :number))
  (make-star-points (num-spikes poly) (outer-diameter poly) (inner-diameter poly) (num-segments poly)))



;;;; gui =======================================================================

(defun procedural-curve-command-table ()
  (let ((table (make-instance `command-table :title "Create Procedural Curve")))
    (ct-make-shape :L "Line"
                   (let ((obj (make-line (p! 0 0 0) (p! 2 2 2) 4)))
                     (show-ui-content (make-scene-item-editor obj #'compute-procedural-node))
                     obj))                     
    (ct-make-shape :R "Rectangle"
                   (let ((obj (make-rectangle 2 1 4)))
                     (show-ui-content (make-scene-item-editor obj #'compute-procedural-node))
                     obj))
    (ct-make-shape :S "Square"
                   (let ((obj (make-square 1.5 1)))
                     (show-ui-content (make-scene-item-editor obj #'compute-procedural-node))
                     obj))                     
    (ct-make-shape :C "Circle"
                   (let ((obj (make-circle 2.0 16)))
                     (show-ui-content (make-scene-item-editor obj #'compute-procedural-node))
                     obj))                     
    (ct-make-shape :A "Arc"
                   (let ((obj (make-arc 2.0 0 90 8)))
                     (show-ui-content (make-scene-item-editor obj #'compute-procedural-node))
                     obj))                     
    (ct-make-shape :N "Sine Curve"
                   (let ((obj (make-sine-curve 360 1 2 1 16)))
                     (show-ui-content (make-scene-item-editor obj #'compute-procedural-node))
                     obj))                     
    (ct-make-shape :P "Spiral"
                   (let ((obj (make-spiral .2 2.0 -1.0 4 64)))
                     (show-ui-content (make-scene-item-editor obj #'compute-procedural-node))
                     obj))                     
    (ct-make-shape :T "Star"
                   (let ((obj (make-star 5 3.0 1.0 1)))
                     (show-ui-content (make-scene-item-editor obj #'compute-procedural-node))
                     obj))                     
    table))

(register-dynamic-command-table-entry
 "Create" :O "Create Procedural Curve Menu"
 (lambda () (make-active-command-table (procedural-curve-command-table)))
 (lambda () t))
