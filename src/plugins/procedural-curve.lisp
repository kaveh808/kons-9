(in-package #:kons-9)

(defmacro def-procedural-curve (name slot-names-and-initforms class-options
                                  inputs &rest compute-expr)
  `(progn
     (defclass-kons-9 ,name (procedural-curve) ,slot-names-and-initforms ,@class-options)
     ,@(mapcar #'(lambda (input)
                   `(def-procedural-input ,name ,input))
               inputs)
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

(def-procedural-input procedural-curve num-segments)
(def-procedural-output procedural-curve points)

;;;; procedural curves =======================================================

(def-procedural-curve
    line
    ((p1 (p! 0 0 0))
     (p2 (p! 0 1 0)))
  ()
  (p1 p2)
  (make-line-points (p1 poly) (p2 poly) (num-segments poly)))

(def-procedural-curve
    rectangle
    ((width 2.0)
     (height 1.0))
  ()
  (width height)
  (make-rectangle-points (width poly) (height poly) (num-segments poly)))

(def-procedural-curve
    square
    ((side 2.0))
  ()
  (side)
  (make-rectangle-points (side poly) (side poly) (num-segments poly)))

(def-procedural-curve
    circle
    ((diameter 2.0))
  ()
  (diameter)
  (make-circle-points (diameter poly) (num-segments poly)))

(def-procedural-curve
    arc
    ((diameter 2.0)
     (start-angle 0.0)
     (end-angle 90.0))
  ((:default-initargs
    :is-closed-curve? nil))
  (diameter start-angle end-angle)
  (make-arc-points (diameter poly) (start-angle poly) (end-angle poly) (num-segments poly)))

(def-procedural-curve
    spiral
    ((start-diameter 0.0)
     (end-diameter 2.0)
     (axis-length 2.0)
     (num-loops 2.0))
  ((:default-initargs
    :is-closed-curve? nil))
  (start-diameter end-diameter axis-length num-loops)
  (make-spiral-points (start-diameter poly) (end-diameter poly) (axis-length poly) (num-loops poly) (num-segments poly)))

(def-procedural-curve
    sine-curve
    ((period 360.0)
     (frequency 1.0)
     (x-scale 1.0)
     (y-scale 1.0))
  ((:default-initargs
    :is-closed-curve? nil))
  (period frequency x-scale y-scale)
  (make-sine-curve-points (period poly) (frequency poly)
                          (x-scale poly) (y-scale poly) (num-segments poly)))

