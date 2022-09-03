(in-package #:kons-9)

(defmacro defclass-kons-9 (name superclasses slot-names-and-initforms &rest class-options)
  `(defclass ,name ,superclasses
     ,(mapcar #'(lambda (slot-info)
                  (let ((slot-name (first slot-info))
                        (slot-value (second slot-info)))
                    (list slot-name
                          :accessor slot-name
                          :initarg (intern (symbol-name slot-name) "KEYWORD")
                          :initform slot-value)))
       slot-names-and-initforms)
     ,@class-options))

(defmacro def-procedural-polygon (name slot-names-and-initforms class-options
                                  inputs &rest compute-expr)
  `(progn
     (defclass-kons-9 ,name (procedural-polygon) ,slot-names-and-initforms ,@class-options)
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

;; (pprint (macroexpand-1
;; '(def-procedural-polygon
;;     sine-curve
;;     ((period 360.0)
;;      (frequency 1.0)
;;      (x-scale 1.0)
;;      (y-scale 1.0))
;;   ((:default-initargs
;;     :is-closed-polygon? nil))
;;   (period frequency x-scale y-scale)
;;   (make-sine-curve-points (period poly) (frequency poly)
;;                           (x-scale poly) (y-scale poly) (num-segments poly)))))

;;;; procedural-polygon ========================================================

(defclass-kons-9 procedural-polygon (polygon procedural-mixin)
  ((num-segments 64)))

(def-procedural-input procedural-polygon num-segments)
(def-procedural-output procedural-polygon points)

;;;; procedural polygons =======================================================

(def-procedural-polygon
    line
    ((p1 (p! 0 0 0))
     (p2 (p! 0 1 0)))
  ()
  (p1 p2)
  (make-line-points (p1 poly) (p2 poly) (num-segments poly)))

(def-procedural-polygon
    rectangle
    ((width 2.0)
     (height 1.0))
  ()
  (width height)
  (make-rectangle-points (width poly) (height poly) (num-segments poly)))

(def-procedural-polygon
    square
    ((side 2.0))
  ()
  (side)
  (make-rectangle-points (side poly) (side poly) (num-segments poly)))

(def-procedural-polygon
    circle
    ((diameter 2.0))
  ()
  (diameter)
  (make-circle-points (diameter poly) (num-segments poly)))

(def-procedural-polygon
    arc
    ((diameter 2.0)
     (start-angle 0.0)
     (end-angle 90.0))
  ((:default-initargs
    :is-closed-polygon? nil))
  (diameter start-angle end-angle)
  (make-arc-points (diameter poly) (start-angle poly) (end-angle poly) (num-segments poly)))

(def-procedural-polygon
    spiral
    ((start-diameter 0.0)
     (end-diameter 2.0)
     (axis-length 2.0)
     (num-loops 2.0))
  ((:default-initargs
    :is-closed-polygon? nil))
  (start-diameter end-diameter axis-length num-loops)
  (make-spiral-points (start-diameter poly) (end-diameter poly) (axis-length poly) (num-loops poly) (num-segments poly)))

(def-procedural-polygon
    sine-curve
    ((period 360.0)
     (frequency 1.0)
     (x-scale 1.0)
     (y-scale 1.0))
  ((:default-initargs
    :is-closed-polygon? nil))
  (period frequency x-scale y-scale)
  (make-sine-curve-points (period poly) (frequency poly)
                          (x-scale poly) (y-scale poly) (num-segments poly)))

