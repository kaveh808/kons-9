(in-package #:kons-9)

#|
The PROCEDURAL-POLYGON class uses the kons-9 PROCEDURAL-MIXIN facility to
automatically update the shape when one of the input slots changes.

The demos below demonstrate this.

Note that if you change the points of an instance, currently they will be reset
if an input slot changes.
|#

#|
(Demo 01 procedural-curve) line ================================================
|#
(progn
  (defparameter *line* (make-line (p! 0 0 0) (p! 0 1 1) 2))
  (with-clear-scene
    (add-shape *scene* *line*)))
#|
Modify slots and shape will change.
|#
(progn
  (setf (num-segments *line*) 4)
  (setf (p2 *line*) (p! 2 1 0)))

#|
(Demo 02 procedural-curve) rectangle ===========================================
|#
(progn
  (defparameter *rectangle* (make-rectangle 4.0 2.0 2))
  (with-clear-scene
    (add-shape *scene* *rectangle*)))
#|
Modify slots and shape will change.
|#
(progn
  (setf (num-segments *rectangle*) 4)
  (setf (width *rectangle*) 8.0))

#|
(Demo 03 procedural-curve) square ==============================================
|#
(progn
  (defparameter *square* (make-square 4.0 8))
  (with-clear-scene
    (add-shape *scene* *square*)))
#|
Modify slots and shape will change.
|#
(progn
  (setf (num-segments *square*) 3)
  (setf (side *square*) 2.0))

#|
(Demo 04 procedural-curve) circle ==============================================
|#
(progn
  (defparameter *circle* (make-circle 4.0 8))
  (with-clear-scene
    (add-shape *scene* *circle*)))
#|
Modify slots and shape will change.
|#
(progn
  (setf (num-segments *circle*) 32)
  (setf (diameter *circle*) 2.0))

#|
(Demo 05 procedural-curve) arc =================================================
|#
(progn
  (defparameter *arc* (make-arc 4.0 0.0 90.0 8))
  (with-clear-scene
    (add-shape *scene* *arc*)))
#|
Modify slots and shape will change.
|#
(progn
  (setf (start-angle *arc*) 45.0)
  (setf (end-angle *arc*) 270.0))

#|
(Demo 06 procedural-curve) spiral ==============================================
|#
(progn
  (defparameter *spiral* (make-spiral  0.0 2.0 2.0 2.0 32))
  (with-clear-scene
    (add-shape *scene* *spiral*)))
#|
Modify slots and shape will change.
|#
(progn
  (setf (num-loops *spiral*) 3.0)
  (setf (end-diameter *spiral*) 4.0))

#|
(Demo 07 procedural-curve) sine-curve ==========================================
|#
(progn
  (defparameter *sine-curve* (make-sine-curve 720 2 4 2 64))
  (with-clear-scene
    (add-shape *scene* *sine-curve*)))
#|
Modify slots and shape will change.
|#
(progn
  (setf (num-segments *sine-curve*) 16)
  (setf (frequency *sine-curve*) 1.0))

#|
END ============================================================================
|#
