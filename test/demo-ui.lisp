(in-package #:kons-9)

#|
Greetings and welcome to kons-9!

These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

The demos below will walk you through some of the features of the software. You
will evaluate the expressions in each demo and the graphics in the window will
update accordingly.

We also assume that you are familiar with the basics of 3D graphics.

As of this writing, kons-9 runs on MacOS (Intel & M1) and Linux.

We hope you find the system enjoyable and useful.
|#

#|
(UI Demo 1) 3D navigation and display ==========================================

This is a proof of concept for command tables, which allow for hierarchical key bindings.

Evaluate all the code below. 

Press 'h' and notice a new 'c' command is available.

Press 'c'. The title bar will change to indicate the current command table.

Press 'h' to see the available commands.

Press 'p'. The title bar will update.

Press 'h' to see the available commands.

Make some curves.

Press 'tab' to go back to the default command table. The title bar will update.

Press 'n' to clear the window.

Press '1' to turn off filled display (so you can better see the polyhedra you will make).

Press 'c'. The title bar will change to indicate the current command table.

Press 'h' to see the available commands.

Press 'y'. The title bar will update.

Press 'h' to see the available commands.

Make some polyhedra.

Press 'tab' to go back to the default command table. The title bar will update.
|#

(let ((table (car (command-tables *default-scene-view*))))
  (ct-subtable :c "Create" (create-command-table)))

(defun create-command-table ()
  (let ((table (make-instance `command-table :title "Create")))
    (ct-subtable :p "Curve" (curve-command-table))
    (ct-subtable :y "Polyhedron" (polyhedron-command-table))
    table))

(defun curve-command-table ()
  (let ((table (make-instance `command-table :title "Curve")))
    (ct-make-shape :l "line curve" (make-line-curve (p! 0 0 0) (p! 2 2 2) 8))
    (ct-make-shape :r "rectangle curve" (make-rectangle-curve 2 1 4))
    (ct-make-shape :s "square curve" (make-square-curve 1.5))
    (ct-make-shape :c "circle curve" (make-circle-curve 2.0 16))
    (ct-make-shape :a "arc curve" (make-arc-curve 2.0 16 0 pi))
    (ct-make-shape :n "sine curve curve" (make-sine-curve-curve 360 1 2 1 16))
    (ct-make-shape :p "spiral curve" (make-spiral-curve .2 2.0 -1.0 4 64))
    table))

(defun polyhedron-command-table ()
  (let ((table (make-instance `command-table :title "Polyhedron")))
    (ct-make-shape :t "tetrahedron" (make-tetrahedron 2.0))
    (ct-make-shape :c "cube" (make-cube 2.0))
    (ct-make-shape :o "octahedron" (make-octahedron 2.0))
    (ct-make-shape :d "dodecahedron" (make-dodecahedron 2.0))
    (ct-make-shape :i "icosahedron" (make-icosahedron 2.0))
    (ct-make-shape :s "cube" (make-cube-sphere 4.0 3))
    table))

#|
END ============================================================================
|#
