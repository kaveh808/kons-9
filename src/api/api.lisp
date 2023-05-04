;;; api.lisp -- Kons-9 API definition and documentation

(pax:define-package :kons-9
  (:use #:common-lisp))
(in-package :kons-9)

;;; Note: Emacs `align-regexp' helps to maintain the pendantic-looking formatting.

;;; Scaffolding

(defmacro doc (object doc-type doc-string)
  `(setf (documentation ,object ',doc-type) ,(string-left-trim '(#\newline) doc-string)))

;;; API Reference

(pax:defsection @api (:title "Kons-9 API Reference")
  (kons-9 asdf:system)
  (@introduction pax:section)
  "Now let's get cracking!"
  (@point pax:section)
  (@color pax:section)
  (@matrix pax:section)
  (@shape pax:section)
  (@group pax:section)
  (@scene pax:section)
  (@command-table pax:section)
  (@top-level pax:section))

;;; Introduction

(pax:defsection @introduction (:title "Introduction") "
Kons-9 is a 3D IDE written in Common Lisp.

This manual aspires to be a full description of the Lisp programming interface
to Kons-9. This manual presumes that you are familiar with Common Lisp
programming. (If you are not already a Common Lisp hacker then consider reading
[Practical Common Lisp](https://gigamonkeys.com/book/).)

Beware! This manual is new and incomplete. It will require the contributions of
many authors to achieve its goal of completeness. If the documentation you are
looking for is not yet written then please consider writing the first draft.

The documentation is prepared using [MGL-PAX](https://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
by Gábor Melis.")

;;; Point

(deftype point () '(simple-array single-float (3)))
(declaim (type point +origin+ +x-axis+ +y-axis+ +z-axis+))

(pax:defsection @point (:title "Points")
  "Points are three-dimensional coordinates represented as specialized single-float vectors.

  Point operations are all non-destructive unless stated otherwise."
  (p!                 pax:function)
  (+origin+           pax:variable)
  (@point-arithmetic  pax:section)
  (@point-metrics     pax:section)
  (@point-computing   pax:section)
  (@point-vector      pax:section)
  (@point-geometry    pax:section)
  (@point-lisp        pax:section))

(pax:defsection @point-arithmetic (:title "Point arithmetic")
  "Point-arithmetic supports either a point or a scalar on the right-hand-side (`VAL`)."
  (p+                               pax:function)
  (p-                               pax:function)
  (p*                               pax:function)
  (p/                               pax:function))

(pax:defsection @point-metrics (:title "Point metrics")
  (p-dist                           pax:function)
  (p-dist-squared                   pax:function))

(doc 'p-dist function "Return the distance between points P1 and P2.")
(doc 'p-dist-squared function "Return the square of the distance between points P1 and P2.")

(pax:defsection @point-computing (:title "Computing new points")
  (p-center                         pax:function)
  (p-average                        pax:function)
  (p-midpoint                       pax:function)
  (p-smooth-lerp                    pax:function)
  (p-jitter                         pax:function))

(doc 'p-center function "Return the center (average) of POINTS.")
(doc 'p-average function "See P-CENTER.")

(doc 'p-smooth-lerp function "
Return a point interpolated smoothly (cubic) between P1 and P2 by factor F.

```cl-transcript
(loop with p1 = (p! 0 0 0)
      with p2 = (p! 0 0 1)
      for f from 0.0 to 1.0 by 0.1
      do (print (p-smooth-lerp f p1 p2)))
..
.. #(0.0 0.0 0.0)
.. #(0.0 0.0 0.028)
.. #(0.0 0.0 0.104)
.. #(0.0 0.0 0.216)
.. #(0.0 0.0 0.352)
.. #(0.0 0.0 0.5)
.. #(0.0 0.0 0.648)
.. #(0.0 0.0 0.7840001)
.. #(0.0 0.0 0.896)
.. #(0.0 0.0 0.9720001)
=> NIL

```
")

(doc 'p-jitter function "
Return a new point with a uniform random number in [-AMOUNT,AMOUNT] added to
each of the coordinates of P.

```cl-transcript
(loop repeat 5
      with p = (p! 0 0 0)
      do (print (p-jitter p 0.1)))
..
.. #(-0.015892074 -0.09005993 -0.007281564)
.. #(-0.013628766 -0.035423063 -0.08748648)
.. #(-0.0026499256 0.05349622 -0.022806786)
.. #(-0.04921451 0.092438884 0.039513953)
.. #(0.055951603 0.097014 -0.032807752)
=> NIL
```
")

(pax:defsection @point-vector (:title "Vectors as points")
  "Vectors are conveniently represented as points too."
  (+x-axis+                         pax:variable)
  (+y-axis+                         pax:variable)
  (+z-axis+                         pax:variable)
  (p-from-to                        pax:function)
  (p-rand                           pax:function)
  (p-rand1                          pax:function)
  (p-rand2                          pax:function)
  (@point-vector-trig               pax:section))

(doc 'p-rand function "
Return a random vector with magnitude MAG.

```cl-transcript
(dotimes (i 5)
 (print (p-rand 10.0)))
..
.. #(8.721577 -4.890044 0.14691272)
.. #(-6.748556 -5.864832 -4.4789214)
.. #(-6.0299535 0.17271549 7.9755774)
.. #(1.810133 8.698843 -4.588415)
.. #(-6.610375 6.457787 3.8209844)
=> NIL

```
")

(doc 'p-rand1 function "
```cl-transcript
(values (p-rand1 (p! 10 0 0))
        (p-rand1 (p! 10 0 0) (p! 0 0 100)))
=> #(0.8208256 0.0 0.0)
=> #(-1.5418701 0.0 100.0)

```
")

(doc 'p-rand2 function "
```cl-transcript
(dotimes (i 5)
  (print (p-rand2 +origin+ (p! 1 10 100))))
..
.. #(0.5472082 9.946655 86.98675)
.. #(0.3072058 6.212449 65.826645)
.. #(0.19817531 2.2756863 37.87581)
.. #(0.8526037 5.9077573 49.116753)
.. #(0.113173604 5.919751 54.794872)
=> NIL

```
")

(pax:defsection @point-vector-trig (:title "Trigonometry")
  "Trigonometric functions are defined on vectors (represented as points.)"
  (p-angle-cosine                   pax:function)
  (p-angle-sine                     pax:function)
  (p-angle                          pax:function)
  (p-z-alignment-angles             pax:function))

(pax:defsection @point-lisp (:title "Lisp object operations on points")
  (point->list                      pax:function)
  (p-set!                           pax:function)
  (p-vec                            pax:function)
  (copy-points                      pax:function)
  (copy-point-array                 pax:function))

(pax:defsection @point-geometry (:title "Geometry operations on points")
  (triangle-normal                  pax:function)
  (quad-normal                      pax:function)
  (triangle-area                    pax:function)
  (barycentric-point                pax:function)
  (random-barycentric-point         pax:function)
  (points-bounds                    pax:function)
  (p-sphericize                     pax:function)
  (point-line-segment-dist          pax:function)
  (point-curve-dist                 pax:function)
  (point-on-plane                   pax:function)
  (point-on-triangle-plane          pax:function)
  (point-barycentric-coordinates    pax:function)
  (point-inside-triangle            pax:function)
  (point-line-segment-closest-point pax:function)
  (point-triangle-closest-point     pax:function)
  )

(doc 'p! function "
Construct a new point object.")

(doc 'p+ function "
```cl-transcript
(p+ (p! 1 2 3)
    (p! 10 20 30))
=> #(11.0 22.0 33.0)
```")

(doc 'p- function "
```cl-transcript
(p- (p! 1 2 3) 1)
=> #(0.0 1.0 2.0)
```")

(doc 'p* function "
```cl-transcript
(p* (p! 1 2 3)
    (p! 10 20 30))
=> #(10.0 40.0 90.0)
```")

(doc 'p/ function "
```cl-transcript
(p/ (p! 1 2 3) 2)
=> #(0.5 1.0 1.5)
```")


;;; Colors

(pax:defsection @color (:title "Color")
  "Colors are four-channel RGBA values represented as vectors of single-float levels between 0.0 and 1.0.

  Operations on colors are non-destructive unless stated otherwise."
  (c!                pax:function)
  (c-red             pax:function)
  (c-green           pax:function)
  (c-blue            pax:function)
  (c-alpha           pax:function)
  (c-set-rgb         pax:function)
  (c-set-alpha       pax:function)
  (c-lerp            pax:function)
  (c-rand            pax:function)
  (c-rand-with-alpha pax:function)
  (c-rand2           pax:function)
  (c+                pax:function)
  (c-scale           pax:function)
  (c-jitter          pax:function)
  (c-rainbow         pax:function)
  )

(doc 'c-red function "
Return the red component of color C.

```cl-transcript
(c-red (c! 0.1 0.2 0.3))
=> 0.1

```
")

(doc 'c-green function "
Return the green component of color C.

```cl-transcript
(c-green (c! 0.1 0.2 0.3))
=> 0.2

```
")

(doc 'c-blue function "
Return the blue component of color C.

```cl-transcript
(c-blue (c! 0.1 0.2 0.3))
=> 0.3

```
")

(doc 'c-alpha function "
Return the alpha component of color C.

```cl-transcript
(c-alpha (c! 0.1 0.2 0.3 0.4))
=> 0.4

```
")

(doc 'c-set-rgb function "
Destructively copy the RGB (but not alpha) values from C1 to C2.")

(doc 'c-set-alpha function "
Destructively set the alpha value of C to ALPHA.")

(doc 'c-lerp function "
Return a color linearly interpolated by factor F between C1 and C2.

```cl-transcript
(c-lerp 0.5 (c! 0 0 0 0) (c! 0.2 0.4 0.6 0.8))
=> #(0.1 0.2 0.3 0.4)

```
")

(doc 'c-rand function "
Return an opaque color with uniform RGB channel values.

```cl-transcript
(dotimes (i 3)
  (print (c-rand)))
..
.. #(0.8987992 0.7864139 0.79885733 1.0)
.. #(0.33613765 0.39076257 0.20800936 1.0)
.. #(0.45029986 0.5191544 0.37873483 1.0)
=> NIL

```
")

(doc 'c-rand-with-alpha function "
Return a color with uniform random RGB and alpha values.

```cl-transcript
(dotimes (i 3)
  (print (c-rand-with-alpha)))
..
.. #(0.564777 0.8026259 0.25154138 0.43752754)
.. #(0.5176456 0.29399824 0.5838525 0.17650497)
.. #(0.1909852 0.15405178 0.6489651 0.350994)
=> NIL

```
")

;;; Matrix

(pax:defsection @matrix (:title "Matrix")
  "A matrix is a 4x4 array of numbers that represents an affine transformation in 3D space."
  (make-matrix               pax:function)
  (make-matrix-with          pax:function)
  (make-id-matrix            pax:function)
  (matrix->list              pax:function)
  (matrix->vector            pax:function)
  (matrix-copy               pax:function)
  (make-translation-matrix   pax:function)
  (make-rotation-matrix      pax:function)
  (make-x-rotation-matrix    pax:function)
  (make-y-rotation-matrix    pax:function)
  (make-z-rotation-matrix    pax:function)
  (make-axis-rotation-matrix pax:function)
  (make-scale-matrix         pax:function)
  (make-shear-matrix         pax:function)
  (make-z-alignment-matrix   pax:function)
  (make-look-at-from-matrix  pax:function)
  (make-look-dir-from-matrix pax:function)
  (matrix-multiply           pax:function)
  (matrix-multiply-n         pax:function)
  (transform-point           pax:function)
  (transform-points          pax:function)
  (transform-point!          pax:function)
  (transform-point-array!    pax:function))

;;; Shapes

(pax:defsection @shape (:title "Shape")
  (shape        pax:class)
  (transform    (pax:accessor shape))
  (is-visible?  (pax:accessor shape))
  (show-axis    (pax:accessor shape))
  (show-bounds? (pax:accessor shape))
  "Relative."
  (translate-by (pax:method () (shape t)))
  (rotate-by (pax:method () (shape t)))
  (scale-by (pax:method () (shape t)))
  "Absolute."
  (translate-to (pax:method () (shape t)))
  (rotate-to (pax:method () (shape t)))
  (scale-to (pax:method () (shape t)))
  (scale-by (pax:method () (shape number)))
  "Special."
  (reset-transform (pax:method () (shape)))
  (center-at-origin (pax:method () (shape)))
  (scale-to-size (pax:method () (shape t)))
  (get-bounds (pax:method () (shape)))
  ;;
  (@point-cloud pax:section)
  )

;;; Point cloud

(pax:defsection @point-cloud (:title "Point cloud")
  "A point-cloud is a shape made up of colored points."
  (point-cloud             pax:class)
  (points                  (pax:accessor point-cloud))
  (point-colors            (pax:accessor point-cloud))
  (make-point-cloud        pax:function)
  (make-line-points        pax:function)
  (make-rectangle-points   pax:function)
  (make-circle-points      pax:function)
  (make-arc-points         pax:function)
  (make-spiral-points      pax:function)
  (make-sine-curve-points  pax:function)
  (make-random-points      pax:function)
  (make-grid-points        pax:function)
  (freeze-transform        (pax:method () (point-cloud))) ;; XXX belongs to shape? lacks DEFGENERIC
  (allocate-point-colors   (pax:method () (point-cloud)))
  (reset-point-colors      (pax:method () (point-cloud)))
  (set-point-colors-by-xyz (pax:method () (point-cloud t)))
  (randomize-points        (pax:method () (point-cloud t)))
  (@polyhedron pax:section)
  )

;;; Polyhedron

(pax:defsection @polyhedron (:title "Polyhedron")
  "A polyhedron is a 3D shape formed by flat polygonal faces. The faces of a
  polyhedron are represented in terms of the vertices of an underlying
  point-cloud."
  (polyhedron                     pax:class)
  (faces                          (pax:accessor polyhedron))
  (face-normals                   (pax:accessor polyhedron))
  (point-normals                  (pax:accessor polyhedron))
  (show-normals                   (pax:accessor polyhedron))
  (point-source-use-face-centers? (pax:accessor polyhedron))
  "Polyhedrons can be constructed to represent (or approximate) many common geometric shapes."
  (make-polyhedron           pax:function)
  (make-polyhedron           pax:function)
  (make-rectangle-polyhedron pax:function)
  (make-square-polyhedron    pax:function)
  (make-circle-polyhedron    pax:function)
  (make-tetrahedron          pax:function)
  (make-box                  pax:function)
  (make-cube                 pax:function)
  (make-cut-cube             pax:function)
  (make-octahedron           pax:function)
  (make-dodecahedron         pax:function)
  (make-icosahedron          pax:function)
  (make-cube-sphere          pax:function)
  ;; Methods
  (empty-polyhedron (pax:method () (polyhedron)))
  (set-face-point-lists (pax:method () (polyhedron t)))
  (set-triangle-arrays (pax:method () (polyhedron t)))
  (face-center (pax:method () (polyhedron t)))
  (face-centers (pax:method () (polyhedron)))
  (face-normal (pax:method () (polyhedron t)))
  (compute-face-normals (pax:method () (polyhedron)))
  (compute-point-normals (pax:method () (polyhedron)))
  (compute-point-normals-SAV (pax:method () (polyhedron)))
  (face-points-list (pax:method () (polyhedron integer)))
  (face-points-list (pax:method () (polyhedron list)))
  (face-points-array (pax:method () (polyhedron integer)))
  (face-points-array (pax:method () (polyhedron list)))
  (reverse-face-normals (pax:method () (polyhedron)))
  (set-point-colors-by-point-and-normal (pax:method () (polyhedron t)))
  (set-point-colors-uniform (pax:method () (polyhedron t)))

  )



;;; Groups

(pax:defsection @group (:title "Groups (mixin)")
  (group-mixin         pax:class)
  (children            (pax:accessor group-mixin))
  (printable-data      (pax:method () (group-mixin)))
  (num-children        (pax:method () (group-mixin)))
  (get-child           (pax:method () (group-mixin t)))
  (children-as-list    (pax:method () (group-mixin)))
  (add-child           (pax:method () (group-mixin scene-item)))
  (remove-child        (pax:method () (group-mixin scene-item)))
  (set-children        (pax:method () (group-mixin t)))
  (remove-all-children (pax:method () (group-mixin)))
  (@shape-group        pax:section))

;;; Shape groups

(pax:defsection @shape-group (:title "Shape Groups")
  (shape-group pax:class)
  (make-shape-group pax:function)
  (get-bounds (pax:method () (shape-group)))
  (set-point-colors-by-xyz (pax:method () (shape-group t)))
  (scatter-shapes-in-group pax:function)
  (scatter-shapes pax:function)
  (scatter-shape-instances pax:function)
  )

;;; Scenes

(declaim (type (or null scene) *scene*))

(pax:defsection @scene (:title "Scene and item")
  "Item"
  (item pax:class)
  "Scene item"
  (scene-item pax:class)
  "Scene"
  (scene pax:class)
  (shape-root (pax:accessor scene))
  (motion-root (pax:accessor scene))
  (interactor (pax:accessor scene))
  (initialized? (pax:accessor scene))
  (selection (pax:accessor scene))
  (start-frame (pax:accessor scene))
  (end-frame (pax:accessor scene))
  (current-frame (pax:accessor scene))
  (fps (pax:accessor scene))
  )

;;; Command tables

(pax:defsection @command-table (:title "Command table")
  (command-table pax:class)
  (title (pax:accessor command-table))
  (entries (pax:accessor command-table))
  "One command table is *active* at any given time."
  (active-command-table pax:function)
  (make-active-command-table pax:function)
  "Commands and nested command tables are defined using macros."
#|
  "Command table entries define individual commands."
  (command-table-entry pax:class)
  (key-binding (pax:accessor command-table-entry))
  (command-fn (pax:accessor command-table-entry))
  (help-string (pax:accessor command-table-entry))
  (add-entry (pax:method () (command-table t t t)))
  (do-command (pax:method () (command-table t)))
|#
  (ct-entry pax:macro)
  (ct-subtable pax:macro)
  "Here is an example of constructing a command table:

```cl-transcript
   (let ((table (make-instance 'command-table :title \"Example\")))
     ;; note: these macros depend on the variable name 'table'
     (ct-entry :C \"Make a Cube\" (make-cube 2.0))
     (ct-subtable :S \"My Submenu\" (make-command-table 'my-submenu))
     (entries table))
```
"
  )

(doc (find-class 'command-table) t "
A command table defines a menu of keyboard-driven user interface commands. Each
command either performs an action or recursively opens a new command table as a
submenu.

MAKE-INSTANCE is used to create command table objects.")

(doc 'active-command-table function "
Return the currently active command table.")

(doc 'make-active-command-table function "
Make TABLE the active command table.")

(doc 'ct-entry function "
Define a user interface command. The new command is added to the command table
named `table` (a variable captured by this macro.)

KEY-BINDING is a keyword designating the keyboard input that invokes the command.

HELP is a short string description e.g. \"Open Scene File\".

EXPR is one or more Lisp forms to be evaluated when the command is invoked.")

(doc 'ct-subtable function "
Define a nested table of user interface commands.

KEY-BINDING is as in the CT-ENTRY macro.

TITLE is the string name of the menu e.g. \"Edit\".

C-TABLE-FN is an expression to be evaluated to return the command table.")

;;; Top-level

(pax:defsection @top-level (:title "Top-level")
  "Kons-9 runs on a dedicated thread."
  (kons-9::run function)
  (*scene* variable))

(doc #'run function "
Open the Kons-9 user interface window and operate it using a dedicated thread.

COMMAND-TABLE can optionally be supplied as a custom top-level command table.
See @COMMAND-TABLES.")

(doc '*scene* variable "
Global scene object.")
