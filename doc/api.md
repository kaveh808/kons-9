<a id="x-28KONS-9-3A-40API-20MGL-PAX-3ASECTION-29"></a>
# Kons-9 API Reference

## Table of Contents

- [1 `KONS-9` ASDF System][4a53]
- [2 Introduction][1cd7]
- [3 Points][ab46]
    - [3.1 Point arithmetic][4cb9]
    - [3.2 Point metrics][3a9c]
    - [3.3 Computing new points][c188]
    - [3.4 Vectors as points][682d]
        - [3.4.1 Trigonometry][7616]
    - [3.5 Geometry operations on points][8dfa]
    - [3.6 Lisp object operations on points][ccbf]
- [4 Color][2eeb]
- [5 Matrix][7f7a]
- [6 Shape][20ea]
    - [6.1 Point cloud][dc52]
        - [6.1.1 Polyhedron][409c]
- [7 Groups (mixin)][c03e]
    - [7.1 Shape Groups][388c]
- [8 Scene and item][0eea]
- [9 Command table][d298]
- [10 Top-level][4578]

###### \[in package KONS-9\]
<a id="x-28-22kons-9-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
## 1 `KONS-9` ASDF System

- Description: Common Lisp 3D Graphics System
- Licence: MIT
- Author: Kaveh Kardan


<a id="x-28KONS-9-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
## 2 Introduction

Kons-9 is a 3D IDE written in Common Lisp.

This manual aspires to be a full description of the Lisp programming interface
to Kons-9. This manual presumes that you are familiar with Common Lisp
programming. (If you are not already a Common Lisp hacker then consider reading
[Practical Common Lisp](https://gigamonkeys.com/book/).)

Beware! This manual is new and incomplete. It will require the contributions of
many authors to achieve its goal of completeness. If the documentation you are
looking for is not yet written then please consider writing the first draft.

The documentation is prepared using [MGL-PAX](https://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
by Gábor Melis.

Now let's get cracking!

<a id="x-28KONS-9-3A-40POINT-20MGL-PAX-3ASECTION-29"></a>
## 3 Points

Points are three-dimensional coordinates represented as specialized single-float vectors.

Point operations are all non-destructive unless stated otherwise.

<a id="x-28KONS-9-3AP-21-20FUNCTION-29"></a>
- [function] **P!** *X Y Z*

    Construct a new point object.

<a id="x-28KONS-9-3A-2BORIGIN-2B-20VARIABLE-29"></a>
- [variable] **+ORIGIN+** *#(0.0 0.0 0.0)*

<a id="x-28KONS-9-3A-40POINT-ARITHMETIC-20MGL-PAX-3ASECTION-29"></a>
### 3.1 Point arithmetic

Point-arithmetic supports either a point or a scalar on the right-hand-side (`VAL`).

<a id="x-28KONS-9-3AP-2B-20FUNCTION-29"></a>
- [function] **P+** *P VAL*

    ```common-lisp
    (p+ (p! 1 2 3)
        (p! 10 20 30))
    => #(11.0 22.0 33.0)
    ```


<a id="x-28KONS-9-3AP--20FUNCTION-29"></a>
- [function] **P-** *P VAL*

    ```common-lisp
    (p- (p! 1 2 3) 1)
    => #(0.0 1.0 2.0)
    ```


<a id="x-28KONS-9-3AP-2A-20FUNCTION-29"></a>
- [function] **P\*** *P VAL*

    ```common-lisp
    (p* (p! 1 2 3)
        (p! 10 20 30))
    => #(10.0 40.0 90.0)
    ```


<a id="x-28KONS-9-3AP-2F-20FUNCTION-29"></a>
- [function] **P/** *P VAL*

    ```common-lisp
    (p/ (p! 1 2 3) 2)
    => #(0.5 1.0 1.5)
    ```


<a id="x-28KONS-9-3A-40POINT-METRICS-20MGL-PAX-3ASECTION-29"></a>
### 3.2 Point metrics

<a id="x-28KONS-9-3AP-DIST-20FUNCTION-29"></a>
- [function] **P-DIST** *P1 P2*

    Return the distance between points `P1` and `P2`.

<a id="x-28KONS-9-3AP-DIST-SQUARED-20FUNCTION-29"></a>
- [function] **P-DIST-SQUARED** *P1 P2*

    Return the square of the distance between points `P1` and `P2`.

<a id="x-28KONS-9-3A-40POINT-COMPUTING-20MGL-PAX-3ASECTION-29"></a>
### 3.3 Computing new points

<a id="x-28KONS-9-3AP-CENTER-20FUNCTION-29"></a>
- [function] **P-CENTER** *POINTS*

    Return the center (average) of `POINTS`.

<a id="x-28KONS-9-3AP-AVERAGE-20FUNCTION-29"></a>
- [function] **P-AVERAGE** *&REST POINTS*

    See [`P-CENTER`][1531].

<a id="x-28KONS-9-3AP-MIDPOINT-20FUNCTION-29"></a>
- [function] **P-MIDPOINT** *P1 P2*

<a id="x-28KONS-9-3AP-SMOOTH-LERP-20FUNCTION-29"></a>
- [function] **P-SMOOTH-LERP** *F P1 P2*

    Return a point interpolated smoothly (cubic) between `P1` and `P2` by factor `F`.
    
    ```common-lisp
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


<a id="x-28KONS-9-3AP-JITTER-20FUNCTION-29"></a>
- [function] **P-JITTER** *P AMOUNT*

    Return a new point with a uniform random number in -AMOUNT,AMOUNT added to
    each of the coordinates of `P`.
    
    ```common-lisp
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


<a id="x-28KONS-9-3A-40POINT-VECTOR-20MGL-PAX-3ASECTION-29"></a>
### 3.4 Vectors as points

Vectors are conveniently represented as points too.

<a id="x-28KONS-9-3A-2BX-AXIS-2B-20VARIABLE-29"></a>
- [variable] **+X-AXIS+** *#(1.0 0.0 0.0)*

<a id="x-28KONS-9-3A-2BY-AXIS-2B-20VARIABLE-29"></a>
- [variable] **+Y-AXIS+** *#(0.0 1.0 0.0)*

<a id="x-28KONS-9-3A-2BZ-AXIS-2B-20VARIABLE-29"></a>
- [variable] **+Z-AXIS+** *#(0.0 0.0 1.0)*

<a id="x-28KONS-9-3AP-FROM-TO-20FUNCTION-29"></a>
- [function] **P-FROM-TO** *P1 P2*

<a id="x-28KONS-9-3AP-RAND-20FUNCTION-29"></a>
- [function] **P-RAND** *&OPTIONAL (MAG 1.0)*

    Return a random vector with magnitude `MAG`.
    
    ```common-lisp
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


<a id="x-28KONS-9-3AP-RAND1-20FUNCTION-29"></a>
- [function] **P-RAND1** *P &OPTIONAL (PIVOT (P! 0 0 0))*

    ```common-lisp
    (values (p-rand1 (p! 10 0 0))
            (p-rand1 (p! 10 0 0) (p! 0 0 100)))
    => #(0.8208256 0.0 0.0)
    => #(-1.5418701 0.0 100.0)
    
    ```


<a id="x-28KONS-9-3AP-RAND2-20FUNCTION-29"></a>
- [function] **P-RAND2** *VAL1 VAL2*

    ```common-lisp
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


<a id="x-28KONS-9-3A-40POINT-VECTOR-TRIG-20MGL-PAX-3ASECTION-29"></a>
#### 3.4.1 Trigonometry

Trigonometric functions are defined on vectors (represented as points.)

<a id="x-28KONS-9-3AP-ANGLE-COSINE-20FUNCTION-29"></a>
- [function] **P-ANGLE-COSINE** *P1 P2*

<a id="x-28KONS-9-3AP-ANGLE-SINE-20FUNCTION-29"></a>
- [function] **P-ANGLE-SINE** *P1 P2*

<a id="x-28KONS-9-3AP-ANGLE-20FUNCTION-29"></a>
- [function] **P-ANGLE** *P1 P2*

<a id="x-28KONS-9-3AP-Z-ALIGNMENT-ANGLES-20FUNCTION-29"></a>
- [function] **P-Z-ALIGNMENT-ANGLES** *POINT*

<a id="x-28KONS-9-3A-40POINT-GEOMETRY-20MGL-PAX-3ASECTION-29"></a>
### 3.5 Geometry operations on points

<a id="x-28KONS-9-3ATRIANGLE-NORMAL-20FUNCTION-29"></a>
- [function] **TRIANGLE-NORMAL** *P0 P1 P2*

<a id="x-28KONS-9-3AQUAD-NORMAL-20FUNCTION-29"></a>
- [function] **QUAD-NORMAL** *P0 P1 P2 P3*

<a id="x-28KONS-9-3ATRIANGLE-AREA-20FUNCTION-29"></a>
- [function] **TRIANGLE-AREA** *P0 P1 P2*

<a id="x-28KONS-9-3ABARYCENTRIC-POINT-20FUNCTION-29"></a>
- [function] **BARYCENTRIC-POINT** *P0 P1 P2 A B*

<a id="x-28KONS-9-3ARANDOM-BARYCENTRIC-POINT-20FUNCTION-29"></a>
- [function] **RANDOM-BARYCENTRIC-POINT** *P0 P1 P2*

<a id="x-28KONS-9-3APOINTS-BOUNDS-20FUNCTION-29"></a>
- [function] **POINTS-BOUNDS** *POINT-ARRAY*

<a id="x-28KONS-9-3AP-SPHERICIZE-20FUNCTION-29"></a>
- [function] **P-SPHERICIZE** *P RADIUS &OPTIONAL (FACTOR 1.0) (CENTER +ORIGIN+)*

<a id="x-28KONS-9-3APOINT-LINE-SEGMENT-DIST-20FUNCTION-29"></a>
- [function] **POINT-LINE-SEGMENT-DIST** *P A B*

<a id="x-28KONS-9-3APOINT-CURVE-DIST-20FUNCTION-29"></a>
- [function] **POINT-CURVE-DIST** *POINT POINTS IS-CLOSED-CURVE?*

<a id="x-28KONS-9-3APOINT-ON-PLANE-20FUNCTION-29"></a>
- [function] **POINT-ON-PLANE** *POINT PLANE-POINT PLANE-NORMAL*

<a id="x-28KONS-9-3APOINT-ON-TRIANGLE-PLANE-20FUNCTION-29"></a>
- [function] **POINT-ON-TRIANGLE-PLANE** *P A B C*

<a id="x-28KONS-9-3APOINT-BARYCENTRIC-COORDINATES-20FUNCTION-29"></a>
- [function] **POINT-BARYCENTRIC-COORDINATES** *P A B C*

<a id="x-28KONS-9-3APOINT-INSIDE-TRIANGLE-20FUNCTION-29"></a>
- [function] **POINT-INSIDE-TRIANGLE** *P A B C*

<a id="x-28KONS-9-3APOINT-LINE-SEGMENT-CLOSEST-POINT-20FUNCTION-29"></a>
- [function] **POINT-LINE-SEGMENT-CLOSEST-POINT** *P A B*

<a id="x-28KONS-9-3APOINT-TRIANGLE-CLOSEST-POINT-20FUNCTION-29"></a>
- [function] **POINT-TRIANGLE-CLOSEST-POINT** *P A B C*

<a id="x-28KONS-9-3A-40POINT-LISP-20MGL-PAX-3ASECTION-29"></a>
### 3.6 Lisp object operations on points

<a id="x-28KONS-9-3APOINT--3ELIST-20FUNCTION-29"></a>
- [function] **POINT-\>LIST** *P*

<a id="x-28KONS-9-3AP-SET-21-20FUNCTION-29"></a>
- [function] **P-SET!** *P X Y Z*

<a id="x-28KONS-9-3AP-VEC-20FUNCTION-29"></a>
- [function] **P-VEC** *VEC3*

<a id="x-28KONS-9-3ACOPY-POINTS-20FUNCTION-29"></a>
- [function] **COPY-POINTS** *POINTS*

<a id="x-28KONS-9-3ACOPY-POINT-ARRAY-20FUNCTION-29"></a>
- [function] **COPY-POINT-ARRAY** *POINT-ARRAY*

<a id="x-28KONS-9-3A-40COLOR-20MGL-PAX-3ASECTION-29"></a>
## 4 Color

Colors are four-channel RGBA values represented as vectors of single-float levels between 0.0 and 1.0.

Operations on colors are non-destructive unless stated otherwise.

<a id="x-28KONS-9-3AC-21-20FUNCTION-29"></a>
- [function] **C!** *R G B &OPTIONAL (A 1.0)*

<a id="x-28KONS-9-3AC-RED-20FUNCTION-29"></a>
- [function] **C-RED** *C*

    Return the red component of color `C`.
    
    ```common-lisp
    (c-red (c! 0.1 0.2 0.3))
    => 0.1
    
    ```


<a id="x-28KONS-9-3AC-GREEN-20FUNCTION-29"></a>
- [function] **C-GREEN** *C*

    Return the green component of color `C`.
    
    ```common-lisp
    (c-green (c! 0.1 0.2 0.3))
    => 0.2
    
    ```


<a id="x-28KONS-9-3AC-BLUE-20FUNCTION-29"></a>
- [function] **C-BLUE** *C*

    Return the blue component of color `C`.
    
    ```common-lisp
    (c-blue (c! 0.1 0.2 0.3))
    => 0.3
    
    ```


<a id="x-28KONS-9-3AC-ALPHA-20FUNCTION-29"></a>
- [function] **C-ALPHA** *C*

    Return the alpha component of color `C`.
    
    ```common-lisp
    (c-alpha (c! 0.1 0.2 0.3 0.4))
    => 0.4
    
    ```


<a id="x-28KONS-9-3AC-SET-RGB-20FUNCTION-29"></a>
- [function] **C-SET-RGB** *C1 C2*

    Destructively copy the `RGB` (but not alpha) values from `C1` to `C2`.

<a id="x-28KONS-9-3AC-SET-ALPHA-20FUNCTION-29"></a>
- [function] **C-SET-ALPHA** *C ALPHA*

    Destructively set the alpha value of `C` to `ALPHA`.

<a id="x-28KONS-9-3AC-LERP-20FUNCTION-29"></a>
- [function] **C-LERP** *F C1 C2*

    Return a color linearly interpolated by factor `F` between `C1` and `C2`.
    
    ```common-lisp
    (c-lerp 0.5 (c! 0 0 0 0) (c! 0.2 0.4 0.6 0.8))
    => #(0.1 0.2 0.3 0.4)
    
    ```


<a id="x-28KONS-9-3AC-RAND-20FUNCTION-29"></a>
- [function] **C-RAND**

    Return an opaque color with uniform `RGB` channel values.
    
    ```common-lisp
    (dotimes (i 3)
      (print (c-rand)))
    ..
    .. #(0.8987992 0.7864139 0.79885733 1.0)
    .. #(0.33613765 0.39076257 0.20800936 1.0)
    .. #(0.45029986 0.5191544 0.37873483 1.0)
    => NIL
    
    ```


<a id="x-28KONS-9-3AC-RAND-WITH-ALPHA-20FUNCTION-29"></a>
- [function] **C-RAND-WITH-ALPHA**

    Return a color with uniform random `RGB` and alpha values.
    
    ```common-lisp
    (dotimes (i 3)
      (print (c-rand-with-alpha)))
    ..
    .. #(0.564777 0.8026259 0.25154138 0.43752754)
    .. #(0.5176456 0.29399824 0.5838525 0.17650497)
    .. #(0.1909852 0.15405178 0.6489651 0.350994)
    => NIL
    
    ```


<a id="x-28KONS-9-3AC-RAND2-20FUNCTION-29"></a>
- [function] **C-RAND2** *C1 C2*

<a id="x-28KONS-9-3AC-2B-20FUNCTION-29"></a>
- [function] **C+** *C1 C2*

<a id="x-28KONS-9-3AC-SCALE-20FUNCTION-29"></a>
- [function] **C-SCALE** *C1 X*

<a id="x-28KONS-9-3AC-JITTER-20FUNCTION-29"></a>
- [function] **C-JITTER** *C C-DELTA*

<a id="x-28KONS-9-3AC-RAINBOW-20FUNCTION-29"></a>
- [function] **C-RAINBOW** *F*

<a id="x-28KONS-9-3A-40MATRIX-20MGL-PAX-3ASECTION-29"></a>
## 5 Matrix

A matrix is a 4x4 array of numbers that represents an affine transformation in 3D space.

<a id="x-28KONS-9-3AMAKE-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-MATRIX**

<a id="x-28KONS-9-3AMAKE-MATRIX-WITH-20FUNCTION-29"></a>
- [function] **MAKE-MATRIX-WITH** *CONTENTS*

<a id="x-28KONS-9-3AMAKE-ID-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-ID-MATRIX**

<a id="x-28KONS-9-3AMATRIX--3ELIST-20FUNCTION-29"></a>
- [function] **MATRIX-\>LIST** *MATRIX*

<a id="x-28KONS-9-3AMATRIX--3EVECTOR-20FUNCTION-29"></a>
- [function] **MATRIX-\>VECTOR** *MATRIX*

<a id="x-28KONS-9-3AMATRIX-COPY-20FUNCTION-29"></a>
- [function] **MATRIX-COPY** *MATRIX*

<a id="x-28KONS-9-3AMAKE-TRANSLATION-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-TRANSLATION-MATRIX** *POINT*

<a id="x-28KONS-9-3AMAKE-ROTATION-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-ROTATION-MATRIX** *POINT ORDER &OPTIONAL (PIVOT NIL)*

<a id="x-28KONS-9-3AMAKE-X-ROTATION-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-X-ROTATION-MATRIX** *ANGLE*

<a id="x-28KONS-9-3AMAKE-Y-ROTATION-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-Y-ROTATION-MATRIX** *ANGLE*

<a id="x-28KONS-9-3AMAKE-Z-ROTATION-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-Z-ROTATION-MATRIX** *ANGLE*

<a id="x-28KONS-9-3AMAKE-AXIS-ROTATION-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-AXIS-ROTATION-MATRIX** *ANGLE AXIS &OPTIONAL (PIVOT NIL)*

<a id="x-28KONS-9-3AMAKE-SCALE-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-SCALE-MATRIX** *POINT &OPTIONAL (PIVOT NIL)*

<a id="x-28KONS-9-3AMAKE-SHEAR-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-SHEAR-MATRIX** *POINT*

<a id="x-28KONS-9-3AMAKE-Z-ALIGNMENT-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-Z-ALIGNMENT-MATRIX** *POINT*

<a id="x-28KONS-9-3AMAKE-LOOK-AT-FROM-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-LOOK-AT-FROM-MATRIX** *FROM TO &OPTIONAL (UP (P! 0 1 0))*

<a id="x-28KONS-9-3AMAKE-LOOK-DIR-FROM-MATRIX-20FUNCTION-29"></a>
- [function] **MAKE-LOOK-DIR-FROM-MATRIX** *FROM DIR &OPTIONAL (UP (P! 0 1 0))*

<a id="x-28KONS-9-3AMATRIX-MULTIPLY-20FUNCTION-29"></a>
- [function] **MATRIX-MULTIPLY** *MATRIX-1 MATRIX-2*

<a id="x-28KONS-9-3AMATRIX-MULTIPLY-N-20FUNCTION-29"></a>
- [function] **MATRIX-MULTIPLY-N** *&REST MATRICES*

<a id="x-28KONS-9-3ATRANSFORM-POINT-20FUNCTION-29"></a>
- [function] **TRANSFORM-POINT** *POINT MATRIX*

<a id="x-28KONS-9-3ATRANSFORM-POINTS-20FUNCTION-29"></a>
- [function] **TRANSFORM-POINTS** *POINTS MATRIX*

<a id="x-28KONS-9-3ATRANSFORM-POINT-21-20FUNCTION-29"></a>
- [function] **TRANSFORM-POINT!** *POINT MATRIX*

<a id="x-28KONS-9-3ATRANSFORM-POINT-ARRAY-21-20FUNCTION-29"></a>
- [function] **TRANSFORM-POINT-ARRAY!** *POINT-ARRAY MATRIX*

<a id="x-28KONS-9-3A-40SHAPE-20MGL-PAX-3ASECTION-29"></a>
## 6 Shape

<a id="x-28KONS-9-3ASHAPE-20CLASS-29"></a>
- [class] **SHAPE** *[SCENE-ITEM][12ec]*

<a id="x-28KONS-9-3ATRANSFORM-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASHAPE-29-29"></a>
- [accessor] **TRANSFORM** *SHAPE (:TRANSFORM = (MAKE-INSTANCE 'EULER-TRANSFORM))*

<a id="x-28KONS-9-3AIS-VISIBLE-3F-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASHAPE-29-29"></a>
- [accessor] **IS-VISIBLE?** *SHAPE (:IS-VISIBLE? = T)*

<a id="x-28KONS-9-3ASHOW-AXIS-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASHAPE-29-29"></a>
- [accessor] **SHOW-AXIS** *SHAPE (:SHOW-AXIS = NIL)*

<a id="x-28KONS-9-3ASHOW-BOUNDS-3F-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASHAPE-29-29"></a>
- [accessor] **SHOW-BOUNDS?** *SHAPE (:SHOW-BOUNDS? = NIL)*

Relative.

<a id="x-28KONS-9-3ATRANSLATE-BY-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-20T-29-29-29"></a>
- [method] **TRANSLATE-BY** *(SELF SHAPE) P*

<a id="x-28KONS-9-3AROTATE-BY-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-20T-29-29-29"></a>
- [method] **ROTATE-BY** *(SELF SHAPE) P*

<a id="x-28KONS-9-3ASCALE-BY-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-20T-29-29-29"></a>
- [method] **SCALE-BY** *(SELF SHAPE) P*

Absolute.

<a id="x-28KONS-9-3ATRANSLATE-TO-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-20T-29-29-29"></a>
- [method] **TRANSLATE-TO** *(SELF SHAPE) P*

<a id="x-28KONS-9-3AROTATE-TO-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-20T-29-29-29"></a>
- [method] **ROTATE-TO** *(SELF SHAPE) P*

<a id="x-28KONS-9-3ASCALE-TO-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-20T-29-29-29"></a>
- [method] **SCALE-TO** *(SELF SHAPE) P*

<a id="x-28KONS-9-3ASCALE-BY-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-20NUMBER-29-29-29"></a>
- [method] **SCALE-BY** *(SELF SHAPE) (S NUMBER)*

Special.

<a id="x-28KONS-9-3ARESET-TRANSFORM-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-29-29-29"></a>
- [method] **RESET-TRANSFORM** *(SELF SHAPE)*

<a id="x-28KONS-9-3ACENTER-AT-ORIGIN-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-29-29-29"></a>
- [method] **CENTER-AT-ORIGIN** *(SELF SHAPE)*

<a id="x-28KONS-9-3ASCALE-TO-SIZE-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-20T-29-29-29"></a>
- [method] **SCALE-TO-SIZE** *(SELF SHAPE) MAX-SIZE*

<a id="x-28KONS-9-3AGET-BOUNDS-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-29-29-29"></a>
- [method] **GET-BOUNDS** *(SELF SHAPE)*

<a id="x-28KONS-9-3A-40POINT-CLOUD-20MGL-PAX-3ASECTION-29"></a>
### 6.1 Point cloud

A point-cloud is a shape made up of colored points.

<a id="x-28KONS-9-3APOINT-CLOUD-20CLASS-29"></a>
- [class] **POINT-CLOUD** *[SHAPE][2231]*

<a id="x-28KONS-9-3APOINTS-20-28MGL-PAX-3AACCESSOR-20KONS-9-3APOINT-CLOUD-29-29"></a>
- [accessor] **POINTS** *POINT-CLOUD (:POINTS = (MAKE-ARRAY 0 :ADJUSTABLE T :FILL-POINTER T))*

<a id="x-28KONS-9-3APOINT-COLORS-20-28MGL-PAX-3AACCESSOR-20KONS-9-3APOINT-CLOUD-29-29"></a>
- [accessor] **POINT-COLORS** *POINT-CLOUD (:POINT-COLORS = NIL)*

<a id="x-28KONS-9-3AMAKE-POINT-CLOUD-20FUNCTION-29"></a>
- [function] **MAKE-POINT-CLOUD** *POINTS*

<a id="x-28KONS-9-3AMAKE-LINE-POINTS-20FUNCTION-29"></a>
- [function] **MAKE-LINE-POINTS** *P1 P2 NUM-SEGMENTS*

<a id="x-28KONS-9-3AMAKE-RECTANGLE-POINTS-20FUNCTION-29"></a>
- [function] **MAKE-RECTANGLE-POINTS** *WIDTH HEIGHT &OPTIONAL (NUM-SEGMENTS 1)*

<a id="x-28KONS-9-3AMAKE-CIRCLE-POINTS-20FUNCTION-29"></a>
- [function] **MAKE-CIRCLE-POINTS** *DIAMETER NUM-SEGMENTS*

<a id="x-28KONS-9-3AMAKE-ARC-POINTS-20FUNCTION-29"></a>
- [function] **MAKE-ARC-POINTS** *DIAMETER START-ANGLE END-ANGLE NUM-SEGMENTS*

<a id="x-28KONS-9-3AMAKE-SPIRAL-POINTS-20FUNCTION-29"></a>
- [function] **MAKE-SPIRAL-POINTS** *START-DIAMETER END-DIAMETER AXIS-LENGTH NUM-LOOPS NUM-SEGMENTS*

<a id="x-28KONS-9-3AMAKE-SINE-CURVE-POINTS-20FUNCTION-29"></a>
- [function] **MAKE-SINE-CURVE-POINTS** *PERIOD FREQUENCY X-SCALE Y-SCALE NUM-SEGMENTS*

<a id="x-28KONS-9-3AMAKE-RANDOM-POINTS-20FUNCTION-29"></a>
- [function] **MAKE-RANDOM-POINTS** *NUM BOUNDS-LO BOUNDS-HI*

<a id="x-28KONS-9-3AMAKE-GRID-POINTS-20FUNCTION-29"></a>
- [function] **MAKE-GRID-POINTS** *NX NY NZ BOUNDS-LO BOUNDS-HI*

<a id="x-28KONS-9-3AFREEZE-TRANSFORM-20-28METHOD-20NIL-20-28KONS-9-3APOINT-CLOUD-29-29-29"></a>
- [method] **FREEZE-TRANSFORM** *(P-CLOUD POINT-CLOUD)*

<a id="x-28KONS-9-3AALLOCATE-POINT-COLORS-20-28METHOD-20NIL-20-28KONS-9-3APOINT-CLOUD-29-29-29"></a>
- [method] **ALLOCATE-POINT-COLORS** *(P-CLOUD POINT-CLOUD)*

<a id="x-28KONS-9-3ARESET-POINT-COLORS-20-28METHOD-20NIL-20-28KONS-9-3APOINT-CLOUD-29-29-29"></a>
- [method] **RESET-POINT-COLORS** *(P-CLOUD POINT-CLOUD)*

<a id="x-28KONS-9-3ASET-POINT-COLORS-BY-XYZ-20-28METHOD-20NIL-20-28KONS-9-3APOINT-CLOUD-20T-29-29-29"></a>
- [method] **SET-POINT-COLORS-BY-XYZ** *(P-CLOUD POINT-CLOUD) COLOR-FN*

<a id="x-28KONS-9-3ARANDOMIZE-POINTS-20-28METHOD-20NIL-20-28KONS-9-3APOINT-CLOUD-20T-29-29-29"></a>
- [method] **RANDOMIZE-POINTS** *(P-CLOUD POINT-CLOUD) DELTA*

<a id="x-28KONS-9-3A-40POLYHEDRON-20MGL-PAX-3ASECTION-29"></a>
#### 6.1.1 Polyhedron

A polyhedron is a 3D shape formed by flat polygonal faces. The faces of a
polyhedron are represented in terms of the vertices of an underlying
point-cloud.

<a id="x-28KONS-9-3APOLYHEDRON-20CLASS-29"></a>
- [class] **POLYHEDRON** *[POINT-CLOUD][6ea6]*

<a id="x-28KONS-9-3AFACES-20-28MGL-PAX-3AACCESSOR-20KONS-9-3APOLYHEDRON-29-29"></a>
- [accessor] **FACES** *POLYHEDRON (:FACES = (MAKE-ARRAY 0 :ADJUSTABLE T :FILL-POINTER T))*

<a id="x-28KONS-9-3AFACE-NORMALS-20-28MGL-PAX-3AACCESSOR-20KONS-9-3APOLYHEDRON-29-29"></a>
- [accessor] **FACE-NORMALS** *POLYHEDRON (:FACE-NORMALS = (MAKE-ARRAY 0 :ADJUSTABLE T :FILL-POINTER T))*

<a id="x-28KONS-9-3APOINT-NORMALS-20-28MGL-PAX-3AACCESSOR-20KONS-9-3APOLYHEDRON-29-29"></a>
- [accessor] **POINT-NORMALS** *POLYHEDRON (:POINT-NORMALS = (MAKE-ARRAY 0 :ADJUSTABLE T :FILL-POINTER T))*

<a id="x-28KONS-9-3ASHOW-NORMALS-20-28MGL-PAX-3AACCESSOR-20KONS-9-3APOLYHEDRON-29-29"></a>
- [accessor] **SHOW-NORMALS** *POLYHEDRON (:SHOW-NORMALS = NIL)*

<a id="x-28KONS-9-3APOINT-SOURCE-USE-FACE-CENTERS-3F-20-28MGL-PAX-3AACCESSOR-20KONS-9-3APOLYHEDRON-29-29"></a>
- [accessor] **POINT-SOURCE-USE-FACE-CENTERS?** *POLYHEDRON (:POINT-SOURCE-USE-FACE-CENTERS? = NIL)*

Polyhedrons can be constructed to represent (or approximate) many common geometric shapes.

<a id="x-28KONS-9-3AMAKE-POLYHEDRON-20FUNCTION-29"></a>
- [function] **MAKE-POLYHEDRON** *POINTS FACES &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-POLYHEDRON-20FUNCTION-29"></a>
- [function] **MAKE-POLYHEDRON** *POINTS FACES &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-RECTANGLE-POLYHEDRON-20FUNCTION-29"></a>
- [function] **MAKE-RECTANGLE-POLYHEDRON** *WIDTH HEIGHT &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-SQUARE-POLYHEDRON-20FUNCTION-29"></a>
- [function] **MAKE-SQUARE-POLYHEDRON** *SIDE &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-CIRCLE-POLYHEDRON-20FUNCTION-29"></a>
- [function] **MAKE-CIRCLE-POLYHEDRON** *DIAMETER NUM-SEGMENTS &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-TETRAHEDRON-20FUNCTION-29"></a>
- [function] **MAKE-TETRAHEDRON** *DIAMETER &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-BOX-20FUNCTION-29"></a>
- [function] **MAKE-BOX** *X-SIZE Y-SIZE Z-SIZE &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-CUBE-20FUNCTION-29"></a>
- [function] **MAKE-CUBE** *SIDE &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-CUT-CUBE-20FUNCTION-29"></a>
- [function] **MAKE-CUT-CUBE** *SIDE &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-OCTAHEDRON-20FUNCTION-29"></a>
- [function] **MAKE-OCTAHEDRON** *DIAMETER &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-DODECAHEDRON-20FUNCTION-29"></a>
- [function] **MAKE-DODECAHEDRON** *DIAMETER &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-ICOSAHEDRON-20FUNCTION-29"></a>
- [function] **MAKE-ICOSAHEDRON** *DIAMETER &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AMAKE-CUBE-SPHERE-20FUNCTION-29"></a>
- [function] **MAKE-CUBE-SPHERE** *SIDE SUBDIV-LEVELS &KEY (NAME NIL) (MESH-TYPE 'POLYHEDRON)*

<a id="x-28KONS-9-3AEMPTY-POLYHEDRON-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-29-29-29"></a>
- [method] **EMPTY-POLYHEDRON** *(POLYH POLYHEDRON)*

<a id="x-28KONS-9-3ASET-FACE-POINT-LISTS-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-20T-29-29-29"></a>
- [method] **SET-FACE-POINT-LISTS** *(POLYH POLYHEDRON) POINT-LISTS*

<a id="x-28KONS-9-3ASET-TRIANGLE-ARRAYS-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-20T-29-29-29"></a>
- [method] **SET-TRIANGLE-ARRAYS** *(POLYH POLYHEDRON) TRIANGLE-ARRAYS*

<a id="x-28KONS-9-3AFACE-CENTER-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-20T-29-29-29"></a>
- [method] **FACE-CENTER** *(POLYH POLYHEDRON) FACE*

<a id="x-28KONS-9-3AFACE-CENTERS-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-29-29-29"></a>
- [method] **FACE-CENTERS** *(POLYH POLYHEDRON)*

<a id="x-28KONS-9-3AFACE-NORMAL-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-20T-29-29-29"></a>
- [method] **FACE-NORMAL** *(POLYH POLYHEDRON) FACE*

<a id="x-28KONS-9-3ACOMPUTE-FACE-NORMALS-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-29-29-29"></a>
- [method] **COMPUTE-FACE-NORMALS** *(POLYH POLYHEDRON)*

<a id="x-28KONS-9-3ACOMPUTE-POINT-NORMALS-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-29-29-29"></a>
- [method] **COMPUTE-POINT-NORMALS** *(POLYH POLYHEDRON)*

<a id="x-28KONS-9-3ACOMPUTE-POINT-NORMALS-SAV-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-29-29-29"></a>
- [method] **COMPUTE-POINT-NORMALS-SAV** *(POLYH POLYHEDRON)*

<a id="x-28KONS-9-3AFACE-POINTS-LIST-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-20INTEGER-29-29-29"></a>
- [method] **FACE-POINTS-LIST** *(POLYH POLYHEDRON) (I INTEGER)*

<a id="x-28KONS-9-3AFACE-POINTS-LIST-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-20LIST-29-29-29"></a>
- [method] **FACE-POINTS-LIST** *(POLYH POLYHEDRON) (FACE LIST)*

<a id="x-28KONS-9-3AFACE-POINTS-ARRAY-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-20INTEGER-29-29-29"></a>
- [method] **FACE-POINTS-ARRAY** *(POLYH POLYHEDRON) (I INTEGER)*

<a id="x-28KONS-9-3AFACE-POINTS-ARRAY-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-20LIST-29-29-29"></a>
- [method] **FACE-POINTS-ARRAY** *(POLYH POLYHEDRON) (FACE LIST)*

<a id="x-28KONS-9-3AREVERSE-FACE-NORMALS-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-29-29-29"></a>
- [method] **REVERSE-FACE-NORMALS** *(POLYH POLYHEDRON)*

<a id="x-28KONS-9-3ASET-POINT-COLORS-BY-POINT-AND-NORMAL-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-20T-29-29-29"></a>
- [method] **SET-POINT-COLORS-BY-POINT-AND-NORMAL** *(POLYH POLYHEDRON) COLOR-FN*

<a id="x-28KONS-9-3ASET-POINT-COLORS-UNIFORM-20-28METHOD-20NIL-20-28KONS-9-3APOLYHEDRON-20T-29-29-29"></a>
- [method] **SET-POINT-COLORS-UNIFORM** *(POLYH POLYHEDRON) COLOR*

<a id="x-28KONS-9-3A-40GROUP-20MGL-PAX-3ASECTION-29"></a>
## 7 Groups (mixin)

<a id="x-28KONS-9-3AGROUP-MIXIN-20CLASS-29"></a>
- [class] **GROUP-MIXIN**

<a id="x-28KONS-9-3ACHILDREN-20-28MGL-PAX-3AACCESSOR-20KONS-9-3AGROUP-MIXIN-29-29"></a>
- [accessor] **CHILDREN** *GROUP-MIXIN (:CHILDREN = (MAKE-ARRAY 0 :ADJUSTABLE T :FILL-POINTER 0))*

<a id="x-28KONS-9-3APRINTABLE-DATA-20-28METHOD-20NIL-20-28KONS-9-3AGROUP-MIXIN-29-29-29"></a>
- [method] **PRINTABLE-DATA** *(SELF GROUP-MIXIN)*

<a id="x-28KONS-9-3ANUM-CHILDREN-20-28METHOD-20NIL-20-28KONS-9-3AGROUP-MIXIN-29-29-29"></a>
- [method] **NUM-CHILDREN** *(GROUP GROUP-MIXIN)*

<a id="x-28KONS-9-3AGET-CHILD-20-28METHOD-20NIL-20-28KONS-9-3AGROUP-MIXIN-20T-29-29-29"></a>
- [method] **GET-CHILD** *(GROUP GROUP-MIXIN) I*

<a id="x-28KONS-9-3ACHILDREN-AS-LIST-20-28METHOD-20NIL-20-28KONS-9-3AGROUP-MIXIN-29-29-29"></a>
- [method] **CHILDREN-AS-LIST** *(GROUP GROUP-MIXIN)*

<a id="x-28KONS-9-3AADD-CHILD-20-28METHOD-20NIL-20-28KONS-9-3AGROUP-MIXIN-20KONS-9-3ASCENE-ITEM-29-29-29"></a>
- [method] **ADD-CHILD** *(GROUP GROUP-MIXIN) (ITEM SCENE-ITEM)*

<a id="x-28KONS-9-3AREMOVE-CHILD-20-28METHOD-20NIL-20-28KONS-9-3AGROUP-MIXIN-20KONS-9-3ASCENE-ITEM-29-29-29"></a>
- [method] **REMOVE-CHILD** *(GROUP GROUP-MIXIN) (ITEM SCENE-ITEM)*

<a id="x-28KONS-9-3ASET-CHILDREN-20-28METHOD-20NIL-20-28KONS-9-3AGROUP-MIXIN-20T-29-29-29"></a>
- [method] **SET-CHILDREN** *(GROUP GROUP-MIXIN) SCENE-ITEMS*

<a id="x-28KONS-9-3AREMOVE-ALL-CHILDREN-20-28METHOD-20NIL-20-28KONS-9-3AGROUP-MIXIN-29-29-29"></a>
- [method] **REMOVE-ALL-CHILDREN** *(GROUP GROUP-MIXIN)*

<a id="x-28KONS-9-3A-40SHAPE-GROUP-20MGL-PAX-3ASECTION-29"></a>
### 7.1 Shape Groups

<a id="x-28KONS-9-3ASHAPE-GROUP-20CLASS-29"></a>
- [class] **SHAPE-GROUP** *[SHAPE][2231] [GROUP-MIXIN][56de]*

<a id="x-28KONS-9-3AMAKE-SHAPE-GROUP-20FUNCTION-29"></a>
- [function] **MAKE-SHAPE-GROUP** *SHAPES &KEY (NAME NIL)*

<a id="x-28KONS-9-3AGET-BOUNDS-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-GROUP-29-29-29"></a>
- [method] **GET-BOUNDS** *(GROUP SHAPE-GROUP)*

<a id="x-28KONS-9-3ASET-POINT-COLORS-BY-XYZ-20-28METHOD-20NIL-20-28KONS-9-3ASHAPE-GROUP-20T-29-29-29"></a>
- [method] **SET-POINT-COLORS-BY-XYZ** *(GROUP SHAPE-GROUP) COLOR-FN*

<a id="x-28KONS-9-3ASCATTER-SHAPES-IN-GROUP-20FUNCTION-29"></a>
- [function] **SCATTER-SHAPES-IN-GROUP** *SHAPE-FN POINT-ARRAY*

<a id="x-28KONS-9-3ASCATTER-SHAPES-20FUNCTION-29"></a>
- [function] **SCATTER-SHAPES** *SHAPES POINT-ARRAY*

<a id="x-28KONS-9-3ASCATTER-SHAPE-INSTANCES-20FUNCTION-29"></a>
- [function] **SCATTER-SHAPE-INSTANCES** *SHAPES POINT-ARRAY*

<a id="x-28KONS-9-3A-40SCENE-20MGL-PAX-3ASECTION-29"></a>
## 8 Scene and item

Item

<a id="x-28KONS-9-3AITEM-20CLASS-29"></a>
- [class] **ITEM**

Scene item

<a id="x-28KONS-9-3ASCENE-ITEM-20CLASS-29"></a>
- [class] **SCENE-ITEM** *[ITEM][e53d]*

Scene

<a id="x-28KONS-9-3ASCENE-20CLASS-29"></a>
- [class] **SCENE** *[ITEM][e53d]*

<a id="x-28KONS-9-3ASHAPE-ROOT-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASCENE-29-29"></a>
- [accessor] **SHAPE-ROOT** *SCENE (:SHAPE-ROOT = (MAKE-INSTANCE 'SHAPE-GROUP :NAME 'SHAPES))*

<a id="x-28KONS-9-3AMOTION-ROOT-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASCENE-29-29"></a>
- [accessor] **MOTION-ROOT** *SCENE (:MOTION-ROOT = (MAKE-INSTANCE 'MOTION-GROUP :NAME 'MOTIONS))*

<a id="x-28KONS-9-3AINTERACTOR-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASCENE-29-29"></a>
- [accessor] **INTERACTOR** *SCENE (:INTERACTOR = NIL)*

<a id="x-28KONS-9-3AINITIALIZED-3F-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASCENE-29-29"></a>
- [accessor] **INITIALIZED?** *SCENE (:INITIALIZED? = NIL)*

<a id="x-28KONS-9-3ASELECTION-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASCENE-29-29"></a>
- [accessor] **SELECTION** *SCENE (:SELECTION = 'NIL)*

<a id="x-28KONS-9-3ASTART-FRAME-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASCENE-29-29"></a>
- [accessor] **START-FRAME** *SCENE (:START-FRAME = 0)*

<a id="x-28KONS-9-3AEND-FRAME-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASCENE-29-29"></a>
- [accessor] **END-FRAME** *SCENE (:END-FRAME = 240)*

<a id="x-28KONS-9-3ACURRENT-FRAME-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASCENE-29-29"></a>
- [accessor] **CURRENT-FRAME** *SCENE (:CURRENT-FRAME = 0)*

<a id="x-28KONS-9-3AFPS-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ASCENE-29-29"></a>
- [accessor] **FPS** *SCENE (:FPS = 24)*

<a id="x-28KONS-9-3A-40COMMAND-TABLE-20MGL-PAX-3ASECTION-29"></a>
## 9 Command table

<a id="x-28KONS-9-3ACOMMAND-TABLE-20CLASS-29"></a>
- [class] **COMMAND-TABLE**

    A command table defines a menu of keyboard-driven user interface commands. Each
    command either performs an action or recursively opens a new command table as a
    submenu.
    
    [`MAKE-INSTANCE`][9743] is used to create command table objects.

<a id="x-28KONS-9-3ATITLE-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ACOMMAND-TABLE-29-29"></a>
- [accessor] **TITLE** *COMMAND-TABLE (:TITLE = NIL)*

<a id="x-28KONS-9-3AENTRIES-20-28MGL-PAX-3AACCESSOR-20KONS-9-3ACOMMAND-TABLE-29-29"></a>
- [accessor] **ENTRIES** *COMMAND-TABLE (:ENTRIES = (MAKE-ARRAY 0 :ADJUSTABLE T :FILL-POINTER T))*

One command table is *active* at any given time.

<a id="x-28KONS-9-3AACTIVE-COMMAND-TABLE-20FUNCTION-29"></a>
- [function] **ACTIVE-COMMAND-TABLE**

    Return the currently active command table.

<a id="x-28KONS-9-3AMAKE-ACTIVE-COMMAND-TABLE-20FUNCTION-29"></a>
- [function] **MAKE-ACTIVE-COMMAND-TABLE** *TABLE*

    Make `TABLE` the active command table.

Commands and nested command tables are defined using macros.

<a id="x-28KONS-9-3ACT-ENTRY-20MGL-PAX-3AMACRO-29"></a>
- [macro] **CT-ENTRY** *KEY-BINDING HELP &REST EXPR*

    Define a user interface command. The new command is added to the command table
    named `table` (a variable captured by this macro.)
    
    `KEY-BINDING` is a keyword designating the keyboard input that invokes the command.
    
    `HELP` is a short string description e.g. "Open Scene File".
    
    `EXPR` is one or more Lisp forms to be evaluated when the command is invoked.

<a id="x-28KONS-9-3ACT-SUBTABLE-20MGL-PAX-3AMACRO-29"></a>
- [macro] **CT-SUBTABLE** *KEY-BINDING TITLE C-TABLE-FN*

    Define a nested table of user interface commands.
    
    `KEY-BINDING` is as in the [`CT-ENTRY`][f913] macro.
    
    `TITLE` is the string name of the menu e.g. "Edit".
    
    `C-TABLE-FN` is an expression to be evaluated to return the command table.

Here is an example of constructing a command table:

```common-lisp
   (let ((table (make-instance 'command-table :title "Example")))
     ;; note: these macros depend on the variable name 'table'
     (ct-entry :C "Make a Cube" (make-cube 2.0))
     (ct-subtable :S "My Submenu" (make-command-table 'my-submenu))
     (entries table))
```


<a id="x-28KONS-9-3A-40TOP-LEVEL-20MGL-PAX-3ASECTION-29"></a>
## 10 Top-level

Kons-9 runs on a dedicated thread.

<a id="x-28KONS-9-3ARUN-20FUNCTION-29"></a>
- [function] **RUN** *&OPTIONAL (COMMAND-TABLE NIL)*

    Open the Kons-9 user interface window and operate it using a dedicated thread.
    
    `COMMAND-TABLE` can optionally be supplied as a custom top-level command table.
    See [Command table][d298]s.

<a id="x-28KONS-9-3A-2ASCENE-2A-20VARIABLE-29"></a>
- [variable] **\*SCENE\*** *#\<SCENE , frame bounds: 0 240, current: 0  {1006EF3833}\>*

    Global scene object.

  [0eea]: #x-28KONS-9-3A-40SCENE-20MGL-PAX-3ASECTION-29 "Scene and item"
  [12ec]: #x-28KONS-9-3ASCENE-ITEM-20CLASS-29 "KONS-9:SCENE-ITEM CLASS"
  [1531]: #x-28KONS-9-3AP-CENTER-20FUNCTION-29 "KONS-9:P-CENTER FUNCTION"
  [1cd7]: #x-28KONS-9-3A-40INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [20ea]: #x-28KONS-9-3A-40SHAPE-20MGL-PAX-3ASECTION-29 "Shape"
  [2231]: #x-28KONS-9-3ASHAPE-20CLASS-29 "KONS-9:SHAPE CLASS"
  [2eeb]: #x-28KONS-9-3A-40COLOR-20MGL-PAX-3ASECTION-29 "Color"
  [388c]: #x-28KONS-9-3A-40SHAPE-GROUP-20MGL-PAX-3ASECTION-29 "Shape Groups"
  [3a9c]: #x-28KONS-9-3A-40POINT-METRICS-20MGL-PAX-3ASECTION-29 "Point metrics"
  [409c]: #x-28KONS-9-3A-40POLYHEDRON-20MGL-PAX-3ASECTION-29 "Polyhedron"
  [4578]: #x-28KONS-9-3A-40TOP-LEVEL-20MGL-PAX-3ASECTION-29 "Top-level"
  [4a53]: #x-28-22kons-9-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"kons-9" ASDF/SYSTEM:SYSTEM'
  [4cb9]: #x-28KONS-9-3A-40POINT-ARITHMETIC-20MGL-PAX-3ASECTION-29 "Point arithmetic"
  [56de]: #x-28KONS-9-3AGROUP-MIXIN-20CLASS-29 "KONS-9:GROUP-MIXIN CLASS"
  [682d]: #x-28KONS-9-3A-40POINT-VECTOR-20MGL-PAX-3ASECTION-29 "Vectors as points"
  [6ea6]: #x-28KONS-9-3APOINT-CLOUD-20CLASS-29 "KONS-9:POINT-CLOUD CLASS"
  [7616]: #x-28KONS-9-3A-40POINT-VECTOR-TRIG-20MGL-PAX-3ASECTION-29 "Trigonometry"
  [7f7a]: #x-28KONS-9-3A-40MATRIX-20MGL-PAX-3ASECTION-29 "Matrix"
  [8dfa]: #x-28KONS-9-3A-40POINT-GEOMETRY-20MGL-PAX-3ASECTION-29 "Geometry operations on points"
  [9743]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ins.htm "MAKE-INSTANCE FUNCTION"
  [ab46]: #x-28KONS-9-3A-40POINT-20MGL-PAX-3ASECTION-29 "Points"
  [c03e]: #x-28KONS-9-3A-40GROUP-20MGL-PAX-3ASECTION-29 "Groups (mixin)"
  [c188]: #x-28KONS-9-3A-40POINT-COMPUTING-20MGL-PAX-3ASECTION-29 "Computing new points"
  [ccbf]: #x-28KONS-9-3A-40POINT-LISP-20MGL-PAX-3ASECTION-29 "Lisp object operations on points"
  [d298]: #x-28KONS-9-3A-40COMMAND-TABLE-20MGL-PAX-3ASECTION-29 "Command table"
  [dc52]: #x-28KONS-9-3A-40POINT-CLOUD-20MGL-PAX-3ASECTION-29 "Point cloud"
  [e53d]: #x-28KONS-9-3AITEM-20CLASS-29 "KONS-9:ITEM CLASS"
  [f913]: #x-28KONS-9-3ACT-ENTRY-20MGL-PAX-3AMACRO-29 "KONS-9:CT-ENTRY MGL-PAX:MACRO"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
