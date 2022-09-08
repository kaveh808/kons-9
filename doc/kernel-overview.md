# kons-9

## Kernel Classes

kons-9 consists of a kernel and plugins. The kernel is intended to act as a simple and minimal set of classes on which various facilities can be built. It allows for the creation of a scene with geometry and animations.

The class hierarchy is as follows:

    ITEM 
        SCENE 
        SCENE-ITEM 
            SHAPE 
                GROUP 
                POINT-CLOUD 
                    POLYGON 
                    POLYHEDRON 
            MOTION 
                MOTION-GROUP 
                ANIMATOR 
                    SHAPE-ANIMATOR 
        TRANSFORM 
            EULER-TRANSFORM 
            ANGLE-AXIS-TRANSFORM 
            GENERALIZED-TRANSFORM
        TRANSFORM-OPERATOR
            TRANSLATE-OPERATOR 
            EULER-ROTATE-OPERATOR 
            ANGLE-AXIS-ROTATE-OPERATOR
            SCALE-OPERATOR 


## Scene Data and Organization

A `SCENE` is the primary organizational class in kons-9. The contents of a scene are subclasses of `SCENE-ITEM` which are animated and rendered in a 3D viewer window.

A scene contains two hierarchies: one of `SHAPE` items for geometry, and one of `MOTION` items for animation.

Every `SHAPE` has a `TRANSFORM` which encodes the geometric transformation (scale, rotate, translate) of the shape. The classes `POLYGON` and `POLYHEDRON` store the geometry data of the scene. The hierarchy is constructed of `GROUP` instances which contain lists of children, and can be a directed acyclic graph.

The second hierarchy in a `SCENE` stores the animation. This hierarchy consists of `MOTION` and `MOTION-GROUP` instances, analogous to the geometric hierarchy. These motion classes have start times and durations and can be used to build groups of animations. The actual animations are carried out by the `ANIMATOR` and `SHAPE-ANIMATOR` classes, which execute their update functions at each frame.

There are three subclasses of `TRANSFORM`, which differ by what transform operators they contain.

