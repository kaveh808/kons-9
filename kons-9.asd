;;;; kons-9.asd

(asdf:defsystem #:kons-9
    :description "Common Lisp 3D Graphics System"
    :author "Kaveh Kardan"
    :license "MIT"
    :depends-on
    #+ccl (#:closer-mop)
    #+sbcl (#:closer-mop
            #:trivial-main-thread
            #:trivial-backtrace
            #:cffi
            #:cl-opengl
            #:cl-glu
            #:cl-glfw3) 
    :serial t
    :components
    ;; ---------------------------------------------------------------------
    #+ccl ((:file "ccl/ccl64")
           (:file "common/package")
           (:file "ccl/kernel/utils")
           (:file "ccl/kernel/color")
           (:file "ccl/kernel/point")
           (:file "ccl/kernel/matrix")
           (:file "ccl/kernel/noise")
           (:file "ccl/kernel/opengl")
           (:file "ccl/kernel/transform")
           (:file "ccl/kernel/scene-item")
           (:file "ccl/kernel/shape")
           (:file "ccl/kernel/point-cloud")
           (:file "ccl/kernel/polygon")
           (:file "ccl/kernel/polyhedron")
           (:file "ccl/kernel/group")
           (:file "ccl/kernel/procedural")
           (:file "ccl/kernel/animator")
           (:file "ccl/kernel/scene")
           (:file "ccl/kernel/scene-draw")
           (:file "ccl/kernel/scene-generics")
           (:file "ccl/kernel/protocol")
           (:file "ccl/kernel/main")
           (:file "ccl/plugins/parametric-curve")
           (:file "ccl/plugins/uv-mesh")
           (:file "ccl/plugins/heightfield")
           (:file "ccl/plugins/superquadric")
           (:file "ccl/plugins/sweep-mesh")
           (:file "ccl/plugins/sweep-mesh-group")
           (:file "ccl/plugins/dynamics-animator")
           (:file "ccl/plugins/procedural-curve-shape")
           (:file "ccl/plugins/manager-group")
           (:file "ccl/plugins/particle")
           (:file "ccl/plugins/l-system")
           (:file "ccl/plugins/poly-mesh")
           (:file "ccl/plugins/usd")
           (:file "ccl/plugins/obj")
           (:file "ccl/minimal-ui"))
    ;; ---------------------------------------------------------------------
    #+sbcl ((:file "common/package")
            (:file "sbcl/kernel/utils")
            (:file "sbcl/kernel/color")
            (:file "sbcl/kernel/point")
            (:file "sbcl/kernel/matrix")
            (:file "sbcl/kernel/noise")
            (:file "sbcl/kernel/portable-opengl")
            (:file "sbcl/kernel/transform")
            (:file "sbcl/kernel/scene-item")
            (:file "sbcl/kernel/shape")
            (:file "sbcl/kernel/point-cloud")
            (:file "sbcl/kernel/polygon")
            (:file "sbcl/kernel/polyhedron")
            (:file "sbcl/kernel/group")
            (:file "sbcl/kernel/procedural")
            (:file "sbcl/kernel/animator")
            (:file "sbcl/kernel/scene")
            (:file "sbcl/kernel/protocol")
            (:file "sbcl/kernel/main")
            (:file "sbcl/plugins/parametric-curve")
            (:file "sbcl/plugins/uv-mesh")
            (:file "sbcl/plugins/heightfield")
            (:file "sbcl/plugins/superquadric")
            (:file "sbcl/plugins/sweep-mesh")
            (:file "sbcl/plugins/sweep-mesh-group")
            (:file "sbcl/plugins/dynamics-animator")
            (:file "sbcl/plugins/procedural-curve-shape")
            (:file "sbcl/plugins/manager-group")
            (:file "sbcl/plugins/particle")
            (:file "sbcl/plugins/l-system")
            (:file "sbcl/plugins/usd")
            (:file "sbcl/plugins/obj")
            (:file "sbcl/minimal-ui")))

#+nil (asdf:load-system :kons-9)

#+nil (in-package :kons-9)

;;; sbcl only:
#+nil (trivial-main-thread:call-in-main-thread
       (lambda ()
         (sb-int:set-floating-point-modes :traps nil)
         (kons-9::show-window kons-9::*scene*)))

;;; ccl only:
#+nil (run)
