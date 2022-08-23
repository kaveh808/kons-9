;;;; kons-9.asd
;;;; Author: Kaveh Kardan
;;;;  SBCL Linux port by Joel Boehland (joel.boehland@evocomputing.com)
;;;;  SBCL macOS port by mikel evins (mikel@evins.net) based on Joel Boehland's work

(asdf:defsystem #:kons-9
    :description "Common Lisp 3D Graphics System"
    :author "Kaveh Kardan"
    :license "MIT"
    :depends-on
    (#:closer-mop
     #:trivial-main-thread
     #:trivial-backtrace
     #:cffi
     #:cl-opengl
     #:cl-glu
     #:cl-glfw3) 
    :serial t
    :components
    ((:file "common/package")
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

#+nil (trivial-main-thread:call-in-main-thread
       (lambda ()
         (sb-int:set-floating-point-modes :traps nil)
         (kons-9::show-window kons-9::*scene*)))
