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
    ((:file "src/package")
     (:file "src/kernel/utils")
     (:file "src/kernel/color")
     (:file "src/kernel/point-simd")
     (:file "src/kernel/matrix")
     (:file "src/kernel/noise")
     (:file "src/graphics/opengl/opengl")
     (:file "src/graphics/opengl/ui")
;     (:file "src/graphics/opengl/text")
     (:file "src/kernel/item")
     (:file "src/kernel/transform")
     (:file "src/kernel/scene-item")
     (:file "src/kernel/shape")
     (:file "src/kernel/point-cloud")
     (:file "src/kernel/curve")
     (:file "src/kernel/polyhedron")
     (:file "src/kernel/group")
     (:file "src/kernel/procedural")
     (:file "src/kernel/motion")
     (:file "src/kernel/animator")
     (:file "src/kernel/animation")
     (:file "src/kernel/scene")
     (:file "src/kernel/scene-draw")
     (:file "src/kernel/scene-hierarchy")
     (:file "src/kernel/scene-duplicate")
     (:file "src/kernel/protocol")
     (:file "src/kernel/main")
     (:file "src/plugins/parametric-curve")
     (:file "src/plugins/uv-mesh")
     (:file "src/plugins/heightfield")
     (:file "src/plugins/superquadric")
     (:file "src/plugins/sweep-mesh")
     (:file "src/plugins/sweep-mesh-group")
     (:file "src/plugins/channel")
     (:file "src/plugins/dynamics-animator")
     (:file "src/plugins/procedural-curve")
     (:file "src/plugins/manager-group")
     (:file "src/plugins/particle")
     (:file "src/plugins/l-system")
     (:file "src/plugins/poly-mesh")
     (:file "src/plugins/usd")
     (:file "src/plugins/obj")
     (:file "src/ui/command-table")
     (:file "src/graphics/glfw/minimal-ui")))

#+nil (asdf:load-system :kons-9)

#+nil (trivial-main-thread:call-in-main-thread
       (lambda ()
         (sb-int:set-floating-point-modes :traps nil)
         (kons-9::show-window kons-9::*scene*)))

#+nil (in-package :kons-9)
