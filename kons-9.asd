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
     (:file "macos/kernel/utils")
     (:file "macos/kernel/color")
     (:file "macos/kernel/point")
     (:file "macos/kernel/matrix")
     (:file "macos/kernel/noise")
     (:file "macos/kernel/portable-opengl")
     (:file "macos/kernel/transform")
     (:file "macos/kernel/scene-item")
     (:file "macos/kernel/shape")
     (:file "macos/kernel/point-cloud")
     (:file "macos/kernel/polygon")
     (:file "macos/kernel/polyhedron")
     (:file "macos/kernel/group")
     (:file "macos/kernel/procedural")
     (:file "macos/kernel/animator")
     (:file "macos/kernel/scene")
     (:file "macos/kernel/protocol")
     (:file "macos/kernel/main")
     (:file "macos/plugins/parametric-curve")
     (:file "macos/plugins/uv-mesh")
     (:file "macos/plugins/heightfield")
     (:file "macos/plugins/superquadric")
     (:file "macos/plugins/sweep-mesh")
     (:file "macos/plugins/sweep-mesh-group")
     (:file "macos/plugins/dynamics-animator")
     (:file "macos/plugins/procedural-curve-shape")
     (:file "macos/plugins/manager-group")
     (:file "macos/plugins/particle")
     (:file "macos/plugins/l-system")
     (:file "macos/plugins/usd")
     (:file "macos/plugins/obj")
     (:file "macos/minimal-ui")))

#+nil (asdf:load-system :kons-9)

#+nil (trivial-main-thread:call-in-main-thread
       (lambda ()
         (sb-int:set-floating-point-modes :traps nil)
         (kons-9::show-window kons-9::*scene*)))

#+nil (in-package :kons-9)
