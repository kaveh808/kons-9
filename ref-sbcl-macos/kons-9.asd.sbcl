;;;; kons-9.asd

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
     #:cl-glfw3
     )
    :components
    ((:file "package")
     (:file "kernel/utils")
     (:file "kernel/color")
     (:file "kernel/point")
     (:file "kernel/matrix")
     (:file "kernel/noise")
     (:file "kernel/portable-opengl")
     (:file "kernel/transform")
     (:file "kernel/scene-item")
     (:file "kernel/shape")
     (:file "kernel/point-cloud")
     (:file "kernel/polygon")
     (:file "kernel/polyhedron")
     (:file "kernel/group")
     (:file "kernel/procedural")
     (:file "kernel/animator")
     (:file "kernel/scene")
     (:file "kernel/protocol")
     (:file "kernel/main")
     (:file "plugins/parametric-curve")
     (:file "plugins/uv-mesh")
     (:file "plugins/heightfield")
     (:file "plugins/superquadric")
     (:file "plugins/sweep-mesh")
     (:file "plugins/sweep-mesh-group")
     (:file "plugins/dynamics-animator")
     (:file "plugins/procedural-curve-shape")
     (:file "plugins/manager-group")
     (:file "plugins/particle")
     (:file "plugins/l-system")
     ;; (:file "plugins/he-mesh")
     (:file "plugins/usd")
     (:file "plugins/obj")
     ;; (:file "ui")
     ;; (:file "minimal-ui")
     (:file "minimal-glfw3-ui"))
    :serial t)

  
#+nil (asdf:load-system :kons-9)
#+nil (trivial-main-thread:call-in-main-thread
       (lambda ()
         (sb-int:set-floating-point-modes :traps nil)
         (kons-9::show-window kons-9::*scene*)))
