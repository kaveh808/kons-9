;;;; kons-9.asd

(asdf:defsystem #:kons-9
  :description "Common Lisp 3D Graphics System"
  :author "Kaveh Kardan"
  :license "MIT"
  :components
  ((:file "ccl64")
   (:file "package")
   (:file "kernel/utils")
   (:file "kernel/color")
   (:file "kernel/point")
   (:file "kernel/matrix")
   (:file "kernel/noise")
   (:file "kernel/opengl")
   (:file "kernel/transform")
   (:file "kernel/shape")
   (:file "kernel/group")
   (:file "kernel/procedural")
   (:file "kernel/animator")
   (:file "kernel/scene")
   (:file "kernel/main")
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
   (:file "plugins/usd")
   (:file "plugins/obj")
   (:file "ui")
   )
  :serial t)
