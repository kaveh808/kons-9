;;;; kons-9.asd
;;;; beginning merge of sbcl port --mle 2022-08-19

(asdf:defsystem #:kons-9
  :description "Common Lisp 3D Graphics System"
  :author "Kaveh Kardan"
  :license "MIT"
;  :depends-on
;  (#:cffi
;   #:cl-opengl
;   #:cl-glu)
  :components
  ((:file "ccl64")
   (:file "package")
   (:file "kernel/utils")
   (:file "kernel/color")
   (:file "kernel/point")
   (:file "kernel/matrix")
   (:file "kernel/noise")
;   (:file "kernel/portable-opengl")
   (:file "kernel/opengl")
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
   (:file "kernel/scene-draw")
   (:file "kernel/scene-generics")
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
;   (:file "plugins/particle")
;   (:file "plugins/l-system")
;   (:file "plugins/he-mesh")
   (:file "plugins/usd")
   (:file "plugins/obj")
;   (:file "ui")
   (:file "minimal-ui")
   )
  :serial t)


