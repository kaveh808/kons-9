;;;; kons-9.asd

(asdf:defsystem #:kons-9
  :description "Common Lisp 3D graphics code"
  :author "Kaveh Kardan"
  :license "MIT"
  :components
  ((:file "ccl64")
   (:file "package")
   (:file "utils")
   (:file "opengl")
   (:file "transform")
   (:file "shape")
   (:file "procedural")
   (:file "uv-mesh")
   (:file "animator")
   (:file "particle")
   (:file "l-system")
   (:file "scene")
   (:file "usd")
   (:file "obj")
   (:file "main"))
  :serial t)
