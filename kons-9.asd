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
  :components ((:file "common/package")
               (:file "common/kernel/utils")
               (:file "common/kernel/color")
               (:file "common/kernel/point")
               (:file "common/kernel/matrix")
               (:file "common/kernel/noise")
               #+ccl(:file "ccl/kernel/opengl")
               #-ccl (:file "common/kernel/portable-opengl")
               (:file "common/kernel/transform")
               (:file "common/kernel/scene-item")
               (:file "common/kernel/shape")
               (:file "common/kernel/point-cloud")
               (:file "common/kernel/polygon")
               #+ccl (:file "ccl/kernel/polyhedron")
               #+sbcl (:file "sbcl/kernel/polyhedron")
               (:file "common/kernel/group")
               ))

#+nil (asdf:load-system :kons-9)
