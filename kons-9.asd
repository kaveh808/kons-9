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
    :components ((:module "common"
                  :serial t
                  :components ((:file "package")))
                 #+ccl
                 (:module "ccl"
                  :serial t
                  :components ())
                 #+sbcl
                 (:module "sbcl"
                  :serial t
                  :components ())))

#+nil (asdf:load-system :kons-9)
