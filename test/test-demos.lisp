(in-package #:kons-9)

#|
This file loads all demos to check for issues.

It does not verify that the resulting graphics are correct, but should catch missing
functions and the like.

|#

(let ((demo-files '("demo-kernel"
                    "demo-procedural-curve"
                    "demo-uv-mesh"
                    "demo-superquadric"
                    "demo-heightfield"
                    "demo-particle"
                    "demo-sweep-mesh"
                    "demo-animation"
                    "demo-isosurface"
                    "demo-sdf"
                    "demo-flex-animator"
                    "demo-boid-system"
                    "demo-misc")))
  (dolist (filename demo-files)
    (format t "Testing file ~a...~%" filename)
    (load (asdf:system-relative-pathname "kons-9" (strcat "test/" filename ".lisp"))))
  (format t "Test completed.~%"))
