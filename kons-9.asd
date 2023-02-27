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
   #:cl-glfw3
   #:cl-paths-ttf                       ;for font libraries
   #:zpb-ttf                            ;for font libraries
   #:cl-vectors                         ;for font libraries
   #:origin
   #:clobber) 
  :serial t
  :components
  ((:file "src/package")
   ;; utils & math
   (:file "src/kernel/utils")
   (:file "src/kernel/color")
   (:file "src/kernel/point-origin")
   (:file "src/kernel/matrix")
   (:file "src/kernel/noise")
   ;; graphics
   (:file "src/graphics/opengl/opengl")
   (:file "src/graphics/opengl/ui-widgets")
   ;; kernel
   (:file "src/kernel/item")
   (:file "src/kernel/transform")
   (:file "src/kernel/scene-item")
   (:file "src/kernel/shape")
   (:file "src/kernel/point-cloud")
   (:file "src/kernel/curve")
   (:file "src/kernel/polyhedron")
   (:file "src/kernel/group-mixin")
   (:file "src/kernel/shape-group")
   (:file "src/kernel/procedural")
   (:file "src/kernel/motion")
   (:file "src/kernel/motion-group")
   (:file "src/kernel/animator")
   (:file "src/kernel/animation")
   (:file "src/kernel/interactor")
   (:file "src/kernel/scene")
   (:file "src/kernel/scene-draw")
   (:file "src/kernel/scene-hierarchy")
   (:file "src/kernel/scene-duplicate")
   (:file "src/kernel/protocol")
   (:file "src/kernel/clobber")
   (:file "src/kernel/main")
   ;; font libraries -- tmp until we use 3b-bmfont
   (:module "lib/JMC-font-libs/font-master"
    :components ((:file "glyph")
                 (:file "font")
                 (:file "glyph-doc")
                 (:file "documentation")))
   (:module "lib/JMC-font-libs/font-zpb-ttf-master"
    :depends-on ("lib/JMC-font-libs/font-master") ; #:zpb-ttf "cl-vectors")
    :components ((:file "package")
                 (:file "glyph-zpb-ttf")
                 (:file "font-zpb-ttf")))
   ;; app user interface
   (:file "src/graphics/glfw/command-table")
   (:file "src/graphics/glfw/application-widgets")
   (:file "src/graphics/glfw/glfw-gui")
   (:file "src/graphics/opengl/text-common")
   (:file "src/graphics/opengl/text-opengl-common")
   #+darwin(:file "src/graphics/opengl/opengl2-text")
   #-darwin(:file "src/graphics/opengl/opengl3-text")
   ;; plugins
   (:file "src/plugins/parametric-curve")
   (:file "src/plugins/outline")
   (:file "src/plugins/uv-mesh")
   (:file "src/plugins/heightfield")
   (:file "src/plugins/superquadric")
   (:file "src/plugins/sweep-mesh")
   (:file "src/plugins/sweep-mesh-group")
   (:file "src/plugins/poly-strand")
   (:file "src/plugins/channel")
   (:file "src/plugins/force-field")
   (:file "src/plugins/dynamics-animator")
   (:file "src/plugins/flex-animator")
   (:file "src/plugins/boid-system")
   (:file "src/plugins/procedural-curve")
   (:file "src/plugins/scalar-field")
   (:file "src/plugins/isosurface")
   (:file "src/plugins/sdf")
   (:file "src/plugins/instancer-shape")
   (:file "src/plugins/manager-group")
   (:file "src/plugins/particle")
   (:file "src/plugins/l-system")
   (:file "src/plugins/poly-mesh")
   (:file "src/plugins/usd")
   (:file "src/plugins/obj")
   (:file "src/plugins/stl")
   ))

(asdf:defsystem #:kons-9/testsuite
  :description "Testsuite for Common Lisp 3D Graphics System"
  :author "Kaveh Kardan"
  :license "MIT"
  :depends-on (:alexandria :org.melusina.confidence :kons-9)
  :components
  ((:module "testsuite"
    :components ((:file "package")
		 (:file "assertions")
 		 (:module "kernel"
 		  :components ((:file "utils")
			       (:file "point-cloud")))
 		 (:file "entrypoint")))))

#+nil (asdf:load-system :kons-9)

#+nil (trivial-main-thread:call-in-main-thread
       (lambda ()
         (sb-int:set-floating-point-modes :traps nil)
         (kons-9::show-window kons-9::*scene*)))

#+nil (in-package :kons-9)
