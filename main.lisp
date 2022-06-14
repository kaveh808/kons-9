
(in-package "CL-USER")

(require "COCOA")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (objc:load-framework "OpenGL" :gl))

(load "~/Development/3D DCC Project/src/utils.lisp")
(load "~/Development/3D DCC Project/src/opengl.lisp")
(load "~/Development/3D DCC Project/src/transform.lisp")
(load "~/Development/3D DCC Project/src/shape.lisp")
(load "~/Development/3D DCC Project/src/procedural.lisp")
(load "~/Development/3D DCC Project/src/uv-mesh.lisp")
(load "~/Development/3D DCC Project/src/animator.lisp")
(load "~/Development/3D DCC Project/src/particle.lisp")
(load "~/Development/3D DCC Project/src/scene.lisp")
(load "~/Development/3D DCC Project/src/usd.lisp")
(load "~/Development/3D DCC Project/src/obj.lisp")


;;; execute code on main thread -- necessary for interacting with UI elements
(defmacro on-main-thread (&rest actions)
  `(ccl::call-in-event-process
     #'(lambda ()
         ,@actions)))

;;;; run graphics =======================================================

(defparameter *scene* (make-instance 'scene))
(defparameter *window* nil)
(defparameter *scene-views* '())

(defun run ()
  (setf *window* (on-main-thread (show-window *scene*))))

(defun run-grid (n)
  (setf *window* (on-main-thread (show-grid-window n))))

(defun redraw ()
  (dolist (v *scene-views*)
    (#/setNeedsDisplay: v t)))

(run)
;;;(run-grid 3)
