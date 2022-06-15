
(in-package "CL-USER")

(require "COCOA")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (objc:load-framework "OpenGL" :gl))

(load "~/Development/kons-9/utils.lisp")
(load "~/Development/kons-9/opengl.lisp")
(load "~/Development/kons-9/transform.lisp")
(load "~/Development/kons-9/shape.lisp")
(load "~/Development/kons-9/procedural.lisp")
(load "~/Development/kons-9/uv-mesh.lisp")
(load "~/Development/kons-9/animator.lisp")
(load "~/Development/kons-9/particle.lisp")
(load "~/Development/kons-9/scene.lisp")
(load "~/Development/kons-9/usd.lisp")
(load "~/Development/kons-9/obj.lisp")


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
