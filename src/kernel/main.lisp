(in-package #:kons-9)

;;;; run graphics ==============================================================

(defparameter *scene* (make-instance 'scene))

;;; execute code on main thread -- necessary for interacting with UI elements
(defun run (&optional (command-table nil))
  (trivial-main-thread:call-in-main-thread
   (lambda ()
     (sb-int:set-floating-point-modes :traps nil)
     (kons-9::show-window kons-9::*scene* command-table))))

