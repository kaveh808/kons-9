(in-package #:kons-9)

;;; execute code on main thread -- necessary for interacting with UI elements
(defmacro on-main-thread (&rest actions)
  #+ccl`(ccl::call-in-event-process
     #'(lambda ()
         ,@actions))
  #-ccl`(tmt:call-in-main-thread
        #'(lambda ()
            ,@actions)))

;;;; run graphics =======================================================

(defparameter *scene* (make-instance 'scene))

(defun run ()
  ;; (setf *window* (on-main-thread (show-window *scene*)))
  (on-main-thread (show-window *scene*)))
