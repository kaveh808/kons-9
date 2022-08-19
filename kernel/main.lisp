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
(defparameter *window* nil)
(defparameter *scene-views* '())

(defun run ()
  ;; (setf *window* (on-main-thread (show-window *scene*)))
  (on-main-thread (show-window *scene*)))

;;(defun run-grid (n)
;;  (setf *window* (on-main-thread (show-grid-window n))))

(defun redraw ()
  (dolist (v *scene-views*)
    ;; (#/setNeedsDisplay: v t)
    (set-needs-redisplay v)
    (when (schematic-view v)
      (update-view (schematic-view v)))))
