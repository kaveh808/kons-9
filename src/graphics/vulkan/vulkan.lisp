(defpackage #:kons-9/vulkan
  (:use :common-lisp :alexandria
        :vk :cffi :%vk))
(in-package #:kons-9/vulkan)

;;; Embryonic vulkan graphics backend for Kons-9.

(defclass kons-9-app (vulkan-application-mixin)
  ((device :accessor device)
   (index :accessor queue-family-index)
   (queue :accessor queue)
   (command-pool :accessor command-pool)
   (command-buffers :accessor command-buffers)))

(defun open-window-for-one-second (&optional (app (make-instance 'kons-9-app)))
  (let* ((device (default-logical-device app))
         (main-window (main-window app))
         (index (queue-family-index (render-surface main-window)))
         (queue (find-queue device index))
         (command-pool (find-command-pool device index))
         (command-buffer (elt (command-buffers command-pool) 0)))
    (device-wait-idle device)
    (reset-command-pool device command-pool)
    (begin-command-buffer command-buffer)
    ;; "one time commands here"
    (end-command-buffer command-buffer)
    (queue-submit1 queue command-buffer)
    (device-wait-idle device)
    (sleep 1)
    (shutdown-application app)
    ))
