(in-package #:kons-9)

;;; dummy code to keep main.lisp happy
(defun update-view (x)
  (declare (ignore x)))

(defparameter *window-x-size* 960)
(defparameter *window-y-size* 540)

;;;; scene-view ================================================================

(defclass scene-view ()
  ((scene :accessor scene :initarg :scene :initform nil)
   (command-tables :accessor command-tables :initarg :command-tables :initform '())))

(defmethod initialize-instance :after ((view scene-view) &rest initargs)
  (declare (ignore initargs))
  (init-view-camera)
  (push (make-default-command-table view) (command-tables view)))

(defun make-default-command-table (view)
  (let ((scene (scene view))
        (table (make-instance 'command-table
                              :mouse-help-string "Drag: orbit, [option/alt] track left/right and up/down, [control] track in/out.")))
    (add-entry table
               :h
               (lambda () (print-command-table-help table))
               "Print this help message.")
    (add-entry table
               :a
               (lambda () (when scene (init-scene scene)))
               "Init scene.")
    (add-entry table
               :n
               (lambda () (clear-scene scene))
               "Clear scene.")
    (add-entry table
               :grave-accent (lambda () (setf *do-lighting?* (not *do-lighting?*)))
               "Toggle lighting.")
    (add-entry table
               :1
               (lambda () (setf *display-filled?* (not *display-filled?*)))
               "Toggle filled display.")
    (add-entry table
               :2
               (lambda () (setf *display-wireframe?* (not *display-wireframe?*)))
               "Toggle wireframe display.")
    (add-entry table
               :3
               (lambda () (setf *display-points?* (not *display-points?*)))
               "Toggle point display.")
    (add-entry table
               :4
               (lambda () (setf *do-backface-cull?* (not *do-backface-cull?*)))
               "Toggle backface culling.")
    (add-entry table
               :5
               (lambda () (setf *do-smooth-shading?* (not *do-smooth-shading?*)))
               "Toggle smooth shading.")
    (add-entry table
               :6
               (lambda () (setf *display-ground-plane?* (not *display-ground-plane?*)))
               "Toggle ground plane display.")
    (add-entry table
               :7
               (lambda () (setf *display-axes?* (not *display-axes?*)))
               "Toggle axes display.")
    (add-entry table                    ;TODO -- lights don't update when camera reset
               :z
               (lambda () (init-view-camera) (3d-update-light-settings))
               "Reset camera.")
    (add-entry table
               :space
               (lambda () (update-scene scene))
               "Update scene (hold down for animation).")
    (add-entry table
               :backspace
               (lambda () (remove-current-selection scene))
               "Delete selected items.")
    table))

;; Hack! Figure out the right analogous representation
;; of a GL-enabled NSView for the GLFW3 backend
(defvar *default-scene-view* nil)

(defvar *draw-scene-count* 0)

;;; display the view
(defmethod draw-scene-view ((view scene-view))
  (3d-setup-buffer)
  (3d-update-light-settings)
  (3d-setup-projection)
  (when (scene view)
    (draw (scene view)))
  (3d-cleanup-render)
  (when *display-ground-plane?*
    (draw-ground-plane))
  (when *display-axes?*
    (draw-world-axes))
  (3d-flush-render)
  (incf *draw-scene-count*))

;;; respond to first click in window
(defmethod accepts-first-mouse ((self scene-view) event)
  (declare (ignore event))
  t)

(defmethod mouse-down ((self scene-view) event)
  )

;;; accept key events
(defmethod accepts-first-responder ((self scene-view))
  t)

(defmethod key-down ((self scene-view) key)
  ;; (format t "key-down self: ~a, key: ~a~%" self key)
  ;; (finish-output)
  (do-command (car (command-tables self)) key))

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)
(defparameter *window-size* nil)

;;XXX This doesn't work on wayland. I think wayland expects clients
;; to draw the window decorations themselves
(defun update-window-title (window)
  (glfw:set-window-title (format nil "kons-9 | window ~A | start-frame ~A | end-frame ~A | curr-frame ~A"
                                 *window-size*
                                 (start-frame (scene *default-scene-view*))
                                 (end-frame (scene *default-scene-view*))
                                 (current-frame (scene *default-scene-view*)))
                         window))

;; (defun update-window-title (window)
;;   (glfw:set-window-title (format nil "size ~A | keys ~A | buttons ~A | frame ~A"
;;                                  *window-size*
;;                                  *keys-pressed*
;;                                  *buttons-pressed*
;;                                  (current-frame (scene *default-scene-view*)))
;;                          window))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  ;; (format t "key-callback: w: ~a, k: ~a, sc: ~a, a: ~a, mk: ~a ~%"
  ;;         window key scancode action mod-keys)
  ;; (finish-output)
  (if (and (eq key :escape) (eq action :press))
      (progn
        ;; (format t "XXX got ESC! Closing...")
        ;; (finish-output)
        (glfw:set-window-should-close))
      (progn
        (cond ((eq action :press)
               (pushnew key *keys-pressed*)
               (when *default-scene-view*
                 (key-down *default-scene-view* key)))
              ((eq action :repeat)
               (when *default-scene-view*
                 (key-down *default-scene-view* key)))
              (t (alexandria:deletef *keys-pressed* key)))
        (update-window-title window))))

(defparameter *current-mouse-pos-x* 0)
(defparameter *current-mouse-pos-y* 0)
(defparameter *current-mouse-modifier* nil)

(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  ;; (format t "mouse-btn-callback: w: ~a, b: ~a, a: ~a, mk: ~a ~%"
  ;;         window button action mod-keys)
  ;; (finish-output)
  (if (eq action :press)
      (let ((pos (glfw:get-cursor-position window)))
        (pushnew button *buttons-pressed*)
        (setf *current-mouse-pos-x* (first pos))
        (setf *current-mouse-pos-y* (second pos))
        (setf *current-mouse-modifier* (and mod-keys (car mod-keys)))
        ;;        (format t "POS: ~a ~a~%" *current-mouse-pos-x* *current-mouse-pos-y*)
        )
      (alexandria:deletef *buttons-pressed* button))
  (update-window-title window))

(glfw:def-cursor-pos-callback cursor-position-callback (window x y)
  (mouse-dragged window x y)
  )

(defun mouse-dragged (window x y)
  (let ((action (glfw:get-mouse-button :left window)))
    (when (eq action :press)
      (let ((dx (- x *current-mouse-pos-x*))
            (dy (- y *current-mouse-pos-y*)))
        (setf *current-mouse-pos-x* x)
        (setf *current-mouse-pos-y* y)
        ;; (format t "mouse-dragged dx: ~a, dy: ~a, mod: ~a~%" dx dy *current-mouse-modifier*)
        (cond ((eq :alt *current-mouse-modifier*)
               (if (>= (abs dx) (abs dy))
                   (incf *cam-side-dist* (* 0.1 dx))
                   (incf *cam-up-dist* (* -0.1 dy))))
              ((eq :control *current-mouse-modifier*)
               (incf *cam-fwd-dist* (* 0.1 dx)))
              (t
               (incf *cam-x-rot* dy)
               (incf *cam-y-rot* dx)))))))

(glfw:def-window-size-callback window-size-callback (window w h)
  ;; (format t "window-size-callback: win: ~a, w: ~a, h: ~a ~%" window w h)
  ;; (finish-output)
  (setf *window-size* (list w h))
  (setf *viewport-aspect-ratio* (/ (first *window-size*) (second *window-size*)))
  ;; redraw while being resized
  #-:darwin(gl:viewport 0 0 w h)
  (draw-scene-view *default-scene-view*)
  (glfw:swap-buffers)
  (update-window-title window))

(defun show-window (scene)
  ;; XXX TODO assert that this is running on the main thread.
  ;; Graphics calls on OS X must occur in the main thread
  ;; Normally this is called by run function in kernel/main.lisp

  (handler-bind ((error
                  (lambda (condition)
                    (trivial-backtrace:print-backtrace condition)
                    (return-from show-window))))
    (sb-int:with-float-traps-masked
        (:invalid
         :inexact
         :overflow
         :underflow
         :divide-by-zero)
      (glfw:with-init-window (:title "glfw3 foo" :width *window-x-size* :height *window-y-size*)
         (let ((scene-view (make-instance 'scene-view :scene scene)))

           ;; Hack! Need to figure out how to tie a scene-view to a window
           ;; in glfw3. For now, just set the first scene-view created
           ;; as default and use that for event handling
           (setf *default-scene-view* scene-view)

           (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
           (glfw:set-key-callback 'key-callback)
           (glfw:set-mouse-button-callback 'mouse-callback)
           (glfw:set-cursor-position-callback 'cursor-position-callback)
           (glfw:set-window-size-callback 'window-size-callback)
           (setf *window-size* (glfw:get-window-size))
           (setf *viewport-aspect-ratio* (/ (first *window-size*) (second *window-size*)))
           (update-window-title glfw:*window*)
           (loop until (glfw:window-should-close-p)
                 do (draw-scene-view *default-scene-view*)
                 do (glfw:swap-buffers)
                 do (glfw:poll-events)))))))

;;; no longer necessary
(defmacro with-redraw (&body body)
  `(let ((result (progn ,@body)))
     result))

(defmacro with-clear-scene (&body body)
  `(progn
     (clear-scene *scene*)
     ,@body))
