(in-package #:kons-9)

;;; dummy code to keep main.lisp happy
(defun update-view (x)
  (declare (ignore x)))

;;;; app-window ================================================================
;; Not being used in glfw3
(defclass app-window ()
  ())

(defparameter *window-x-size* 960)
(defparameter *window-y-size* 540)

;;;; scene-view ================================================================

(defclass scene-view ()
  ((scene :accessor scene :initarg :scene :initform nil)
   (schematic-view :accessor schematic-view :initarg :schematic-view :initform nil); dummy code to keep main.lisp happy
   ;; XXX thread safety???
   (needs-display? :accessor needs-display? :initform nil)))

(defmethod initialize-instance :after ((view scene-view) &rest initargs)
  (declare (ignore initargs))
  (init-view-camera))

;; Hack! Figure out the right analogous representation
;; of a GL-enabled NSView for the GLFW3 backend
(defvar *default-scene-view* nil)


;;; notify scene view that it needs to be redrawn
(defmethod set-needs-redisplay ((view scene-view))
  ;;XXX NOT being used
  )

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
    (draw-ground-plane 10.0 10))
  (when *display-axes?*
    (draw-world-axes 3.0))
  (3d-flush-render)
  (incf *draw-scene-count*))

;;; respond to first click in window
(defmethod accepts-first-mouse ((self scene-view) event)
  (declare (ignore event))
  t)

(defmethod mouse-down ((self scene-view) event)
  (redraw))

;;; accept key events
(defmethod accepts-first-responder ((self scene-view))
   t)

(defmethod mouse-dragged ((self scene-view) event)
  (format t "mouse-dragged NIY...")
  (redraw))

(defun print-scene-view-help ()
  (format t "Mouse drag: orbit, [option] track left/right and up/down, [command] track in/out~%~
`: toggle lighting~%~
1: toggle filled display~%~
2: toggle wireframe display~%~
3: toggle point display~%~
4: toggle backface culling~%~
5: toggle smooth shading~%~
6: toggle ground plane display~%~
7: toggle axes display~%~
z: reset camera~%~
a: init scene~%~
n: clear scene~%~
space: update scene (hold down for animation) ~%~
delete: delete selected items ~%~
tab: show/hide contextual menu ~%~
h or ?: print this help message~%"))

;;; update scene when key pressed
(defmethod key-down ((self scene-view) event)
  (let* (;;(str (objc:lisp-string-from-nsstring (#/charactersIgnoringModifiers event)))
         (str (or event "h"))
         (char (char str 0))
         (scene (scene self)))
    (format t "key-down scene: ~a, event: ~a ~%" scene event)
    (finish-output)
    (case char
      (#\h (print-scene-view-help))
      (#\? (print-scene-view-help))
      (#\a (when scene (init-scene scene)))
      (#\n (dolist (v *scene-views*) (clear-scene (scene v))))
      (#\` (setf *do-lighting?* (not *do-lighting?*)))
      (#\1 (setf *display-filled?* (not *display-filled?*)))
      (#\2 (setf *display-wireframe?* (not *display-wireframe?*)))
      (#\3 (setf *display-points?* (not *display-points?*)))
      (#\4 (setf *do-backface-cull?* (not *do-backface-cull?*)))
      (#\5 (setf *do-smooth-shading?* (not *do-smooth-shading?*)))
      (#\6 (setf *display-ground-plane?* (not *display-ground-plane?*)))
      (#\7 (setf *display-axes?* (not *display-axes?*)))
      (#\z (init-view-camera) (3d-update-light-settings)) ;TODO -- lights don't update when camera reset
      (#\space (dolist (v *scene-views*) (update-scene (scene v))))
      (#\rubout (dolist (v *scene-views*) (remove-current-selection (scene v))))
      ))
  (redraw))

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)
(defparameter *window-size* nil)

;;XXX This doesn't work on wayland. I think wayland expects clients
;; to draw the window decorations themselves
(defun update-window-title (window)
  (glfw:set-window-title (format nil "size ~A | keys ~A | buttons ~A"
                            *window-size*
                            *keys-pressed*
                            *buttons-pressed*)
                    window))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (format t "key-callback: w: ~a, k: ~a, sc: ~a, a: ~a, mk: ~a ~%"
          window key scancode action mod-keys)
  (finish-output)
  (when (and (eq key :escape) (eq action :press))
    (format t "XXX got ESC! Closing...")
    (finish-output)
    (glfw:set-window-should-close))
  (if (eq action :press)
      (progn
        (pushnew key *keys-pressed*)
        (when *default-scene-view*
          (key-down *default-scene-view* (string-downcase (string key)))))
    (alexandria:deletef *keys-pressed* key))
  (update-window-title window))

(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  (format t "mouse-btn-callback: w: ~a, b: ~a, a: ~a, mk: ~a ~%"
          window button action mod-keys)
  (finish-output)
  (if (eq action :press)
    (pushnew button *buttons-pressed*)
    (alexandria:deletef *buttons-pressed* button))
  (update-window-title window))

(defun set-viewport (width height)
  (format t "set-viewport: w: ~a, h: ~a ~%" width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(glfw:def-window-size-callback window-size-callback (window w h)
  (format t "window-size-callback: win: ~a, w: ~a, h: ~a ~%" window w h)
  (finish-output)
  (setf *window-size* (list w h))
  (update-window-title window)
  (set-viewport w h))

(defun show-window (scene)
  (setf *scene-views* '())
  ;; XXX TODO assert that this is running on the main thread.
  ;; Graphics calls on OS X must occur in the main thread
  ;; Normally this is called by run function in kernel/main.lisp

  (handler-bind ((error
                   (lambda (condition)
                     (trivial-backtrace:print-backtrace condition)
                     (return-from show-window))))
      (glfw:with-init-window (:title "glfw3 foo"
                              :width *window-x-size* :height *window-y-size*)
        (let ((scene-view (make-instance 'scene-view :scene scene)))
          (push scene-view *scene-views*)
          ;; Hack! Need to figure out how to tie a scene-view to a window
          ;; in glfw3. For now, just set the first scene-view created
          ;; as default and use that for event handling
          (setf *default-scene-view* scene-view)

          (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
          (glfw:set-key-callback 'key-callback)
          (glfw:set-mouse-button-callback 'mouse-callback)
          (glfw:set-window-size-callback 'window-size-callback)
          (setf *window-size* (glfw:get-window-size))
          (update-window-title glfw:*window*)
          (loop until (glfw:window-should-close-p)
                do (draw-scene-view *default-scene-view*)
                do (glfw:swap-buffers)
                do (glfw:poll-events))))))

(defmacro with-redraw (&body body)
  `(let ((result (progn ,@body)))
     (redraw)
     result))

;;; with-redraw macro: add variant that clears scene
(defmacro with-clear-and-redraw (&body body)
  `(progn
     (clear-scene *scene*)
     (setf (init-done? *scene*) nil)
     (setf (current-frame *scene*) 0)
     (let ((_result (progn ,@body)))
       (redraw)
       _result)))

(defmacro with-grid-clear-and-redraw (&body body)
  `(progn
     (dolist (v *scene-views*)
       (clear-scene (scene v)))
     (let ((_result (progn ,@body)))
       (redraw)
       _result)))
