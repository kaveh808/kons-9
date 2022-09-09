(in-package #:kons-9)

(defparameter *window-size* '(960 540))
(defparameter *current-mouse-pos-x* 0)
(defparameter *current-mouse-pos-y* 0)
(defparameter *current-mouse-modifier* nil)
;; Hack! Figure out the right analogous representation
;; of a GL-enabled NSView for the GLFW3 backend
(defvar *default-scene-view* nil)

(defparameter *current-highlighted-ui-item* nil)

;;;; scene-view ================================================================

(defclass scene-view ()
  ((scene :accessor scene :initarg :scene :initform nil)
   (menu :accessor menu :initarg :menu :initform nil)
   (command-tables :accessor command-tables :initarg :command-tables :initform '())))

(defmethod initialize-instance :after ((view scene-view) &rest initargs)
  (declare (ignore initargs))
  (init-view-camera)
  (push (default-command-table view) (command-tables view)))

(defun default-command-table (view)
  (let ((scene (scene view))
        (table (make-instance 'command-table
                              :title "Default"
                              :mouse-help-string "Drag: orbit, [option/alt] track left/right and up/down, [control] track in/out.")))
    (ct-entry :a "Initialize scene" (when scene (init-scene scene)))
    (ct-entry :n "Clear scene" (when scene (clear-scene scene)))
    (ct-entry :grave-accent "Toggle lighting" (setf *do-lighting?* (not *do-lighting?*)))
    (ct-entry :1 "Toggle filled display" (setf *display-filled?* (not *display-filled?*)))
    (ct-entry :2 "Toggle wireframe display" (setf *display-wireframe?* (not *display-wireframe?*)))
    (ct-entry :3 "Toggle point display" (setf *display-points?* (not *display-points?*)))
    (ct-entry :4 "Toggle backface culling" (setf *do-backface-cull?* (not *do-backface-cull?*)))
    (ct-entry :5 "Toggle smooth shading" (setf *do-smooth-shading?* (not *do-smooth-shading?*)))
    (ct-entry :6 "Toggle ground plane display" (setf *display-ground-plane?* (not *display-ground-plane?*)))
    (ct-entry :7 "Toggle world axes display" (setf *display-axes?* (not *display-axes?*)))
    (ct-entry :z "Reset camera" (init-view-camera) (3d-update-light-settings))
    (ct-entry :space "Update scene (hold down for animation)" (update-scene scene))
    (ct-entry :backspace "Delete selected items" (remove-current-selection scene))
    table))

;;; display the view
(defmethod draw-scene-view ((view scene-view))
;  (text-engine-begin-frame)
  
  (3d-setup-buffer)
  (3d-setup-projection)
  (3d-update-light-settings)
  (when (scene view)
    (draw (scene view)))
  (3d-cleanup-render)
  (when *display-axes?*
    (draw-world-axes))
  (when *display-ground-plane?*
    (draw-ground-plane))

  ;; display ui layer
  (2d-setup-projection)
  (draw-scene-view-ui view)

;  (test-text)
;  (text-engine-end-frame)
  
  (3d-flush-render))

(defmethod draw-scene-view-ui ((view scene-view))
  (when (command-tables view)
    (let ((table (car (command-tables view))))
      ;; test to avoid rebuilding menu every frame -- kinda kludgy
      (when (or (null (menu view))
                (not (eq (command-table (menu view)) table))
                (not (= (length (children (menu view))) (length (entries table)))))
        (setf (menu view)
              (make-instance 'ui-popup-menu :ui-x 20 :ui-y 20 :command-table (car (command-tables view))))
        (update-layout (menu view)))))
  (when (menu view)
    (draw-view (menu view))))

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
  (if (eq :tab key)
      (setf (command-tables self) (last (command-tables self))) ;pop all but original table
      (do-command (car (command-tables self)) key)))

(defmethod key-up ((self scene-view) key)
  )

;;XXX This doesn't work on wayland. I think wayland expects clients
;; to draw the window decorations themselves
(defun update-window-title (window)
  (glfw:set-window-title
   (if (= 1 (length (command-tables *default-scene-view*)))
       (format nil "kons-9 | window ~A | start-frame ~A | end-frame ~A | curr-frame ~A"
               *window-size*
               (start-frame (scene *default-scene-view*))
               (end-frame (scene *default-scene-view*))
               (current-frame (scene *default-scene-view*)))
       (format nil "kons-9 | ~A [tab to reset]"
               (apply #'strcat (mapcar (lambda (table) (strcat (title table) " > "))
                                       (reverse (butlast (command-tables *default-scene-view*)))))))
       window))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  ;; (format t "key-callback: w: ~a, k: ~a, sc: ~a, a: ~a, mk: ~a ~%"
  ;;         window key scancode action mod-keys)
  ;; (finish-output)
  (cond ((and (eq key :escape) (eq action :press))
         (glfw:set-window-should-close))
        ((or (eq action :press) (eq action :repeat))
         (when *default-scene-view*
           (key-down *default-scene-view* key)))
        ((eq action :release)
         (when *default-scene-view*
           (key-up *default-scene-view* key))))
  (update-window-title window))

(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  ;; (format t "mouse-btn-callback: w: ~a, b: ~a, a: ~a, mk: ~a ~%"
  ;;         window button action mod-keys)
  ;; (finish-output)
  (let ((pos (glfw:get-cursor-position window)))
    (setf *current-mouse-pos-x* (first pos))
    (setf *current-mouse-pos-y* (second pos))
    (setf *current-mouse-modifier* (and mod-keys (car mod-keys)))
    (cond ((eq action :press)
           (mouse-click (first pos) (second pos) button mod-keys)))))

(glfw:def-cursor-pos-callback cursor-position-callback (window x y)
;;  (format t "mouse x: ~a, y: ~a~%" x y)
  (let ((dx (- x *current-mouse-pos-x*))
        (dy (- y *current-mouse-pos-y*)))
    (setf *current-mouse-pos-x* x)
    (setf *current-mouse-pos-y* y)
    (let ((action (glfw:get-mouse-button :left window)))
      (cond ((eq action :press)
             (mouse-dragged x y dx dy))
            (t
             (mouse-moved x y dx dy))))))

(defun mouse-moved (x y dx dy)
  (declare (ignore dx dy))
  ;;  (format t "mouse-moved x: ~a, y: ~a~%" x y)
  (when *current-highlighted-ui-item*
    (setf (highlight? *current-highlighted-ui-item*) nil))
  (let ((ui-item (find-ui-at-point (menu *default-scene-view*) x y)))
    (when (and ui-item (eq 'ui-menu-item (type-of ui-item)))
      (setf (highlight? ui-item) t)
      (when (not (eq ui-item *current-highlighted-ui-item*)) ;new highlighted item
        (setf *current-highlighted-ui-item* ui-item)
        (print (text ui-item)))))
  )

(defun mouse-click (x y button modifiers)
  (declare (ignore button modifiers))
  (let ((ui-item (find-ui-at-point (menu *default-scene-view*) x y)))
    (when (and ui-item (subtypep (type-of ui-item) 'ui-button-item))
      (do-action ui-item))))

(defun mouse-dragged (x y dx dy)
  (declare (ignore x y))
;;  (format t "mouse-dragged dx: ~a, dy: ~a, mod: ~a~%" dx dy *current-mouse-modifier*)
  (cond ((eq :alt *current-mouse-modifier*)
         (if (>= (abs dx) (abs dy))
             (incf *cam-side-dist* (* 0.1 dx))
             (incf *cam-up-dist* (* -0.1 dy))))
        ((eq :control *current-mouse-modifier*)
         (incf *cam-fwd-dist* (* 0.1 dx)))
        (t
         (incf *cam-x-rot* dy)
         (incf *cam-y-rot* dx))))

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
      (glfw:with-init-window (:title "kons-9" :width (first *window-size*) :height (second *window-size*))
         (let ((scene-view (make-instance 'scene-view :scene scene)))

           ;; Hack! Need to figure out how to tie a scene-view to a window
           ;; in glfw3. For now, just set the first scene-view created
           ;; as default and use that for event handling
           (setf *default-scene-view* scene-view)

           ;; assume monitor scale is same in x and y, just use first value
           ;; also assume we are running on the "primary" monitor
           (setf (monitor-scale *drawing-settings*)
                 (first (glfw:get-monitor-content-scale (glfw:get-primary-monitor))))
           (set-lines-thin)

           (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
           (glfw:set-key-callback 'key-callback)
           (glfw:set-mouse-button-callback 'mouse-callback)
           (glfw:set-cursor-position-callback 'cursor-position-callback)
           (glfw:set-window-size-callback 'window-size-callback)
           (setf *window-size* (glfw:get-window-size))
           (setf *viewport-aspect-ratio* (/ (first *window-size*) (second *window-size*)))
           (update-window-title glfw:*window*)
;	   (initial-text-engine-setup)
           (loop until (glfw:window-should-close-p)
	      do (draw-scene-view *default-scene-view*)
                 do (glfw:swap-buffers)
                 do (glfw:poll-events)))))))

(defmacro with-clear-scene (&body body)
  `(progn
     (clear-scene *scene*)
     ,@body))

