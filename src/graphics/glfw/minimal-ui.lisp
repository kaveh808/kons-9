(in-package #:kons-9)

(defparameter *window-size* '(960 540))
(defparameter *current-mouse-pos-x* 0)
(defparameter *current-mouse-pos-y* 0)
(defparameter *current-mouse-modifier* nil)
;; Hack! Figure out the right analogous representation
;; of a GL-enabled NSView for the GLFW3 backend
(defvar *default-scene-view* nil)

(defparameter *current-highlighted-ui-item* nil)

#|
;;;; app-view ================================================================

(defclass-kons-9 app-view (ui-group)
  ((scene-view (make-instance 'scene-view))
   (outliner-view (make-instance 'ui-view))
   (inspector-view (make-instance 'ui-inspector :obj *scene*))
   (timeline-view (make-instance 'ui-button-item))))

(defmethod initialize-instance :after ((view app-view) &rest initargs)
  (declare (ignore initargs))
  (update-layout view)
  (ui-add-child view (scene-view view))
  (ui-add-child view (outliner-view view))
  (ui-add-child view (inspector-view view))
  (ui-add-child view (timeline-view view)))

(defmethod update-layout ((view app-view))
  (with-accessors ((w ui-w) (h ui-h))
      view
    (let ((x0 (* w .25))
          (y0 (* h .25))
          (x1 (* w .75))
          (y1 (* h .75))
          (x2 (* h .50)))
      (ui-set-rect (outliner-view view) 0 0 x0 h)
      (ui-set-rect (inspector-view view) x1 0 x0 h)
      (ui-set-rect (scene-view view) x0 0 x2 y1)
      (ui-set-rect (timeline-view view) x0 y1 x2 y0)))
  view)
|#

;;;; user input ================================================================
;;;; temporary until text box ui working

(defun get-user-string-input (prompt)
  (format t "~%~a: " prompt)
  (finish-output t)
  (read-line t))

;;;; scene-view ================================================================

(defclass-kons-9 scene-view ()
  ((scene nil)
   (status-bar nil)
   (ui-contents '())
   (menu nil)
   (command-tables '())))

(defmethod initialize-instance :after ((view scene-view) &rest initargs)
  (declare (ignore initargs))
  (init-view-camera)
  (setf (status-bar view) (make-status-bar))
  (push (scene-command-table view) (command-tables view)))

(defun show-ui-content (view)
  (setf (ui-contents *default-scene-view*) (list view))
  (update-ui-content-position view))

(defun update-ui-content-position (view)
  ;; (case location
  ;;   (:top-left
  ;;    (ui-set-position view 20 20))
  ;;   (:top-right
     (ui-set-position view (- (first *window-size*) (ui-w view) 20) 20))
    ;; (:bottom-left
    ;;  (ui-set-position view 20 (- (second *window-size*) (ui-h view) 20)))
    ;; (:bottom-right
    ;;  (ui-set-position view
    ;;                   (- (first *window-size*) (ui-w view) 20)
    ;;                   (- (second *window-size*) (ui-h view) 20)))))

(defun scene-command-table (view)
  (let ((table (make-instance 'command-table :title "Scene")))
    (ct-entry :n "New scene" (when (scene view) (clear-scene (scene view))))
    (ct-entry :o "Open scene" (hide-menu view) (show-open-scene-dialog))
    (ct-entry :s "Save scene" (hide-menu view) (show-save-scene-dialog))
    (ct-entry :a "Initialize scene" (when (scene view) (init-scene (scene view))))
    (ct-entry :z "Reset camera" (init-view-camera) (3d-update-light-settings))
;    (ct-entry :l "Select shape" (hide-menu view) (show-ui-select-shape-hierarchy (scene view)))
;;    (ct-entry :v "View Selection" ... TODO -- TBD
    (ct-entry :i "Inspector" (show-ui-content (make-ui-inspector (or (selection (scene view))
                                                                     (scene view)))))
    (ct-entry :p "Shape hierarchy" (show-ui-content (make-ui-outliner-viewer "Shapes" (scene view) #'shapes)))
    (ct-entry :m "Motion hierarchy" (show-ui-content (make-ui-outliner-viewer "Motions" (scene view) #'motions)))
    (ct-entry :backspace "Delete selection" (remove-current-selection (scene view)))
    (ct-subtable :d "Display" (display-command-table))
    (ct-subtable :c "Create" (create-command-table))
    (ct-entry :q "Quit scene" (glfw:set-window-should-close))
;    (ct-entry :space "Update scene (hold down for animation)" (update-scene (scene view)))
    table))

(defun display-command-table ()
  (let ((table (make-instance 'command-table :title "Display")))
    (ct-entry :grave-accent "Toggle lighting" (setf *do-lighting?* (not *do-lighting?*)))
    (ct-entry :1 "Toggle filled display" (setf *display-filled?* (not *display-filled?*)))
    (ct-entry :2 "Toggle wireframe display" (setf *display-wireframe?* (not *display-wireframe?*)))
    (ct-entry :3 "Toggle point display" (setf *display-points?* (not *display-points?*)))
    (ct-entry :4 "Toggle backface culling" (setf *do-backface-cull?* (not *do-backface-cull?*)))
    (ct-entry :5 "Toggle smooth shading" (setf *do-smooth-shading?* (not *do-smooth-shading?*)))
    (ct-entry :6 "Toggle ground plane display" (setf *display-ground-plane?* (not *display-ground-plane?*)))
    (ct-entry :7 "Toggle world axes display" (setf *display-axes?* (not *display-axes?*)))
    (ct-entry :8 "Set bright theme" (set-theme-bright))
    (ct-entry :9 "Set dark theme" (set-theme-dark))
    table))

(defun create-command-table ()
  (let ((table (make-instance `command-table :title "Create")))
    (ct-subtable :c "Create Curve" (curve-command-table))
    (ct-subtable :p "Create Polyhedron" (polyhedron-command-table))
    table))

(defun curve-command-table ()
  (let ((table (make-instance `command-table :title "Create Curve")))
    (ct-make-shape :l "line curve" (make-line-curve (p! 0 0 0) (p! 2 2 2) 4))
    (ct-make-shape :r "rectangle curve" (make-rectangle-curve 2 1 4))
    (ct-make-shape :s "square curve" (make-square-curve 1.5))
    (ct-make-shape :c "circle curve" (make-circle-curve 2.0 16))
    (ct-make-shape :a "arc curve" (make-arc-curve 2.0 0 90 8))
    (ct-make-shape :n "sine curve" (make-sine-curve-curve 360 1 2 1 16))
    (ct-make-shape :p "spiral curve" (make-spiral-curve .2 2.0 -1.0 4 64))
    table))

(defun polyhedron-command-table ()
  (let ((table (make-instance `command-table :title "Create Polyhedron")))
    (ct-make-shape :t "tetrahedron" (make-tetrahedron 2.0))
    (ct-make-shape :c "cube" (make-cube 2.0))
    (ct-make-shape :o "octahedron" (make-octahedron 2.0))
    (ct-make-shape :d "dodecahedron" (make-dodecahedron 2.0))
    (ct-make-shape :i "icosahedron" (make-icosahedron 2.0))
    (ct-make-shape :s "sphere" (make-cube-sphere 4.0 3))
    table))

(defmethod draw-scene-view ((view scene-view))
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

  (2d-setup-projection (first *window-size*) (second *window-size*))

  (progn
    (text-engine-begin-frame)
    (draw-scene-view-ui view)
;    (test-text)
    (text-engine-end-frame)
    )

  (3d-flush-render))

(defmethod draw-scene-view-ui ((view scene-view))
  (draw-view (status-bar view))
  (let ((menu (menu view)))
    (when (and menu (is-visible? menu))
      (when (not (eq (command-table menu) (car (command-tables view))))
        (make-popup-menu view)
        (setf (is-visible? (menu view)) t))
      (draw-view (menu view))))
  (dolist (ui-item (ui-contents view))
    (draw-view ui-item)))

(defmethod make-popup-menu ((view scene-view))
  (when (command-tables view)
    (setf (menu view)
          (make-instance 'ui-popup-menu :ui-x 20 :ui-y 20 :command-table (car (command-tables view))))
    (create-contents (menu view))))

;;; respond to first click in window
(defmethod accepts-first-mouse ((self scene-view) event)
  (declare (ignore event))
  t)

(defmethod mouse-down ((self scene-view) event)
  ;; TODO -- do 3D selection
  )

;;; accept key events
;; (defmethod accepts-first-responder ((self scene-view))
;;   t)

(defmethod show-menu ((self scene-view))
  (setf (menu self) (make-popup-menu self))
  (setf (is-visible? (menu self)) t))

(defmethod hide-menu ((self scene-view))
  (setf (menu self) nil)
  (setf (command-tables self) (last (command-tables self)))) ;pop all but original table

(defmethod key-down ((self scene-view) key mod-keys)
  ;; (format t "key-down self: ~a, key: ~a mod-keys: ~a~%" self key mod-keys)
  ;; (finish-output)
  (cond ((eq :space key)
         (update-scene (scene self)))
        ((eq :tab key)
         (cond ((and (menu self) (is-visible? (menu self)))
                (hide-menu self))
               (t
                (show-menu self))))
        ((eq :escape key)
         (cond ((ui-contents self)
                (setf (ui-contents self) nil))))
        (*ui-keyboard-focus*
         (cond ((and (eq :v key) (member :super mod-keys))
                (do-paste-input *ui-keyboard-focus* (glfw:get-clipboard-string)))
               ((and (eq :c key) (member :super mod-keys))
                (glfw:set-clipboard-string (do-copy-input *ui-keyboard-focus*)))
               ((and (eq :x key) (member :super mod-keys))
                (glfw:set-clipboard-string (do-cut-input *ui-keyboard-focus*)))
               ((eq :backspace key)
                (do-backspace-input *ui-keyboard-focus*))
               ))
;;;(and *current-highlighted-ui-item* (can-have-keyboard-focus? *current-highlighted-ui-item*))
;;;         (do-key-input *ui-keyboard-focus* key mod-keys))
        ((eq :left key)
         (when (and (menu self) (is-visible? (menu self)) (> (length (command-tables self)) 1))
           (setf (command-tables self) (cdr (command-tables self)))))
        ((and (menu self) (is-visible? (menu self)))
         (do-command (car (command-tables self)) key)
         (update-scene-ui))))

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
       (format nil "kons-9 | ~A [tab to reset, left arrow to go back]"
               (apply #'strcat (mapcar (lambda (table) (strcat (title table) " > "))
                                       (reverse (butlast (command-tables *default-scene-view*)))))))
       window))

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore scancode))
  ;; (format t "key-callback: w: ~a, k: ~a, sc: ~a, a: ~a, mk: ~a ~%"
  ;;         window key scancode action mod-keys)
  ;; (finish-output)
  (cond ;;((and (eq key :escape) (eq action :press))
         ;;(glfw:set-window-should-close))
        ((or (eq action :press) (eq action :repeat))
         (when *default-scene-view*
           (key-down *default-scene-view* key mod-keys)))
        ((eq action :release)
         (when *default-scene-view*
           (key-up *default-scene-view* key))))
  (update-window-title window))

(glfw:def-char-callback char-callback (window char)
  ;; (format t "char-callback: w: ~a, k: ~a~%" window char)
  ;; (finish-output)
  (when (and *default-scene-view* *ui-keyboard-focus*)
    (do-char-input *ui-keyboard-focus* char)))

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

(defun highlight-ui-item-under-mouse (ui-view x y)
  (when *current-highlighted-ui-item*
    (setf (highlight? *current-highlighted-ui-item*) nil)
    (setf *current-highlighted-ui-item* nil))
  (let ((ui-item (find-ui-at-point ui-view x y)))
    (if (and ui-item (is-active? ui-item))
        (progn
          (setf (highlight? ui-item) t)
          (when (not (eq ui-item *current-highlighted-ui-item*)) ;new highlighted item
            (setf *current-highlighted-ui-item* ui-item))
          ui-item)
        nil)))

(defun do-action-ui-item-under-mouse (ui-view x y button modifiers)
  (let ((ui-item (find-ui-at-point ui-view x y)))
    (if (and ui-item (is-active? ui-item))
        (progn
          (do-action ui-item x y button modifiers)
          (update-scene-ui)
          ui-item)
      nil)))

(defun mouse-moved (x y dx dy)
  (declare (ignore dx dy))
  ;;  (format t "mouse-moved x: ~a, y: ~a~%" x y)
  ;; menu takes priority over other ui components in view -- assume no overlap
  (let ((menu (menu *default-scene-view*)))
    (if (and menu (is-visible? menu))
        (when (not (highlight-ui-item-under-mouse menu x y))
          (dolist (ui-view (ui-contents *default-scene-view*))
            (highlight-ui-item-under-mouse ui-view x y)))
        (dolist (ui-view (ui-contents *default-scene-view*))
          (highlight-ui-item-under-mouse ui-view x y)))))        

(defun mouse-click (x y button modifiers)
  ;; clear keyboard focus
  (setf *ui-keyboard-focus* nil)
  ;; menu takes priority over other ui components in view -- assume no overlap
  (let ((menu (menu *default-scene-view*)))
    (if (and menu (is-visible? menu))
        (when (not (do-action-ui-item-under-mouse menu x y button modifiers))
          (dolist (ui-view (ui-contents *default-scene-view*))
            (do-action-ui-item-under-mouse ui-view x y button modifiers)))
        (dolist (ui-view (ui-contents *default-scene-view*))
          (do-action-ui-item-under-mouse ui-view x y button modifiers)))))

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

(defun update-gl-3d-viewport ()
  (let* ((w (first *window-size*))
         (h (second *window-size*)))
    ;;      (x0 (* w .25))
    ;;      (y0 (* h .25))
    ;;      (w0 (* w .50))
    ;;      (h0 (* h .75)))
    ;; (setf *viewport-aspect-ratio* (/ w0 h0))
    ;; (gl:viewport x0 y0 w0 h0)))
    (setf *viewport-aspect-ratio* (/ w h))
    ;; TODO -- figure out why this is half-size viewport on retina display
#-:darwin(gl:viewport 0 0 w h)
    ))

(defun window-resized (window w h)
  ;; (format t "window-resized: win: ~a, w: ~a, h: ~a ~%" window w h)
  ;; (finish-output)
  (setf *window-size* (list w h))
  (update-gl-3d-viewport)
  (draw-scene-view *default-scene-view*)      ; redraw while being resized
  (glfw:swap-buffers)
  (update-status-bar (status-bar *default-scene-view*) :view-width w :view-height h)
  (update-window-title window))

(glfw:def-window-position-callback window-position-callback (window x y)
  (declare (ignore window x y))
  (update-gl-3d-viewport))

(glfw:def-window-size-callback window-size-callback (window w h)
  ;; (format t "window-size-callback: win: ~a, w: ~a, h: ~a ~%" window w h)
  ;; (finish-output)
  (window-resized window w h))

(defun update-scene-ui ()
  (dolist (view (ui-contents *default-scene-view*))
    (create-contents view)
    (update-ui-content-position view)))

(defun update-status-bar-for-scene ()
  (let ((scene (scene *default-scene-view*)))
    (update-status-bar (status-bar *default-scene-view*)
                       :str0 (cond (*current-highlighted-ui-item*
                                    (help-string *current-highlighted-ui-item*))
                                   ((= 1 (length (command-tables *default-scene-view*)))
                                    "Mouse: orbit, [alt] pan, [ctrl] in/out")
                                   (t
                                    (format nil "Menu: ~A [tab to reset, left arrow to go back]"
                                            (apply #'strcat
                                                   (mapcar (lambda (table)
                                                             (strcat (title table) " > "))
                                                           (reverse (butlast (command-tables *default-scene-view*))))))))
                       :str1 (format nil "Current Frame: ~a [~a-~a]"
                                     (current-frame scene) (start-frame scene) (end-frame scene))
                       :str2 (format nil "Selection: ~a shape~p" (length (selection scene))  (length (selection scene)))
                       :str3 (format nil "Cursor: (~a, ~a)"
                                     (floor *current-mouse-pos-x*) (floor *current-mouse-pos-y*)))))

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
        ;; (let ((app-view (make-instance 'app-view)))
        ;;   (setf (scene (scene-view app-view)) scene)
        ;;   (setf *default-scene-view* (scene-view app-view))

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
          (glfw:set-char-callback 'char-callback)
          (glfw:set-mouse-button-callback 'mouse-callback)
          (glfw:set-cursor-position-callback 'cursor-position-callback)
          (glfw:set-window-position-callback 'window-position-callback)
          (glfw:set-window-size-callback 'window-size-callback)
          (setf *window-size* (glfw:get-window-size))
          ;;           (setf *viewport-aspect-ratio* (/ (first *window-size*) (second *window-size*)))
          (update-gl-3d-viewport)
          (update-window-title glfw:*window*)
          (update-status-bar (status-bar *default-scene-view*)
                             :view-width (first *window-size*) :view-height (second *window-size*))
          (initial-text-engine-setup)
          (loop until (glfw:window-should-close-p)
                do (draw-scene-view *default-scene-view*)
                do (update-status-bar-for-scene)
                do (glfw:swap-buffers)
                do (glfw:poll-events)))))))

(defmacro with-clear-scene (&body body)
  `(progn
     (clear-scene (scene *default-scene-view*))
     ,@body))

