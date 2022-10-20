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

;;;; scene-view ================================================================

(defclass-kons-9 scene-view ()
  ((scene nil)
   (status-bar nil)
   (ui-contents (make-instance 'ui-group :layout :vertical :justification :right/bottom
                                         :draw-border? nil :padding 0 :spacing 10))
   (ui-contents-scroll 0)               ;offset in y
   (menu nil)
   (command-tables '())
   (mouse-binding nil)                  ;fn to call when control-mouse-drag
   (mouse-binding-string nil)
   ))

(defmethod initialize-instance :after ((view scene-view) &rest initargs)
  (declare (ignore initargs))
  (init-view-camera)
  (setf (status-bar view) (make-status-bar))
  (push (app-command-table view) (command-tables view)))

(defun show-ui-content (view)
  (let ((contents (ui-contents *default-scene-view*)))
    (ui-add-child contents view)
    (update-layout contents)
    ;; scroll if going off bottom of view
    (let ((delta-y (- (+ (ui-y contents) (ui-h contents))
                      (- (second *window-size*) (ui-h (status-bar *default-scene-view*)) 20))))
      (when (> delta-y 0)
        (decf (ui-contents-scroll *default-scene-view*) delta-y)))
    (update-ui-content-position)))

(defun hide-ui-content (view)
  (let ((contents (ui-contents *default-scene-view*)))
    (ui-remove-child contents view)
    (update-layout contents)))

(defun update-ui-content-position ()
  (let ((view (ui-contents *default-scene-view*)))
  ;; (case location
  ;;   (:top-left
  ;;    (ui-set-position view 20 20))
  ;;   (:top-right
    (ui-set-position view
                     (- (first *window-size*) (ui-w view) 20)
                     (+ 20 (ui-contents-scroll *default-scene-view*)))))
    ;; (:bottom-left
    ;;  (ui-set-position view 20 (- (second *window-size*) (ui-h view) 20)))
    ;; (:bottom-right
    ;;  (ui-set-position view
    ;;                   (- (first *window-size*) (ui-w view) 20)
    ;;                   (- (second *window-size*) (ui-h view) 20)))))

(defun reposition-ui-content-after-delete ()
  (when (< (ui-contents-scroll *default-scene-view*) 0)
    (let* ((contents (ui-contents *default-scene-view*))
           (delta-y (- (+ (ui-y contents) (ui-h contents))
                       (- (second *window-size*) (ui-h (status-bar *default-scene-view*)) 20))))
      (when (< delta-y 0)
        (decf (ui-y contents) delta-y)
        (setf (ui-y contents) (min 20 (ui-y contents)))))))

(defun app-command-table (view)
  (let ((table (make-instance 'command-table :title "kons-9")))
    (ct-subtable :S "Scene" (scene-command-table view))
    (ct-subtable :E "Edit" (edit-command-table view))
    (ct-subtable :C "Create" (create-command-table view))
    (ct-subtable :I "Inspect" (inspect-command-table view))
    (ct-subtable :D "Display" (display-command-table view))
    (ct-subtable :X "Context" (context-command-table))
    table))

(defun scene-command-table (view)
  (let ((table (make-instance 'command-table :title "Scene")))
    (ct-entry :N "New Scene" (clear-scene (scene view)) (ui-clear-children (ui-contents view)))
;; TODO -- open and save need work
;;    (ct-entry :O "Open Scene" (hide-menu view) (ui-clear-children (ui-contents view)) (show-open-scene-dialog))
;;    (ct-entry :S "Save Scene" (hide-menu view) (show-save-scene-dialog))
    (ct-entry :E "Export OBJ Scene" (show-export-obj-scene-dialog))
    (ct-entry :U "Export USD Scene" (show-export-usd-scene-dialog))
    (ct-entry :I "Initialize Scene" (init-scene (scene view))) ;same as '[' key binding
    (ct-entry :Q "Quit Scene" (glfw:set-window-should-close))
    table))

(defun show-open-scene-dialog ()
  (show-ui-content
   (make-text-input-dialog-box "Open Scene File"
                               (lambda (str)
                                 (let ((scene (load-scene str)))
                                   (setf *scene* scene)
                                   (setf (scene *default-scene-view*) *scene*))))))

(defun show-save-scene-dialog ()
  (show-ui-content
   (make-text-input-dialog-box "Save Scene File"
                               (lambda (str)
                                 (save-scene (scene *default-scene-view*) str)))))

(defun show-export-obj-scene-dialog ()
  (show-ui-content
   (make-text-input-dialog-box "Export OBJ File"
                               (lambda (str)
                                 (export-obj (scene *default-scene-view*) str)))))

(defun show-export-usd-scene-dialog ()
  (show-ui-content
   (make-text-input-dialog-box "Export USD File"
                               (lambda (str)
                                 (export-usd (scene *default-scene-view*) str)))))

(defun edit-command-table (view)
  (let ((table (make-instance 'command-table :title "Edit")))
;;    (ct-entry :S "Select (TBD)")
    ;; (ct-entry :backspace "Delete" (remove-current-selection (scene view)))
    ;; TODO -- handle selected motions -- activate/deactivate...
    (ct-entry :S "Show" (dolist (shape (selected-shapes (scene view))) (setf (is-visible? shape) t)))
    (ct-entry :H "Hide" (dolist (shape (selected-shapes (scene view))) (setf (is-visible? shape) nil)))
    ;; (ct-entry :D "Duplicate (TBD)")
    ;; (ct-entry :G "Group (TBD)")
    ;; (ct-entry :U "Ungroup (TBD)")
    ;; (ct-entry :F "Find (TBD)")
    table))

(defun create-command-table (view)
  (declare (ignore view))
  (let ((table (make-instance `command-table :title "Create")))
    (ct-subtable :C "Create Curve" (curve-command-table))
    (ct-subtable :P "Create Polyhedron" (polyhedron-command-table))
    table))

(defun curve-command-table ()
  (let ((table (make-instance `command-table :title "Create Curve")))
    (ct-make-shape :L "Line Curve" (make-line-curve (p! 0 0 0) (p! 2 2 2) 4))
    (ct-make-shape :R "Rectangle Curve" (make-rectangle-curve 2 1 4))
    (ct-make-shape :S "Square Curve" (make-square-curve 1.5))
    (ct-make-shape :C "Circle Curve" (make-circle-curve 2.0 16))
    (ct-make-shape :A "Arc Curve" (make-arc-curve 2.0 0 90 8))
    (ct-make-shape :N "Sine Curve" (make-sine-curve-curve 360 1 2 1 16))
    (ct-make-shape :P "Spiral Curve" (make-spiral-curve .2 2.0 -1.0 4 64))
    table))

(defun polyhedron-command-table ()
  (let ((table (make-instance `command-table :title "Create Polyhedron")))
    (ct-make-shape :T "Tetrahedron" (make-tetrahedron 2.0))
    (ct-make-shape :C "Cube" (make-cube 2.0))
    (ct-make-shape :O "Octahedron" (make-octahedron 2.0))
    (ct-make-shape :D "Dodecahedron" (make-dodecahedron 2.0))
    (ct-make-shape :I "Icosahedron" (make-icosahedron 2.0))
    (ct-make-shape :S "Sphere" (make-cube-sphere 4.0 3))
    table))

(defun inspect-command-table (view)
  (let ((table (make-instance `command-table :title "Inspect")))
    (ct-entry :R "Reset Camera" (init-view-camera) (3d-update-light-settings))
    (ct-entry :F "Frame Selection")
    (ct-entry :I "Inspector" (show-ui-content (make-ui-inspector (or (selection (scene view))
                                                                     (scene view)))))
    (ct-entry :S "Shapes" (show-ui-content (make-ui-outliner-viewer "Shapes" (shape-root (scene view)))))
    (ct-entry :M "Motions" (show-ui-content (make-ui-motion-outliner-viewer "Motions" (motion-root (scene view)))))
    table))

(defun display-command-table (view)
  (declare (ignore view))
  (let ((table (make-instance 'command-table :title "Display")))
    (ct-entry :grave-accent "Toggle Lighting" (setf *do-lighting?* (not *do-lighting?*)))
    (ct-entry :1 "Toggle Filled Display" (setf *display-filled?* (not *display-filled?*)))
    (ct-entry :2 "Toggle Wireframe Display" (setf *display-wireframe?* (not *display-wireframe?*)))
    (ct-entry :3 "Toggle Point Display" (setf *display-points?* (not *display-points?*)))
    (ct-entry :4 "Toggle Backface Culling" (setf *do-backface-cull?* (not *do-backface-cull?*)))
    (ct-entry :5 "Toggle Smooth Shading" (setf *do-smooth-shading?* (not *do-smooth-shading?*)))
    (ct-entry :6 "Toggle Ground Plane Display" (setf *display-ground-plane?* (not *display-ground-plane?*)))
    (ct-entry :7 "Toggle World Axes Display" (setf *display-axes?* (not *display-axes?*)))
    (ct-entry :8 "Set Bright Theme" (set-theme-bright))
    (ct-entry :9 "Set Dark Theme" (set-theme-dark))
    table))

(defun context-command-table ()
  (let ((table (make-instance 'command-table :title "Context" :is-dynamic? t)))
    table))

(defun transform-command-table ()
  (let ((table (make-instance `command-table :title "Transform")))
    (ct-subtable :T "Translate" (translate-command-table))
    (ct-subtable :R "Rotate" (rotate-command-table))
    (ct-subtable :S "Scale" (scale-command-table))
    table))

(defmacro mouse-transform-setup (help-str &rest action-exprs)
  `(progn
     (setf (mouse-binding-string *default-scene-view*)
           (strcat "Mouse [CTRL]: " ,help-str "."))
     (setf (mouse-binding *default-scene-view*)
           (lambda (scene dx dy)
             (declare (ignore dy))
             (dolist (shape (selected-shapes scene))
               ,@action-exprs)))))

(defun translate-command-table ()
  (let ((table (make-instance `command-table :title "Translate")))
    (ct-entry :X "Translate X" (mouse-transform-setup
                                "Translate X"
                                (translate-by shape (p! (* dx .01) 0 0))))
    (ct-entry :Y "Translate Y" (mouse-transform-setup
                                "Translate Y"
                                (translate-by shape (p! 0 (* dx .01) 0))))
    (ct-entry :Z "Translate Z" (mouse-transform-setup
                                "Translate Z"
                                (translate-by shape (p! 0 0 (* dx .01)))))
    table))

(defun rotate-command-table ()
  (let ((table (make-instance `command-table :title "Rotate")))
    (ct-entry :X "Rotate X" (mouse-transform-setup
                                "Rotate X"
                                (rotate-by shape (p! (* dx 1) 0 0))))
    (ct-entry :Y "Rotate Y" (mouse-transform-setup
                                "Rotate Y"
                                (rotate-by shape (p! 0 (* dx 1) 0))))
    (ct-entry :Z "Rotate Z" (mouse-transform-setup
                                "Rotate Z"
                                (rotate-by shape (p! 0 0 (* dx 1)))))
    table))

(defun scale-command-table ()
  (let ((table (make-instance `command-table :title "Scale")))
    (ct-entry :U "Scale Uniformly" (mouse-transform-setup
                                    "Scale Uniformly"
                                    (scale-by shape (p! (+ 1 (* dx .01)) (+ 1 (* dx .01)) (+ 1 (* dx .01))))))
    (ct-entry :X "Scale X" (mouse-transform-setup
                                "Scale X"
                                (scale-by shape (p! (+ 1 (* dx .01)) 1 1))))
    (ct-entry :Y "Scale Y" (mouse-transform-setup
                                "Scale Y"
                                (scale-by shape (p! 1 (+ 1 (* dx .01)) 1))))
    (ct-entry :Z "Scale Z" (mouse-transform-setup
                                "Scale Z"
                                (scale-by shape (p! 1 1 (+ 1 (* dx .01))))))
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
  (draw-view (ui-contents view)))

(defmethod make-popup-menu ((view scene-view))
  (when (command-tables view)
    (setf (menu view)
          (make-instance 'ui-popup-menu :ui-x 20 :ui-y 20 :command-table (car (command-tables view))))
    (create-contents (menu view))))

;;; respond to first click in window
;; (defmethod accepts-first-mouse ((self scene-view) event)
;;   (declare (ignore event))
;;   t)

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

;;; TODO -- register global key bindings: esc, space, tab, [, etc
(defmethod key-down ((self scene-view) key mod-keys)
  ;; (format t "key-down self: ~a, key: ~a mod-keys: ~a~%" self key mod-keys)
  ;; (finish-output)
  (cond ((eq :space key)                ;play animation
         (update-scene (scene self))
         (update-scene-ui))
        ((eq :left-bracket key)         ;init scene
         (init-scene (scene self)))
        ((eq :tab key)                  ;hide/show menu
         (cond ((and (menu self) (is-visible? (menu self)))
                (hide-menu self))
               (t
                (show-menu self))))
        ((eq :escape key)               ;hide all inspectors
         (ui-clear-children (ui-contents self))
         (setf (ui-contents-scroll self) 0))
        ((and (member :shift mod-keys) (eq :left key)) ;hide last inspector
         (ui-remove-last-child (ui-contents self))
         (update-scene-ui)
         (if (= 0 (length (children (ui-contents self))))
             (setf (ui-contents-scroll self) 0)
             (reposition-ui-content-after-delete)))
        (*ui-keyboard-focus*            ;handle text box input,  TODO -- add arrow keys
         (cond ((and (eq :v key) (member :super mod-keys))
                (do-paste-input *ui-keyboard-focus* (glfw:get-clipboard-string)))
               ((and (eq :c key) (member :super mod-keys))
                (glfw:set-clipboard-string (do-copy-input *ui-keyboard-focus*)))
               ((and (eq :x key) (member :super mod-keys))
                (glfw:set-clipboard-string (do-cut-input *ui-keyboard-focus*)))
               ((eq :backspace key)
                (do-backspace-input *ui-keyboard-focus*))
               ))
        ((eq :left key)                 ;go to previous menu
         (when (and (menu self) (is-visible? (menu self)) (> (length (command-tables self)) 1))
           (setf (command-tables self) (cdr (command-tables self)))))
        ((or (eq :up key) (eq :down key)) ;scroll inspectors
         (incf (ui-contents-scroll self) (if (eq :up key) -10 10))
         (update-ui-content-position))
        ((and (menu self) (is-visible? (menu self))) ;do menu selection
         (do-command (car (command-tables self)) key)
         (update-scene-ui))
        ((= (length (command-tables self)) 1) ;do menu selection for top menu even if hidden
         (when (do-command (car (command-tables self)) key)
           (show-menu self)))))

(defmethod key-up ((self scene-view) key)
  )

;;XXX This doesn't work on wayland. I think wayland expects clients
;; to draw the window decorations themselves
(defun update-window-title (window)
  (glfw:set-window-title (format nil "kons-9 | window ~A" *window-size*) window))

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
          (highlight-ui-item-under-mouse (ui-contents *default-scene-view*) x y))
        (highlight-ui-item-under-mouse (ui-contents *default-scene-view*) x y))))

(defun mouse-click (x y button modifiers)
  ;; clear keyboard focus
  (setf *ui-keyboard-focus* nil)
  ;; menu takes priority over other ui components in view -- assume no overlap
  (let ((menu (menu *default-scene-view*)))
    (if (and menu (is-visible? menu))
        (when (not (do-action-ui-item-under-mouse menu x y button modifiers))
          (do-action-ui-item-under-mouse (ui-contents *default-scene-view*) x y button modifiers))
        (do-action-ui-item-under-mouse (ui-contents *default-scene-view*) x y button modifiers))))

(defun mouse-dragged (x y dx dy)
  (declare (ignore x y))
;;  (format t "mouse-dragged dx: ~a, dy: ~a, mod: ~a~%" dx dy *current-mouse-modifier*)
  (cond ((eq :control *current-mouse-modifier*) ;user-defined mouse binding
         (when (mouse-binding *default-scene-view*)
           (funcall (mouse-binding *default-scene-view*) (scene *default-scene-view*) dx dy)))
        ((eq :alt *current-mouse-modifier*)    ;default mouse scene navigation -- pan
         (if (>= (abs dx) (abs dy))
             (incf *cam-side-dist* (* 0.1 dx))
             (incf *cam-up-dist* (* -0.1 dy))))
        ((eq :shift *current-mouse-modifier*)  ;default mouse scene navigation -- in/out
         (incf *cam-fwd-dist* (* 0.1 dx)))
        (t                                     ;default mouse scene navigation -- orbit
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
  (update-scene-ui)
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
  (loop for view across (children (ui-contents *default-scene-view*))
        do (create-contents view))
  (update-layout (ui-contents *default-scene-view*))
  (update-ui-content-position))

(defun update-status-bar-for-scene ()
  (let* ((scene (scene *default-scene-view*))
         (menu-str (format nil "TAB: hide menu. L-ARROW: previous menu."))
         (mouse-str "Mouse: orbit, [ALT] pan, [SHIFT] in/out. TAB: show/hide menu.")
         (mouse-binding-str (mouse-binding-string *default-scene-view*))
         (inspector-str (if (> (length (children (ui-contents *default-scene-view*))) 0)
                            "UP/DN-ARROW: scroll inspectors. ESC: close inspectors, SHIFT-L-ARROW: close last inspector."
                            ""))
         (secondary-str (if mouse-binding-str
                            mouse-binding-str
                            inspector-str)))
    (update-status-bar (status-bar *default-scene-view*)
                       :str0 (format nil "Current Frame: ~a [~a-~a]"
                                     (current-frame scene) (start-frame scene) (end-frame scene))
                       :str1 (let ((n-shapes (num-shapes scene))
                                   (n-motions (num-motions scene)))
                               (format nil "Scene: ~a shape~p, ~a motion~p"
                                       n-shapes n-shapes n-motions n-motions))
                       :str2 (format nil "Selection: ~a shape~p" (length (selection scene))  (length (selection scene)))
                       :str3 (format nil "Cursor: (~a, ~a)"
                                     (floor *current-mouse-pos-x*) (floor *current-mouse-pos-y*))
                       :str4 (cond (*current-highlighted-ui-item*
                                    (help-string *current-highlighted-ui-item*))
                                   ((not (menu *default-scene-view*))
                                    (strcat mouse-str " " secondary-str))
                                   (t
                                    (strcat menu-str " " secondary-str)))
                       )))

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
          ;; use FLOOR due to bug encountered with user's 4K monitor setting of 1.1458334
          (setf (monitor-scale *drawing-settings*)
                (floor (first (glfw:get-monitor-content-scale (glfw:get-primary-monitor)))))
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

;;; does not return a useful value
(defmacro with-clear-scene (&body body)
  `(progn
     (clear-scene (scene *default-scene-view*))
     ,@body
     (update-scene-ui)))


