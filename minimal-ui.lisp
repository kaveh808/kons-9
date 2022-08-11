(in-package #:kons-9)

;;; dummy code to keep main.lisp happy
(defun update-view (x)
  (declare (ignore x)))

;;;; scene-view ================================================================

(defclass scene-view (ns:ns-opengl-view)
  ((scene :accessor scene :initarg :scene :initform nil)
   (schematic-view :accessor schematic-view :initarg :schematic-view :initform nil)) ; dummy code to keep main.lisp happy
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((view scene-view) &rest initargs)
  (declare (ignore initargs))
  (#/setPixelFormat: view (new-pixel-format #$NSOpenGLPFAColorSize 32
                                            #$NSOpenGLPFADepthSize 24))
  ;; (#/setWantsLayer: view t)
  ;; (#/setBorderWidth: (#/layer view) 1)
  (init-view-camera))

;;; display the view
(objc:defmethod (#/drawRect: :void) ((view scene-view) (rect :<NSR>ect))
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
  (3d-flush-render))

;;; respond to first click in window
(objc:defmethod (#/acceptsFirstMouse: :<BOOL>) ((self scene-view) event)
  (declare (ignore event))
  t)

(objc:defmethod (#/mouseDown: :void) ((self scene-view) event)
  (redraw))

;;; accept key events
(objc:defmethod (#/acceptsFirstResponder :<BOOL>) ((self scene-view))
   t)

(objc:defmethod (#/mouseDragged: :void) ((self scene-view) event)
  (let ((flags (#/modifierFlags event))
        (p (#/locationInWindow event))
        (dx (coerce (#/deltaX event) 'single-float))
        (dy (coerce (#/deltaY event) 'single-float)))
    (cond ((or (= flags 524320) (= flags 524352)) ; #$NSAlternateKeyMask -- this has been deprecated
           (if (>= (abs dx) (abs dy))
               (incf *cam-side-dist* (* 0.1 dx))
               (incf *cam-up-dist* (* -0.1 dy))))
          ((or (= flags 1048584) (= flags 1048592)) ; command
           (incf *cam-fwd-dist* (* 0.1 dx)))
          (t
           (incf *cam-x-rot* dy)
           (incf *cam-y-rot* dx))))
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
(objc:defmethod (#/keyDown: :void) ((self scene-view) event)
  (let* ((str (objc:lisp-string-from-nsstring (#/charactersIgnoringModifiers event)))
         (char (char str 0))
         (scene (scene self)))
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

;;;; app-window ================================================================

(defclass app-window (ns:ns-window)
  ()
  (:metaclass ns:+ns-object))

(defparameter *window-x-size* 960)
(defparameter *window-y-size* 540)

;;; create and display a window containing an OpeGL view
(defun show-window (scene)
  (setf *scene-views* '())
  (ns:with-ns-rect
    (frame 0 0 *window-x-size* *window-y-size*)
    (let* ((w (make-instance 'app-window
                             :title "foo"
                             :with-content-rect frame
                             :style-mask (logior #$NSTitledWindowMask
                                                 #$NSClosableWindowMask
                                                 #$NSMiniaturizableWindowMask)
                             :backing #$NSBackingStoreBuffered
                             :defer t))
           (v (make-instance 'scene-view :scene scene)))
      (#/setContentView: w v)

      ;; (#/addSubview: (#/contentView w) v)
      ;; (#/setFrameOrigin: v (ns:make-ns-point 0 20))
      ;; (#/setFrameSize: v (ns:make-ns-point *window-x-size* *window-y-size*))
      (push v *scene-views*)
      (redraw)
      (#/release v)
      (#/center w)
      (#/orderFront: w nil)
      w)))


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
