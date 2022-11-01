(in-package #:kons-9)

;;;; ui-motion-outliner-item ==========================================================

(defclass-kons-9 ui-motion-outliner-item (ui-outliner-item)
  ())

(defmethod absolute-timing ((view ui-motion-outliner-item))
  (let ((motion (data view))
        (parent-motion (and (outliner-parent view) (data (outliner-parent view)))))
    (if (not parent-motion)
        (vector (start-time motion) (duration motion))
        (compute-motion-absolute-timing motion (absolute-timing (outliner-parent view))))))

(defmethod draw-view :after ((view ui-motion-outliner-item) x-offset y-offset)
  ;; draw bar showing motion timing data
  (with-accessors ((x ui-x) (y ui-y) (w ui-w) (h ui-h))
      view
    (gl:color 0.5 0.5 1.0 0.8)
    (let* (;(motion (data view))
           (base-width (- w 20))
           (timing (absolute-timing view))
           (x0 (+ x (* base-width (aref timing 0)))) ;(start-time motion))))
           (w0 (* base-width (aref timing 1)))) ;(duration motion))))
      (draw-rect-fill (+ x0 x-offset 10) (+ y y-offset +5) w0 (- h 10)))))

;;;; ui-motion-outliner-viewer ========================================================

(defclass-kons-9 ui-motion-outliner-viewer (ui-outliner-viewer)
  ())

(defmethod outliner-item-class ((view ui-motion-outliner-viewer))
  'ui-motion-outliner-item)

(defun make-ui-motion-outliner-viewer (title obj &optional (accessor-fn nil))
  (create-contents (make-instance 'ui-motion-outliner-viewer
                                  :ui-x 20 :ui-y 20 :title title :data-object obj :data-accessor-fn accessor-fn)))

(defmethod draw-view :after ((view ui-motion-outliner-viewer) x-offset y-offset)
  ;; draw current frame indicator
  (gl:color 0.3 0.3 0.6 1.0)
  (let ((rel-time (relative-current-time (scene *default-scene-view*)))
        (x-lo (+ (ui-x view) x-offset 10))
        (x-hi (+ (ui-x view) (ui-w view) x-offset -10))
        (y-lo (+ (ui-y view) y-offset *ui-button-item-height*))
        (y-hi (+ (ui-y view) (ui-h view) y-offset)))
    (draw-line (lerp rel-time x-lo x-hi) y-lo
               (lerp rel-time x-lo x-hi) y-hi)))
