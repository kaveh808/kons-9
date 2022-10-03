(in-package #:kons-9)

;;;; ui utils ==================================================================

(defparameter *ui-popup-menu-width* 200)
(defparameter *ui-button-item-width* 200)
(defparameter *ui-button-item-height* 25)
(defparameter *ui-default-padding* 20)
(defparameter *ui-default-spacing* 5)

(defparameter *ui-border-width* 1)
(defparameter *ui-border-color* (c! 0 0 0))
(defparameter *ui-highlight-border-width* 4)
(defparameter *ui-highlight-border-color* (c! 0 0 1))

(defparameter *ui-keyboard-focus* nil)

;;; TODO -- temporary, query font later
(defparameter *ui-font-width* 7.3)


;;;; uitils ====================================================================

(defun ui-text-width (text)
  (* (length text) *ui-font-width*))

(defun ui-centered-text-x (text width)
  (floor (* 0.5 (- width (ui-text-width text)))))

;;;; ui-rect ===================================================================

(defclass-kons-9 ui-rect ()
  ((ui-x 0.0)
   (ui-y 0.0)
   (ui-w 0.0)
   (ui-h 0.0)))

(defmethod ui-set-position ((rect ui-rect) x y)
  (setf (ui-x rect) x)
  (setf (ui-y rect) y)
  rect)
  
(defmethod ui-set-size ((rect ui-rect) w h)
  (setf (ui-w rect) w)
  (setf (ui-h rect) h)
  rect)
  
(defmethod ui-set-rect ((rect ui-rect) x y w h)
  (setf (ui-x rect) x)
  (setf (ui-y rect) y)
  (setf (ui-w rect) w)
  (setf (ui-h rect) h)
  rect)
  
;;;; ui-view ===================================================================

(defclass-kons-9 ui-view (ui-rect)
  ((ui-parent nil)
   (bg-color (c! 0 0 0 0))
   (fg-color (c! 0 0 0 1))
   (is-visible? t)
   (draw-border? t)
   (is-active? nil)
   (on-click-fn nil)                    ;(fn modifiers)
   (highlight? nil)
   (help-string "")
   (can-have-keyboard-focus? nil)))

(defmethod world-coords ((view ui-view) &optional (accum-coords '(0 0)))
  (if (null (ui-parent view))
      (list (+ (ui-x view) (first accum-coords))
            (+ (ui-y view) (second accum-coords)))
      (world-coords (ui-parent view) (list (+ (ui-x view) (first accum-coords))
                                           (+ (ui-y view) (second accum-coords))))))

(defmethod local-coords ((view ui-view) x y)
  (let ((world-coords (world-coords view)))
    (list (- x (first world-coords)) (- y (second world-coords)))))

;;; entry point for all UI actions
(defmethod do-action ((view ui-view) x y button modifiers)
  (declare (ignore x y button))
  (when (on-click-fn view)
    (funcall (on-click-fn view) modifiers)))

;;;; ui-label-item =============================================================

(defclass-kons-9 ui-label-item (ui-view)
  ((text "Label")
   (text-padding 10))
  (:default-initargs
   :draw-border? nil
   :ui-h *ui-button-item-height*))

(defmethod set-width-for-text ((view ui-view))
  (setf (ui-w view) (+ (ui-text-width (text view)) (* 2 (text-padding view)))))

;;;; ui-data-item ==============================================================

(defclass-kons-9 ui-data-item (ui-label-item)
  ((data nil)))

;;;; ui-button-item ============================================================

(defun key-binding-string (key-binding)
  (if key-binding
      (case key-binding
        (:backspace "BS")
        (:space "SP")
        (:grave-accent "`")
        (t (string key-binding)))
      ""))

(defclass-kons-9 ui-button-item (ui-label-item)
  ((key-text ""))
  (:default-initargs
   :draw-border? t
   :bg-color (c! 0.7 0.7 0.7 0.8)
   :is-active? t))

(defmethod set-width-for-text ((view ui-button-item))
  (setf (ui-w view) (max 80 (+ (ui-text-width (text view)) (* 2 (text-padding view))))))

;;;; ui-check-box-item =========================================================

(defclass-kons-9 ui-check-box-item (ui-label-item)
  ((is-pushed? nil)
   (check-bg-color (c! 0.7 0.7 0.7 0.8)))
  (:default-initargs
   :draw-border? nil
   :is-active? t))

(defmethod set-width-for-text ((view ui-check-box-item))
  (setf (ui-w view) (+ (ui-text-width (text view)) (* 2 (text-padding view)) 30)))

(defmethod do-action :before ((view ui-check-box-item) x y button modifiers)
  (declare (ignore x y button modifiers))
  (setf (is-pushed? view) (not (is-pushed? view))))

;;;; ui-text-box-item ==========================================================

(defclass-kons-9 ui-text-box-item (ui-label-item)
  ((cursor-position 0))
  (:default-initargs
   :draw-border? t
   :bg-color (c! 1 1 1 1)
   :is-active? t
   :can-have-keyboard-focus? t))

(defmethod do-action ((view ui-text-box-item) x y button modifiers)
  (declare (ignore button modifiers))
  (setf *ui-keyboard-focus* view)
  (setf (cursor-position view) (max 0
                                    (min (length (text view))
                                         (floor (/ (first (local-coords view x y)) *ui-font-width*))))))

;;; TODO ++ edit text
;;; TODO ++ handle char input properly
;;; TODO ++ do not insert modifier key text
;;; TODO ++ draw cursor when is *ui-keyboard-focus*
;;; TODO -- mark region (shift-click, drag, double click, etc)
;;; TODO -- arrow keys

(defun insert-string (string insert position)
  (concatenate 'string
               (subseq string 0 position)
               insert
               (if (< position (length string))
                   (subseq string position (length string))
                   "")))
  
(defmethod do-char-input ((view ui-text-box-item) char)
  (setf (text view) (insert-string (text view) (string char) (cursor-position view)))
  (incf (cursor-position view)))

(defmethod do-paste-input ((view ui-text-box-item) string)
  (setf (text view) (insert-string (text view) string (cursor-position view)))
  (incf (cursor-position view) (length string)))

(defmethod do-copy-input ((view ui-text-box-item))
  (text view))

(defmethod do-cut-input ((view ui-text-box-item))
  (let ((result (text view)))
    (setf (text view) "")
    result))

(defmethod do-backspace-input ((view ui-text-box-item))
  (when (> (cursor-position view) 0)
    (setf (text view) (concatenate 'string
                                   (subseq (text view) 0 (1- (cursor-position view)))
                                   (subseq (text view) (cursor-position view) (length (text view)))))
    (decf (cursor-position view))))

;;;; ui-menu-item ==============================================================

(defclass-kons-9 ui-menu-item (ui-button-item)
  ()
  (:default-initargs
   :bg-color (c! 0.8 0.8 0.8 0.25)))

;;;; ui-group ==============================================================

(defclass-kons-9 ui-group (ui-view)
  ((layout :vertical) ; :fit-children, :vertical, :horizontal
   (justification :center) ; :center :left/top :right/bottom
   (spacing *ui-default-spacing*)
   (padding *ui-default-padding*)
   (title nil)
   (children (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod create-contents ((view ui-group))
  ;; subclass responsibility
  view)

(defmethod ui-add-child ((view ui-group) (child ui-view))
  (setf (ui-parent child) view)
  (vector-push-extend child (children view))
  child)

(defmethod ui-add-children ((view ui-group) children)
  (dolist (child children)
    (ui-add-child view child))
  view)

(defmethod ui-add-child-at ((view ui-group) (child ui-view) index)
  (setf (ui-parent child) view)
  (insert-into-array (children view) child index)
  child)

(defmethod ui-remove-child ((view ui-group) (child ui-view))
  (setf (ui-parent child) nil)
  (setf (children view) (delete child (children view)))
  child)

(defmethod ui-clear-children ((view ui-group))
  (loop for child across (children view)
        do (setf (ui-parent child) nil))
  (setf (fill-pointer (children view)) 0)
  view)

(defmethod update-layout ((view ui-group))
  (ecase (layout view)
    (:fit-children (update-layout-fit-children view))
    (:vertical (update-layout-vertical view))
    (:horizontal (update-layout-horizontal view)))
  view)

(defmethod update-layout-fit-children ((view ui-group))
  (with-accessors ((title title) (padding padding) (children children))
      view
    (if (= (length children) 0)
        (progn
          (setf (ui-w view) (if title (+ (ui-text-width title) *ui-default-padding*) *ui-button-item-width*))
          (setf (ui-h view) *ui-button-item-height*))
        (let* ((x (reduce 'min children :key 'ui-x))
               (y (reduce 'min children :key 'ui-y))
               (w (reduce 'max (map 'vector (lambda (child) (+ (ui-x child) (ui-w child))) children)))
               (h (reduce 'max (map 'vector (lambda (child) (+ (ui-y child) (ui-h child))) children)))
               (dx (- x padding))
               (dy (- y padding)))
          (setf (ui-w view) (+ dx w padding))
          (setf (ui-h view) (+ dy h padding))
          (loop for child across children
                do (incf (ui-x child) dx)
                   (incf (ui-y child) dy))))))

(defmethod update-layout-horizontal ((view ui-group))
  (with-accessors ((title title) (padding padding) (spacing spacing) (justification justification)
                   (children children))
      view
    (if (= (length children) 0)
        (progn
          (setf (ui-w view) (if title (+ (ui-text-width title) *ui-default-padding*) *ui-button-item-width*))
          (setf (ui-h view) *ui-button-item-height*))
        (let* ((title-w (if title (+ (ui-text-width title) *ui-default-padding*) 0))
               (title-h (if title *ui-button-item-height* 0))
               (width (max (+ (reduce '+ children :key 'ui-w)
                              (* spacing (1- (length children)))
                              (* padding 2))
                           title-w))
               (height (+ (reduce 'max children :key 'ui-h)
                          (* padding 2)
                          title-h))
               (x padding))
          (setf (ui-w view) width)
          (setf (ui-h view) height)
          (loop for child across children
                do (setf (ui-y child) (case justification
                                        (:left/top padding)
                                        (:right/bottom (- (ui-h view) (ui-h child) padding))
                                        (:center (/ (- (ui-h view) (ui-h child)) 2))))
                   (setf (ui-x child) x)
                   (incf x (+ (ui-w child) spacing))))))
  view)

(defmethod update-layout-vertical ((view ui-group))
  (with-accessors ((title title) (padding padding) (spacing spacing) (justification justification)
                   (children children))
      view
    (if (= (length children) 0)
        (progn
          (setf (ui-w view) (if title (+ (ui-text-width title) *ui-default-padding*) *ui-button-item-width*))
          (setf (ui-h view) *ui-button-item-height*))
        (let* ((title-w (if title (+ (ui-text-width title) *ui-default-padding*) 0))
               (title-h (if title *ui-button-item-height* 0))
               (width (max (+ (reduce 'max children :key 'ui-w)
                              (* padding 2))
                           title-w))
               (height (+ (reduce '+ children :key 'ui-h)
                          (* spacing (1- (length children)))
                          (* padding 2)
                          title-h))
               (y (+ padding title-h)))
          (setf (ui-w view) width)
          (setf (ui-h view) height)
          (loop for child across children
                do (setf (ui-x child) (case justification
                                        (:left/top padding)
                                        (:right/bottom (- (ui-w view) (ui-w child) padding))
                                        (:center (/ (- (ui-w view) (ui-w child)) 2))))
                   (setf (ui-y child) y)
                   (incf y (+ (ui-h child) spacing))))))
  view)

;;;; ui-message-box ===========================================================

(defclass-kons-9 ui-message-box (ui-group)
  ()
  (:default-initargs
   :is-visible? t
   :bg-color (c! 0.8 0.8 0.8 0.8)))

(defun make-message-box (message-text &optional (ok-button-text "OK"))
  (let* ((box (make-instance 'ui-message-box
                             :ui-x 200
                             :ui-y 200
                             :spacing 10
                             :padding 10))
         (message (make-instance 'ui-label-item :text message-text))
         (button (make-instance 'ui-button-item :text ok-button-text
                                                :on-click-fn (lambda (modifiers)
                                                               (declare (ignore modifiers))
                                                               (setf (is-visible? box) nil)))))
    (set-width-for-text message)
    (set-width-for-text button)
    (ui-add-children box (list message button))
    (update-layout box)))

#|
(setf (ui-contents *default-scene-view*)
      (list (make-message-box "Hello, kons-9! Doing 3D graphics in Common Lisp.")))
|#

;;;; ui-dialog-box ===========================================================

(defclass-kons-9 ui-dialog-box (ui-group)
  ()
  (:default-initargs
   :is-visible? t
   :bg-color (c! 0.8 0.8 0.8 0.8)))

(defun make-dialog-box (contents &optional (ok-button-text "OK") (cancel-button-text "Cancel"))
  (let* ((box (make-instance 'ui-dialog-box
                             :ui-x 20
                             :ui-y 20
                             :title "Dialog Box Test"))
         (contents-group (make-instance 'ui-group
                                        :layout :vertical
                                        :spacing 5
                                        :padding 0
                                        :draw-border? t ;for debugging
                                        ))
         (buttons-group (make-instance 'ui-group
                                        :layout :horizontal
                                        :spacing 5
                                        :padding 10
                                        :draw-border? t ;for debugging
                                        ))
         (cancel-button (make-instance 'ui-button-item
                                       :ui-w 80
                                       :text cancel-button-text
                                       :on-click-fn (lambda (modifiers)
                                                      (declare (ignore modifiers))
                                                      (setf (is-visible? box) nil))
                                       ))
         (ok-button (make-instance 'ui-button-item
                                   :ui-w 80
                                   :text ok-button-text
                                   :on-click-fn (lambda (modifiers)
                                                  (declare (ignore modifiers))
                                                  (setf (is-visible? box) nil))
                                   ))
         )
    (ui-add-children contents-group contents)
    (ui-add-children buttons-group (list cancel-button ok-button))
    (ui-add-child box (update-layout contents-group))
    (ui-add-child box (update-layout buttons-group))
    (update-layout box)))

#|
(setf (ui-contents *default-scene-view*)
      (list (make-dialog-box (list (make-instance 'ui-label-item :ui-w *ui-popup-menu-width*
                                                                 :text "Label 1")
                                   (make-instance 'ui-label-item :ui-w *ui-popup-menu-width*
                                                                 :text "Label 2")
                                   (make-instance 'ui-button-item :ui-w *ui-popup-menu-width*
                                                                  :text "My Button")
                                   (make-instance 'ui-check-box-item :ui-w *ui-popup-menu-width*
                                                                     :text "Check Box")
                                   (make-instance 'ui-text-box-item :ui-w *ui-popup-menu-width*
                                                                    :text "Text Box")
                                   ))))
|#

(defun make-text-input-dialog-box (title ok-action-fn &optional (ok-button-text "OK") (cancel-button-text "Cancel"))
  (let* ((box (make-instance 'ui-dialog-box
                             :ui-x 20
                             :ui-y 20
                             :title title))
         (contents-group (make-instance 'ui-group
                                        :layout :vertical
                                        :spacing 5
                                        :padding 0
                                        :draw-border? t ;for debugging
                                        ))
         (text-box (make-instance 'ui-text-box-item :ui-w 400 :text ""))
         (buttons-group (make-instance 'ui-group
                                        :layout :horizontal
                                        :spacing 5
                                        :padding 10
                                        :draw-border? t ;for debugging
                                        ))
         (cancel-button (make-instance 'ui-button-item
                                       :ui-w 80
                                       :text cancel-button-text
                                       :on-click-fn (lambda (modifiers)
                                                      (declare (ignore modifiers))
                                                      (hide-ui-content box))
;;                                                      (setf (is-visible? box) nil))
                                       ))
         (ok-button (make-instance 'ui-button-item
                                   :ui-w 80
                                   :text ok-button-text
                                   :on-click-fn (lambda (modifiers)
                                                  (declare (ignore modifiers))
;;                                                  (setf (is-visible? box) nil)
                                                  (funcall ok-action-fn (text text-box))
                                                  (hide-ui-content box))
                    ))
         )
    (ui-add-child contents-group text-box)
    (ui-add-children buttons-group (list cancel-button ok-button))
    (ui-add-child box (update-layout contents-group))
    (ui-add-child box (update-layout buttons-group))
    (update-layout box)))

#|
(setf (ui-contents *default-scene-view*)
      (list (make-text-input-dialog-box "Save Scene File" (lambda (str) (save-scene *scene* str)))))

(setf (ui-contents *default-scene-view*)
      (list (make-text-input-dialog-box "Open Scene File" (lambda (str) (load-scene str)))))
|#

;;; TODO ++ inspector align left
;;; TODO ++ outliner title with alt hint
;;; TODO xx menubar?
;;; TODO ++ Menu: Scene [New, Open, Save, Quit]
;;;               Edit [Select, Delete, Duplicate, Group, Ungroup, Find]
;;;               Create [...]
;;;               Transform [Translate X, ..., Rotate X, ..., Scale Uniform, Scale X, ...] -- alt for dialogs
;;;               View [Inspect, Shapes, Motions]
;;;               Display [...]
;;;               Contextual [...]
;;; TODO ++ status bar
;;; xxx
;;; TODO -- how to add contextual items?
;;; TODO -- delete shapes in hierarchy
;;; TODO -- update ui-contents due to scene updates -- eg hierarchy viewer when shape deleted
;;;      -- works but not with hierarchy display
;;; TODO ++ inspector sizing issues

;;;; ui-status-bar =============================================================

(defclass-kons-9 ui-status-bar (ui-group)
  ((scene-view nil))
  (:default-initargs
   :is-visible? t
   :bg-color (c! 1 1 1 0.5)))

(defun make-status-bar ()
  (let* ((status-bar (make-instance 'ui-status-bar :ui-x 0 :ui-h (* 2 *ui-button-item-height*))))
    (ui-add-child status-bar (make-instance 'ui-label-item :text "Status Bar Item 1"))
    (ui-add-child status-bar (make-instance 'ui-label-item :text "Status Bar Item 2"))
    (ui-add-child status-bar (make-instance 'ui-label-item :text "Status Bar Item 3"))
    (ui-add-child status-bar (make-instance 'ui-label-item :text "Status Bar Item 4"))
    (ui-add-child status-bar (make-instance 'ui-label-item :text "Status Bar Item 5"))
    status-bar))

(defmethod update-status-bar ((view ui-status-bar) &key (view-width nil) (view-height nil)
                                                     (str0 nil) (str1 nil) (str2 nil) (str3 nil) (str4 nil))
  (when view-height
    (setf (ui-y view) (- view-height (ui-h view))))
  (when view-width
    (setf (ui-w view) view-width)
    (update-layout view))
  (when str0
    (setf (text (aref (children view) 0)) str0))
  (when str1
    (setf (text (aref (children view) 1)) str1))
  (when str2
    (setf (text (aref (children view) 2)) str2))
  (when str3
    (setf (text (aref (children view) 3)) str3))
  (when str4
    (setf (text (aref (children view) 4)) str4))
  view)

;;; custom layout
(defmethod update-layout ((view ui-status-bar))
  (let ((w (* 0.25 (ui-w view)))
        (h (* 0.5  (ui-h view))))
    (ui-set-rect (aref (children view) 0) (* 0 w) 0 w h)
    (ui-set-rect (aref (children view) 1) (* 1 w) 0 w h)
    (ui-set-rect (aref (children view) 2) (* 2 w) 0 w h)
    (ui-set-rect (aref (children view) 3) (* 3 w) 0 w h)
    (ui-set-rect (aref (children view) 4)    0    h (ui-w view) h)
    view))
                        
;;;; ui-popup-menu =============================================================

(defclass-kons-9 ui-popup-menu (ui-group)
  ((command-table nil))
  (:default-initargs
   :is-visible? nil
   :bg-color (c! 1 1 1 0.5)
   :layout :vertical
   :spacing 0
   :padding 0))

(defmethod create-contents ((view ui-popup-menu))
  (setf (fill-pointer (children view)) 0)
  (let ((table (command-table view)))
    (when table
      (setf (title view) (title table))
      (when (> (length (entries table)) 0)
        (let ((width (reduce 'max
                             (map 'vector (lambda (entry)
                                            (+ (ui-text-width (string (help-string entry)))
                                               (ui-text-width (key-binding-string (key-binding entry)))
                                               30
                                               (* 2 *ui-default-spacing*)))
                                  (entries table)))))
          (loop for entry across (entries table)
                do (let ((tmp entry))     ;TODO -- without this all entries get fn of last entry???
                     (vector-push-extend
                      (make-instance 'ui-menu-item :ui-w width
                                                   :ui-h *ui-button-item-height* 
                                                   :text (string (help-string entry))
                                                   :key-text (key-binding-string (key-binding entry))
                                                   :is-active? t
                                                   :help-string (format nil "Mouse: ~a. TAB: hide menu."
                                                                        (string (help-string entry)))
                                                   :on-click-fn (lambda (modifiers)
                                                                  (declare (ignore modifiers))
                                                                  (funcall (command-fn tmp))))
                      (children view))))))))
  (update-layout view))

;;;; TODO xxx
;;++ app table bindings in effect even if menu not visible
;;++ two-line status bar
;;  ++ mouse action, key action
;;-- context menu
;;  -- transform
;;  -- register/generate entries
;;-- register new command tables from plugins
;;  -- procedural-curve
;;  -- uv-mesh
;;  -- heightfield
;;-- auto-generate procedural-curve create and edit dialogs
;;-- application class?
;;-- multiple visible inspectors? esc closes one under mouse?

;;;; draw-view -- :before bg, :after border
;;;; change text slot to caption slot
;;;; make-outliner obj children-fn name-fn
;;;; ui-3d-view (ui-group) -- root-node
;;;; -- scene-view
;;;; app-window -- outliner, 3d-view, inspector/aspect, timeline
;;;; clip to view when drawing
;;;; text-box -- validate-fn
;;;; left-arrow to go to previous inspector
;;;; arrow and Enter menu/command-table navigation
;;;; base command-table -- Display, Inspect Selection, View Selection, Reset Camera, Init Scene, Play Animation, Create

#| DONE
;;;; store parent for ui-view
;;;; ui-message-box -- OK button, text
;;;; -- update-layout
;;;; ui-dialog-box -- Cancel, OK buttons, contents, :fit-children
;;;; -- update-layout
;;;; group title
;;;; ui-sequence-viewer seq -- necessary?
;;;; ui-inspector obj -- vertical group of horizontal groups of slot-name, slot-value
;;;; Display -- Dark Theme, Bright Theme

|#

;;;; ui-sequence-viewer ========================================================

(defclass-kons-9 ui-sequence-viewer (ui-group)
  ((data nil))
  (:default-initargs
;;   :bg-color (c! 0.8 0.8 0.8 0.5)
   :layout :vertical))

(defmethod create-contents ((view ui-sequence-viewer))
  (setf (fill-pointer (children view)) 0)
  (let ((data (data view)))
    (when data
      (loop for entry across (coerce data 'vector)
            do (let ((tmp entry) ; TODO -- necessary otherwise all on-click-fn's use final entry???
                     (text (format nil "~s" entry)))
                 (vector-push-extend
                  (make-instance 'ui-data-item :ui-w (+ (ui-text-width text) (* 4 *ui-default-spacing*))
                                               :ui-h *ui-button-item-height* 
                                               :data entry
                                               :text text
                                               :is-active? t
                                               :on-click-fn (lambda (modifiers)
                                                              (declare (ignore modifiers))
                                                              (show-ui-content (make-ui-inspector tmp))))
                  (children view))))))
  (update-layout view))

#|
(setf (ui-contents *default-scene-view*)
      (list (create-contents (make-instance 'ui-sequence-viewer
                                            :data '(1 2 3 4 5 6 7)))))
|#

;;;; ui-outliner-item ==========================================================

(defun has-children-method? (obj)
  (compute-applicable-methods #'children (list obj)))

(defclass-kons-9 ui-outliner-item (ui-data-item)
  ((show-children? nil)
   (outliner-children '())))

(defmethod initialize-instance :after ((view ui-outliner-item)  &rest initargs)
  (declare (ignore initargs))
  (setf (on-click-fn view) (lambda (modifiers)
                             (cond ((member :alt modifiers)
                                    (when (has-children-method? (data view))
                                      (toggle-show-children view)
                                      (when (ui-parent view)
                                        (update-parent-contents view))))
                                   (t
                                    (toggle-selection (scene (data view))
                                                      (data view))
                                    (setf (bg-color view) (if (is-selected? (data view))
                                                              (c! 0.8 0.2 0.2 0.5)
                                                              (c! 0 0 0 0))))))))
  
(defmethod toggle-show-children ((view ui-outliner-item))
  (setf (show-children? view) (not (show-children? view))))

(defmethod outliner-item-text ((view ui-outliner-item))
  (strcat (if (has-children-method? (data view))
              (if (show-children? view) "- " "+ ")
              "  ")
          (text view)))

(defmethod update-parent-contents ((view ui-outliner-item))
  (when (ui-parent view)
    (if (show-children? view)
        (add-parent-contents view)
        (remove-parent-contents view))
    (update-layout (ui-parent view))))

(defmethod add-parent-contents ((view ui-outliner-item))
  (let ((i (position view (children (ui-parent view)))))
    (loop for child in (children (data view))
          do (let* ((text (format nil "~a" (printable-data child)))
                    (item (make-instance 'ui-outliner-item
                                         :ui-w (+ (ui-text-width text) (* 4 *ui-default-spacing*))
                                         :ui-h *ui-button-item-height*
                                         :bg-color (if (is-selected? child)
                                                       (c! 0.8 0.2 0.2 0.5)
                                                       (c! 0 0 0 0))
                                         :text-padding (+ 20 (text-padding view))
                                         :data child
                                         :text text
                                         :is-active? t
                                         :help-string (format nil "Mouse: select ~a, [ALT] show/hide children"
                                                              (name child)))))
               (setf (text item) (outliner-item-text item))
               (ui-add-child-at (ui-parent view) item (incf i))
               (push item (outliner-children view))))))

(defmethod remove-parent-contents ((view ui-outliner-item))
  (loop for child in (outliner-children view)
        do (remove-parent-contents child)
           (ui-remove-child (ui-parent view) child))
  (setf (outliner-children view) '()))

;;;; ui-outliner-viewer ========================================================

(defclass-kons-9 ui-outliner-viewer (ui-sequence-viewer)
;  ((roots nil))
  ((data-object nil)
   (data-accessor-fn nil))
  (:default-initargs
   :bg-color (c! 1 1 1 0.5)
   :layout :vertical
   :justification :left/top
   :spacing 0
   :padding 0))

(defmethod viewer-data ((view ui-outliner-viewer))
  (if (data-object view)
      (if (data-accessor-fn view)
          (funcall (data-accessor-fn view) (data-object view))
          (data-object view))
      nil))

;; (defmethod create-data ((view ui-outliner-viewer))
;;   (setf (data view) nil)
;;   (dolist (node (roots view))
;;     (create-data-aux view node))
;;   (setf (data view) (reverse (data view))))

;; (defmethod create-data-aux ((view ui-outliner-viewer) node)
;;   (setf (data view) (cons node (data view)))
;;   (when (compute-applicable-methods #'children (list node))
;;     (dolist (child (children node))
;;       (create-data-aux view child))))

;;; TODO -- keep track of show status so we can update intelligently when called from update-scene-ui
;;;      -- otherwise hierarchy display does not work
(defmethod create-contents ((view ui-outliner-viewer))
  (setf (fill-pointer (children view)) 0)
;  (create-data view)
  (let ((data (viewer-data view)))
    (when data
      (loop for entry across (coerce data 'vector)
            do (let* (;(tmp entry) ; TODO -- necessary otherwise all on-click-fn's use final entry???
                      (text (format nil "~a" (printable-data entry)))
                      (item (make-instance 'ui-outliner-item
                                           :ui-h *ui-button-item-height*
                                           :bg-color (if (is-selected? entry)
                                                         (c! 0.8 0.2 0.2 0.5)
                                                         (c! 0 0 0 0))
                                           :data entry
                                           :text text
                                           :is-active? t
                                           :help-string (format nil "Mouse: select ~a, [alt] show/hide children"
                                                                (name entry)))))
                 (setf (text item) (outliner-item-text item))
                 (setf (ui-w item) (+ (ui-text-width (text item)) (* 4 *ui-default-spacing*)))
                 (ui-add-child view item)))))
  (update-layout view)
  (resize-contents view))

(defmethod resize-contents ((view ui-outliner-viewer))
  (when (eq :vertical (layout view))
    (let ((width (ui-w view))
          (x (padding view)))
      (loop for child across (children view)
            do (setf (ui-x child) x)
            do (setf (ui-w child) width))))
  view)

(defun make-ui-outliner-viewer (title obj &optional (accessor-fn nil))
  (create-contents (make-instance 'ui-outliner-viewer
                                  :ui-x 20 :ui-y 20 :title title :data-object obj :data-accessor-fn accessor-fn)))

;; (defun show-ui-outliner-viewer (roots)
;;   (setf (ui-contents *default-scene-view*)
;;         (list (create-contents (make-instance 'ui-outliner-viewer :ui-x 20 :ui-y 20 :roots roots)))))

;; (defun show-ui-shape-hierarchy (scene)
;;   (show-ui-outliner-viewer (shapes scene)))

;; (defun show-ui-motion-hierarchy (scene)
;;   (show-ui-outliner-viewer (motions scene)))

#|
(setf (ui-contents *default-scene-view*)
      (list (create-contents (make-instance 'ui-sequence-viewer
                                            :data '(1 2 3 4 5 6 7)))))
|#

;;;; ui-inspector ==============================================================

(defclass-kons-9 ui-inspector (ui-sequence-viewer)
  ((obj nil))
  (:default-initargs
   :bg-color (c! 1 1 1 0.5)
   :layout :vertical
   :justification :left/top
   :spacing 0
   :padding 0))

(defun ui-cleanup-inspector-string (text)
  (string-trim-to-length (remove-extra-spaces text) 60))

;;;; TODO -- truncate long entries using "..."
;;;; TODO -- limit size of inspector to window
;;;; TODO -- SBCL inspect limits display of sequences to 10 items

(defmethod create-contents ((view ui-inspector))
  (setf (fill-pointer (children view)) 0)
  (multiple-value-bind (description named-p elements)
      (sb-impl::inspected-parts (obj view))
    (setf (title view) (ui-cleanup-inspector-string description))
    (setf (data view) (if (typep elements 'sequence)
                          (coerce elements 'vector)
                          (vector (obj view))))
    (loop for entry across (data view)
          for i from 0
          ;; TODO -- tmp necessary otherwise all on-click-fn's use final entry???
          do (let* ((tmp entry)
                    (text (ui-cleanup-inspector-string
                           (cond ((and named-p
                                       (typep (cdr tmp) 'sequence)
                                       (> (length (cdr tmp)) 1)
                                       (not (typep (cdr tmp) '(simple-array single-float (3))))) ;POINT
                                  (format nil "~a: ~s..." (car tmp) (elt (cdr tmp) 0)))
                                 (named-p
                                  (format nil "~a: ~s" (car tmp) (cdr tmp)))
                                 ((> (length (data view)) 1)
                                  (format nil "~a: ~s" i tmp))
                                 (t
                                  (format nil "~s" tmp)))))
                    (datum (if named-p (cdr tmp) tmp)))

;;;               (print (list (type-of (cdr tmp)) (format nil "~s" (cdr tmp))))

               (vector-push-extend
                (make-instance 'ui-data-item :ui-w (+ (ui-text-width text) (* 4 *ui-default-spacing*))
                                             :ui-h *ui-button-item-height* 
                                             :data datum
                                             :text text
                                             :is-active? t
                                             :help-string (format nil "Mouse: inspect ~a. UP/DOWN ARROW: scroll inspector. ESC: close inspector."
                                                                  (ui-cleanup-inspector-string
                                                                   (format nil "~a" datum)))
                                             :on-click-fn (lambda (modifiers)
                                                            (declare (ignore modifiers))
                                                            (show-ui-content (make-ui-inspector (if named-p (cdr tmp) tmp)))))
                (children view))))
    (update-layout view)))

(defun make-ui-inspector (obj)
  (create-contents (make-instance 'ui-inspector :ui-x 20 :ui-y 20 :obj obj)))

#|
(show-ui-inspector *scene*)

(show-ui-inspector 42)

(show-ui-inspector (p! 1 2 3))

(show-ui-inspector :zzz)

;;;; ui-inspector
(defun ui-inspector-contents (obj)
  (multiple-value-bind (description named-p elements)
      (sb-impl::inspected-parts obj)
;;    (sb-impl::tty-display-inspected-parts description named-p elements t)
    (print elements)))

(ui-inspector-contents 42)
(ui-inspector-contents 'x)
(ui-inspector-contents (p! 1 2 3))
(ui-inspector-contents (first (shapes *scene*)))
|#




;;;; drawing -------------------------

(defun draw-rect-fill (x y w h &optional (inset 0.0))
  (gl:polygon-mode :front-and-back :fill)
  (gl:begin :polygon)
  (gl:vertex    (+ x inset)       (+ y inset))
  (gl:vertex (- (+ x w) inset)    (+ y inset))
  (gl:vertex (- (+ x w) inset) (- (+ y h) inset))
  (gl:vertex    (+ x inset)    (- (+ y h) inset))
  (gl:end))

;; draw as lines so corners look clean
(defun draw-rect-border (x y w h &optional (inset 0.0) (line-width *ui-border-width*))
  (gl:line-width line-width)
  (gl:begin :lines)
  (gl:vertex       x             (+ y inset))
  (gl:vertex    (+ x w)           (+ y inset))
  (gl:vertex       x           (- (+ y h) inset))
  (gl:vertex    (+ x w)        (- (+ y h) inset))
  (gl:vertex (- (+ x w) inset)    (+ y inset))
  (gl:vertex (- (+ x w) inset) (- (+ y h) inset))
  (gl:vertex    (+ x inset)       (+ y inset))
  (gl:vertex    (+ x inset)    (- (+ y h) inset))
  (gl:end))

;; draw as lines so corners look clean
(defun draw-rect-border-SAV (x y w h &optional (inset 0.0) (line-width *ui-border-width*))
  (gl:polygon-mode :front-and-back :line)
  (gl:line-width line-width)
  (gl:begin :polygon)
  (gl:vertex    (+ x inset)       (+ y inset))
  (gl:vertex (- (+ x w) inset)    (+ y inset))
  (gl:vertex (- (+ x w) inset) (- (+ y h) inset))
  (gl:vertex    (+ x inset)    (- (+ y h) inset))
  (gl:end))

(defun draw-rect-x-mark (x y w h &optional (inset 0.0) (line-width *ui-border-width*))
  (gl:line-width line-width)
  (gl:begin :lines)
  (gl:vertex    (+ x inset)       (+ y inset))
  (gl:vertex (- (+ x w) inset) (- (+ y h) inset))
  (gl:vertex    (+ x inset)    (- (+ y h) inset))
  (gl:vertex (- (+ x w) inset)    (+ y inset))
  (gl:end))

(defun draw-cursor (x y)
  (gl:line-width *ui-border-width*)
  (gl:begin :lines)
  (let ((x0 (- x 3))
        (x1 (+ x 2))
        (y0 (+ y 3))
        (y1 (- (+ y *ui-button-item-height*) 4)))
    (gl:vertex x y0)
    (gl:vertex x y1)
    (gl:vertex x0 y0)
    (gl:vertex x1 y0)
    (gl:vertex x0 y1)
    (gl:vertex x1 y1)
    (gl:end)))

(defmethod draw-title-bar ((view ui-group) &optional x-offset y-offset)
  (gl:color 0.4 0.4 0.4 0.8)
  (with-accessors ((fg fg-color) (x ui-x) (y ui-y) (w ui-w))
      view
    ;; fill
    (draw-rect-fill (+ x x-offset) (+ y y-offset) w *ui-button-item-height*)
    ;; border
    (gl:color (c-red fg) (c-green fg) (c-blue fg) (c-alpha fg))
    (draw-rect-border (+ x x-offset) (+ y y-offset) w *ui-button-item-height*)
    ;; title
    (render-text (+ (ui-centered-text-x (title view) w) x x-offset)
                 (+ 16 y y-offset) (title view) :color #xffffffff)))

  
(defmethod draw-ui-view ((view ui-view) &optional x-offset y-offset)
  (with-accessors ((bg bg-color) (fg fg-color) (x ui-x) (y ui-y) (w ui-w) (h ui-h))
      view
    ;; fill
    (when (> (c-alpha bg) 0)
      (gl:color (c-red bg) (c-green bg) (c-blue bg) (c-alpha bg))
      (draw-rect-fill (+ x x-offset) (+ y y-offset) w h))
    ;; border
    (when (or (draw-border? view) (highlight? view)) ;draw border if highlighted
      (let ((line-width (if (highlight? view) *ui-highlight-border-width* *ui-border-width*)))
        (gl:color (c-red fg) (c-green fg) (c-blue fg) (c-alpha fg))
        (draw-rect-border (+ x x-offset) (+ y y-offset) w h
                          (if (> line-width 1) (* 0.5 line-width) 0)
                          line-width)))))
  
(defgeneric draw-view (view &optional x-offset y-offset)

  (:method ((view ui-view) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)))
  
  (:method ((view ui-label-item) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (render-text (+ (text-padding view) x x-offset) (+ 16 y y-offset) (text view)))))

  (:method ((view ui-outliner-item) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (render-text (+ (text-padding view) x x-offset) (+ 16 y y-offset) (text view)))))

  (:method ((view ui-button-item) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (render-text (+ 5 x x-offset) (+ 16 y y-offset) (key-text view))
        (render-text (+ (ui-centered-text-x (text view) (ui-w view)) x x-offset)
                     (+ 16 y y-offset) (text view)))))

  (:method ((view ui-menu-item) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (render-text (+ 5 x x-offset) (+ 16 y y-offset) (key-text view))
        (render-text (+ 30 x x-offset) (+ 16 y y-offset) (text view)))))

  (:method ((view ui-check-box-item) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y) (cbg check-bg-color) (fg fg-color))
          view
        (render-text (+ 30 x x-offset) (+ 16 y y-offset) (text view))
        (when (> (c-alpha cbg) 0)
          (gl:color (c-red cbg) (c-green cbg) (c-blue cbg) (c-alpha cbg))
          (draw-rect-fill (+ x x-offset 4) (+ y y-offset 4)
                          (- *ui-button-item-height* 8) (- *ui-button-item-height* 8)))
        (gl:color (c-red fg) (c-green fg) (c-blue fg) (c-alpha fg))
        (draw-rect-border (+ x x-offset 4) (+ y y-offset 4)
                          (- *ui-button-item-height* 8) (- *ui-button-item-height* 8))
        (when (is-pushed? view)
          (draw-rect-x-mark (+ x x-offset 4) (+ y y-offset 4)
                            (- *ui-button-item-height* 9) (- *ui-button-item-height* 8)
                            0 (* 2 *ui-border-width*))))))

  (:method ((view ui-text-box-item) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (let ((local-x (+ 5 x x-offset))
              (local-y (+ y y-offset)))
          (render-text local-x (+ 16 local-y) (text view))
          (when (eq view *ui-keyboard-focus*)
            (draw-cursor (+ local-x (* *ui-font-width* (cursor-position view))) local-y))))))

  (:method :after ((view ui-group) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (when (title view)
        (draw-title-bar view x-offset y-offset))
      (loop for child across (children view)
            do (draw-view child (+ (ui-x view) x-offset) (+ (ui-y view) y-offset)))))
  )

;;;; hit testing -------------------------

(defun ui-point-in-rect? (px py rx ry rw rh)
  (and (> px rx) (> py ry) (< px (+ rx rw)) (< py (+ ry rh))))

(defgeneric find-ui-at-point (view global-x global-y &optional x-offset y-offset)

  (:method ((view ui-rect) global-x global-y &optional (x-offset 0) (y-offset 0))
    (if (is-visible? view)
        (with-accessors ((x ui-x) (y ui-y) (w ui-w) (h ui-h))
            view
          (let ((local-x (- global-x x-offset))
                (local-y (- global-y y-offset)))
            (if (ui-point-in-rect? local-x local-y x y w h)
                view
                nil)))
        nil))
  
  (:method ((view ui-group) global-x global-y &optional (x-offset 0) (y-offset 0))
    (cond ((call-next-method)           ;is in view
           (loop for child across (children view)
                 do (let ((found (find-ui-at-point child global-x global-y
                                                   (+ (ui-x view) x-offset) (+ (ui-y view) y-offset))))
                      (when found
                        (return-from find-ui-at-point found))))
             view)
          (t nil)))
  )

;;;; test ====================

;; (defun display-ui (view)
;;   (let ((menu (make-instance 'ui-popup-menu :x 20 :y 20 :command-table (car (command-tables view)))))
;;     (update-layout menu)
;;     (draw-view menu)))

  ;; (draw-view (make-instance 'ui-view :x 400 :y 300 :w *ui-button-item-width* :h *ui-button-item-height*))
  ;; (draw-view (make-instance 'ui-view :x 400 :y 200 :w *ui-button-item-width* :h *ui-button-item-height*
  ;;                          :highlight? t)))
