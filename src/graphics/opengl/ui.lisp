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
(defparameter *ui-font-width* 7)


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

(defmethod ui-set-rect ((rect ui-rect) x y w h)
  (setf (ui-x rect) x)
  (setf (ui-y rect) y)
  (setf (ui-w rect) w)
  (setf (ui-h rect) h)
  rect)
  
;;;; ui-view ===================================================================

;;; TODO -- use *drawing-settings* fg and bg colors
;;; TODO -- pass modifiers and mouse button info to on-click-fn
(defclass-kons-9 ui-view (ui-rect)
  ((ui-parent nil)
   (bg-color (c! 0 0 0 0))
   (fg-color (c! 0 0 0 1))
   (is-visible? t)
   (draw-border? t)
   (is-active? nil)
   (on-click-fn nil)
   (highlight? nil)
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
         
(defmethod do-action ((view ui-view) x y)
  (declare (ignore x y))
  (when (on-click-fn view)
    (funcall (on-click-fn view))))

;;;; ui-label-item =============================================================

(defclass-kons-9 ui-label-item (ui-view)
  ((text "Label")
   (text-padding 10))
  (:default-initargs
   :draw-border? nil))

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
   :is-active? t))

;;;; ui-check-box-item =========================================================

(defclass-kons-9 ui-check-box-item (ui-label-item)
  ((is-pushed? nil))
  (:default-initargs
   :draw-border? nil
   :is-active? t))

(defmethod do-action :before ((view ui-check-box-item) x y)
  (declare (ignore x y))
  (setf (is-pushed? view) (not (is-pushed? view))))

;;;; ui-text-box-item ==========================================================

(defclass-kons-9 ui-text-box-item (ui-label-item)
  ((cursor-position 0))
  (:default-initargs
   :draw-border? t
   :is-active? t
   :can-have-keyboard-focus? t))

(defmethod do-action ((view ui-text-box-item) x y)
  (setf *ui-keyboard-focus* view)
  (setf (cursor-position view) (max 0
                                    (min (length (text view))
                                         (floor (/ (first (local-coords view x y)) *ui-font-width*))))))

;;; TODO -- edit text...
(defmethod do-key-input ((view ui-text-box-item) key mod-keys)
  (print (list key mod-keys)))

;;;; ui-menu-item ==============================================================

(defclass-kons-9 ui-menu-item (ui-button-item)
  ())

;;;; ui-group ==============================================================

(defclass-kons-9 ui-group (ui-view)
  ((layout :fit-children) ; :fit-children, :vertical, :horizontal
   (spacing *ui-default-spacing*)
   (padding *ui-default-padding*)
   (title nil)
   (children (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod ui-add-child ((view ui-group) (child ui-view))
  (setf (ui-parent child) view)
  (vector-push-extend child (children view))
  child)

;;; TODO -- move to utils.lisp
(defun insert-into-array (vector value position)
  (vector-push-extend value vector) ; ensure that the array is large enough
  ;; shift the end of the array right
  (loop for i from (1- (length vector)) downto (1+ position) do
      (setf (aref vector i) (aref vector (1- i))))
  (setf (aref vector position) value) ; insert value into the right place
  vector)

(defmethod ui-add-child-at ((view ui-group) (child ui-view) index)
  (setf (ui-parent child) view)
  (insert-into-array (children view) child index)
  child)

(defmethod ui-remove-child ((view ui-group) (child ui-view))
  (setf (ui-parent child) nil)
  (delete child (children view))
  child)

(defmethod update-layout ((view ui-group))
  (ecase (layout view)
    (:fit-children (update-layout-fit-children view))
    (:vertical (update-layout-vertical view))
    (:horizontal (update-layout-horizontal view)))
  view)

(defmethod update-layout-fit-children ((view ui-group))
  (let ((children (children view)))
    (if (= (length children) 0)
        (progn
          (setf (ui-w view) (padding view))
          (setf (ui-h view) (padding view)))
        (let* ((x (reduce 'min children :key 'ui-x))
               (y (reduce 'min children :key 'ui-y))
               (w (reduce 'max (map 'vector (lambda (child) (+ (ui-x child) (ui-w child))) children)))
               (h (reduce 'max (map 'vector (lambda (child) (+ (ui-y child) (ui-h child))) children)))
               (dx (- x (padding view)))
               (dy (- y (padding view))))
          (setf (ui-w view) (+ dx w (padding view)))
          (setf (ui-h view) (+ dy h (padding view)))
          (loop for child across children
                do (incf (ui-x child) dx)
                   (incf (ui-y child) dy))))))

(defmethod update-layout-horizontal ((view ui-group))
  ;; TODO
  )

(defmethod update-layout-vertical ((view ui-group))
  (let ((children (children view)))
    (if (= (length children) 0)
        (progn
          (setf (ui-w view) (padding view))
          (setf (ui-h view) (padding view)))
        (let* ((title-w (if (title view) (+ (ui-text-width (title view)) *ui-default-padding*) 0))
               (title-h (if (title view) *ui-button-item-height* 0))
               (width (max (+ (reduce 'max children :key 'ui-w)
                              (* (padding view) 2))
                           title-w))
               (height (+ (reduce '+ children :key 'ui-h)
                          (* (spacing view) (1- (length children)))
                          (* (padding view) 2)
                          title-h))
               (y (+ (padding view) title-h)))
          (setf (ui-w view) width)
          (setf (ui-h view) height)
          (loop for child across children
                do (setf (ui-x child) (padding view))
                   (setf (ui-y child) y)
                   (incf y (+ (ui-h child) (spacing view)))))))
  view)

;;;; ui-message-box ===========================================================

(defclass-kons-9 ui-message-box (ui-group)
  ()
  (:default-initargs
   :is-visible? t
   :bg-color (c! 1 1 1 0.8)
   :layout :fit-children))

;;; TODO -- center pane in parent view
(defun make-message-box (message-text &optional (ok-button-text "OK"))
  (let* ((message-text-width (ui-text-width message-text))
         (width (+ message-text-width (* 2 *ui-default-spacing*)))
         (height (+ (* 2 *ui-button-item-height*) (* 3 *ui-default-spacing*)))
         (ok-button-text-width (ui-text-width ok-button-text))
         (ok-button-width (max 80 (+ ok-button-text-width (* 2 *ui-default-spacing*))))
         (box (make-instance 'ui-message-box
                             :ui-x 200
                             :ui-y 200
                             :ui-w (max width ok-button-width)
                             :ui-h height)))
    (ui-add-child box (make-instance 'ui-label-item
                                      :ui-x *ui-default-spacing*
                                      :ui-y *ui-default-spacing*
                                      :ui-w message-text-width
                                      :ui-h *ui-button-item-height*
                                      :text message-text
                                      :draw-border? nil
                                      :text-padding 0
                                      ))
    (ui-add-child box (make-instance 'ui-button-item
                                      :ui-x (floor (* 0.5 (- (ui-w box) ok-button-width)))
                                      :ui-y (+ *ui-button-item-height* (* 2 *ui-default-spacing*))
                                      :ui-w ok-button-width
                                      :ui-h *ui-button-item-height*
                                      :text ok-button-text
                                      :on-click-fn (lambda () (setf (is-visible? box) nil))
                                      ))
    box))

#|
(setf (ui-contents *default-scene-view*)
      (list (make-message-box "Hello, kons-9! Doing 3D graphics in Common Lisp.")))
|#

;;;; ui-dialog-box ===========================================================

(defclass-kons-9 ui-dialog-box (ui-group)
  ()
  (:default-initargs
   :is-visible? t
   :bg-color (c! 1 1 1 0.8)
   :layout :fit-children))

;;; TODO -- center pane in parent view
;;; TODO -- make vertical group from contents ?
;;; TODO -- adjust pane size to fit contents
;;; TODO -- place buttons intelligently
;;; TODO -- specify OK function
(defun make-dialog-box (contents &optional (ok-button-text "OK") (cancel-button-text "Cancel"))
  (let ((box (make-instance 'ui-dialog-box
                            :ui-x 200
                            :ui-y 200
                            :ui-w 500
                            :ui-h 300
                            :layout :fit-children))
        (contents-group (make-instance 'ui-group
                                       :ui-x 20
                                       :ui-y 20
                                       :ui-w 460
                                       :ui-h 220
                                       :layout :vertical
                                       :spacing 5
                                       :padding 0
                                       :draw-border? t ;for debugging
                                       )))
    (dolist (view contents)
      (ui-add-child contents-group view))
    (ui-add-child box (update-layout contents-group))
    (ui-add-child box (make-instance 'ui-button-item
                                      :ui-x 250
                                      :ui-y 260
                                      :ui-w 80
                                      :ui-h *ui-button-item-height*
                                      :text cancel-button-text
                                      :on-click-fn (lambda () (setf (is-visible? box) nil))
                                      ))
    (ui-add-child box (make-instance 'ui-button-item
                                      :ui-x 350
                                      :ui-y 260
                                      :ui-w 80
                                      :ui-h *ui-button-item-height*
                                      :text ok-button-text
                                      :on-click-fn (lambda () (setf (is-visible? box) nil))
                                      ))
    (update-layout box)))

#|
(setf (ui-contents *default-scene-view*)
      (list (make-dialog-box (list (make-instance 'ui-label-item :ui-w *ui-popup-menu-width*
                                                                 :ui-h *ui-button-item-height*
                                                                 :text "Label 1")
                                   (make-instance 'ui-label-item :ui-w *ui-popup-menu-width*
                                                                 :ui-h *ui-button-item-height*
                                                                 :text "Label 2")
                                   (make-instance 'ui-button-item :ui-w *ui-popup-menu-width*
                                                                  :ui-h *ui-button-item-height*
                                                                  :text "My Button")
                                   (make-instance 'ui-check-box-item :ui-w *ui-popup-menu-width*
                                                                     :ui-h *ui-button-item-height*
                                                                     :text "Check Box")
                                   (make-instance 'ui-text-box-item :ui-w *ui-popup-menu-width*
                                                                    :ui-h *ui-button-item-height*
                                                                    :text "Text Box")
                                   ))))
|#

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
      (let ((width (reduce 'max
                           (map 'vector (lambda (entry)
                                          (+ (ui-text-width (string (help-string entry)))
                                             (ui-text-width (key-binding-string (key-binding entry)))
                                             30
                                             (* 2 *ui-default-spacing*)))
                                (entries table)))))
        (loop for entry across (entries table)
              do (vector-push-extend
                  (make-instance 'ui-menu-item :ui-w width
                                               :ui-h *ui-button-item-height* 
                                               :text (string (help-string entry))
                                               :key-text (key-binding-string (key-binding entry))
                                               :is-active? t
                                               :on-click-fn (command-fn entry))
                  (children view))))))
  (update-layout view))

;;;; TODO
;;;; draw-view -- :before bg, :after border
;;;; change text slot to caption slot
;;;; make-outliner obj children-fn
;;;; ui-3d-view (ui-group) -- root-node
;;;; -- scene-view
;;;; app-window -- outliner, 3d-view, inspector/aspect, timeline
;;;; clip to view when drawing
;;;; text-box -- validate-fn
;;;; left-arrow to go to previous inspector
;;;; arrow and Enter menu/command-table navigation
;;;; base command-table -- Display, Inspect Selection, View Selection, Reset Camera, Init Scene, Play Animation, Create
;;;; -- Display -- Dark Theme, Bright Theme

#| DONE
;;;; store parent for ui-view
;;;; ui-message-box -- OK button, text
;;;; -- update-layout
;;;; ui-dialog-box -- Cancel, OK buttons, contents, :fit-children
;;;; -- update-layout
;;;; group title
;;;; ui-sequence-viewer seq -- necessary?
;;;; ui-inspector obj -- vertical group of horizontal groups of slot-name, slot-value

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
                                               :on-click-fn (lambda () (show-ui-inspector tmp)))
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
  (setf (on-click-fn view) (lambda ()
                             ;; TODO -- option click to select -- pass x,y, key-mods
                             (when (has-children-method? (data view))
                               (toggle-show-children view)
                               (when (ui-parent view)
                                 (update-parent-contents view))))))
  
(defmethod toggle-show-children ((view ui-outliner-item))
  (setf (show-children? view) (not (show-children? view))))

(defmethod effective-text ((view ui-outliner-item))
  (strcat (if (has-children-method? (data view))
              (if (show-children? view) "- " "+ ")
              "  ")
          (text view)))

;;;; ui-outliner-viewer ========================================================

(defclass-kons-9 ui-outliner-viewer (ui-sequence-viewer)
  ((roots nil))
  (:default-initargs
   :bg-color (c! 1 1 1 0.5)
   :layout :vertical
   :spacing 0
   :padding 0))

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

(defmethod update-parent-contents ((view ui-outliner-item))
  (when (ui-parent view)
    (if (show-children? view)
        (add-parent-contents view)
        (remove-parent-contents view))
    (update-layout (ui-parent view))))

(defmethod add-parent-contents ((view ui-outliner-item))
  (let ((i (position view (children (ui-parent view)))))
    (loop for child in (children (data view))
          do (let* ((text (format nil "~s" child))
                    (item (make-instance 'ui-outliner-item
                                         :ui-w (+ (ui-text-width text) (* 4 *ui-default-spacing*))
                                         :ui-h *ui-button-item-height*
                                         :bg-color (if (is-selected? child)
                                                       (c! 0.8 0.2 0.2 0.5)
                                                       (c! 0 0 0 0))
                                         :text-padding (+ 20 (text-padding view))
                                         :data child
                                         :text text
                                         :is-active? t)))
               (ui-add-child-at (ui-parent view) item (incf i))
               (push item (outliner-children view))))))

(defmethod remove-parent-contents ((view ui-outliner-item))
  (loop for child in (outliner-children view)
        do (remove-parent-contents child)
           (ui-remove-child (ui-parent view) child))
  (setf (outliner-children view) '()))

(defmethod create-contents ((view ui-outliner-viewer))
  (setf (fill-pointer (children view)) 0)
;  (create-data view)
  (let ((data (roots view)))
    (when data
      (loop for entry across (coerce data 'vector)
            do (let ((tmp entry) ; TODO -- necessary otherwise all on-click-fn's use final entry???
                     (text (format nil "~s" entry)))
                 (ui-add-child view
                               (make-instance 'ui-outliner-item
                                              :ui-w (+ (ui-text-width text) (* 4 *ui-default-spacing*))
                                              :ui-h *ui-button-item-height*
                                              :bg-color (if (is-selected? entry)
                                                            (c! 0.8 0.2 0.2 0.5)
                                                            (c! 0 0 0 0))
                                              :data entry
                                              :text text
                                              :is-active? t
                                              ))))))
                                              ;; :on-click-fn (lambda ()
                                              ;;                (toggle-selection (scene *default-scene-view*)
                                              ;;                                  tmp)
                                              ;;                (update-state view))))))))
  (update-layout view))

(defun show-ui-outliner-viewer (roots)
  (setf (ui-contents *default-scene-view*)
        (list (create-contents (make-instance 'ui-outliner-viewer :ui-x 20 :ui-y 20 :roots roots)))))

(defun show-ui-shape-hierarchy (scene)
  (show-ui-outliner-viewer (shapes scene)))

(defun show-ui-motion-hierarchy (scene)
  (show-ui-outliner-viewer (motions scene)))

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
   :spacing 0
   :padding 0))

(defun ui-cleanup-inspector-description (text)
  (string-trim '(#\space) (substitute #\space #\newline text)))

;;;; TODO -- truncate long entries using "..."
;;;; TODO -- limit size of inspector to window
;;;; TODO -- SBCL inspect limits display of sequences to 10 items

(defmethod create-contents ((view ui-inspector))
  (multiple-value-bind (description named-p elements)
      (sb-impl::inspected-parts (obj view))
    (setf (title view) (ui-cleanup-inspector-description description))
    (setf (data view) (if (typep elements 'sequence)
                          (coerce elements 'vector)
                          (vector (obj view))))
    (loop for entry across (data view)
          for i from 0
          ;; TODO -- tmp necessary otherwise all on-click-fn's use final entry???
          do (let* ((tmp entry)
                    (text (cond ((and named-p
                                      (typep (cdr tmp) 'sequence)
                                      (> (length (cdr tmp)) 1)
                                      (not (typep (cdr tmp) 'point)))
                                 (format nil "~a: ~s..." (car tmp) (elt (cdr tmp) 0)))
                                (named-p
                                 (format nil "~a: ~s" (car tmp) (cdr tmp)))
                                ((> (length (data view)) 1)
                                 (format nil "~a: ~s" i tmp))
                                (t
                                 (format nil "~s" tmp)))))

;;;               (print (list (type-of (cdr tmp)) (format nil "~s" (cdr tmp))))
               
               (vector-push-extend
                (make-instance 'ui-data-item :ui-w (+ (ui-text-width text) (* 4 *ui-default-spacing*))
                                             :ui-h *ui-button-item-height* 
                                             :data (if named-p (cdr tmp) tmp)
                                             :text text
                                             :is-active? t
                                             :on-click-fn (lambda ()
                                                            (show-ui-inspector (if named-p (cdr tmp) tmp))))
                (children view))))
    (update-layout view)))

(defun show-ui-inspector (obj)
  (setf (ui-contents *default-scene-view*)
        (list (create-contents (make-instance 'ui-inspector :ui-x 20 :ui-y 20 :obj obj)))))

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
  (gl:color 0.8 0.8 0.8 0.75)
  (with-accessors ((fg fg-color) (x ui-x) (y ui-y) (w ui-w))
      view
    ;; fill
    (draw-rect-fill (+ x x-offset) (+ y y-offset) w *ui-button-item-height*)
    ;; border
        (gl:color (c-red fg) (c-green fg) (c-blue fg) (c-alpha fg))
    (draw-rect-border (+ x x-offset) (+ y y-offset) w *ui-button-item-height*)
    ;; title
    (render-text (+ (ui-centered-text-x (title view) w) x x-offset)
                 (+ 16 y y-offset) (title view) :color #x000000ff)))

  
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
        (render-text (+ (text-padding view) x x-offset) (+ 16 y y-offset) (text view) :color #x000000ff))))

  (:method ((view ui-outliner-item) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (render-text (+ (text-padding view) x x-offset) (+ 16 y y-offset) (effective-text view) :color #x000000ff))))

  (:method ((view ui-button-item) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (render-text (+ 5 x x-offset) (+ 16 y y-offset) (key-text view) :color #x000000ff)
        (render-text (+ (ui-centered-text-x (text view) (ui-w view)) x x-offset)
                     (+ 16 y y-offset) (text view) :color #x000000ff))))

  (:method ((view ui-menu-item) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (render-text (+ 5 x x-offset) (+ 16 y y-offset) (key-text view) :color #x000000ff)
        (render-text (+ 30 x x-offset) (+ 16 y y-offset) (text view) :color #x000000ff))))

  (:method ((view ui-check-box-item) &optional (x-offset 0) (y-offset 0))
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (render-text (+ 30 x x-offset) (+ 16 y y-offset) (text view) :color #x000000ff)
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
          (render-text local-x (+ 16 local-y) (text view) :color #x000000ff)
          (draw-cursor (+ local-x (* *ui-font-width* (cursor-position view))) local-y)))))

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
