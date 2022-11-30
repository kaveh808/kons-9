(in-package #:kons-9)

;;;; ui globals ================================================================

(defparameter *ui-clip-rect* nil)

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
(defparameter *ui-font-height* 16)

;;;; utils ====================================================================

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

(defmethod print-object ((self ui-rect) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "X: ~a, Y: ~a, W: ~a, H: ~a" (ui-x self) (ui-y self) (ui-w self) (ui-h self))))

(defmethod ui-right ((rect ui-rect))
  (+ (ui-x rect) (ui-w rect)))

(defmethod ui-bottom ((rect ui-rect))
  (+ (ui-y rect) (ui-h rect)))

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
  ((ui-name nil)
   (ui-parent nil)
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

(defmethod do-drag-action ((view ui-view) x y dx dy)
  (declare (ignore x y dx dy))
  ;; do nothing
  )

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

;;;; ui-choice-button ==========================================================

(defclass-kons-9 ui-choice-button (ui-label-item)
  (;(choice-index 0)
   (choices (make-array 0 :adjustable t :fill-pointer 0))
   (is-popped-up? nil)
   (choice-menu nil))
  (:default-initargs
   :draw-border? t ;nil
   :is-active? t))

(defmethod initialize-instance :after ((view ui-choice-button)  &rest initargs)
  (declare (ignore initargs))
  ;; assumes choices have been set
  (set-width-for-text view)
  (setf (choice-menu view) (make-instance 'ui-choice-menu :choice-button view))
  (create-contents (choice-menu view)))

(defmethod set-width-for-text ((view ui-choice-button))
  (setf (ui-w view) (max 80
                         (if (> (length (choices view)) 0)
                             (+ (reduce #'max (map 'vector #'ui-text-width (choices view)))
                                (* 2 (text-padding view)))
                             0))))

(defmethod do-action ((view ui-choice-button) x y button modifiers)
  (declare (ignore x y button modifiers))
  (setf (is-visible? (choice-menu view)) t))

;;;; ui-text-box-item ==========================================================

(defclass-kons-9 ui-text-box-item (ui-label-item)
  ((cursor-position 0)
   (mark-position 0))
  (:default-initargs
   :draw-border? t
   :bg-color (c! 1 1 1 1)
   :is-active? t
   :can-have-keyboard-focus? t))

(defmethod coord-to-text-pos ((view ui-text-box-item) x y)
  (max 0
       (min (length (text view))
            (floor (/ (first (local-coords view x y)) *ui-font-width*)))))

(defmethod do-action ((view ui-text-box-item) x y button modifiers)
  (declare (ignore button))
  (setf *ui-keyboard-focus* view)
  (let ((pos (coord-to-text-pos view x y)))
    (if (member :shift modifiers)       ;shift-click to set cursor only
        (setf (mark-position view) pos)
        (progn                          ;click sets both mark and cursor
          (setf (mark-position view) pos)
          (setf (cursor-position view) pos)))))

(defmethod do-drag-action ((view ui-text-box-item) x y dx dy)
  (declare (ignore dx dy))
  (let ((pos (coord-to-text-pos view x y)))
    (setf (cursor-position view) pos)))

;;; TODO ++ edit text
;;; TODO ++ handle char input properly
;;; TODO ++ do not insert modifier key text
;;; TODO ++ draw cursor when is *ui-keyboard-focus*
;;; TODO ++ arrow keys
;;; TODO ++ mark region
;;; + shift-click
;;; + drag
;;; - double click -- not handled by glfw?
;;; + alt A (select all)
;;; TODO ++ emacs bindings -- C-a, C-e, C-d, C-k, C-y
;;; TODO -- handle text that is wider than text-box -- clip text to widget?

(defun insert-string (string insertion position)
  (concatenate 'string
               (subseq string 0 position)
               insertion
               (if (< position (length string))
                   (subseq string position (length string))
                   "")))
  
(defun remove-string (string position-0 position-1)
  (let ((p0 (min position-0 position-1))
        (p1 (max position-0 position-1)))
    (concatenate 'string
                 (subseq string 0 p0)
                 (subseq string p1 (length string)))))

(defmethod remove-selected-text ((view ui-text-box-item))
  (let ((pos (cursor-position view))
        (mark (mark-position view)))
    (when (not (= pos mark))            ;text selected
      (let ((p0 (min pos mark)))
        (setf (text view) (remove-string (text view) pos mark))
        (setf (cursor-position view) p0)
        (setf (mark-position view) p0)))))
  
(defmethod do-char-input ((view ui-text-box-item) char)
  ;; delete selected text before insertion
  (remove-selected-text view)
  ;; insert text
  (setf (text view) (insert-string (text view) (string char) (cursor-position view)))
  (incf (cursor-position view))
  (setf (mark-position view) (cursor-position view)))

(defmethod do-paste-input ((view ui-text-box-item) string)
  (setf (text view) (insert-string (text view) string (cursor-position view)))
  (incf (cursor-position view) (length string))
  ;; update mark
  (setf (mark-position view) (cursor-position view)))

(defmethod do-copy-input ((view ui-text-box-item))
  (let ((pos (cursor-position view))
        (mark (mark-position view)))
    (if (not (= pos mark))            ;text selected
      (let ((p0 (min pos mark))
            (p1 (max pos mark)))
        (subseq (text view) p0 p1))
      "")))                             ;no text selection

(defmethod do-cut-input ((view ui-text-box-item))
  (let ((result (do-copy-input view)))
    (remove-selected-text view)
    result))

(defmethod do-select-all ((view ui-text-box-item))
  (setf (mark-position view) 0)
  (setf (cursor-position view) (length (text view))))

(defmethod do-backspace-input ((view ui-text-box-item) mod-keys)
  (let ((pos (cursor-position view))
        (mark (mark-position view)))
    (cond ((= pos mark)                 ;no text selection
           (when (> pos 0)
             (cond ((member :shift mod-keys)   ;delete to start of text when shift modifier
                    (setf (text view) (subseq (text view) pos (length (text view))))
                    (setf (cursor-position view) 0)
                    (setf (mark-position view) 0))
                   (t                          ;delete single character
                    (setf (text view) (concatenate 'string
                                                   (subseq (text view) 0 (1- pos))
                                                   (subseq (text view) pos (length (text view)))))
                    (decf (cursor-position view))
                    (decf (mark-position view))))))
          (t                            ;text selected
           (remove-selected-text view)))))

(defmethod do-delete-forward-input ((view ui-text-box-item))
  (let ((pos (cursor-position view))
        (mark (mark-position view)))
    (cond ((= pos mark)                 ;no text selection
           (when (< pos (length (text view)))
             (setf (text view) (concatenate 'string
                                            (subseq (text view) 0 pos)
                                            (subseq (text view) (1+ pos) (length (text view)))))))
          (t                            ;text selected
           (remove-selected-text view)))))

(defmethod do-kill-line-input ((view ui-text-box-item))
  (let ((pos (cursor-position view)))
    (when (< pos (length (text view)))
      (setf (text view) (subseq (text view) 0 pos)))
    (setf (mark-position view) pos)))

(defmethod do-arrow-input ((view ui-text-box-item) key)
  (let ((max-position (length (text view))))
    (cond ((eq :left key)
           (setf (cursor-position view) (clamp (1- (cursor-position view)) 0 max-position)))
          ((eq :right key)
           (setf (cursor-position view) (clamp (1+ (cursor-position view)) 0 max-position)))
          ((eq :up key)
           (setf (cursor-position view) 0))
          ((eq :down key)
           (setf (cursor-position view) max-position))))
  ;; update mark
  (setf (mark-position view) (cursor-position view)))

;;;; ui-menu-item ==============================================================

(defclass-kons-9 ui-menu-item (ui-button-item)
  ()
  (:default-initargs
   :bg-color (c! 0.8 0.8 0.8 0.5)))

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

(defmethod ui-remove-last-child ((view ui-group))
  (if (> (length (children view)) 0)
      (ui-remove-child view (aref (children view) (1- (length (children view)))))
      view))

(defmethod ui-clear-children ((view ui-group))
  (loop for child across (children view)
        do (setf (ui-parent child) nil))
  (setf (fill-pointer (children view)) 0)
  view)

(defmethod find-child ((view ui-group) name)
  (find-if (lambda (child) (eql name (ui-name child)))
           (children view)))

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
          (setf (ui-w view) (if title
                                (+ (ui-text-width title) *ui-default-padding*)
                                *ui-button-item-width*))
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

;;;; ui-data-entry =============================================================

(defclass-kons-9 ui-data-entry (ui-group)
  ()
  (:default-initargs
   :is-visible? t
   :layout :horizontal
   :spacing 0
   :padding 0))

(defmethod get-entry-value ((view ui-data-entry) type)
  (case type
    ((:boolean)
     (let ((check-box (aref (children view) 1)))
       (is-pushed? check-box)))
    ((:choice)
     (let* ((choice-button (aref (children view) 1))
            (str (text choice-button)))
       (read-from-string str)))
    (otherwise
     (let* ((text-box (aref (children view) 1))
            (str (text text-box)))
       (ecase type
         ((:number :symbol) (read-from-string str))
         ((:string) str)
         ((:point) (with-input-from-string (s str) (p! (read s) (read s) (read s)))))))))

(defmethod set-entry-value ((view ui-data-entry) value type)
  (case type
    ((:boolean)
     (let ((check-box (aref (children view) 1)))
       (setf (is-pushed? check-box) value)))
    ((:choice)
     (let ((choice-button (aref (children view) 1)))
       (setf (text choice-button) (format nil "~A" value))))
    (otherwise
     (let ((text-box (aref (children view) 1)))
       (setf (text text-box)
             (ecase type
               ((:number :symbol :string) (format nil "~A" value))
               ((:point) (format nil "~A ~A ~A" (p:x value) (p:y value) (p:z value)))))))))

(defun make-data-entry (name label value type &optional (choices nil))
  (let* ((view (make-instance 'ui-data-entry :ui-name name :draw-border? nil))
         (label (make-instance 'ui-label-item :ui-x 0 :ui-y 0
                                              :ui-w (+ 10 (ui-text-width label))
                                              :ui-h *ui-button-item-height*
                                              :text label :text-padding 5))
         (value-widget (case type
                         ((:boolean)
                          (make-instance 'ui-check-box-item :ui-x 80 :ui-y 0
                                                            :ui-w 120 :ui-h *ui-button-item-height*
                                                            :text ""))
                         ((:choice)
                          (make-instance 'ui-choice-button :ui-x 80 :ui-y 0
                                                           :ui-w 120 :ui-h *ui-button-item-height*
                                                           :text "" :choices choices))
                         (otherwise
                          (make-instance 'ui-text-box-item :ui-x 80 :ui-y 0
                                                           :ui-w 120 :ui-h *ui-button-item-height*)))))
    (ui-add-children view (list label value-widget))
    (update-layout view)
    (set-entry-value view value type)
    view))

;;;; scene-item-editor =========================================================

;; editable-slot-info -- name type validate-fn text-box-hints
;; highlight illegal text field if fails validation

(defun ui-get-child-values (view names types)
  (mapcar (lambda (name type) (get-entry-value (find-child view name) type))
          names
          types))

(defun ui-make-entries (names values types choices-list)
  (mapcar (lambda (name value type choices)
            (make-data-entry name (format nil "~A:" name) value type choices))
          names
          values
          types
          choices-list))

(defun make-scene-item-editor (obj update-obj-fn &key (title (format nil "Edit ~A" (name obj)))
                                                   (close-button? t)
                                                   (update-button-text "Update")
                                                   (close-after-update? nil))
  (let* ((param-info (editable-slots obj))                          ;get param names from class slot
         (param-names (mapcar #'first param-info))
         (param-types (mapcar #'second param-info))
         (param-choices (mapcar (lambda (param) (if (= 3 (length param))
                                                    (elt param 2)
                                                    nil))
                                param-info))
         (param-values (get-slot-values obj param-names))           ;get param values from instance
         (contents (ui-make-entries param-names param-values param-types param-choices));create ui widgets for params
         (update-fn (lambda (editor)                                ;define update func for editor
                      (let* ((ui-values (ui-get-child-values (find-child editor 'contents)
                                                             param-names param-types)))
                        (set-slot-values obj param-names ui-values) ;set instance slot values
                        (when close-after-update? (hide-ui-content editor))
                        (when update-obj-fn
                          (funcall update-obj-fn obj))))))          ;update instance
    (make-editor-panel title update-fn contents
                       :close-button? close-button?
                       :update-button-text update-button-text)))

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

#| xxxx
(show-ui-content
 (make-dialog-box
  (list (make-instance 'ui-label-item :ui-w *ui-popup-menu-width*
                                      :text "Label 1")
        (make-instance 'ui-choice-button :ui-w *ui-popup-menu-width*
;;                                         :choice-index 0
                                         :text "Pulldown"
                                         :choices #("Choice 0" "Best Choice 1" "Choice 2"))
        (make-instance 'ui-label-item :ui-w *ui-popup-menu-width*
                                      :text "Label 2")
        (make-instance 'ui-button-item :ui-w *ui-popup-menu-width*
                                       :text "My Button")
        (make-instance 'ui-check-box-item :ui-w *ui-popup-menu-width*
                                          :text "Check Box")
        (make-instance 'ui-text-box-item :ui-w *ui-popup-menu-width*
                                         :text "Text Box")
        )))
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

;;; TODO -- how to add contextual items from plugins?
;;;         -- plugin class -- register method adds UI elements -- eg. Create, Context, Edit, ...
;;; TODO -- delete shapes in hierarchy
;;; TODO -- update ui-contents due to scene updates -- eg hierarchy viewer when shape deleted
;;; TODO -- global key bindings (eg. space, backspace) -- register properly and avoid shadowing with c-t

;;;; editor panel --------------------------------------------------------------

(defun make-editor-panel (title update-func contents &key (close-button? t) (update-button-text "Update"))
  (let* ((box (make-instance 'ui-dialog-box
                             :ui-x 20
                             :ui-y 20
                             :title title
                             :spacing 10))
         (contents-group (make-instance 'ui-group
                                        :ui-name 'contents
                                        :layout :vertical
                                        :spacing 5
                                        :padding 0
                                        :justification :right/bottom
                                        :draw-border? nil ;t ;for debugging
                                        ))
         (buttons-group (make-instance 'ui-group
                                        :layout :horizontal
                                        :spacing 5
                                        :padding 5
                                        :draw-border? nil ;t ;for debugging
                                        ))
         (close-button (when close-button?
                         (make-instance 'ui-button-item
                                        :ui-w 80
                                        :text "Close"
                                        :on-click-fn (lambda (modifiers)
                                                       (declare (ignore modifiers))
                                                       (setf (is-visible? box) nil)))))
         (update-button (make-instance 'ui-button-item
                                   :ui-w 80
                                   :text update-button-text
                                   :on-click-fn (lambda (modifiers)
                                                  (declare (ignore modifiers))
                                                  (when update-func (funcall update-func box))))))
    (ui-add-children contents-group contents)
    (ui-add-children buttons-group (if close-button?
                                       (list close-button update-button)
                                       (list update-button)))
    (ui-add-child box (update-layout contents-group))
    (ui-add-child box (update-layout buttons-group))
    (update-layout box)))

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
                        
;;;; ui-choice-menu ============================================================

(defclass-kons-9 ui-choice-menu (ui-group)
  ((choice-button nil))
  (:default-initargs
   :is-visible? nil
   :bg-color (c! .8 .8 .8 1.0)          ;opaque as we may draw over other widgets
   :layout :vertical
   :spacing 0
   :padding 0
   :ui-x 0
   :ui-y *ui-button-item-height*))      ;position menu below button

(defmethod create-contents ((view ui-choice-menu))
  (setf (fill-pointer (children view)) 0)
  (let ((button (choice-button view)))
    (when button
      (setf (title view) nil)
      (when (> (length (choices button)) 0)
        (loop for text across (choices button)
              do (let ((tmp text))     ;TODO -- without this all entries get fn of last entry???
                   (vector-push-extend
                    (make-instance 'ui-menu-item :ui-w (ui-w button)
                                                 :ui-h *ui-button-item-height* 
                                                 :text text
                                                 :key-text nil
                                                 :is-active? t
                                                 :on-click-fn (lambda (modifiers)
                                                                (declare (ignore modifiers))
                                                                (setf (text button) tmp)
                                                                (setf (is-visible? view) nil)
                                                                (unregister-choice-menu)));unregister menu
                                                                
                    (children view)))))))
  (update-layout view))

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
;;-- context menu
;;  ++ transform
;;  -- register/generate entries
;;-- register new command tables from plugins
;;  -- procedural-curve
;;  ++ uv-mesh
;;  ++ heightfield
;;-- auto-generate procedural-curve create and edit dialogs
;;-- application class?
;;-- multiple visible inspectors? esc closes one under mouse?

;;;; draw-view -- :before bg, :after border
;;;; make-outliner obj children-fn name-fn
;;;; ui-3d-view (ui-group) -- root-node
;;;; -- scene-view
;;;; app-window -- outliner, 3d-view, inspector/aspect, timeline
;;;; clip to view when drawing
;;;; text-box -- validate-fn
;;;; arrow and Enter menu/command-table navigation

#| DONE
;;++ app table bindings in effect even if menu not visible
;;++ two-line status bar
;;  ++ mouse action, key action
;;;; store parent for ui-view
;;;; ui-message-box -- OK button, text
;;;; -- update-layout
;;;; ui-dialog-box -- Cancel, OK buttons, contents, :fit-children
;;;; -- update-layout
;;;; group title
;;;; ui-sequence-viewer seq -- necessary?
;;;; ui-inspector obj -- vertical group of horizontal groups of slot-name, slot-value
;;;; Display -- Dark Theme, Bright Theme
;;;; left-arrow to go to previous menu

|#

;;;; ui-sequence-viewer ========================================================

(defclass-kons-9 ui-sequence-viewer (ui-group)
  ((data nil))
  (:default-initargs
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

;;;; ui-outliner-item ==========================================================

(defun has-children-method? (obj)
  (compute-applicable-methods #'children (list obj)))

(defclass-kons-9 ui-outliner-item (ui-data-item)
  ((show-children? t)                   ;show hierarchy by default
   (outliner-children '())
   (outliner-parent nil)))

(defmethod initialize-instance :after ((view ui-outliner-item)  &rest initargs)
  (declare (ignore initargs))
  (setf (on-click-fn view) (lambda (modifiers)
                             (declare (ignore modifiers))
                             (cond
                               ;; disable show/hide of children until update and delete are implemented
                               ;; ((member :alt modifiers)
                               ;;      (when (has-children-method? (data view))
                               ;;        (toggle-show-children view)
                               ;;        (when (ui-parent view)
                               ;;          (update-parent-contents view))))
                                   (t
                                    (toggle-selection (scene (data view))
                                                      (data view))
                                    (setf (bg-color view) (if (is-selected? (data view))
                                                              (c! 0.8 0.2 0.2 0.5)
                                                              (c! 0 0 0 0))))))))
                                    ;; update context menu if applicable
                                    ;; (update-active-dynamic-command-table)
                                    ;; (draw-scene-view-ui *default-scene-view*))))))

(defmethod add-outliner-child ((view ui-outliner-item) (child ui-outliner-item))
  (push child (outliner-children view))
  (setf (outliner-parent child) view)
  view)

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
    (update-layout (ui-parent view))
    (resize-contents (ui-parent view))))

(defmethod add-parent-contents ((view ui-outliner-item) &key (recurse? nil))
  (let ((i (position view (children (ui-parent view))))
        (children (children (data view))))
    (dotimes (j (min 10 (length children)))
      ;; TODO -- cap num children entries to 10 to avoid text engine overflow
      ;; clipping does not work in all cases -- looks like outliner-view draw gets called without
      ;; update-layout being called -- maybe due to text render threading?
      ;; happens when shapes inspector is open and point-instancer-group demo in demo-misc.lisp
      ;; is run
      (let* ((child (aref children j))
             (text (format nil "~a" (printable-data child)))
             (item (make-instance (outliner-item-class (ui-parent view))
                                  :ui-w (+ (ui-text-width text)
                                           (* 4 *ui-default-spacing*)
                                           (+ 20 (text-padding view)))
                                  :ui-h *ui-button-item-height*
                                  :bg-color (if (is-selected? child)
                                                (c! 0.8 0.2 0.2 0.5)
                                                (c! 0 0 0 0))
                                  :text-padding (+ 20 (text-padding view))
                                  :data child
                                  :text text
                                  :is-active? t
                                  :help-string (format nil "Mouse: select ~a" ;, [ALT] show/hide children"
                                                       (name child)))))
        (ui-add-child-at (ui-parent view) item (incf i))
        (add-outliner-child view item))))
  (when recurse?
    (dolist (item (outliner-children view))
      (when (and (has-children-method? (data item)) (show-children? item))
      (add-parent-contents item :recurse? recurse?)))))

(defmethod remove-parent-contents ((view ui-outliner-item))
  (loop for child in (outliner-children view)
        do (remove-parent-contents child)
           (ui-remove-child (ui-parent view) child))
  (setf (outliner-children view) '()))

;;;; ui-outliner-viewer ========================================================

(defclass-kons-9 ui-outliner-viewer (ui-sequence-viewer)
  ((data-object nil)
   (data-accessor-fn nil)
   (items-show-children '()))
  (:default-initargs
   :bg-color (c! 1 1 1 0.75)
   :layout :vertical
   :justification :left/top
   :spacing 0
   :padding 0))

(defmethod outliner-item-class ((view ui-outliner-viewer))
  'ui-outliner-item)

(defmethod item-show-child? ((view ui-outliner-viewer) item)
  (let ((data (assoc item (items-show-children view))))
    (if data
        (cdr data)
        nil)))

(defmethod set-item-show-child ((view ui-outliner-viewer) item show?)
  (let ((data (assoc item (items-show-children view))))
    (if data
	(rplacd data show?)
        (push (cons item show?) (items-show-children view)))))

;;; returns a list
(defmethod viewer-data ((view ui-outliner-viewer))
  (if (data-object view)
      (if (data-accessor-fn view)
          (funcall (data-accessor-fn view) (data-object view))
          (list (data-object view)))
      nil))

(defmethod create-contents ((view ui-outliner-viewer))
  ;; create-contents gets called on every update, but we want to preserve our state
  ;; TODO -- this means the outliner does not reflect scene changes (add/delete items)
  ;; (when (> (length (children view)) 0)
  ;;   (return-from create-contents view))
  (setf (fill-pointer (children view)) 0)
  (let ((data (viewer-data view)))
    (when data
      (loop for entry across (coerce data 'vector)
            do (let* (;(tmp entry) ; TODO -- necessary otherwise all on-click-fn's use final entry???
                      (text (format nil "~a" (printable-data entry)))
                      (item (make-instance (outliner-item-class view)
                                           :ui-h *ui-button-item-height*
                                           :bg-color (if (is-selected? entry)
                                                         (c! 0.8 0.2 0.2 0.5)
                                                         (c! 0 0 0 0))
                                           :data entry
                                           :text text
                                           :is-active? t
                                           :help-string (format nil "Mouse: select ~a" ;, [alt] show/hide children" ; not implemented yet
                                                                (name entry)))))
                 (setf (ui-w item)
                       (+ (ui-text-width (outliner-item-text item)) (* 4 *ui-default-spacing*)))
                 (ui-add-child view item)
                 ;; show children if any
                 (when (and (has-children-method? (data item)) (show-children? item))
                   (add-parent-contents item :recurse? t))))))
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

;;;; ui-inspector ==============================================================

(defclass-kons-9 ui-inspector (ui-sequence-viewer)
  ((obj nil))
  (:default-initargs
   :bg-color (c! 1 1 1 0.75)
   :layout :vertical
   :justification :left/top
   :spacing 0
   :padding 0))

(defun ui-cleanup-inspector-string (text)
  (string-trim-to-length (remove-extra-spaces text) 60))

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
                                             :help-string (format nil "Mouse: inspect ~a. UP/DN-ARROW: scroll inspectors. ESC: close inspectors, SHIFT-L-ARROW: close last inspector."
                                                                  (ui-cleanup-inspector-string
                                                                   (format nil "~a" datum)))
                                             :on-click-fn (lambda (modifiers)
                                                            (declare (ignore modifiers))
                                                            (show-ui-content (make-ui-inspector (if named-p (cdr tmp) tmp)))))
                (children view))))
    (update-layout view)))

(defun make-ui-inspector (obj)
  (create-contents (make-instance 'ui-inspector :ui-x 20 :ui-y 20 :obj obj)))

;;;; drawing ===================================================================

(defun ui-is-clipped? (x-lo y-lo x-hi y-hi)

  ;; (print (list x-lo y-lo x-hi y-hi
  ;;              *ui-clip-rect*
  ;;              (and *ui-clip-rect*
  ;;                   (or (> x-lo (ui-right  *ui-clip-rect*))
  ;;                       (< x-hi (ui-x      *ui-clip-rect*))
  ;;                       (> y-lo (ui-bottom *ui-clip-rect*))
  ;;                       (< y-hi (ui-y      *ui-clip-rect*))))))
  
  (and *ui-clip-rect*
       (or (> x-lo (ui-right  *ui-clip-rect*))
           (< x-hi (ui-x      *ui-clip-rect*))
           (> y-lo (ui-bottom *ui-clip-rect*))
           (< y-hi (ui-y      *ui-clip-rect*)))))
         
(defun draw-line (x1 y1 x2 y2 &optional (line-width *ui-border-width*))
  (when (not (ui-is-clipped? x1 y1 x2 y2))
    (gl:line-width line-width)
    (gl:begin :lines)
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)
    (gl:end)))

(defun draw-rect-fill (x y w h &optional (inset 0.0))
  (when (not (ui-is-clipped? x y (+ x w) (+ y h)))
    (gl:polygon-mode :front-and-back :fill)
    (gl:begin :polygon)
    (gl:vertex    (+ x inset)       (+ y inset))
    (gl:vertex (- (+ x w) inset)    (+ y inset))
    (gl:vertex (- (+ x w) inset) (- (+ y h) inset))
    (gl:vertex    (+ x inset)    (- (+ y h) inset))
    (gl:end)))

;; draw as lines so corners look clean
(defun draw-rect-border (x y w h &optional (inset 0.0) (line-width *ui-border-width*))
  (when (not (ui-is-clipped? x y (+ x w) (+ y h)))
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
    (gl:end)))

(defun draw-rect-x-mark (x y w h &optional (inset 0.0) (line-width *ui-border-width*))
  (when (not (ui-is-clipped? x y (+ x w) (+ y h)))
    (gl:line-width line-width)
    (gl:begin :lines)
    (gl:vertex    (+ x inset)       (+ y inset))
    (gl:vertex (- (+ x w) inset) (- (+ y h) inset))
    (gl:vertex    (+ x inset)    (- (+ y h) inset))
    (gl:vertex (- (+ x w) inset)    (+ y inset))
    (gl:end)))

(defun draw-text-selection (x0 x1 y)
  (let ((x-lo (min x0 x1))
        (x-hi (max x0 x1))
        (y-lo (+ y 3))
        (y-hi (- (+ y *ui-button-item-height*) 4)))
    (gl:color 0.7 0.7 1.0 1.0)
    (draw-rect-fill x-lo y-lo (- x-hi x-lo) (- y-hi y-lo))))

(defun draw-cursor (x y)
  (let ((x0 (- x 3))
        (x1 (+ x 2))
        (y0 (+ y 5))
        (y1 (- (+ y *ui-button-item-height*) 4)))
    (when (not (ui-is-clipped? x0 y0 x1 y1))
      (gl:color 0.0 0.0 0.0 1.0)
      (gl:line-width *ui-border-width*)
      (gl:begin :lines)
      (gl:vertex x y0)
      (gl:vertex x y1)
      (gl:vertex x0 y0)
      (gl:vertex x1 y0)
      (gl:vertex x0 y1)
      (gl:vertex x1 y1)
      (gl:end))))

(defmethod draw-title-bar ((view ui-group) x-offset y-offset)
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

  
(defmethod draw-ui-view ((view ui-view) x-offset y-offset)
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
  
(defgeneric draw-view (view x-offset y-offset)

  (:method ((view ui-view) x-offset y-offset)
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)))
  
  (:method ((view ui-label-item) x-offset y-offset)
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (render-text (+ (text-padding view) x x-offset) (+ 16 y y-offset) (text view)))))

  (:method ((view ui-outliner-item) x-offset y-offset)
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (render-text (+ (text-padding view) x x-offset) (+ 16 y y-offset) (outliner-item-text view)))))

  (:method ((view ui-button-item) x-offset y-offset)
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (cond ((key-text view)
               (render-text (+ 5 x x-offset) (+ 16 y y-offset) (key-text view))
               (render-text (+ (ui-centered-text-x (text view) (ui-w view)) x x-offset)
                            (+ 16 y y-offset)
                            (text view)))
              (t
               (render-text (+ (ui-centered-text-x (text view) (ui-w view)) x x-offset)
                            (+ 16 y y-offset)
                            (text view)))))))

  (:method ((view ui-menu-item) x-offset y-offset)
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (cond ((key-text view)
               (render-text (+ 5 x x-offset) (+ 16 y y-offset) (key-text view))
               (render-text (+ 30 x x-offset) (+ 16 y y-offset) (text view)))
              (t
               (render-text (+ 5 x x-offset) (+ 16 y y-offset) (text view)))))))

  (:method ((view ui-check-box-item) x-offset y-offset)
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

  (:method ((view ui-choice-button) x-offset y-offset)
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (render-text (+ (text-padding view) x x-offset) (+ 16 y y-offset) (text view))
        (when (is-visible? (choice-menu view))
          (draw-view (choice-menu view) (+ x x-offset) (+ y y-offset))))))
  
  (:method ((view ui-text-box-item) x-offset y-offset)
    (when (is-visible? view)
      (draw-ui-view view x-offset y-offset)
      (with-accessors ((x ui-x) (y ui-y))
          view
        (let ((local-x (+ 5 x x-offset))
              (local-y (+ y y-offset)))
          (render-text local-x (+ 16 local-y) (text view))
          (when (eq view *ui-keyboard-focus*)
            (when (not (= (cursor-position view) (mark-position view)))
              (draw-text-selection (+ local-x (* *ui-font-width* (cursor-position view)))
                                   (+ local-x (* *ui-font-width* (mark-position view)))
                                   local-y))
            (draw-cursor (+ local-x (* *ui-font-width* (cursor-position view))) local-y))))))

  (:method :after ((view ui-group) x-offset y-offset)
    (when (is-visible? view)
      (when (title view)
        (draw-title-bar view x-offset y-offset))
      (loop for child across (children view)
            do (draw-view child (+ (ui-x view) x-offset) (+ (ui-y view) y-offset)))))
  )

(defgeneric draw-view-overlay (view x-offset y-offset)

  (:method ((view ui-view) x-offset y-offset)
    ;; do nothing
    )

  (:method ((view ui-choice-button) x-offset y-offset)
    (when (and (is-visible? view) (is-visible? (choice-menu view)))
      (draw-view (choice-menu view) (+ (ui-x view) x-offset) (+ (ui-y view) y-offset))
      ;; we do this here because we need the global x and y of the menu
      (register-choice-menu (choice-menu view) (+ (ui-x view) x-offset) (+ (ui-y view) y-offset))))

  (:method ((view ui-group) x-offset y-offset)
    (when (is-visible? view)
      (loop for child across (children view)
            do (draw-view-overlay child (+ (ui-x view) x-offset) (+ (ui-y view) y-offset)))))
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

  (:method ((view ui-choice-button) global-x global-y &optional (x-offset 0) (y-offset 0))
    (if (is-visible? (choice-menu view))
        (progn
          (print (list global-x global-y x-offset y-offset (ui-x view) (ui-y view)
                       (find-ui-at-point (choice-menu view) global-x global-y
                                         (+ (ui-x view) x-offset) (+ (ui-y view) y-offset))))
          
        (find-ui-at-point (choice-menu view) global-x global-y
                          (+ (ui-x view) x-offset) (+ (ui-y view) y-offset))
        )
        (call-next-method)))
  )

