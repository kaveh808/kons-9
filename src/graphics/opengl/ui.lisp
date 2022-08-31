(in-package #:kons-9)

;;;; ui utils ==================================================================

;; (defun draw-centered-text (str rect-width y)
;;   (let* ((dict (make-instance 'ns:ns-mutable-dictionary :with-capacity 2)))
;;     (#/setObject:forKey: dict (#/fontWithName:size:
;;                                ns:ns-font
;;                                #@"Monaco" 12.0) #&NSFontAttributeName)
;;     (let* ((str-width (ns:ns-size-width (#/sizeWithAttributes: (objc:make-nsstring str) dict)))
;;            (x (/ (- rect-width str-width) 2)))
;;       (#/drawAtPoint:withAttributes: (objc:make-nsstring str)
;;                                      (ns:make-ns-point x y)
;;                                      dict))))

(defparameter *ui-popup-menu-width* 200)
(defparameter *ui-button-item-width* 200)
(defparameter *ui-button-item-height* 25)

(defparameter *ui-border-width* 1)
(defparameter *ui-border-color* (c! 0 0 0))
(defparameter *ui-highlight-border-width* 4)
(defparameter *ui-highlight-border-color* (c! 0 0 1))

;;;; ui-rect ===================================================================

(defclass ui-rect ()
  ((x :accessor x :initarg :x :initform 0.0)
   (y :accessor y :initarg :y :initform 0.0)
   (w :accessor w :initarg :w :initform 0.0)
   (h :accessor h :initarg :h :initform 0.0)))

;;;; ui-view ===================================================================

(defclass ui-view (ui-rect)
  ((bg-color :accessor bg-color :initarg :bg-color :initform (c! 0 0 0 0))
   (fg-color :accessor fg-color :initarg :fg-color :initform (c! 0 0 0 1))
   (highlight? :accessor highlight? :initarg :highlight? :initform nil)))

;;;; ui-button-item ============================================================

(defclass ui-button-item (ui-view)
  ((text :accessor text :initarg :text :initform "Button")
   (action-fn :accessor action-fn :initarg :action-fn :initform nil)))

(defmethod do-action ((view ui-button-item))
  (when (action-fn view)
    (funcall (action-fn view))))

;; (defmethod draw-view :after ((view ui-button-item))
;;   (draw-centered-text (text view) (w view) 5))

;;;; ui-menu-item ==============================================================

(defclass ui-menu-item (ui-button-item)
  ())

;;;; ui-group ==============================================================

(defclass ui-group (ui-view)
  ((children :accessor children :initarg :children :initform '())))

;;;; ui-popup-menu =============================================================

(defclass ui-popup-menu (ui-group)
  ((command-table :accessor command-table :initarg :command-table :initform nil))
  (:default-initargs
   :bg-color (c! 1 1 1 0.5)))

(defmethod update-layout ((view ui-popup-menu))
  (let ((table (command-table view)))
    (when table
      (let* ((height (* (length (entries table)) *ui-button-item-height*))
             (y height))
        (setf (w view) *ui-popup-menu-width*)
        (setf (h view) height)
        (loop for entry across (entries table)
              do (push (make-instance 'ui-menu-item :x 0 :y (decf y *ui-button-item-height*)
                                                    :w *ui-popup-menu-width* :h *ui-button-item-height*
                                                    :text (string (help-string entry))
                                                    :action-fn (command-fn entry))
                       (children view))))))
  view)

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
(defun draw-rect-border (x y w h &optional (inset 0.0))
  (gl:polygon-mode :front-and-back :line)
  (gl:begin :lines)
  (gl:vertex       x              (+ y inset))
  (gl:vertex    (+ x w)           (+ y inset))
  (gl:vertex       x           (- (+ y h) inset))
  (gl:vertex    (+ x w)        (- (+ y h) inset))
  (gl:vertex (- (+ x w) inset)    (+ y inset));   y)
  (gl:vertex (- (+ x w) inset) (- (+ y h) inset))
  (gl:vertex    (+ x inset)       (+ y inset));   y)
  (gl:vertex    (+ x inset)    (- (+ y h) inset))
  (gl:end))

(defgeneric draw-view (view &optional x-offset y-offset)

  (:method ((view ui-view) &optional (x-offset 0) (y-offset 0))  
    (with-accessors ((bg bg-color) (fg fg-color) (x x) (y y) (w w) (h h))
        view
      ;; fill
      (when (> (c-alpha bg) 0)
        (gl:color (c-red bg) (c-green bg) (c-blue bg) (c-alpha bg))
        (draw-rect-fill (+ x x-offset) (+ y y-offset) w h))
      ;; border
      (let ((line-width (if (highlight? view) *ui-highlight-border-width* *ui-border-width*)))
        (gl:line-width line-width)
        (gl:color (c-red fg) (c-green fg) (c-blue fg) (c-alpha fg))
        (draw-rect-border (+ x x-offset) (+ y y-offset) w h (* 0.5 line-width)))))
  
  (:method :after ((view ui-group) &optional (x-offset 0) (y-offset 0))
    (dolist (child (children view))
      (draw-view child (+ (x view) x-offset) (+ (y view) y-offset))))
  )

;;;; hit testing -------------------------

(defun ui-point-in-rect? (px py rx ry rw rh)
  (and (> px rx) (> py ry) (< px (+ rx rw)) (< py (+ ry rh))))

(defgeneric find-ui-at-point (view global-x global-y &optional x-offset y-offset)

  (:method ((view ui-rect) global-x global-y &optional (x-offset 0) (y-offset 0))
    (with-accessors ((x x) (y y) (w w) (h h))
        view
      (let ((local-x (- global-x x-offset))
            (local-y (- global-y y-offset)))
        (if (ui-point-in-rect? local-x local-y x y w h)
            view
            nil))))
  
  (:method ((view ui-group) global-x global-y &optional (x-offset 0) (y-offset 0))
    (cond ((call-next-method)           ;is in view
           (when (children view)
             (dolist (child (children view))
               (let ((found (find-ui-at-point child global-x global-y
                                              (+ (x view) x-offset) (+ (y view) y-offset))))
                 (when found
                   (return-from find-ui-at-point found))))
             view))
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
