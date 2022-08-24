(in-package #:kons-9)

;;;; group =====================================================================

;;; class for managing hierarchies of shapes
(defclass group (shape)
  ((children :accessor children :initarg :children :initform '())))

(defmethod add-child ((self group) (s shape))
  (push s (children self))
  self)

(defmethod set-children ((self group) shapes)
  (setf (children self) shapes)
  self)

(defmethod remove-all-children ((self group))
  (setf (children self) '())
  self)

(defun make-group (&rest shapes)
  (make-instance 'group :children shapes))

(defmethod draw ((self group))
  (mapc #'draw (children self)))

(defmethod print-hierarchy ((self shape) &optional (indent 0))
  (print-spaces indent)
  (format t "~a~%" self))

(defmethod print-hierarchy :after ((self group) &optional (indent 0))
  (dolist (child (children self))
    (print-hierarchy child (+ indent 2))))

(defmethod do-hierarchy ((self shape) func &key (test nil))
  (when (or (null test) (funcall test self))
    (funcall func self))
  self)

(defmethod do-hierarchy :after ((self group) func &key (test nil))
  (dolist (child (children self))
    (do-hierarchy child func :test test))
  self)

(defmethod is-leaf? ((self shape))
  t)

(defmethod is-leaf? ((self group))
  nil)

(defun scatter-shapes-in-group (shape-fn points)
  (make-instance 'group :children (mapcar (lambda (p)
                                            (translate-to (funcall shape-fn) p))
                                          (coerce points 'list))))

(defmethod get-bounds ((self group))
  (let ((bounds-lo nil)
        (bounds-hi nil))
    (dolist (child (children self))
      (multiple-value-bind (lo hi)
          (get-bounds child)
        (when lo
          (setf bounds-lo (if bounds-lo
                              (p-min bounds-lo lo)
                              lo)))
        (when hi
          (setf bounds-hi (if bounds-hi
                              (p-max bounds-hi hi)
                              hi)))))
    (values bounds-lo bounds-hi)))

(defmethod set-point-colors-by-xyz ((group group) color-fn)
  (dolist (child (children group))
    (set-point-colors-by-xyz child color-fn)))

(defmethod set-point-colors-by-point-and-normal ((group group) color-fn)
  (dolist (child (children group))
    (set-point-colors-by-point-and-normal child color-fn)))

