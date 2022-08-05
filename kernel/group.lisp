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

(defmethod bounds-and-center ((self group))
  (let ((bounds-lo nil)
        (bounds-hi nil))
    (dolist (child (children self))
      (multiple-value-bind (lo hi center)
          (bounds-and-center child)
        (declare (ignore center))
        (when lo
          (setf bounds-lo (if bounds-lo
                              (p-min bounds-lo lo)
                              lo)))
        (when hi
          (setf bounds-hi (if bounds-hi
                              (p-max bounds-hi hi)
                              hi)))))
    (values bounds-lo bounds-hi (if (and bounds-lo bounds-hi)
                                    (p-average bounds-lo bounds-hi)
                                    nil))))

