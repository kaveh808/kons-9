(in-package #:kons-9)

;;;; group =====================================================================

;;; class for managing hierarchies of shapes
(defclass shape-group (shape group-mixin)
  ())

(defmethod printable-data ((self shape-group))
  (strcat (call-next-method) (format nil ", ~a children" (length (children self)))))

(defun make-shape-group (shapes &key (name nil))
  (let ((group (make-instance 'shape-group :name name)))
    (dolist (shape shapes)
      (add-child group shape))
    group))

(defmethod get-bounds ((group shape-group))
  (let ((bounds-lo nil)
        (bounds-hi nil))
    (do-children (child group)
      (multiple-value-bind (lo hi)
          (get-bounds child)
        (when (and lo hi)
          (let* ((matrix (transform-matrix (transform child)))
                 (xform-lo (transform-point lo matrix))
                 (xform-hi (transform-point hi matrix)))
            (setf bounds-lo (if bounds-lo
                                (p:min bounds-lo xform-lo)
                                xform-lo))
            (setf bounds-hi (if bounds-hi
                                (p:max bounds-hi xform-hi)
                                xform-hi))))))
    (values bounds-lo bounds-hi)))

(defmethod set-point-colors-by-xyz ((group shape-group) color-fn)
  (do-children (child group)
    (set-point-colors-by-xyz child color-fn)))

(defmethod set-point-colors-by-point-and-normal ((group shape-group) color-fn)
  (do-children (child group)
    (set-point-colors-by-point-and-normal child color-fn)))

;;;; TODO
;;;; find a better place for these functions -- modeling.lisp?
;;;; shapes is a list, points is a vector -- confusing?

(defun scatter-shapes-in-group (shape-fn points)
  (let ((shapes (mapcar (lambda (p)
                          (translate-to (funcall shape-fn) p))
                        (coerce points 'list))))
    (make-shape-group shapes)))

(defun scatter-shapes (shapes points)
  (if (= (length shapes) (length points))
      (loop for shape in shapes
            for point across points
            do (translate-to shape point))
      (error "Mismatch in sizes of shapes (~a) and points (~a)" (length shapes) (length points))))

(defun scatter-shape-instances (shapes points)
  (if (= (length shapes) (length points))
      (let ((instances (mapcar (lambda (shape) (make-shape-group (list shape)))
                               shapes)))
        (scatter-shapes instances points)
        instances)
      (error "Mismatch in sizes of shapes (~a) and points (~a)" (length shapes) (length points))))

