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

;;;; modeling with groups ======================================================

(defun scatter-shapes-in-group (shape-fn point-array)
  (let ((shapes (map 'list
                     (lambda (p) (translate-to (funcall shape-fn) p))
                     point-array)))
    (make-shape-group shapes)))

(defun scatter-shapes (shapes point-array)
  (if (= (length shapes) (length point-array))
      (loop for shape in shapes
            for point across point-array
            do (translate-to shape point))
      (error "Mismatch in sizes of shapes (~a) and point-array (~a)" (length shapes) (length point-array))))

(defun scatter-shape-instances (shapes point-array)
  (if (= (length shapes) (length point-array))
      (let ((instances (mapcar (lambda (shape) (make-shape-group (list shape)))
                               shapes)))
        (scatter-shapes instances point-array)
        instances)
      (error "Mismatch in sizes of shapes (~a) and point-array (~a)" (length shapes) (length point-array))))

