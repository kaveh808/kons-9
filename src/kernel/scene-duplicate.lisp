(in-package #:kons-9)

;;;; scene duplicate functions =================================================

(defun duplicate-shape (scene scene-path)
  (let* ((item (find-shape-by-name scene (scene-path-item scene-path)))
         (dup (duplicate item))
         (parent (find-shape-by-name scene (scene-path-parent-item scene-path))))
    (if parent
        (add-child parent dup)
        (add-shape scene dup))
;;; TODO -- use auto name generation for now till we have glbal solution
;;;    (setf (name dup) (build-scene-item-name (name item)))
    dup))

;; duplicate-motion

(defgeneric duplicate (obj)
  
  (:method ((item item))
    (let ((dup (make-instance (class-name (class-of item)))))
      (copy-instance-data dup item)
      dup))
  (:method ((seq sequence))
    (copy-seq seq))
  (:method ((a array))
    (if (and (arrayp a) (equal '(4 4) (array-dimensions a)))
        (matrix-copy a)
        (copy-seq a)))
  )

(defmacro copy-simple-slot (slot)
  `(setf (,slot dst) (,slot src)))

(defmacro copy-point-slot (slot)
  `(setf (,slot dst) (p:copy (,slot src))))

(defmacro copy-array-slot (slot)
  `(setf (,slot dst) (map 'vector #'duplicate (,slot src))))

(defmacro copy-list-slot (slot)
  `(setf (,slot dst) (mapcar #'duplicate (,slot src))))

(defmacro copy-instance-slot (slot)
  `(copy-instance-data (,slot dst) (,slot src)))
  
(defmacro copy-custom-slot (slot func)
  `(setf (,slot dst) (,func (,slot src))))



(defgeneric copy-instance-data (dst src)
  
  (:method ((dst item) (src item))
    )

  (:method ((dst scene-item) (src scene-item))
    (call-next-method)
    ;; new name assigned when item created
    ;; use src + '-1 name?
    ;; or src + '-copy?
    (copy-simple-slot scene)
    )

  (:method ((dst shape) (src shape))
    (call-next-method)
    (copy-custom-slot transform duplicate) ;create same class transform
    (copy-simple-slot show-axis)
    (copy-simple-slot show-bounds?))

  (:method ((dst group) (src group))
    (call-next-method)
    (copy-list-slot children))

  (:method ((dst point-cloud) (src point-cloud))
    (call-next-method)
    (copy-array-slot points))

  (:method ((dst curve) (src curve))
    (call-next-method)
    (copy-simple-slot is-closed-curve?))

  (:method ((dst polyhedron) (src polyhedron))
    (call-next-method)
    (copy-array-slot faces)
    (copy-array-slot face-normals)
    (copy-array-slot point-normals)
    (copy-array-slot point-colors)
    (copy-simple-slot show-normals))

  (:method ((dst transform) (src transform))
    (call-next-method)
    (copy-simple-slot operator-order))

  (:method ((dst euler-transform) (src euler-transform))
    (call-next-method)
    (copy-instance-slot translate)
    (copy-instance-slot rotate)
    (copy-instance-slot scale))

  (:method ((dst angle-axis-transform) (src angle-axis-transform))
    (call-next-method)
    (copy-instance-slot translate)
    (copy-instance-slot rotate)
    (copy-instance-slot scale))

  (:method ((dst generalized-transform) (src generalized-transform))
    (call-next-method)
    (copy-list-slot operators))

  (:method ((dst transform-operator) (src transform-operator))
    )

  (:method ((dst translate-operator) (src translate-operator))
    (call-next-method)
    (copy-point-slot offset)
    )

  (:method ((dst euler-rotate-operator) (src euler-rotate-operator))
    (call-next-method)
    (copy-point-slot angles)
    (copy-simple-slot rotate-order)
    (copy-point-slot pivot)
    )

  (:method ((dst angle-axis-rotate-operator) (src angle-axis-rotate-operator))
    (call-next-method)
    (copy-simple-slot angle)
    (copy-point-slot axis)
    (copy-point-slot pivot)
    )

  (:method ((dst scale-operator) (src scale-operator))
    (call-next-method)
    (copy-point-slot scaling)
    (copy-point-slot pivot)
    )

  (:method ((dst motion) (src motion))
    (call-next-method)
    (copy-simple-slot start-time)
    (copy-simple-slot duration)
    (copy-simple-slot timing-mode)
    (copy-simple-slot local-time)
    )

  (:method ((dst motion-group) (src motion-group))
    (call-next-method)
    (copy-list-slot children))

  (:method ((dst animator) (src animator))
    (call-next-method)
    ;; setup-done? -- do not copy
    (copy-simple-slot setup-fn)
    (copy-simple-slot update-fn))

  (:method ((dst shape-animator) (src shape-animator))
    (call-next-method)
    ;; (copy-simple-slot shape) -- do not copy
    (copy-custom-slot data copy-alist))

  ;; TODO -- test this
  (:method ((dst animation) (src animation))
    (call-next-method)
    (copy-instance-slot shape)
    (copy-instance-slot shape-animator)
    (typecase (shape-animator dst)
      (shape-animator (setf (shape (shape-animator dst)) (shape dst)))))

  (:method ((dst scene) (src scene))
    (call-next-method)
    (copy-list-slot shapes)
    (copy-list-slot motions)
    (copy-list-slot selection)
    ;; initialized? -- do not copy
    (copy-simple-slot start-frame)
    (copy-simple-slot end-frame)
    (copy-simple-slot current-frame)
    (copy-simple-slot fps))
  )
