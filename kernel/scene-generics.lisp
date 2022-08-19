(in-package #:kons-9)

;;;; scene generic functions ===================================================

;;; utils ----------------------------------------------------------------------

(defun cleanup-nested-path-list (l)
  (if (not (listp (car l)))
      (list l)
      (if (and (= 1 (length l)) (listp (car l)))
          (cleanup-nested-path-list (car l))
          l)))

;;; find-scene-items -----------------------------------------------------------

(defgeneric find-scene-items (root test-fn)
  
  (:method ((scene scene) test-fn)
    (remove nil
            (flatten-list (mapcar (lambda (child) (find-scene-items child test-fn))
                                  (shapes scene)))))

  (:method ((group group) test-fn)
    (remove nil
            (flatten-list (cons (if (funcall test-fn group)
                                    group
                                    nil)
                                (mapcar (lambda (child) (find-scene-items child test-fn))
                                        (children group))))))

  (:method ((scene-item scene-item) test-fn)
    (if (funcall test-fn scene-item)
        scene-item
        nil))
  )

;;; find-scene-item-by-name ----------------------------------------------------

(defgeneric find-scene-item-by-name (root name)
  
  (:method ((scene scene) name)
    (let ((results (find-scene-items scene (lambda (item) (eq name (name item))))))
      (if results
          (first results)
          nil)))

  (:method ((group group) name)
    (let ((results (find-scene-items group (lambda (item) (eq name (name item))))))
      (if results
          (first results)
          nil)))
  )

;;; find-scene-item-by-path ----------------------------------------------------

(defgeneric find-scene-item-by-path (obj scene-path)
  
  (:method ((scene scene) scene-path)
    (if (null scene-path)
        scene
        (let ((child (find (first scene-path) (shapes scene) :key #'name)))
          (if child
              (find-scene-item-by-path child (rest scene-path))
              nil))))

  (:method ((group group) scene-path)
    (if (null scene-path)
        group
        (let* ((child (find (first scene-path) (children group) :key #'name)))
          (if child
              (find-scene-item-by-path child (rest scene-path))
              nil))))

  (:method ((scene-item scene-item) scene-path)
    (if (null scene-path)
        scene-item
        nil))
  )

;;; get-scene-paths ------------------------------------------------------------

(defmethod get-scene-paths ((scene scene) (item scene-item))
  (get-scene-paths-aux scene item))

(defgeneric get-scene-paths-aux (obj item)
  
  (:method ((scene scene) item)
    (if (eq scene item)
        '(SCENE)
        (let ((paths ()))
          (dolist (child (shapes scene))
            (let ((path (get-scene-paths-aux child item)))
              (when path
                (push path paths))))
          (cleanup-nested-path-list paths))))

  (:method ((group group) item)
    (if (eq group item)
        (list (name item))
        (let ((result ()))
          (dolist (child (children group))
            (let ((path (get-scene-paths-aux child item)))
              (when path
                (push (mapcar (lambda (p) (cons (name group) (flatten-list p))) path) result))))
          result)))

  (:method ((scene-item scene-item) item)
    (if (eq scene-item item)
        (list (name item))
        nil))
  )

;;; global-matrix --------------------------------------------------------------

(defmethod global-matrix ((scene scene) scene-path)
  (let ((matrix-list (get-matrix-list scene scene-path)))
    (if matrix-list
        (apply #'matrix-multiply-n matrix-list)
        (error "Shape not found for scene path ~a" scene-path))))

(defgeneric get-matrix-list (obj scene-path)
  
  (:method ((scene scene) scene-path)
    (if (null scene-path)
        (make-id-matrix)
        (let ((child (find (first scene-path) (shapes scene) :key #'name)))
          (if child
              (get-matrix-list child (rest scene-path))
              nil))))

  (:method ((group group) scene-path)
    (if (null scene-path)
        (list (transform-matrix (transform group)))
        (let* ((child (find (first scene-path) (children group) :key #'name)))
          (if child
              (cons (transform-matrix (transform group))
                    (get-matrix-list child (rest scene-path)))
              nil))))

  (:method ((shape shape) scene-path)
    (if (null scene-path)
        (list (transform-matrix (transform shape)))
        nil))
  )

;;; print-hierarchy ------------------------------------------------------------

(defgeneric print-hierarchy (obj &optional indent)

  (:method ((scene scene) &optional (indent 0))
    (print-spaces indent)
    (format t "~a~%" scene)
    (dolist (shape (shapes scene))
      (print-hierarchy shape (+ indent 2))))

  (:method :after ((group group) &optional (indent 0))
    (dolist (child (children group))
      (print-hierarchy child (+ indent 2))))

  (:method ((scene-item scene-item) &optional (indent 0))
    (print-spaces indent)
    (format t "~a ~s~%" scene-item (name scene-item))))
