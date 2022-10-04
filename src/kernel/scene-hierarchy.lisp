(in-package #:kons-9)

;;; TODO:
;;; does instancing of motions make sense? they do same anim on same shapes...


;;; print scene hierarchies ====================================================

(defmethod print-shape-hierarchy ((scene scene) &key (names-only t) (indent 0))
  (print-spaces indent)
  (format t "~%~a~%" scene)
  (dolist (scene-item (shapes scene))
    (print-hierarchy scene-item :names-only names-only :indent (+ indent 2))))

(defmethod print-motion-hierarchy ((scene scene) &key (names-only t) (indent 0))
  (print-spaces indent)
  (format t "~%~a~%" scene)
  (dolist (scene-item (motions scene))
    (print-hierarchy scene-item :names-only names-only :indent (+ indent 2))))

(defgeneric print-hierarchy (obj &key names-only indent)

  (:method :after ((group group) &key (names-only t) (indent 0))
    (dolist (child (children group))
      (print-hierarchy child :names-only names-only :indent (+ indent 2))))

  (:method :after ((group motion-group) &key (names-only t) (indent 0))
    (dolist (child (children group))
      (print-hierarchy child :names-only names-only :indent (+ indent 2))))

  (:method ((scene-item scene-item) &key (names-only t) (indent 0))
    (print-spaces indent)
    (if names-only
        (format t "~a~%" (name scene-item))
        (format t "~a~%" scene-item))))

;;;; map scene hierarchies =====================================================

(defmethod map-shape-hierarchy ((scene scene) func &key (test nil))
  (dolist (child (shapes scene))
    (map-hierarchy child func :test test))
  scene)

(defmethod map-motion-hierarchy ((scene scene) func &key (test nil))
  (dolist (child (motions scene))
    (map-hierarchy child func :test test))
  scene)

(defgeneric map-hierarchy (self func &key test)

  (:method ((shape shape) func &key (test nil))
    (when (or (null test) (funcall test shape))
      (funcall func shape))
    shape)

  (:method :after ((group group) func &key (test nil))
    (dolist (child (children group))
      (map-hierarchy child func :test test))
    group)

  (:method ((motion motion) func &key (test nil))
    (when (or (null test) (funcall test motion))
      (funcall func motion))
    motion)

  (:method :after ((motion motion-group) func &key (test nil))
    (dolist (child (children motion))
      (map-hierarchy child func :test test))
    motion)
  )

(defgeneric is-leaf? (self)

  (:method ((shape shape))
    t)

  (:method ((group group))
    nil)

  (:method ((motion motion))
    t)

  (:method ((motion-group motion-group))
    nil)
)

;;;; utils =====================================================================

(defun scene-path-item (scene-path)
  (if (null scene-path)
      nil
      (first (last scene-path))))
  
(defun scene-parent-path (scene-path)
  (if (or (null scene-path) (= 1 (length scene-path)))
      ()
      (butlast scene-path)))

(defun scene-path-parent-item (scene-path)
  (scene-path-item (scene-parent-path scene-path)))

(defun cleanup-nested-path-list (l)
  (if (not (listp (car l)))
      (list l)
      (if (and (= 1 (length l)) (listp (car l)))
          (cleanup-nested-path-list (car l))
          l)))

;;;; scene shape hierarchy functions ===========================================

;;; find-shapes ----------------------------------------------------------------

(defgeneric find-shapes (root test-fn &key groups)
  
  (:method ((scene scene) test-fn &key (groups t))
    (remove-duplicates
     (remove nil
             (flatten-list (mapcar (lambda (child) (find-shapes child test-fn :groups groups))
                                   (shapes scene))))))

  (:method ((group group) test-fn &key (groups t))
    (remove-duplicates
     (remove nil
             (flatten-list (cons (if (and groups (funcall test-fn group))
                                     group
                                     nil)
                                 (mapcar (lambda (child) (find-shapes child test-fn :groups groups))
                                         (children group)))))))

  (:method ((scene-item scene-item) test-fn &key (groups t))
    (declare (ignore groups))
    (if (funcall test-fn scene-item)
        scene-item
        nil))
  )

;;; find-shape-by-name ---------------------------------------------------------

(defgeneric find-shape-by-name (root name)
  
  (:method ((scene scene) name)
    (let ((results (find-shapes scene (lambda (item) (eq name (name item))))))
      (if results
          (first results)
          nil)))

  (:method ((group group) name)
    (let ((results (find-shapes group (lambda (item) (eq name (name item))))))
      (if results
          (first results)
          nil)))
  )

;;; find-shape-by-path ---------------------------------------------------------

#| Not tested and maybe unnecessary

(defgeneric find-shape-by-path (obj shape-path)
  
  (:method ((scene scene) shape-path)
    (if (null shape-path)
        scene
        (let ((child (find (first shape-path) (shapes scene) :key #'name)))
          (if child
              (find-shape-by-path child (rest shape-path))
              nil))))

  (:method ((group group) shape-path)
    (if (null shape-path)
        group
        (let* ((child (find (first shape-path) (children group) :key #'name)))
          (if child
              (find-shape-by-path child (rest shape-path))
              nil))))

  (:method ((scene-item scene-item) shape-path)
    (if (null shape-path)
        scene-item
        nil))
  )
|#

;;; get-shape-paths ------------------------------------------------------------

(defmethod get-shape-paths ((scene scene) (item scene-item))
  (get-shape-paths-aux scene item))

(defgeneric get-shape-paths-aux (obj item)
  
  (:method ((scene scene) item)
    (if (eq scene item)
        '()
        (let ((paths ()))
          (dolist (child (shapes scene))
            (let ((path (get-shape-paths-aux child item)))
              (when path
                (push path paths))))
          (cleanup-nested-path-list paths))))

  (:method ((group group) item)
    (if (eq group item)
        (list (name item))
        (let ((result ()))
          (dolist (child (children group))
            (let ((path (get-shape-paths-aux child item)))
              (when path
                (push (mapcar (lambda (p) (cons (name group) (flatten-list p))) path) result))))
          result)))

  (:method ((scene-item scene-item) item)
    (if (eq scene-item item)
        (list (name item))
        nil))
  )

;;; shape-global-matrix --------------------------------------------------------

(defmethod shape-global-matrix ((scene scene) shape-path)
  (let ((matrix-list (get-shape-matrix-list scene shape-path)))
    (if matrix-list
        (apply #'matrix-multiply-n matrix-list)
        (error "Shape not found for scene path ~a" shape-path))))

(defgeneric get-shape-matrix-list (obj shape-path)
  
  (:method ((scene scene) shape-path)
    (if (null shape-path)
        (make-id-matrix)
        (let ((child (find (first shape-path) (shapes scene) :key #'name)))
          (if child
              (get-shape-matrix-list child (rest shape-path))
              nil))))

  (:method ((group group) shape-path)
    (if (null shape-path)
        (list (transform-matrix (transform group)))
        (let* ((child (find (first shape-path) (children group) :key #'name)))
          (if child
              (cons (transform-matrix (transform group))
                    (get-shape-matrix-list child (rest shape-path)))
              nil))))

  (:method ((shape shape) shape-path)
    (if (null shape-path)
        (list (transform-matrix (transform shape)))
        nil))
  )

;;; remove-shape-path ----------------------------------------------------------

;; TODO -- cf remove-selection
;; -- what to do if item has no parent? -- replace scene shapes with root shape group?
;; (defmethod remove-shape-path ((scene scene) shape-path)
;;   (let ((item (scene-path-item shape-path))
;;         (parent (scene-path-parent-item shape-path)))
;;     (when (and item parent)
;;       (remove-child parent item))))      

;;;; scene motion hierarchy functions ==========================================

;;; find-motions ----------------------------------------------------------------

(defgeneric find-motions (root test-fn &key groups)
  
  (:method ((scene scene) test-fn &key (groups t))
    (remove-duplicates
     (remove nil
             (flatten-list (mapcar (lambda (child) (find-motions child test-fn :groups groups))
                                   (motions scene))))))

  (:method ((group motion-group) test-fn &key (groups t))
    (remove-duplicates
     (remove nil
             (flatten-list (cons (if (and groups (funcall test-fn group))
                                     group
                                     nil)
                                 (mapcar (lambda (child) (find-motions child test-fn :groups groups))
                                         (children group)))))))

  (:method ((scene-item scene-item) test-fn &key (groups t))
    (declare (ignore groups))
    (if (funcall test-fn scene-item)
        scene-item
        nil))
  )

;;; find-motion-by-name ---------------------------------------------------------

(defgeneric find-motion-by-name (root name)
  
  (:method ((scene scene) name)
    (let ((results (find-motions scene (lambda (item) (eq name (name item))))))
      (if results
          (first results)
          nil)))

  (:method ((group motion-group) name)
    (let ((results (find-motions group (lambda (item) (eq name (name item))))))
      (if results
          (first results)
          nil)))
  )

;;; find-motion-by-path ---------------------------------------------------------

#| Not tested and maybe unnecessary

(defgeneric find-motion-by-path (obj motion-path)
  
  (:method ((scene scene) motion-path)
    (if (null motion-path)
        scene
        (let ((child (find (first motion-path) (motions scene) :key #'name)))
          (if child
              (find-motion-by-path child (rest motion-path))
              nil))))

  (:method ((group motion-group) motion-path)
    (if (null motion-path)
        group
        (let* ((child (find (first motion-path) (children group) :key #'name)))
          (if child
              (find-motion-by-path child (rest motion-path))
              nil))))

  (:method ((scene-item scene-item) motion-path)
    (if (null motion-path)
        scene-item
        nil))
  )
|#

;;; get-motion-paths ------------------------------------------------------------

(defmethod get-motion-paths ((scene scene) (item scene-item))
  (get-motion-paths-aux scene item))

(defgeneric get-motion-paths-aux (obj item)
  
  (:method ((scene scene) item)
    (if (eq scene item)
        ()
        (let ((paths ()))
          (dolist (child (motions scene))
            (let ((path (get-motion-paths-aux child item)))
              (when path
                (push path paths))))
          (cleanup-nested-path-list paths))))

  (:method ((group motion-group) item)
    (if (eq group item)
        (list (name item))
        (let ((result ()))
          (dolist (child (children group))
            (let ((path (get-motion-paths-aux child item)))
              (when path
                (push (mapcar (lambda (p) (cons (name group) (flatten-list p))) path) result))))
          result)))

  (:method ((scene-item scene-item) item)
    (if (eq scene-item item)
        (list (name item))
        nil))
  )

;;; motion-global-timing -------------------------------------------------------

#| Not tested and maybe unnecessary

(defmethod motion-global-timing ((scene scene) motion-path)
  (let ((timing (get-motion-absolute-timing-aux scene motion-path nil)))
    (if timing
        timing
        (error "Motion not found for motion path ~a" motion-path))))

(defgeneric motion-global-timing-aux (obj motion-path parent-absolute-timing)
  
  (:method ((scene scene) motion-path parent-absolute-timing)
    (declare (ignore parent-absolute-timing))
    (let ((timing (vector (start-time scene) (- (end-time scene) (start-time scene)))))
      (if (null motion-path)
          timing
          (let ((child (find (first motion-path) (motions scene) :key #'name)))
            (if child
                (motion-global-timing-aux child (rest motion-path) timing)
                nil)))))

  (:method ((motion motion-group) motion-path parent-absolute-timing)
    (let ((timing (compute-motion-absolute-timing motion parent-absolute-timing)))
      (if (null motion-path)
          timing
          (let* ((child (find (first motion-path) (children motion) :key #'name)))
            (if child
                (motion-global-timing-aux child (rest motion-path) timing)
                nil)))))

  (:method ((motion motion) motion-path parent-absolute-timing)
    (if (null motion-path)
        (compute-motion-absolute-timing motion parent-absolute-timing)
        nil))
  )
|#
