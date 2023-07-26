(in-package #:kons-9)

(defparameter *picking-enabled* t)
(defparameter *picking-request* nil)
(defparameter *picking-selector* nil)

(defparameter *object-picking-selection-cone-angle* (/ PI 180))

;; While debugging, it can be useful to see what is actually being
;; intersected. Setting the below debug flag to true causes the selection cone
;; to be drawn along with the points of intersection. Note: The selection cone
;; only becomes visible once the scene is rotated immediately after a pick.
(defparameter *debug-object-picking-selection-cone* nil)

(defmacro make-pick-request (x y multi-select)
  `(when *picking-enabled*
     (setf *picking-request* (list ,x ,y ,multi-select))))

(defmacro when-pick-requested ((from to multi-select) &body body)
  `(progn
     (when *picking-request*
       (let ((screen-x (elt *picking-request* 0))
             (screen-y (elt *picking-request* 1))
             (,multi-select (elt *picking-request* 2)))
         (multiple-value-bind (,from ,to)
             (gl-get-picking-ray-coords screen-x screen-y)
           (setf *picking-request* nil)
           ,@body)))
     (when *debug-object-picking-selection-cone*
       (draw-previous-selection-cone))))

;; Given a scene, the below macro gets the currently selected items. It then
;; sets the scene selection to those items returned by the body form.
(defmacro update-scene-selection ((current-selection scene) &body body)
  (let ((g-scene (gensym))
        (g-new-selection (gensym)))
    `(let* ((,g-scene ,scene)
            (,current-selection (copy-list (selected-shapes ,g-scene))))
       (let ((,g-new-selection (progn ,@body)))
         (when (listp ,g-new-selection)
           (clear-selection ,g-scene)
           (dolist (item ,g-new-selection)
             (add-to-selection ,g-scene item)))))))

(defun pick (from to multi-select scene)
  (flet ((make-ray () (make-instance 'ray :from from :to to))
         (make-cone ()
           (make-instance 'selection-cone
                          :from from
                          :to to
                          :angle *object-picking-selection-cone-angle*)))
    (let ((shapes (find-shapes scene #'identity)))
      (multiple-value-bind (xs-hit xs-miss)
          (intersect-shapes (make-ray) (make-cone) shapes)
        (update-scene-selection (current-selection scene)
          (funcall (choose-picking-selector multi-select)
                   :xs-hit xs-hit
                   :xs-miss xs-miss
                   :xs-current current-selection))))))

(defun intersect-shape (ray cone shape)
  (if (typep shape 'curve)
      (intersect cone shape)
      (intersect ray shape)))

(defun intersect-shapes (ray cone shapes)
  (when *debug-object-picking-selection-cone*
    (set-previous-selection-cone-and-intersects cone))
  (let ((xs-hit-distances '())
        (xs-miss '()))
    (mapc (lambda (shape)
            (let ((distance (intersect-shape ray cone shape)))
              (if distance
                  (push (cons distance shape) xs-hit-distances)
                  (push shape xs-miss))))
          shapes)
    (setf xs-hit-distances (stable-sort xs-hit-distances #'< :key #'car))
    (let ((xs-hit (mapcar #'cdr xs-hit-distances)))
      (values xs-hit xs-miss))))

(defun choose-picking-selector (multi-select)
  (when (functionp *picking-selector*)
    (return-from choose-picking-selector *picking-selector*))

  (if multi-select
      #'picking-selector-click-multi
      #'picking-selector-click-1))

;; picking selector functions ==================================================

(let ((prev-xs-hit nil)
      (i -1))
  (defun picking-selector-click-1 (&key xs-hit xs-miss xs-current)
    (declare (ignore xs-miss xs-current))
    (flet ((next-i ()
             (setf i (mod (+ 1 i) (length xs-hit)))
             i))
      (when (not (equal prev-xs-hit xs-hit))
        (setf prev-xs-hit xs-hit)
        (setf i -1))

      (when (not (null xs-hit))
        (list (elt xs-hit (next-i)))))))

(defun picking-selector-click-multi (&key xs-hit xs-miss xs-current)
  (declare (ignore xs-miss))
  (let ((xs-hit-unselected (list-subtract xs-hit xs-current)))
    (if (null xs-hit-unselected)
        xs-current
        (cons (car xs-hit-unselected) xs-current))))
