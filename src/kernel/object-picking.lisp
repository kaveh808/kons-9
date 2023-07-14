(in-package #:kons-9)

(defparameter *picking-enabled* t)
(defparameter *picking-request* nil)
(defparameter *picking-selector* nil)

(defmacro make-pick-request (x y multi-select)
  `(when *picking-enabled*
     (setf *picking-request* (list ,x ,y ,multi-select))))

(defmacro when-pick-requested ((ray multi-select) &body body)
  `(when *picking-request*
     (let ((,multi-select (elt *picking-request* 2))
           (,ray (make-ray (elt *picking-request* 0)
                           (elt *picking-request* 1))))
       (setf *picking-request* nil)
       ,@body)))

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

(defun make-ray (screen-x screen-y)
  (multiple-value-bind (from to) (gl-get-picking-ray-coords screen-x screen-y)
    (make-instance 'ray :from from :to to)))

(defun pick (ray multi-select scene)
  (multiple-value-bind (xs-hit xs-miss) (intersect ray scene)
    (update-scene-selection (current-selection scene)
      (funcall (choose-picking-selector multi-select)
               :xs-hit xs-hit
               :xs-miss xs-miss
               :xs-current current-selection
               ))))

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
