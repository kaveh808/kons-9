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
    (when *picking-selector*
      (update-scene-selection (current-selection scene)
        (funcall *picking-selector* :xs-hit xs-hit
                                    :xs-miss xs-miss
                                    :xs-current current-selection
                                    )))))

;; object selection functions ==================================================

(defmacro use-picking-selector (f)
  `(setf *picking-selector* ,f))

(defun picking-selector-closest-item (&key xs-hit xs-miss xs-current)
  (declare (ignore xs-miss xs-current))
  (when (car xs-hit)
    (list (car xs-hit))))


(use-picking-selector #'picking-selector-closest-item)
