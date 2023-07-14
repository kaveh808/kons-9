(in-package #:kons-9)

#|

These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

|#

;;; Object selection ===========================================================

;;; To select objects, a ray is cast out from the camera to the far point under
;;; the mouse cursor. The ray penetrates through all objects in its path.

;;; xs-hit is a list of objects hit by the ray.

;;; xs-miss is a list of objects not hit.

;;; Below are a couple of custom "picking selection" functions to demonstrate
;;; how xs-hit and xs-miss along with the current selection can be used to
;;; decide which objects remain selected in a scene after a ray is cast out.


;;; Example 1 - Laser gun ======================================================

;;; First, create random shapes to shoot at by evaluating the following:

(with-clear-scene
  (flet ((random-shape (size)
           (funcall
            (elt '(make-cube make-octahedron make-icosahedron) (random 3))
            size)))
    (let ((step 1) (shape-size 0.4) (bound 1))
      (do ((x (- bound) (+ x step)))
          ((> x bound))
        (do ((y (- bound) (+ y step)))
            ((> y bound))
          (do ((z (- bound) (+ z step)))
              ((> z bound))
            (add-shape (scene *scene-view*)
                       (translate-to (random-shape shape-size) (p! x y z)))))))))

;;; Next, assign a function to global variable *picking-selector*. Normally,
;;; *picking-selector* remains nil and in which case the default inbuilt
;;; selector function is used.

;;; This function should have `(&key xs-hit xs-miss xs-current)` as its expected
;;; arguments (lambda list).  This function gets called by kons-9 whenever its
;;; time to decide which objects make up the new selection of the scene after a
;;; object picking ray is cast out.

;;; The list returned by the function becomes the new scene selection.

;;; btw, for this demo shooting an object only selects it.

;;; To act like a laser gun, shooting should select all xs-hit but we also
;;; maintain previously hit objects. So we need to return a concatenation of
;;; xs-hit and xs-current.

;;; xs-current which is also passed as an argument to this function is a list of
;;; all items currently selected in the scene.

(setf *picking-selector*
      (lambda (&key xs-hit xs-miss xs-current)
        (declare (ignore xs-miss))
        ;; laser shoot - append to the current selection all objects which were
        ;; hit by the laser.
        (concatenate 'list xs-current xs-hit)))

;;; Now, try clicking on objects in the scene to shoot them!

;;; At anytime to clear selections, evaluate:

(clear-selection *scene*)

;;; You can also try rotating the scene until many objects line up and then
;;; shoot all aligned objects in one shot.


;;; Example 2 - All except closest =============================================

;;; This selector function will select all items in the scene except for the
;;; closest hit.

(setf *picking-selector*
      (lambda (&key xs-hit xs-miss xs-current)
        (declare (ignore xs-current))
        ;; select everything except the closest hit
        (when (> (length xs-hit) 0)
          (concatenate 'list (cdr xs-hit) xs-miss))))


;;; Once done, you can bring back the default selector using:

(setf *picking-selector* nil)

;;; Default selectors ==========================================================

;;; By default, there are actually two selector functions, not one:

;;; 1. `picking-selector-click-1` - This selector function comes into effect
;;; when a left click is done without using any modifier keys such as shift or
;;; control. Only one object gets selected at a time.

;;; Apart from selecting only one object at a time, this function selects a
;;; different object on each subsequent click in the case when multiple objects
;;; are in the line of the picking ray. This behaviour also makes it easy to
;;; choose objects which are extremely close to each other.

;;; To bring in the default picking behaviour set *picking-selector* to nil:

(setf *picking-selector* nil)

;;; Populate the scene with closely placed cubes and try single left clicks
;;; multiple times to select the cubes. As long as the cubes are oriented such
;;; that they all in the line of the ray, they will each get their turn in being
;;; selected.

(with-clear-scene 
  (add-shapes (scene *scene-view*)
              (list
               (translate-to (make-cube 1) (p! 0 0 0))
               (translate-to (make-cube 1) (p! -0.05 0.05 -0.05))
               (translate-to (make-cube 1) (p! -0.1 0.1 -0.1)))))

;;; 2. `picking-selector-click-multi` - This selector function comes into effect
;;; when a left click occurs while the shift key was pressed down. The behaviour
;;; is to `add` to the current selection, the closest unselected object.
