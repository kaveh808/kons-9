(in-package #:kons-9)

;;;; sweep-mesh-group ===========================================

(defclass sweep-mesh-group (manager-group)
  ((profile-curve-generator :accessor profile-curve-generator :initarg :profile-curve-generator :initform nil)
   (path-curve-generator :accessor path-curve-generator :initarg :path-curve-generator :initform nil)
   (twist :accessor twist :initarg :twist :initform 0.0)
   (taper :accessor taper :initarg :taper :initform 1.0)
   (from-end? :accessor from-end? :initarg :from-end? :initform nil)))

(defmethod initialize-instance :after ((group sweep-mesh-group) &rest initargs)
  (declare (ignore initargs))
  (push 'profile-curve-generator (input-slots group))
  (push 'path-curve-generator (input-slots group)))

;;; for now sweep 0-th profile along all path curves
(defmethod compute-procedural-node ((group sweep-mesh-group))
  (set-children group (sweep-extrude (profile-curve-generator group)
                                     (path-curve-generator group)
                                     :twist (twist group)
                                     :taper (taper group)
                                     :from-end? (from-end? group))))

(defun make-sweep-mesh-group (profile-curve-generator path-curve-generator &rest initargs)
  (apply #'make-instance 'sweep-mesh-group :profile-curve-generator profile-curve-generator
                                           :path-curve-generator path-curve-generator
                                           initargs))
