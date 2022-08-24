(in-package #:kons-9)
 
;;;; sweep-mesh ====================================================

(defclass sweep-mesh (uv-mesh dependency-node-mixin)
  ((profile-curve-generator :accessor profile-curve-generator :initarg :profile-curve-generator :initform nil)
   (profile-curve-index :accessor profile-curve-index :initarg :profile-curve-index :initform nil)
   (path-curve-generator :accessor path-curve-generator :initarg :path-curve-generator :initform nil)
   (path-curve-index :accessor path-curve-index :initarg :path-curve-index :initform nil)
   (twist :accessor twist :initarg :twist :initform 0.0)
   (taper :accessor taper :initarg :taper :initform 1.0)
   (from-end? :accessor from-end? :initarg :from-end? :initform nil)))

(defmethod profile-points ((mesh sweep-mesh))
  (coerce (elt (source-curves (profile-curve-generator mesh)) (profile-curve-index mesh))
          'list))

(defmethod is-profile-closed? ((mesh sweep-mesh))
  (elt (source-curves-closed (profile-curve-generator mesh)) (profile-curve-index mesh)))

(defmethod path-points ((mesh sweep-mesh))
  (coerce (elt (source-curves (path-curve-generator mesh)) (path-curve-index mesh))
          'list))

(defmethod is-path-closed? ((mesh sweep-mesh))
  (elt (source-curves-closed (path-curve-generator mesh)) (path-curve-index mesh)))

(defmethod initialize-instance :after ((mesh sweep-mesh) &rest initargs)
  (declare (ignore initargs))
  (push 'profile-curve-generator (input-slots mesh))
  (push 'path-curve-generator (input-slots mesh)))

(def-procedural-input sweep-mesh profile-curve-generator)
(def-procedural-input sweep-mesh profile-curve-index)
(def-procedural-input sweep-mesh path-curve-generator)
(def-procedural-input sweep-mesh path-curve-index)
(def-procedural-input sweep-mesh twist)
(def-procedural-input sweep-mesh taper)
(def-procedural-input sweep-mesh from-end?)

(def-procedural-output sweep-mesh uv-point-array)
(def-procedural-output sweep-mesh points)
(def-procedural-output sweep-mesh faces)

;;; assumes profile curve has z-axis as normal
(defmethod compute-procedural-node ((mesh sweep-mesh))
  (sweep-extrude-aux mesh
                     (profile-points mesh) (is-profile-closed? mesh)
                     (path-points mesh) (is-path-closed? mesh)
                     :twist (twist mesh) :taper (taper mesh) :from-end? (from-end? mesh)))

(defun make-sweep-mesh (profile-curve-generator profile-curve-index path-curve-generator path-curve-index
                        &rest initargs)
  (apply #'make-instance 'sweep-mesh :profile-curve-generator profile-curve-generator
                                     :profile-curve-index profile-curve-index
                                     :path-curve-generator path-curve-generator
                                     :path-curve-index path-curve-index
                                     initargs))

