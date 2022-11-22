(in-package #:kons-9)
 
;;;; sweep-mesh ====================================================

(defclass sweep-mesh (uv-mesh dependency-node-mixin)
  ((profile-curve-source :accessor profile-curve-source :initarg :profile-curve-source :initform nil)
   (profile-curve-index :accessor profile-curve-index :initarg :profile-curve-index :initform nil)
   (path-curve-source :accessor path-curve-source :initarg :path-curve-source :initform nil)
   (path-curve-index :accessor path-curve-index :initarg :path-curve-index :initform nil)
   (twist :accessor twist :initarg :twist :initform 0.0)
   (taper :accessor taper :initarg :taper :initform 1.0)
   (from-end? :accessor from-end? :initarg :from-end? :initform nil)))

(defmethod profile-points ((mesh sweep-mesh))
  (elt (source-curves (profile-curve-source mesh)) (profile-curve-index mesh)))

(defmethod is-profile-closed? ((mesh sweep-mesh))
  (elt (source-curves-closed (profile-curve-source mesh)) (profile-curve-index mesh)))

(defmethod path-points ((mesh sweep-mesh))
  (elt (source-curves (path-curve-source mesh)) (path-curve-index mesh)))

(defmethod is-path-closed? ((mesh sweep-mesh))
  (elt (source-curves-closed (path-curve-source mesh)) (path-curve-index mesh)))

(defmethod initialize-instance :after ((mesh sweep-mesh) &rest initargs)
  (declare (ignore initargs))
  (push 'profile-curve-source (input-slots mesh))
  (push 'path-curve-source (input-slots mesh)))

(def-procedural-input sweep-mesh profile-curve-source)
(def-procedural-input sweep-mesh profile-curve-index)
(def-procedural-input sweep-mesh path-curve-source)
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
                     :twist (twist mesh) :taper (taper mesh) :from-end? (from-end? mesh))
  mesh)

(defun make-sweep-mesh (profile-curve-source profile-curve-index path-curve-source path-curve-index
                        &rest initargs)
  (compute-procedural-node
   (apply #'make-instance 'sweep-mesh :profile-curve-source profile-curve-source
                                      :profile-curve-index profile-curve-index
                                      :path-curve-source path-curve-source
                                      :path-curve-index path-curve-index
                                      initargs)))

