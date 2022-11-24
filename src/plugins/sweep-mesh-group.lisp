(in-package #:kons-9)

;;;; sweep-mesh-group ===========================================

(defclass sweep-mesh-group (manager-group)
  ((profile-curve-source :accessor profile-curve-source :initarg :profile-curve-source :initform nil)
   (path-curve-source :accessor path-curve-source :initarg :path-curve-source :initform nil)
   (twist :accessor twist :initarg :twist :initform 0.0)
   (taper :accessor taper :initarg :taper :initform 1.0)
   (from-end? :accessor from-end? :initarg :from-end? :initform nil)))

(defmethod initialize-instance :after ((group sweep-mesh-group) &rest initargs)
  (declare (ignore initargs))
  (push 'profile-curve-source (input-slots group))
  (push 'path-curve-source (input-slots group)))

;;; for now sweep first profile along all path curves
(defmethod compute-procedural-node ((group sweep-mesh-group))
  (set-children group (sweep-extrude (profile-curve-source group)
                                     (path-curve-source group)
                                     :twist (twist group)
                                     :taper (taper group)
                                     :from-end? (from-end? group)))
  group)

(defun make-sweep-mesh-group (profile-curve-source path-curve-source &rest initargs)
  (compute-procedural-node
   (apply #'make-instance 'sweep-mesh-group :profile-curve-source profile-curve-source
                                            :path-curve-source path-curve-source
                                            initargs)))

;;;; gui =======================================================================

(defun single-curve-source-selected? ()
  (let ((shape (selected-shape (scene *scene-view*))))
    (and shape
         (provides-curve-source-protocol? shape))))

(defun sweep-mesh-group-command-table ()
  (let ((table (make-instance `command-table :title "Create Sweep Mesh Group")))
    (ct-make-shape :S "Sweep Mesh Group" (when (single-curve-source-selected?)
                                           (make-sweep-mesh-group (make-circle 0.2 6)
                                                                  (selected-shape (scene *scene-view*))
                                                                  :taper 0.0)))
    table))

(register-dynamic-command-table-entry
 "Context" :S "Create Sweep Mesh Group"
 (lambda () (make-active-command-table (sweep-mesh-group-command-table)))
 #'single-curve-source-selected?)
