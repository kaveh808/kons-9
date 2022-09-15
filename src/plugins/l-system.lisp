(in-package #:kons-9)

;;;; turtle ====================================================================

(defclass turtle ()
  ((location :accessor location :initarg :location :initform +origin+)
   (heading :accessor heading :initarg :heading :initform +y-axis+)
   (paths :accessor paths :initarg :paths :initform '(()))
   (stack :accessor stack :initarg :stack :initform '())))

(defmethod turtle-init ((self turtle) location heading)
  (setf (location self) location)
  (setf (heading self) heading)
  (setf (paths self) (list (list location)))
  (setf (stack self) '())
  self)

(defmethod turtle-forward ((turtle turtle) &optional (distance 1.0))
  (setf (location turtle) (p:+ (location turtle) (p:scale (heading turtle) distance)))
  (push (location turtle) (first (paths turtle))))

(defmethod turtle-rotate ((turtle turtle) &optional (angle 90.0))
  (let ((matrix (make-z-rotation-matrix (radians angle))))
    (setf (heading turtle) (transform-point (heading turtle) matrix))))

(defmethod turtle-push ((turtle turtle))
  (push (list (location turtle) (heading turtle)) (stack turtle)))

(defmethod turtle-pop ((turtle turtle))
  (let ((data (pop (stack turtle))))
    (setf (location turtle) (first data))
    (setf (heading turtle) (second data)))
  (push '() (paths turtle))             ;new path
  (push (location turtle) (first (paths turtle))))

(defmethod turtle-move ((turtle turtle) command-list)
  (mapc #'(lambda (command) (funcall command turtle)) command-list))

;;;; l-system ==================================================================

;;; override source-curves-closed to return nil

;;; close to particle-system ... implement as subclass? -- turtle as particle type?

(defclass l-system (polyhedron animator)
  ((axiom :accessor axiom :initarg :axiom :initform '())
   (initial-location :accessor initial-location :initarg :initial-location :initform +origin+)
   (initial-heading :accessor initial-heading :initarg :initial-heading :initform +y-axis+)
   (production-rules :accessor production-rules :initarg :production-rules :initform '())
   (graphics-rules :accessor graphics-rules :initarg :graphics-rules :initform '())
   (genome :accessor genome :initarg :genome :initform '())
   (turtle :accessor turtle :initarg :turtle :initform nil)))

(defun apply-production-rules (genome rules)
  (let ((new-genome '()))
    (dolist (gene genome)
      (let ((rule (assoc gene rules)))
        (if rule
            (setf new-genome (append new-genome (cdr rule)))
            (setf new-genome (append new-genome (list gene))))))
    new-genome))
  
(defun apply-graphics-rules (genome rules)
  (let ((moves '()))
    (dolist (gene genome)
      (let ((rule (assoc gene rules)))
        (when rule
            (push (cadr rule) moves))))
    (nreverse moves)))

(defmethod source-curves-closed ((l-sys l-system))
  (make-list (length (faces l-sys)) :initial-element nil))

(defmethod setup-motion ((l-sys l-system))
  (setf (genome l-sys) '())
  (setf (turtle l-sys)
        (turtle-init (make-instance 'turtle) (initial-location l-sys) (initial-heading l-sys)))
  (empty-polyhedron l-sys)
  nil)

(defmethod update-motion ((l-sys l-system) parent-absolute-timing)
  (declare (ignore parent-absolute-timing))
  (if (null (genome l-sys))
      (setf (genome l-sys) (copy-list (axiom l-sys)))
      (setf (genome l-sys) (apply-production-rules (genome l-sys) (production-rules l-sys))))
  (setf (turtle l-sys)
        (turtle-init (make-instance 'turtle) (initial-location l-sys) (initial-heading l-sys)))
;  (empty-polyhedron l-sys)
  (turtle-move (turtle l-sys) (apply-graphics-rules (genome l-sys) (graphics-rules l-sys)))
  (set-face-point-lists l-sys (mapcar #'reverse (paths (turtle l-sys)))))

(defmethod draw ((l-sys l-system))
  (when *display-wireframe?*
    (draw-wireframe l-sys))
  (when *display-points?*
    (draw-points l-sys)))

(defmethod draw-wireframe ((l-sys l-system))
  (3d-draw-wireframe-polygons (points l-sys) (faces l-sys) :closed? nil))

(defmethod draw-normals ((l-sys l-system))
  ;; do nothing
  )


(defun test-l-system (l-sys)
  (dotimes (i 4)
    (update-animator l-sys)
    (format t "~a: ~a~%" i (genome l-sys))))

(defun make-koch-curve-l-system ()
  (make-instance 'l-system
                 :initial-heading +x-axis+
                 :axiom '(F)
                 :production-rules '((F F + F - F - F + F))
                 :graphics-rules `((F ,#'(lambda (tu) (turtle-forward tu)))
                                   (+ ,#'(lambda (tu) (turtle-rotate tu  90.0)))
                                   (- ,#'(lambda (tu) (turtle-rotate tu -90.0))))))

(defun make-binary-tree-l-system ()
  (make-instance 'l-system
                 :initial-heading +y-axis+
                 :axiom '(0)
                 :production-rules '((1 1 1) (0 1 [ 0 ] 0))
                 :graphics-rules `((0 ,#'(lambda (tu) (turtle-forward tu (rand1 0.2 1.0))))
                                   (1 ,#'(lambda (tu) (turtle-forward tu (rand1 0.2 1.0))))
                                   ([ ,#'(lambda (tu) (turtle-push tu) (turtle-rotate tu (rand1 20.0 45.0))))
                                   (] ,#'(lambda (tu) (turtle-pop tu) (turtle-rotate tu (rand1 20.0 -45.0)))))))

(defun make-serpinski-triangle-l-system ()
  (make-instance 'l-system
                 :initial-heading +y-axis+
                 :axiom '(F - G - G)
                 :production-rules '((F F - G + F + G - F) (G G G))
                 :graphics-rules `((F ,#'(lambda (tu) (turtle-forward tu)))
                                   (G ,#'(lambda (tu) (turtle-forward tu)))
                                   (+ ,#'(lambda (tu) (turtle-rotate tu  120.0)))
                                   (- ,#'(lambda (tu) (turtle-rotate tu -120.0))))))

(defun make-serpinski-arrowhead-l-system ()
  (make-instance 'l-system
                 :initial-heading +y-axis+
                 :axiom '(A)
                 :production-rules '((A B - A - B) (B A + B + A))
                 :graphics-rules `((A ,#'(lambda (tu) (turtle-forward tu)))
                                   (B ,#'(lambda (tu) (turtle-forward tu)))
                                   (+ ,#'(lambda (tu) (turtle-rotate tu  60.0)))
                                   (- ,#'(lambda (tu) (turtle-rotate tu -60.0))))))

(defun make-dragon-curve-l-system ()
  (make-instance 'l-system
                 :initial-heading +y-axis+
                 :axiom '(F)
                 :production-rules '((F F + G) (G F - G))
                 :graphics-rules `((F ,#'(lambda (tu) (turtle-forward tu)))
                                   (G ,#'(lambda (tu) (turtle-forward tu)))
                                   (+ ,#'(lambda (tu) (turtle-rotate tu  90.0)))
                                   (- ,#'(lambda (tu) (turtle-rotate tu -90.0))))))

(defun make-fractal-plant-l-system ()
  (make-instance 'l-system
                 :initial-heading +y-axis+
                 :axiom '(X)
                 :production-rules '((X F + [ [ X ] - X ] - F [ - F X ] + X) (F F F))
                 :graphics-rules `((F ,#'(lambda (tu) (turtle-forward tu (rand1 0.2 1.0))))
                                   (+ ,#'(lambda (tu) (turtle-rotate tu (rand1 5.0 25.0))))
                                   (- ,#'(lambda (tu) (turtle-rotate tu (rand1 5.0 -25.0))))
                                   ([ ,#'(lambda (tu) (turtle-push tu)))
                                   (] ,#'(lambda (tu) (turtle-pop tu))))))

#|
(test-l-system (make-instance 'l-system :axiom '(A)
                                        :production-rules '((A A B) (B A))))
|#
