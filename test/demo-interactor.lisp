(in-package #:kons-9)

#|
These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

An INTERACTOR is a class for handling user interactions. It receives keyboard
input and executes some actions.

The demos below illustrate some examples of the uses of INTERACTOR.
|#

#|
(Demo 01 interactor) simple keyboard interaction ===============================

Translate a shape using the keyboard.
|#

(format t "  interactor 1...~%") (finish-output)

(with-clear-scene
  (let* ((shape (make-cube 2.0))
         (interactor (make-instance 'interactor
                                    :update-fn (lambda (key key-mods)
                                                 (declare (ignore key-mods))
                                                 (cond ((eq :left key)
                                                        (translate-by shape (p!  .5 0 0)))
                                                       ((eq :right key)
                                                        (translate-by shape (p!  -.5 0 0)))
                                                        ((eq :up key)
                                                        (translate-by shape (p! 0 0 .5)))
                                                       ((eq :down key)
                                                        (translate-by shape (p! 0 0 -.5)))
                                                       )))))
    (add-shape *scene* shape)
    (setf (interactor *scene*) interactor)))

#|
(Demo 02 interactor) snake =====================================================

Implement a simple snake in the XZ plane. Very fast: running at full speed.
|#

(format t "  interactor 2...~%") (finish-output)

(with-clear-scene
  (let* ((loc (p! 0 0 0))
         (vel (p! 0 0 0))
         (interactor (make-instance 'interactor
                                    :update-fn (lambda (key key-mods)
                                                 (declare (ignore key-mods))
                                                 ;; get keyboard input
                                                 (cond ((eq :left  key) (setf vel (p!  .5 0   0)))
                                                       ((eq :right key) (setf vel (p! -.5 0   0)))
                                                       ((eq :up    key) (setf vel (p!   0 0  .5)))
                                                       ((eq :down  key) (setf vel (p!   0 0 -.5))))
                                                 ;; do action
                                                 (when (not (eq vel +origin+))
                                                   (setf loc (p:+ loc vel))
                                                   (add-shape *scene*
                                                              (translate-to (make-cube 0.5) loc)))))))
    (setf (interactor *scene*) interactor)))

#|
(Demo 03 interactor) snake with collisions =====================================

Implement a simple snake in the XZ plane, with collision detection. Slowed down
for easier gameplay using (sleep 0.05) -- 1/20 second.

Play field is hardwired to 50x50 units. Hitting the edge is a crash.
|#

(format t "  interactor 3...~%") (finish-output)

(defun init-field-boundary (field)
  (dotimes (i (array-dimension field 0))
    (setf (aref field i 0) t)
    (setf (aref field i (1- (array-dimension field 1))) t))
  (dotimes (j (array-dimension field 1))
    (setf (aref field 0 j) t)
    (setf (aref field (1- (array-dimension field 0)) j) t)))

(with-clear-scene
  (let* ((loc (p! 0 0 0))
         (vel (p! 0 0 0))
         (i 50)
         (j 50)
         (alive? t)
         (field (make-array '(101 101) :initial-element nil))
         (interactor (make-instance 'interactor
                                    :setup-fn (lambda ()
                                                (init-field-boundary field))
                                    :update-fn (lambda (key key-mods)
                                                 (declare (ignore key-mods))
                                                 (when alive?
                                                   ;; timing for gameplay
                                                   (sleep 0.05)
                                                   ;; get keyboard input
                                                   (cond ((eq :left  key) (setf vel (p!  .5 0   0)))
                                                         ((eq :right key) (setf vel (p! -.5 0   0)))
                                                         ((eq :up    key) (setf vel (p!   0 0  .5)))
                                                         ((eq :down  key) (setf vel (p!   0 0 -.5))))
                                                   ;; do action
                                                   (when (not (p:= vel +origin+))
                                                     (setf loc (p:+ loc vel))
                                                     (cond ((> (p:x vel) 0) (incf i))
                                                           ((< (p:x vel) 0) (decf i))
                                                           ((> (p:z vel) 0) (incf j))
                                                           ((< (p:z vel) 0) (decf j)))
                                                     (if (aref field i j)
                                                         (progn
                                                           (setf alive? nil)
                                                           (print 'crash))
                                                         (progn
                                                           (add-shape *scene*
                                                                      (translate-to (make-cube 0.5) loc))
                                                           (setf (aref field i j) t)))))))))
    (setf (interactor *scene*) interactor)))

#|
(Demo 04 interactor) snake with collisions and explosion =======================

Combine interaction and animators.

Play field is hardwired to 50x50 units. Hitting the edge is a crash.
|#

(format t "  interactor 4...~%") (finish-output)

(defun init-field-boundary (field)
  (dotimes (i (array-dimension field 0))
    (setf (aref field i 0) t)
    (setf (aref field i (1- (array-dimension field 1))) t))
  (dotimes (j (array-dimension field 1))
    (setf (aref field 0 j) t)
    (setf (aref field (1- (array-dimension field 0)) j) t)))

(defun create-explosion (loc)
  (let ((shapes '()))
    (dotimes (i 100) (push (make-cube 0.2) shapes))
    (add-shape *scene* (make-shape-group shapes))
    (add-motions *scene*
                 (mapcar (lambda (s)
                           (translate-to s loc)
                           (make-instance 'dynamics-animator
                                          :shape s
                                          :velocity (p-rand2 (p! -.2 .4 -.2) (p! .2 .6 .2))
                                          :do-collisions? t
                                          :collision-padding 0.1
                                          :elasticity 0.5
                                          :force-fields (list (make-instance 'constant-force-field
                                                                             :force-vector (p! 0 -.02 0)))))
                           shapes))))

(with-clear-scene
  (let* ((loc (p! 0 0 0))
         (vel (p! 0 0 0))
         (i 50)
         (j 50)
         (alive? t)
         (field (make-array '(101 101) :initial-element nil))
         (interactor (make-instance 'interactor
                                    :setup-fn (lambda ()
                                                (init-field-boundary field))
                                    :update-fn (lambda (key key-mods)
                                                 (declare (ignore key-mods))
                                                 (when alive?
                                                   ;; timing for gameplay
                                                   (sleep 0.05)
                                                   ;; get keyboard input
                                                   (cond ((eq :left  key) (setf vel (p!  .5 0   0)))
                                                         ((eq :right key) (setf vel (p! -.5 0   0)))
                                                         ((eq :up    key) (setf vel (p!   0 0  .5)))
                                                         ((eq :down  key) (setf vel (p!   0 0 -.5))))
                                                   ;; do action
                                                   (when (not (p:= vel +origin+))
                                                     (setf loc (p:+ loc vel))
                                                     (cond ((> (p:x vel) 0) (incf i))
                                                           ((< (p:x vel) 0) (decf i))
                                                           ((> (p:z vel) 0) (incf j))
                                                           ((< (p:z vel) 0) (decf j)))
                                                     (if (aref field i j)
                                                         (progn
                                                           (setf alive? nil)
                                                           (create-explosion loc))
                                                         (progn
                                                           (add-shape *scene*
                                                                      (translate-to (make-cube 0.5) loc))
                                                           (setf (aref field i j) t)))))))))
    (setf (interactor *scene*) interactor)))

#|
(Demo 05 interactor) snake with collisions and large explosion =================

Combine interaction and animators. Make entire snake body explode.

Play field is hardwired to 50x50 units. Hitting the edge is a crash.
|#

(format t "  interactor 4...~%") (finish-output)

(defun init-field-boundary (field)
  (dotimes (i (array-dimension field 0))
    (setf (aref field i 0) t)
    (setf (aref field i (1- (array-dimension field 1))) t))
  (dotimes (j (array-dimension field 1))
    (setf (aref field 0 j) t)
    (setf (aref field (1- (array-dimension field 0)) j) t)))

(defun create-explosion (shape-array)
  (add-motions *scene*
               (map 'list (lambda (s)
                            (make-instance 'dynamics-animator
                                           :shape s
                                           :velocity (p-rand2 (p! -.2 .4 -.2) (p! .2 .6 .2))
                                           :do-collisions? t
                                           :collision-padding 0.1
                                           :elasticity 0.5
                                           :force-fields (list (make-instance 'constant-force-field
                                                                              :force-vector (p! 0 -.02 0)))))
                           shape-array)))

(with-clear-scene
  (let* ((loc (p! 0 0 0))
         (vel (p! 0 0 0))
         (i 50)
         (j 50)
         (alive? t)
         (field (make-array '(101 101) :initial-element nil))
         (interactor (make-instance 'interactor
                                    :setup-fn (lambda ()
                                                (init-field-boundary field))
                                    :update-fn (lambda (key key-mods)
                                                 (declare (ignore key-mods))
                                                 (when alive?
                                                   ;; timing for gameplay
                                                   (sleep 0.05)
                                                   ;; get keyboard input
                                                   (cond ((eq :left  key) (setf vel (p!  .5 0   0)))
                                                         ((eq :right key) (setf vel (p! -.5 0   0)))
                                                         ((eq :up    key) (setf vel (p!   0 0  .5)))
                                                         ((eq :down  key) (setf vel (p!   0 0 -.5))))
                                                   ;; do action
                                                   (when (not (p:= vel +origin+))
                                                     (setf loc (p:+ loc vel))
                                                     (cond ((> (p:x vel) 0) (incf i))
                                                           ((< (p:x vel) 0) (decf i))
                                                           ((> (p:z vel) 0) (incf j))
                                                           ((< (p:z vel) 0) (decf j)))
                                                     (if (aref field i j)
                                                         (progn
                                                           (setf alive? nil)
                                                           (create-explosion (children (shape-root *scene*))))
                                                         (progn
                                                           (add-shape *scene*
                                                                      (translate-to (make-cube 0.5) loc))
                                                           (setf (aref field i j) t)))))))))
    (setf (interactor *scene*) interactor)))


#|
END ============================================================================
|#
