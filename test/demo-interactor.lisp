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

(format t "  interactor 5...~%") (finish-output)

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
(Demo 06 interactor) Conway's Game of Life =====================================

User selects initial pattern using number keys.
|#

(format t "  interactor 6...~%") (finish-output)

(progn
  (defparameter *field*     (make-array '(20 20) :initial-element 0))
  (defparameter *field-aux* (make-array '(20 20) :initial-element 0))
         
  (defun init-field-random ()
    (dotimes (i (array-dimension *field* 0))
      (dotimes (j (array-dimension *field* 1))
        (setf (aref *field* i j) (if (< (random 1.0) 0.5) 1 0)))))

  (defun init-field-clear ()
    (dotimes (i (array-dimension *field* 0))
      (dotimes (j (array-dimension *field* 1))
        (setf (aref *field* i j) 0))))

  (defun init-field (coords)
    (let* ((i (floor (/ (array-dimension *field* 0) 2)))
           (j (floor (/ (array-dimension *field* 1) 2)))
           (centered-coords (mapcar (lambda (coord) (list (+ i (first coord)) (+ j (second coord))))
                                    coords)))
      (dolist (coord centered-coords)
        (setf (aref *field* (first coord) (second coord)) 1))))

  (defun init-field-glider ()
    (init-field '((0 0) (1 1) (1 2) (0 2) (-1 2))))

  (defun init-field-light-spaceship ()
    (init-field '((1 0) (2 0) (3 0) (4 0) (0 1) (4 1) (4 2) (0 3) (3 3))))

  (defun init-field-medium-spaceship ()
    (init-field '((1 0) (2 0) (3 0) (4 0) (5 0) (0 1) (5 1) (5 2) (0 3) (4 3) (2 4))))

  (defun init-field-heavy-spaceship ()
    (init-field '((1 0) (2 0) (3 0) (4 0) (5 0) (6 0) (0 1) (6 1) (6 2) (0 3) (5 3) (2 4) (3 4))))

  (defun num-live-neighbors (i j)
    (let* ((idim (array-dimension *field* 0))
           (jdim (array-dimension *field* 1))
           (i-1 (mod (1- i) idim))
           (i+1 (mod (1+ i) idim))
           (j-1 (mod (1- j) jdim))
           (j+1 (mod (1+ j) jdim)))
      (+ (aref *field* i-1 j-1)
         (aref *field* i   j-1)
         (aref *field* i+1 j-1)
         (aref *field* i-1 j  )
         (aref *field* i+1 j  )
         (aref *field* i-1 j+1)
         (aref *field* i   j+1)
         (aref *field* i+1 j+1))))

  (defun update-field ()
    (dotimes (i (array-dimension *field* 0))
      (dotimes (j (array-dimension *field* 1))
        (let ((neighbors (num-live-neighbors i j)))
          (setf (aref *field-aux* i j)
                (cond ((and (= 1 (aref *field* i j)) (or (= 2 neighbors) (= 3 neighbors)))
                       1)
                      ((and (= 0 (aref *field* i j)) (= 3 neighbors))
                       1)
                      (t 0))))))
    (let ((tmp *field*))
      (setf *field* *field-aux*)
      (setf *field-aux* tmp)))
  
  (defun field-points ()
    (let ((points (make-array 0 :adjustable t :fill-pointer 0))
          (idim (array-dimension *field* 0))
          (jdim (array-dimension *field* 1)))
      (dotimes (i idim)
        (dotimes (j jdim)
          (when (= 1 (aref *field* i j))
            (vector-push-extend (p! (- i (/ idim 2)) 0 (- j (/ jdim 2)))
                                points))))
      points))
  
  (with-clear-scene
    (let* ((cube (scale-by (make-cube 1.0) (p! 1 .2 1)))
           (instancer (make-point-instancer-group nil cube))
           (interactor (make-instance 'interactor
                                      :setup-fn (lambda ()
                                                  (init-field-random)
                                                  (format t "~%")
                                                  (format t "0: clear field~%")
                                                  (format t "1: random field~%")
                                                  (format t "2: glider~%")
                                                  (format t "3: light spaceship~%")
                                                  (format t "4: medium spaceship~%")
                                                  (format t "5: heavy spaceship~%"))
                                      :update-fn (lambda (key key-mods)
                                                   (declare (ignore key-mods))
                                                   ;; timing for gameplay
                                                   (sleep 0.05)
                                                   ;; get keyboard input
                                                   (cond ((eq :0 key) (init-field-clear))
                                                         ((eq :1 key) (init-field-random))
                                                         ((eq :2 key) (init-field-glider))
                                                         ((eq :3 key) (init-field-light-spaceship))
                                                         ((eq :4 key) (init-field-medium-spaceship))
                                                         ((eq :5 key) (init-field-heavy-spaceship))
                                                         )
                                                   ;; do action
                                                   (update-field)
                                                   (setf (point-source instancer)
                                                         (make-point-cloud (field-points)))))))
      (add-shape *scene* instancer)
      (setf (interactor *scene*) interactor))))


#|
(Demo 07 interactor) Conway's Game of Life in 3D ===============================

User selects initial pattern using number keys.
|#

(format t "  interactor 7...~%") (finish-output)

(progn
  (defparameter *field-3d*     (make-array '(20 20 20) :initial-element 0))
  (defparameter *field-3d-aux* (make-array '(20 20 20) :initial-element 0))
         
  (defun init-field-3d-random ()
    (dotimes (i (array-dimension *field-3d* 0))
      (dotimes (j (array-dimension *field-3d* 1))
        (dotimes (k (array-dimension *field-3d* 2))
          (setf (aref *field-3d* i j k) (if (< (random 1.0) 0.0625) 1 0))))))

  (defun init-field-3d-clear ()
    (dotimes (i (array-dimension *field-3d* 0))
      (dotimes (j (array-dimension *field-3d* 1))
        (dotimes (k (array-dimension *field-3d* 2))
          (setf (aref *field-3d* i j k) 0)))))

  (defun init-field-3d (coords)
    (let* ((i (floor (/ (array-dimension *field-3d* 0) 2)))
           (j (floor (/ (array-dimension *field-3d* 1) 2)))
           (k (floor (/ (array-dimension *field-3d* 2) 2)))
           (centered-coords (mapcar (lambda (coord)
                                      (list (+ i (elt coord 0))
                                            (+ j (elt coord 1))
                                            (+ k (elt coord 2))))
                                    coords)))
      (dolist (coord centered-coords)
        (setf (aref *field-3d* (elt coord 0) (elt coord 1) (elt coord 2)) 1))))

  (defun init-field-3d-stairs ()
    (init-field-3d '((0 0 0) (1 0 0) (1 1 0) (0 1 1))))

  (defun init-field-3d-tube ()
    (init-field-3d '((-1 2 0) (0 2 0) (1 2 0)
                     (-2 1 0) (2 1 0)
                     (-2 0 0) (2 0 0)
                     (-2 -1 0) (2 -1 0)
                     (-1 -2 0) (0 -2 0) (1 -2 0))))

  (defun init-field-3d-cube ()
    (init-field-3d '(( 1  2  1) ( 2  1  1) ( 1  1  2)
                     (-1  2  1) (-2  1  1) (-1  1  2)
                     (-1  2 -1) (-2  1 -1) (-1  1 -2)
                     ( 1  2 -1) ( 2  1 -1) ( 1  1 -2)
                     ( 1 -2  1) ( 2 -1  1) ( 1 -1  2)
                     (-1 -2  1) (-2 -1  1) (-1 -1  2)
                     (-1 -2 -1) (-2 -1 -1) (-1 -1 -2)
                     ( 1 -2 -1) ( 2 -1 -1) ( 1 -1 -2))))

  (defun num-live-neighbors-3d (i j k)
    (let* ((idim (array-dimension *field-3d* 0))
           (jdim (array-dimension *field-3d* 1))
           (kdim (array-dimension *field-3d* 2))
           (i-1 (mod (1- i) idim))
           (i+1 (mod (1+ i) idim))
           (j-1 (mod (1- j) jdim))
           (j+1 (mod (1+ j) jdim))
           (k-1 (mod (1- k) kdim))
           (k+1 (mod (1+ k) kdim)))
      (+ (aref *field-3d* i-1 j-1 k-1)
         (aref *field-3d* i   j-1 k-1)
         (aref *field-3d* i+1 j-1 k-1)
         (aref *field-3d* i-1 j   k-1)
         (aref *field-3d* i   j   k-1)
         (aref *field-3d* i+1 j   k-1)
         (aref *field-3d* i-1 j+1 k-1)
         (aref *field-3d* i   j+1 k-1)
         (aref *field-3d* i+1 j+1 k-1)
         (aref *field-3d* i-1 j-1 k  )
         (aref *field-3d* i   j-1 k  )
         (aref *field-3d* i+1 j-1 k  )
         (aref *field-3d* i-1 j   k  )
         (aref *field-3d* i+1 j   k  )
         (aref *field-3d* i-1 j+1 k  )
         (aref *field-3d* i   j+1 k  )
         (aref *field-3d* i+1 j+1 k  )
         (aref *field-3d* i-1 j-1 k+1)
         (aref *field-3d* i   j-1 k+1)
         (aref *field-3d* i+1 j-1 k+1)
         (aref *field-3d* i-1 j   k+1)
         (aref *field-3d* i   j   k+1)
         (aref *field-3d* i+1 j   k+1)
         (aref *field-3d* i-1 j+1 k+1)
         (aref *field-3d* i   j+1 k+1)
         (aref *field-3d* i+1 j+1 k+1))))

  (defun update-field-3d ()
    (dotimes (i (array-dimension *field-3d* 0))
      (dotimes (j (array-dimension *field-3d* 1))
        (dotimes (k (array-dimension *field-3d* 2))
          (let ((neighbors (num-live-neighbors-3d i j k)))
            (setf (aref *field-3d-aux* i j k)
                  (cond ((and (= 1 (aref *field-3d* i j k)) (or (= 5 neighbors) (= 6 neighbors)))
                         1)
                        ((and (= 0 (aref *field-3d* i j k)) (= 4 neighbors))
                         1)
                        (t 0)))))))
    (let ((tmp *field-3d*))
      (setf *field-3d* *field-3d-aux*)
      (setf *field-3d-aux* tmp)))

  (defun field-3d-points ()
    (let* ((points (make-array 0 :adjustable t :fill-pointer 0))
           (idim (array-dimension *field-3d* 0))
           (jdim (array-dimension *field-3d* 1))
           (kdim (array-dimension *field-3d* 2))
           (idim/2 (/ idim 2))
           (jdim/2 (/ jdim 2))
           (kdim/2 (/ kdim 2)))
      (dotimes (i idim)
        (dotimes (j jdim)
          (dotimes (k kdim)
            (when (= 1 (aref *field-3d* i j k))
              (vector-push-extend (p! (- i idim/2) (- j jdim/2) (- k kdim/2))
                                  points)))))
      points))

  (with-clear-scene
    (let* ((cube (scale-by (make-cube 1.0) (p! 1 1 1)))
           (instancer (make-point-instancer-group nil cube))
           (interactor (make-instance 'interactor
                                      :setup-fn (lambda ()
                                                  (init-field-3d-random)
                                                  (format t "~%")
                                                  (format t "0: clear field~%")
                                                  (format t "1: random field~%")
                                                  (format t "2: stairs~%")
                                                  (format t "3: tube~%")
                                                  (format t "4: cube~%"))
                                      :update-fn (lambda (key key-mods)
                                                   (declare (ignore key-mods))
                                                   ;; timing for gameplay
                                                   (sleep 0.05)
                                                   ;; get keyboard input
                                                   (cond ((eq :0 key) (init-field-3d-clear))
                                                         ((eq :1 key) (init-field-3d-random))
                                                         ((eq :2 key) (init-field-3d-stairs))
                                                         ((eq :3 key) (init-field-3d-tube))
                                                         ((eq :4 key) (init-field-3d-cube))
                                                         )
                                                   ;; do action
                                                   (update-field-3d)
                                                   (setf (point-source instancer)
                                                         (make-point-cloud (field-3d-points)))))))
      (add-shape *scene* instancer)
      (setf (interactor *scene*) interactor)))
  )

#|
END ============================================================================
|#
