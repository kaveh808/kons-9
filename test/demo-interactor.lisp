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

Implement a simple snake in the XZ plane.
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
END ============================================================================
|#
