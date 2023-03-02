(in-package #:kons-9)

#|
These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

The ISOSURFACE class is a polyhedral mesh class which generates a surface at
a given threshold of a SCALAR-FIELD.

ISOSURFACE inherits from POLYHEDRON and internally creates polygonal (triangle)
faces. In this way it renders itself as a POLYHEDRON.

In these demos we use a signed distance function with APPLY-FIELD-FUNCTION
to set the values of a SCALAR-FIELD, then generate an ISOSURFACE from the field.
|#

;;; settings for SDF scenes
(progn
  (setf *do-smooth-shading?* t)
  (setf *display-wireframe?* nil)
  (setf *display-points?* nil)
  ;; turn off backface culling as isosurfaces are reversed for some reason
  (setf *do-backface-cull?* nil)
  (defparameter *sdf-grid-size* 100))

#|
(Demo 01 sdf) sd-sphere ========================================================
|#
(format t "  sdf 01...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-sphere p 1.0))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 02 sdf) sd-box ===========================================================
|#
(format t "  sdf 02...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-box p (p! 0.2 0.4 0.8)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 03 sdf) sd-round-box =====================================================
|#
(format t "  sdf 03...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-round-box p (p! 0.2 0.4 0.8) 0.15))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 04 sdf) sd-box-frame =====================================================
|#
(format t "  sdf 04...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-box-frame p (p! 0.5 0.3 0.5) .05))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 05 sdf) sd-torus =========================================================
|#
(format t "  sdf 05...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-torus p (p! .5 .2 0.0)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 06 sdf) sd-capped-torus ==================================================
|#
(format t "  sdf 06...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-capped-torus p (p! .5 -.866 0.0) 0.4 0.1))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 07 sdf) sd-link ==========================================================
|#
(format t "  sdf 07...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-link p 0.13 0.2 0.09))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 08 sdf) sd-cylinder ======================================================
|#
(format t "  sdf 08...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-cylinder p (p! 0 0 0.5)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 09 sdf) sd-cone ==========================================================
|#
(format t "  sdf 09...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-cone p (p! .5 .5 0) 1.5))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 10 sdf) sd-plane =========================================================
|#
(format t "  sdf 10...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-plane p (p! .707 .707 0) 0))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 11 sdf) sd-tri-prism =====================================================
|#
(format t "  sdf 11...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-tri-prism p (p! 1 1 0)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 12 sdf) sd-vertical-capsule ==============================================
|#
(format t "  sdf 12...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-vertical-capsule p .7 .2))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 13 sdf) sd-capped-cylinder ===============================================
|#
(format t "  sdf 13...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-capped-cylinder p .7 .2))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 14 sdf) sd-cut-hollow-sphere =============================================
|#
(format t "  sdf 14...~%") (finish-output)

(with-clear-scene
  (let* ((field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (sd-cut-hollow-sphere p 0.8 .3 .1))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso)))

#|
(Demo 15 sdf) sd-op-union ======================================================
|#
(format t "  sdf 15...~%") (finish-output)

(with-clear-scene
  (let* ((mtx (make-z-rotation-matrix (/ pi 4)))
         (field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (let ((d1 (sd-capped-cylinder (transform-point p mtx)
                                                                      1.0 .25))
                                              (d2 (sd-sphere p 0.75)))
                                          (sd-op-union d1 d2)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 16 sdf) sd-op-subtraction ================================================
|#
(format t "  sdf 16...~%") (finish-output)

(with-clear-scene
  (let* ((mtx (make-z-rotation-matrix (/ pi 4)))
         (field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (let ((d1 (sd-capped-cylinder (transform-point p mtx)
                                                                      1.0 .25))
                                              (d2 (sd-sphere p 0.75)))
                                          (sd-op-subtraction d1 d2)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 17 sdf) sd-op-intersection ===============================================
|#
(format t "  sdf 17...~%") (finish-output)

(with-clear-scene
  (let* ((mtx (make-z-rotation-matrix (/ pi 4)))
         (field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (let ((d1 (sd-capped-cylinder (transform-point p mtx)
                                                                      1.0 .25))
                                              (d2 (sd-sphere p 0.75)))
                                          (sd-op-intersection d1 d2)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 18 sdf) sd-op-smooth-union ===============================================
|#
(format t "  sdf 18...~%") (finish-output)

(with-clear-scene
  (let* ((mtx (make-z-rotation-matrix (/ pi 4)))
         (field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (let ((d1 (sd-capped-cylinder (transform-point p mtx)
                                                                      1.0 .25))
                                              (d2 (sd-sphere p 0.75)))
                                          (sd-op-smooth-union d1 d2 0.2)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 19 sdf) sd-op-smooth-subtraction =========================================
|#
(format t "  sdf 19...~%") (finish-output)

(with-clear-scene
  (let* ((mtx (make-z-rotation-matrix (/ pi 4)))
         (field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (let ((d1 (sd-capped-cylinder (transform-point p mtx)
                                                                      1.0 .25))
                                              (d2 (sd-sphere p 0.75)))
                                          (sd-op-smooth-subtraction d1 d2 0.2)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso))
  )

#|
(Demo 20 sdf) sd-op-smooth-intersection ========================================
|#
(format t "  sdf 20...~%") (finish-output)

(with-clear-scene
  (let* ((mtx (make-z-rotation-matrix (/ pi 4)))
         (field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (let ((d1 (sd-capped-cylinder (transform-point p mtx)
                                                                      1.0 .25))
                                              (d2 (sd-sphere p 0.75)))
                                          (sd-op-smooth-intersection d1 d2 0.2)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))
    (add-shape *scene* iso))
  )

#|
Profiling test

(require :sb-sprof)

(with-clear-scene
  (let* ((mtx (make-z-rotation-matrix (/ pi 4)))
         (field (apply-field-function (make-scalar-field *sdf-grid-size* *sdf-grid-size* *sdf-grid-size*)
                                      (lambda (p)
                                        (let ((d1 (sd-capped-cylinder (transform-point p mtx)
                                                                      1.0 .25))
                                              (d2 (sd-sphere p 0.75)))
                                          (sd-op-smooth-union d1 d2 0.2)))))
         (iso (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0))))

    (sb-sprof:with-profiling (:max-samples 1000
                              :report :flat
                              :loop t)
      (generate-isosurface (make-instance 'isosurface :field field :threshold 0.0)))
    
    (add-shape *scene* iso))
  )

|#
