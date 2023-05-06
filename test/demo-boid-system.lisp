(in-package #:kons-9)

#|
These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

Make sure you have opened the graphics window by doing:

(in-package :kons-9)
(run)

The BOID-ANIMATOR and BOID-SYSTEM classes simulate creatures with behaviors in
an ecosystem-like setting. It is intended as a test platform for more complex
behaviors.

The demos below sets up some VEGETATION and GRAZER boids which eat them. The
GRAZER boids are in turn eaten by PREDATOR boids.

Boids spend energy each frame and gain energy by eating food. If a boid's
energy falls to zero it dies.

Moving boids wrap around in X and Z to keep them within the BOID-SYSTEM's
bounds.

Press 'space' to run the animation.
|#

#|
(Demo 01 boid-system) vegetation, grazers, and predators =======================
|#
(with-clear-scene
  (setf (end-frame *scene*) 1000)
  (let ((b-sys (make-instance 'boid-system)))
    ;; initial population
    (spawn-boids b-sys 'vegetation 40)
    (spawn-boids b-sys 'grazer 10)
    (spawn-boids b-sys 'predator 2)
    ;; spawn new boids
    (setf (spawn-new-boids-fn b-sys)
          (lambda (sys)
            ;; spawn vegetation
            (when (< (rand1 1.0) 0.01)
              (spawn-boids sys 'vegetation 1))))
    ;; add to scene
    (add-motion *scene* b-sys)
    (add-shape *scene* (shape b-sys))))

;;; for automated testing
(update-scene *scene* 100)

#|
END ============================================================================
|#
