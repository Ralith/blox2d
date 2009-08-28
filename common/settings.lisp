(in-package :blox2d)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +version+ '(2 0 2))
  (defparameter +max-manifold-points+ 2)
  (defparameter +max-polygon-vertices+ 8)
  (defparameter +fat-aabb-factor+ 1.5f0)
  (defparameter +node-pool-size+ 50)
  (defparameter +max-proxies+ (expt 2 9)) ; Must be a power of 2
  (defparameter +max-pairs+ (* 8 +max-proxies+)) ; Must be a power of 2
  (defparameter +linear-slop+ 0.005f0)
  (defparameter +angular-slop+ (* pi (/ 2f0 180f0)))
  (defparameter +polygon-radius+ (* 2 +linear-slop+))
  (defparameter +max-toi-contacts-per-island+ 32)
  (defparameter +max-toi-joints-per-island+ 32)
  (defparameter +velocity-threshold+ 1f0)
  (defparameter +max-linear-correction+ 0.2f0)
  (defparameter +max-angular-correction+ (* pi (/ 8f0 180f0)))
  (defparameter +max-translation+ 2f0)
  (defparameter +max-translation-squared+ (* +max-translation+ +max-translation+))
  (defparameter +max-rotation+ (* 0.5f0 pi))
  (defparameter +max-rotation-squared+ (* +max-rotation+ +max-rotation+))
  (defparameter +contact-baumgarte+ 0.2f0)
  (defparameter +time-to-sleep+ 0.5f0)
  (defparameter +linear-sleep-tolerance+ 0.01f0)
  (defparameter +angular-sleep-tolerance+ (* pi (/ 2f0 180f0))))


(defun mix-friction (friction1 friction2)
  (sqrt (* friction1 friction2)))

(defun mix-restitution (restitution1 restitution2)
  (if (> restitution1 restitution2)
      restitution1
      restitution2))