(in-package :blox2d)

(defstruct contact-id
  (reference-edge 0 :type (unsigned-byte 8))
  (incident-edge 0 :type (unsigned-byte 8))
  (incident-vertex 0 :type (unsigned-byte 8))
  (flip 0 :type (signed-byte 8)))

(defstruct manifold-point
  (local-point (vec2 0 0) :type vec2)
  (normal-impulse 0.0 :type single-float)
  (tangent-impulse 0.0 :type single-float)
  (id (make-contact-id) :type contact-id))

(deftype manifold-type ()
  '(enum :circles :faceA :faceB))

(defstruct manifold
  (points (make-array +max-manifold-points+ :initial-contents (list (make-manifold-point)
                                                (make-manifold-point)))
          :type (simple-array manifold-point (#.+max-manifold-points+)))
  (local-plane-normal (vec2 0 0) :type vec2)
  (local-point (vec2 0 0) :type vec2)
  (type :circles :type manifold-type)
  (point-count 0 :type (signed-byte 32)))

(defstruct world-manifold
  (normal (vec2 0 0) :type vec2)
  (points (make-array +max-manifold-points+
                      :initial-contents (repeat (vec2 0 0) +max-manifold-points+))
          :type (simple-array vec2 (#.+max-manifold-points+))))

(defun manifold->world-manifold (manifold
                                 transform1 radius1
                                 transform2 radius2)
  (let ((normal) (points (make-array +max-manifold-points+
                                     :initial-contents (repeat (vec2 0 0) +max-manifold-points+))))
    (when (not (= 0 (manifold-point-count manifold)))
      (case (manifold-type manifold)
        (:circles
         (progn
           (let ((pointA (transform-mult transform1 (manifold-local-point manifold)))
                 (pointB (transform-mult transform2 (manifold-point-local-point (aref (manifold-points manifold) 0)))))
             (if (> (vec2-distance-squared pointA pointB)
                    (expt single-float-epsilon 2))
                 (setf normal (vec2-normalized (vec2- pointA pointB)))
                 (setf normal (vec2 1 0)))
             (setf (aref points 0) (vec2* (vec2+
                                           (vec2+ pointA (vec2* normal radius1))
                                           (vec2- pointB (vec2* normal radius2)))
                                          0.5)))))
        (:faceA
         (progn
           (setf normal (matrix2x2-mult (transform-rotation transform1)
                                        (manifold-local-plane-normal manifold)))
           (dotimes (i (manifold-point-count manifold))
             (let ((clip-point (transform-mult transform2 (manifold-point-local-point (elt (manifold-points manifold) i)))))
               (setf (elt (manifold-points manifold) i)
                     (vec2*
                      (vec2+
                       (vec2+
                        clip-point
                        (vec2* normal
                               (- radius1
                                  (dot
                                   (- clip-point (transform-mult
                                                  transform1
                                                  (manifold-local-point manifold)))
                                   normal))))
                       (vec2- clip-point (* radius2 normal)))
                      0.5))))))
        (:faceB
         (progn
           (setf normal (vec2- (matrix2x2-mult (transform-rotation transform2)
                                               (manifold-local-plane-normal manifold))))
           (dotimes (i (manifold-point-count manifold))
             (let ((clip-point (transform-mult transform1 (manifold-point-local-point (elt (manifold-points manifold) i)))))
               (setf (elt (manifold-points manifold) i)
                     (vec2*
                      (vec2+
                       (vec2+
                        clip-point
                        (vec2* normal
                               (- radius2
                                  (dot
                                   (- clip-point (transform-mult
                                                  transform2
                                                  (manifold-local-point manifold)))
                                   normal))))
                       (vec2- clip-point (* radius1 normal)))
                      0.5))))))))
    (make-world-manifold :normal normal :points points)))

(deftype point-state ()
  '(enum :null :add :persist :remove))

(defun get-point-states (manifold1 manifold2)
  (let* ((state1 (make-array +max-manifold-points+
                            :initial-contents (repeat :null +max-manifold-points+)))
        (state2 state1))
    (dotimes (i (manifold-point-count manifold1))
      (setf (aref state1 i) :remove)
      (let ((id (manifold-point-id (aref (manifold-points manifold1) i))))
        (dotimes (j (manifold-point-count manifold2))
          (when (equalp id (manifold-point-id (aref (manifold-points manifold2) j)))
            (setf (aref state1 i) :persist)
            (return)))))
    (dotimes (i (manifold-point-count manifold2))
      (setf (aref state2 i) :add)
      (let ((id (manifold-point-id (aref (manifold-points manifold2) i))))
        (dotimes (j (manifold-point-count manifold1))
          (when (equalp id (manifold-point-id (aref (manifold-points manifold1) j)))
            (setf (aref state2 i) :persist)
            (return)))))
    (list state1 state2)))

(defstruct clip-vertex
  (vertex (vec2 0 0) :type vec2)
  (id (make-contact-id) :type contact-id))

(defstruct ray-cast-input
  (point1 (vec2 0 0) :type vec2)
  (point2 (vec2 0 0) :type vec2)
  (max-fraction 0.0 :type single-float))

(defstruct ray-cast-output
  (normal (vec2 0 0) :type vec2)
  (fraction 0.0 :type single-float)
  (hit nil))                            ;bool

(defstruct line-segment
  (point1 (vec2 0 0) :type vec2)
  (point2 (vec2 0 0) :type vec2))

(defun test-segments (a b maxLambda)
  (let* ((s (line-segment-point1 b))
         (r (vec2- (line-segment-point2 b) s))
         (d (vec2- (line-segment-point2 a) (line-segment-point1 a)))
         (n (cross d 1))
         (k_slop (* 100f0 single-float-epsilon))
         (denom (- (dot r n)))
         (b (vec2- s (line-segment-point1 a)))
         (a (dot b n))
         (mu2 (+ (* (- (x-coord r)) (y-coord b)) (* (y-coord r) (x-coord b)))))
    (when (and (> denom k_slop)
               (and (<= 0f0 a) (<= a (* maxLambda denom)))
               (and (<= (* (- k_slop) denom) mu2) (<= mu2 (* denom (+ 1f0 k_slop)))))
      (values t (/ a denom) (vec2-normalized n)))))

(defstruct aabb
  (lower-bound (vec2 0 0) :type vec2)
  (upper-bound (vec2 0 0) :type vec2))
