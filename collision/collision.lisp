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

(defstruct manifold
  (points 0 :type (simple-vector 2))  ;b2_maxManifoldPoints = 2
  (local-plane-normal (vec2 0 0) :type vec2)
  (local-point (vec2 0 0) :type vec2)
  (type 0 :type symbol)
  (point-count 0 :type (signed-byte 32)))

(defstruct world-manifold
  (normal (vec2 0 0) :type vec2)
  (points 0 :type (simple-vector 2))) ;b2_maxManifoldPoints = 2

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
  (hit 0))                            ;bool

(defstruct line-segment
  (point1 (vec2 0 0) :type vec2)
  (point2 (vec2 0 0) :type vec2))

(defstruct axis-aligned-bounding-box
  (lower-bound (vec2 0 0) :type vec2)
  (upper-bound (vec2 0 0) :type vec2))
