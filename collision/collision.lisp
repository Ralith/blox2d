(in-package :blox2d)

(defstruct contact-id
  (reference-edge nil :type (unsigned-byte 8))
  (incident-edge nil :type (unsigned-byte 8))
  (incident-vertex nil :type (unsigned-byte 8))
  (flip nil :type (signed-byte 8)))

(defstruct manifold-point
  (local-point nil :type vec2)
  (normal-impulse nil :type double-float)
  (tangent-impulse nil :type double-float)
  (id nil :type contact-id))

(defstruct manifold
  (points nil :type (simple-vector 2))  ;b2_maxManifoldPoints = 2
  (local-plane-normal nil :type vec2)
  (local-point nil :type vec2)
  (type nil :type symbol)
  (point-count nil :type (signed-byte 32)))

(defstruct world-manifold
  (normal nil :type vec2)
  (points nil :type (simple-vector 2))) ;b2_maxManifoldPoints = 2

(defstruct clip-vertex
  (vertex nil :type vec2)
  (id nil :type contact-id))

(defstruct ray-cast-input
  (point1 nil :type vec2)
  (point2 nil :type vec2)
  (max-fraction nil :type double-float))

(defstruct ray-cast-output
  (normal nil :type vec2)
  (fraction nil :type double-float)
  (hit nil))                            ;bool

(defstruct aabb
  (lower-bound nil :type vec2)
  (upper-bound nil :type vec2))