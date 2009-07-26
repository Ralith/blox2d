(in-package :blox2d)

(defstruct contact-id
  (reference-edge nil :type (signed-byte 8))
  (incident-edge nil :type (signed-byte 8))
  (incident-vertex nil :type (signed-byte 8))
  (flip nil :type (signed-byte 8)))

(defstruct manifold-point
  (local-point nil :type vec2)
  (normal-impulse nil :type double-float)
  (tangent-impulse nil :type double-float)
  (id nil :type contact-id))

(defstruct aabb
  (lower-bound nil :type vec2)
  (upper-bound nil :type vec2))