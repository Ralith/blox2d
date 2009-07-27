(in-package :blox2d)

(defstruct contact-id
  (reference-edge 0 :type (unsigned-byte 8))
  (incident-edge 0 :type (unsigned-byte 8))
  (incident-vertex 0 :type (unsigned-byte 8))
  (flip 0 :type (signed-byte 8)))

(defstruct manifold-point
  (local-point +zero-vec2+ :type vec2)
  (normal-impulse 0d0 :type double-float)
  (tangent-impulse 0d0 :type double-float)
  (id (make-contact-id) :type contact-id))

(defstruct manifold
  (points 0 :type (simple-vector 2))  ;b2_maxManifoldPoints = 2
  (local-plane-normal +zero-vec2+ :type vec2)
  (local-point +zero-vec2+ :type vec2)
  (type 0 :type symbol)
  (point-count 0 :type (signed-byte 32)))

(defstruct world-manifold
  (normal +zero-vec2+ :type vec2)
  (points 0 :type (simple-vector 2))) ;b2_maxManifoldPoints = 2

(defstruct clip-vertex
  (vertex +zero-vec2+ :type vec2)
  (id (make-contact-id) :type contact-id))

(defstruct ray-cast-input
  (point1 +zero-vec2+ :type vec2)
  (point2 +zero-vec2+ :type vec2)
  (max-fraction 0d0 :type double-float))

(defstruct ray-cast-output
  (normal +zero-vec2+ :type vec2)
  (fraction 0d0 :type double-float)
  (hit 0))                            ;bool

(defstruct line-segment
  (point1 +zero-vec2+ :type vec2)
  (point2 +zero-vec2+ :type vec2))

(defstruct axis-aligned-bounding-box
  (lower-bound +zero-vec2+ :type vec2)
  (upper-bound +zero-vec2+ :type vec2))
