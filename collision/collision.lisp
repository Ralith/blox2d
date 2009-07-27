(in-package :blox2d)

(defstruct contact-id
  (reference-edge 0 :type (unsigned-byte 8))
  (incident-edge 0 :type (unsigned-byte 8))
  (incident-vertex 0 :type (unsigned-byte 8))
  (flip 0 :type (signed-byte 8)))

(defstruct manifold-point
  (local-point #(0 0) :type vec2)
  (normal-impulse 0 :type double-float)
  (tangent-impulse 0 :type double-float)
  (id 0 :type contact-id))

(defstruct manifold
  (points 0 :type (simple-vector 2))  ;b2_maxManifoldPoints = 2
  (local-plane-normal #(0 0) :type vec2)
  (local-point #(0 0) :type vec2)
  (type 0 :type symbol)
  (point-count 0 :type (signed-byte 32)))

(defstruct world-manifold
  (normal #(0 0) :type vec2)
  (points 0 :type (simple-vector 2))) ;b2_maxManifoldPoints = 2

(defstruct clip-vertex
  (vertex #(0 0) :type vec2)
  (id 0 :type contact-id))

(defstruct ray-cast-input
  (point1 #(0 0) :type vec2)
  (point2 #(0 0) :type vec2)
  (max-fraction 0 :type double-float))

(defstruct ray-cast-output
  (normal #(0 0) :type vec2)
  (fraction 0 :type double-float)
  (hit 0))                            ;bool

(defstruct line-segment
  (point1 #(0 0) :type vec2)
  (point2 #(0 0) :type vec2))

(defstruct axis-aligned-bounding-box
  (lower-bound #(0 0) :type vec2)
  (upper-bound #(0 0) :type vec2))
