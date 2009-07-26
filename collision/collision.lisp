(in-package :blox2d)

(defstruct contact-id
  (reference-edge nil :type (signed-byte 8))
  (incident-edge nil :type (signed-byte 8))
  (incident-vertex nil :type (signed-byte 8))
  (flip nil :type (signed-byte 8)))

(defstruct aabb
  (lower-bound nil :type vector)
  (upper-bound nil :type vector))