(in-package :blox2d)

(deftype vec2 ()
  '(simple-array single-float (2)))

(defun vec2 (x y)
  (make-array 2
              :element-type 'single-float
              :initial-contents (list (float x 1.0) (float y 1.0))))

(defun vec2-x (vec)
  (elt vec 0))

(defun vec2-y (vec)
  (elt vec 1))

(defconstant +zero-vec2+ (vec2 0.0 0.0)
  "The zero vector.")