(in-package :blox2d)

(deftype enum (&rest symbols)
  `(and symbol (member ,@symbols)))

(defun repeat (value times)
  (if (> times 1)
      (cons value (repeat (1- times) value))
      (cons value nil)))
