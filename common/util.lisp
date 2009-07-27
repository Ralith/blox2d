(in-package :blox2d)

(deftype enum (&rest symbols)
  `(and symbol (member ,@symbols)))