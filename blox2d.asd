(asdf:defsystem :blox2d
  :description "A lisp port of the Box2D physics library."
  :components
  ((:module "common"
            :components
            ((:file "math")
             (:file "util")))
   (:module "collision"
            :components
            ((:file "collision"
                    :depends-on ("math" "util"))))))