(in-package :blox2d)

(deftype vec2 ()
  '(simple-array single-float (2)))

(defun vec2 (x y)
  (make-array 2
              :element-type 'single-float
              :initial-contents (list (float x 1.0) (float y 1.0))))

(defun vec2-x (vec)
  (aref vec 0))

(defun vec2-y (vec)
  (aref vec 1))

(defun (setf vec2-x) (value vec)
  (setf (aref vec 0) (float value 1.0)))

(defun (setf vec2-y) (value vec)
  (setf (aref vec 1) (float value 1.0)))

(defun vec2= (a b)
  (and
   (= (vec2-x a) (vec2-x b))
   (= (vec2-y a) (vec2-y b))))

(defun vec2+ (a b)
  (vec2 (+ (vec2-x a) (vec2-x b))
        (+ (vec2-y a) (vec2-y b))))

(defun vec2- (a b)
  (vec2 (- (vec2-x a) (vec2-x b))
        (- (vec2-y a) (vec2-y b))))

(defun vec2* (vec factor)
  (vec2 (* (vec2-x vec) factor)
        (* (vec2-y vec) factor)))

(defun vec2-length (vec)
  (sqrt (+ (expt (vec2-x vec) 2)
           (expt (vec2-y vec) 2))))

(defun vec2-length-squared (vec)
  (+ (expt (vec2-x vec) 2)
     (expt (vec2-y vec) 2)))

(defun vec2-normalized (vec)
  (let ((len (vec2-length vec)))
   (if (= 0.0 len)                      ;TODO: approx. equal
       vec
       (let ((inv-length (/ 1 len)))
         (vec2 (* (vec2-x vec) inv-length)
               (* (vec2-y vec) inv-length))))))


(deftype vec3 ()
  '(simple-array single-float (3)))

(defun vec3 (x y z)
  (make-array 3
              :element-type 'single-float
              :initial-contents (list (float x 1.0)
                                      (float y 1.0)
                                      (float z 1.0))))

(defun vec3-x (vec)
  (aref vec 0))

(defun vec3-y (vec)
  (aref vec 1))

(defun vec3-z (vec)
  (aref vec 2))

(defun (setf vec3-x) (value vec)
  (setf (aref vec 0) (float value 1.0)))

(defun (setf vec3-y) (value vec)
  (setf (aref vec 1) (float value 1.0)))

(defun (setf vec3-z) (value vec)
  (setf (aref vec 2) (float value 1.0)))

(defun vec3-inverted (vec)
  (vec3 (- (vec3-x vec))
        (- (vec3-y vec))
        (- (vec3-z vec))))

(defun vec3= (a b)
  (and
   (= (vec3-x a) (vec3-x b))
   (= (vec3-y a) (vec3-y b))
   (= (vec3-z a) (vec3-z b))))

(defun vec3+ (a b)
  (vec3 (+ (vec3-x a) (vec3-x b))
        (+ (vec3-y a) (vec3-y b))
        (+ (vec3-z a) (vec3-z b))))

(defun vec3- (a b)
  (vec3 (- (vec3-x a) (vec3-x b))
        (- (vec3-y a) (vec3-y b))
        (- (vec3-z a) (vec3-z b))))

(defun vec3* (vec factor)
  (vec3 (* (vec3-x vec) factor)
        (* (vec3-y vec) factor)
        (* (vec3-z vec) factor)))


(defstruct matrix2x2
  (column1 (vec2 0 0) :type vec2)
  (column2 (vec2 0 0) :type vec2))

(defparameter +identity2x2+
  (make-matrix2x2 :column1 (vec2 1 0)
                  :column2 (vec2 0 1)))

(defun angle->matrix2x2 (angle)
  (let ((cos (cos angle))
        (sin (sin angle)))
    (make-matrix2x2 :column1 (vec2 cos sin)
                    :column2 (vec2 (- sin) cos))))

(defun matrix2x2->angle (matrix2x2)
  (atan (vec2-y (matrix2x2-column1 matrix2x2))
        (vec2-x (matrix2x2-column1 matrix2x2))))

(defun matrix2x2-inverted (matrix2x2)
  (let ((c1x (vec2-x (matrix2x2-column1 matrix2x2)))
        (c2x (vec2-x (matrix2x2-column2 matrix2x2)))
        (c1y (vec2-y (matrix2x2-column1 matrix2x2)))
        (c2y (vec2-y (matrix2x2-column2 matrix2x2))))
    (assert (not (= 0.0 (- (* c1x c2y) (* c2x c1y)))))
    (let ((determinant (/ (- (* c1x c2y) (* c2x c1y)))))
      (make-matrix2x2 :column1 (vec2 (* determinant c2y)
                                     (* (- determinant) c1y))
                      :column2 (vec2 (* (- determinant) c2x)
                                     (* determinant c1x))))))

(defun matrix2x2-solve (matrix2x2 vec2)
  (let ((c1x (vec2-x (matrix2x2-column1 matrix2x2)))
        (c2x (vec2-x (matrix2x2-column2 matrix2x2)))
        (c1y (vec2-y (matrix2x2-column1 matrix2x2)))
        (c2y (vec2-y (matrix2x2-column2 matrix2x2))))
    (assert (not (= 0.0 (- (* c1x c2y) (* c2x c1y)))))
    (let ((determinant (/ (- (* c1x c2y) (* c2x c1y)))))
      (vec2 (* determinant (- (* c2y (vec2-x vec2))
                              (* c2x (vec2-y vec2))))
            (* determinant (- (* c1x (vec2-y vec2))
                              (* c1y (vec2-x vec2))))))))

