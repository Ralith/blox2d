(in-package :blox2d)

(deftype vec2 ()
  '(simple-array single-float (2)))

(defun vec2 (x y)
  (make-array 2
              :element-type 'single-float
              :initial-contents (list (float x 1.0) (float y 1.0))))

(defun x-coord (vec)
  (aref vec 0))

(defun y-coord (vec)
  (aref vec 1))

(defun z-coord (vec)
  (aref vec 2))

(defun (setf x-coord) (value vec)
  (setf (aref vec 0) (float value 1.0)))

(defun (setf y-coord) (value vec)
  (setf (aref vec 1) (float value 1.0)))

(defun (setf z-coord) (value vec)
  (setf (aref vec 2) (float value 1.0)))

(defun vec2= (a &rest vecs)
  (and (mapcar (lambda (v) (and
                            (= (x-coord a) (x-coord v))
                            (= (y-coord a) (y-coord v))))
               vecs)))

(defun vec2+ (&rest vecs)
  (let ((x 0) (y 0))
    (dolist (vec vecs (vec2 x y))
      (incf x (x-coord vec))
      (incf y (y-coord vec)))))

(defun vec2- (&rest vecs)
  (let ((x 0) (y 0))
    (dolist (vec vecs (vec2 x y))
      (decf x (x-coord vec))
      (decf y (y-coord vec)))))

(defun vec2* (vec factor)
  (vec2 (* (x-coord vec) factor)
        (* (y-coord vec) factor)))

(defun vec2-length (vec)
  (sqrt (+ (expt (x-coord vec) 2)
           (expt (y-coord vec) 2))))

(defun vec2-length-squared (vec)
  (+ (expt (x-coord vec) 2)
     (expt (y-coord vec) 2)))

(defun vec2-normalized (vec)
  (let ((len (vec2-length vec)))
    (if (> single-float-epsilon len)
        vec
        (let ((inv-length (/ 1 len)))
          (vec2 (* (x-coord vec) inv-length)
                (* (y-coord vec) inv-length))))))


(deftype vec3 ()
  '(simple-array single-float (3)))

(defun vec3 (x y z)
  (make-array 3
              :element-type 'single-float
              :initial-contents (list (float x 1.0)
                                      (float y 1.0)
                                      (float z 1.0))))

(defun vec3-inverted (vec)
  (vec3 (- (x-coord vec))
        (- (y-coord vec))
        (- (z-coord vec))))

(defun vec3= (a &rest vecs)
  (and (mapcar (lambda (b)
                 (= (x-coord a) (x-coord b))
                 (= (y-coord a) (y-coord b))
                 (= (z-coord a) (z-coord b)))
               vecs)))

(defun vec3+ (&rest vecs)
  (let ((x 0) (y 0) (z 0))
    (dolist (vec vecs (vec3 x y z))
      (incf (x-coord vec))
      (incf (y-coord vec))
      (incf (z-coord vec)))))

(defun vec3- (&rest vecs)
  (let ((x 0) (y 0) (z 0))
    (dolist (vec vecs (vec3 x y z))
      (decf (x-coord vec))
      (decf (y-coord vec))
      (decf (z-coord vec)))))

(defun vec3* (vec factor)
  (vec3 (* (x-coord vec) factor)
        (* (y-coord vec) factor)
        (* (z-coord vec) factor)))


(defun dot (a b)
  (loop
     for i from 0 to (1- (length a))
     sum (* (aref a i) (aref b i))))

(defun cross2-vv (a b)
  "Cross product of two vec2s"
  (- (* (x-coord a) (y-coord b)) (* (y-coord a) (x-coord b))))

(defun cross2-vs (vec scalar)
  "Cross product of a vec2 and a scalar"
  (vec2 (* scalar (y-coord vec)) (* (- scalar) (x-coord vec))))

(defun cross2-sv (scalar vec)
  "Cross product of a scalar and a vec2"
  (vec2 (* (- scalar) (y-coord vec)) (* scalar (x-coord vec))))

(defun cross3-vv (a b)
  "Cross product of two vec3s"
  (vec3 (- (* (y-coord a) (z-coord b)) (* (z-coord a) (y-coord b)))
        (- (* (z-coord a) (x-coord b)) (* (x-coord a) (z-coord b)))
        (- (* (x-coord a) (y-coord b)) (* (y-coord a) (x-coord b)))))

(defun vec2-distance (a b)
  (vec2-length (vec2- a b)))

(defun vec2-distance-squared (a b)
  (let ((vec (vec2- a b)))
        (dot vec vec)))

(defun vec2-abs (vec)
  (vec2 (abs (x-coord vec))
        (abs (y-coord vec))))

(defun vec2-min (a &rest vecs)
  (let ((x (x-coord a))
        (y (y-coord a)))
    (dolist (vec vecs (vec2 x y))
      (let ((vx (x-coord vec))
            (vy (y-coord vec)))
       (when (< vx x)
         (setf x vx))
       (when (< vy y)
         (setf y vy))))))

(defun vec2-max (a &rest vecs)
  (let ((x (x-coord a))
        (y (y-coord a)))
    (dolist (vec vecs (vec2 x y))
      (let ((vx (x-coord vec))
            (vy (y-coord vec)))
       (when (> vx x)
         (setf x vx))
       (when (> vy y)
         (setf y vy))))))

(defun vec2-clamp (x min max)
  (vec2-max min (vec2-min x max)))


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
  (atan (y-coord (matrix2x2-column1 matrix2x2))
        (x-coord (matrix2x2-column1 matrix2x2))))

(defun matrix2x2-inverted (matrix2x2)
  (let ((c1x (x-coord (matrix2x2-column1 matrix2x2)))
        (c2x (x-coord (matrix2x2-column2 matrix2x2)))
        (c1y (y-coord (matrix2x2-column1 matrix2x2)))
        (c2y (y-coord (matrix2x2-column2 matrix2x2))))
    (assert (not (= 0.0 (- (* c1x c2y) (* c2x c1y)))))
    (let ((determinant (/ (- (* c1x c2y) (* c2x c1y)))))
      (make-matrix2x2 :column1 (vec2 (* determinant c2y)
                                     (* (- determinant) c1y))
                      :column2 (vec2 (* (- determinant) c2x)
                                     (* determinant c1x))))))

(defun matrix2x2-solve (matrix2x2 vec2)
  (let ((c1x (x-coord (matrix2x2-column1 matrix2x2)))
        (c2x (x-coord (matrix2x2-column2 matrix2x2)))
        (c1y (y-coord (matrix2x2-column1 matrix2x2)))
        (c2y (y-coord (matrix2x2-column2 matrix2x2))))
    (assert (not (= 0.0 (- (* c1x c2y) (* c2x c1y)))))
    (let ((determinant (/ (- (* c1x c2y) (* c2x c1y)))))
      (vec2 (* determinant (- (* c2y (x-coord vec2))
                              (* c2x (y-coord vec2))))
            (* determinant (- (* c1x (y-coord vec2))
                              (* c1y (x-coord vec2))))))))

(defun matrix2x2-mult (matrix vec2)
  (vec2 (+ (* (x-coord (matrix2x2-column1 matrix))
              (x-coord vec2))
           (* (x-coord (matrix2x2-column2 matrix))
              (y-coord vec2)))
        (+ (* (y-coord (matrix2x2-column1 matrix))
              (x-coord vec2))
           (* (y-coord (matrix2x2-column2 matrix))
              (y-coord vec2)))))

(defun matrix2x2-mult-transp (matrix vec)
  (vec2 (dot vec (matrix2x2-column1 matrix))
        (dot vec (matrix2x2-column2 matrix))))

(defun matrix2x2-mult-matrix (matrix1 matrix2)
  (make-matrix2x2
   :column1 (matrix2x2-mult matrix1 (matrix2x2-column1 matrix2))
   :column2 (matrix2x2-mult matrix1 (matrix2x2-column1 matrix2))))

(defun matrix2x2-mult-matrix-transp (matrix1 matrix2)
  (make-matrix2x2
   :column1 (vec2 (dot (matrix2x2-column1 matrix1)
                       (matrix2x2-column1 matrix2))
                  (dot (matrix2x2-column2 matrix1)
                       (matrix2x2-column1 matrix2)))
   :column2 (vec2 (dot (matrix2x2-column1 matrix1)
                       (matrix2x2-column2 matrix2))
                  (dot (matrix2x2-column2 matrix1)
                       (matrix2x2-column2 matrix2)))))

(defun matrix2x2-abs (matrix)
  (make-matrix2x2 :column1 (vec2-abs (matrix2x2-column1 matrix))
                  :column2 (vec2-abs (matrix2x2-column2 matrix))))


(defstruct matrix3x3
  (column1 (vec3 0 0 0) :type vec3)
  (column2 (vec3 0 0 0) :type vec3)
  (column3 (vec3 0 0 0) :type vec3))

(defun matrix3x3* (matrix factor)
  (make-matrix3x3 :column1 (vec3* (matrix3x3-column1 matrix) factor)
                  :column2 (vec3* (matrix3x3-column2 matrix) factor)
                  :column3 (vec3* (matrix3x3-column3 matrix) factor)))

(defun matrix3x3-solve (matrix3x3 vec3)
  (let ((determinant (/ (dot (matrix3x3-column1 matrix3x3)
                             (cross3-vv (matrix3x3-column2 matrix3x3)
                                        (matrix3x3-column3 matrix3x3))))))
    (vec3 (* determinant
             (dot vec3
                  (cross3-vv (matrix3x3-column2 matrix3x3)
                             (matrix3x3-column3 matrix3x3))))
          (* determinant
             (dot (matrix3x3-column1 matrix3x3)
                  (cross3-vv vec3
                             (matrix3x3-column3 matrix3x3))))
          (* determinant
             (dot (matrix3x3-column1 matrix3x3)
                  (cross3-vv (matrix3x3-column2 matrix3x3)
                             vec3))))))

(defun matrix3x3-solve2x2 (matrix3x3 vec2)
  (let ((c1x (x-coord (matrix3x3-column1 matrix3x3)))
        (c2x (x-coord (matrix3x3-column2 matrix3x3)))
        (c1y (y-coord (matrix3x3-column1 matrix3x3)))
        (c2y (y-coord (matrix3x3-column2 matrix3x3))))
    (assert (not (= 0.0 (- (* c1x c2y) (* c2x c1y)))))
    (let ((determinant (/ (- (* c1x c2y) (* c2x c1y)))))
      (vec2 (* determinant (- (* c2y (x-coord vec2))
                              (* c2x (y-coord vec2))))
            (* determinant (- (* c1x (y-coord vec2))
                              (* c1y (x-coord vec2))))))))

(defun matrix3x3-mult (matrix vec3)
  (vec3+ (vec3* (matrix3x3-column1 matrix) (x-coord vec3))
         (vec3* (matrix3x3-column2 matrix) (y-coord vec3))
         (vec3* (matrix3x3-column3 matrix) (z-coord vec3))))


(defstruct transform
  (position (vec2 0 0)
            :type vec2)
  (rotation (make-matrix2x2 :column1 (vec2 0 0)
                            :column2 (vec2 0 0))
            :type matrix2x2))

(defparameter +identity-transform+
  (make-transform :position (vec2 0 0)
                  :rotation +identity2x2+))

(defun transform-mult (transform vec2)
  (vec2+ (transform-position transform)
         (matrix2x2-mult (transform-rotation transform)
                         vec2)))

(defun transform-mult-transp (transform vec2)
  (matrix2x2-mult-transp (transform-rotation transform)
                         (vec2- vec2 (transform-position transform))))


(defstruct sweep
  (center-local (vec2 0 0) :type vec2)
  (initial-center-world (vec2 0 0) :type vec2)
  (center-world (vec2 0 0) :type vec2)
  (initial-angle 0.0 :type single-float)
  (angle 0.0 :type single-float)
  (time-interval 0.0 :type single-float))

(defun sweep->transform (sweep alpha)
  (make-transform
   :position (vec2+ (vec2* (sweep-initial-center-world sweep)
                           (- 1.0 alpha))
                    (vec2* (sweep-center-world sweep)
                           alpha))
   :rotation (angle->matrix2x2 (+ (* (sweep-initial-angle sweep)
                                     (- 1.0 alpha))
                                  (* (sweep-angle sweep)
                                     alpha)))))

(defun sweep-advance (sweep time-interval)
  (when (and (< (sweep-time-interval sweep)
                time-interval)
             (> (- 1.0 (sweep-time-interval sweep))
                single-float-epsilon))
    (let ((alpha (/ (- time-interval (sweep-time-interval sweep))
                    (- 1.0 (sweep-time-interval sweep)))))
      (setf (sweep-initial-center-world sweep)
            (vec2+ (vec2* (sweep-initial-center-world sweep)
                          (- 1.0 alpha))
                   (vec2* (sweep-center-world sweep) alpha))))))


(defun clamp (x min max)
  (max min (min x max)))

(defmacro swap (a b)
  (let ((tmp (gensym)))
   `(let ((,tmp ,a))
      (setf ,a ,b)
      (setf ,b ,tmp))))