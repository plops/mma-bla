(defpackage :vector
  (:use :cl)
  (:export 
   #:vec2 #:make-vec2 #:vec2-x #:vec2-y :v2. :v2*
   #:norm2 #:x #:y #:z
   #:vec 
   #:make-vec 
   #:v
   #:v-spherical
   #:vec-x #:vec-y #:vec-z
   #:v. #:v+ #:v- #:v* 
   #:norm #:normalize
   #:cross
   #:m #:rotation-matrix #:m*
   #:vec-i #:make-vec-i #:vec-i-x #:vec-i-y #:vec-i-z
   #:v.-i #:v+-i #:v--i #:v*-i #:norm-i
   #:ray
   #:ray-lost
   #:direction-not-normalized
   #:check-direction-norm
   #:check-unit-norm))

(in-package :vector)

;;;; double-float 2 vector
(deftype vec2 ()
  `(simple-array double-float (2)))

(defstruct (vec2 (:type (vector double-float)))
  (x 0d0) (y 0d0))

(defun norm2 (vec2)
  (declare (vec2 vec2)
	   (values double-float &optional))
  (let ((x (vec2-x vec2))
	(y (vec2-y vec2)))
    (sqrt (+ (* x x) (* y y)))))

(defun v2. (a b)
  "Dot product between two vectors."
  (declare (vec2 a b)
	   (values double-float &optional))
  (let ((sum 0d0))
    (declare (double-float sum))
    (dotimes (i 2)
      (incf sum (* (aref a i)
		   (aref b i))))
    sum))

(defun  v2* (a scalar)
  "Multiply vector with scalar."
  (declare (double-float scalar)
	   (vec2 a)
	   (values vec2 &optional))
  (let* ((result (make-vec2)))
    (dotimes (i 2)
      (setf (aref result i) (* scalar (aref a i))))
    result))

;;;; double-float 3 vector

(deftype vec ()
  `(simple-array double-float (3)))

(defun vec-x (vec)
  (aref vec 0))
(defun vec-y (vec)
  (aref vec 1))
(defun vec-z (vec)
  (aref vec 2))

(deftype mat ()
  `(simple-array double-float (3 3)))

(defun make-vec (&optional (x 0d0) (y 0d0) (z 0d0))
  (declare (double-float x y z)
	   (values vec &optional))
  (make-array 3
	      :element-type 'double-float
	      :initial-contents (list x y z)))

(defmacro v (&rest args)
  `(make-vec ,@(mapcar #'(lambda (x)
			   (coerce x 'double-float))
		       args)))

(defun v-spherical (theta phi)
  "Convert spherical coordinates into cartesian."
  (declare ((double-float 0d0 #.(/ pi 4)) theta)
	   ((double-float 0d0 #.(* 2 pi)) phi)
	   (values vec &optional))
  (let* ((st (sin theta)))
    (make-vec (* st (cos phi))
	      (* st (sin phi))
	      (cos theta))))

(defun v. (a b)
  "Dot product between two vectors."
  (declare (vec a b)
	   (values double-float &optional))
  (let ((sum 0d0))
    (declare (double-float sum))
    (dotimes (i 3)
      (incf sum (* (aref a i)
		   (aref b i))))
    sum))
(defun cross (a b)
  (declare (vec a b)
	   (values vec &optional))
  (make-vec (- (* (aref a 1) (aref b 2))
        (* (aref a 2) (aref b 1)))
     (- (* (aref a 2) (aref b 0))
        (* (aref a 0) (aref b 2)))
     (- (* (aref a 0) (aref b 1))
        (* (aref a 1) (aref b 0)))))
#+nil
(cross (v 1d0)
       (v 0d0 1d0))

(declaim (ftype (function (vec vec)
			  (values vec &optional))
		v- v+))
(defmacro v-op (op a b)
  "Subtracting and adding vectors."
  `(let ((result (v)))
     (dotimes (i 3)
       (setf (aref result i) (,op (aref ,a i)
				  (aref ,b i))))
     result))

(defun v+ (a b)
  "Add two vectors."
  (v-op + a b))

(defun v- (a b)
  "Subtract two vectors."
  (v-op - a b))

#+nil (defun  v* (a scalar)
  "Multiply vector with scalar."
  (declare (double-float scalar)
	   (vec a)
	   (values vec &optional))
  (let* ((result (v)))
    (dotimes (i 3)
      (setf (aref result i) (* scalar (aref a i))))
    result))

(defun  %v* (a scalar)
  "Multiply vector with scalar."
  (declare (double-float scalar)
	   (vec a)
	   (values vec &optional))
  (let* ((result (v)))
    (dotimes (i 3)
      (setf (aref result i) (* scalar (aref a i))))
    result))

(defmacro v* (a scalar)
  (if (numberp scalar)
      `(%v* ,a (load-time-value (* 1d0 ,scalar)))
      `(%v* ,a ,scalar)))
#+nil
(v* (v 1d0) 4)



(declaim (ftype (function (vec)
			  (values double-float &optional))
		norm))
(defun norm (a)
  "Length of a vector."
  (let ((l2 (v. a a)))
    (declare (type (double-float 0d0) l2)) ;; Otherwise warning with complex-double
    (sqrt l2)))


(declaim (ftype (function (vec)
			  (values vec &optional))
		normalize))
(defun normalize (a)
  "Rescale vector to unit length."
  (let ((len (norm a)))
    (if (eq len 0d0)
	(v 0d0 0d0 1d0)
	(v* a (/ len)))))


;;;; 3x3 Matrix 

(declaim (ftype (function (double-float double-float double-float
					double-float double-float double-float
					double-float double-float double-float)
			  (values mat &optional))
		m))
(defun m (a b c d e f g h i)
  (make-array '(3 3)
              :element-type 'double-float
              :initial-contents (list (list a b c) (list d e f) (list g h i))))

(defun rotation-matrix (angle vect)
  "Create matrix that rotates by ANGLE radians around the direction
 VECT. VECT must be normalized."
  (declare ((double-float 0d0 #.(* 2d0 pi)) angle)
	   (vec vect)
	   (values mat &optional))
  (let* ((u (aref vect 0))
         (v (aref vect 1))
         (w (aref vect 2))
	 (c (cos angle))
         (s (sin angle))
         (1-c (- 1 c))
         (su (* s u))
         (sv (* s v))
         (sw (* s w)))
    (m (+ c (* 1-c u u))
       (+ (* 1-c u v) sw)
       (- (* 1-c u w) sv)

       (- (* 1-c u v) sw)
       (+ c (* 1-c v v))
       (+ (* 1-c v w) su)

       (+ (* 1-c u w) sv)
       (- (* 1-c v w) su)
       (+ c (* 1-c w w)))))
#+nil
(rotation-matrix (/ pi 2) (v 0d0 0d0 1d0))

(defun m* (matrix vect)
  "Multiply MATRIX with VECT. Copies 4th component w from VECT into
result."
  (declare (mat matrix)
	   (vec vect)
	   (values vec &optional))
  (let ((res (v)))
    (dotimes (i 3)
      (dotimes (j 3)
	(incf (aref res i)
	      (* (aref matrix i j) (aref vect j)))))
    res))
#+nil
(m* (rotation-matrix (/ pi 2) (v 0d0 0d0 1d0)) (v 1d0))




;;;; Integer vectors


(deftype vec-i ()
  `(simple-array fixnum (3)))

(defstruct (vec-i (:type (vector fixnum)))
  (x 0) (y 0) (z 0))

(defun v.-i (a b)
  (declare (vec-i a b)
	   (values fixnum &optional))
  (+ (* (vec-i-x a) (vec-i-x b))
     (* (vec-i-y a) (vec-i-y b))
     (* (vec-i-z a) (vec-i-z b))))

(declaim (ftype (function (vec-i vec-i)
			  (values vec-i &optional))
		v--i v+-i))
(defun v--i (a b)
  (make-vec-i :x (- (vec-i-x a) (vec-i-x b))
	      :y (- (vec-i-y a) (vec-i-y b))
	      :z (- (vec-i-z a) (vec-i-z b))))
(defun v+-i (a b)
  (make-vec-i :x (+ (vec-i-x a) (vec-i-x b))
	      :y (+ (vec-i-y a) (vec-i-y b))
	      :z (+ (vec-i-z a) (vec-i-z b))))

(defun norm-i (a)
  (declare (vec-i a)
	   (values double-float &optional))
  (sqrt (* 1d0 (v.-i a a))))

(defun  v*-i (a scalar)
  "Multiply vector with scalar."
  (declare (fixnum scalar)
	   (vec-i a)
	   (values vec-i &optional))
  (let* ((result (make-vec-i)))
    (dotimes (i 3)
      (setf (aref result i) (* scalar (aref a i))))
    result))

(define-condition ray-lost () ())
(define-condition direction-not-normalized () ())

(defclass ray ()
  ((start :accessor start :initarg :start
          :initform (v) :type vec)
   (direction :accessor direction :initarg :direction
	      :initform (v 0 0 1) :type vec)))

(defmethod print-object ((ray ray) stream)
  (with-slots (start direction) ray
   (format stream "#<ray start: ~a dir: ~a>" start direction)))

(defmethod check-direction-norm ((ray ray))
  (with-slots (direction)
      ray
    (unless (< (abs (- (norm direction) 1)) 1d-12)
      (error 'direction-not-normalized))))

(defun check-unit-norm (vec)
  (declare (vec vec)
	   (values null &optional))
  (unless (< (abs (- (norm vec) 1)) 1d-12)
    (error 'direction-not-normalized)))

#+nil
(make-instance 'ray)
