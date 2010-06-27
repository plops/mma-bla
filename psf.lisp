(defpackage :psf
  (:use :cl :vol)
  (:export #:init
	   #:intensity-psf
	   #:electric-field-psf
	   #:get-uv
	   #:abs2))

(in-package :psf)

(declaim (optimize (speed 2) (debug 3) (safety 3)))
 
(sb-alien:define-alien-routine j0
    sb-alien:double-float
  (x sb-alien:double-float))
 
(sb-alien:define-alien-routine j1
    sb-alien:double-float
  (x sb-alien:double-float))
 
(sb-alien:define-alien-routine jn
    sb-alien:double-float
  (n sb-alien:int)
  (x sb-alien:double-float))
 
(declaim (ftype (function ((complex double-float))
                          (values double-float &optional)) abs2)
         (inline abs2))
(defun abs2 (z) 
  (declare (type (complex double-float) z))
  (let* ((x (realpart z))
         (y (imagpart z)))
    (+ (* x x) (* y y))))

(declaim (type fixnum n)
	 (type double-float /sin-alpha /sin-alpha2  sin-alpha2)
	 (type (double-float -1d0 1d0) sin-alpha)
	 (type (simple-array double-float 1) ac as as^2 ac2))       

(defparameter n 31)
(defparameter ac (make-array n :element-type 'double-float))
(defparameter as (make-array n :element-type 'double-float))
(defparameter as^2 (make-array n :element-type 'double-float))
(defparameter ac2 (make-array n :element-type 'double-float))
(defparameter sin-alpha .4d0)
(defparameter sin-alpha2 (* sin-alpha sin-alpha))
(defparameter /sin-alpha (/ sin-alpha))
(defparameter /sin-alpha2 (/ sin-alpha2))

(declaim (ftype (function (&key (:numerical-aperture double-float)
				(:immersion-index double-float)
				(:integrand-evaluations fixnum))
			  (values null &optional))
		init))

(defun init (&key (numerical-aperture 1.2d0) (immersion-index 1.515d0) (integrand-evaluations 31))
  (setf n integrand-evaluations
	ac (make-array n :element-type 'double-float)
	as (make-array n :element-type 'double-float)
	as^2 (make-array n :element-type 'double-float)
	ac2 (make-array n :element-type 'double-float)
	sin-alpha (/ numerical-aperture immersion-index)
	sin-alpha2 (* sin-alpha sin-alpha)
	/sin-alpha (/ sin-alpha)
	/sin-alpha2 (/ sin-alpha2))
  (let* ((alpha (asin sin-alpha))
	 (dalpha (/ alpha n)))

    (dotimes (iter n)
      (let* ((theta (the (double-float 0d0 2d0) (* dalpha iter)))
	    (ct (cos theta))
	     (st (sin theta))
	     (ct2 (sqrt ct))
	    (st^2 (* st st)))
	(declare ((double-float 0d0 1d0) ct)) ;; for the sqrt
	(setf (aref ac iter) ct
	      (aref as iter) st
	      (aref as^2 iter) st^2
	      (aref ac2 iter) ct2)))))
#+nil
(init)

;; k = omega/c = 2 pi / lambda
;; u = k z sin(alpha)^2
;; v = k sqrt(x^2+y^2) sin(alpha)

;; for small angular aperture alpha << 1
;; u ~ k (a/R)^2 z
;; v ~ k (a/R) sqrt(x^2+y^2)
;; with a .. radius of the exit pupil and
;;      R .. distance between exit pupil and focal plane

(declaim (ftype (function (double-float double-float double-float
					&key (:immersion-index double-float)
					(:numerical-aperture double-float)
					(:wavelength double-float))
			  (values double-float double-float &optional))))
(defun transform-xyz-to-uv (x y z &key (immersion-index 1.515d0)
			    (numerical-aperture 1.38d0) (wavelength .480d0))
  "Return the cylindrical coordinates u and v of a given 3D point. All
lengths in micrometer."
  (let* ((k (/ (* 2d0 pi)
	       wavelength))
	 (sina (/ numerical-aperture
		  immersion-index))
	 (u (* k z sina sina))
	 (v (* k (sqrt (+ (* x x) (* y y))) sina)))
   (values u v)))

#+nil
(tranform-xyz-to-uv 0 1 1)

(declaim (ftype (function (double-float double-float)
			  (values (complex double-float) 
				  (complex double-float)
				  (complex double-float) &optional))
		intermediate-integrals-point)
	 (inline intermediate-integrals-point))
(defun intermediate-integrals-point (u v)
  (let* ((i0 (complex 0d0 0d0))
	 (i1 (complex 0d0 0d0))
	 (i2 (complex 0d0 0d0)))
     (declare (type (complex double-float) i0 i1 i2))
     (let ((scale 1))
       (dotimes (iter n)
         (let* ((s (aref as iter))
                (c (aref ac iter))
                (c2 (aref ac iter))
                (vv (* v s /sin-alpha))
                (uu (* u c /sin-alpha2))
                (e (exp (complex 0 uu)))
                (cmul0 (* scale e (* c2 s (+ 1 c) (j0 vv))))
                (cmul1 (* scale e (* c2 (aref as^2 iter) (j1 vv))))
                (cmul2 (* scale e (* c2 s (- 1 c) (jn 2 vv)))))
           (incf i0 cmul0)
           (incf i1 cmul1)
           (incf i2 cmul2)
           (cond ((eq iter 0) (setf scale 2))
                 ((eq iter (- n 2)) (setf scale 1))
                 ((eq scale 2) (setf scale 4)) 
                 ((eq scale 4) (setf scale 2))))))
     (let* ((s (* n 3d0)))
       (values (* s i0) (* s i1) (* s i2)))))

(declaim (ftype (function (double-float double-float)
			  (values double-float &optional))
		energy-density)
	 (inline energy-density))
(defun energy-density (u v)
  (multiple-value-bind (i0 i1 i2)
      (intermediate-integrals-point u v)
    (+ (abs2 i0)
       (abs2 i1)
       (abs2 i2))))

(declaim (ftype (function 
		 (&optional fixnum fixnum double-float double-float)
		 (values (simple-array double-float (* *)) &optional))
		energy-density-cyl))
(defun energy-density-cyl (&optional (nu 100) (nv 100) (du .1d0) (dv .1d0))
  (let* ((res (make-array (list nu nv) 
                          :element-type 'double-float)))
    (dotimes (iu nu)
      (let ((u (* iu du)))
       (dotimes (iv nv)
         (let ((v (* iv dv)))
           (setf (aref res iu iv) (energy-density u v))))))
    res))

(declaim (ftype (function 
		 (&optional fixnum fixnum double-float double-float)
		 (values (simple-array (complex double-float) (* *))
			 (simple-array (complex double-float) (* *)) 
			 (simple-array (complex double-float) (* *))
			 &optional))
		intermediate-integrals-cyl))
 
(defun intermediate-integrals-cyl (&optional (nu 100) (nv 100)
				   (du .1d0) (dv .1d0))
  (let* ((dims (list nu nv))
	 (ii0 (make-array dims :element-type '(complex double-float)))
	 (ii1 (make-array dims :element-type '(complex double-float)))
	 (ii2 (make-array dims :element-type '(complex double-float))))
    (dotimes (iu nu)
      (let ((u (* iu du)))
       (dotimes (iv nv)
         (let ((v (* iv dv)))
	   (multiple-value-bind (i0 i1 i2)
	     (intermediate-integrals-point u v)
	     (setf (aref ii0 iu iv) i0
		   (aref ii1 iu iv) i1
		   (aref ii2 iu iv) i2))))))
    (values ii0 ii1 ii2)))


(declaim (ftype (function (fixnum fixnum fixnum double-float double-float
				  &key 
				  (:numerical-aperture double-float)
				  (:immersion-index double-float)
				  (:wavelength double-float)
				  (:integrand-evaluations fixnum)) 
			  (values (simple-array (complex double-float) 3)
				  (simple-array (complex double-float) 3)
				  (simple-array (complex double-float) 3)
				  &optional))
		electric-field-psf))

#+nil
(let* ((x 10)
       (y 10)
       (z 10)
       (size-x 3d0)
       (size-z 1d0)
       (nradius (1+ (ceiling (* (sqrt 2d0) (max x y)))))
       (nz (1+ (ceiling z 2))))
  (multiple-value-bind (u v)
      (transform-xyz-to-uv size-x 0 (* .5d0 size-z))
    (multiple-value-bind (i0 i1 i2)
	(intermediate-integrals-cyl nz nradius (/ u nz) (/ v nradius))
      (vol::interpolate2-cdf i0 2d0 2d0))))

;; size radius is either the extend in x or y (in um) depending on what is bigger
(defun electric-field-psf
    (z y x size-z size-radius &key (numerical-aperture 1.38d0) 
     (immersion-index 1.515d0) (wavelength .480d0)
     (integrand-evaluations 31))
  (init :numerical-aperture numerical-aperture
	:immersion-index immersion-index
	:integrand-evaluations integrand-evaluations)
  (let* ((nradius (1+ (ceiling (* (sqrt 2d0) (max x y)))))
	 (nz (ceiling z 2))
	 (dims (list z y x))
	 (e0 (make-array dims :element-type '(complex double-float)))
	 (e1 (make-array dims :element-type '(complex double-float)))
	 (e2 (make-array dims :element-type '(complex double-float))))
    (multiple-value-bind (u v)
	(transform-xyz-to-uv 0 (* (sqrt 2d0) size-radius) (* .5d0 size-z)
			     :numerical-aperture numerical-aperture
			     :immersion-index immersion-index
			     :wavelength wavelength)
      (multiple-value-bind (i0 i1 i2)
	  (intermediate-integrals-cyl nz nradius
				      (/ (* 1d0 u) nz) (/ (* 1d0 v) nradius))
	(let ((rad-a (make-array (list y x) :element-type 'double-float))
	      (cphi-a (make-array (list y x) :element-type 'double-float))
	      (c2phi-a (make-array (list y x) :element-type 'double-float))
	      (s2phi-a (make-array (list y x) :element-type 'double-float)))
	  (do-rectangle (j i 0 y 0 x)
	    (let* ((ii (- i (floor x 2)))
		   (jj (- j (floor y 2)))
		   (radius (sqrt (+ (* 1d0 ii ii) (* jj jj))))
		   (phi (atan (* 1d0 jj) ii)))
	      (setf (aref rad-a j i) radius
		    (aref cphi-a j i) (cos phi)
		    (aref c2phi-a j i) (cos (* 2d0 phi))
		    (aref s2phi-a j i) (sin (* 2d0 phi)))))
	  (let ((del (if (eq 1 (mod z 2))
			 0d0
			 .5d0))) ;; add .5 if z is even
	   (do-box (k j i 0 nz 0 y 0 x)
	     (let* ((zi (- nz k (- 1d0 del)))
		    (r (aref rad-a j i))
		    (v0 (interpolate2 i0 zi r))
		    (v1 (interpolate2 i1 zi r))
		    (v2 (interpolate2 i2 zi r)))
	       (setf (aref e0 k j i) (* (complex 0d0 -1d0)
					(+ v0 (* v2 (aref c2phi-a j i))))
		     (aref e1 k j i) (* (complex 0d0 -1d0)
					v2 (aref s2phi-a j i))
		     (aref e2 k j i) (* -2d0 v1 (aref cphi-a j i))))))
	  (do-box (k j i 0 nz 0 y 0 x)
	    (setf (aref e0 (- z k 1) j i) (- (conjugate (aref e0 k j i)))
		  (aref e1 (- z k 1) j i) (- (conjugate (aref e1 k j i)))
		  (aref e2 (- z k 1) j i) (conjugate (aref e2 k j i)))))))
    (values e0 e1 e2)))

#+nil
(defparameter *e0* (electric-field 10 10 10 3d0 3d0))

(declaim (ftype (function (double-float double-float &key 
					(:numerical-aperture double-float)
					(:immersion-index double-float)
					(:nz fixnum)
					(:nradius fixnum)
					(:wavelength double-float)
					(:integrand-evaluations fixnum))
			  (values (simple-array double-float 2) &optional))
		intensity-psf-cyl))
(defun intensity-psf-cyl (z radius &key (numerical-aperture 1.38d0) 
			  (immersion-index 1.515d0)
			  (nz 50) (nradius 50) (wavelength .480d0)
			  (integrand-evaluations 31)) 
  "Calculate 2D cylindrical PSF with axial extend Z micrometers and
transversal extend RADIUS micrometers."
  (psf:init :numerical-aperture numerical-aperture 
	    :immersion-index immersion-index
	    :integrand-evaluations integrand-evaluations)
  (multiple-value-bind (u v)
      (transform-xyz-to-uv 0 radius z
		  :numerical-aperture numerical-aperture
		  :immersion-index immersion-index
		  :wavelength wavelength)
    (let* ((a (energy-density-cyl nz nradius (/ u nz) (/ v nradius))))
      a)))
#+nil
(time
 (progn (cyl-psf 3d0 1.5d0)
	nil))

(declaim (ftype (function (fixnum fixnum fixnum double-float double-float
				  &key (:numerical-aperture double-float)
				  (:immersion-index double-float)
				  (:integrand-evaluations fixnum)
				  (:wavelength double-float)) 
			  (values (simple-array (complex double-float) 3) &optional))
		intensity-psf))
(defun intensity-psf (z y x size-z size-radius &key (numerical-aperture 1.38d0)
		      (immersion-index 1.515d0) (integrand-evaluations 31)
		      (wavelength .480d0))
   "Calculate an intensity point spread function for an aplanatic microobjective with the given NUMERICAL-APERTURE, IMMERSION-INDEX and WAVELENGTH. Distances in micrometer."
   (let* ((psf (make-array (list z y x)
			   :element-type '(complex double-float)))
	  (nz (1+ (ceiling z 2)))
	  (nradius (1+ (ceiling (* (sqrt 2d0) (max x y)))))
	  (cyl (intensity-psf-cyl
		(* .5d0 size-z) 
		(* (sqrt 2d0)
		   size-radius)
		:nz nz 
		:nradius nradius
		:numerical-aperture numerical-aperture
		:immersion-index immersion-index
		:integrand-evaluations integrand-evaluations
		:wavelength wavelength)))
     (let ((rad-a (make-array (list y x) :element-type 'double-float)))
       (do-rectangle (j i 0 y 0 x)
	 (let* ((ii (- i (floor x 2)))
		(jj (- j (floor y 2)))
		(radius (sqrt (+ (* 1d0 ii ii) (* jj jj)))))
	   (setf (aref rad-a j i) radius)))
       (let ((del (if (eq 1 (mod z 2)) ;; add .5 when z is even
		      0d0
		      .5d0)))
	 (do-box (k j i 0 nz 0 y 0 x)
	   (setf (aref psf k j i)
		 (complex (vol::interpolate2-df cyl
						(-  nz k (- 1d0 del)) (aref rad-a j i))))))
       (do-box (k j i 0 nz 0 y 0 x)
	 (setf (aref psf (- z k 1) j i) (aref psf k j i))))
     psf))


#+nil
(time (init))
#+nil
(time (progn (integ-all)
	     nil))

;; cylindrical coordinates:
;; v is rho
;; u is z 

#+nil ;; print out a cross section of the psf transversal 1.5 um and
      ;; axial 3 um wide.
(let ((numerical-aperture 1.38d0)
      (immersion-index 1.515d0)) 
  (init :numerical-aperture numerical-aperture
	:immersion-index immersion-index)
  (multiple-value-bind (u v)
      (transform-xyz-to-uv 0 1.5 3 
			   :numerical-aperture numerical-aperture
			   :immersion-index immersion-index)
   (let* ((nu 10)
	  (nv 20)
	  (a (energy-density-cyl nu nv  (/ u nu) (/ v nv)))
	  (max (aref a 0 0))
	  (scale (/ 99 max)))
     (destructuring-bind (uu vv)
	 (array-dimensions a)
       (format t "~%")
       (dotimes (u uu)
	 (dotimes (v vv)
	   (format t "~2d" (truncate (* scale (aref a u v)))))
	 (format t "~%"))))))
