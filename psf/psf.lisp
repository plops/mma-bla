(defpackage :psf
  (:use :cl :vol)
  (:export #:init
	   #:intensity-psf
	   #:electric-field-psf
	   #:get-uv
	   #:abs2
	   #:my-float
	   #:my-float-helper
	   #:+one+))

(in-package :psf)

(declaim (optimize (speed 2) (debug 3) (safety 3)))
 
#+nil  (progn
    (sb-alien:define-alien-routine j0
	sb-alien:double-float
      (x sb-alien:double-float))
    
    (sb-alien:define-alien-routine j1
	sb-alien:double-float
      (x sb-alien:double-float))
    
    (sb-alien:define-alien-routine jn
	sb-alien:double-float
      (n sb-alien:int)
      (x sb-alien:double-float)))

(progn
  (sb-alien:define-alien-routine ("j0f" j0)
      sb-alien:single-float
    (x sb-alien:single-float))
 
  (sb-alien:define-alien-routine ("j1f" j1)
      sb-alien:single-float
    (x sb-alien:single-float))
 
  (sb-alien:define-alien-routine ("jnf" jn)
      sb-alien:single-float
    (n sb-alien:int)
    (x sb-alien:single-float)))

(deftype my-float-helper ()
  `single-float)

(deftype my-float (&optional (low '*) (high '*))
  `(single-float ,(if (eq low '*)
		      '*
		      (coerce low 'my-float-helper)) 
		 ,(if (eq high '*)
		      '*
		      (coerce high 'my-float-helper))))

(defconstant +one+ #.(coerce 1 'my-float))
 
(defun abs2 (z) 
  (declare ((complex my-float) z)
	   (values my-float &optional))
  (let* ((x (realpart z))
         (y (imagpart z)))
    (+ (* x x) (* y y))))


(declaim (type fixnum n)
	 (type my-float /sin-alpha /sin-alpha2  sin-alpha2)
	 (type (my-float -1 1) sin-alpha)
	 (type (simple-array my-float 1) ac as as^2 ac2))       

(progn
 (defparameter n 31)
 (defparameter ac (make-array n :element-type 'my-float))
 (defparameter as (make-array n :element-type 'my-float))
 (defparameter as^2 (make-array n :element-type 'my-float))
 (defparameter ac2 (make-array n :element-type 'my-float))
 (defparameter sin-alpha (coerce .4 'my-float))
 (defparameter sin-alpha2 (* sin-alpha sin-alpha))
 (defparameter /sin-alpha (/ sin-alpha))
 (defparameter /sin-alpha2 (/ sin-alpha2)))

(defun init (&key (numerical-aperture (coerce 1.38 'my-float)) 
	     (immersion-index (coerce 1.515 'my-float))
	     (integrand-evaluations 31))
  (declare (my-float numerical-aperture immersion-index)
	   (fixnum integrand-evaluations)
	   (values null &optional))
  (setf n (+ 1 (* 2 (floor integrand-evaluations 2))) ;; make sure its odd
	ac (make-array n :element-type 'my-float)
	as (make-array n :element-type 'my-float)
	as^2 (make-array n :element-type 'my-float)
	ac2 (make-array n :element-type 'my-float)
	sin-alpha (/ numerical-aperture immersion-index)
	sin-alpha2 (* sin-alpha sin-alpha)
	/sin-alpha (/ sin-alpha)
	/sin-alpha2 (/ sin-alpha2))
  (let* ((alpha (asin sin-alpha))
	 (dalpha (/ alpha n)))

    (dotimes (iter n)
      (let* ((theta (the (my-float 0d0 2d0) (* dalpha iter)))
	    (ct (cos theta))
	     (st (sin theta))
	     (ct2 (sqrt ct))
	    (st^2 (* st st)))
	(declare ((my-float 0 1) ct)) ;; for the sqrt
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

(defun transform-xyz-to-uv (x y z &key (immersion-index (coerce 1.515 'my-float))
			    (numerical-aperture (coerce 1.38 'my-float))
			    (wavelength (coerce .480 'my-float)))
  "Return the cylindrical coordinates u and v of a given 3D point. All
lengths in micrometer."
  (declare (my-float x y z immersion-index numerical-aperture wavelength)
	   (values my-float my-float &optional))
  (let* ((k (/ (* #.(coerce 2 'my-float) (coerce pi 'my-float) immersion-index)
	       wavelength))
	 (sina (/ numerical-aperture
		  immersion-index))
	 (u (* k z sina sina))
	 (v (* k (sqrt (+ (* x x) (* y y))) sina)))
   (values u v)))

#+nil
(transform-xyz-to-uv 0.0 1.0 1.0)

(defun intermediate-integrals-point (u v)
  "Calculate the three integrals I1, I2 and I3 at a point u,v."
  (declare (my-float u v)
	   (values (complex my-float) (complex my-float) 
		   (complex my-float) &optional))
  (let* ((zero #.(complex (coerce 0 'my-float)))
	 (i0 zero)
	 (i1 zero)
	 (i2 zero))
     (let ((scale 1))
       (dotimes (iter n)
         (let* ((s (aref as iter))
                (c (aref ac iter))
                (c2 (aref ac iter))
                (vv (* v s /sin-alpha))
                (uu (* u c /sin-alpha2))
                (e (exp (complex #.(coerce 0 'my-float) uu)))
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
     (let* ((s (* n #.(coerce 3 'my-float))))
       (values (* s i0) (* s i1) (* s i2)))))

#+nil
(init :integrand-evaluations 301)
#+nil
(intermediate-integrals-point .0 .0)

(defun energy-density (u v)
  (declare (my-float u v)
	   (values my-float &optional))
  (multiple-value-bind (i0 i1 i2)
      (intermediate-integrals-point u v)
    (+ (abs2 i0)
       (abs2 i1)
       (abs2 i2))))

#+nil
(energy-density .0 .0)

(defun energy-density-cyl (&optional (nu 100) (nv 100) (du #.(coerce .1 'my-float))
			   (dv du))
  "Calculate a 2D image containing the energy density in cylindrical coordinates."
  (declare (fixnum nu nv)
	   (my-float du dv)
	   (values (simple-array my-float 2) &optional))
  (let* ((res (make-array (list nu nv) 
                          :element-type 'my-float)))
    (dotimes (iu nu)
      (let ((u (* iu du)))
       (dotimes (iv nv)
         (let ((v (* iv dv)))
           (setf (aref res iu iv) (energy-density u v))))))
    res))
 
(defun intermediate-integrals-cyl (&optional (nu 100) (nv 100)
				   (du (coerce .1 'my-float)) (dv du))
  (declare (fixnum nu nv)
	   (my-float du dv)
	   (values (simple-array (complex my-float) 2)
		   (simple-array (complex my-float) 2) 
		   (simple-array (complex my-float) 2) &optional))
  (let* ((dims (list nu nv))
	 (ii0 (make-array dims :element-type '(complex my-float)))
	 (ii1 (make-array dims :element-type '(complex my-float)))
	 (ii2 (make-array dims :element-type '(complex my-float))))
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

;; size radius is either the extend in x or y (in um) depending on
;; what is bigger, by default they are chosen so that the intensity
;; psf is sufficiently sampled
(defun electric-field-psf
    (z y x &key
     (numerical-aperture (coerce 1.38 'my-float)) 
     (immersion-index (coerce 1.515 'my-float))
     (wavelength (coerce .480 'my-float))
     (integrand-evaluations 31)
     (size-z (let* ((alpha (asin (/ numerical-aperture immersion-index)))
		    (dz (/ wavelength (* 2.0 immersion-index (- 1 (cos alpha))))))
	       (* z dz))) 
     (size-radius (let* ((dxy (/ wavelength (* 4.0 numerical-aperture))))
		    (* dxy (max x y)))))
  (declare (fixnum z y x integrand-evaluations)
	   (my-float size-z size-radius numerical-aperture immersion-index
		     wavelength)
	   (values (simple-array (complex my-float) 3)
		   (simple-array (complex my-float) 3)
		   (simple-array (complex my-float) 3) &optional))
  (init :numerical-aperture numerical-aperture
	:immersion-index immersion-index
	:integrand-evaluations integrand-evaluations)
  (let* ((nradius (1+ (ceiling (* (sqrt (* +one+ 2)) (max x y)))))
	 (nz (ceiling z 2))
	 (dims (list z y x))
	 (e0 (make-array dims :element-type '(complex my-float)))
	 (e1 (make-array dims :element-type '(complex my-float)))
	 (e2 (make-array dims :element-type '(complex my-float))))
    (multiple-value-bind (u v)
	(transform-xyz-to-uv #.(coerce 0 'my-float)
			     (* (sqrt (* +one+ 2)) size-radius)
			     (* +one+ .5 size-z)
			     :numerical-aperture numerical-aperture
			     :immersion-index immersion-index
			     :wavelength wavelength)
      (multiple-value-bind (i0 i1 i2)
	  (intermediate-integrals-cyl nz nradius
				      (/ (* +one+ u) nz) (/ (* +one+ v) nradius))
	(let ((rad-a (make-array (list y x) :element-type 'my-float))
	      (cphi-a (make-array (list y x) :element-type 'my-float))
	      (c2phi-a (make-array (list y x) :element-type 'my-float))
	      (s2phi-a (make-array (list y x) :element-type 'my-float)))
	  (do-region ((j i) (y x))
	    (let* ((ii (- i (floor x 2)))
		   (jj (- j (floor y 2)))
		   (one #.(coerce 1 'my-float))
		   (radius (sqrt (+ (* one ii ii) (* jj jj))))
		   (phi (atan (* one jj) ii)))
	      (setf (aref rad-a j i) radius
		    (aref cphi-a j i) (cos phi)
		    (aref c2phi-a j i) (cos (* one 2 phi))
		    (aref s2phi-a j i) (sin (* one 2 phi)))))
	  (let ((neg-i #.(complex (coerce 0 'my-float) (coerce -1 'my-float)))
		(del (if (eq 1 (mod z 2))
			 #.(coerce 0 'my-float)
			 #.(coerce .5 'my-float)))) ;; add .5 if z is even
	   (do-region ((k j i) (nz y x))
	     (let* ((zi (- nz k (- +one+ del)))
		    (r (aref rad-a j i))
		    (v0 (interpolate i0 zi r))
		    (v1 (interpolate i1 zi r))
		    (v2 (interpolate i2 zi r)))
	       (setf (aref e0 k j i) 
		     (* neg-i (+ v0 (* v2 (aref c2phi-a j i))))
		     (aref e1 k j i) 
		     (* neg-i v2 (aref s2phi-a j i))
		     (aref e2 k j i) (* +one+ -2 v1 (aref cphi-a j i))))))
	  (do-region ((k j i) (nz y x))
	    (setf (aref e0 (- z k 1) j i) (- (conjugate (aref e0 k j i)))
		  (aref e1 (- z k 1) j i) (- (conjugate (aref e1 k j i)))
		  (aref e2 (- z k 1) j i) (conjugate (aref e2 k j i)))))))
    (values e0 e1 e2)))

#+nil
(progn (electric-field-psf 10 10 10 3.0 3.0) nil)

(defun intensity-psf-cyl (z radius &key 
			  (numerical-aperture #.(coerce 1.38 'my-float)) 
			  (immersion-index #.(coerce 1.515 'my-float))
			  (nz 50) (nradius 50)
			  (wavelength #.(coerce .480 'my-float))
			  (integrand-evaluations 31)) 
  "Calculate 2D cylindrical PSF with axial extend Z micrometers and
transversal extend RADIUS micrometers."
  (declare (fixnum nz nradius integrand-evaluations)
	   (my-float z radius numerical-aperture immersion-index
		     wavelength)
	   (values (simple-array my-float 2) &optional))
  (psf:init :numerical-aperture numerical-aperture 
	    :immersion-index immersion-index
	    :integrand-evaluations integrand-evaluations)
  (multiple-value-bind (u v)
      (transform-xyz-to-uv #.(coerce 0 'my-float) radius z
		  :numerical-aperture numerical-aperture
		  :immersion-index immersion-index
		  :wavelength wavelength)
    (let* ((a (energy-density-cyl nz nradius (/ u nz) (/ v nradius))))
      a)))
#+nil
(time
 (progn (intensity-psf-cyl (coerce 3 'my-float) (coerce 1.5 'my-float))
	nil))

(defun intensity-psf (z y x size-z size-radius &key 
		      (numerical-aperture #.(coerce 1.38 'my-float))
		      (immersion-index #.(coerce 1.515 'my-float))
		      (integrand-evaluations 31)
		      (wavelength #.(coerce .480 'my-float)))
   "Calculate an intensity point spread function for an aplanatic
microobjective with the given NUMERICAL-APERTURE, IMMERSION-INDEX and
WAVELENGTH. Distances in micrometer."
     (declare (fixnum z y x integrand-evaluations)
	      (my-float size-z size-radius numerical-aperture immersion-index
			wavelength)
	      (values (simple-array (complex my-float) 3) &optional))
   (let* ((psf (make-array (list z y x) :element-type '(complex my-float)))
	  (nz (1+ (ceiling z 2)))
	  (nradius (1+ (ceiling (* (sqrt (* +one+ 2)) (max x y)))))
	  (cyl (intensity-psf-cyl
		(* +one+ .5 size-z) 
		(* (sqrt (* +one+ 2)) size-radius)
		:nz nz :nradius nradius :numerical-aperture numerical-aperture
		:immersion-index immersion-index
		:integrand-evaluations integrand-evaluations
		:wavelength wavelength)))
     (let ((rad-a (make-array (list y x) :element-type 'my-float)))
       (do-region ((j i) (y x))
	 (let* ((ii (- i (floor x 2)))
		(jj (- j (floor y 2)))
		(radius (sqrt (+ (* +one+ ii ii) (* jj jj)))))
	   (setf (aref rad-a j i) radius)))
       (let ((del (if (eq 1 (mod z 2)) ;; add .5 when z is even
		      (* +one+ 0)
		      (* +one+ .5))))
	 (do-region ((k j i) (nz y x))
	   (setf (aref psf k j i)
		 (complex (interpolate cyl
				       (-  nz k (- +one+ del)) 
				       (aref rad-a j i))))))
       (do-region ((k j i) (nz y x))
	 (setf (aref psf (- z k 1) j i) (aref psf k j i))))
     psf))


#+nil
(time (init))

;; cylindrical coordinates:
;; v is rho
;; u is z 

#+nil ;; print out a cross section of the psf transversal 1.5 um and
      ;; axial 3 um wide.
(let ((numerical-aperture (coerce 1.38 'my-float))
      (immersion-index (coerce 1.515 'my-float))) 
  (init :numerical-aperture numerical-aperture
	:immersion-index immersion-index)
  (multiple-value-bind (u v)
      (transform-xyz-to-uv (coerce 0 'my-float) (coerce 1.5 'my-float)
			   (coerce 3 'my-float) 
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
