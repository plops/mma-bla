#.(progn
    (require :asdf)
    (require :vector)
#+nil    (require :simple-gl)
    (require :simplex-anneal))

;; for i in `cat raytrace.lisp|grep defun|grep -v '^;'|cut -d " " -f2`;
;; do echo \#:$i ;done


(defpackage :raytrace
#+nil  (:shadowing-import-from :cl close get special)
(:export #:quadratic-roots
	 #:ray-sphere-intersection-length
	 #:direction
	 #:ray-spheres-intersection
	 #:sphere
	 #:center
	 #:radius
	 #:make-sphere
	 #:sphere-center
	 #:sphere-radius)
(:use :cl #+nil :gl #+nil :glut :vector :simplex-anneal))

(in-package :raytrace)

(declaim (optimize (speed 2) (safety 3) (debug 3)))

(declaim (ftype (function (double-float double-float double-float)
			  (values (or null double-float) 
				  &optional double-float))
		quadratic-roots))
(defun quadratic-roots (a b c)
  "Find the two roots of ax^2+bx+c=0 and return them as multiple
  values."
  ;; see numerical recipes sec 5.6, p. 251 on how to avoid roundoff
  ;; error
  (let ((det2 (- (* b b) (* 4 a c))))
    #+nil (let ((tiny 1d-12))
      (when (or (< (abs a) tiny)
		(< (abs c) tiny) 
		(< (abs b) tiny))
       ;; i get division by zero when centered sphere
	;; note: I didn't implement all the corner cases I think
       (return-from quadratic-roots nil #+nil (values 0d0 0d0))))
    (when (<= 0d0 det2) ;; we don't want complex results
      (let* ((pdet2 det2)
	     (q (* .5d0 (+ b (* (signum b) (sqrt pdet2)))))
	     (x1 (/ q a))
	     (x2 (/ c q)))
	(declare ((double-float 0d0) pdet2)) ;; its positive
	(values x1 x2)))))

#+nil 
(quadratic-roots 1d0 2d0 -3d0)
#+nil
(quadratic-roots 0d0 -0d0 0d0)

(declaim (ftype (function (vec vec vec double-float)
			  (values (or null double-float) &optional))
		ray-sphere-intersection-length))
(defun ray-sphere-intersection-length 
    (ray-start ray-direction sphere-center sphere-radius)
  ;; (c-x)^2=r^2 defines the sphere, substitute x with the rays p+alpha a,
  ;; the raydirection should have length 1, solve the quadratic equation,
  ;; the distance between the two solutions is the distance that the ray
  ;; travelled through the sphere
  (let* ((l (v- sphere-center ray-start))
	 (c (- (v. l l) (* sphere-radius sphere-radius)))
	 (a (normalize ray-direction))
	 (b (* -2d0 (v. l a))))
    (multiple-value-bind (x1 x2)
	(quadratic-roots 1d0 b c)
      (when x1
	(abs (- x1 x2))))))

#+nil
(ray-sphere-intersection-length (v 0d0 .1d0 -12d0) (v 0d0 0d0 1d0) (v) 3d0)

(defun direction (theta-degree phi-degree)
  "Convert spherical coordinates into cartesian."
  (let* ((theta (* theta-degree (/ pi 180d0)))
	 (st (sin theta))
	 (phi (* phi-degree (/ pi 180d0))))
    #+nil    (declare ((double-float 0d0 #.pi) theta) ;; ranges were a bit complicated
		      ((double-float 0d0 #.pi) phi))
    (make-vec (* st (cos phi))
	      (* st (sin phi))
	      (* (cos theta)))))

#+nil
(direction 45 0)


(defstruct sphere 
  (center (v) :type vec)
  (radius 0d0 :type double-float))

(declaim (ftype (function (vec vec (simple-array sphere 1) fixnum)
			  (values double-float &optional))
		ray-spheres-intersection))
(defun ray-spheres-intersection (ray-position ray-dir spheres
				 illuminated-sphere-index)
 (let ((sum 0d0))
   (dotimes (i (length spheres))
     (unless (eq i illuminated-sphere-index)
       (with-slots (center radius)
	   (aref spheres i)
	(let ((len (ray-sphere-intersection-length
		    ray-position ray-dir
		    center radius)))
	  (when len
	    (incf sum len))))))
   sum))

(defmethod print-object ((sphere sphere) stream)
  (with-slots (center radius) sphere
   (format stream "#<sphere radius: ~4f center: <~4f ~4f ~4f>>" 
	   radius (vec-x center) (vec-y center) (vec-z center))))

#+nil
(make-sphere :radius 3.1415d0 :center (v 1d0 2d0 3.032032d0)) 
 
#+nil 
(defparameter centers 
  '(( 6 87 157) ( 7 69 111) ( 7 87 196) ( 7 88 66) ( 7 92 33) ( 7 137 224)
    ( 7 144 149) ( 8 41 163) ( 8 61 201) ( 8 110 123)( 8 126 180)
    ( 8 131 99)( 9 34 107) ( 9 81 238) ( 10 166 208)( 11 58 77)  ( 11 111 245)
    ( 11 143 75)  ( 11 164 112)( 11 167 173)  ( 12 72 142) ( 12 99 76)
    ( 12 100 156) ( 12 122 44)  ( 12 144 248) ( 13 108 200)   ( 13 136 137)
    ( 14 138 221)   ( 15 50 219) ( 15 113 106)   ( 15 175 145)
    ( 16 56 107)  ( 16 77 189) ( 17 37 140)   ( 17 45 179)
    ( 17 89 51)   ( 17 162 187)( 18 88 240)    ( 18 129 171)
    ( 18 130 64) ( 18 163 188)( 19 151 99) ( 20 61 68)
    ( 21 81 139)  ( 21 86 97)( 21 136 210) ( 21 151 141)
    ( 22 48 109)( 22 53 151) ( 22 99 219)  ( 23 67 206)
    ( 23 98 187)   ( 23 107 72)( 25 107 146)( 25 113 111)))
#+nil 
(progn
 (defparameter *central-sphere* 22)
 (defparameter *spheres* 
   (make-array (length centers)
	       :element-type 'sphere
	       :initial-contents
	       (let* ((q (elt centers *central-sphere*))
		      (s (/ 70d0))
		      (cen (make-vec (* s (third q))
				     (* s (second q))
				     (* 5d0 s (first q)))))
		 (loop for c in centers collect
		      (make-sphere :center
				   (v- (v (* s (third c))
					  (* s (second c))
					  (* 5d0 s (first c)))
				       cen)
				   :radius (* s 22d0)))))
   "center diameter"))


#+nil
(progn
   (declaim (ftype (function ((array double-float (2)))
			     (values double-float &optional))
		   merit-function))
   (defun merit-function (vec)
     (ray-spheres-intersection 
      (v .1d0 .2d0 0d0) 
      (normalize (direction (aref vec 0) (aref vec 1)))
      *spheres*
      *central-sphere*)))

#+nil
(let ((start (make-array 2 :element-type 'double-float
			 :initial-contents (list 100d0 100d0))))
  (with-open-file (*standard-output* "/dev/shm/anneal.log"
				     :direction :output
				     :if-exists :supersede)
    (anneal (make-simplex start 1d0)
	    #'merit-function
	    :start-temperature 12d0)))

;; scan over the full parameters space
#+nil
(progn
 (with-open-file (s "/dev/shm/o.dat" :direction :output :if-exists :supersede)
   (let ((ntheta 60)
	 (nphi 90))
     (dotimes (theta ntheta)
       (dotimes (phi  nphi)
	 (let ((a (* 90 (/ theta ntheta)))
	       (b (* 180 (/ phi nphi))))
	  (format s "~f ~f ~f~%" a b
		  (ray-spheres-intersection (v) (direction a b)))))
      (terpri s))))
 (with-open-file (s "/dev/shm/p1.gp" :direction :output
		    :if-exists :supersede)
   (format s "set term posts; set outp \"/dev/shm/p~2,'0d.ps\";set hidden
set title \"nucleus nr. ~d\"
unset key
splot \"/dev/shm/o.dat\" u 1:2:3 w l
#pause -1
" *central-sphere* *central-sphere*)))
