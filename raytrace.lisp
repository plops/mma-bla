#.(progn
    (require :asdf)
#+nil    (require :simple-gl)
    (require :simplex-anneal))

;; for i in `cat raytrace.lisp|grep defun|grep -v '^;'|cut -d " " -f2`;
;; do echo \#:$i ;done


(defpackage :raytrace
#+nil  (:shadowing-import-from :cl close get special)
(:export #:v
	 #:v.
	 #:v-
	 #:v+
	 #:quadratic-roots
	 #:ray-sphere-intersection-length
	 #:direction
	 #:ray-spheres-intersection
	 #:vec
	 #:mat
	 #:make-sphere)
(:use :cl #+nil :gl #+nil :glut :simplex-anneal))

(in-package :raytrace)

(declaim (optimize (speed 2) (safety 3) (debug 3)))

(deftype vec ()
  `(simple-array double-float (3)))
(deftype mat ()
  `(simple-array double-float (3 3)))

(declaim (ftype (function (&optional double-float double-float double-float)
			  (values vec &optional))
		v))
(defun v (&optional (x 0d0) (y 0d0) (z 0d0))
  (make-array 3 :element-type 'double-float
	      :initial-contents (list x y z)))

(declaim (ftype (function (vec vec)
			  (values double-float &optional))
		v.))
(defun v. (a b)
  "Dot product between two vectors."
  (let ((sum 0d0))
    (declare (double-float sum))
    (dotimes (i 3)
      (incf sum (* (aref a i)
		   (aref b i))))
    sum))

(declaim (ftype (function (vec vec)
			  (values vec &optional))
		v- v+))
(defun v- (a b)
  "Subtract two vectors."
  (let ((result (v)))
    (dotimes (i 3)
      (setf (aref result i) (- (aref a i)
			       (aref b i))))
    result))

(defun v+ (a b)
  "Add two vectors."
  (let ((result (v)))
    (dotimes (i 3)
      (setf (aref result i) (+ (aref a i)
			       (aref b i))))
    result))

(declaim (ftype (function (vec double-float)
			  (values vec &optional))
		v*))
(defmethod v* (a scalar)
  "Multiply vector with scalar."
  (declare (double-float scalar)
	   (vec a))
  (let* ((result (v)))
    (dotimes (i 3)
      (setf (aref result i) (* scalar (aref a i))))
    result))

(declaim (ftype (function (vec)
			  (values double-float &optional))
		norm))
(defmethod norm (a)
  "Length of a vector."
  (let ((l2 (v. a a)))
    (declare (type (double-float 0d0) l2)) ;; Otherwise warning with complex-double
    (sqrt l2)))

(declaim (ftype (function (vec)
			  (values vec &optional))
		normalize))
(defmethod normalize (a)
  "Rescale vector to unit length."
  (let ((len (norm a)))
    (if (eq len 0d0)
	(v 0d0 0d0 1d0)
	(v* a (/ len)))))

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
  (let* ((theta (* theta-degree (/ pi 180d0)))
	 (st (sin theta))
	 (phi (* phi-degree (/ pi 180d0))))
#+nil    (declare ((double-float 0d0 #.pi) theta) ;; ranges were a bit complicated
	     ((double-float 0d0 #.pi) phi))
    (v (* st (cos phi)) ;; convert spherical coordinates into cartesian
       (* st (sin phi))
       (* (cos theta)))))

#+nil
(direction 45 0)

#+nil 
(monte-carlo)

#+nil (defparameter *rays* '())

#+nil (defparameter centers 
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
#+nil (progn
 (defparameter *central-sphere* 22)
 (defparameter *spheres* 
   (let* ((q (elt centers *central-sphere*))
	  (s (/ 70d0))
	  (cen (v (* s (third q))
		  (* s (second q))
		  (* 5d0 s (first q)))))
     (loop for c in centers collect
	  (list (v- (v (* s (third c))
		       (* s (second c))
		       (* 5d0 s (first c)))
		    cen)
		(* s 11d0))))
   "center diameter"))

(defstruct sphere 
  (center (v) :type vec)
  (radius 0d0 :type double-float))

#+nil (dotimes (i 100)
  (setf *spheres* (append *spheres*
			  (list (list (v* (direction (random 180d0)
						     (random 360d0))
					  (1+ (random 1d0))) 
				      (+ .15d0 
					 (random .18d0)))))))
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

;; (declaim (ftype (function (vec)
;; 			  (values double-float &optional))
;; 		merit-function))
;; (defun merit-function (vec)
;;   (ray-spheres-intersection 
;;    (v .1d0 .2d0 0d0) 
;;    (normalize (direction (aref vec 0) (aref vec 1)))))

#+nil
(let ((start (make-array 2 :element-type 'double-float
			  :initial-contents (list 100d0 100d0))))
   (with-open-file (*standard-output* "/dev/shm/anneal.log"
				      :direction :output
				      :if-exists :supersede)
     (anneal (make-simplex start 1d0)
	     #'merit-function
	     :start-temperature 3d0)))

#+nil (RAY-SPHERE-INTERSECTION-LENGTH
 (v 0.0d0 0.0d0 0.0d0)
 (v 0.0d0 0.0d0 1.0d0)
 (v 0.0d0 0.0d0 0.0d0)
 0.15714285714285714d0)

;; scan over the full parameters spcae
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
 
#+nil
(ray-spheres-intersection (v .1d0 .2d0 0d0) 
			  (direction -1631995.127159034d0 -881148.5633918238d0))

;; (direction -1631995.127159034d0 -881148.5633918238d0)
;; #(0.5991641440565072d0 -0.6787435869828116d0 -0.4246286278699771d0)

;; smallest value in the plot
;; cat /dev/shm/o.dat |cut -d " " -f 3|sort -n|uniq -c|head


;; (declaim (ftype (function () (values (or null double-float) &optional))
;; 		sum-random-dir))
;; (defun sum-random-dir ()
;;   (let ((start (v (random 1d0) (random 1d0) 1d0))
;; 	(end (v 0d0 0d0 -1d0))
;; 	(sum 0d0))
;;     (setf *rays* (append *rays* (list (list start end))))
;;     (dolist (s *spheres*)
;;       (let ((len (ray-sphere-intersection-length 
;; 		  start (v- end start)
;; 		  (first s) (second s))))
;; 	(when len
;; 	  (incf sum len))))
;;     sum))

;; (declaim (ftype (function () (values double-float &optional))
;; 		monte-carlo))
;; (defun monte-carlo ()
;;   ;; nr3 p. 422
;;   (let ((f 0d0)
;; 	(f2 0d0)
;; 	(rel-err 20d0)
;; 	(val 0d0))
;;     (declare (double-float f f2 rel-err val)) 
;;     (do ((count 0))
;; 	((and (< 100 count)
;; 	      (< rel-err .02d0)))
;;       (declare (fixnum count))
;;       (let ((integ (sum-random-dir)))
;; 	(when integ
;; 	  (incf f integ)
;; 	  (incf f2 (* integ integ))
;; 	  (incf count)
;; 	  (when (< 1 count)
;; 	    (setf val (/ f count))
;; 	    (setf rel-err (/ (sqrt (/ (- (/ f2 count) (* val val))
;; 				      (1+ count)))
;; 			     val)))
;; 	 #+nil (format t "~a~%" (list count val rel-err)))))
;;     val))


;; (declaim (ftype (function (vec)
;; 			  (values null &optional))
;; 		vertex-v translate-v))
;; (defun vertex-v (vert)
;;   (vertex (aref vert 0) (aref vert 1) (aref vert 2))
;;   nil)
;; (defun translate-v (vert)
;;   (translate (aref vert 0) (aref vert 1) (aref vert 2))
;;   nil)

;; (declaim (ftype (function (double-float vec)
;; 			  (values null &optional))
;; 		rotate-v))
;; (defun rotate-v (angle vert)
;;   (rotate angle (aref vert 0) (aref vert 1) (aref vert 2))
;;   nil)

;; (defmacro with-light (&body body)
;;   `(progn 
;;      (enable :light0 :light1 :lighting)
;;      (shade-model :flat)
;;      (material :front :specular #(1d0 1d0 1d0 1d0))
;;      (material :front :shininess 70d0)
;;      (light :light0 :position #(12d0 32d0 3d0 1d0))
;;      (light :light0 :ambient #(.3d0 .3d0 .3d0 1d0))
;;      (light :light1 :position #(12d0 -32d0 -3d0 1d0))
;;      (light :light1 :diffuse #(1d0 1d0 1d0 1d0))
;;      ,@body
;;      (disable :light0 :light1 :lighting)))

;; (defparameter *rot* 0d0)

;; (defun draw ()
;;   (if (< *rot* 360d0)
;;       (incf *rot*)
;;       (setf *rot* 0d0))
;;   (clear :depth-buffer-bit)
;;   (with-primitive :lines
;;     (color 1 0 0) (vertex 0 0 0) (vertex 1 0 0)
;;     (color 0 1 0) (vertex 0 0 0) (vertex 0 1 0)
;;     (color 0 0 1) (vertex 0 0 0) (vertex 0 0 1))

;;   (rotate *rot* 0 0 1)
;;   (line-width 3)
;; #+nil  (enable :blend)
;;   (blend-func :src-alpha :one)
;;   (color 1 1 1 .2)
;;   (enable :depth-test)
;;   (with-light 
;;       (let ((s 10))
;; 	(dolist (e *spheres*)
;; 	 (with-pushed-matrix
;; 	   (translate-v (first e))
;; 	   (solid-sphere (second e) (* 2 s) s)))))
;;   (disable :depth-test)
;;   (color 1 1 1 .002)
;;   (with-primitive :lines
;;     (dolist (c *rays*)
;;       (vertex-v (first c))
;;       (vertex-v (second c))))
;;   (disable :blend)
;;   (scale 3 3 3)
;;   (with-primitive :lines
;;     (color 1 0 0) (vertex 0 0 0) (vertex 1 0 0)
;;     (color 0 1 0) (vertex 0 0 0) (vertex 0 1 0)
;;     (color 0 0 1) (vertex 0 0 0) (vertex 0 0 1)))

;; (defun run ()
;;   (simple-gl:with-simple-gl
;;       (draw)))

;; (eval-when (:execute)
;;  (run))
