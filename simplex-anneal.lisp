(defpackage :simplex-anneal
  (:use :cl)
  (:export #:make-simplex
	   #:anneal))
(in-package :simplex-anneal)

;; nr3.pdf p.526 10.5 downhill simplex

#|| A simplex in N dimensions has N+1 points, i.e. a triangle in 2D.
You give one ND point. The algorithm constructs the simplex around
it (distances depend on the problems characteristic length scale)
 and goes downhill.

There are five different types of steps:
a) reflection (conserving the volume)
b) reflection and expansion
c) contraction
d) multiple-contraction

We keep track of highest and next to highest

Termination:
Step is fractionally smaller than some tolerance tol.
OR
Decrease in function value is fractionally smaller than some tolerance tol.

tol > sqrt(eps), tol can be \approx eps

If a minimum is found, restart the routine with 2 new points and the
current best one.

3x2 matrix defines simplex in 2D (rows are coordinates).

Store 3 function values.
Determine highest, next highest and lowest (best) point.

Fractional range from lowest value l to highest value h:
rtol=2 |h-l| / ( |h| + |l| + TINY) 
TINY is 1e-20 in cvd

Extrapolate by factor -1 through the face oposite of the high point.
 - If result better than best point extrapolate by two in same direction.
 - Else if worse than second highest do 1d contraction of 0.5,
   searching for lower one

amotry does the extrapolation
it gets the points psum, the values, a double fac

fac1=(1-fac)/n
fac2=fac1-fac   = 1/n - fac/n -fac = 1/n - fac ( 1/n - 1 )

mirrored point
j goes over n:
try_j= psum_j *fac1 -p [high]_j *fac2

if the new value is better than the worst (highest) replace that point

get_psum has 2 values for 2D and contains the sum of the coordinates
of all points

in cvd the reflection is around the centroid
alpha 1  reflected size
rho 2  expansion ratio
gamma .5 contraction ratio
sigma .5 shrink ratio
epsilon sqrt(eps) tolerance

For simulated annealing add a positive logarithmic distributed random
variable proportional to the temperature: (* temp (log (random))) to
every stored function value of the vertices. A similar variable is
subtracted from every point that is tried.

At finite T the simplex has approximately the shape of the region,
that can be reached at this temperature. It tumbles with Brownian
motion within that region.

Possible anneling temperature schedules:
- T=(1-eps)*T after every m moves. eps/m is determined by experiment.
- Budget K moves. Reduce T after m moves to T_0*(1-k/K)^alpha with k the
  count of moves so far and a constant alpha=1,2 or 4 or something. alpha
  depends on the statistical distribution of relative minima at
  various depths. Larger alpha spend more iterations at low temperature.
- After every m moves set T to beta*(f_1-f_b) with beta\approx 1, f_1 is the
  smallest function value currently in the simplex and f_b is the best
  value ever encountered. But never reduce T by more than some
  fraction gamma.

An occasional restart may help.

||#

(declaim (optimize (speed 2) (safety 3) (debug 3)))

(defparameter *print-debug-message* nil)

(defmacro dformat (&rest rest)
  `(progn ;; when simplex-anneal::*print-debug-message* 
     (format ,@rest)))

#+nil
(dformat t "~a~%" (list 'test))

(declaim (ftype (function (double-float double-float)
			  (values double-float &optional))
		heat))
(defun heat (temperature value)
  (- value (* temperature (log (random 1d0)))))

(defmacro aref-heat (temperature array index)
  "Access an array and change values with random fluctation."
  `(heat ,temperature (aref ,array ,index)))

(declaim (ftype (function ((simple-array double-float *) double-float)
			  (values fixnum double-float
				  fixnum double-float
				  fixnum double-float &optional))
		find-extrema))
(defun find-extrema (values temperature) 
  "Find the best, second-worst and worst point in the array of simplex
function values."
  (let* ((best 0)
	 (worst best)
	 (bestv (aref-heat temperature values best))
	 (worstv bestv)
	 (second-worstv 0d0)
	 (second-worst 0))
    ;; be careful to maintain the right relationship between worst
    ;; and second-worst when initializing: if 0 could be the worst
    ;; one use something better, so check 1 and use this if its
    ;; better.
    (let ((help (aref-heat temperature values 1)))
      (if (< help worstv)
	  (setf second-worst 1 ;; 1 is better
		second-worstv help)
	  (setf second-worst 0 ;; 1 is worse
		second-worstv worstv
		worst 1 
		worstv help)))
    ;; we don't have to loop over 0, so start with 1
    (loop for i from 1 below (length values) do 
      (let ((v (aref-heat temperature values i)))
	(when (< v bestv)
	  (setf bestv v
		best i))
	(when (< worstv v)
	  (setf worstv v
		worst i))
	(unless (eq i worst)
	  (when (< second-worstv v)
	    (setf second-worstv v
		  second-worst i)))))
    (dformat t "~a~%" (list 
		       'best best bestv 
		       'second-worst second-worst second-worstv
		       'worst worst worstv))
    (values best bestv second-worst second-worstv worst worstv)))

(declaim (ftype (function ((array double-float *)
			   (array double-float *))
			  (values (simple-array double-float *) &optional))
		v+ v- v-inc))
(defun v+ (a b)
  (let* ((n (array-dimension a 0))
	 (result (make-array n :element-type 'double-float)))
    (dotimes (i n)
      (setf (aref result i) (+ (aref a i) (aref b i))))
    result))

(defun v- (a b)
  (let* ((n (array-dimension a 0))
	 (result (make-array n :element-type 'double-float)))
    (dotimes (i n)
      (setf (aref result i) (- (aref a i) (aref b i))))
    result))

(defun v-inc (vec-result vec)
  "Increase the values in VEC-RESULT with values from VEC. Return the
modified VEC-RESULT."
  (let* ((n (array-dimension vec-result 0)))
    (dotimes (i n)
      (incf (aref vec-result i) (aref vec i)))
    vec-result))

(declaim (ftype (function ((array double-float *)
			   (array double-float *))
			  (values (array double-float *) &optional))
		v-set))
(defun v-set (vec-result vec)
  "Set the values in the vector VEC-RESULT to the contents of VEC."
  (let* ((n (array-dimension vec-result 0)))
    (dotimes (i n)
      (setf (aref vec-result i) (aref vec i)))
    vec-result))

(declaim (ftype (function ((array double-float *)
			   double-float)
			  (values (simple-array double-float *) &optional))
		v*))
(defun v* (vec scalar)
  (let* ((n (array-dimension vec 0))
	 (result (make-array n :element-type 'double-float)))
    (dotimes (i n)
      (setf (aref result i) (* scalar (aref vec i))))
    result))

(declaim (ftype (function ((simple-array double-float (* *))
			   fixnum)
			  (values (array double-float (*)) &optional))
		displace))
(defun displace (simplex i)
  "Extract the i-th i=0..n point from the simplex."
  (let ((n (array-dimension simplex 1))) ;; we want the small index
    (make-array n 
		:element-type 'double-float
		:displaced-to simplex
		:displaced-index-offset (* i n))))

(declaim (ftype (function ((array double-float (*))
			   (array double-float (*))
			   double-float)
			  (values (array double-float (*)) &optional))
		v-extrapolate))
(defun v-extrapolate (vec-a vec-b alpha)
  "Extrapolate the two vectors like this: <1+alpha> a - alpha b."
  (v+ (v* vec-a (1+ alpha)) 
      (v* vec-b (- alpha))))

(declaim (ftype (function ((simple-array double-float (* *)) fixnum)
			  (values (simple-array double-float *) &optional))
		calc-centroid))
(defun calc-centroid (simplex worst)
  "Go through all n+1 points in the simplex except the worst and sum
the coordinates. Return the centroid."
  (destructuring-bind (nr-points n)
      (array-dimensions simplex) ;; first dimension is bigger (n+1)
    (let ((result (make-array n :element-type 'double-float
			      :initial-element 0d0)))
      (dotimes (i nr-points)
	(unless (eq i worst)
	  (v-inc result (displace simplex i))))
      (dotimes (j n)
	(setf (aref result j) (/ (aref result j) n)))
      (dformat t "~a~%" (list 'centroid result))
      result)))


(declaim (ftype (function ((simple-array double-float (* *))
			   function 
			   &key (:itmax fixnum)
			   (:ftol double-float)
			   (:temperature double-float))
			  (values double-float 
				  (simple-array double-float *)
				  double-float
				  double-float 
				  (simple-array double-float *)
				  &optional))))
(defun amoeba (simplex funk &key (itmax 500) (ftol 1d-5) (temperature 1000d0))
  (destructuring-bind (nr-points n)
      (array-dimensions simplex)
   (let* ((alpha 1d0) ;; reflected size
	  (rho 2d0) ;;  expansion ratio
	  (gamma .5d0) ;; contraction ratio
	  (sigma .5d0) ;; shrink ratio
	  (iteration 0)
	  (best-ever (make-array n :element-type 'double-float))
	  (best-ever-value 1d100)
	  (values (make-array nr-points :element-type 'double-float)))
     ;; evaluate function on all vertices
     (dotimes (i nr-points)
       (setf (aref values i)
	     (funcall funk (displace simplex i))))
     (tagbody
      label1
	(incf iteration)
	(multiple-value-bind (best best-value
				   second-worst second-worst-value
				   worst worst-value)
	    (find-extrema values temperature)
	  (declare (ignore second-worst))
	  (when (< best-value best-ever-value) ;; store the best value 
	    (v-set best-ever (displace simplex best))
	    (setf best-ever-value best-value))
	  (dformat t "~a~%" (list 'best-ever best-ever best-ever-value))
	  ;;      rtol=2 |h-l| / ( |h| + |l| + TINY) 
	  (let ((rtol (* 2d0 (/ (abs (- best-value worst-value))
				(+ (abs best-value)
				   (abs worst-value)
				   1d-20)))))
	    (when (or (< rtol ftol)
		      (< itmax iteration))
	      (return-from amoeba (values best-value
					  (displace simplex best)
					  rtol
					  best-ever-value
					  best-ever))))
	  
	  (macrolet ((replace-worst (new-point new-value)
		       `(progn
			  (v-set (displace simplex worst) ,new-point)
			  (setf (aref values worst) ,new-value))))
	    (let* ((centroid (calc-centroid simplex worst))
		   ;; reflect worst point around centroid
		   (reflected (v-extrapolate centroid (displace simplex worst) alpha))
		   (reflected-value (funcall funk reflected))
		   (reflected-heat (heat (- temperature) reflected-value)))
	      (dformat t "~a~%" (list 'reflected reflected reflected-value))
	      ;; if new point better than current best, expand the simplex
	      (when (< reflected-heat best-value)
		(let* ((expanded (v+ (v* reflected rho)
				     (v* centroid (- 1 rho))))
		       (expanded-value (funcall funk expanded))
		       (expanded-heat (heat (- temperature) expanded-value)))
		  (dformat t "~a~%" (list 'expanded expanded expanded-value))
		  ;; keep whichever is best
		  (if (< expanded-heat reflected-value)
		      (replace-worst expanded expanded-value)
		      (replace-worst reflected reflected-value))
		  (go label1)))
	      ;; new point lies between others, keep:
	      (when (< reflected-heat second-worst-value)
		(dformat t "keep~%")
		(replace-worst reflected reflected-value)
		(go label1))
	      ;; new point is better than the worst one we had, contract:
	      (when (< reflected-heat worst-value)
		(let* ((contracted (v-extrapolate centroid 
						  (displace simplex worst)
						  gamma))
		       (contracted-value (funcall funk contracted))
		       (contracted-heat (heat (- temperature) contracted-value)))
		  (dformat t "~a~%" (list 'contracted
					 contracted contracted-value))
		  ;; use it if it helped
		  (when (< contracted-heat reflected-value)
		    (replace-worst contracted contracted-value)
		    (go label1))))
	      ;; otherwise if reflected is worse than everything else or
	      ;; contracted was worse than reflected shrink the whole
	      ;; simplex around the best point
	      (dotimes (i nr-points)
		(unless (eq i best)
		  (v-set (displace simplex i)
			 (v+ (displace simplex best)
			     (v* (v- (displace simplex i)
				     (displace simplex best))
				 sigma)))
		  (setf (aref values i) (funcall funk (displace simplex i)))))
	      (go label1))))))))

(declaim (ftype (function ((simple-array double-float (* *))
			   function 
			   &key (:itmax fixnum) (:ftol double-float)
			   (:start-temperature double-float))
			  (values double-float 
				  (simple-array double-float *) &optional))))
(defun anneal (simplex funk &key (itmax 500) (ftol 1d-5) (start-temperature 100d0))
  (let* ((m 30)
	 (eps/m .02d0)
	 (eps (* eps/m m))
	 (temp start-temperature))
    (do ((count 0 (incf count)))
	(())
      (multiple-value-bind (value point rtol best-ever-value best-ever)
	  (amoeba simplex funk :itmax m :ftol ftol 
		  :temperature temp)
	(declare (ignore best-ever-value
			 best-ever))
	(when (or (< itmax count)
		  (< rtol ftol))
	  (return-from anneal (values value point))))
      (setf temp (* (- 1 eps) temp)))))

;; run the following code to test the downhill simplex optimizer on a
;; 2d function:
#+nil
(time (let ((start (make-array 2 :element-type 'double-float
			  :initial-contents (list 1.5d0 1.5d0))))
   (anneal (make-simplex start 1d0)
	   #'rosenbrock
	   :ftol 1d-5)))

(declaim (ftype (function ((simple-array double-float *)
			   double-float)
			  (values (simple-array double-float (* *)) &optional))
		make-simplex))
(defun make-simplex (start step)
  (let* ((n (array-dimension start 0))
	 (result (make-array (list (1+ n) n) :element-type 'double-float)))
    (dotimes (j (1+ n))
      (v-set (displace result j) start))
    ;; change the first n points by step
    ;; the last point in result stays unaltered
    (dotimes (j n)
      (incf (aref result j j) step))
    
    result))

;; (declaim (ftype (function (double-float)
;; 			  (values double-float &optional))
;; 		sq))
;; (defun sq (x)
;;   (* x x))

;; (declaim (ftype (function ((array double-float *))
;; 			  (values double-float &optional))
;; 		rosenbrock))
;; (defun rosenbrock (p)
;;   (let* ((x (aref p 0))
;; 	 (y (aref p 1))
;; 	 (result (+ (sq (- 1 x))
;; 		    (* 100 (sq (- y (sq x)))))))
;;     (dformat t "~a~%" (list 'rosenbrock p result))
;;     result))
;; #+nil
;; (rosenbrock (make-array 2 :element-type 'double-float
;; 			 :initial-contents (list 1.5d0 1.5d0)))