(in-package :frontend)

;; it turns out that using a few points on the border of the circles
;; is too sparse. the following scheme should sample the space a bit
;; better and therefore give a better approximation of the exposure
;; integral
;;
;;	          ---------
;;	      ---/         \---	   xy-cross section through a
;;           /       	       \      nucleus in the sample plane
;;         -/                   \-
;;        /                       \   choose s points along a line
;;        |                       | 	.
;;     	 /                         \	.
;;     	 0     	      1	       	   2  ...
;;     	 \                         /
;;        |                       |
;;        \                       /
;;         -\                   /-
;;           \       	       /
;;	      ---\         /--        	   -----------
;;	          ---------            ---/           \---
;;		               	     -/   	          \-
;;		                   -/ 	 back focal  	    \-
;;		                  / 	    plane  	      \
;;		       	         /  	 	 	       \
;;	              	         |  	 	 	       |
;;	     choose    	        /      	       	 	  	\
;;	   b points   ....      a     b	     c     d   	 e     	f
;;     along a lin              \      	 			/
;;     with the same   	         |	 		       |
;;     inclination theta         \	 		       /
;;		                  \	 		      /
;;  		                   -\ 	 		    /-
;;    		               	     -\   	          /-
;;     	       	       	               ---\           /---
;;  		                      	   -----------
;;
;; now shoot rays from each o the points in the bfp to each of the
;; points in the sample plane. we have to make sure that a nucleus
;; inside the illumination cone convolved with the cross section of
;; the illuminated nucleus is hit by at least one ray. the necessary
;; number of points s and b depends on the size of the nuclei as well
;; as the furthest distance to the border of the stack. for now i
;; don't want to think too hard about that. i guess it will be
;; sufficient to sample with .01 in the back focal plane and 3 points
;; in the nucleus.
;; two dimensional coordinates will be represented as complex numbers.

(defun sample-line (nr-ffp nr-bfp)
  "Create two lists like -1 0 1 and -1 -.5 0 .5 1. Here nr-ffp would
be 3 and nr-bfp would be 5. The result is the outer product -1 -1, -1
-.5, -1, 0, ..., a list with all interconnections. Note that the pair
0,0 isn't emitted as it would result in duplications when turned by
theta."
  (declare (fixnum nr-ffp nr-bfp))
  (let ((ffps (loop for i below nr-ffp collect
		   (complex (- (/ (* 2d0 i) (1- nr-ffp)) 1))))
	(bfps (loop for i below nr-bfp collect
		   (complex (- (/ (* 2d0 i) (1- nr-bfp)) 1))))
	(result nil))
    (loop for f in ffps do
	 (loop for b in bfps do
	      (unless (= (complex 0d0) f b) ;; prevent duplication of central ray
		(push (list f b) result))))
    (nreverse result)))

#+nil
(sample-line 3 3)

(defun sample-circles (nr-ffp nr-bfp nr-theta)
  "Create coordinates in front and backfocal plane that sample circles
  in a regular pattern."
  (declare (fixnum nr-ffp nr-bfp nr-theta))
  (let ((line (sample-line nr-ffp nr-bfp))
	(result nil))
    (loop for theta below nr-theta do
	 (let ((rotator (exp (complex 0d0 (/ (* 2d0 pi theta) nr-theta)))))
	   (loop for (f b) in line do
		(push (list (* rotator f)
			    (* rotator b))
		      result))))
    (when (and (oddp nr-ffp) (oddp nr-bfp)) ;; central ray was omitted above
      (push (list (complex 0d0) (complex 0d0)) result))
    (nreverse result)))

#+nil
(sample-circles 2 2 4)

;;				      ------+------
;;			          ---/      |      \---
;;			       --/    	    |---+---   \--
;;			     -/            -|   |   \-    \-
;;			    /       	  / | z |     \     \
;;		          -/   	         /  |   |  r   \     \-
;;		         /       	 |y +---+------+       \
;;		        /                \  |   |      /        \
;;		        |                 \ |   |     /         |
;;	               /       	     	   -|   |   /-           \
;;	               |                    |---+---      	 |
;;	       	      -+--------------------+---+----------------+--
;;	               |                    |   x       rr       |
;;	               \       	     	    |  	                 /

(defun move-complex-circle (z rr x/rr y/rr r/rr)
  "Given a complex number Z inside the unit circle, move the unit
circle to position X,Y inside the BFP with radius RR. Scale the unit
circle to the window-radius R."
  (declare ((complex double-float) z)
	   ((double-float -1d0 1d0) x/rr y/rr)
	   ((double-float 0d0 1d0) r/rr)
	   (double-float rr)
	   (values (complex double-float) &optional))
  (+ (complex (* x/rr rr) (* y/rr rr))
     (* r/rr rr z)))

#+nil
(move-complex-circle (complex 1d0 0d0) 2d0 .9d0 0d0 .1d0)

#+nil
(move-complex-circle (complex 1d0 0d0) 1d0 .9d0 0d0 .1d0)

(defmethod make-rays ((objective lens::objective) (model sphere-model)
		      nucleus positions win-x/r win-y/r win-r/r)
  "Given an objective and a nucleus in a model generate rays from a
circle on the back focal plane into the front focal plane. The pattern
of the rays is given as a list of 2-lists of complex numbers. The
first complex number gives the relative position inside the central
cross section of the nucleus and the second number gives the relative
position in the bfp. The coordinates and size of the window in the
back focal plane are given relative to the radius of the bfp. The
return value is a list of 2-lists of rays, where the first ray starts
from the principal sphere and the second ray from the bfp."
  (declare (fixnum nucleus)
	   (cons positions)
	   (double-float win-x/r win-y/r win-r/r)
	   (values cons &optional))
  (assert (subtypep (type-of (first (first positions))) '(complex double-float)))
  (assert (subtypep (type-of (second (first positions))) '(complex double-float)))
  (with-slots (centers-mm
	       radii-mm) model
    (let ((center (elt centers-mm nucleus))
	  (radius (elt radii-mm nucleus))
	  (result nil))
      (with-slots ((r lens::bfp-radius)
		   (ri lens::immersion-index)
		   (f lens::focal-length)) objective
	(loop for (f b) in positions do
	     (let ((br (move-complex-circle b r win-x/r win-y/r win-r/r))
		   (fr (move-complex-circle f 1d0 (vec-x center) (vec-y center)
					    radius)))
	       (handler-case
		   (multiple-value-bind (exit enter)
		       (lens:get-ray-behind-objective
			objective
			(realpart fr) (imagpart fr)
			(realpart br) (imagpart br))
		     (push (list exit enter) result))
		 (ray-lost () nil))))
	(nreverse result)))))

#+nil
(make-rays (lens:make-objective) *model* 0 (sample-circles 2 2 4)
	   .2d0 0d0 .1d0)

(defun merit-function (vec2 params)
  (declare ((simple-array double-float (2)) vec2)
	   (cons params)
	   (values double-float &optional))
  (destructuring-bind (objective model nucleus-index window-radius)
      params
   (let* ((border-value 0d0) ;; value to return when outside of bfp
	  ;; this has to be considerably bigger than the maxima on the bfp
	  (border-width window-radius) ;; in this range to the
	  ;; border of the bfp
	  ;; enforce bad merit
	  ;; function
	  (sum 0d0)
	  (radius (norm2 vec2)))
     (if (< radius (- .99d0 border-width))
	 ;; inside
	 (loop for dirs in '((:right :left)
			     (:top :bottom)) do
	      (loop for dir in dirs do
		   (loop for bfp-dir in dirs do
			(let ((ray (make-ray objective
					     model
					     nucleus-index dir
					     (vec2-x vec2) (vec2-y vec2)
					     window-radius bfp-dir)))
			  (incf sum
				(raytrace:ray-spheres-intersection
				 ray model nucleus-index))))))
	 ;; in the border-width or outside of bfp
	 (incf sum border-value))
     sum)))

(defun find-optimal-bfp-window-center (nucleus params)
  (declare (fixnum nucleus)
	   (cons params)
	   (values vec2 &optional))
  (setf (elt params 2) nucleus)
  (loop
     (multiple-value-bind (min point)
	 (simplex-anneal:anneal (simplex-anneal:make-simplex
				 (make-vec2 :x -1d0 :y -1d0) 1d0)
				#'merit-function
				;; set temperature bigger than the
				;; maxima in the bfp but smaller
				;; than border-value
				:start-temperature 2.4d0
				:eps/m .02d0
				:itmax 1000
				:ftol 1d-3
				:params params)
       (when (< min 100d0)
	 (return-from find-optimal-bfp-window-center point)))))

#+nil
(find-optimal-bfp-window-center 0)
