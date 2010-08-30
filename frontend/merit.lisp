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
    (when (and (oddp nr-ffp) (odd-p nr-bfp)) ;; central ray was omitted above
      (push (list (complex 0d0) (complex 0d0)) result))
    (nreverse result)))

#+nil
(sample-circles 2 2 4)

(defmethod make-rays ((objective lens::objective) (model sphere-model)
		     illuminated-sphere-index sample-position
		     bfp-ratio-x bfp-ratio-y window-radius-ratio
		     bfp-position)
  (declare (fixnum illuminated-sphere-index)
	   (double-float bfp-ratio-x bfp-ratio-y window-radius-ratio)
	   (values ray ray &optional))
  (with-slots (centers-mm
	       radii-mm) model
    (let ((center (elt centers-mm illuminated-sphere-index))
	  (radius (elt radii-mm illuminated-sphere-index)))
      (with-slots ((bfp-radius lens::bfp-radius)
		   (ri lens::immersion-index)
		   (f lens::focal-length)) objective
	(let* ((sample-pos (sample-circle
			    (complex (vec-x center) (vec-y center))
			    radius sample-position))
	       (bfp-pos (sample-circle (complex bfp-ratio-x bfp-ratio-y)
				       window-radius-ratio
				       bfp-position)))
	  (lens:get-ray-behind-objective
	   objective
	   (realpart sample-pos) (imagpart sample-pos)
	   (realpart bfp-pos)    (imagpart bfp-pos)))))))

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
