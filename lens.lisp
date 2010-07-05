#.(require :vector)
(defpackage :lens
  (:use :cl :vector)
  (:export #:plane-ray
	   #:lens-ray
	   #:thin-objective-ray
	   #:back-focal-plane-radius
	   #:focal-length-from-magnification
	   #:etendue
	   #:oil-objective-etendue
	   #:magnification-from-angles
	   #:mirror-ray
	   #:make-thin-objective
	   #:rotate-vector
	   #:find-inverse-ray-angle))

;; for i in `cat lens.lisp|grep "^(defun"|cut -d " " -f2`;do echo \#:$i ;done
(in-package :lens)

(declaim (optimize (speed 2) (safety 3) (debug 3)))

(declaim (ftype (function (disk vec vec)
			  (values vec (or null vec) &optional))
		plane-ray))
(defun plane-ray (disk ray-start ray-direction)
  "Find the point where a ray intersects a plane. It returns two
vectors first the unchanged direction of the beam and then the
intersection point. The ray is defined by one position RAY-START and a
direction RAY-DIRECTION. The plane is defined by a point PLANE-CENTER
and its normal PLANE-NORMAL."
  (with-slots (center normal)
      disk
    (let* ((hess-dist (v. center normal)) ;; distance of plane to origin
	   (div (v. normal ray-direction)))
      (if (< (abs div) 1d-13)
	  ;; plane and ray are parallel and don't intersect
	  (values ray-direction nil)
	  (let ((eta (/ (- hess-dist (v. normal ray-start))
		      ;; scaling along ray to hit exactly on plane
		      div))) 
	    (values ray-direction
		    (v+ ray-start (v* ray-direction eta))))))))
#+nil
(plane-ray (make-disk) (v 0d0 .1d0 -1d0) (v 0d0 0d0 1d0))

;;
;;		   --\
;;		  |   -----\   intersection
;;		  | 	    --+
;;	   |	   \	   ---|--\
;;	   |	   | -----/   |   ----\
;;	   |   ----+/  	r'    |	       ----\
;;       f |--/    c	      |		    ----\      dir
;;         |  	r	      |		         ---\
;;	   |  		      |rho		     ----\
;;	   |   		      |				  ----\
;;	   |   		      |				       ----\
;;	   |   		      |				  phi	    ----\
;;  -------+----------<-------+-------------------------------------------
;;	   |             n    |	center
;;	   |     	      |
;;	   |     	      |
;;	   |    	      |
;;	      		      |
;;	     	      f	      |


(declaim (ftype (function (lens vec vec
				&key (:normalize boolean))
			  (values (or null vec) vec &optional))
		lens-ray))
(defun lens-ray (lens
		 ray-start ray-direction
		 &key (normalize t))
  "Return new ray-direction and intersection after thin lens."
  (with-slots (center normal focal-length radius)
      lens
    (unless (< (abs (- 1 (norm ray-direction))) 1d-12)
      (error "ray-direction doesn't have unit length"))
   (unless (< (abs (- 1 (norm normal))) 1e-12)
     (error "lens-normal doesn't have unit length"))
   (multiple-value-bind (dir intersection)
       (plane-ray lens
		  ray-start ray-direction)
     (declare (ignore dir))
     (let* ((rho (v- intersection center)))
       (format t "~a~%" (list 'intersection intersection 'center center
			      'rho rho 'norm (norm rho)))
       (unless (< (norm rho) radius)
	 (return-from lens-ray (values nil intersection)))
       (let* ((cosphi (v. normal ray-direction))
	      (r (v- (v* ray-direction (/ focal-length cosphi))
		     rho)))
	 (values (if normalize
		     (normalize r)
		     r) intersection))))))
#+nil
(lens-ray (make-lens)
	  (v 0d0 .1d0 -2d0)
	  (v 0d0 0d0 1d0))

;;
;;				   |
;;				   |
;;  ---------------	s	   |
;;-/  		   X----	   |
;;                /     \-- ----   |
;;             ro/         \--  \--+----
;;               |          ----/  |    \-------
;;    	        /     -----/    \  |            \------
;;             / ----/ ru        \ | 	               \-------
;;            /-/                 \|rho		               \-------
;;     	                           |			               \---
;;                     	           |
;;   ------------------------------+-----------------------------------------
;;                     	           |
;;     	       |        nf         /
;;             +--------------------
;;             |                 /

(declaim (ftype (function (thin-objective
			    vec vec)
			  (values (or null vec) vec &optional))
		thin-objective-ray))
;; 2008 Hwang Simulation of an oil immersion objective lens...
(defun thin-objective-ray (objective
			   ray-start ray-direction)
  "Returns direction of outgoing ray and intersection with principal
sphere. If this sphere isn't hit inside the maximum angle (given by
NA), then nil as a direction and the intersection with the
lens-surface is returned instead."
  (with-slots (center normal focal-length
		      radius ;; bfp-radius
		      immersion-index numerical-aperture)
      objective
    (unless (< (abs (- 1 (norm normal))) 1d-12)
      (error "lens normal doesn't have unit length"))
   (unless (< (abs (- 1 (norm ray-direction))) 1d-12)
     (error "ray-direction doesn't have unit length"))

   (multiple-value-bind (r intersection)
       (lens-ray objective
		 ray-start ray-direction :normalize nil)
     (unless r
       (return-from thin-objective-ray (values nil intersection)))
     (let* ((a (v* normal (* focal-length (- immersion-index 1))))
	    (ru (v+ r a))
	    (rho (v- intersection center))
	    (rho2 (v. rho rho))
	    (nf (* immersion-index focal-length))
	    (nf2 (* nf nf))
	    (rat (- nf2 rho2)))
       (unless (<= 0d0 rat)
	 (return-from thin-objective-ray (values nil intersection))
	 #+nil	(error "ray can't pass through objective"))
       (let* ((s (v* ray-direction (- nf (sqrt rat))))
	      (ro (v- ru s))
	      (cosu (v. ro normal))
	      (sinu2 (- 1 (* cosu cosu)))
	      (sinu-max (/ numerical-aperture immersion-index)))
	 (unless (<= sinu2 (* sinu-max sinu-max))
	   (return-from thin-objective-ray (values nil intersection)))
	 (values ro (v+ s intersection)))))))

#+nil
(thin-objective-ray (v 0d0 0d0 0d0)
		    (v 0d0 0d0 -1d0)
		    2.3d0 1.515d0
		    (v 0d0 1d0 10d0)
		    (normalize (v 0d0 0d0 -1d0)))


;; 		        |
;; --------		|
;;   SS    \---		|
;;             \--      |
;;                X-----+-------------------------------
;;              /-  \   |
;;            /-     \  |
;;    nf=h  /-        \ | D/2=R
;;        /-           \|
;;      /-              |
;;    /- alpha	        |
;; ---------------------+----------------------------------------
;;             nf       |
;;
;; sin(alpha) = R/nf


(declaim (ftype (function (double-float double-float)
			  (values double-float &optional))
		back-focal-plane-radius))
(defun back-focal-plane-radius (focal-length numerical-aperture)
  (* focal-length numerical-aperture))
#+nil
(back-focal-plane-radius 2.61d0 1.4d0)

(declaim (ftype (function (double-float)
	                  (values double-float &optional))
		focal-length-from-magnification))
(defun focal-length-from-magnification (mag)
  (/ 164.5 mag))
#+nil
(focal-length-from-magnification 63d0)

(declaim (ftype (function (double-float double-float
					&optional  double-float
					double-float double-float)
	                  (values double-float &optional))
		etendue))
(defun etendue (chief-height marginal-angle
		&optional
		(refractive-index 1d0)
		(marginal-height 0d0) (chief-angle 0d0))
  (let ((q (- (* chief-height refractive-index marginal-angle)
	      (* marginal-height refractive-index chief-angle))))
    (* q q)))
#+nil
(etendue .07d0 (* 67d0 (/ pi 180)) 1.515d0)


(declaim (ftype (function (double-float &optional double-float
					double-float)
	                  (values double-float &optional))
		oil-objective-etendue))
(defun oil-objective-etendue (field-radius &optional (numerical-aperture 1.4d0)
			      (refractive-index 1.515d0))
  (let ((rat (/ numerical-aperture refractive-index)))
    (unless (<= (abs rat) 1)
      (error "impossible angle, check numerical aperture and refractive index."))
   (let* ((marginal-angle (asin rat)))
     (etendue field-radius marginal-angle refractive-index))))
#+nil
(oil-objective-etendue .07d0)

(declaim (ftype (function (double-float double-float
					&optional double-float double-float)
	                  (values double-float &optional))
		 magnification-from-angles))
(defun magnification-from-angles (u uu &optional (n 1d0) (nn 1d0))
  "u is the objects marginal ray angle and uu for the image."
  (/ (* n (sin u))
     (* nn (sin uu))))

(declaim (ftype (function (mirror vec vec)
			  (values (or null vec) vec &optional))
		mirror-ray))
;;   sketch of the mirror for incoming parallel light
;;   --------------------+-----------------------
;; 		      /|\
;; 		     / | \
;; 		    / n|  \	       N=n*(- (p.n))
;; 		q  /   |   \  p	       p+N=r
;; 	          /    v    \	       q=N+r
;; 	         /           \
;; 	        /      |      \	       q=p-2(p.n)*n
;; 	       /       |       \
;; 	      /       N|        \
;; 	     /         |         \
;; 	    /          |     r    \
;; 	   /   	       v<----------\
;; p .. ray-direction
;; N .. mirror-normal

(defun mirror-ray (mirror ray-start ray-direction)
  "Mirror center C, mirror normal N.
Return reflected direction and intersection. If the ray isn't inside
of the radius return nil and the intersection."
  (with-slots (center normal radius)
      mirror
    (unless (< (abs (- 1 (norm normal))) 1d-12)
      (error "mirror normal doesn't have unit length"))
   (unless (< (abs (- 1 (norm ray-direction))) 1d-12)
     (error "ray-direction doesn't have unit length"))
   (multiple-value-bind (dir intersection)
       (plane-ray mirror ray-start ray-direction)
     (declare (ignore dir))
     (unless (< (norm (v- intersection center)) radius)
       (return-from mirror-ray (values nil intersection)))
     (let ((dir (v+ ray-direction (v* normal
				      (* -2d0 (v. ray-direction normal))))))
       (values dir intersection)))))

(deftype optical-component ()
  `(or disk mirror lens thin-objective))


(defstruct ray
  (start (v) :type vec)
  (direction (v) :type vec))

(defstruct disk
  (normal (v 0d0 0d0 1d0) :type vec)
  (center (v) :type vec)
  (radius 1d0 :type double-float))

(defstruct (mirror (:include disk)))

(defstruct (lens (:include disk))
  (focal-length 1d0 :type double-float))

(defstruct (thin-objective (:include lens))
  (immersion-index 1.515d0 :type double-float)
  (numerical-aperture 1.38d0 :type double-float))

(declaim (ftype (function (double-float vec vec)
			  (values vec &optional))
		rotate-vector))
(defun rotate-vector (angle axis vector)
  (let ((m (rotation-matrix angle axis)))
    (m* m vector)))

;; 	  +-------+--
;; 	  |	  |  \-----
;;     h  |	  |	   \-----
;; 	  |	  |	         \----
;; 	  |	  |	   phi	      \-----
;;   -----+-------+------------o------------\------------------------
;; 	   (n-1)f     f	              f
(declaim (ftype (function (double-float double-float thin-objective)
			  (values double-float &optional))
		find-inverse-ray))
;; 2005wolf p. 180 formula (8) sine condition for one image in infinity 
(defun find-inverse-ray-angle (point-x point-y objective)
  "Find the angle in the back focal plane for a ray that originates
from a POINT in the object. The coordinates of the point are given
relative to the center of the front focal plane."
  (with-slots (focal-length)
      objective
    (let* ((h (sqrt (+ (* point-x point-x) (* point-y point-y))))
	   (phi (asin (/ h focal-length))))
      phi)))

#+nil
(let* ((f (focal-length-from-magnification 63d0))
       (na 1.38d0)
       (ri 1.515d0)
       (bfp-radius (back-focal-plane-radius f na))
       (obj (make-thin-objective :normal (v 0d0 0d0 -1d0)
				 :center (v 0d0 0d0 (- f))
				 :focal-length f
				 :numerical-aperture na
				 :immersion-index ri))
       (field (v .0d0 0d0 (- (+ f (* ri f)))))
       (pupil (v bfp-radius 0d0 0d0)))
  (list bfp-radius f (find-inverse-ray-angle 0 .07 obj)))
