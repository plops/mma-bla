;; for i in `cat lens.lisp|grep "^(defun"|cut -d " " -f2`;do echo \#:$i ;done
(in-package :lens)

(declaim (optimize (speed 2) (safety 3) (debug 3)))

(defgeneric intersect (ray object))

(defmethod intersect ((ray ray) (plane plane))
  "Find the point where a ray intersects a plane."
  (declare (values vec &optional))
  (with-slots (center normal) plane
    (with-slots ((start vector::start) (dir vector::direction)) ray
      (let* ((hess-dist (v. center normal)) ;; distance of plane to origin
	     (div (v. normal dir)))
	(when (< (abs div) 1d-12) ;; plane and ray are parallel
	  (error 'lost-ray))
	(let ((eta (/ (- hess-dist (v. normal start))
		      ;; scaling along ray to hit exactly on plane
		      div)))
	  (v+ start (v* dir eta)))))))

#+nil
(intersect 
 (make-instance 'ray :start (v 0 1 -1) :direction (v 0 0 1))
 (make-instance 'plane :normal (v 0 0 1) :center (v)))

(defgeneric refract (ray object))

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

(defmethod refract ((ray ray) (lens lens))
  "Return new ray after refraction on thin lens. In general you will
have to normalize its direction. The refraction on an objective needs
the non-normalized result. When the ray doesn't hit the lens the
condition RAY-LOST is signalled."
  (declare (values ray &optional))
  (check-direction-norm ray)
  (with-slots ((start vector::start)
	       (dir vector::direction)) ray
    (with-slots ((c center)
		 (n normal)
		 (f focal-length)
		 (r radius)) lens
      (check-unit-norm n)
      (let* ((i (intersect ray lens))
             (rho (v- i c)))
	(when (< r (norm rho)) ;; ray doesn't hit free aperture of lens
          (error 'ray-lost))
        (let* ((cosphi (v. n dir)))
          (make-instance 'ray
			 :start i
                         :direction (v- (v* dir (/ f cosphi))
                                        rho)))))))
#+nil
(handler-case 
    (refract (make-instance 'ray 
			    :start (v 0 .1 -10)
                            :direction (v 0 0 1))
             (make-instance 'lens 
                            :focal-length 10.0
                            :center (v)
                            :normal (v 0 0 1)
                            :radius 200d0))
  (ray-lost () 3))

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

;; 2008 Hwang Simulation of an oil immersion objective lens...
(defmethod refract ((ray ray) (objective objective))
  "Returns the refracted ray with the starting point on the principle
sphere of the objective. If the cap of the principal sphere (given by
NA) is missed then the condition LOST-RAY is signalled."
  (declare (values ray &optional))
  (check-direction-norm ray)
  (with-slots ((start vector::start)
	       (dir vector::direction)) ray
    (with-slots ((c center)
		 (n normal)
		 (f focal-length) 
		 (rad radius)
		 (bfprad bfp-radius)
		 (ri immersion-index) 
		 (na numerical-aperture)) objective
      (check-unit-norm n)
      ;; call refract for lens and refine the result
      (let* ((lens-ray (call-next-method ray objective))
             (r (vector::direction lens-ray))
             (i (vector::start lens-ray)) ;; intersection with lens plane
             (a (v* n (* f (- ri 1))))
             (ru (v+ r a))
             (rho (v- i c))
             (rho2 (v. rho rho))
             (nf (* ri f))
             (nf2 (* nf nf))
             (rat (- nf2 rho2)))
        (when (<= rat 0d0) ;; ray doesn't hit principal sphere
          (error 'ray-lost))
        (let* ((s (v* dir (- nf (sqrt rat))))
               (ro (v- ru s))
	       (nro (normalize ro))
               (cosu (v. nro n))
               (sinu2 (- 1 (* cosu cosu)))
               (sinu-max (/ na ri)))
          (when (<= (* sinu-max sinu-max) sinu2) ;; angle to steep
            (error 'ray-lost))
          (make-instance 'ray
                         :direction ro 
                         :start (v+ s i)))))))
#+nil
(handler-case 
    (refract (make-instance 'ray 
                            :direction (normalize (v 0 .001 1))
                            :start (v 0 0 -10))
             (make-objective :center (v)
			     :normal (v 0 0 1)))
  (ray-lost () 'lost))

;; 	  +-------+--
;; 	  |	  |  \-----
;;     h  |	  |	   \-----
;; 	  |	  |	         \----
;; 	  |	  |	   phi	      \-----
;;   -----+-------+------------o------------\------------------------
;; 	   (n-1)f     f	              f

;; 2005wolf p. 180 formula (8) sine condition for one image in infinity 
(defmethod find-inverse-ray-angle ((objective objective) point-x point-y)
  "Find the angle in the back focal plane for a ray that originates
from a POINT (coordinates in mm) in the focal plane in the object. The
coordinates of the point are given relative to the center of the front
focal plane."
  (declare (double-float point-x point-y)
	   (values double-float &optional))
  (with-slots (focal-length)
      objective
    (let* ((h (sqrt (+ (* point-x point-x) (* point-y point-y))))
	   (phi (asin (/ h focal-length))))
      phi)))

#+nil
(let* ((f (focal-length-from-magnification 63d0))
       (ri 1.515d0)
       (obj (make-objective :immersion-index ri
			    :normal (v 0d0 0d0 -1d0)
			    :center (make-vec 0d0 0d0 (- f))))
       (bfp-radius (back-focal-plane-radius obj))
       (field (make-vec .0d0 0d0 (- (+ f (* ri f)))))
       (pupil (make-vec bfp-radius 0d0 0d0)))
  (find-inverse-ray-angle obj 0d0 .07d0))
