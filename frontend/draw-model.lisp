(in-package :frontend)

(defmethod draw-hidden-spheres ((model sphere-model))
  (with-slots (centers-mm radii-mm) model
    (gl:with-pushed-matrix
      (gl:line-width 1)
      (loop for c in centers-mm and r in radii-mm do
	   (gl:with-pushed-matrix
	     (translate-v c)
	     (gl:color 0 0 0 1)
	     (glut:solid-sphere r 8 4)
	     (gl:color 1 1 1 1)
	     (glut:wire-sphere (* 1.08 r) 8 4))))))

;; sketch of the coordinate system:
;;
;; the objective sits below the sample. its (thin) lens has a distance
;; nf to the in-focus plane. z is directed from the objective towards
;; the sample. the first slice of the stack is furthest from the
;; objective.
;;
;;     	               ^ z
;;                     |
;;            	       |   /
;;           	 +-----+-/----+
;;       --------+-----/------+---------    nf
;;           	 +---/-+------+
;;            	    /  |
;;     \          /-   |               /
;;     	\       /-     |              /	principal
;;     	 -\   /-       |            /-	  sphere
;;         --/         |         /--
;;     	     |---\     |     /---
;;           | 	  -----+-----------------    0
;;   	     | 	       |
;;    	     | 	       |
;;           | 	       |
;;     	     |         |
;;       ----+---------+-----------------   -f
;;           |	       |   back focal plane
;;	               |

(defvar *rot* 0)
(defvar *tex* nil)
(defmethod draw ((model sphere-model) &key (nucleus 0)
		 (objective (lens:make-objective :normal (v 0 0 1)
						 :center (v))))
  (declare (fixnum nucleus)
	   (lens:objective objective))
  (with-slots (dimensions spheres centers-mm dx dy dz) model
    (with-slots ((f lens::focal-length)
		 (bfp-radius lens::bfp-radius)
		 (na lens::numerical-aperture)
		 (ri lens::immersion-index)) objective
     (destructuring-bind (z y x)
	 dimensions
       (let* ((cent (elt centers-mm nucleus))
	      (x-mm (vec-x cent))
	      (y-mm (vec-y cent))
	      (z-mm (vec-z cent))
	      (bfp-ratio-x .2d0 #+nil (- (random 2d0) 1d0))
	      (bfp-ratio-y 0d0)
	      (theta (lens:find-inverse-ray-angle objective x-mm y-mm))
	      (phi (atan y-mm x-mm))
	      (start (make-vec (* bfp-radius bfp-ratio-x)
			       (* bfp-radius bfp-ratio-y)
			       (- f)))
	      (nf (* ri f))
	      (ez (v 0 0 1)))
	 (progn
	   (gl:enable :depth-test)
	   (when (< 360 (incf *rot*))
	     (setf *rot* 0))
	   (translate-v (v* ez (- nf)))
	   (gl:rotate *rot* 0 0 1)
	   (let ((s 100))
	     (gl:scale s s s))
	   (draw-axes)
	   (translate-v (v* ez (- nf)))
	   (gl:with-pushed-matrix
	     (translate-v (v* ez (- nf z-mm)))
	     (draw-hidden-spheres model)))
	 (let ((lens (make-instance 'lens:disk :center (v) :radius bfp-radius))
	       (bfp (make-instance 'lens:disk :center (make-vec 0d0 0d0 (- f))
				    :radius bfp-radius)))
	  (gui::draw lens)
	  (gui::draw bfp))
	 (macrolet ((plane (direction position)
		      ;; for defining a plane that is perpendicular to an
		      ;; axis and crosses it at POSITION
		      (declare (type (member :x :y :z) direction))
		      (let* ((normal (ecase direction
				       (:x (v 1))
				       (:y (v 0 1))
				       (:z (v 0 0 1)))))
			`(let* ((pos ,position)
				(center (v* ,normal pos))
				(outer-normal (normalize center)))
			   (declare (type double-float pos))
			   (make-instance 'lens:disk
					  :radius (* ri .01)
					  :normal outer-normal
					  :center center)))))
	   (let ((p+z (plane :z (- nf z-mm)))
		 (p-z (plane :z (+ nf (* 1d-3 dz z)
				   (- z-mm)))))
	     (gui::draw p+z)
	     (gui::draw p-z)
	     (handler-case
		 (let* ((ro (lens:refract (make-instance 
					   'ray :start start
					   :direction (v-spherical theta phi))
					  objective))
		       (nro (make-instance 'ray 
					   :start (vector::start ro)
					   :direction (normalize 
						       (vector::direction ro)))))
		   (let ((h+z (lens:intersect nro p+z))
			 (h-z (lens:intersect nro p-z)))
		     (gl:line-width 7)
		     (gl:with-primitive :lines
		       (gl:color 1 0 0 1)
		       (vertex-v start)
		       (vertex-v (vector::start ro))
		       
		       (vertex-v (vector::start ro))
		       (vertex-v h+z)
		       
		       (gl:color 0 1 0 1)
		       (vertex-v h+z)
		       (vertex-v (v+ (make-vec x-mm y-mm 0d0) (v* ez nf)))

		       (gl:color 0 .7 1 1)
		       (vertex-v (v+ (make-vec x-mm y-mm 0d0) (v* ez nf)))
		       (vertex-v h-z))))
	       (ray-lost () nil))
	     #+NIL (multiple-value-bind (ro s)
		 (lens:thin-objective-ray obj
					  start
					  (make-vec (* (cos phi) (sin theta))
						    (* (sin phi) (sin theta))
						    (cos theta)))
	       (when ro
		 (let* ((nro (normalize ro)))
		   

		   #+nil(let* ((h+z (pixel (hit p+z)))
			       (h-z (pixel (hit p-z)))
			       (h+y (pixel (hit p+y)))
			       (h-y (pixel (hit p-y)))
			       (h+x (pixel (hit p+x)))
			       (h-x (pixel (hit p-x)))
			       ;; make a list of all the points
			       (hlist (list h+z h-z h+y h-y h+x h-x))
			       ;; throw away points that are nil or that contain
			       ;; coordinates outside of the array dimensions
			       (filtered-hlist
				(remove-if-not #'(lambda (v)
						   (if v
						       (and (< -1 (vec-i-x v) x)
							    (< -1 (vec-i-y v) y)
							    (< -1 (vec-i-z v) z))
						       nil)) hlist))
			       ;; sort best points by x
			       (choice (sort filtered-hlist #'< :key (lambda (v) (vec-i-x v)))))
			  (debug-out h+z h-z)
			  (format t "~a~%" (list 'choice choice))
			  #+nil (scan-convert-line3
				 (first choice)
				 (second choice)
				 *spheres-ub8*))))))))))))


