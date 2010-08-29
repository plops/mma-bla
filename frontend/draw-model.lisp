(in-package :frontend)

(defmethod draw-hidden-spheres ((model sphere-model))
  (with-slots (centers-mm radii-mm) model
    (gl:with-pushed-matrix
      (gl:line-width 1)
      (let ((n 4))
       (loop for c in centers-mm and r in radii-mm do
	    (gl:with-pushed-matrix
	      (translate-v c)
	      (gl:color 0 0 0 .2)
	      (glut:solid-sphere r (* 2 n) n)
	      (gl:color .7 .7 .7)
	      (glut:wire-sphere (* 1.08 r) (* 2 n) n)))))))

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

(defvar *new-tex* nil) ;; set this to a ub3 volume, draw will upload
		       ;; it next time it's called
(defvar *rot* 0)
(defvar *tex* nil)
(defmethod draw ((model sphere-model) &key (nucleus 0)
		 (objective (lens:make-objective :normal (v 0 0 1)
						 :center (v)))
		 (bfp-ratio-x 0d0)
		 (bfp-ratio-y 0d0))
  (declare (fixnum nucleus)
	   (lens:objective objective))
  (with-slots (dimensions spheres centers-mm centers dx dy dz) model
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
	   (gl:color .1 .1 .1)
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
		 (p-z (plane :z (+ nf (* 1d-3 ri dz z)
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
		   ;; draw light ray from back focal plane through sample
		   (let ((h+z (lens:intersect nro p+z))
			 (h-z (lens:intersect nro p-z)))
		     (gl:line-width 7)
		     (gl:with-primitive :lines
		       (gl:color .8 .3 .3)
		       (vertex-v start)
		       (vertex-v (vector::start ro))
		       
		       (vertex-v (vector::start ro))
		       (vertex-v h+z)
		       
		       (gl:color .3 .8 .3)
		       (vertex-v h+z)
		       (vertex-v (v+ (make-vec x-mm y-mm 0d0) (v* ez nf)))

		       (gl:color .3 .6 .8)
		       (vertex-v (v+ (make-vec x-mm y-mm 0d0) (v* ez nf)))
		       (vertex-v h-z))))
	       (ray-lost () nil))
	     
	     (let* ((z+ (- nf z-mm))
		    (z- (+ nf (- (* 1d-3 ri dz z) z-mm)))
		    (ty (/ (* 1d0 (vec-i-y (elt centers 0)))
			   y))
		    (x+ (* 1d-3 ri dx x)))
	       (progn ;; load and display the 3d texture
		 (gl:color 1 1 1 1)	
		 (when (and *new-tex* *tex*)
		   (destroy *tex*)
		   (setf *tex* nil))
		 (unless *tex*
		   (setf *tex* (make-instance 
				'texture-luminance-ub8
				:data *new-tex*))
		   (when *new-tex*
		     (setf *new-tex* nil)))
		 (draw-xz *tex* x+ 0d0 z+ z- :ty ty :y y-mm))
	       ;; draw rectangle
	       #+nil (gl:with-primitive :line-loop
		 (gl:color .5 .5 .5)
		 (dolist (v vertexs)
		   (vertex-v v)))))))))))

