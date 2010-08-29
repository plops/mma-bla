(in-package :frontend)

(defmethod draw-hidden-spheres ((model sphere-model))
  (with-slots (centers-mm radii-mm) model
    (gl:with-pushed-matrix
      (gl:line-width 1)
      (let ((n 4))
       (loop for c in centers-mm and r in radii-mm do
	    (gl:with-pushed-matrix
	      (translate-v c)
	      (let ((c .13))
	       (gl:color c c c))
	      (glut:solid-sphere r (* 2 n) n)
	      (gl:color .2 .2 .2)
	      (gl:line-width 5)
	      (glut:wire-sphere (* 1.06 r) (* 2 n) n)
	      (gl:color .5 .5 .5)
	      (gl:line-width 1)
	      (glut:wire-sphere (* 1.12 r) (* 2 n) n)))))))

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

(let ((rot 0)
      (tex nil)
      (new-tex nil)
      (scale '(300)))
  ;; call update-tex from anywhere to upload image data, the closure
  ;; stores the data until ensure-uptodate-tex is called from within
  ;; an opengl context
  (defun update-tex (data)
    (setf new-tex data))
  (defun ensure-uptodate-tex ()
    (when new-tex
      (when tex
	(destroy tex)
	(setf tex nil))
      (setf tex (make-instance 'texture-luminance-ub8 :data new-tex))
      (setf new-tex nil)))
  (defun update-scale (target-value &optional (steps 10))
    (let* ((current (car scale))
	   (exponent (if (< target-value current)
			 .01d0
			 7d0))) 
      (setf scale
	    (loop for i from 1 upto steps collect
		(let* ((x (/ (* 1d0 i) steps))
		       (y (expt x exponent)))
		  (+ (* (- 1 y) current) (* y target-value)))))))
  (defmethod draw ((model sphere-model) &key (nucleus 0)
		  (objective (lens:make-objective :normal (v 0 0 1)
						  :center (v)))
		  (bfp-ratio-x 0d0)(bfp-ratio-y 0d0))
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
		(y-mm (vec-y cent))
		(z-mm (vec-z cent))   
		(nf (* ri f))
		(ez (v 0 0 1)))
	   (progn
	     (gl:enable :depth-test)
	     (when (< 360 (incf rot 10))
	       (setf rot 0))
	     (gl:rotate (+ 120 (* 15 (expt (* .5 
					      (1+ (sin (* 2 pi 
							  (/ rot 360)))))
					   3.3))) 0 0 1)
	     (gl:translate 0 0 (- nf))
	     (let ((s (if (cdr scale)
			  (pop scale)
			  (car scale))))
	       (gl:scale s s s))
	     (draw-axes)
	     (translate-v (v* ez (- nf)))
	     (gl:with-pushed-matrix
	       (translate-v (v* ez (- nf z-mm)))
	       (draw-hidden-spheres model)))
	   (let ((lens (make-instance 'lens:disk :center (v) :radius bfp-radius))
		 (bfp (make-instance 'lens:disk :center (make-vec 0d0 0d0 (- f))
				     :radius bfp-radius)))
	     (gl:color .4 .4 .4)
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
	     (let* ((z+ (- nf z-mm))
		    (z- (+ nf (- (* 1d-3 ri dz z) z-mm)))
		    (x+ (* 1d-3 ri dx x))
		    (y+ (* 1d-3 ri dy x))
		    (p+z (plane :z z+))
		    (p-z (plane :z z-))
		    (bfps '(:left :left :right :right :top :top :bottom :bottom
			    :left :right :bottom :top))
		    (samples '(:left :right :right :left :bottom :top :bottom :top
			       :center :center :center :center)))
	       #+nil(gui::draw p+z)
	       #+nil(gui::draw p-z)
	       (let* ((start (make-vec 0d0 0d0 z+))
		      (dim (make-vec x+ y+ (* 1d-3 ri dz z))))
		 (gl:color .6 .6 .6)
		 (draw-wire-box start (v+ start dim)))
	       (handler-case
		   (loop for bfp-pos in bfps and sample-pos in samples do
			(multiple-value-bind (exit enter)
			    (make-ray objective model
				      nucleus sample-pos
				      bfp-ratio-x
				      bfp-ratio-y 
				      .02d0 bfp-pos)
			  ;; draw light ray from back focal plane through sample
			  (let ((h+z (lens:intersect exit p+z))
				(h-z (lens:intersect exit p-z)))
			    (gl:line-width 7)
			    (gl:with-primitive :lines
			      (gl:color .8 .3 .3)
			      (vertex-v (vector::start enter))
			      (vertex-v (vector::start exit))
		       
			      (vertex-v (vector::start exit))
			      (vertex-v h+z)
		       
			      (gl:color .3 .8 .3)
			      (vertex-v h+z)
			      (vertex-v h-z)))))
		 (ray-lost () nil))
	     
	       (let* ((ty (/ (* 1d0 (vec-i-y (elt centers 0)))
			     y)))
		 (progn ;; load and display the 3d texture
		   (gl:color 1 1 1 1)
		   (ensure-uptodate-tex)
		   (when tex
		    (draw-xz tex x+ 0d0 z+ z- :ty ty :y y-mm))))))))))))

