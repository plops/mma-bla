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
;; the sample. the first slice of the stack is nearest to the
;; objective.  in the test sample the single sphere is centered on
;; slice 10, the plane of spheres is in slice 20.
;;
;;     	               ^ z
;;                     |
;;            	       |   /
;;           	 +-----+-/----+
;;       --------+-----/------+---------    z-plane-mm = z_slice * n * dz * 1d-3
;;           	 +---/-+------+       --  0
;;            	    /  |
;;     \          /-   |               /
;;     	\       /-     |              /	principal
;;     	 -\   /-       |            /-	  sphere
;;         --/         |         /--
;;     	     |---\     |     /---
;;           | 	  -----+-----------------    - nf + z-plane-mm
;;   	     | 	       |
;;    	     | 	       |
;;           | 	       |
;;     	     |         |
;;       ----+---------+-----------------   - f - nf + z-plane-mm
;;           |	       |   back focal plane
;;	               |

(defmacro pop-until-end (l)
    `(if (cdr ,l)
	 (pop ,l) ;; needs to be a macro so that pop has an effect
	 (car ,l)))

(let ((rot 0)
      (tex nil)
      (new-tex nil)
      (scale '(300))
      (view-center (list (v))))
  (defun update-tex (data)
    "Supply either an image or a volume of unsigned-byte. It will be
displayed as a texture."
    (setf new-tex data))
  (defun ensure-uptodate-tex ()
    "Call this function within an OpenGL context to check for new
texture data."
    (when new-tex
      (when tex
	(destroy tex)
	(setf tex nil))
      (setf tex (make-instance 'texture-luminance-ub8 :data new-tex))
      (setf new-tex nil)))
  (defun update-scale (target-value &optional (steps 10))
    "Smooth zooming. Meant to enable viewing of the microscopic sample
as well as the macroscopic objective with its back focal plane."
    (let* ((current (car scale))) 
      (setf scale
	    (loop for i from 1 upto steps collect
		 (let* ((x (/ (* 1d0 i) steps)))
		   (+ (* (- 1 x) current) (* x target-value)))))))
  (defun update-view-center (target-value &optional (steps 10))
    "Create smooth transition to different view center. This is meant
to shift the current nucleus into view."
    (let ((current (car view-center)))
      (setf view-center
	    (loop for i from 1 upto steps collect
		 (let* ((x (/ (* 1d0 i) steps)))
		   (v+ (v* current (- 1 x)) (v* target-value x)))))))
  (defun rotate-translate-sample-space ()
    "Wiggle sample around a point that has been set via
update-view-center."
     (when (< 360 (incf rot 10))
       (setf rot 0))
     (let ((center (pop-until-end view-center))
	   (angle (+ 120 (* 15 (expt (* .5 
					(1+ (sin (* 2 pi 
						    (/ rot 360)))))
				     3.3)))))
       (translate-v center)
       (gl:rotate angle 0 0 1)
       (translate-v (v* center -1d0))))
  (defmethod draw ((model sphere-model) &key (nucleus 0)
		   (objective (lens:make-objective :normal (v 0 0 1)
						   :center (v)))
		   (win-x/r 0d0) (win-y/r 0d0)
		   (win-r/r .02d0)
		   (z-plane-mm (vec-z (elt (raytrace::centers-mm model) nucleus))))
    (declare (fixnum nucleus)
	     (lens:objective objective)
	     (double-float win-x/r win-y/r win-r/r))
    (gl:clear-color .32 .3 .3 1)
    (with-slots (dimensions spheres centers-mm centers dx dy dz) model
      (with-slots ((f lens::focal-length)
		   (bfp-radius lens::bfp-radius)
		   (center lens::center)
		   (na lens::numerical-aperture)
		   (ri lens::immersion-index)) objective
	(setf center (make-vec 0d0 0d0 (+ (- (* ri f)) z-plane-mm)))
	(destructuring-bind (z y x)
	    dimensions
	  (let* ((cent (elt centers-mm nucleus))
		 (y-mm (vec-y cent))
		 (ez (v 0 0 1)))
	    (progn
	      (gl:enable :depth-test)
	      (let ((s (pop-until-end scale)))
		(gl:scale s s s))
	      #+nil (gl:translate 0 0 (- nf))
	      (rotate-translate-sample-space)
	      (draw-axes) ;; move axes into focal plane
	      (draw-hidden-spheres model) 
	      (let ((lens (make-instance 'lens:disk :center center 
					 :radius bfp-radius))
		    (bfp (make-instance 'lens:disk :center 
					(v- center (make-vec 0d0 0d0 f))
					:radius bfp-radius)))
		(gl:color .4 .4 .4) ;; draw planes defining the objective
		(gui::draw lens)
		(gui::draw bfp))
	      (labels ((plane (direction position)
			 "Define a plane that is perpendicular to
an axis and crosses it at POSITION."
			 (declare ((member :x :y :z) direction)
				  (double-float position))
			 (let* ((normal (ecase direction
					  (:x (v 1))
					  (:y (v 0 1))
					  (:z (v 0 0 1)))))
			   (let* ((center (v* normal position))
				  (outer-normal (normalize center)))
			     (make-instance 'lens:disk
					    :radius (* ri .01)
					    :normal outer-normal
					    :center center)))))
		(let* ((z+ 0d0)
		       (z- (* 1d-3 ri dz z))
		       (p-z (plane :z z-)) ;; slice that's furthest from objective
		       (x+ (* 1d-3 ri dx x))
		       (y+ (* 1d-3 ri dy x)))
		  (let* ((start (make-vec 0d0 0d0 z+)) ;; draw bounding box
			 (dim (make-vec x+ y+ z-)))
		    (gl:color .6 .6 .6)
		    (draw-wire-box start (v+ start dim)))
		  ;; rays from back focal plane through sample
		  (loop for (exit enter) in
		       (make-rays objective model nucleus 
				  (sample-circles 3 3 1)
				  win-x/r win-y/r win-r/r) do
		       (let ((h-z (lens:intersect exit p-z)))
			 (gl:line-width 1)
			 (gl:color .2 .6 .8)
			 (gl:with-primitive :line-strip
			   (vertex-v (vector::start enter))
			   (vertex-v (vector::start exit))
			   (vertex-v h-z))))
		  (let* ((ty (/ (* 1d0 (vec-i-y (elt centers 0)))
				y)))
		    (gl:color 1 1 1 1) ;; load and display the 3d texture
		    (ensure-uptodate-tex)
		    (when tex
		      (draw-xz tex x+ 0d0 z+ z- :ty ty :y y-mm))))))))))))
#+nil
(defparameter *look*
 (loop for (exit enter) in
      (make-rays (lens:make-objective) *model* 0 (sample-circles 2 2 1)
		 0d0 0d0 .1d0)
      collect
      (vector::start enter)))

#+nil
(defparameter *look*
 (list (lens:make-objective) (sample-circles 2 2 2)))