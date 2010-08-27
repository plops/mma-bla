(in-package :frontend)

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

(defvar *obj* 0)
(defvar *spheres-ub8* nil)
(defun draw ()
  (destructuring-bind (z y x)
      *dims*
    (let* ((cent (sphere-center (aref *spheres-c-r* 0)))
	   (x-mm (vec-x cent))
	   (y-mm (vec-y cent))
	   (z-mm (vec-z cent))
	   (bfp-ratio-x (- (random 2d0) 1d0))
	   (bfp-ratio-y 0d0)
	   (f (lens:focal-length-from-magnification 63d0))
	   (na 1.38d0)
	   (ri 1.515d0)
	   (bfp-radius (lens:back-focal-plane-radius f na))
	   (obj (lens:make-thin-objective :normal (v 0 0 1)
					  :center (v)
					  :focal-length f
					  :radius bfp-radius
					  :numerical-aperture na
					  :immersion-index ri))
	   (theta (lens:find-inverse-ray-angle x-mm y-mm obj))
	   (phi (atan y-mm x-mm))
	   (start (make-vec (* bfp-radius bfp-ratio-x)
			    (* bfp-radius bfp-ratio-y)
			    (- f)))
	   (dx (* ri .2d-3))
	   (dz (* ri 1d-3))
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
    (gl:disable :lighting)
    (gl:line-width 3)
    (gl:with-primitive :lines
      (gl:color 1 0 0 1) (gl:vertex 0 0 0) (gl:vertex 1 0 0)
      (gl:color 0 1 0 1) (gl:vertex 0 0 0) (gl:vertex 0 1 0)
      (gl:color 0 0 1 1) (gl:vertex 0 0 0) (gl:vertex 0 0 1))
    (translate-v (v* ez (- nf)))
    (gl:with-pushed-matrix
      (translate-v (v* ez (- nf z-mm)))
      (gl:color 0 0 0 1)
      (dotimes (i (length *spheres-c-r*))
	(gl:with-pushed-matrix
	  (with-slots (center radius)
	      (aref *spheres-c-r* i)
	    (translate-v center)
	    (glut:solid-sphere radius 8 4))))
      (gl:color 1 1 1 1)
      (gl:line-width 1)

      (dotimes (i (length *spheres-c-r*))
	(gl:with-pushed-matrix
	  (with-slots (center radius)
	      (aref *spheres-c-r* i)
	    (translate-v center)
	    (glut:wire-sphere (* 1.08 radius) 8 4))))))

      #+nil (debug-out f bfp-radius theta phi)
      (draw-disk (make-vec 0d0 0d0 (- f)) bfp-radius)
      (draw-disk (make-vec 0d0 0d0 0d0) bfp-radius)
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
			(lens::make-disk :normal outer-normal :center center)))))
	(let ((p+z (plane :z (- nf z-mm)))
	      (p-z (plane :z (+ nf (- (* dz z) z-mm))))
	      #+nil (p+y (plane :y (* dx (- y cy))))
	      #+nil (p-y (plane :y (* dx (- (- y cy)))))
	      #+nil (p+x (plane :x (* dx (- x cx))))
	      #+nil (p-x (plane :x (* dx (- (- x cx))))))
	  (draw-disk (v* ez (- nf z-mm))              (* ri .01))
	  (draw-disk (v* ez (+ nf (- (* dz z) z-mm))) (* ri .01))
	  (multiple-value-bind (ro s)
	      (lens:thin-objective-ray obj
				       start
				       (make-vec (* (cos phi) (sin theta))
						 (* (sin phi) (sin theta))
						 (cos theta)))
	    (when ro
	      (let* ((nro (normalize ro)))
		(macrolet ((hit (plane)
			     ;; find intersection between plane and the ray
			     `(multiple-value-bind (dir hit-point)
				  (lens::plane-ray ,plane
						   s
						   nro)
				(declare (ignore dir))
				hit-point))
			   #+nil (pixel (hit-expr)
				   ;; convert coordinates from mm into integer pixel positions
				   `(let ((h ,hit-expr))
				      (declare (type (or null vec) h))
				      (when h
					(make-vec-i
					 :z (floor (+ cz (/ (+ (aref h 2) nf) dz)))
					 :y (floor (+ cy (/ (aref h 1) dx)))
					 :x (floor (+ cx (/ (aref h 0) dx))))))))
		  (let ((h+z (hit p+z))
			(h-z (hit p-z)))
		    (when (and h+z h-z)
		      (gl:line-width 7)
		      (gl:with-primitive :lines
			(gl:color 1 0 0 1)
			(vertex-v start)
			(vertex-v s)
		       
			(vertex-v s)
			(vertex-v h+z)
		       
			(gl:color 0 1 0 1)
			(vertex-v h+z)
			(vertex-v (v+ (make-vec x-mm y-mm 0d0) (v* ez nf)))

			(gl:color 0 .7 1 1)
			(vertex-v (v+ (make-vec x-mm y-mm 0d0) (v* ez nf)))
			(vertex-v h-z))))
		  (progn
		    #+nil
		    (format t "~f ~f~%"
			    bfp-ratio-x 
			    (ray-spheres-intersection (v- s (v* ez (- nf z-mm)))
						      nro *spheres-c-r* 0))
		    (dotimes (i (length *spheres-c-r*))
		      (with-slots (center radius)
			  (aref *spheres-c-r* i)
			(let ((ray-start s)
			      (ray-direction nro))
			  ;; (c-x)^2=r^2 defines the sphere, substitute x with the rays p+alpha a,
			  ;; the raydirection should have length 1, solve the quadratic equation,
			  ;; the distance between the two solutions is the distance that the ray
			  ;; travelled through the sphere
			  (let* ((l (v- center (v- ray-start (v* ez (- nf z-mm)))))
				 (c (- (v. l l) (* radius radius)))
				 (a ray-direction)
				 (b (* -2d0 (v. l a))))
			    #+nil (format t "~a~%" (list i center ray-start ray-direction))
			    (multiple-value-bind (x1 x2)
				(quadratic-roots 1d0 b c)
			      (when x1
				(gl:color 1 0 0 1)
				(gl:point-size 12)
				(gl:with-primitive :points
				  (vertex-v (v+ s (v* nro (- x1))))
				  (vertex-v (v+ s (v* nro (- x2))))))))))))
		 
		  (progn
		    (gl:color .8 1 .2 1)
		    (gl:with-primitive :lines
		      (vertex-v s)
		      (vertex-v (v+ s (v* nro 4.2d0)))))
		  (let ((z+ (- nf z-mm))
			(z- (+ nf (- (* dz z) z-mm))))
		    (gl:with-primitive :line-loop
		      (gl:color .5 .5 .5)
		      (vertex-v (make-vec 0d0 y-mm z+))
		      (vertex-v (make-vec (* dx x) y-mm z+))
		      (vertex-v (make-vec (* dx x) y-mm z-))
		      (vertex-v (make-vec 0d0 y-mm z-)))
		    (let* ((target :texture-rectangle-nv)
			   (im (cross-section-xz *spheres-ub8*
						 (vec-i-y (aref *centers* 0)))))
		      (defparameter *obj* (first (gl:gen-textures 1)))
		      (gl:bind-texture target *obj*)
		      (gl:enable target)
		      (gl:tex-parameter target :texture-min-filter :linear)
		      (gl:tex-parameter target :texture-mag-filter :linear)
		      (destructuring-bind (h w)
			  (array-dimensions im)
			(let* ((dat1 (sb-ext:array-storage-vector im)))
			  (sb-sys:with-pinned-objects (im)
			    (cffi:with-pointer-to-vector-data (ptr dat1)
			      (gl:tex-image-2d target 0 :luminance w h
					       0 :luminance :unsigned-byte ptr))))
			(let ((texcoords (list (v)
					       (make-vec (* 1d0 w) 0d0)
					       (make-vec (* 1d0 w) (* 1d0 h))
					       (make-vec 0d0 (* 1d0 h))))
			      (vertexs (list (make-vec (* dx x) y-mm z-)
					     (make-vec 0d0 y-mm z-)
					     (make-vec 0d0 y-mm z+)
					     (make-vec (* dx x) y-mm z+))))
			  (gl:with-primitive :quads
			    (dotimes (i (length vertexs))
			      (tex-coord-v (elt texcoords i))
			      (vertex-v (elt vertexs i)))))
			(gl:disable target)))))

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
			      *spheres-ub8*))))))))))


