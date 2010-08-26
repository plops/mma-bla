(in-package :frontend)

(defmacro defstuff ()
  `(progn
     ,@(loop for i in '(*dims*    ;; dimensions of the input stack in
				  ;; pixels and slices
			*centers* ;; integral center coordinates of
				  ;; the nuclei (0 .. dim-x) ...
			*index-spheres* ;; each nuclei is drawn with its index
			*spheres-c-r* ;; scaled (isotropic axis, in
				     ;; mm) and shifted (so that
				     ;; origin in center of volume)
				     ;; coordinates
			)
	  collect
	    `(defvar ,i nil))))

(defstuff)

(defun get-visible-nuclei (k)
  "Find all the nuclei in slice K."
  (declare (fixnum k)
	   (values list &optional))
  (destructuring-bind (z y x)
      (array-dimensions *index-spheres*)
    (unless (< k z)
      (error "slice k isn't contained in array *index-spheres*."))
    ;; use bit-vector to store which nuclei are contained
    (let* ((n (length *centers*))
	   (result (make-array n
			       :element-type 'boolean
			       :initial-element nil)))
      (do-region ((j i) (y x))
	(let ((v (round (realpart (aref *index-spheres* k j i)))))
	  (when (< 0 v n)
	   (setf (aref result v) t))))
      (loop for i from 1 below n
	 when (aref result i)
	 collect
	 (1- i)))))
#+nil
(get-visible-nuclei 25)

#+nil
(time
 (loop for i below (array-dimension *index-spheres* 0)
    collect
    (list i (get-visible-nuclei i))))

;; create a volume containing just the current slice
(defun get-lcos-volume (k nucleus)
  (declare (fixnum k)
	   (values (simple-array (complex my-float) 3) &optional))
  (destructuring-bind (z y x)
      (array-dimensions *index-spheres*)
    (unless (< 0 k z)
      (error "slice index k out of range."))
    (let ((vol (make-array (list z y x)
			   :element-type '(complex my-float))))
      ;; only the current nucleus will be illuminated
      ;; note that nucleus 0 has value 1 in index-spheres
      (do-region ((j i) (y x))
	(if (< (abs (- nucleus (1- (aref *index-spheres* k j i)))) .5)
	    (setf (aref vol k j i) (aref *spheres* k j i))))
      vol)))

(defun write-section (fn vol &optional (y (floor (array-dimension vol 1) 2)))
  (declare (simple-string fn)
	   ((simple-array (complex my-float) 3) vol)
	   (values null &optional))
  (write-pgm fn (normalize-2-csf/ub8-realpart (cross-section-xz vol y))))

;; The merit function should get two parameters r and phi.  if r isn't
;; inside the back focal plane radius (minus the diameter of the
;; aperture window) some high value is returned. Several rays should
;; be sent through the spheres starting from different positions on
;; the aperture window and targetting different positions in the
;; circle that should be illuminated in the sample.

;; Maybe later I can add the aperture size in the back focal plane as
;; another parameter. The bigger the aperture, the better for the
;; optimization.

;; Possibly I shouldn't call it merit function as I try to minimize
;; its result.


;; In *spheres-c-r* I stored the coordinates of all the nuclei
;; relative to the center of the initial stack of images. It also
;; contains the radius of each nuclieus. Now I consider how to
;; illuminate selected circles inside of the sample. The nucleus which
;; is beeing illuminated will be centered on the focal plane.  The
;; length of the vector ro coming out of the objective is
;; nf=1.515*2.6mm~3000um and therefore a lot bigger than the z extent
;; of the stack (~40 um). It is not necessary to z-shift the nuclei
;; before intersecting them with the rays. So I will just use the
;; nucleus' x and y coordinates as arguments to
;; get-ray-behind-objective. I also supply the position in the back
;; focal plane from where the ray originates.

(deftype direction ()
  `(member :left :right :top :bottom))

(defun sample-circle (center radius direction)
  "Given a circle CENTER and RADIUS return the point in the left,
right, top or bottom of its periphery. CENTER and result are complex
numbers x+i y."
  (declare ((complex double-float) center)
	   (double-float radius)
	   (direction direction)
	   (values (complex double-float) &optional))
  (let ((phi (ecase direction
	       (:right 0d0)
	       (:top (* .5d0 pi))
	       (:left pi)
	       (:bottom (* 1.5d0 pi)))))
   (+ center (* radius (exp (complex 0d0 phi))))))

#+nil
(sample-unit-circle (complex 1d0 1d0) :right)

(defmethod illuminate-ray ((objective lens::objective) spheres-c-r 
			   illuminated-sphere-index sample-position 
			   bfp-ratio-x bfp-ratio-y window-radius-ratio 
			   bfp-position)
  "Trace a ray from a point in the back focal plane through the disk
that encompasses the nucleus with index
ILLUMINATED-SPHERE-INDEX. SAMPLE-POSITION and BFP-POSITION can assume
one of the four values :LEFT, :RIGHT, :TOP and :BOTTOM indicating
which point on the periphery of the correspondi2ng circle is meant
Coordinates in the back focal plane are ratios, e.g. bfp-ratio-x=-1
and 1 are on the border and a window with window-radius-ratio=1 passes
all light through the bfp. When the ray gets lost in the
objective (shouldn't happen if you stay inside the bfp) a big value is
returned. "
  (declare (fixnum illuminated-sphere-index)
	   (direction sample-position bfp-position)
	   (double-float bfp-ratio-x bfp-ratio-y window-radius-ratio)
	   ((simple-array sphere 1) spheres-c-r)
	   (values double-float &optional))
  (with-slots ((center raytrace::center)
	       (radius raytrace::radius))
      (aref spheres-c-r illuminated-sphere-index)
    (with-slots ((bfp-radius lens::bfp-radius)
		 (ri lens::immersion-index)
		 (f lens::focal-length)) objective
      (handler-case
	  (let* ((sample-pos (sample-circle
			      (complex (vec-x center) (vec-y center))
			      radius sample-position))
		 (bfp-pos (sample-circle (complex bfp-ratio-x bfp-ratio-y)
					 window-radius-ratio	 
					 bfp-position))
		 (ray1 (lens:get-ray-behind-objective 
			objective
			(realpart sample-pos) (imagpart sample-pos)
			(realpart bfp-pos)    (imagpart bfp-pos)))
		 (ray2 (make-instance 
			'ray
			:start
			(v- (vector::start ray1)
			    (make-vec 0d0
				      0d0
				      (- (* ri f) (vec-z center))))
			:direction (normalize (vector::direction ray1))))
		 (exposure (raytrace:ray-spheres-intersection 
			    ray2 spheres-c-r
			    illuminated-sphere-index)))
	    exposure)
	(ray-lost () 0d0)))))

#+nil ;; store the scan for each nucleus in the bfp
(time
 (let* ((n 10)
	(nn (length *spheres-c-r*))
	(mosaicx (ceiling (sqrt nn)))
	(mosaic (make-array (list (* n mosaicx) (* n mosaicx))
			    :element-type 'double-float))
	(obj (lens:make-objective :center (v) :normal (v 0 0 1)))
	(window-radius .05d0))
   (dotimes (nuc nn)
     (let* ((params (list obj
			  window-radius
			  nuc
			  *spheres-c-r*))
	    (px (* n (mod nuc mosaicx)))
	    (py (* n (floor nuc mosaicx))))
       (do-region ((j i) (n n))
	 (let ((x (- (* 2d0 (/ i n)) 1d0))
	       (y (- (* 2d0 (/ j n)) 1d0)))
	   (setf (aref mosaic (+ py j) (+ px i))
		 (merit-function (make-vec2 :x x :y y)
				 params))))))
   (write-pgm "/home/martin/tmp/scan-mosaic.pgm" (normalize-2-df/ub8 mosaic))))

