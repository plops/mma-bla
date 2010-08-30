(in-package :frontend)

(defmethod get-visible-nuclei ((model sphere-model-angular) k)
  "Find all the nuclei in slice K."
  (declare (fixnum k)
	   (values list &optional))
  (with-slots (dimensions centers index-spheres) model
   (destructuring-bind (z y x) dimensions
     (unless (< k z)
       (error "slice k isn't contained in array *index-spheres*."))
     ;; use bit-vector to store which nuclei are contained
     (let* ((n (length centers))
	    (result (make-array n
				:element-type 'boolean
				:initial-element nil)))
       (do-region ((j i) (y x))
	 (let ((v (round (realpart (aref index-spheres k j i)))))
	   (when (< 0 v n)
	     (setf (aref result v) t))))
       (loop for i from 1 below n
	  when (aref result i)
	  collect
	  (1- i))))))
#+nil
(let ((m (make-test-model)))
  (with-slots (dimensions) m
    (loop for i below (first dimensions) do
	 (format t "~a~%" (get-visible-nuclei m i)))))

;; create a volume containing just the current slice
(defmethod get-lcos-volume ((model sphere-model-angular) k nucleus)
  (declare (fixnum k)
	   (values (simple-array (complex my-float) 3) &optional))
  (with-slots (dimensions spheres index-spheres) model
   (destructuring-bind (z y x) dimensions
     (unless (< 0 k z)
       (error "slice index k out of range."))
     (let ((vol (make-array (list z y x)
			    :element-type '(complex my-float))))
       ;; only the current nucleus will be illuminated
       ;; note that nucleus 0 has value 1 in index-spheres
       (do-region ((j i) (y x))
	 (if (< (abs (- nucleus (1- (aref index-spheres k j i)))) .5)
	     (setf (aref vol k j i) (aref spheres k j i))))
       vol))))

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
  `(member :left :right :top :bottom :center))

(defun sample-circle (center radius direction)
  "Given a circle CENTER and RADIUS return the point in the left,
right, top or bottom of its periphery. CENTER and result are complex
numbers x+i y."
  (declare ((complex double-float) center)
	   (double-float radius)
	   (direction direction)
	   (values (complex double-float) &optional))
  (let ((phi (case direction
	       (:right 0d0)
	       (:top (* .5d0 pi))
	       (:left pi)
	       (:bottom (* 1.5d0 pi))
	       (t 0d0))))
   (case direction
     (:center center)
     (t (+ center (* radius (exp (complex 0d0 phi))))))))

#+nil
(sample-unit-circle (complex 1d0 1d0) :right)

(defmethod make-ray ((objective lens::objective) (model sphere-model)
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
all light through the bfp. The return values are the ray exiting the
principal sphere and the ray entering the bfp."
  (declare (fixnum illuminated-sphere-index)
	   (direction sample-position bfp-position)
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

#+nil ;; store the scan for each nucleus in the bfp
(time
 (let* ((n 30)
	(nn (length (centers *model*)))
	(mosaicx (ceiling (sqrt nn)))
	(mosaic (make-array (list (* n mosaicx) (* n mosaicx))
			    :element-type 'double-float))
	(obj (lens:make-objective :center (v) :normal (v 0 0 1)))
	(window-radius .1d0)
	(positions (sample-circles 3 3 1)))
   (dotimes (nuc 1 nn)
     (let* ((params (list obj
			  *model*
			  nuc
			  window-radius
			  positions))
	    (px (* n (mod nuc mosaicx)))
	    (py (* n (floor nuc mosaicx))))
       (do-region ((j i) (n n))
	 (let ((x (- (* 2d0 (/ i n)) 1d0))
	       (y (- (* 2d0 (/ j n)) 1d0)))
	   (setf (aref mosaic (+ py j) (+ px i))
		 (merit-function (make-vec2 :x x :y y)
				 params))))))
   (write-pgm "/home/martin/tmp/scan-mosaic.pgm" (normalize-2-df/ub8 mosaic))))

