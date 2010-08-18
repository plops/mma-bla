#+nil
(progn
  (require :vol)
  (require :lens)
  (require :raytrace)
  (require :bresenham)
  (require :psf))
 
(defpackage :run
  (:use :cl :vector :vol :raytrace :bresenham))
(in-package :run)

(deftype my-float-helper ()
  `single-float)

(deftype my-float (&optional (low '*) (high '*))
  `(single-float ,(if (eq low '*)
		      '*
		      (coerce low 'my-float-helper)) 
		 ,(if (eq high '*)
		      '*
		      (coerce high 'my-float-helper))))

(defconstant +one+ #.(coerce 1 'my-float))


;; run the following code to test the downhill simplex optimizer on a
;; 2d function:


#+nil
(time (let ((start (make-array 2 :element-type 'my-float
			       :initial-contents (list 1.5d0 1.5d0))))
	(simplex-anneal:anneal (simplex-anneal:make-simplex start 1d0)
		#'rosenbrock
		:ftol 1d-5)))


;; +-----	 |       |
;; |     \--     |       |
;; |        \-   |       |
;; |          \- |       |
;; |            \|       |
;; |            ||       |
;; |             |       |
;; |-------------+-------+------- <- z
;; -nf          /0       f
;; object      lens     bfp

(defun draw-ray-into-vol (x-mm y-mm bfp-ratio-x bfp-ratio-y vol
			  &key (dx-mm .2d-3) (dz-mm 1d-3)
			  (shift-z 0d0))
  (destructuring-bind (z y x)
      (array-dimensions vol)
   (let* ((f (lens:focal-length-from-magnification 63d0))
	  (na 1.38d0)
	  (ri 1.515d0)
	  (bfp-radius (lens:back-focal-plane-radius f na))
	  (obj (lens:make-thin-objective :normal (v 0d0 0d0 -1d0)
					 :center (v)
					 :focal-length f
					 :radius bfp-radius
					 :numerical-aperture na
					 :immersion-index ri))
	  (theta (lens:find-inverse-ray-angle x-mm y-mm obj))
	  (phi (atan y-mm x-mm))
	  (start (v (* bfp-ratio-x bfp-radius)
		    (* bfp-ratio-y bfp-radius)
		    f))
	  (dx dx-mm)
	  (dz dz-mm)
	  (cz (* .5d0 z)) ;; position that is in the center of front focal plane
	  (cy (* .5d0 y))
	  (cx (* .5d0 x))
	  (nf (* ri f)))
     (macrolet ((plane (direction position)
		  ;; for defining a plane that is perpendicular to an
		  ;; axis and crosses it at POSITION
		  (declare (type (member :x :y :z) direction))
		  (let* ((normal (ecase direction
				   (:x (v 1d0))
				   (:y (v 0d0 1d0))
				   (:z (v 0d0 0d0 1d0)))))
		    `(let* ((pos ,position)
			    (center (v* ,normal pos))
			    (outer-normal (normalize center)))
		       (declare (type my-float pos))
		       (lens::make-disk :normal outer-normal :center center)))))
       ;; define the borders of the viewing volume, distances in mm
       (let ((p+z (plane :z (- (* dz (- z cz))
			       nf)))
	     (p-z (plane :z (- (* dz (- (- z cz)))
			       nf)))
	     (p+y (plane :y (* dx (- y cy))))
	     (p-y (plane :y (* dx (- (- y cy)))))
	     (p+x (plane :x (* dx (- x cx))))
	     (p-x (plane :x (* dx (- (- x cx))))))
	 (multiple-value-bind (ro s)
	     (lens:thin-objective-ray obj
				      start
				      (v* (v (* (cos phi) (sin theta))
							(* (sin phi) (sin theta))
							(cos theta))
						-1d0))
	   (setf s (v+ s (v 0d0 0d0 (* dz shift-z))))
	   (let* ((nro (normalize ro)))
	     (macrolet ((hit (plane)
			  ;; (declare (type lens::disk plane))
			  ;; find intersection between plane and the ray
			  `(multiple-value-bind (dir hit-point)
			       (lens::plane-ray ,plane
						;; shift start of vector a bit
						s
						nro)
			     (declare (ignore dir))
			     hit-point))
			(pixel (hit-expr)
			  ;; convert coordinates from mm into integer pixel positions
			  `(let ((h ,hit-expr))
			     (declare (type (or null vec) h))
			     (when h
			       (make-vec-i
				:z (floor (+ cz (/ (+ (aref h 2) nf) dz)))
				:y (floor (+ cy (/ (aref h 1) dx)))
				:x (floor (+ cx (/ (aref h 0) dx))))))))
	       (let* ((h+z (pixel (hit p+z)))
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
		 (format t "~a~%" (list 'choice choice))
		 (scan-convert-line3
		  (first choice)
		  (second choice)
		  vol))))))))))

#+nil
(let ((vol (make-array (list 128 128 128) :element-type '(unsigned-byte 8))))
  (loop for i in '(4.0d-3 -.2d-3) do
   (draw-ray-into-vol i 0d0 .99d0 .0d0 vol)
   #+nil(draw-ray-into-vol i 0d0 -.99d0 .0d0 vol)
   (draw-ray-into-vol i 0d0 0d0 .99d0 vol)
   (draw-ray-into-vol i 0d0 0d0 -.99d0 vol))

  (save-stack-ub8 "/home/martin/tmp/line"
		  vol))


#+nil
(let ((vol (make-array (list 128 128 128) :element-type '(unsigned-byte 8))))
 (draw-line3 (make-vec-i :x 108 :y 112 :z  103)
	    (make-vec-i :x 82 :y 102 :z 10)
	    vol))


;; 		 |
;;        -------+-------
;;     -/  h (3) |       \---   (2) q_R=NA/ri*q_max
;;    -----------+------------/------------
;;               | alpha  /---  \-
;;               |     /--     	  \
;;        	 | /---            \
;;      ---------+-----------------+-------
;;               | (0)             / (1) q_max=1/(2*pixel)
;;
;; The resolution of the image has to be big enough to fit the top
;; section of the k-sphere with radius |k|=2pi*q_max into the k space.
;; q_max (see (1)) is due to the nyquist theorem and corresponds to 1
;; over two sample widths. The radius of the backfocal plane
;; corresponds to q_R (see (2) ri is the refractive index,
;; e.g. 1.515). It is bigger for an objective with a high NA.

;; A transform with uneven number of samples doesn't have a bin for
;; the nyquist sampling (draw the unit circle and divide it into n
;; equal bins starting from e^(i*0). For uneven n there will be no bin
;; on e^-i (i.e. -1, 1, -1 ...), e.g. n=3).  For even n there will be
;; n/2+1 bins ontop of the real axis (e.g. 0=1, 1=e^(-i pi/2), 2=e^-i
;; for n=4, the arguments to the exponential are (i 2 pi j/n) for the
;; j-th bin) and n/2-1 bins below (e.g 3=e^(i pi/2)).  In order to
;; simplify fftshift3 I only consider transforms with even n.
;; fftshift moves the n/2+1 bins from the front of the array to the
;; back (for n=4: [0 1 2 3] -> [3 0 1 2]).  In the shifted array the
;; highest reverse frequency (bin 3) is mapped to index 0.  The origin
;; of k-space (see (0) in the sketch) is therefor mapped to bin n/2-1
;; (bin 1 for n=4). The nyquist frequency is in the last bin n-1 (bin
;; 3 for n=4).

;; We now search for the right z-sampling dz to fit the top of the
;; sphere below the nyquist bin (which corresponds to q_max=1/(2*dz)).
;; |k|=2 pi/lambda = 2 pi q_max, with wavelength lambda
;; lambda=2 dz -> dz = lambda/2.

;; We could use the same sampling x and y to represent the electric
;; field. For small numerical apertures the sampling distance can be
;; increased. This time the radius q_R has to be smaller than the nyquist
;; frequency:
;; 1/(2*dx)=q_R=NA/ri * 1/lambda
;; -> dx= lambda/2 * ri/NA= dz *ri/NA=dz*1.515/1.38

;; The sampling distances dz and dx that I derived above are only good
;; to represent the amplitude psf. When the intensity is to be
;; calculated the sampling distance has to be changed to accomodate
;; for the convolution in k space.

;; The height of the sphere cap (h see (3) in sketch) is
;; h=q_max-q_max*cos(alpha)=q_max ( 1-cos(alpha))
;; =q_max*(1-sqrt(1-sin(alpha)^2))=q_max*(1-sqrt(1-(NA/ri)^2)) The z
;; sample distance dz2 for the intensity psf should correspond to 1/(2
;; dz2)=2 h, i.e. dz2=1/h=dz*2/(1-sqrt(1-(NA/ri)^2))>dz so the necessary z
;; sampling distance for the intensity is in general bigger than for
;; the amplitude.

;; The radius of the convolved donut shape is 2 q_R. Therefor the
;; transversal sampling distance for the intensity has to be smaller:
;; dx2=dx/2.

;; As we are only interested in the intensity psf we can sample the
;; amplitude psf with a sampling distance dz2. The sphere cap is
;; possibly wrapped along the k_z direction. The transversal direction
;; of the amplitude psf has to be oversampled with dx2.

;; To get an angular illumination psf we multiply the values on the
;; sphere with a k_z plane containing a disk that is centered at any
;; k_x and k_y inside the back focal plane.  Later I might want to
;; replace this with a gaussian or a more elaborate window function.

;; With a sampling dx2 the radius of the backfocal plane fills half of
;; the k space. The coordinate calculations below are corrected for
;; this. So setting cx to 1. and cy to 0. below would center the
;; circle on the border of the bfp.

;; For n=1.515 and NA=1.38 the ratio dz2/dx2 is ca. 6.  Angular
;; blocking allows to increase dz2 and dx2 a bit. Depending on which
;; and how big an area of the BFP is transmitted. Calculating these
;; smaller bounds seems to be quite complicated and I don't think it
;; will speed things up considerably. Also it will be possible to
;; calculate many different angular illuminations from an amplitude
;; otf that has been sampled with dx2 and dz2 without reevalutation of
;; Wolfs integrals.

;; I want to be able to set dz3 and dx3 to the same values that
;; Jean-Yves used for the confocal stack. I have to introduce
;; sx=dx2/dx3 to scale cx and cy into the back focal plane.


(let ((k0 nil)
      (k1 nil)
      (k2 nil))
 (defun angular-psf (&key (x 64) (y x) (z 40)
		     (window-radius #.(coerce .2 'my-float))
		     (window-x (- #.(coerce 1 'my-float) window-radius))
		     (window-y #.(coerce 0 'my-float))
		     (pixel-size-x nil)
		     (pixel-size-z nil)
		     (wavelength #.(coerce .480 'my-float))
		     (numerical-aperture #.(coerce 1.38 'my-float))
		     (immersion-index #.(coerce 1.515 'my-float))
		     (integrand-evaluations 30)
		     (debug nil)
		     (initialize nil))
   (declare (fixnum x y z integrand-evaluations)
	    (my-float window-radius window-x window-y
			  wavelength numerical-aperture
			  immersion-index)
	    ((or null my-float) pixel-size-x pixel-size-z)
	    (boolean debug initialize)
	    (values (simple-array (complex my-float) 3) &optional))
   ;; changing z,y,x without touching dx or dz leaves the area that is
   ;; shown in k space constant
   (let* ((na numerical-aperture)
	  (ri immersion-index)
	  (lambd (/ wavelength ri))
	  (dz (* +one+ .5 lambd))
	  (dz2 (* dz (/ 2 (- 1 (sqrt (- 1
					(let ((sinphi (/ na ri)))
					  (* sinphi sinphi))))))))
	  (dx (* dz (/ ri na)))
	  (dx2 (* dx .5))
	  (dx3 (if pixel-size-x
		   (progn
		     #+nil (unless (< pixel-size-x dx2)
			     (error "pixel-size-x is ~a but should be smaller than ~a"
				    pixel-size-x dx2))
		     pixel-size-x)
		   dx2))
	  (dz3 (if pixel-size-z
		   (progn
		     #+nil(unless (< pixel-size-z dz2)
			    (error "pixel-size-z is ~a but should be smaller than ~a"
				   pixel-size-z dz2))
		     pixel-size-z)
		   dz2)))
     
     ;; electric field caps are only calculated upon first call FIXME: or
     ;; when parameters change (implemented via reinitialize argument)
     (when (or (null k0) (null k1) (null k2) initialize)
       (multiple-value-bind (e0 e1 e2)
	   (psf:electric-field-psf z y x (* dz3 z) (* dx3 x)
				   :numerical-aperture na
				   :immersion-index ri
				   :wavelength wavelength
				   :integrand-evaluations integrand-evaluations)
	 (when debug
	   (write-pgm "/home/martin/tmp/cut-0psf.pgm"
		      (normalize-2-csf/ub8-abs (cross-section-xz e0))))
	 (setf k0 (fftshift (ft e0))
	       k1 (fftshift (ft e1))
	       k2 (fftshift (ft e2)))
	 (when debug (write-pgm "/home/martin/tmp/cut-1psf-k.pgm"
				(normalize-2-csf/ub8-abs (cross-section-xz k0))))))
     (let* ((cr window-radius)
	    (cx window-x)
	    (cy window-y)
	    (sx (/ dx2 dx3))
	    (cr2 (* cr cr))
	    (window (make-array (list y x) :element-type 'my-float))
	    (kk0 (make-array (array-dimensions k0)
			     :element-type '(complex my-float)))
	    (kk1 (make-array (array-dimensions k1)
			     :element-type '(complex my-float)))
	    (kk2 (make-array (array-dimensions k2)
			     :element-type '(complex my-float))))
	     ;; 2d window
	     (do-region ((j i) (y x))
	       (let* ((xx (- (* sx (* 4 (- (* i (/ +one+ x)) .5))) cx))
		      (yy (- (* sx (* 4 (- (* j (/ +one+ y)) .5))) cy))
		      (r2 (+ (* xx xx) (* yy yy))))
		 (when (< r2 cr2)
		   (setf (aref window j i) +one+))))
	     (do-region ((k j i) (z y x))
	       (setf (aref kk0 k j i) (* (aref k0 k j i) (aref window j i))
		     (aref kk1 k j i) (* (aref k1 k j i) (aref window j i))
		     (aref kk2 k j i) (* (aref k2 k j i) (aref window j i))))
	     (when debug 
	       (write-pgm "/home/martin/tmp/cut-2psf-k-mul.pgm"
			  (normalize-2-csf/ub8-abs (cross-section-xz kk0))))
	     (let* ((e0 (ift (fftshift kk0)))
		    (e1 (ift (fftshift kk1)))
		    (e2 (ift (fftshift kk2)))
		    (intens k0)) ;; instead of allocating a new array
				 ;; we store into k0
	       (do-region ((k j i) (z y x))
		 (setf (aref intens k j i)
		       (+ (* (aref e0 k j i) (conjugate (aref e0 k j i)))
			  (* (aref e1 k j i) (conjugate (aref e1 k j i)))
			  (* (aref e2 k j i) (conjugate (aref e2 k j i))))))
	       (when debug
		 (write-pgm 
		  "/home/martin/tmp/cut-3psf-intens.pgm"
		  (normalize-2-csf/ub8-realpart (cross-section-xz intens)))
		 (let ((k (fftshift (ft intens))))
		   (write-pgm "/home/martin/tmp/cut-4psf-intk.pgm"
			      (normalize-2-csf/ub8-abs (cross-section-xz k)))))
	       intens)))))

(write-pgm "/home/martin/tmp/psf.pgm"
	   (normalize-2-sf/ub8 (.- (resample-2-sf (draw-disk-sf 25.0 75 75) 1s0 1s0 .25 .25)
				   (draw-disk-sf 100.0 300 300))))
(psf:intensity-psf)

#+nil
(time (progn
	(angular-psf :x 128 :z 64 :integrand-evaluations 280 :debug t)
	nil))

(defmacro debug-out (&rest rest)
  `(format t "~a~%"
	   (list ,@(mapcar #'(lambda (x) `(list ,(format nil "~a" x) ,x))
			   rest))))


#|| sketch of the cap kz(kx,ky) of the k-vectors that enter the back
focal plane.  the small circle with the x in the center is a
transparent window.  the distance from the center of the window to the
center of the cap is rho.  we are interested in the cap height at
points A and B along the vector rho. the point A will be the highest
point of the cap and B the lowest. when the window encloses the center
of the cap the highest point is k. when the window touches the
periphery of the bfp, the lowest point is kz(R), where R=NA*k0 is the
diameter of the bfp.

   	       	   -----+-----
 from top:     ---/     |     \---
   	     -/         |         \-
           -/           |           \-  /  kx
          /             |             \/
         /              |    -----   / \
        /      	        |   /     \     \
       /                |  /       \     \
       |                |  |   x   |     |
      /        	        |  |       |      \
      |                 |  |\     /|      |
      |        	        o  | ----- |   	  |
      |                 |  |       |      |
      \        	        |  |       |      |
       |                |  |       |     ||
       \                |  |       |     /|
        \      	        |  |       |    / |
         \              |  |       |   /  |
          \             |  |       |  /	  |
           -\           |  |       |/-	  |
	     -\         |  |      /+	  |
	       ---\     |  |  /--- |	  |
	       	   -----+--+--	   |	  |
       	          ------+--+---	   |	  |
 from side   ----/   kz |      \---v	  |
          --/           |           \--	  |
        -/              |              \- |
      -/                |                \/
    -/                  |               -/ \-
   /                    |              /     \ kz(kx)
  /                     |            -/       \
 /                      |           /          \
/              	        |      	  -/            \
/             	        | alph  -/               \
/      	                |      /                  \
|                       |    -/                   |
/                       |   /                  	   \
|              	        | -/           	     	   |
|                       |/             	     	   |
|-----------------------+--o-------o------o--------+ kx
|                       |  A       B   	  R 	   |
|              	        |              	     	  k=k0*n
\                       |  |-------|      	   /
|                       |     2 rr                 |
||#

(defun angular-intensity-psf-minimal-resolution 
    (&key
     (x-um #.(coerce 50 'my-float)) (y-um x-um) (z-um #.(coerce 40 'my-float))
     (window-radius #.(coerce .1 'my-float))
     (window-x #.(coerce .5 'my-float))
     (window-y #.(coerce 0 'my-float))
     (wavelength #.(coerce .480 'my-float))
     (numerical-aperture #.(coerce 1.38 'my-float))
     (immersion-index #.(coerce 1.515 'my-float))
     (integrand-evaluations 30)
     (debug nil)
     (initialize t)
     (store-cap nil)
     (big-window nil))
  "Calculate angular intensity PSF, ensure that the maximum sampling
distance is chosen. Set INITIALIZE to nil if the e-field can be reused
from a previous calculation. In that case you may need to set
STORE-CAP to true for all evaluations and use a lot more memory. Only
if the window diameter is going to be bigger than the radius of the
back focal plane set BIG-WINDOW to true."
  (declare ((my-float 0 1) window-radius)
	   ((my-float -1 1) window-x window-y)
	   (my-float wavelength numerical-aperture immersion-index
			 x-um y-um z-um)
	   (fixnum integrand-evaluations)
	   (boolean debug initialize store-cap big-window)
	   (values (simple-array (complex my-float) 3)
		   my-float my-float &optional))
  (let* ((k0 (/ #.(coerce (* 2 pi) 'my-float) wavelength))
	 (k (* immersion-index k0))
	 (R (* numerical-aperture k0))
	 (rr (* R window-radius)))
    (labels ((kz (kx)
	       (sqrt (- (* k k) (* kx kx)))))
      (let* ((rho (sqrt (+ (* window-x window-x)
			   (* window-y window-y))))
	     (kza (kz (- rho rr)))
	     (kzb (kz (+ rho rr)))
	     (top (if (< rho rr)
		      (max k kza kzb)
		      (max kza kzb)))
	     (bot (if (< rho (- R r)) ;; window is in center of bfp
		      (min (kz R) kza kzb)
		      (min kza kzb)))
	     (kzextent (if store-cap
			   ;; store the full cap without wrapping,
			   ;; this way one can reuse the efields
			   (- k (kz R)) 
			   ;; just leave enough space to accommodate
			   ;; the final donut, this improves
			   ;; performance a lot for small windows
			   (* 16 (- top bot))))
	     (kxextent (if big-window
			   ;; for window diameter bigger than
			   ;; bfp-diam/2 transversally the bfp has to
			   ;; fit in twice, to accommodate the full
			   ;; donut
			   (* +one+ 2 R)
			   (* +one+ R))) 
	     (pif #.(coerce pi 'my-float))
	     (dx (/ pif kxextent))
	     (dz (/ pif kzextent)))
	(debug-out dx dz kxextent R rr kzextent k rho)
	(values
	 (angular-psf :x (floor x-um dx) :y (floor y-um dx) :z (floor z-um dz)
		      :window-radius window-radius
		      :window-x window-x :window-y window-y
		      :wavelength wavelength
		      :numerical-aperture numerical-aperture
		      :immersion-index immersion-index
		      :integrand-evaluations integrand-evaluations
		      :pixel-size-x dx
		      :pixel-size-z dz
		      :debug debug
		      :initialize initialize)
	 dx dz)))))

#+nil
(time
 (multiple-value-bind (a dx dz)
     (angular-intensity-psf-minimal-resolution 
      :x-um (* +one+ 20)
      :z-um (* +one+ 40)
      :window-radius (* +one+ .14)
      :window-x (* +one+ .73)
      :window-y (* +one+ 0) 
      :integrand-evaluations 30 
      :initialize t
      :debug t)
   (write-pgm "/home/martin/tmp/cut5-resampled.pgm"
	      (normalize-2-csf/ub8-realpart
	       (cross-section-xz 
		a #+nil (resample-3-csf a dx dx dz .2 .2 .2))))
   (sb-ext:gc :full t)))


(defmacro defstuff ()
  `(progn
     ,@(loop for i in '(*dims*    ;; dimensions of the input stack in
				  ;; pixels and slices
			*centers* ;; integral center coordinates of
				  ;; the nuclei (0 .. dim-x) ...
			*index-spheres* ;; each nuclei is drawn with its index
			*spheres-c-r* ;; scaled (isotropic axis, in
				     ;; micrometeres) and shifted (so
				     ;; that origin in center of
				     ;; volume) coordinates
			)
	  collect
	    `(defparameter ,i nil))))

(defstuff)

(defun create-sphere-array (dims centers)
  (declare (cons dims)
	   ((simple-array vec-i 1) centers)
	   (values (simple-array sphere 1) &optional))
 (destructuring-bind (z y x)
     dims
   (declare (fixnum z y x))
   (let* ((dx (* +one+ .2e-3))
	  (dz (* +one+  1e-3))
	  (xh (* +one+ .5d0 x))
	  (yh (* +one+ .5d0 y))
	  (zh (* +one+ .5d0 z))
	  (n (length centers))
	  (result-l nil))
     (labels ((convert-point (point)
		(declare (vec-i point)
			 (values vec &optional))
		(v (* dx (- (vec-i-x point) xh))
		   (* dx (- (vec-i-y point) yh))
		   (* dz (- (vec-i-z point) zh)))))
       (dotimes (i n)
	 (push (make-sphere :center (convert-point (aref centers i))
			    :radius (* dx 17d0))
	       result-l))
       (make-array (length result-l) :element-type 'sphere
		   :initial-contents result-l)))))

(defun init-angular-model ()
  ;; find the centers of the nuclei and store into *centers*
  (multiple-value-bind (c ch dims)
      (find-centers)
    (declare (ignore ch))
    (defparameter *centers* c)
    (defparameter *dims* dims)
    (sb-ext:gc :full t))

  (defparameter *spheres-c-r*
    (create-sphere-array *dims* *centers*))
  (let ((spheres
	 (destructuring-bind (z y x)
	     *dims*
	   (draw-indexed-ovals 12d0 *centers* z y x))))
    (setf *index-spheres* spheres)
    (write-pgm "/home/martin/tmp/angular-indexed-spheres-cut.pgm"
	       (normalize-2-cdf/ub8-realpart
		(cross-section-xz *index-spheres* 
				  (vec-i-y (elt *centers* 31)))))
    (sb-ext:gc :full t))
  (let ((spheres
	 (destructuring-bind (z y x)
	     *dims*
	   (draw-ovals 12d0 *centers* z y x))))
    (setf *spheres* spheres)
    (write-pgm "/home/martin/tmp/angular-spheres-cut.pgm"
	       (normalize-2-cdf/ub8-realpart
		(cross-section-xz *index-spheres* 
				  (vec-i-y (elt *centers* 31)))))
    (sb-ext:gc :full t)))

#+nil
(time (init-angular-model)) 

(defun init-angular-psf ()
  ;; calculate angular intensity psf, make extend in z big enough to
  ;; span the full fluorophore concentration even when looking at the
  ;; bottom plane of it

  ;; the size of the k space must be big enough: 2*bfp-diameter for
  ;; k_{x,y}, and 2*cap-height for k_z. then the full donut can be
  ;; accomodated.

  ;; if only a small part of the cap is selected the dimensions can be
  ;; reduced accordingly. calculating the size requires to find the
  ;; intersection of a cylinder and the sphere cap.
  (let* ((dx .2d0)
	 (dz 1d0)
	 (psf (destructuring-bind (z y x)
		  *dims*
		(declare (ignore y x))
		(let ((r 100))
		  (angular-psf
		   :window-radius .4d0
		   :window-x .6d0
		   :z (* 8 z) :x (* 2 r) :y (* 2 r)
		   :pixel-size-z (* .25d0 dz) :pixel-size-x (* .5d0 dx)
		   :integrand-evaluations 400
		   :debug t
		   :initialize t)))))
    (defparameter *psf* psf)
    (write-pgm "/home/martin/tmp/psf.pgm"
	       (normalize-2-cdf/ub8-realpart (cross-section-xz psf)))
    (sb-ext:gc :full t)))

#+nil
(time (init-angular-psf)) ;; 62.2 s

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
	(if (< (abs (- nucleus (1- (aref *index-spheres* k j i)))) .5d0)
	    (setf (aref vol k j i) (aref *spheres* k j i))))
      vol)))

#+ni
(let* ((k 25)
       (nuc (first (get-visible-nuclei k)))
       (vol (get-lcos-volume k nuc)))
  (format t "~a~%" `(nuc ,nuc))
  (write-section "/home/martin/tmp/angular-0lcos-cut.pgm" vol)
  (save-stack-ub8 "/home/martin/tmp/angular-0lcos" (normalize3-cdf/ub8-realpart vol)))


(defun write-section (fn vol &optional (y (floor (array-dimension vol 1) 2)))
  (declare (simple-string fn)
	   ((simple-array (complex my-float) 3) vol)
	   (values null &optional))
  (write-pgm fn (normalize-2-cdf/ub8-realpart (cross-section-xz vol y))))

(defvar *nucleus-index* 50)
(defvar *bfp-window-radius* .08d0)

;; the following global variable contain state for merit-function:
;; *bfp-window-radius* *nucleus-index* *spheres-c-r*
(defun merit-function (vec2)
  (declare ((simple-array my-float (2)) vec2)
	   (values my-float &optional))
  (let* ((border-value 100d0) ;; value to return when outside of bfp
	 ;; this has to be considerably bigger than the maxima on the bfp
	 (border-width *bfp-window-radius*) ;; in this range to the
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
		       (incf sum
			     (illuminate-ray *spheres-c-r* *nucleus-index* dir
					     (vec2-x vec2) (vec2-y vec2)
					     *bfp-window-radius*
					     bfp-dir)))))
	;; in the border-width or outside of bfp
	(incf sum border-value))
    sum))

(defun find-optimal-bfp-window-center (nucleus)
  (declare (fixnum nucleus)
	   (values vec2 &optional))
  (let ((*nucleus-index* nucleus))
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
				  :ftol 1d-3)
	 (when (< min 100d0)
	   (return-from find-optimal-bfp-window-center point))))))

#+nil
(find-optimal-bfp-window-center 50)
;; FIXME: are these coordinates in mm or relative positions for a bfp-radius of 1?
;; I think the latter, but I'm not sure.
#+nil
(time
 (let*((dx .2d0)
       (dz 1d0)
       (r 100)
       (z (array-dimension *spheres* 0))
       (psf (angular-psf :x r :y r :z (* 2 z) 
		:pixel-size-x dx :pixel-size-z dz
		:window-radius *bfp-window-radius*
		:window-x -.714d0
		:window-y .16d0
		:initialize t
		:debug t
		:integrand-evaluations 400))) 
   (save-stack-ub8 "/home/martin/tmp/psf" (normalize3-cdf/ub8-realpart psf))
   nil))


;; calculate the excitation one nucleus
(defun calc-light-field (k nucleus)
  (declare (fixnum k nucleus))
  (let* ((result nil)
	 (lcos (get-lcos-volume k nucleus))
	 (bfp-pos (find-optimal-bfp-window-center nucleus))
	 (psf (let ((dx .2d0)
		    (dz 1d0)
		    (r 100)
		    (z (array-dimension *spheres* 0))) 
		(multiple-value-bind (h ddx ddz)
		    (angular-intensity-psf-minimal-resolution
				 :x-um (* r dx) :y-um (* r dx) :z-um (* 2 z dz) 
				 :window-radius *bfp-window-radius*
				 :window-x (vec2-x bfp-pos)
				 :window-y (vec2-y bfp-pos)
				 :initialize t
				 :debug t
				 :integrand-evaluations 1000)
		  (resample-3-cdf h ddx ddx ddz dx dx dz)))))
    (format t "~a~%" `(bfp-pos ,bfp-pos))
    (write-section (format nil "/home/martin/tmp/angular-1expsf-cut-~3,'0d.pgm" nucleus) psf)

    (multiple-value-bind (conv conv-start)
	(convolve-nocrop lcos psf)
      ;; light distribution in sample
      (defparameter *angular-light-field* conv)
      (defparameter *angular-light-field-start* conv-start)
      (write-section (format nil "/home/martin/tmp/angular-2light-cut-~3,'0d.pgm" nucleus) 
		     conv
		     (vec-i-y (aref *centers* nucleus)))
      (save-stack-ub8 (format nil "/home/martin/tmp/angular-2light-~3,'0d/" nucleus)
		      (normalize-3-cdf/ub8-realpart conv))
      ;; multiply fluorophore concentration with light distribution
      (let ((excite (.* conv *spheres* conv-start)))
	(defparameter *excite* excite)
	(write-section (format nil "/home/martin/tmp/angular-3excite-cut-~3,'0d.pgm" nucleus)
		       excite
		       (vec-i-y (aref *centers* nucleus)))
	(save-stack-ub8 (format nil "/home/martin/tmp/angular-3excite-~3,'0d/" nucleus)
			(normalize-3-cdf/ub8-realpart excite))
	(destructuring-bind (z y x)
	    (array-dimensions excite)
	  (declare (ignorable z))
	  (let* ((in-focus (extract-bbox-3-cdf excite
					      (make-bbox :start (v 0d0 0d0 (* 1d0 k))
							 :end (v (* 1d0 (1- x))
								 (* 1d0 (1- y))
								 (* 1d0 k))))))
	    (save-stack-ub8 "/home/martin/tmp/angular-4in-focus/"
			    (normalize-3-cdf/ub8-realpart in-focus))
	    (let*((mplane (mean (convert-3-cdf/df-realpart in-focus)))
		  (mvol (mean (convert-3-cdf/df-realpart excite)))
		  (gamma (/ mplane mvol)))
	      (push (list mplane mvol gamma) result)
	      (debug-out mplane mvol gamma)
	      (format t "plane-result ~f ~f ~f~%" mplane mvol gamma))
	    result))))))

#+nil 
(time
 (progn
  (with-open-file (*standard-output* "/home/martin/tmp/angular-stack.log"
				     :direction :output
				     :if-exists :supersede
				     :if-does-not-exist :create)
    (let ((result nil))
      (dotimes (k (array-dimension *spheres* 0))
	(let* ((nucs (get-visible-nuclei k)))
	  (loop for nuc in nucs do
	       (format t "~a~%" (list 'doing k nucs))
	       (push (list k nuc (calc-light-field k nuc)) result))))
      (defparameter *scan-result* result)))
  (with-open-file (s "/home/martin/tmp/angular-stack-struc.lisp"
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (write *scan-result* :stream s))))

#+nil ;; overlay lightfield and spheres
(time
 (save-stack-ub8 "/home/martin/tmp/sphere-and-excite/"
		 (normalize3-cdf/ub8-realpart 
		  (labels ((con (vol)
			     (convert3-ub8/cdf-complex (normalize3-cdf/ub8-realpart vol))))
		    (.+ (con *angular-light-field*)
			(s* .7d0 (con *spheres*)) 
			*angular-light-field-start*)))))

#+nil
(dotimes (i (length *centers*))
  (format t "~a~%"
   (raytrace::ray-spheres-intersection (v) (v 0d0 0d0 -1d0) *sphere-c-r* i)))

#+nil
(progn
  (defun merit-function (vec)
    (declare (vec vec)
	     (values my-float &optional))
    (raytrace:ray-spheres-intersection
     (v 0d0 0d0 0d0)
     (normalize (direction (aref vec 0) (aref vec 1)))
     *sphere-c-r*))
  (let ((start (make-array 2 :element-type 'my-float
                          :initial-contents (list 100d0 100d0))))
   (with-open-file (*standard-output* "/dev/shm/anneal.log"
                                      :direction :output
                                      :if-exists :supersede)
     (anneal (make-simplex start 1d0)
             #'merit-function
             :start-temperature 3d0))))


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

(defun get-ray-behind-objective (x-mm y-mm bfp-ratio-x bfp-ratio-y)
  "Take a point on the back focal plane and a point in the sample and
 calculate the ray direction ro that leaves the objective. So its the
 same calculation that is used for draw-ray-into-vol."
  (declare (my-float x-mm y-mm bfp-ratio-x bfp-ratio-y)
	   (values vec vec &optional))
  (let* ((f (lens:focal-length-from-magnification 63d0))
	 (na 1.38d0)
	 (ri 1.515d0)
	 (bfp-radius (lens:back-focal-plane-radius f na))
	 (obj (lens:make-thin-objective :normal (v 0d0 0d0 -1d0)
					:center (v)
					:focal-length f
					:radius bfp-radius
					:numerical-aperture na
					:immersion-index ri))
	 (theta (lens:find-inverse-ray-angle x-mm y-mm obj))
	 (phi (atan y-mm x-mm))
	 (start (v (* bfp-ratio-x bfp-radius)
		   (* bfp-ratio-y bfp-radius)
		   f)))
    (multiple-value-bind (ro s)
	(lens:thin-objective-ray obj
				 start
				 (v* (v (* (cos phi) (sin theta))
					(* (sin phi) (sin theta))
					(cos theta))
				     -1d0))
      (values ro s))))

#+nil
(get-ray-behind-objective .1d0 .1d0 0d0 0d0)

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
  (declare ((complex my-float) center)
	   (my-float radius)
	   (direction direction)
	   (values (complex my-float) &optional))
  (let ((phi (ecase direction
	       (:right 0d0)
	       (:top (* .5d0 pi))
	       (:left pi)
	       (:bottom (* 1.5 pi)))))
   (+ center (* radius (exp (complex 0d0 phi))))))

#+nil
(sample-circle (complex 1d0 1d0) 1d0 :right)

(defun illuminate-ray (spheres-c-r illuminated-sphere-index
		       sample-position
		       bfp-center-x bfp-center-y
		       bfp-radius bfp-position)
  "Trace a ray from a point in the back focal plane through the disk
that encompasses the nucleus with index
ILLUMINATED-SPHERE-INDEX. SAMPLE-POSITION and BFP-POSITION can assume
one of the four values :LEFT, :RIGHT, :TOP and :BOTTOM indicating
which point on the periphery of the corresponding circle is meant."
  (declare (fixnum illuminated-sphere-index)
	   (direction sample-position bfp-position)
	   (my-float bfp-center-x bfp-center-y bfp-radius)
	   ((simple-array sphere 1) spheres-c-r)
	   (values my-float &optional))
  (with-slots (center radius)
      (aref spheres-c-r illuminated-sphere-index)
    (let* ((sample-pos (sample-circle (complex (vec-x center) (vec-y center))
				      radius
				      sample-position))
	   (bfp-pos (sample-circle (complex bfp-center-x bfp-center-y)
				   bfp-radius
				   bfp-position)))
      (multiple-value-bind (ro s)
	  (get-ray-behind-objective (realpart sample-pos)
				    (imagpart sample-pos)
				    (realpart bfp-pos)
				    (imagpart bfp-pos))
	(let* ((exposure
		(ray-spheres-intersection
		 ;; shift by nf so that sample is in origin
		 (v+ s
		     (v 0d0
			0d0
			(* 1.515 (lens:focal-length-from-magnification 63d0))))
		 (normalize ro)
		 spheres-c-r
		 illuminated-sphere-index)))
	  exposure)))))

#+nil
(illuminate-ray *spheres-c-r* 30 :bottom
		.1d0 .0d0
		.01d0 :right)

#+nil ;; store the scan for each nucleus in the bfp
(time
 (let* ((n 100)
	(a (make-array (list n n) :element-type 'my-float))
	(nn (length *spheres-c-r*))
	(mosaicx (ceiling (sqrt nn)))
	(mosaic (make-array (list (* n mosaicx) (* n mosaicx))
			    :element-type 'my-float)))
   (dotimes (*nucleus-index* nn)
     (dotimes (i n)
       (dotimes (j n)
	 (let ((x (- (* 2d0 (/ i n)) 1d0))
	       (y (- (* 2d0 (/ j n)) 1d0)))
	   (setf (aref a j i)
		 (merit-function
		  (make-vec2 :x x :y y))))))
     (do-rectangle (j i 0 n 0 n)
       (let ((x (mod *nucleus-index* mosaicx))
	     (y (floor *nucleus-index* mosaicx)))
	 (setf (aref mosaic (+ (* n y) j) (+ (* n x) i))
	       (aref a j i)))))
   (write-pgm "/home/martin/tmp/scan-mosaic.pgm" (normalize-ub8 mosaic))))


#+nil
(time
 (let* ((n 100)
	(a (make-array (list n n) :element-type '(unsigned-byte 8)))
	(nn (length *spheres-c-r*))
	(mosaicx (ceiling (sqrt nn)))
	(mosaic (make-array (list (* n mosaicx) (* n mosaicx))
			    :element-type '(unsigned-byte 8))))
   (with-open-file (*standard-output* "/dev/shm/a"
				      :direction :output
				      :if-exists :supersede)
     (dotimes (*nucleus-index* nn)
       (dotimes (i 10)
	 (tagbody again
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
				       :ftol 1d-3)
	      (unless (<= min 100d0)
		(go again))
	      (let* ((x (aref point 0))
		     (y (aref point 1))
		     (ix (floor (* n (+ x 1)) 2))
		     (iy (floor (* n (+ y 1)) 2))
		     (mx (mod *nucleus-index* mosaicx))
		     (my (floor *nucleus-index* mosaicx)))
		(incf (aref mosaic (+ (* n my) iy) (+ (* n mx) ix)))
		(format t "min ~a~%" (list min ix iy))))))))
   (write-pgm "/home/martin/tmp/scan-mosaic-max.pgm" mosaic)))



