(in-package :frontend)

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
	       (let* ((xx (+ (* sx (* 4 (- (* i (/ +one+ x)) .5))) cx))
		      (yy (+ (* sx (* 4 (- (* j (/ +one+ y)) .5))) cy))
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

#+nil
(write-pgm "/home/martin/tmp/interpolation-test.pgm"
	   (normalize-2-sf/ub8 (.- (resample-2-sf (draw-disk-sf 25.0 75 75) 1s0 1s0 .25 .25)
				   (draw-disk-sf 100.0 300 300))))
#+nil
(time (progn
	(angular-psf :x 128 :z 128 :integrand-evaluations 280 :debug t :initialize t)
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
back focal plane set BIG-WINDOW to true. Multiple values are returned:
the volume, dx in um and dz in um."
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
	#+nil (debug-out dx dz kxextent R rr kzextent k rho)
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
      :integrand-evaluations 180
      :initialize t
      :debug t)
   (write-pgm "/home/martin/tmp/cut-5resampled.pgm"
	      (normalize-2-csf/ub8-realpart
	       (cross-section-xz
		(resample-3-csf a dx dx dz .2 .2 .2))))
   (sb-ext:gc :full t)))
