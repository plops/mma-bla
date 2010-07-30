
(declaim (ftype (function (double-float)
			  (values double-float &optional))
		sq))
(defun sq (x)
  (* x x))


(declaim (ftype (function ((array double-float *))
			  (values double-float &optional))
		rosenbrock))
(defun rosenbrock (p)
  (let* ((x (aref p 0))
	 (y (aref p 1))
	 (result (+ (sq (- 1 x))
		    (* 100 (sq (- y (sq x)))))))
    (format t "~a~%" (list 'rosenbrock p result))
    result))
#+nil
(rosenbrock (make-array 2 :element-type 'double-float
			 :initial-contents (list 1.5d0 1.5d0)))
;; run the following code to test the downhill simplex optimizer on a
;; 2d function:

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

#+nil
(time (let ((start (make-array 2 :element-type 'double-float
			       :initial-contents (list 1.5d0 1.5d0))))
	(simplex-anneal:anneal (simplex-anneal:make-simplex start 1d0)
		#'rosenbrock
		:ftol 1d-5)))

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
		       (declare (type double-float pos))
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
		      (filtered-hlist (remove-if-not #'(lambda (v)
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
   (draw-ray-into-vol i 0d0 -.99d0 .0d0 vol)
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
;;               | alpha      /---  \-
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

(defun angular-psf (&key (x 64) (y x) (z 40)
		    (window-radius .2d0)
		    (window-x (- 1d0 window-radius))
		    (window-y 0d0)
		    (pixel-size-x nil)
		    (pixel-size-z nil)
		    (wavelength .480d0)
		    (numerical-aperture 1.38d0)
		    (immersion-index 1.515d0)
		    (integrand-evaluations 30)
		    (debug nil))
  (declare (fixnum x y z integrand-evaluations)
	   (double-float window-radius window-x window-y
			 wavelength numerical-aperture
			 immersion-index)
	   ((or null double-float) pixel-size-x pixel-size-z)
	   (boolean debug)
	   (values (simple-array (complex double-float) 3) &optional))
 ;; changing z,y,x without touching dx or dz leaves the area that is
 ;; shown in k space constant
  (let* ((na numerical-aperture)
	 (ri immersion-index)
	 (lambd (/ wavelength ri))
	 (dz (* .5d0 lambd))
	 (dz2 (* dz (/ 2d0 (- 1d0 (sqrt (- 1d0
					   (let ((sinphi (/ na ri)))
					     (* sinphi sinphi))))))))
	 (dx (* dz (/ ri na)))
	 (dx2 (* .5 dx))
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
    (multiple-value-bind (e0 e1 e2)
	(psf:electric-field-psf z y x (* z dz3) (* x dx3)
				:numerical-aperture na
				:immersion-index ri
				:wavelength wavelength
				:integrand-evaluations integrand-evaluations)
      (when debug 
	(write-pgm (normalize-ub8 (cross-section-xz e0))
		   "/home/martin/tmp/cut-0psf.pgm"))
     (let ((k0 (fftshift3 (ft3 e0)))
	   (k1 (fftshift3 (ft3 e1)))
	   (k2 (fftshift3 (ft3 e2))))
       (when debug (write-pgm (normalize-ub8 (cross-section-xz k0))
			      "/home/martin/tmp/cut-1psf-k.pgm"))
       (let* ((cr window-radius)
	      (cx window-x)
	      (cy window-y)
	      (sx (/ dx2 dx3))
	      (cr2 (* cr cr))
	      (window (make-array (list y x)
				  :element-type 'double-float)))
	 ;; 2d window 
	 (do-rectangle (j i 0 y 0 x)
	   (let* ((xx (- (* sx (* 4d0 (- (* i (/ 1d0 x)) .5d0))) cx))
		  (yy (- (* sx (* 4d0 (- (* j (/ 1d0 y)) .5d0))) cy))
		  (r2 (+ (* xx xx) (* yy yy))))
	     (when (< r2 cr2)
	       (setf (aref window j i) 1d0))))
	 (do-box (k j i 0 z 0 y 0 x)
	   (setf (aref k0 k j i) (* (aref k0 k j i) (aref window j i))
		 (aref k1 k j i) (* (aref k1 k j i) (aref window j i))
		 (aref k2 k j i) (* (aref k2 k j i) (aref window j i))))
	 (when debug (write-pgm (normalize-ub8 (cross-section-xz k0))
		     "/home/martin/tmp/cut-2psf-k-mul.pgm"))
	 (let* ((e0 (ift3 (fftshift3 k0)))
		(e1 (ift3 (fftshift3 k1)))
		(e2 (ift3 (fftshift3 k2)))
		(intens k0)) ;; instead of allocating a new array we store into k0
	   (do-box (k j i 0 z 0 y 0 x)
	     (setf (aref intens k j i)
		   (+ (* (aref e0 k j i) (conjugate (aref e0 k j i)))
		      (* (aref e1 k j i) (conjugate (aref e1 k j i)))
		      (* (aref e2 k j i) (conjugate (aref e2 k j i))))))
	   (when debug
	     (write-pgm (normalize-ub8 (cross-section-xz intens))
			"/home/martin/tmp/cut-3psf-intens.pgm")
	     (let ((k (fftshift3 (ft3 intens))))
	       (write-pgm (normalize-ub8 (cross-section-xz k))
			  "/home/martin/tmp/cut-4psf-intk.pgm")))
	   intens))))))

#+nil
(time (progn
	(angular-psf :x 128 :z 64 :integrand-evaluations 120 :debug t)
	nil))

#+nil ;; convert coordinates of spheres from integers into doubles,
      ;; corresponding to mm.
(let* ((dx .2d-3)
       (dz 1d-3)
       (n (length *centers*))
       (sph (make-array n :element-type 'raytrace::sphere)))
  (dotimes (i n)
    (setf (aref sph i)
	  (make-sphere :center (let ((a (elt *centers* i)))
							    (v (* dx (vec-i-x a))
							       (* dx (vec-i-y a))
							       (* dz (vec-i-z a))))
		       :radius (* dx 7d0))))
  (defparameter *sphere-c-r* sph))

#+nil
(dotimes (i (length *centers*))
  (format t "~a~%"
   (raytrace::ray-spheres-intersection (v) (v 0d0 0d0 -1d0) *sphere-c-r* i)))

#+nil
(declaim (ftype (function (vec)
			  (values double-float &optional))
		merit-function))
#+nil
(defun merit-function (vec)
  (raytrace:ray-spheres-intersection 
   (v 0d0 0d0 0d0) 
   (normalize (direction (aref vec 0) (aref vec 1)))
   *sphere-c-r*))


#+nil
(let ((start (make-array 2 :element-type 'double-float
                          :initial-contents (list 100d0 100d0))))
   (with-open-file (*standard-output* "/dev/shm/anneal.log"
                                      :direction :output
                                      :if-exists :supersede)
     (anneal (make-simplex start 1d0)
             #'merit-function
             :start-temperature 3d0)))


(defun create-sphere-array (dims centers)
  (declare (cons dims)
	   ((simple-array vec-i 1) centers)
	   (values (simple-array sphere 1) &optional))
 (destructuring-bind (z y x)
     dims
   (declare (fixnum z y x))
   (let* ((dx .2d-3)
	  (dz 1d-3)
	  (xh (* .5d0 x))
	  (yh (* .5d0 y))
	  (zh (* .5d0 z))
	  (n (length centers))
	  (result (make-array n :element-type 'sphere
			      :initial-contents (loop for i below n collect
						     (make-sphere)))))
     (labels ((convert-point (point)
		(declare (vec-i point)
			 (values vec &optional))
		(v (* dx (- (vec-i-x point) xh))
		   (* dx (- (vec-i-y point) yh))
		   (* dz (- (vec-i-z point) zh)))))
       (dotimes (i n)
	 (setf (aref result i) 
	       (make-sphere :center (convert-point (aref centers i))
			    :radius (* dx 17d0))))
       result))))

#+nil 
(progn
 (defparameter *central-sphere* 22)
 (defparameter *spheres-c-r* 
   (create-sphere-array *dims* *centers*)))



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

;; get-ray-behind-objective takes a point on the back focal plane and
;; a point in the sample and calculates the ray direction ro that
;; leaves the objective. So its the same calculation that is used for
;; draw-ray-into-vol.
(declaim (ftype (function (double-float double-float double-float double-float)
			  (values vec vec &optional))
		get-ray-behind-objective))
(defun get-ray-behind-objective (x-mm y-mm bfp-ratio-x bfp-ratio-y)
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
  (declare ((complex double-float) center)
	   (double-float radius)
	   (direction direction)
	   (values (complex double-float) &optional))
  (let ((phi (ecase direction
	       (:right 0d0)
	       (:top (* .5d0 pi))
	       (:left pi)
	       (:bottom (* 1.5 pi)))))
   (+ center (* radius (exp (complex 0d0 phi))))))

#+nil
(sample-circle (complex 1d0 1d0) 1d0 :right)

(defun illuminate-ray 
    (spheres-c-r illuminated-sphere-index
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
	   (double-float bfp-center-x bfp-center-y bfp-radius)
	   ((simple-array sphere 1) spheres-c-r)
	   (values double-float &optional))
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

#+nil ;; scan the bfp
(time
 (with-open-file (s "/home/martin/tmp/scan.dat"
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (let ((bfp-window-radius .08d0)
	 (nr 32))
     (dotimes (ir nr)
       (let ((np (ceiling (1+ (* ir ir)) 4)))
	 (terpri s)
	 (dotimes (ip np)
	   (let* ((r (* ir (/ (- .99d0 bfp-window-radius) (1- nr))))
		  (phi (* (/ (* 2d0 pi) np) ip))
		 (z (* r (exp (complex 0d0 phi)))))
	     (format s "~4,4f ~4,4f ~4,4f~%" (realpart z) (imagpart z)
		     (merit-function
		      (make-vec2 :x (realpart z)
				 :y (imagpart z)))))))))))
#+nil
(time
 (with-open-file (s "/home/martin/tmp/scan.dat"
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (let ((dx .025d0))
    (loop for x from -1d0 upto 1d0 by dx do
	 (loop for y from -1d0 upto 1d0 by dx do
	      (format s "~4,4f ~4,4f ~4,4f~%"
		      x y (merit-function
			   (make-vec2 :x x :y y))))
	 (terpri s)))))

(defvar *nucleus-index* 26)
(defvar *bfp-window-radius* .1d0)
(defvar *spheres-c-r* nil)

;; the following global variable contain state for merit-function:
;; *bfp-window-radius* *nucleus-index* *spheres-c-r*
(defun merit-function (vec2)
  (declare (vec2 vec2)
	   (values double-float &optional))
  (let* ((border-value .08d0) ;; value to return when outside of bfp
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

#+nil
(let ((start (make-array 2 :element-type 'double-float
			 :initial-contents (list 100d0 100d0))))
  (with-open-file (*standard-output* "/dev/shm/anneal.log"
				     :direction :output
				     :if-exists :supersede)
    (simplex-anneal:anneal (simplex-anneal:make-simplex start 1d0)
			   #'merit-function
			   :start-temperature 12d0)))

;; scan over the full parameters space
#+nil
(progn
 (with-open-file (s "/dev/shm/o.dat" :direction :output :if-exists :supersede)
   (let ((ntheta 60)
	 (nphi 90))
     (dotimes (theta ntheta)
       (dotimes (phi  nphi)
	 (let ((a (* 90 (/ theta ntheta)))
	       (b (* 180 (/ phi nphi))))
	  (format s "~f ~f ~f~%" a b
		  (ray-spheres-intersection (v) (direction a b)))))
      (terpri s))))
 (with-open-file (s "/dev/shm/p1.gp" :direction :output
		    :if-exists :supersede)
   (format s "set term posts; set outp \"/dev/shm/p~2,'0d.ps\";set hidden
set title \"nucleus nr. ~d\"
unset key
splot \"/dev/shm/o.dat\" u 1:2:3 w l
#pause -1
" *central-sphere* *central-sphere*)))

