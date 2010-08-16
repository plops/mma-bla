#+nil
(progn
   (require :vol)
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

#+nil
(init-ft)

(defmacro defstuff ()
  `(progn
     ,@(loop for i in '(*centers* *dims* *spheres* *psf* *conv-l* *conv-l-s*
			*conv-plane* *conv-plane-s*
			Lf L-plane*f)
	  collect
	    `(defparameter ,i nil))))

(defstuff)


(defun draw-spheres (radius centers z y x)
  "put points into the centers of nuclei and convolve a sphere around each"
  (declare (single-float radius)
	   ((simple-array vec-i 1) centers)
	   (fixnum z y x)
	   (values (simple-array (complex my-float) 3) &optional))
  (let* ((dims (list z y x))
	 (points (make-array dims
			     :element-type '(complex my-float)))
	 (n (length centers)))
    (dotimes (i n)
      (let ((c (aref centers i)))
	(setf (aref points
		    (* 5 (vec-i-z c))
		    (vec-i-y c)
		    (vec-i-x c))
	      (complex +one+))))
    (convolve-circ points 
		   (fftshift (draw-sphere-csf radius z y x)))))

(defun draw-ovals (radius centers z y x)
  (declare (single-float radius)
	   ((simple-array vec-i 1) centers)
	   (fixnum z y x)
	   (values (simple-array (complex my-float) 3) &optional))
  (let* ((dims (list z y x))
	 (points (make-array dims
			     :element-type '(complex my-float)))
	 (n (length centers)))
    (dotimes (i n)
      (let ((c (aref centers i)))
	(setf (aref points
		    (vec-i-z c)
		    (vec-i-y c)
		    (vec-i-x c))
	      (complex +one+))))
    (convolve-circ points (fftshift (draw-oval-csf radius z y x)))))

(defun draw-indexed-ovals (radius centers z y x)
  "The first oval contains the value 1 the second 2, ..."
  (declare (single-float radius)
	   ((simple-array vec-i 1) centers)
	   (fixnum z y x)
	   (values (simple-array (complex my-float) 3) &optional))
  (let* ((dims (list z y x))
	 (big (* x y z))
	 (points (make-array dims
			     :element-type '(complex my-float)))
	 (n (length centers)))
    (dotimes (i n)
      (let ((c (aref centers i)))
	(setf (aref points
		    (vec-i-z c)
		    (vec-i-y c)
		    (vec-i-x c))
	      (complex (* (+ +one+ i) big)))))
    (convolve-circ points
		   (fftshift (draw-oval-csf radius z y x)))))


;; to find centers of cells do a convolution with a sphere
(defun find-centers ()
  (declare (values (simple-array vec-i 1)
		   (simple-array my-float 1)
		   cons &optional))
  (let* ((stack-byte (read-stack "/home/martin/tmp/xa*.pgm"))
	 (dims (array-dimensions stack-byte))
	 (stack (make-array dims :element-type '(complex my-float))))
    (destructuring-bind (z y x) dims
      (do-region ((k j i) (z y x))
	(setf (aref stack k j i) (complex (+ (* #.(coerce .43745 'my-float) k)
					     (aref stack-byte k j i)))))
      (write-pgm "/home/martin/tmp/cut-stack.pgm"
		   (normalize-2-csf/ub8-realpart (cross-section-xz stack)))
      ;; find centers of cells by convolving with sphere, actually an
      ;; oval because the z resolution is smaller than the transversal
      (let* ((conv (convolve-circ 
		    stack (fftshift (#.(cond 
					 ((subtypep 'my-float 'single-float) 'draw-oval-csf)
					 ((subtypep 'my-float 'double-float) 'draw-oval-cdf))
				       12.0 z y x))))
	     (cv (convert conv 'sf 'realpart))
	     (centers nil)
	     (center-heights nil))
	(write-pgm "/home/martin/tmp/cut-stack2.pgm"
		   (normalize-2-csf/ub8-realpart (cross-section-xz 
					 (ft (.* (ft stack)
						 (ft (draw-oval-csf 12.0 z y x)))))))
	(do-region ((k j i) ((- z 3) (- y 1) (- x 1)) (6 1 1))
	  (macrolet ((c (a b c)
		       `(aref cv (+ k ,a) (+ j ,b) (+ i ,c))))
	    (let ((v (c 0 0 0)))
	      (when (and (< (c 0 0 -1) v)
			 (< (c 0 0 1) v)
			 (< (c 0 -1 0) v)
			 (< (c 0 1 0) v)
			 (< (c -1 0 0) v)
			 (< (c 1 0 0) v))
		(push (make-vec-i :z k :y j :x i) centers)
		(push v center-heights)))))
	(let ((c (make-array (length centers)
			     :element-type 'vec-i
			     :initial-contents centers))
	      (ch (make-array (length center-heights)
			      :element-type 'my-float
			      :initial-contents center-heights)))
	  (values c ch dims))))))

#+nil
(sb-ext:gc :full t)
#+nil
(time
 (progn 
   (format t "~a~%" (multiple-value-list (find-centers)))
   nil))

#+nil
(write-pgm "/home/martin/tmp/fftw.pgm"
 (normalize-2-csf/ub8-abs
	    (cross-section-xz 
	     (let ((a (ft (draw-sphere-csf 12.0 34 206 296)))
		   #+nil (b (ft (draw-sphere-csf 5.0 34 206 296))))
	       a))))

(defun init-model ()
  ;; find the centers of the nuclei and store into *centers*
  (multiple-value-bind (c ch dims)
      (find-centers)
    (declare (ignore ch))
    (defparameter *centers* c)
    (defparameter *dims* dims)
    (sb-ext:gc :full t))

  ;; as a model of fluorophore concentration draw ovals around the
  ;; positions in *centers* and store into *spheres*
  (let ((spheres
	 (destructuring-bind (z y x)
	     *dims*
	   (draw-ovals 12.0 *centers* z y x))))
    (defparameter *spheres* spheres)
    (write-pgm "/home/martin/tmp/comp0-spheres-cut.pgm"
	       (normalize-2-csf/ub8-realpart
	       (cross-section-xz *spheres* 
				 (vec-i-y (elt *centers* 21)))))
   (sb-ext:gc :full t))
  
  ;; store the fluorophore concentration
  (save-stack-ub8 "/home/martin/tmp/spheres" 
		  (normalize-3-csf/ub8-realpart *spheres*)))

#+nil
(time (init-model))



(defun init-psf ()
  ;; calculate intensity psf, make extend in z big enough to span the
  ;; full fluorophore concentration even when looking at the bottom
  ;; plane of it
  (let* ((dx .2)
	 (dz 1.0)
	 (psf (destructuring-bind (z y x)
		  *dims*
		(declare (ignore y x))
		(let ((r 100))
		 (psf:intensity-psf (* 2 z) r r (* z dz) (* r dx)
				    :integrand-evaluations 400)))))
    (defparameter *psf* psf)
    (write-pgm "/home/martin/tmp/comp1-psf.pgm"
	       (normalize-2-csf/ub8-realpart (cross-section-xz psf)))
    (sb-ext:gc :full t)))
#+nil
(time (init-psf))


(defun clem ()
  ;; Extract one specific plane from the fluorophore concentration
  ;; model and convolve with intensity psf. The result is the light
  ;; distribution in the sample.
  (destructuring-bind (z y x)
      (array-dimensions *spheres*)
    (declare (ignore z))
    (let* ((zz (vec-i-z (elt *centers* 31)))
	   (current-slice-bbox (make-bbox :start (v 0d0 0d0 (* 1d0 zz))
					  :end (v (* 1d0 (1- x))
						  (* 1d0 (1- y))
						  (* 1d0 zz))))
	   (current-slice (extract-bbox3-cdf *spheres* current-slice-bbox)))
      (multiple-value-bind (conv conv-start)
	  (convolve3-nocrop current-slice *psf*)
	(defparameter *conv-l* conv)
	(defparameter *conv-l-s* (v--i (make-vec-i :z zz)
				       conv-start))
	(write-pgm "/home/martin/tmp/comp2-conv.pgm"
		   (normalize2-cdf/ub8-realpart 
		    (cross-section-xz 
		     conv
		     (vec-i-y (v+-i conv-start (elt *centers* 31)))))))
      (sb-ext:gc :full t)))
  
  ;; store light distribution
  (save-stack-ub8 "/home/martin/tmp/conv-l"
		 (normalize3-cdf/ub8-realpart *conv-l*))
  
  ;; multiply fluorophore concentration with light distribution, this
  ;; gives the excitation pattern in the sample
  (progn
    (defparameter Lf (.* *conv-l* *spheres* *conv-l-s*))
    (write-pgm "/home/martin/tmp/comp3-conv-lf.pgm"
	      (normalize2-cdf/ub8-realpart 
	       (cross-section-xz Lf (vec-i-y (elt *centers* 31))))))
  
  ;; estimate in-focus and out-of-focus excitation for this particular
  ;; excitation regime
  (destructuring-bind (z y x)
      (array-dimensions Lf)
    (let* ((zz (1- (floor z 2)))
	   (in-focus (extract-bbox3-cdf Lf (make-bbox :start (v 0d0 0d0 (* 1d0 zz))
						      :end (v (* 1d0 (1- x))
							      (* 1d0 (1- y))
							      (* 1d0 zz))))))
      (save-stack-ub8 "/home/martin/tmp/Lf" (normalize3-cdf/ub8-realpart in-focus))
      (/ (mean-realpart in-focus)
	 (mean-realpart Lf)))))


(defun widefield ()
  ;; convolve a plane with the psf
  (destructuring-bind (z y x)
      (array-dimensions *spheres*)
    (declare (ignore z))
    (let* ((zz (vec-i-z (elt *centers* 31)))
	   (current-slice (make-array (list 1 y x)
				      :element-type '(complex my-float)
				      :initial-element (complex 1d0))))
      (multiple-value-bind (conv conv-start)
	  (convolve3-nocrop current-slice *psf*)
	(defparameter *conv-plane* conv)
	(defparameter *conv-plane-s* (v--i (make-vec-i :z zz)
					   conv-start))
	(write-pgm "/home/martin/tmp/comp4-conv-plane.pgm"
		   (normalize2-cdf/ub8-realpart 
		    (cross-section-xz 
		     conv
		     (vec-i-y (v+-i conv-start (elt *centers* 31)))))))
      (sb-ext:gc :full t)))

  ;; multiply fluorophore concentration with light distribution, this
  ;; gives the excitation pattern in the sample
  (progn
    (defparameter L-plane*f (.* *conv-plane* *spheres* *conv-plane-s*))
    (write-pgm "/home/martin/tmp/comp5-conv-plane-lf.pgm"
	       (normalize2-cdf/ub8-realpart 
		(cross-section-xz L-plane*f (vec-i-y (elt *centers* 31))))))
  
  ;; estimate in-focus and out-of-focus excitation for this particular
  ;; excitation regime
  (destructuring-bind (z y x)
      (array-dimensions L-plane*f)
    (let* ((zz (1- (floor z 2)))
	   (in-focus (extract-bbox3-cdf L-plane*f (make-bbox :start (v 0d0 0d0 (* 1d0 zz))
							     :end (v (* 1d0 (1- x))
								     (* 1d0 (1- y))
								     (* 1d0 zz))))))
      #+nil  (save-stack-ub8 "/home/martin/tmp/Lf" (normalize3-cdf/ub8-realpart in-focus))
      (/ (mean-realpart in-focus)
	 (mean-realpart L-plane*f)))))

#+nil
(time
 (progn
   (init-model)
   (init-psf))) ;; 7.5s 

#+nil
(time
 (clem)) ;; 8s, result: 6.5

#+nil
(time
 (widefield)) ;; 5.7s result: 1.8

#||
mkdir ~/tmp
cp /home/martin/0519/MedianofCelegans-10-02-09-LSM700-t58.tif ~/tmp/med.tif
cd ~/tmp/ 
tiffsplit med.tif
for i in *.tif ; do tifftopnm $i > `basename $i .tif`.pgm;done
||#




(declaim (ftype (function (fixnum)
			  (values fixnum &optional))
		ensure-even))
(defun ensure-even (x)
  (if (eq 1 (mod x 2))
      (1+ x)
      x))

(declaim (ftype (function (my-float (simple-array vec-i 1)
					&key (:div-x my-float)
					(:div-y my-float)
					(:div-z my-float))
			  (values (simple-array (complex my-float) 3)
				  &optional))
		draw-scaled-spheres))
;; put points into the centers of nuclei and convolve a sphere around each
(defun draw-scaled-spheres (radius centers &key (div-x 5d0)
		     (div-y 5d0)
		     (div-z 1d0))
  (let* ((max-x (floor (reduce #'max
			       (map '(simple-array fixnum 1) #'vec-i-x
				    centers)) div-x))
	 (max-y (floor (reduce #'max
			       (map '(simple-array fixnum 1) #'vec-i-y
				    centers)) div-y))
	 (max-z (floor (reduce #'max
			       (map '(simple-array fixnum 1) #'vec-i-z
				    centers)) div-z))
	 (cr (ceiling radius))
	 (rh (floor radius 2))
	 (x (ensure-even (+ max-x cr)))
	 (y (ensure-even (+ max-y cr)))
	 (z (ensure-even (+ max-z cr)))
	 (dims (list z y x))
	 (points (make-array dims
			     :element-type '(complex my-float))))
    (loop for c across centers do
	 (setf (aref points
		     (- (floor (vec-i-z c) div-z) rh)
		     (- (floor (vec-i-y c) div-y) rh)
		     (- (floor (vec-i-x c) div-x) rh))
	       (complex 1d0 0d0)))
    (convolve3-circ points
		    (convert3-ub8/cdf-complex (draw-sphere-ub8 radius z y x)))))





#+nil
(progn
 (defparameter *merge*
   (let ((a (make-array (array-dimensions *stack*)
			:element-type '(unsigned-byte 8))))
     (destructuring-bind (z y x)
	 (array-dimensions *stack*)
       (do-box (k j i 0 z 0 y 0 x)
	 (setf (aref a k j i)
	       (clamp (if (eq 0 (aref *blobs*
				      k j i))
			  (* (aref *stack* k j i) 2)
			  0))))
       a)))
 (save-stack-ub "/home/martin/tmp/merge" *merge*))

;; (- z k 1) (- y j 1) (- x i 1)



#+nil ;; model with isotropic pixels
(save-stack-ub "/home/martin/tmp/iso/iso"
	       (convert-vol (draw-scaled-spheres 2d0 *centers*)))

#+nil
(save-stack-ub "/home/martin/tmp/blobs" *blobs*)

#+nil
(save-scaled-stack 100 100 "/home/martin/tmp/sca" *blobs*)

#+nil ;; find maximum
(reduce #'max (map 'vector #'abs (sb-ext:array-storage-vector *stack*)))

#+nil ;; scale to 1
(destructuring-bind (z y x)
    (array-dimensions *stack*)
    (do-box (k j i 0 z 0 y 0 x)
      (setf (aref *stack* k j i)
	    (/ (aref *stack* k j i) 257d0))))

#+nil
(save-stack "/home/martin/tmp/stack" *stack*)


#+nil ;; make a text image of the psf
(let ((numerical-aperture 1.38d0))
  (psf:init :numerical-aperture numerical-aperture)
  (multiple-value-bind (u v)
      (psf:get-uv 0 1.5 3 :numerical-aperture numerical-aperture)
   (let* ((nu 31)
	  (nv 61)
	  (a (psf:integ-all nu nv  (/ u nu) (/ v nv)))
	  (max (aref a 0 0))
	  (scale (/ 99 max)))
     (destructuring-bind (uu vv)
	 (array-dimensions a)
       (format t "~%")
       (dotimes (u uu)
	 (dotimes (v vv)
	   (format t "~2d" (truncate (* scale (aref a u v)))))
	 (format t "~%"))))))


#+nil
(time
 (let ((z 18d0))
  (save-stack "/home/martin/tmp/psf"
	      (fftshift3 (ft3 (sim-psf 128 128 128 z (* .5d0 z))))
	      :function #'(lambda (x) (* .01d0 (abs x))))))

;; worm stack 0.198 um in X and Y and 1 um in Z

#+nil
(array-dimensions *blobs*)

#+nil ;; write ft of object
(time
 (save-stack "/home/martin/tmp/kblobs" (fftshift3 (ft3 *blobs*))
	     :function #'(lambda (x) (* 1d-4 (abs x)))))

#+nil ;; write ft of psf
(time
 (destructuring-bind (z y x)
     (array-dimensions *blobs*)
   (save-stack "/home/martin/tmp/otf"
	       (fftshift3 (ft3 (sim-psf z y x
					(* .198d0 z) 
					(* .198d0
					   (ceiling (* (sqrt 2d0) (max y x)))))))
	      :function #'(lambda (x) (* 1d-1 (abs x))))))

(declaim (ftype (function ((simple-array (complex my-float) (* * *)))
			  (values (simple-array (complex my-float) (* * *))
				  &optional))
		zshift3))
(defun zshift3 (in)
  (let ((out (make-array (array-dimensions in)
			 :element-type '(complex my-float))))
   (destructuring-bind (w2 w1 w0)
       (array-dimensions in)
     (dotimes (k w2)
       (dotimes (j w1)
	 (dotimes (i w0)
	   (let* ((kk (if (> k (/ w2 2))
			  (+ w2 (/ w2 2) (- k))
			  (- (/ w2 2) k))))
	     (setf (aref out k j i)
		   (aref in kk j i))
	     nil)))))
   out))



#+nil
(time
 (destructuring-bind (z y x)
     (array-dimensions *blobs*)
  (let ((psf (sim-psf z y x
		      (* .198d0 z) (* .198d0 (ceiling (* (sqrt 2d0) (max y x)))))))
    (save-stack "/home/martin/tmp/impsf"
		(convolve3 *blobs* psf)
		:function #'(lambda (x) (* 1d-8 (realpart x)))))))



#+nil ;; compare intensity and |e-field|^2
(time
 (let ((z 128)
       (y 128)
       (x 128))
  (multiple-value-bind (e0 e1 e2)
      (psf:electric-field-psf z x y 10d0 5d0)
    (let ((intens (make-array (array-dimensions e0)
			      :element-type '(complex my-float))))
      (do-box (k j i 0 z 0 y 0 x)
	(setf (aref intens k j i) (complex (+ (psf::abs2 (aref e0 k j i))
					      (psf::abs2 (aref e1 k j i))
					      (psf::abs2 (aref e2 k j i))))))
      (let* ((k0 (fftshift3 (ft3 intens)))
	     (k1 (fftshift3 (ft3 (psf:intensity-psf z y x 10d0 5d0))))
	     (k- (make-array (array-dimensions k0)
			     :element-type '(complex my-float))))
	(do-box (k j i 0 z 0 y 0 x)
	  (setf (aref k- k j i) (- (aref k0 k j i)
				   (aref k1 k j i))))
	(save-stack "/home/martin/tmp/intens0"
		    k-
		    :function #'(lambda (x) (* 1d-1 (abs x))))
	(write-pgm (convert-img (cross-section-xz k-)
				#'(lambda (z) (* 1e-1 (abs z))))
		   "/home/martin/tmp/intens0xz.pgm"))))))



#+nil ;; find centers of nuclei 12.5s 3.1s
(time
 (multiple-value-bind (c ch dims)
       (find-centers)
   (defparameter *centers* c)
      (defparameter *center-heights* ch)
   (defparameter *dims* dims)
   (sb-ext:gc :full t)))

#+nil ;; draw the spheres (squeezed in z) 9.7s 1.8s
(time
 (let ((spheres
	(destructuring-bind (z y x)
	    *dims*
	  (draw-ovals 7d0 *centers* z y x))))
   (setf *spheres* spheres)
   #+nil (save-stack-ub8 "/home/martin/tmp/spheres" (normalize-vol *spheres*))
   (write-pgm (normalize-img (cross-section-xz *spheres* 
					       (vec-i-y (elt *centers* 31))))
	      "/home/martin/tmp/spheres-cut.pgm")
   (sb-ext:gc :full t)))

#+nil ;; draw the spheres (squeezed in z) and emphasize one of them
(time
 (let ((spheres
	(destructuring-bind (z y x)
	    *dims*
	  (let* ((dims (list z y x))
		 (points (make-array dims
			     :element-type '(complex my-float)))
		 (centers *centers*)
		 (radius 7d0)
		 (n (length centers)))
	    (dotimes (i n)
	      (let ((c (aref centers i)))
		(setf (aref points
			    (vec-i-z c)
			    (vec-i-y c)
			    (vec-i-x c))
		      (complex (if (eq i 31)
				   2d0
				   1d0) 0d0))))
	    (convolve3-circ points (fftshift3 (draw-oval radius z y x)))))))
   (save-stack-ub8 "/home/martin/tmp/spheres" (normalize-ub8 spheres))
   (sb-ext:gc :full t)))

#+nil ;; construct LCOS image
(let ((coord (aref *centers* 31))
      (radius 7d0)
      (slice (make-array (array-dimensions *spheres*)
			 :element-type '(complex my-float))))
  (destructuring-bind (z y x)
      (array-dimensions *spheres*)
    ;; draw only the center sphere
    (let* ((xc (vec-i-x coord))
	   (yc (vec-i-y coord))
	   (zc (vec-i-z coord))
	   (k  zc))
      (do-rectangle (j i 0 y 0 x)
       (let ((r (sqrt (+ (square (* 1d0 (- i xc)))
			 (square (* 1d0 (- j yc)))
			 (square (* 1d0 (- k zc)))))))
	 (setf (aref slice k j i)
	       (if (< r radius)
		   (complex 255d0)
		   (complex 0d0)))))))
  (defparameter *slice* slice)
  #+nil (save-stack-ub8 "/home/martin/tmp/slice" (convert-vol slice))
  (write-pgm (normalize-img (cross-section-xz slice (vec-i-y coord)))
	     "/home/martin/tmp/slice-cut.pgm")
  (sb-ext:gc :full t))

(defparameter *bfp-circ-radius* .3d0)
(defparameter *bfp-circ-center-x* .4d0 #+nil (- .999d0 *bfp-circ-radius*))

#+nil ;; 11.3s 2.6s
(time
 (progn
  (angular-psf :x 80 :z 90 
	       :window-x *bfp-circ-center-x* 
	       :window-y 0d0 :window-radius *bfp-circ-radius*
	       :numerical-aperture 1.38d0
	       :immersion-index 1.515d0
	       :pixel-size-x .1d0 :pixel-size-z .5d0
	       :integrand-evaluations 160
	       :debug t)
  nil))

#+nil ;; light distribution in the specimen
;; default resolution is isotropic 12 um /64 = 187.5 nm/pixel
(time ;; 32.5s 5.4s
 (let* ((radius .2d0)
	(x .3d0)
	(xx 120)
	(zz 120)
	(dx .1d0)
	(dz .5d0)
	(psf (resample-half 
	      (angular-psf :window-x *bfp-circ-center-x*
			   :window-y 0d0
			   :window-radius *bfp-circ-radius*
			   :x (* 2 xx) :z (* 2 zz)
			   :pixel-size-x dx :pixel-size-z dz
			   :integrand-evaluations 200)))
	(dims (destructuring-bind (z y x)
		  *dims*
		(list z y x))))
   (write-pgm (normalize-img (cross-section-xz psf))
	      "/home/martin/tmp/small-psf-cut.pgm")
   (sb-ext:gc :full t)
   (defparameter *slice-x-psf* (convolve3 *slice* psf))
   (sb-ext:gc :full t)))

#+nil
(defparameter *slice-x-psf* nil)
#+nil
(sb-ext:gc :full t)
#+nil
(write-pgm (normalize-img
	    (cross-section-xz *slice-x-psf* 
			      (vec-i-y (elt *centers* 31))))
	   "/home/martin/tmp/slice-x-psf-cut.pgm")

#+nil
(save-stack-ub8 "/home/martin/tmp/psf" (normalize-ub8 *psf*))


#+nil ;; draw lines into the light distribution in the specimen
(destructuring-bind (z y x)
    (array-dimensions *slice-x-psf*)
  (let ((coord (elt *centers* 31))
	(vol (normalize-ub8 *slice-x-psf*))
	(dx 2.d-4)
	(dz 1d-3))
    #+nil(draw-ray-into-vol (* dx (- (floor x 2) (vec-i-x coord)))
		       (* dx (- (floor y 2) (vec-i-y coord)))
		       -.6d0 0d0 vol)
    (loop for pos in (list (list (* (- (floor x 2) (- (vec-i-x coord) 7)) dx)
				 (* (- (floor y 2) (vec-i-y coord)) dx))
			   (list (* (- (floor x 2) (+ (vec-i-x coord) 7)) dx)
				 (* (- (floor y 2) (vec-i-y coord)) dx))) do
	 (loop for angle in (list ;;-.010d0
				  ;;-.6d0
				  ;;(- (- *bfp-circ-center-x* *bfp-circ-radius*))
				  ;;-.8d0
				  ;;-.99d0
			     (- *bfp-circ-center-x*)
				  ;;(- (+ *bfp-circ-center-x* *bfp-circ-radius*))
				  ) do
	      (draw-ray-into-vol (first pos) (second pos)
				 angle 0d0
				 vol
				 :shift-z (- (vec-i-z coord)
		 			     (floor z 2)))))
    nil
    #+nil (write-pgm (normalize-img
		(cross-section-xz vol
				  (vec-i-y (elt *centers* 31))))
	       "/home/martin/tmp/slice-x-psf-lines-cut.pgm")
    (save-stack-ub8 "/home/martin/tmp/slice-x-psf" vol)))


#+nil ;; excited fluorophores
(progn
  (setf *slice-x-psf-times-spheres* (.* *spheres* *slice-x-psf*))
  (save-stack-ub8 "/home/martin/tmp/slice-x-psf-times-spheres"
		 (normalize-ub8 *slice-x-psf-times-spheres*)))


#+nil ;; blur with detection psf
(time
 (let* ((radius .5d0)
	(x (- 1d0 radius))
	(xx 80)
	(yy xx)
	(zz 128)
	(dx .2d0)
	(psf (psf:intensity-psf zz yy xx (* zz dx) (* xx dx)
				:integrand-evaluations 100))
	(dims (destructuring-bind (z y x)
		  *dims*
		(list (* z 5) y x)))
	(psf-big (make-array dims
			     :element-type '(complex my-float))))
   (setf *psf-big* psf-big)
   (destructuring-bind (z y x)
       dims
     (let ((ox (- (floor x 2) (floor xx 2)))
	   (oy (- (floor y 2) (floor yy 2)))
	   (oz (- (floor z 2) (floor zz 2))))
       (do-box (k j i 0 zz 0 yy 0 xx)
	 (setf (aref psf-big (+ oz k) (+ oy j) (+ ox i))
	       (aref psf k j i)))))
   (save-stack-ub8 "/home/martin/tmp/psf-detect-big" (normalize-ub8 psf-big))
   (sb-ext:gc :full t)
   (defparameter *camera-volume* (convolve3-circ *slice-x-psf-times-spheres*
						 (fftshift3 psf-big)))
   (save-stack-ub8 "/home/martin/tmp/camera-volume"
		   (normalize-ub8 *camera-volume*))
   (sb-ext:gc :full t)))



#+nil ;; check convolution
(time
 (let ((a (make-array (list 64 64 64)
		      :element-type '(complex my-float)))
       (b (psf:intensity-psf 64 64 64 20d0 20d0) #+nil (make-array (list 64 64 64)
		      :element-type '(complex my-float))))
   (setf (aref a 12 12 12) (complex 255d0))
#+nil   (setf (aref b 0 0 0) (complex 255d0))
   (save-stack-ub8 "/home/martin/tmp/conv-test" (normalize-ub8 (convolve3-circ a (fftshift3 b))))))


#+nil ;; output the xz cross section centered on a sphere in the middle
(let ((coord (aref *centers* 30)))
  (write-pgm (normalize-img (cross-section-xz *spheres* (* 5 (vec-i-z coord))))
	     "/home/martin/tmp/cut-spheres.pgm")
  (write-pgm (normalize-img (cross-section-xz *psf-big*))
	     "/home/martin/tmp/cut-psf-big.pgm")
  (write-pgm (normalize-img (cross-section-xz *slice-x-psf* (* 5 (vec-i-z coord))))
	     "/home/martin/tmp/cut-slice-x-psf.pgm")
  (write-pgm (normalize-img (cross-section-xz (.* *spheres* *slice-x-psf*) (* 5 (vec-i-z coord))))
	     "/home/martin/tmp/cut-exfluo.pgm"))

#+nil
(save-stack-ub8 "/home/martin/tmp/spheres" (normalize-ub8 *spheres*))

#+nil
(let ((sli (make-array (array-dimensions *spheres*)
		       :element-type '(complex my-float))))
  (destructuring-bind (z y x)
      (array-dimensions *spheres*)
    (do-box (k j i 0 z 0 y 0 x)
      (setf (aref sli k j i)
	    (aref *spheres* k j i)))
    (let ((k (* 5 (vec-i-z (aref *centers* 30)))))
     (do-rectangle (j i 0 y 0 x)
       (setf (aref sli k j i)
	     (* 10 (aref sli k j i)))))
    (defparameter *sli* sli))
  (save-stack-ub8 "/home/martin/tmp/spheres" (normalize-ub8 *sli*)))



#+nil
(let ((a (sb-ext:array-storage-vector *slice*)))
  (reduce #'max (map 'vector #'abs a)))

#+nil
(sb-ext:gc :full t)

#+nil ;; model with unscaled spheres
(defparameter *blobs*
  (destructuring-bind (z y x)
      *dims*
    (draw-spheres 7d0 *centers* (* 5 z) y x)))


#+nil ;; print ft of angular psf
(time (let* ((radius .5d0)
	(x (- 1d0 radius))
	(psf (angular-psf x 0d0 radius)))
   (write-pgm (normalize-ub8 (cross-section-xz (fftshift3 (ft3 psf))))
	      "/home/martin/tmp/cut-intens.pgm")))
#+nil
(let* ((intens (psf:intensity-psf 64 64 64 10d0 5d0))
       (k0 intens #+nil(fftshift3 (ft3 intens))))
  (save-stack "/home/martin/tmp/intens1" k0
	      :function #'(lambda (x) (* 1d-5 (abs x))))
  (write-pgm (convert-img
	      (cross-section-xz k0)
	      #'(lambda (z) (* 1d-4 (abs z))))
	     "/home/martin/tmp/intens1xz.pgm"))

#+nil
(time
 (destructuring-bind (z y x)
     (array-dimensions *blobs*)
   (save-stack "/home/martin/tmp/blobs"
	       *blobs*
	       )))


#+nil ;; clean up the garbage
(sb-ext:gc :full t)


#+nil
(sb-vm:memory-usage :print-spaces t :count-spaces t)
#+nil
(sb-vm:memory-usage)

#+nil
(sb-vm:instance-usage :dynamic)

#+nil
(sb-vm:list-allocated-objects :dynamic)

#+nil
(sb-vm:print-allocated-objects :dynamic)

#+nil
(format t "~a~%" (sb-vm::type-breakdown :dynamic))


;; 
(defun split-by-char (char string)
    "Returns a list of substrings of string
divided by ONE character CHAR each.
Note: Two consecutive CHAR will be seen as
if there were an empty string between them."
    (declare (character char)
	     (string string))
    (loop for i = 0 then (1+ j)
       as j = (position char string :start i)
       collect (subseq string i j)
       while j))

#+nil
(split-by-char #\x "12x124x42")

(defun parse-raw-filename (fn)
  "Parses a filename like
/home/martin/d0708/stacks/c-291x354x41x91_dx200_dz1000.raw and returns
291 354 41 and 91 as multiple values."
  (declare (string fn)
	   (values (or null fixnum) fixnum fixnum fixnum &optional))
  (let* ((p- (position #\- fn :from-end t))
	 (part (subseq fn (1+ p-)))
	 (p_ (position #\_ part))
	 (sizes (subseq part 0 p_))
	 (numlist (split-by-char #\x sizes)))
    (unless (eq 4 (length numlist))
      (error "didn't read 4 dimensions as expected."))
    (destructuring-bind (x y z time)
	(mapcar #'read-from-string numlist)
     (values x y z time))))


#+nil
(parse-raw-filename "/home/martin/d0708/stacks/c-291x354x41x91_dx200_dz1000.raw")

(defun read-raw-stack-video-frame (fn time)
  (declare (string fn)
	   (fixnum time)
	   (values (simple-array (unsigned-byte 8) 3) &optional))
  (multiple-value-bind (x y z maxtime)
      (parse-raw-filename fn)
      (unless (< time maxtime)
	(error "requested time ~d is to big (must be <~d!)" time maxtime))
      (let* ((vol (make-array (list z y x)
			      :element-type '(unsigned-byte 8)))
	     (vol1 (sb-ext:array-storage-vector vol)))
	(with-open-file (s fn :direction :input
			   :element-type '(unsigned-byte 8))
	  (file-position s (* x y z time))
	  (read-sequence vol1 s))
	vol)))
#+nil
(time 
 (let* ((fn "/home/martin/d0708/stacks/c-291x354x41x91_dx200_dz1000_2.raw")
	(ao (decimate-xy-ub8 5
			     (read-raw-stack-video-frame fn 0))))
   (destructuring-bind (z y x)
       (array-dimensions ao)
     (let* ((timestep 20)
	    (o (loop for radius from 1 upto 10 collect
		    (let* ((oval (draw-sphere-ub8 (* 1d0 radius) z y x))
			   (volume (count-non-zero-ub8 oval)))
		      (list radius volume
			    (ft3 (convert3-ub8/cdf-complex oval)))))))
       (let* ((ao (decimate-xy-ub8 5
				   (read-raw-stack-video-frame fn timestep)))
	      (a (convert3-ub8/cdf-complex ao))
	      (ka (ft3 a)))
	 (loop for i in o do
	      (destructuring-bind (radius volume oval)
		  i
		(let* ((dir (format nil "/home/martin/tmp/o~d" radius))
		       (conv (fftshift3 (ift3 (.* ka oval))))
		       (conv-df (convert3-cdf/df-realpart conv)))
		 (save-stack-ub8 dir
				 (normalize-ub8-df/ub8-realpart conv-df))
		 (with-open-file (s (format nil "~a/maxima" dir)
				    :if-exists :supersede
				   :direction :output)
		   (loop for el in (find-maxima3-df conv-df) do
			(destructuring-bind (height pos)
			    el
			  (format s "~f ~d ~a~%" 
				  (/ height volume)
				  volume
				  (v*-i 
				   (map 'vec-i #'(lambda (x) (floor x 2)) pos)
				   2))))))))
	nil)))))
#+nil
(sb-ext:gc :full t)
#+nil
(save-stack-ub8 "/home/martin/tmp/conv" conv)

#+nil
(find-maxima3 conv)

#+nil
(destructuring-bind (z y x)
    (array-dimensions *a*)
  (let ((b (draw-ovals 12d0 (find-maxima3 conv) (ensure-even z) (ensure-even y) (ensure-even x))))
    (write-pgm (convert-img (cross-section-xz b 42))
	       "/home/martin/tmp/time0.pgm")))

(defun find-maxima3-df (conv)
  (declare ((simple-array my-float 3) conv)
	   (values (simple-array * 1) &optional))
 (destructuring-bind (z y x)
     (array-dimensions conv)
   (let ((centers nil #+nil(make-array 0 :element-type 'vec-i
			      :initial-element (make-vec-i)
			      :adjustable t
			      :fill-pointer t)))
     (do-box (k j i 6 (- z 3) 1 (1- y) 1 (1- x))
       (let ((v (aref conv k j i)))
	 (when (and (< (aref conv k j (1- i)) v)
		    (< (aref conv k j (1+ i)) v)
		    (< (aref conv k (1- j) i) v)
		    (< (aref conv k (1+ j) i) v)
		    (< (aref conv (1- k) j i) v)
		    (< (aref conv (1+ k) j i) v))
	   ;; this is TOO slow
	   #+nil(setf centers (append centers
				 (list (list v (make-vec-i :z k :y j :x i)))))
	   ;; this is faster
	   #+nil(setf centers (nconc centers 
				(list (list v (make-vec-i :z k :y j :x i)))))
	   ;; I think push is the right thing to do
	   (push (list v (make-vec-i :z k :y j :x i)) centers)
	   #+nil(vector-push-extend
	    (make-vec-i :z k :y j :x i)
	    centers))))
;;    (nreverse centers)
     (coerce centers 'simple-vector) ;; I saw this in raylisps load-obj
   #+nil  (make-array (length centers)
		 :element-type 'vec-i
		 :initial-contents centers))))

