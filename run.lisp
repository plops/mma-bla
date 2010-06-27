#.(progn (require :asdf)
	 (require :vol)
	 (require :psf)
	 (require :simplex-anneal)
	 (require :raytrace)
	 (require :lens))

(defpackage :run
  (:use :cl :vol :raytrace))
(in-package :run)
#||
mkdir ~/tmp
cp /home/martin/0519/MedianofCelegans-10-02-09-LSM700-t58.tif ~/tmp/med.tif
cd ~/tmp/
tiffsplit med.tif
for i in *.tif ; do tifftopnm $i > `basename $i .tif`.pgm;done
||#

(deftype vec-i ()
  `(simple-array fixnum (3)))

(defstruct (vec-i (:type (vector fixnum)))
  (z 0) (y 0) (x 0))

(declaim (ftype (function (vec-i vec-i)
			  (values fixnum &optional))
		v.-i))
(defun v.-i (a b)
  (+ (* (vec-i-x a) (vec-i-x b))
     (* (vec-i-y a) (vec-i-y b))
     (* (vec-i-z a) (vec-i-z b))))

(declaim (ftype (function (vec-i vec-i)
			  (values vec-i &optional))
		v--i v+-i))
(defun v--i (a b)
  (make-vec-i :x (- (vec-i-x a) (vec-i-x b))
	      :y (- (vec-i-y a) (vec-i-y b))
	      :z (- (vec-i-z a) (vec-i-z b))))
(defun v+-i (a b)
  (make-vec-i :x (+ (vec-i-x a) (vec-i-x b))
	      :y (+ (vec-i-y a) (vec-i-y b))
	      :z (+ (vec-i-z a) (vec-i-z b))))

(declaim (ftype (function (vec-i)
			  (values double-float &optional))
		norm-i))
(defun norm-i (a)
  (sqrt (* 1d0 (v.-i a a))))

(declaim (ftype (function (string (simple-array (complex double-float) 3)
				  &key (:function function))
			  (values null &optional))
		save-stack))
(defun save-stack (fn vol &key (function #'realpart))
  (ensure-directories-exist fn)
  (destructuring-bind (z y x)
      (array-dimensions vol)
    (let ((b (make-array (list y x)
			 :element-type '(unsigned-byte 8))))
      (dotimes (k z)
	(do-rectangle (j i 0 y 0 x)
	  (setf (aref b j i)
		(clamp (floor (* 255 (funcall function (aref vol k j i)))))))
	(write-pgm b (format nil "~a/~3,'0d.pgm" fn k)))))
  nil)

(declaim (ftype (function (string (simple-array (unsigned-byte 8) 3))
			  (values null &optional))
		save-stack-ub8))
(defun save-stack-ub8 (fn vol)
  (ensure-directories-exist fn)
  (destructuring-bind (z y x)
      (array-dimensions vol)
    (let ((b (make-array (list y x)
			 :element-type '(unsigned-byte 8))))
      (dotimes (k z)
	(do-rectangle (j i 0 y 0 x)
	  (setf (aref b j i)
		(aref vol k j i)))
	(write-pgm b (format nil "~a/~3,'0d.pgm" fn k)))))
  nil)


(declaim (ftype (function (fixnum fixnum string
				  (simple-array (complex double-float) 3))
			  (values null &optional))
		save-scaled-stack))
(defun save-scaled-stack (h w fn vol)
    (destructuring-bind (z y x)
      (array-dimensions vol)
      (let* ((b (make-array (list y x)
			    :element-type '(unsigned-byte 8)))
	     (bb (make-array (list h w)
			     :element-type '(unsigned-byte 8))))
       (dotimes (k z)
	 (do-rectangle (j i 0 y 0 x)
	   (setf (aref b j i)
		 (clamp (floor (* 255 (realpart (aref vol k j i)))))))
	 (do-rectangle (j i 0 h 0 w)
	   (setf (aref bb j i)
		 (floor (interpolate2 b (* (/ 1d0 h) j y) (* (/ 1d0 w) i x)))))
	 (write-pgm bb (format nil "~a~3,'0d.pgm" fn k)))))
  nil)

(declaim (ftype (function ((simple-array (complex double-float) 3)
			   (simple-array (complex double-float) 3))
			  (values (simple-array (complex double-float) 3) &optional))
		.*))
(defun .* (vola volb)
  (let ((result (make-array (array-dimensions vola)
			    :element-type (array-element-type vola))))
   (destructuring-bind (z y x)
       (array-dimensions vola)
     (do-box (k j i 0 z 0 y 0 x)
       (setf (aref result k j i)
	     (* (aref vola k j i)
		(aref volb k j i)))))
   result))

(declaim (ftype (function (double-float (simple-array (complex double-float) 3))
			  (values (simple-array (complex double-float) 3) &optional))
		s*))

(defun s* (s vol)
  (let* ((a (sb-ext:array-storage-vector vol))
	 (n (length a)))
    (dotimes (i n)
      (setf (aref a i) (* s (aref a i)))))
  vol)


(declaim (ftype (function ((simple-array (complex double-float) 3)
			   (simple-array (complex double-float) 3))
			  (values (simple-array (complex double-float) 3) &optional))
		convolve3-circ convolve3))
(defun convolve3-circ (vola volb)
  (let* ((da (array-dimensions vola))
	 (db (array-dimensions volb))
	 (compare-ab (map 'list #'(lambda (x y) (eq x y)) da db)))
    (when (some #'null compare-ab)
      (error "convolve3-circ expects both input arrays to have the same dimensions.")))
  (ift3 (.* (ft3 vola) (ft3 volb))))

;; volb is the kernel
(defun convolve3 (vola volb)
  (destructuring-bind (za ya xa)
      (array-dimensions vola)
    (destructuring-bind (zb yb xb)
	(array-dimensions volb)
      (let* ((biga (make-array (list (+ za zb)
				     (+ ya yb)
				     (+ xa xb))
			       :element-type '(complex double-float)))
	     (bigb (make-array (array-dimensions biga)
			       :element-type '(complex double-float)))
	     (fzb (floor zb 2))
	     (fyb (floor yb 2))
	     (fxb (floor xb 2))
	     (fza (floor za 2))
	     (fya (floor ya 2))
	     (fxa (floor xa 2)))
	(do-box (k j i 0 za 0 ya 0 xa)
	  (setf (aref biga (+ k fzb) (+ j fyb) (+ i fxb))
		(aref vola k j i)))
	(do-box (k j i 0 zb 0 yb 0 xb)
	  (setf (aref bigb (+ k fza) (+ j fya) (+ i fxa))
		(aref volb k j i)))
	(let* ((conv (convolve3-circ biga (fftshift3 bigb)))
	       (result (make-array (array-dimensions vola)
				   :element-type '(complex double-float))))
	  (do-box (k j i 0 za 0 ya 0 xa)
	    (setf (aref result k j i)
		  (aref conv (+ k fzb) (+ j fyb) (+ i fxb))))
	  result)))))

#+nil
(let ((a (make-array (list 100 200 300)
		     :element-type '(complex double-float)))
      (b (make-array (list 10 200 30)
		     :element-type '(complex double-float))))
  (convolve3 a b)
  nil)

(declaim (ftype (function (double-float fixnum fixnum fixnum &key (:scale-z double-float))
			  (values (simple-array (complex double-float) 3) &optional))
		draw-sphere))
(defun draw-sphere (radius z y x &key (scale-z 1d0))
  (let ((sphere (make-array (list z y x)
			    :element-type '(complex double-float))))
    (do-box (k j i 0 z 0 y 0 x)
      (let ((r (sqrt (+ (square (- i (* .5d0 x)))
			(square (- j (* .5d0 y)))
			(square (* scale-z (- k (* .5d0 z))))))))
	(setf (aref sphere k j i)
	      (if (< r radius)
		  (complex 1d0)
		  (complex 0d0)))))
    sphere))

(declaim (ftype (function ()
			  (values (simple-array vec-i 1)
				  (simple-array double-float 1)
				  cons
				  &optional))
		find-centers))
(defvar *stack* ())
;; to find centers of cells do a convolution with a sphere
(defun find-centers ()
  (let* ((stack-byte (read-stack "/home/martin/tmp/xa*.pgm"))
	 (stack (make-array (array-dimensions stack-byte)
			    :element-type '(complex double-float))))
    (destructuring-bind (z y x)
	(array-dimensions stack)
      (dotimes (k z)
	(dotimes (j y)
	  (dotimes (i x)
	    (let ((v (+ (* .43745d0 k)
			(aref stack-byte k j i))))
	      (setf
	       (aref stack-byte k j i) (clamp (floor v))
	       (aref stack k j i) (complex v))))))
      #+nil(setf *stack* stack)
      ;; find centers of cells by convolving with sphere
      (let* ((sphere (draw-sphere 11d0 z y x :scale-z 5d0))
	     (conv (convolve3-circ stack (fftshift3 sphere)))
	     (conv-byte (make-array (list y x)
				   :element-type '(unsigned-byte 8)))
	     (centers (make-array 0 :element-type 'vec-i
				  :initial-element (make-vec-i)
				  :adjustable t
				  :fill-pointer t))
	     (center-heights (make-array 0 :element-type 'double-float
				  :adjustable t
				  :fill-pointer t)))
	(loop for k from 6 below (- z 3) do
	     (loop for j from 1 below (1- y) do
		  (loop for i from 1 below (1- x) do
		       (let ((v (abs (aref conv k j i))))
			 (setf (aref conv-byte j i)
			       (if (and (< (abs (aref conv k j (1- i))) v)
					(< (abs (aref conv k j (1+ i))) v)
					(< (abs (aref conv k (1- j) i)) v)
					(< (abs (aref conv k (1+ j) i)) v)
					(< (abs (aref conv (1- k) j i)) v)
					(< (abs (aref conv (1+ k) j i)) v))
				   (progn
				     (vector-push-extend
				      (make-vec-i :z k :y j :x i)
				      centers)
				     (vector-push-extend v center-heights)
				     0)
				   (clamp (floor (/ v (* 4e2 x y z)))))))))
	     #+nil(write-pgm conv-byte
			(format nil "/home/martin/tmp/conv~3,'0d.pgm" k)))
	(let ((c (make-array (length centers)
			     :element-type 'vec-i
			     :initial-contents centers))
	      (ch (make-array (length center-heights)
			     :element-type 'double-float
			     :initial-contents center-heights)))
	  (values c ch (array-dimensions stack)))))))


(declaim (ftype (function (fixnum)
			  (values fixnum &optional))
		ensure-even))
(defun ensure-even (x)
  (if (eq 1 (mod x 2))
      (1+ x)
      x))

(declaim (ftype (function (double-float (simple-array vec-i 1)
					&key (:div-x double-float)
					(:div-y double-float)
					(:div-z double-float))
			  (values (simple-array (complex double-float) 3)
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
			     :element-type '(complex double-float))))
    (loop for c across centers do
	 (setf (aref points
		     (- (floor (vec-i-z c) div-z) rh)
		     (- (floor (vec-i-y c) div-y) rh)
		     (- (floor (vec-i-x c) div-x) rh))
	       (complex 1d0 0d0)))
    (convolve3-circ points (draw-sphere radius z y x))))

(declaim (ftype (function ((simple-array (complex double-float) 3))
			  (values (simple-array (unsigned-byte 8) 3)
				  &optional))
		convert-vol))
(defun convert-vol (vol)
  (destructuring-bind (z y x)
      (array-dimensions vol)
   (let ((result (make-array (array-dimensions vol)
			     :element-type '(unsigned-byte 8))))
     (do-box (k j i 0 z 0 y 0 x)
       (setf (aref result k j i)
	     (clamp (floor (* 255 (abs (aref vol k j i)))))))
     result)))

(declaim (ftype (function ((simple-array (complex double-float) 2)
			   &optional function)
			  (values (simple-array (unsigned-byte 8) 2)
				  &optional))
		convert-img))
(defun convert-img (img &optional (function #'abs))
  (destructuring-bind (y x)
      (array-dimensions img)
   (let ((result (make-array (array-dimensions img)
			     :element-type '(unsigned-byte 8))))
     (do-rectangle (j i 0 y 0 x)
       (setf (aref result j i)
	     (clamp (floor (funcall function (aref img j i))))))
     result)))

(declaim (ftype (function (double-float (simple-array vec-i 1)
					fixnum fixnum
					fixnum)
			  (values (simple-array (complex double-float) 3)
				  &optional))
		draw-spheres))
;; put points into the centers of nuclei and convolve a sphere around each
(defun draw-spheres (radius centers z y x)
  (let* ((dims (list z y x))
	 (points (make-array dims
			     :element-type '(complex double-float)))
	 (n (length centers)))
    (dotimes (i n)
      (let ((c (aref centers i)))
       (setf (aref points
		   (* 5 (vec-i-z c))
		   (vec-i-y c)
		   (vec-i-x c))
	     (complex 1d0 0d0))))
    (convolve3-circ points (fftshift3 (draw-sphere radius z y x)))))



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
					(* .198d0 z) (* .198d0 (ceiling (* (sqrt 2d0) (max y x)))))))
	      :function #'(lambda (x) (* 1d-1 (abs x))))))

(declaim (ftype (function ((simple-array (complex double-float) (* * *)))
			  (values (simple-array (complex double-float) (* * *))
				  &optional))
		zshift3))
(defun zshift3 (in)
  (let ((out (make-array (array-dimensions in)
			 :element-type '(complex double-float))))
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


(declaim (ftype (function ((simple-array (complex double-float) 3)
			   &optional fixnum)
			  (values (simple-array (complex double-float) 2)
				  &optional))
		cross-section-xz))
(defun cross-section-xz (a &optional (y (floor (array-dimension a 1) 2)))
  (destructuring-bind (z y-size x)
      (array-dimensions a)
    (unless (<= 0 y (1- y-size))
      (error "Y is out of bounds."))
    (let ((b (make-array (list z x)
			 :element-type `(complex double-float))))
      (do-rectangle (j i 0 z 0 x)
	(setf (aref b j i)
	      (aref a j y i)))
      b)))

(declaim (ftype (function ((simple-array (complex double-float) 2)
			   &key (:function function))
			  (values (simple-array (unsigned-byte 8) 2)
				  &optional))
		normalize-img))
(defun normalize-img (a &key (function #'abs))
  (destructuring-bind (y x)
      (array-dimensions a)
    (let ((b (make-array (list y x)
			 :element-type 'double-float)))
      (do-rectangle (j i 0 y 0 x)
	(setf (aref b j i)
	      (funcall function (aref a j i))))
      (let* ((b1 (sb-ext:array-storage-vector b))
	     (ma (reduce #'max b1))
	     (mi (reduce #'min b1))
	     (result (make-array (list y x)
				 :element-type '(unsigned-byte 8)))
	     (s (/ 255d0 (- ma mi))))
	(do-rectangle (j i 0 y 0 x)
	  (setf (aref result j i)
		(floor (* s (- (aref b j i) mi)))))
	result))))

(declaim (ftype (function ((simple-array (complex double-float) 3)
			   &key (:function function))
			  (values (simple-array (unsigned-byte 8) 3)
				  &optional))
		normalize-vol))
(defun normalize-vol (a &key (function #'abs))
  (destructuring-bind (z y x)
      (array-dimensions a)
    (let ((b (make-array (list z y x)
			 :element-type 'double-float)))
      (do-box (k j i 0 z 0 y 0 x)
	(setf (aref b k j i)
	      (funcall function (aref a k j i))))
      (let* ((b1 (sb-ext:array-storage-vector b))
	     (ma (reduce #'max b1))
	     (mi (reduce #'min b1))
	     (result (make-array (list z y x)
				 :element-type '(unsigned-byte 8)))
	     (s (/ 255d0 (- ma mi))))
	(do-box (k j i 0 z 0 y 0 x)
	  (setf (aref result k j i)
		(floor (* s (- (aref b k j i) mi)))))
	result))))

#+nil ;; compare intensity and |e-field|^2
(time
 (let ((z 128)
       (y 128)
       (x 128))
  (multiple-value-bind (e0 e1 e2)
      (psf:electric-field-psf z x y 10d0 5d0)
    (let ((intens (make-array (array-dimensions e0)
			      :element-type '(complex double-float))))
      (do-box (k j i 0 z 0 y 0 x)
	(setf (aref intens k j i) (complex (+ (psf::abs2 (aref e0 k j i))
					      (psf::abs2 (aref e1 k j i))
					      (psf::abs2 (aref e2 k j i))))))
      (let* ((k0 (fftshift3 (ft3 intens)))
	     (k1 (fftshift3 (ft3 (psf:intensity-psf z y x 10d0 5d0))))
	     (k- (make-array (array-dimensions k0)
			     :element-type '(complex double-float))))
	(do-box (k j i 0 z 0 y 0 x)
	  (setf (aref k- k j i) (- (aref k0 k j i)
				   (aref k1 k j i))))
	(save-stack "/home/martin/tmp/intens0"
		    k-
		    :function #'(lambda (x) (* 1d-1 (abs x))))
	(write-pgm (convert-img (cross-section-xz k-)
				#'(lambda (z) (* 1e-1 (abs z))))
		   "/home/martin/tmp/intens0xz.pgm"))))))

;; create psf by blocking light in the back-focal plane. only a disk
;; centered on point p=(CENTER-X, CENTER-Y) with r=RADIUS is left
;; transparent. p=(0,0) and r=1 will unblock the full
;; bfp. p=(.9,0). r=.1 will illuminate with a high angle from positive
;; x.

;; SIZE-X and SIZE-Z give the extend in um. the extend in the y
;; direction is set to SIZE-X.
(declaim (ftype (function (double-float double-float double-float
					&key (:x fixnum) (:y fixnum) (:z fixnum)
					(:size-x double-float)
					(:size-z double-float)
					(:wavelength double-float)
					(:numerical-aperture double-float)
					(:immersion-index double-float)
					(:integrand-evaluations fixnum))
			  (values (simple-array (complex double-float) 3)))
		angular-psf))
(defun angular-psf (center-x center-y radius &key
		    (x 64) (y 64) (z 64) (size-x 12d0)
		    (size-z 12d0)
		    (wavelength .480d0)
		    (numerical-aperture 1.38d0)
		    (immersion-index 1.515d0)
		    (integrand-evaluations 31))
  (let* ((dx (/ x size-x)) ;; pixels/um
	 )
    ;; calculate the electric field in focus
    (multiple-value-bind (e0 e1 e2)
	(psf:electric-field-psf z y x size-z size-x
				:numerical-aperture numerical-aperture
				:immersion-index immersion-index
				:wavelength wavelength
				:integrand-evaluations integrand-evaluations)
      (write-pgm (normalize-img (cross-section-xz e0))
		 "/home/martin/tmp/cut-asf-e0-no.pgm")
      ;; k0, k1 and k2 contain sections of the k-sphere
      (let* ((k0 (fftshift3 (ft3 e0)))
	     (k1 (fftshift3 (ft3 e1)))
	     (k2 (fftshift3 (ft3 e2)))
	     ;; radius of the circular border of the cut k-sphere
	     ;; sin(phi)=r/k, k=2pi/lambda, sin(phi)=NA/n -> r=k*sin(phi)
	     (sinphi (/ numerical-aperture immersion-index))
	     (klen (/ (* 2d0 pi) wavelength))
	     (bfp-radius (* klen sinphi))
	     (bfp-radius-pixels (* bfp-radius (/ dx (* 2d0 (sqrt 2d0)))))
	     ;; wiil contain distance to center-x,center-y
	     (rad-a (make-array (list y x)
				:element-type 'double-float)))
	(defparameter *efield-k* (list k0 k1 k2))
	(write-pgm (normalize-img (cross-section-xz k0))
		   "/home/martin/tmp/cut-e0-no.pgm")
	;; calculate distances to center point for all points in one slice
	(do-rectangle (j i 0 y 0 x)
	  (let* ((ii (- i (floor x 2) (* center-x bfp-radius-pixels)))
		 (jj (- j (floor y 2) (* center-y bfp-radius-pixels)))
		 (radius (sqrt (+ (* 1d0 ii ii) (* jj jj)))))
	    (setf (aref rad-a j i) radius)))

	;; set fourier field to zero if outside of transparent circle
	(do-box (k j i 0 z 0 y 0 x)
	  (when (< (* radius bfp-radius-pixels) (aref rad-a j i))
	    (setf (aref k0 k j i) (complex 0d0)
		  (aref k1 k j i) (complex 0d0)
		  (aref k2 k j i) (complex 0d0))))
	(write-pgm (normalize-img (cross-section-xz k0))
		   "/home/martin/tmp/cut-e0.pgm")

	;; deteriorated electric fields
	(setf e0 (ift3 (fftshift3 k0))
	      e1 (ift3 (fftshift3 k1))
	      e2 (ift3 (fftshift3 k2)))

	;; calculate energy density of electric field
	(let ((density (make-array (array-dimensions e0)
				   :element-type '(complex double-float))))
	  (do-box (k j i 0 z 0 y 0 x)
	    (setf (aref density k j i)
		  (complex (+ (psf:abs2 (aref e0 k j i))
			      (psf:abs2 (aref e1 k j i))
			      (psf:abs2 (aref e2 k j i))))))
	  (write-pgm (normalize-img (cross-section-xz density))
		    "/home/martin/tmp/cut-intens.pgm")
	  density)))))

#+nil ;; find centers of nuclei
(time
 (multiple-value-bind (c ch dims)
       (find-centers)
   (defparameter *centers* c)
      (defparameter *center-heights* ch)
   (defparameter *dims* dims)
   (sb-ext:gc :full t)))

#+nil ;; draw the spheres
(let ((spheres
       (destructuring-bind (z y x)
	   *dims*
	 (draw-spheres 7d0 *centers* (* 5 z) y x))))
  (setf *spheres* spheres)
  (save-stack-ub8 "/home/martin/tmp/spheres" (normalize-vol *spheres*)))


#+nil ;; construct LCOS image
(let ((coord (aref *centers* 0))
      (radius 7d0)
      (slice (make-array (array-dimensions *spheres*)
			 :element-type '(complex double-float))))
  (destructuring-bind (z y x)
      (array-dimensions *spheres*)
    ;; draw only the center sphere
    (let* ((xc (vec-i-x coord))
	   (yc (vec-i-y coord))
	   (zc (* 5 (vec-i-z coord)))
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
  (save-stack-ub8 "/home/martin/tmp/slice" (convert-vol slice))
  #+nil (write-pgm (normalize-img (cross-section-xz slice (* 5 (vec-i-z coord))))
	     "/home/martin/tmp/cut-intens2.pgm"))

(defparameter *bfp-circ-radius* .5d0)
(defparameter *bfp-circ-center-x* (- .999d0 *bfp-circ-radius*))

#+nil
(time
 (progn
  (angular-psf *bfp-circ-center-x* 0d0 *bfp-circ-radius* :x 200 :y 200 :z 200
	       :size-x (* 200 .2d0) :size-z (* 200 .2d0)
	       :integrand-evaluations 160)
  nil))

#+nil ;; light distribution in the specimen
;; default resolution is isotropic 12 um /64 = 187.5 nm/pixel
(time
 (let* ((radius *bfp-circ-radius*)
	(x *bfp-circ-center-x*)
	(xx 300)
	(yy xx)
	(zz 300)
	(dx .2d0)
	(psf (angular-psf x 0d0 radius :x xx :y yy :z zz
			  :size-x (* xx dx) :size-z (* zz dx)
			  :integrand-evaluations 190))
	(dims (destructuring-bind (z y x)
		  *dims*
		(list (* z 5) y x)))
	#+nil (psf-big (make-array dims
			     :element-type '(complex double-float))))
   #+nil   (destructuring-bind (z y x)
	       dims
	     (let ((ox (- (floor x 2) (floor xx 2)))
		   (oy (- (floor y 2) (floor yy 2)))
		   (oz (- (floor z 2) (floor zz 2))))
	       (do-box (k j i 0 zz 0 yy 0 xx)
		 (setf (aref psf-big (+ oz k) (+ oy j) (+ ox i))
		       (aref psf k j i)))))
   #+nil (defparameter *psf-big* psf-big)
   #+nil (save-stack-ub8 "/home/martin/tmp/psf-big" (normalize-vol psf-big))
   (defparameter *psf* psf)
   (sb-ext:gc :full t)
   (defparameter *slice-x-psf* (convolve3 *slice* psf))
   (sb-ext:gc :full t)))

#+nil
(save-stack-ub8 "/home/martin/tmp/psf" (normalize-vol *psf*))


;;                                                   /-
;;  	     ---			      /---
;;  	        \-----			  /---
;;  		      \-----	      /---
;;  		            \---- /---
;;  			      /--------
;;  			  /---         \----
;;  		      /---  		    \-----
;;  		  /---  			  \-----
;;  		-- 					\--




#+nil ;; draw lines into the light distribution in the specimen
(destructuring-bind (z y x)
    (array-dimensions *slice-x-psf*)
  (let ((coord (elt *centers* 0))
	(vol (normalize-vol *slice-x-psf*))
	(dx 2.d-4))
    (loop for pos in (list (list (* (- (floor x 2) (- (vec-i-x coord) 7)) dx)
				 (* (- (floor y 2) (vec-i-y coord)) dx))
			   (list (* (- (floor x 2) (+ (vec-i-x coord) 7)) dx)
				 (* (- (floor y 2) (vec-i-y coord)) dx))) do
	 (loop for angle in (list 0d0
				  -.4d0
				  (- (- *bfp-circ-center-x* *bfp-circ-radius*))
				  (- (+ *bfp-circ-center-x* *bfp-circ-radius*))) do
	      (draw-ray-into-vol (first pos) (second pos)
				 angle 0d0
				 vol
				 :shift-z (- (* 5d0 (vec-i-z coord))
					     (floor z 2)))))
    (save-stack-ub8 "/home/martin/tmp/slice-x-psf" vol)))


#+nil ;; draw lines into the volume of the psf
(destructuring-bind (z y x)
    (array-dimensions *psf-big*)
  (let ((vol (normalize-vol *psf-big*))
	(dx 2.d-4))
    (draw-ray-into-vol 0d0 0d0
		       (- (- 1d0 1d-4)) 0d0
		       vol
		       :center-z (floor z 2))
    (draw-ray-into-vol 0d0 0d0
		       -.7d0 0d0 vol
		       :center-z (floor z 2))
    (draw-ray-into-vol 0d0 0d0
		       -.4d0 0d0 vol
		       :center-z (floor z 2))
    (draw-ray-into-vol 0d0 0d0
		       0d0 0d0 vol
		       :center-z (floor z 2))
    (save-stack-ub8 "/home/martin/tmp/psf-big" vol)))


#+nil ;; excited fluorophores
(progn
  (setf *slice-x-psf-times-spheres* (.* *spheres* *slice-x-psf*))
  (save-stack-ub8 "/home/martin/tmp/slice-x-psf-times-spheres"
		 (normalize-vol *slice-x-psf-times-spheres*)))


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
			     :element-type '(complex double-float))))
   (setf *psf-big* psf-big)
   (destructuring-bind (z y x)
       dims
     (let ((ox (- (floor x 2) (floor xx 2)))
	   (oy (- (floor y 2) (floor yy 2)))
	   (oz (- (floor z 2) (floor zz 2))))
       (do-box (k j i 0 zz 0 yy 0 xx)
	 (setf (aref psf-big (+ oz k) (+ oy j) (+ ox i))
	       (aref psf k j i)))))
   (save-stack-ub8 "/home/martin/tmp/psf-detect-big" (normalize-vol psf-big))
   (sb-ext:gc :full t)
   (defparameter *camera-volume* (convolve3-circ *slice-x-psf-times-spheres*
						 (fftshift3 psf-big)))
   (save-stack-ub8 "/home/martin/tmp/camera-volume"
		   (normalize-vol *camera-volume*))
   (sb-ext:gc :full t)))



#+nil ;; check convolution
(time
 (let ((a (make-array (list 64 64 64)
		      :element-type '(complex double-float)))
       (b (psf:intensity-psf 64 64 64 20d0 20d0) #+nil (make-array (list 64 64 64)
		      :element-type '(complex double-float))))
   (setf (aref a 12 12 12) (complex 255d0))
#+nil   (setf (aref b 0 0 0) (complex 255d0))
   (save-stack-ub8 "/home/martin/tmp/conv-test" (normalize-vol (convolve3-circ a (fftshift3 b))))))


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
(save-stack-ub8 "/home/martin/tmp/spheres" (normalize-vol *spheres*))

#+nil
(let ((sli (make-array (array-dimensions *spheres*)
		       :element-type '(complex double-float))))
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
  (save-stack-ub8 "/home/martin/tmp/spheres" (normalize-vol *sli*)))



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
   (write-pgm (normalize-img (cross-section-xz (fftshift3 (ft3 psf))))
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
			  &key (dx-mm .2d-3)
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
	  (start (lens:v (* bfp-ratio-x bfp-radius)
			 (* bfp-ratio-y bfp-radius)
			 f))
	  (dx dx-mm)
	  (cz (* .5d0 z)) ;; position that is in the center of front focal plane
	  (cy (* .5d0 y))
	  (cx (* .5d0 x))
	  (nf (* ri f)))
     (macrolet ((plane (direction position)
		  ;; for defining a plane that is perpendicular to an
		  ;; axis and crosses it at POSITION
		  (declare (type (member :x :y :z) direction))
		  (let* ((normal (ecase direction
				   (:x (lens:v 1d0))
				   (:y (lens:v 0d0 1d0))
				   (:z (lens:v 0d0 0d0 1d0)))))
		    `(let* ((pos ,position)
			    (center (lens::v* ,normal pos))
			    (outer-normal (lens::normalize center)))
		       (declare (type double-float pos))
		       (lens::make-disk :normal outer-normal :center center)))))
       ;; define the borders of the viewing volume, distances in mm
       (let ((p+z (plane :z (- (* dx (- z cz))
			       nf)))
	     (p-z (plane :z (- (* dx (- (- z cz)))
			       nf)))
	     (p+y (plane :y (* dx (- y cy))))
	     (p-y (plane :y (* dx (- (- y cy)))))
	     (p+x (plane :x (* dx (- x cx))))
	     (p-x (plane :x (* dx (- (- x cx))))))
	 (multiple-value-bind (ro s)
	     (lens:thin-objective-ray obj
				      start
				      (lens::v* (lens:v (* (cos phi) (sin theta))
							(* (sin phi) (sin theta))
							(cos theta))
						-1d0))
	   (setf s (lens::v+ s (lens:v 0d0 0d0 (* dx shift-z))))
	   (let* ((nro (lens::normalize ro)))
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
			     (declare (type (or null lens::vec) h))
			     (when h
			       (make-vec-i
				:z (floor (+ cz (/ (+ (aref h 2) nf) dx)))
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

(declaim (ftype (function (fixnum fixnum fixnum
			   fixnum fixnum fixnum
			   (simple-array (unsigned-byte 8) 3))
			  (values (simple-array (unsigned-byte 8) 3) &optional))
		scan-convert-line3x scan-convert-line3y scan-convert-line3z))
;; 1986 kaufman
(defun scan-convert-line3x (x1 y1 z1 x2 y2 z2 vol)
  ;; x2 - x1 has to be the biggest difference between endpoint
  ;; coordinates
  (let* ((x x1)
	 (delx (- x2 x1))
	 ;; initialization for y
	 (y y1)
	 (ddy (- y2 y1))
	 (dely (abs ddy))
	 (ysign (signum ddy))
	 (dy (- (* 2 dely) delx)) ;; decision variable along y
	 (yinc1 (* 2 dely)) ;; increment along y for dy<0
	 (yinc2 (* 2 (- dely delx))) ;; inc along y for dy>=0
	 ;; same initialization for z
	 (z z1)
	 (ddz (- z2 z1))
	 (delz (abs ddz))
	 (zsign (signum ddz))
	 (dz (- (* 2 delz) delx))
	 (zinc1 (* 2 delz))
	 (zinc2 (* 2 (- delz delx))))
    (when (<= delx 0)
	(error "x2 is <= x1."))
    (setf (aref vol z y x) 255)
    (loop while (< x x2) do
	 (incf x) ;; step in x
	 (if (< dy 0) ;; then no change in y
	     (incf dy yinc1) ;; update dy
	     (progn
	       (incf dy yinc2) ;; update dy, and
	       (incf y ysign)));; increment/decrement y

	 (if (< dz 0)
	     (incf dz zinc1)
	     (progn
	       (incf dz zinc2)
	       (incf z zsign)))
	 (setf (aref vol z y x) 255)))
  vol)
;; start from scan-convert-line3x and replace x->$, y->x, $->y
(defun scan-convert-line3y (x1 y1 z1 x2 y2 z2 vol)
  (let* ((y y1)
	 (dely (- y2 y1))
	 (x x1)
	 (ddx (- x2 x1))
	 (delx (abs ddx))
	 (xsign (signum ddx))
	 (dx (- (* 2 delx) dely))
	 (xinc1 (* 2 delx))
	 (xinc2 (* 2 (- delx dely)))
	 (z z1)
	 (ddz (- z2 z1))
	 (delz (abs ddz))
	 (zsign (signum ddz))
	 (dz (- (* 2 delz) dely))
	 (zinc1 (* 2 delz))
	 (zinc2 (* 2 (- delz dely))))
    (when (<= dely 0)
	(error "y2 is <= y1."))
    (setf (aref vol z y x) 255)
    (loop while (< y y2) do
	 (incf y)
	 (if (< dx 0)
	     (incf dx xinc1)
	     (progn
	       (incf dx xinc2)
	       (incf x xsign)))
	 (if (< dz 0)
	     (incf dz zinc1)
	     (progn
	       (incf dz zinc2)
	       (incf z zsign)))
	 (setf (aref vol z y x) 255)))
  vol)
;; replace x->$, z->x, $->z
(defun scan-convert-line3z (x1 y1 z1 x2 y2 z2 vol)
  (let* ((z z1)
	 (delz (- z2 z1))
	 (y y1)
	 (ddy (- y2 y1))
	 (dely (abs ddy))
	 (ysign (signum ddy))
	 (dy (- (* 2 dely) delz))
	 (yinc1 (* 2 dely))
	 (yinc2 (* 2 (- dely delz)))
	 (x x1)
	 (ddx (- x2 x1))
	 (delx (abs ddx))
	 (xsign (signum ddx))
	 (dx (- (* 2 delx) delz))
	 (xinc1 (* 2 delx))
	 (xinc2 (* 2 (- delx delz))))
    (when (<= delz 0)
	(error "z2 is <= z1."))
    (setf (aref vol z y x) 255)
    (loop while (< z z2) do
	 (incf z)
	 (if (< dy 0)
	     (incf dy yinc1)
	     (progn
	       (incf dy yinc2)
	       (incf y ysign)))

	 (if (< dx 0)
	     (incf dx xinc1)
	     (progn
	       (incf dx xinc2)
	       (incf x xsign)))
	 (setf (aref vol z y x) 255)))
  vol)

(declaim (ftype (function (vec-i vec-i (simple-array (unsigned-byte 8) 3))
			  (values (simple-array (unsigned-byte 8) 3) &optional))
		scan-convert-line3))
(defun scan-convert-line3 (start end vol)
  (let* ((diff (v--i end start))
	 (ls (list (list (vec-i-x diff) 2)
		   (list (vec-i-y diff) 1)
		   (list (vec-i-z diff) 0)))
	 (diffa (mapcar #'(lambda (e) (list (abs (first e))
				       (second e))) ls))
	 ;; find the direction with the biggest difference
	 (sorted-diff-a (sort diffa #'> :key #'car))
	 (main-direction (second (first sorted-diff-a))) ;; 2 corresponds to x, 1->y, 0->z
	 ;; find the order in which to deliver the points
	 (main-diff (aref diff main-direction))
	 ;; we have to swap the points when main-diff is negative
	 (swap-points? (< main-diff 0))
	 ;; create the function name to dispatch to
	 (function (intern (format nil "SCAN-CONVERT-LINE3~a" (ecase main-direction
								(2 'X)
								(1 'Y)
								(0 'Z))))))
    (when (eq 0 main-diff)
      (error "start and end point are the same."))
    (if swap-points?
	(funcall function
		 (vec-i-x end)
		 (vec-i-y end)
		 (vec-i-z end)
		 (vec-i-x start)
		 (vec-i-y start)
		 (vec-i-z start)
		 vol)
	(funcall function
		 (vec-i-x start)
		 (vec-i-y start)
		 (vec-i-z start)
		 (vec-i-x end)
		 (vec-i-y end)
		 (vec-i-z end)
		 vol))))


#+nil
(time
 (let ((vol (make-array (list 128 128 128) :element-type '(unsigned-byte 8))))
   (save-stack-ub8 "/home/martin/tmp/line"
		   (scan-convert-line3 (make-vec-i :x 0 :y 0 :z 0)
				       (make-vec-i :x 120 :y 127 :z 127)
				       vol))))

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

#+nil
(time
 ;; changing z,y,x without touching dx or dz leaves the area that is
 ;; shown in k space constant
 (let* ((z 64)
	(y 64)
	(x y)
	(na 1.38d0)
	(ri 1.515d0)
	(lambd .480d0)
	(dz (* .5d0 lambd))
	(dz2 (* dz (/ 2d0 (- 1d0 (sqrt (- 1d0
					  (let ((sinphi (/ na ri)))
					    (* sinphi sinphi))))))))
	(dx (* dz (/ ri na)))
	(dx2 (* .5 dx)))
   (format t "~a~%" (list 'aspect dx2 dz2 (/ dx2 dz2) (/ dz2 dx2)))
   (multiple-value-bind (e0 e1 e2)
       (psf:electric-field-psf z y x (* z dz2) (* x dx2)
			       :integrand-evaluations 140)
     (write-pgm (normalize-img (cross-section-xz e0))
		"/home/martin/tmp/cut-psf.pgm")
     (let ((k0 (fftshift3 (ft3 e0)))
	   (k1 (fftshift3 (ft3 e1)))
	   (k2 (fftshift3 (ft3 e2))))
       (write-pgm (normalize-img (cross-section-xz k0))
		  "/home/martin/tmp/cut-psf-k.pgm")
       (let* ((cr .3d0)
	      (cx (- .5d0 cr))
	      (cy .0d0)
	      (cr2 (* cr cr))
	      (mul0 (make-array (array-dimensions k0)
				 :element-type '(complex double-float)))
	      (mul1 (make-array (array-dimensions k0)
				 :element-type '(complex double-float)))
	      (mul2 (make-array (array-dimensions k0)
				:element-type '(complex double-float)))
	      (mul (make-array (list y x)
			      :element-type 'double-float)))
	 (do-rectangle (j i 0 y 0 x)
	   (let* ((xx (- (* 4d0 (- (* i (/ 1d0 x)) .5d0)) cx))
		  (yy (- (* 4d0 (- (* j (/ 1d0 y)) .5d0)) cy))
		  (r2 (+ (* xx xx) (* yy yy))))
	     (when (< r2 cr2)
	       (setf (aref mul j i) 1d0))))
	 (do-box (k j i 0 z 0 y 0 x)
	   (setf (aref mul0 k j i) (* (aref k0 k j i) (aref mul j i))
		 (aref mul1 k j i) (* (aref k1 k j i) (aref mul j i))
		 (aref mul2 k j i) (* (aref k2 k j i) (aref mul j i))))
	 (write-pgm (normalize-img (cross-section-xz mul0))
		    "/home/martin/tmp/cut-psf-k-mul.pgm")
	 (let* ((em0 (ift3 (fftshift3 mul0)))
		(em1 (ift3 (fftshift3 mul1)))
		(em2 (ift3 (fftshift3 mul2)))
		(intens (make-array (array-dimensions e0)
				   :element-type '(complex double-float))))
	   (do-box (k j i 0 z 0 y 0 x)
	     (setf (aref intens k j i)
		   (+ (* (aref em0 k j i) (conjugate (aref em0 k j i)))
		      (* (aref em1 k j i) (conjugate (aref em1 k j i)))
		      (* (aref em2 k j i) (conjugate (aref em2 k j i))))))
	   (write-pgm (normalize-img (cross-section-xz intens))
		      "/home/martin/tmp/cut-psf-intens.pgm")
	   (let ((k (fftshift3 (ft3 intens))))
	     (write-pgm (normalize-img (cross-section-xz k))
			"/home/martin/tmp/cut-psf-intk.pgm"))))))))