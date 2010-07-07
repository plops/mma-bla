(defpackage :vol
  (:use :cl :sb-alien :sb-c-call)
  (:export
   #:fftshift3
   #:ft3
   #:ift3
   #:read-pgm
   #:write-pgm
   #:histogram
   #:get-linear-array
   #:read-stack
   #:square
   #:linear-regression
   #:clamp
   #:interp1
   #:interpolate2

   #:+forward+
   #:+backward+
   #:+estimate+
   
   #:do-rectangle
   #:do-box
   #:with-slice
   
   #:save-stack
   #:save-stack-ub8
   #:.*
   #:.+
   #:s*
   #:convolve3-circ
   #:convolve3

   #:convert-vol
   #:convert-img
   #:resample-half
   #:cross-section-xz
   #:normalize-img
   #:normalize-vol))

;; for i in `cat vol.lisp|grep defconst|cut -d " " -f 2`;do echo \#:$i ;done

(in-package :vol)

#+nil (declaim (optimize (speed 3) (debug 1) (safety 1)))
(declaim (optimize (speed 2) (debug 3) (safety 3)))
 
(load-shared-object "/usr/lib/libfftw3.so.3")
 
(define-alien-routine ("fftw_plan_dft_3d" plan-dft-3d)
    (* int)
  (n0 int)
  (n1 int)
  (n2 int)
  (in (* double-float))
  (out (* double-float))
  (sign int)
  (flags unsigned-int))
 
;; C-standard "row-major" order, so that the last dimension has the
;; fastest-varying index in the array.
 

 
(defmacro do-rectangle ((j i ymin ymax xmin xmax) &body body)
  "Loop through 2d points in ymin .. ymax-1. e.g. call with 0 y 0 x to
visit every point."
  `(loop for ,j from ,ymin below ,ymax do
	(loop for ,i from ,xmin below ,xmax do
	     (progn ,@body))))

(defmacro do-box ((k j i zmin zmax ymin ymax xmin xmax) &body body)
  "Loop through 3d points e.g. call with 0 z 0 y 0 x to visit every
point."
  `(loop for ,k from ,zmin below ,zmax do
	(loop for ,j from ,ymin below ,ymax do
	     (loop for ,i from ,xmin below ,xmax do
		  (progn ,@body)))))
 
 
;; fftshift3 /home/martin/usb/y2009/1123/1.lisp
(declaim (ftype (function ((simple-array (complex double-float) (* * *)))
			  (values (simple-array (complex double-float) (* * *))
				  &optional))
		fftshift3))
(defun fftshift3 (in)
  (let ((out (make-array (array-dimensions in)
			 :element-type '(complex double-float))))
   (destructuring-bind (w2 w1 w0)
       (array-dimensions in)
     (dotimes (k w2) 
       (dotimes (j w1)
	 (dotimes (i w0)
	   (let* ((ii (if (> i (/ w0 2))
			  (+ w0 (/ w0 2) (- i))
			  (- (/ w0 2) i)))
		  (jj (if (> j (/ w1 2))
			  (+ w1 (/ w1 2) (- j))
			  (- (/ w1 2) j)))
		  (kk (if (> k (/ w2 2))
			  (+ w2 (/ w2 2) (- k))
			  (- (/ w2 2) k))))
	     (setf (aref out k j i)
		   (aref in kk jj ii))
	     nil)))))
   out))
 
(define-alien-routine ("fftw_execute" execute)
    void
  (plan (* int)))
 
(defconstant +forward+ 1)
(defconstant +backward+ -1)
(defconstant +estimate+ (ash 1 6))
 
(declaim (ftype (function ((simple-array (complex double-float) (* * *))
			   &key (:forward boolean))
			  (values (simple-array (complex double-float) (* * *))
				  &optional))
		ft3))
(defun ft3 (in &key (forward t))
  (let ((dims (array-dimensions in)))
   (destructuring-bind (z y x)
       dims
     (let* ((out (make-array dims :element-type '(complex double-float))))
       (sb-sys:with-pinned-objects (in out)
	 (let ((p (plan-dft-3d z y x
			       (sb-sys:vector-sap 
				(sb-ext:array-storage-vector in))
			       (sb-sys:vector-sap 
				(sb-ext:array-storage-vector out))
			       (if forward
				   +forward+
				   +backward+)
			       +estimate+)))
	   (execute p)))
       (when forward ;; normalize if forward
	 (let ((1/n (/ 1d0 (* x y z))))
	  (do-box (k j i 0 z 0 y 0 x)
	    (setf (aref out k j i) (* 1/n (aref out k j i))))))
       out))))

(defmacro ift3 (in)
  `(ft3 ,in :forward nil))
 
(declaim 
 (type (function 
	(string) 
	(values (simple-array (unsigned-byte 8) (* *))
		&optional))
       read-pgm))
(defun read-pgm (filename)
  (with-open-file (s filename)
    (unless (equal (symbol-name (read s)) "P5")
      (error "no PGM file"))
    (let* ((w (read s))
	   (h (read s))
	   (grays (read s))
	   (pos (file-position s))
	   (data (make-array 
		  (list h w)
		  :element-type '(unsigned-byte 8)))
	   (data-1d (make-array 
		     (* h w)
		     :element-type '(unsigned-byte 8)
		     :displaced-to data)))
      (declare ((simple-array (unsigned-byte 8) (* *)) data)
	       ((array (unsigned-byte 8) (*)) data-1d)
	       ((integer 0 65535) grays w h))
      (unless (= grays 255)
	(error "image has wrong bitdepth"))
      (with-open-file (s filename
			 :element-type '(unsigned-byte 8))
	(file-position s pos)
	(read-sequence data-1d s))
      data)))
 
(declaim
 (ftype (function
	 ((array (unsigned-byte 8) 2)
	  string) 
	 (values null &optional))
	write-pgm))
(defun write-pgm (img filename)
  (destructuring-bind (h w)
      (array-dimensions img)
    (declare ((integer 0 65535) w h))
    (with-open-file (s filename
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%255~%" w h))
    (with-open-file (s filename 
		       :element-type '(unsigned-byte 8)
		       :direction :output
		       :if-exists :append)
      (let ((data-1d (make-array 
		      (* h w)
		      :element-type '(unsigned-byte 8)
		      :displaced-to img)))
	(write-sequence data-1d s)))
    nil))
 
(declaim
 (ftype (function
	 ((array (unsigned-byte 8) (*)) &optional fixnum) 
	 (values (simple-array fixnum (*)) (unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 8)
		 &optional))
	histogram))
(defun histogram (img &optional (bins 30))
  (let* ((maxval (aref img 0))
	 (minval maxval)
	 (w (length img)))
    (dotimes (i w)
      (let ((v (aref img i)))
	(when (< v minval)
	  (setf minval v))
	(when (< maxval v)
	  (setf maxval v))))
    (let* ((len (1+ (- maxval minval)))
	   (result (make-array len :element-type 'fixnum))
	   (binsm (min (- maxval minval) bins)))
      (when (eq maxval minval)
	#+nil (error "data is too boring.")
	(return-from histogram (values result minval maxval binsm)))
      (dotimes (i w)
	(incf (aref result (floor (* binsm
				     (- (aref img i)
					minval))
				  (- maxval minval)))))
      (values result minval maxval binsm))))

(declaim (ftype (function ((array * (* *)))
			  (values (simple-array * (*)) &optional))
		get-linear-array))
(defun get-linear-array (img)
  (sb-ext:array-storage-vector img)
  #+nil (make-array (* (array-dimension img 0)
		 (array-dimension img 1))
	      :element-type (array-element-type img)
	      :displaced-to img))
 
 
(declaim (ftype (function (string)
			  (values (simple-array (unsigned-byte 8) (* * *))
				  &optional))
		read-stack))
(defun read-stack (fn)
  (let* ((files (directory fn))
	 (slices (length files))
	 (a (read-pgm (first files))))
    (destructuring-bind (h w)
	(array-dimensions a)
      (let* ((result (make-array (list slices h w)
				 :element-type '(unsigned-byte 8))))
	(dotimes (k slices)
	  (let* ((a (read-pgm (elt files k))))
	    (dotimes (j h)
	      (dotimes (i w)
		(setf (aref result k j i)
		      (aref a j i))))))
	result))))
 
(defmacro with-slice ((slice-array array slice-nr) &body body)
  "Returns SLICE-NRth slice of ARRAY as the 2D SLICE-ARRAY."
  (let ((x (gensym))
	(y (gensym))
	(z (gensym)))
    `(destructuring-bind (,z ,y ,x)
	(array-dimensions ,array)
      (when (or (< ,slice-nr 0) (<= ,z ,slice-nr))
	(error "slice-nr=~d out of range [0,~d]" ,slice-nr (1- ,z)))
      (let* ((,slice-array (make-array (list ,y ,x)
				       :element-type '(unsigned-byte 8)
				       :displaced-to ,array
				       :displaced-index-offset (* ,slice-nr ,x ,y))))
	,@body))))
 
(declaim (ftype (function (double-float) (values double-float &optional))
		square))
(defun square (x)
  (* x x))
 
(declaim (ftype (function ((array double-float *)
			   &optional
			   (array double-float *))
			  (values double-float double-float
				  double-float double-float &optional))
		linear-regression))
;; chernov/book.pdf p. 20
(defun linear-regression (y &optional
			  (x (let* ((n (length y)))
			       (make-array 
				n
				:element-type 'double-float
				:initial-contents 
				(loop for i below n collect (* 1d0 i))))))
  "Linear regression of the values in Y with the function y=a*x+b. If
X isn't supplied its assumed to be 0,1,2, ... . Returned are the
fitting parameters A and B and their errors DELTA_A and DELTA_B."
  (let* ((n (length y))
	 (xmean (/ (loop for xi across x sum xi) n))
	 (ymean (/ (loop for xi across y sum xi) n))
	 (sxx (loop for xi across x sum (square (- xi xmean))))
	#+nil (syy (loop for xi across y sum (square (- xi ymean))))
	 (sxy (loop for i below n sum (* (- (aref x i) xmean)
					 (- (aref y i) ymean))))
	 (bhat (/ sxy sxx))
	 (ahat (- ymean (* bhat xmean)))
	 (var (/ (loop for i below n sum (square (- (aref y i) ahat
						       (* bhat (aref x i)))))
		    (- n 2)))
	 (vara (* var (+ (/ (square xmean)
			    sxx)
			 (/ n))))
	 (varb (/ var sxx)))
    (values ahat bhat (sqrt vara) (sqrt varb))))
 
#+nil
(linear-regression (let* ((ll (list 1d0 2.01d0 3d0 4d0)))
		     (make-array (length ll)
		      :element-type 'double-float
		      :initial-contents ll)))
 
(declaim (ftype (function (integer)
			  (values (unsigned-byte 8) &optional))
		clamp))
(defun clamp (a)
  (when (< a 0)
    (return-from clamp 0))
  (when (< 255 a)
    (return-from clamp 255))
  a)
 

(declaim (ftype (function ((simple-array (unsigned-byte 8) 2)
			   fixnum fixnum)
			  (values (unsigned-byte 8) &optional))
		aref2-zero-ub8))
(defun aref2-zero-ub8 (a j i)
  "Like AREF but return zero if subscripts J and I point outside of
the array bounds."
  (if (array-in-bounds-p a j i)
      (aref a j i)
      0))

(declaim (ftype (function ((simple-array double-float 2)
			   fixnum fixnum)
			  (values double-float &optional))
		aref2-zero-df))
(defun aref2-zero-df (a j i)
  "Like AREF but return zero if subscripts J and I point outside of
the array bounds."
  (if (array-in-bounds-p a j i)
      (aref a j i)
      0d0))

(declaim (ftype (function ((simple-array (complex double-float) 2)
			   fixnum fixnum)
			  (values (complex double-float) &optional))
		aref2-zero-cdf))
(defun aref2-zero-cdf (a j i)
  "Like AREF but return zero if subscripts J and I point outside of
the array bounds."
  (if (array-in-bounds-p a j i)
      (aref a j i)
      (complex 0d0)))


(declaim (ftype (function ((unsigned-byte 8)
			   (unsigned-byte 8)
			   double-float)
			  (values double-float &optional))
		interp1-ub8)
	 (inline interp1-ub8))
(defun interp1-ub8 (a b xi)
    "Interpolate between values A and B. Returns A for xi=0 and B for
  xi=1."
  (+ (* (- 1d0 xi) a) (* xi b)))

(declaim (ftype (function (double-float double-float double-float)
			  (values double-float &optional))
		interp1-df)
	 (inline interp1-df))
(defun interp1-df (a b xi)
    "Interpolate between values A and B. Returns A for xi=0 and B for
  xi=1."
  (+ (* (- 1d0 xi) a) (* xi b)))

(declaim (ftype (function ((complex double-float) 
			   (complex double-float) double-float)
			  (values (complex double-float) &optional))
		interp1-cdf)
	 (inline interp1-cdf))
(defun interp1-cdf (a b xi)
    "Interpolate between values A and B. Returns A for xi=0 and B for
  xi=1."
  (+ (* (- 1d0 xi) a) (* xi b)))

(declaim (ftype (function ((or (unsigned-byte 8)
			       double-float
			       (complex double-float))
			   (or (unsigned-byte 8)
			       double-float
			       (complex double-float))
			   double-float)
			  (values (or double-float
				      (complex double-float)) &optional))
		interp1)
	 (inline interp1))
(defun interp1 (a b xi)
  (etypecase a
    ((unsigned-byte 8) (interp1-ub8 a b xi))
    (double-float (interp1-df a b xi))
    ((complex double-float) (interp1-cdf a b xi))))

(declaim (ftype (function ((simple-array (unsigned-byte 8) 2)
			   double-float double-float)
			  (values double-float &optional))
		interpolate2-ub8))
(defun interpolate2-ub8 (img x y) 
  "Bilinear interpolation on an image IMG. The coordinates X and Y can
be floating point values. If they point outside of IMG 0 is returned."
  (multiple-value-bind (i ix)
      (floor x)
    (multiple-value-bind (j jx)
	(floor y)
      ;; values on grid points, top left is i,j and stored in a
      ;; |
      ;;-a-b
      ;; |
      ;; c d
      (let ((a (aref img i j))
	    (b (aref2-zero-ub8 img (1+ i) j))
	    (c (aref2-zero-ub8 img i (1+ j)))
	    (d (aref2-zero-ub8 img (1+ i) (1+ j))))
	;; now interpolate verticals
	;; a  b
	;; |  |
	;; e  f
	;; |  |
	;; c  d
	(let ((e (interp1-ub8 a c jx))
	      (f (interp1-ub8 b d jx)))
	  ;; and now horizontal
	  ;; e - g - f
	  (let ((g (interp1-df e f ix)))
	    g))))))

(declaim (ftype (function ((simple-array double-float 2)
			   double-float double-float)
			  (values double-float &optional))
		interpolate2-df))
(defun interpolate2-df (img x y) 
  "Bilinear interpolation on an image IMG. The coordinates X and Y can
be floating point values. If they point outside of IMG 0 is returned."
  (multiple-value-bind (i ix)
      (floor x)
    (multiple-value-bind (j jx)
	(floor y)
      (let ((a (aref img i j))
	    (b (aref2-zero-df img (1+ i) j))
	    (c (aref2-zero-df img i (1+ j)))
	    (d (aref2-zero-df img (1+ i) (1+ j))))
	(let ((e (interp1-df a c jx))
	      (f (interp1-df b d jx)))
	  (let ((g (interp1-df e f ix)))
	    g))))))

(declaim (ftype (function ((simple-array (complex double-float) 2)
			   double-float double-float)
			  (values (complex double-float) &optional))
		interpolate2-cdf))
(defun interpolate2-cdf (img x y) 
  "Bilinear interpolation on an image IMG. The coordinates X and Y can
be floating point values. If they point outside of IMG 0 is returned."
  (multiple-value-bind (i ix)
      (floor x)
    (multiple-value-bind (j jx)
	(floor y)
      (let ((a (aref img i j))
	    (b (aref2-zero-cdf img (1+ i) j))
	    (c (aref2-zero-cdf img i (1+ j)))
	    (d (aref2-zero-cdf img (1+ i) (1+ j))))
	(let ((e (interp1-cdf a c jx))
	      (f (interp1-cdf b d jx)))
	  (let ((g (interp1-cdf e f ix)))
	    g))))))


(declaim (ftype (function ((simple-array * 2) double-float double-float)
			  (values (or double-float
				      (complex double-float)) &optional))
		interpolate2))
(defun interpolate2 (img x y)
  "Bilinear interpolation on an image IMG. The coordinates X and Y can
be floating point values. If they point outside of IMG 0 is returned."
  (etypecase img
    ((simple-array (unsigned-byte 8) 2) (interpolate2-ub8 img x y))
    ((simple-array double-float 2) (interpolate2-df img x y))
    ((simple-array (complex double-float) 2) (interpolate2-cdf img x y))))

#+nil
(let ((a (make-array (list 2 2) :element-type '(unsigned-byte 8)
		     :initial-contents '((1 2) (2 3)))))
  (interpolate2 a .5d0 .2d0)) 



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

(declaim (ftype (function ((simple-array (complex double-float) 3)
			   (simple-array (complex double-float) 3))
			  (values (simple-array (complex double-float) 3) &optional))
		.* .+))
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

(defun .+ (vola volb)
  (let ((result (make-array (array-dimensions vola)
			    :element-type (array-element-type vola))))
   (destructuring-bind (z y x)
       (array-dimensions vola)
     (do-box (k j i 0 z 0 y 0 x)
       (setf (aref result k j i)
	     (+ (aref vola k j i)
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

(declaim (ftype (function ((simple-array (complex double-float) 3))
			  (values (simple-array (complex double-float) 3) 
				  &optional))
		resample-half))
(defun resample-half (vol)
  (destructuring-bind (z y x)
      (array-dimensions vol)
    (let* ((xx (floor x 2))
	   (yy (floor y 2))
	   (zz (floor z 2))
	   (small (make-array (list zz yy xx)
			      :element-type '(complex double-float))))
      (do-box (k j i 0 zz 0 xx 0 xx)
       (setf (aref small k j i)
	     (aref vol (* k 2) (* j 2) (* i 2))))
      small)))


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
				 :element-type '(unsigned-byte 8))))
	(when (< (abs (- ma mi)) 1d-12)
	  return-from normalize-img result
	  #+nil(error "image contains constant data, can't normalize."))
	(let ((s (/ 255d0 (- ma mi))))
	 (do-rectangle (j i 0 y 0 x)
	   (setf (aref result j i)
		 (floor (* s (- (aref b j i) mi))))))
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

