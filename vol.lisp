#.(require :alexandria)
#.(require :vector)
(defpackage :vol
   (:use :cl :sb-alien :sb-c-call :vector)
   (:export
    #:fftshift2
    #:ft2
    #:ift2
    #:convolve2-circ

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
    #:.*2
    #:.+2
    #:s*2
    #:convolve3-circ
    #:convolve3-nocrop
    #:convolve3

    #:resample-half
    #:cross-section-xz

    #:convert3-cdf/df-imagpart
    #:convert3-df/ub8-floor
    #:convert2-ub8/cdf-complex
    #:convert2-cdf/df-realpart
    #:convert2-cdf/df-imagpart
    #:convert2-df/cdf-complex
    #:convert3-ub8/cdf-complex
    #:convert2-cdf/ub8-realpart
    #:convert3-cdf/df-phase
    #:convert2-cdf/ub8-abs
    #:convert3-cdf/df-realpart
    #:convert3-cdf/df-imagpart
    #:convert3-df/cdf-complex
    #:convert2-cdf/ub8-phase
    #:convert3-cdf/df-abs
    #:convert2-cdf/df-imagpart
    #:convert3-cdf/ub8-abs
    #:convert2-cdf/df-phase
    #:convert2-cdf/df-abs
    #:convert3-cdf/ub8-phase
    #:convert2-df/ub8-floor
    #:convert3-cdf/ub8-realpart
    #:convert3-cdf/ub8-imagpart

    #:normalize2-cdf/ub8-phase
    #:normalize3-cdf/ub8-phase
    #:normalize2-cdf/ub8-realpart
    #:normalize2-cdf/ub8-imagpart
    #:normalize2-df/ub8-floor
    #:normalize2-cdf/ub8-abs
    #:normalize3-df/ub8-realpart
    #:normalize3-cdf/ub8-abs
    #:normalize3-cdf/ub8-realpart
    #:normalize3-cdf/ub8-imagpart
    #:normalize3-df/ub8-floor
    #:normalize-ub8

    #:count-non-zero-ub8
    #:decimate-xy-ub8
    
    #:bbox
    #:make-bbox
    #:bbox-start
    #:bbox-end
    #:extract-bbox2-ub8
    #:replace-bbox2-ub8
    #:find-bbox2-ub8
    #:find-bbox3-ub8
    #:extract-bbox3-ub8
    #:extract-bbox3-cdf
    #:extract-bbox3-df
    #:replace-bbox3-ub8
    
    #:init-ft
    #:mean-realpart
    #:normalize2-df/df
    #:with-arrays
    #:normalize->ub8))

;; for i in `cat vol.lisp|grep defconst|cut -d " " -f 2`;do echo \#:$i ;done

(in-package :vol)

#+nil (declaim (optimize (speed 3) (debug 1) (safety 1)))
(declaim (optimize (speed 2) (debug 3) (safety 3)))
 
(load-shared-object "/usr/lib/libfftw3.so")

;; multithreading for fftw is just a matter of two initializing
;; function calls, see:
;; http://www.fftw.org/fftw3_doc/Usage-of-Multi_002dthreaded-FFTW.html#Usage-of-Multi_002dthreaded-FFTW
#+nil
(progn
(load-shared-object "/usr/lib/libfftw3_threads.so")

(define-alien-routine ("fftw_init_threads" init-threads)
    int)
(define-alien-routine ("fftw_plan_with_nthreads" plan-with-nthreads)
    void
  (nthreads int))

(defun init-ft ()
  (init-threads)
  (plan-with-nthreads 8)))

;; to clean up completely call void fftw_cleanup_threads(void)


(define-alien-routine ("fftw_execute" execute)
    void
  (plan (* int)))
 
(defconstant +forward+ 1)
(defconstant +backward+ -1)
(defconstant +estimate+ (ash 1 6))

 
(define-alien-routine ("fftw_plan_dft_3d" plan-dft-3d)
    (* int)
  (n0 int)
  (n1 int)
  (n2 int)
  (in (* double-float))
  (out (* double-float))
  (sign int)
  (flags unsigned-int))

(define-alien-routine ("fftw_plan_dft_2d" plan-dft-2d)
    (* int)
  (n0 int)
  (n1 int)
  (in (* double-float))
  (out (* double-float))
  (sign int)
  (flags unsigned-int))
 
;; C-standard "row-major" order, so that the last dimension has the
;; fastest-varying index in the array.

(defmacro with-arrays (arrays &body body)
  "Provides a corresponding accessor for each array as a local macro,
so that (ARRAY ...) corresponds to (AREF ARRAY ...)."
  `(macrolet ,(mapcar (lambda (array)
                        `(,array (&rest indices) `(aref ,',array ,@indices)))
                      arrays)
     ,@body))
 
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
 
(defun fftshift1 (in)
  (declare ((simple-array (complex double-float) 1) in)
	   (values (simple-array (complex double-float) 1) &optional))
  (let* ((n (length in))
	 (nh (floor n 2))
	 (out (make-array n
			  :element-type '(complex double-float))))
    (dotimes (i (length in))
      (let ((ii (mod (+ i nh) n)))
	(setf (aref out ii) (aref in i))))
    out))
#+nil
(let* ((ls '(1 2 3 4 5 6 7 8 9))
      (a (make-array (length ls)
		     :element-type '(complex double-float)
		     :initial-contents (mapcar
					#'(lambda (z) (coerce z 
							 '(complex double-float)))
					ls))))
  (fftshift1 a))

(defun fftshift2 (in)
  (declare ((simple-array (complex double-float) 2) in)
	   (values (simple-array (complex double-float) 2) &optional))
  (let ((out (make-array (array-dimensions in)
			 :element-type '(complex double-float))))
   (destructuring-bind (w1 w0)
       (array-dimensions in)
     (let ((wh0 (floor w0 2))
	   (wh1 (floor w1 2)))
      (do-rectangle (j i 0 w1 0 w0)
	(let* ((ii (mod (+ i wh0) w0))
	       (jj (mod (+ j wh1) w1)))
	  (setf (aref out j i)
		(aref in jj ii))))))
   out))

(defun ft2 (in &key (forward t))
  (declare ((simple-array (complex double-float) 2) in)
	   (boolean forward)
	   (values (simple-array (complex double-float) 2) &optional))
  (let ((dims (array-dimensions in)))
    (destructuring-bind (y x)
	dims
      (let* ((out (make-array dims :element-type '(complex double-float))))
	(sb-sys:with-pinned-objects (in out)
	  (let ((p (plan-dft-2d y x
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
	  (let ((1/n (/ 1d0 (* x y))))
	    (do-rectangle (j i 0 y 0 x)
	      (setf (aref out j i) (* 1/n (aref out j i))))))
	out))))

(defmacro ift2 (in)
  `(ft2 ,in :forward nil))

 
;; was originally fftshift3 /home/martin/usb/y2009/1123/1.lisp
;; now it is better and work for odd dimensions
(defun fftshift3 (in)
  (declare ((simple-array (complex double-float) 3) in)
	   (values (simple-array (complex double-float) 3) &optional))
  (let ((out (make-array (array-dimensions in)
			 :element-type '(complex double-float))))
    (destructuring-bind (w2 w1 w0)
	(array-dimensions in)
      (let ((wh0 (floor w0 2))
	    (wh1 (floor w1 2))
	    (wh2 (floor w2 2)))
	(do-box (k j i 0 w2 0 w1 0 w0)
	  (let ((ii (mod (+ i wh0) w0))
		(jj (mod (+ j wh1) w1))
		(kk (mod (+ k wh2) w2)))
	    (setf (aref out k j i)
		  (aref in kk jj ii))))))
    out))
 
 
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

(defun convolve2-circ (vola volb)
  (declare ((simple-array (complex double-float) 2) vola volb)
	   (values (simple-array (complex double-float) 2) &optional))
  (let* ((da (array-dimensions vola))
	 (db (array-dimensions volb))
	 (compare-ab (map 'list #'(lambda (x y) (eq x y)) da db)))
    (when (some #'null compare-ab)
      (error "convolve3-circ expects both input arrays to have the same dimensions."))
    (ift2 (s*2 (* 1d0 (reduce #'* da)) (.*2 (ft2 vola) (ft2 volb))))))
 
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
 
(defun write-pgm (filename img)
  (declare (simple-string filename)
	   ((array (unsigned-byte 8) 2) img)
	   (values null &optional))
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

(defmacro def-interp1 ()
  (let ((types '((ub8 (unsigned-byte 8) double-float)
		 (df double-float double-float)
		 (cdf (complex double-float) (complex double-float)))))
   `(progn
      ,@(loop for ty in types collect
	     (destructuring-bind (short long retour)
		 ty
	       (let ((fun (intern (format nil "INTERP1-~A" short))))
		 `(progn 
		   (declaim (inline ,fun))
		   (defun ,fun (a b xi)
		    "Interpolate between values A and B. Returns A for
  xi=0 and B for xi=1."
		    (declare (,long a b)
			     (double-float xi)
			     (values ,retour &optional))
		    (+ (* (- 1d0 xi) a) (* xi b))))))))))

(def-interp1)

(declaim (inline interp1))
(defun interp1 (a b xi)
  (declare ((or (unsigned-byte 8)
		double-float
		(complex double-float)) a b)
	   (double-float xi)
	   (values (or double-float (complex double-float)) &optional))
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
  ;; add a slash / if there isn't one
  (ensure-directories-exist (if (eq (1- (length fn))
					    (position #\/ fn :from-end t))
					fn
					(format nil "~a/" fn)))
  (destructuring-bind (z y x)
      (array-dimensions vol)
    (let ((b (make-array (list y x)
			 :element-type '(unsigned-byte 8))))
      (dotimes (k z)
	(do-rectangle (j i 0 y 0 x)
	  (setf (aref b j i)
		(clamp (floor (* 255 (funcall function (aref vol k j i)))))))
	(write-pgm (format nil "~a/~3,'0d.pgm" fn k) b))))
  nil)

(declaim (ftype (function (string (simple-array (unsigned-byte 8) 3))
			  (values null &optional))
		save-stack-ub8))
(defun save-stack-ub8 (fn vol)
  (ensure-directories-exist (if (eq (1- (length fn))
				    (position #\/ fn :from-end t))
				fn
				(format nil "~a/" fn)))
  (destructuring-bind (z y x)
      (array-dimensions vol)
    (let ((b (make-array (list y x)
			 :element-type '(unsigned-byte 8))))
      (dotimes (k z)
	(do-rectangle (j i 0 y 0 x)
	  (setf (aref b j i)
		(aref vol k j i)))
	(write-pgm (format nil "~a/~3,'0d.pgm" fn k) b))))
  nil)

(defun .*2 (vola volb)
  (declare ((simple-array (complex double-float) 2) vola volb)
	   (values (simple-array (complex double-float) 2) &optional))
  (let ((result (make-array (array-dimensions vola)
			    :element-type (array-element-type vola))))
   (destructuring-bind (y x)
       (array-dimensions vola)
     (do-rectangle (j i 0 y 0 x)
       (setf (aref result j i)
	     (* (aref vola j i)
		(aref volb j i)))))
   result))

(defun .+2 (vola volb)
  (declare ((simple-array (complex double-float) 2) vola volb)
	   (values (simple-array (complex double-float) 2) &optional))
  (let ((result (make-array (array-dimensions vola)
			    :element-type (array-element-type vola))))
   (destructuring-bind (y x)
       (array-dimensions vola)
     (do-rectangle (j i 0 y 0 x)
       (setf (aref result j i)
	     (+ (aref vola j i)
		(aref volb j i)))))
   result))

(defun .* (vola volb &optional volb-start)
  "Elementwise multiplication of VOLA and VOLB. Both volumes must have
the same dimensions or VOLB must be smaller in all dimensions. In the
latter case a vec-i has to be supplied in VOLB-START to define the
relative position of VOLB inside VOLA."
  (declare ((simple-array (complex double-float) 3) vola volb)
	   ((or null vec-i) volb-start)
	   (values (simple-array (complex double-float) 3) &optional))
  (let ((result (make-array (array-dimensions volb)
			    :element-type '(complex double-float))))
   (destructuring-bind (z y x)
       (array-dimensions vola)
     (destructuring-bind (zz yy xx)
	 (array-dimensions volb)
       (if volb-start
	   ;; fill the result with volb multiplied by the
	   ;; corresponding values from the bigger vola
	   (let ((sx (vec-i-x volb-start))
		 (sy (vec-i-y volb-start))
		 (sz (vec-i-z volb-start)))
	     (unless (and (<= zz (+ z sz))
			  (<= yy (+ y sy))
			  (<= xx (+ x sx)))
	       (error "VOLB isn't contained in VOLA when shifted by VOLB-START. ~a" 
		      (list zz (+ z sz))))
	     (do-box (k j i 0 zz 0 yy 0 xx)
	       (setf (aref result k j i)
		     (* (aref volb k j i)
			(aref vola (+ k sz) (+ j sy) (+ i sx))))))
	   (progn 
	     (unless (and (= z zz) (= y yy) (= x xx))
	       (error "volumes don't have the same size, maybe you can supply a start vector."))
	     (do-box (k j i 0 z 0 y 0 x)
	       (setf (aref result k j i)
		     (* (aref vola k j i)
			(aref volb k j i))))))))
   result))

(defun .+ (vola volb)
  (declare ((simple-array (complex double-float) 3) vola volb)
	   (values (simple-array (complex double-float) 3) &optional))
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

(defun s*2 (s vol)
  (declare (double-float s)
	   ((simple-array (complex double-float) 2) vol)
	   (values (simple-array (complex double-float) 2) &optional))
  (let* ((a (sb-ext:array-storage-vector vol))
	 (n (length a)))
    (dotimes (i n)
      (setf (aref a i) (* s (aref a i)))))
  vol)

(defun convolve3-circ (vola volb)
  (declare ((simple-array (complex double-float) 3) vola volb)
	   (values (simple-array (complex double-float) 3) &optional))
  (let* ((da (array-dimensions vola))
	 (db (array-dimensions volb))
	 (compare-ab (map 'list #'(lambda (x y) (eq x y)) da db)))
    (when (some #'null compare-ab)
      (error "convolve3-circ expects both input arrays to have the same dimensions.")))
  (ift3 (.* (ft3 vola) (ft3 volb))))

(defun front (i) ;; extra size needed to accommodate kernel overlap
		 ;; there is a difference between even and odd kernels
  (declare (fixnum i)
	   (values fixnum &optional))
  (max 0
       (if (evenp i)
	   (floor i 2)
	   (1- (floor i 2)))))

;; volb is the kernel
(defun convolve3-nocrop (vola volb)
  "Convolve VOLA with VOLB. We consider VOLB as the convolution
kernel. Returns (values result vec). RESULT is an arrays that is as
big as necessary to accommodate the convolution and VEC contains the
relative coordinates to find the original sample positions of array
VOLA in RESULT."
    (declare ((simple-array (complex double-float) 3) vola volb)
	   (values (simple-array (complex double-float) 3) vec-i &optional))
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
	     (fzb (front zb))
	     (fyb (front yb))
	     (fxb (front xb))
	     (fza (front za))
	     (fya (front ya))
	     (fxa (front xa))
	     (start (make-vec-i :x fxb :y fyb :z fzb)))
	(do-box (k j i 0 za 0 ya 0 xa)
	  (setf (aref biga (+ k fzb) (+ j fyb) (+ i fxb))
		(aref vola k j i)))
	(do-box (k j i 0 zb 0 yb 0 xb)
	  (setf (aref bigb (+ k fza) (+ j fya) (+ i fxa))
		(aref volb k j i)))
	(values (convolve3-circ biga (fftshift3 bigb))
		start)))))

(defun convolve3 (vola volb)
  (destructuring-bind (za ya xa)
      (array-dimensions vola)
    (multiple-value-bind (conv start)
	(convolve3-nocrop vola volb)
      (let* ((result (make-array (array-dimensions vola)
				 :element-type '(complex double-float)))
	     (oz (vec-i-z start))
	     (oy (vec-i-y start))
	     (ox (vec-i-x start)))
	(do-box (k j i 0 za 0 ya 0 xa)
	  (setf (aref result k j i)
	       (aref conv (+ k oz) (+ j oy) (+ i ox))))
	result))))

#+nil
(let ((a (make-array (list 100 200 300)
		     :element-type '(complex double-float)))
      (b (make-array (list 10 200 30)
		     :element-type '(complex double-float))))
  (convolve3 a b)
  nil)


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

;; for writing several type conversion functions
;; will have a name like convert3-ub8/cdf-complex
;; the dimension is looped over 1, 2 and 3
(defmacro def-convert (in-type out-type &optional (function '#'identity)
		       (short-function-name nil))
  `(progn
     ,@(loop for dim from 1 upto 3 collect
	   (let ((short-image-types
		  '(((complex double-float) . cdf)
		    (double-float . df)
		    ((unsigned-byte 8) . ub8))))
	     (labels ((find-short-type-name (type)
			(cdr (assoc type short-image-types :test #'equal)))
		      (find-long-type-name (short-type)
			(car (rassoc short-type short-image-types))))
	       `(defun ,(intern (format nil "CONVERT~d-~a/~a-~a" 
					dim 
					(find-short-type-name in-type)
					(find-short-type-name out-type)
					(if short-function-name
					    short-function-name
					    (subseq (format nil "~a" function)
						    2)))) (a)
		  (declare ((simple-array ,in-type ,dim) a)
			   (values (simple-array ,out-type ,dim) &optional))
		  (let* ((res (make-array (array-dimensions a)
					  :element-type (quote ,out-type)))
			 (res1 (sb-ext:array-storage-vector res))
			 (a1 (sb-ext:array-storage-vector a))
			 (n (length a1)))
		    (dotimes (i n)
		      (setf (aref res1 i)
			    (funcall ,function (aref a1 i))))
		    res)))))))

(def-convert (unsigned-byte 8) (complex double-float)
		  #'(lambda (c) (complex (* 1d0 c))) complex)
(def-convert double-float (complex double-float)
	     #'(lambda (d) (complex d)) complex)
(def-convert double-float (unsigned-byte 8) #'floor)
(def-convert (complex double-float) double-float #'realpart)
(def-convert (complex double-float) double-float #'imagpart)
(def-convert (complex double-float) double-float #'abs)
(def-convert (complex double-float) double-float #'phase)
(def-convert (complex double-float) (unsigned-byte 8)
	     #'(lambda (z) (floor (realpart z)))
	     realpart)
(def-convert (complex double-float) (unsigned-byte 8)
	     #'(lambda (z) (floor (imagpart z)))
	     imagpart)
(def-convert (complex double-float) (unsigned-byte 8)
	     #'(lambda (z) (floor (phase z)))
	     phase)
(def-convert (complex double-float) (unsigned-byte 8)
	     #'(lambda (z) (floor (abs z)))
	     abs)

#+nil
(convert3-cdf/ub8-realpart
 (make-array (list 3 3 3) :element-type '(complex double-float)))

;; will have a name like normalize3-cdf/ub8-abs
(defmacro def-normalize-cdf (function)
  `(progn
    ,@(loop for dim from 2 upto 3 collect
     `(defun ,(intern (format nil "NORMALIZE~d-CDF/UB8-~a" dim function)) (a)
	(declare ((simple-array (complex double-float) ,dim) a)
		 (values (simple-array (unsigned-byte 8) ,dim) &optional))
	(let* ((res (make-array (array-dimensions a)
				:element-type '(unsigned-byte 8)))
	       (res1 (sb-ext:array-storage-vector res))
	       (b (,(intern (format nil "CONVERT~d-CDF/DF-~a" dim function)) a))
	       (b1 (sb-ext:array-storage-vector b))
	       (ma (reduce #'max b1))
	       (mi (reduce #'min b1))
	       (s (if (< (abs (- mi ma)) 1d-12)
		      0d0
		      (/ 255d0 (- ma mi)))))
	  (dotimes (i (length b1))
	    (setf (aref res1 i)
		  (floor (* s (- (aref b1 i) mi)))))
	  res)))))

(def-normalize-cdf abs)
(def-normalize-cdf realpart)
(def-normalize-cdf imagpart)
(def-normalize-cdf phase)


(defmacro def-general-normalize ()
  (let ((cases nil)
	(types '((df double-float floor)
		 (cdf (complex double-float) realpart))))
    (loop for dim from 1 upto 3 collect
	 (loop for q in types
	    do
	      (destructuring-bind (short long func)
		  q
		(let* ((norm (intern (format nil "NORMALIZE~D-~A/UB8-~A"
					     dim short func))))
		  (push `((simple-array ,long ,dim) (,norm a))
			cases)))))
   `(defun normalize->ub8 (a)
      "Convert a 1d, 2d or 3d double or complex array into ub8."
      (declare ((simple-array * *) a)
	       (values (simple-array (unsigned-byte 8) *) &optional))
      (typecase a
	,@cases))))

(def-general-normalize)

#+nil
(normalize->ub8 (make-array (list 12 23) :element-type '(complex double-float)))

(defmacro def-normalize-df (dim function)
  `(defun ,(intern (format nil "NORMALIZE~d-DF/UB8-~a" dim function)) (a)
     (declare ((simple-array double-float ,dim) a)
	      (values (simple-array (unsigned-byte 8) ,dim) &optional))
     (let* ((res (make-array (array-dimensions a)
			     :element-type '(unsigned-byte 8)))
	    (res1 (sb-ext:array-storage-vector res))
	    (b1 (sb-ext:array-storage-vector a))
	    (ma (reduce #'max b1))
	    (mi (reduce #'min b1))
	    (s (if (< (abs (- mi ma)) 1d-12) 
		   0d0
		   (/ 255d0 (- ma mi)))))
       (dotimes (i (length b1))
	 (setf (aref res1 i)
	       (floor (* s (- (aref b1 i) mi)))))
       res)))

(def-normalize-df 3 floor)
(def-normalize-df 2 floor)

(defun normalize2-df/df (a)
  (declare ((simple-array double-float 2) a)
	   (values (simple-array double-float 2) &optional))
  (let* ((res (make-array (array-dimensions a)
			  :element-type 'double-float))
	 (res1 (sb-ext:array-storage-vector res))
	 (a1 (sb-ext:array-storage-vector a))
	 (ma (reduce #'max a1))
	 (mi (reduce #'min a1))
	 (s (if (eq mi ma)
		0d0
		(/ 1d0 (- ma mi)))))
    (dotimes (i (length a1))
      (setf (aref res1 i)
	    (* s (- (aref a1 i) mi))))
    res))

(defun normalize-ub8 (a)
  (declare ((simple-array * *) a)
	   (values (simple-array (unsigned-byte 8) *) &optional))
  (etypecase a
    ((simple-array (complex double-float) 2) (normalize2-cdf/ub8-realpart a))
    ((simple-array (complex double-float) 3) (normalize3-cdf/ub8-realpart a))
    ((simple-array double-float 2) (normalize2-df/ub8-floor a))
    ((simple-array double-float 3) (normalize3-df/ub8-floor a))))

#+nil
(let ((a (make-array (list 3 4 5)
		     :element-type '(complex double-float))))
  (setf (aref a 1 1 1) (complex 1d0 1d0))
  (normalize3-cdf/ub8 a))

#+nil ;; find the names of all the functions that were defined by the
      ;; above macros, for exporting
(let ((res ()))
  (with-package-iterator (next-symbol *package* :internal)
    (loop (multiple-value-bind (more? symbol)
	      (next-symbol)
	    (if more?
		(push symbol res)
		(return)))))
  (loop for s in (delete-if-not #'(lambda (x) 
				    (let* ((pat #+nil "CONVERT" 
					     "NORMALI"
					     )
					   (lpat (length pat)))
				      (when (< lpat (length x))
					(string= pat x 
						 :end1 lpat
						 :end2 lpat))))
				(mapcar #'(lambda (x) 
					    (format nil "~a" x)) res))
     do (format t "#:~a~%" (string-downcase s))))


(defun decimate-xy-ub8 (dx vol)
  "Increase transversal sampling distance by odd integer factor DX."
  (declare (fixnum dx)
	   ((simple-array (unsigned-byte 8) 3) vol)
	   (values (simple-array (unsigned-byte 8) 3) &optional))
  (unless (eq (mod dx 2) 1)
    (error "Factor DX has to be odd."))
  (destructuring-bind (z y x)
      (array-dimensions vol)
    (let* ((dx2 (* dx dx))
	   (nx (floor x dx))
	   (ny (floor y dx))
	   (result (make-array (list z ny nx)
			       :element-type '(unsigned-byte 8))))
      (do-box (k j i 0 z 0 ny 0 nx)
	(let ((sum 0))
	  (do-rectangle (jj ii 0 dx 0 dx)
	    (incf sum (aref vol
			    k
			    (+ (* dx j) jj)
			    (+ (* dx i) ii))))
	  (setf (aref result k j i) (floor sum dx2))))
      result)))
;; for dx=5:
;; 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3
;; x x x q x
;;           x o x x x
;;                     x x x x x
;;                               x x x x x
;;                                         x x x x ?
;; dxh=2, n=24
;; output size floor(n,dx)=4
;; position of q: ii=3, i=0: dx*i+ii=5*0+3=3
;; position of o: ii=1, i=1: dx*i+ii=5+1=6

(defun count-non-zero-ub8 (vol)
  (declare ((simple-array (unsigned-byte 8) 3) vol)
	   (values fixnum &optional))
  (let* ((sum 0)
	 (vol1 (sb-ext:array-storage-vector vol)))
    (dotimes (i (length vol1))
      (when (< 0 (aref vol1 i))
	(incf sum)))
    sum))

;; bbox contains float values but is also used to represent pixel
;; positions. In that case start is the first sample that is non-zero
;; and end is the last non-zero pixel.
(defstruct bbox
  (start (v) :type vec)
  (end (alexandria:required-argument) :type vec))

(defun extract-bbox2-ub8 (a bbox)
  (declare ((simple-array (unsigned-byte 8) 2) a)
	   (bbox bbox)
	   (values (simple-array (unsigned-byte 8) 2) &optional))
  (destructuring-bind (y x)
      (array-dimensions a)
    (with-slots (start end)
	bbox
     (unless (and (< (vec-x end) x)
		  (< (vec-y end) y))
       (error "bbox is bigger than array"))
     (let* ((sx (floor (vec-x start)))
	    (sy (floor (vec-y start)))
	    (widths (v+ (v- end start) (v 1d0 1d0)))
	    (res (make-array (list (floor (vec-y widths))
				   (floor (vec-x widths)))
			     :element-type '(unsigned-byte 8))))
       (destructuring-bind (yy xx)
	   (array-dimensions res)
	(do-rectangle (j i 0 yy 0 xx)
	  (setf (aref res j i)
		(aref a (+ j sy) (+ i sx)))))
       res))))

(defun replace-bbox2-ub8 (a b bbox)
  "A beeing a big array, and B a smaller one with BBOX giving its
coordinates relative to A, replace the contents of A with B."
  (declare ((simple-array (unsigned-byte 8) 2) a b)
	   (bbox bbox)
	   (values (simple-array (unsigned-byte 8) 2) &optional))
  (destructuring-bind (y x)
      (array-dimensions a)
    (destructuring-bind (yy xx)
	(array-dimensions b)
      (with-slots (start end)
	  bbox
	(unless (and (< (vec-x end) x)
		     (< (vec-y end) y))
	  (error "bbox is bigger than array"))
	(let ((widths (v+ (v- end start) (v 1d0 1d0))))
	  (unless (and (= (floor (vec-x widths)) xx)
		       (= (floor (vec-y widths)) yy))
	    (error "size of BBOX isn't the same as size of small array B"))
	  (let ((sx (floor (vec-x start)))
		(sy (floor (vec-y start))))
	   (do-rectangle (j i 0 yy 0 xx)
	     (setf (aref a (+ sy j) (+ sx i))
		   (aref b j i)))
	   a))))))

(defun find-bbox2-ub8 (a)
  "Return the rectangle containing non-zero pixels. Returns nil if all
pixels are zero."
  (declare ((simple-array (unsigned-byte 8) 2) a)
	   (values (or null bbox) &optional))
  (destructuring-bind (y x)
      (array-dimensions a)
    (labels ((top () 
	       ;; Note: the order of the loops of do-rectangle is important
	       (do-rectangle (j i 0 y 0 x)
		 (unless (= 0 (aref a j i))
		   (return-from top j)))
	       (1- y))
	     (left () ;; search from left side for first non-zero
	       (do-rectangle (i j 0 x 0 y)
		 (unless (= 0 (aref a j i))
		   (return-from left i)))
	       (1- x))
	     (bottom ()
	       (do-rectangle (j i 0 y 0 x)
		 ;; invert j so that it starts search from bottom
		 (let ((jj (- (1- y) j)))
		  (unless (= 0 (aref a jj i))
		    (return-from bottom jj))))
	       0)
	     (right () 
	       (do-rectangle (i j 0 x 0 y)
		 (let ((ii (- (1- x) i)))
		  (unless (= 0 (aref a j ii))
		    (return-from right ii))))
	       0))
      (let ((l (left))
	    (r (right)))
	(when (<= l r) ;; otherwise all pixels are zero
	 (make-bbox :start (v (* 1d0 l) (* 1d0 (top)))
		    :end (v (* 1d0 r) (* 1d0 (bottom)))))))))

#+nil
(let* ((a (make-array (list 5 5) 
		      :element-type '(unsigned-byte 8)
		      :initial-contents '((0 0 0 0 0)
					  (0 1 0 0 0)
					  (0 0 0 1 0)
					  (0 0 0 0 0)
					  (0 0 0 0 0))))
       (empty (make-array (list 5 5) 
			  :element-type '(unsigned-byte 8)))
       (box (find-bbox2-ub8 a))
       (ex (extract-bbox2-ub8 a box)))
  (replace-bbox2-ub8 empty ex box))

(defun find-bbox3-ub8 (a)
  "Return the box containing non-zero pixels. Returns nil if all
pixels are zero."
  (declare ((simple-array (unsigned-byte 8) 3) a)
	   (values (or null bbox) &optional))
  (destructuring-bind (z y x)
      (array-dimensions a)
    (labels ((front ()
	       (do-box (k j i 0 z 0 y 0 x)
		 (unless (= 0 (aref a k j i))
		   (return-from front k)))
	       (1- z))
	     (back ()
	       (do-box (k j i 0 z 0 y 0 x)
		 (let ((kk (- (1- z) k)))
		   (unless (= 0 (aref a kk j i))
		    (return-from back kk))))
	       0)
	     (top () 
	       (do-box (j k i 0 y 0 z 0 x)
		 (unless (= 0 (aref a k j i))
		   (return-from top j)))
	       (1- y))
	     (left ()
	       (do-box (i k j 0 x 0 z 0 y)
		 (unless (= 0 (aref a k j i))
		   (return-from left i)))
	       (1- x))
	     (bottom ()
	       (do-box (j k i 0 y 0 z 0 x)
		 (let ((jj (- (1- y) j)))
		  (unless (= 0 (aref a k jj i))
		    (return-from bottom jj))))
	       0)
	     (right () 
	       (do-box (i k j 0 x 0 z 0 y)
		 (let ((ii (- (1- x) i)))
		  (unless (= 0 (aref a k j ii))
		    (return-from right ii))))
	       0))
      (let ((l (left))
	    (r (right)))
	(when (<= l r) ;; otherwise all pixels are zero
	 (make-bbox :start (v (* 1d0 l) (* 1d0 (top)) (* 1d0 (front)))
		    :end (v (* 1d0 r) (* 1d0 (bottom)) (* 1d0 (back)))))))))

(defmacro def-extract-bbox3 ()
  `(progn
     ,@(loop for i in '((df double-float)
			(cdf (complex double-float))
			(ub8 (unsigned-byte 8))) collect
	    (destructuring-bind (short long)
	       i
	      `(defun ,(intern (format nil "EXTRACT-BBOX3-~a" short)) (a bbox)
		(declare ((simple-array ,long 3) a)
			 (bbox bbox)
			 (values (simple-array ,long 3) &optional))
		(destructuring-bind (z y x)
		    (array-dimensions a)
		  (with-slots (start end)
		      bbox
		    (unless (and (< (vec-x end) x)
				 (< (vec-y end) y)
				 (< (vec-z end) z))
		      (error "bbox is bigger than array"))
		    (let* ((sx (floor (vec-x start)))
			   (sy (floor (vec-y start)))
			   (sz (floor (vec-z start)))
			   (widths (v+ (v- end start) (v 1d0 1d0 1d0)))
			   (res (make-array (list (floor (vec-z widths))
						  (floor (vec-y widths))
						  (floor (vec-x widths)))
					    :element-type ',long)))
		      (destructuring-bind (zz yy xx)
			  (array-dimensions res)
			(do-box (k j i 0 zz 0 yy 0 xx)
			  (setf (aref res k j i)
				(aref a (+ k sz) (+ j sy) (+ i sx)))))
		      res))))))))
(def-extract-bbox3)


(defun replace-bbox3-ub8 (a b bbox)
  "A beeing a big array, and B a smaller one with BBOX giving its
coordinates relative to A, replace the contents of A with B."
  (declare ((simple-array (unsigned-byte 8) 3) a b)
	   (bbox bbox)
	   (values (simple-array (unsigned-byte 8) 3) &optional))
  (destructuring-bind (z y x)
      (array-dimensions a)
    (destructuring-bind (zz yy xx)
	(array-dimensions b)
      (with-slots (start end)
	  bbox
	(unless (and (< (vec-x end) x)
		     (< (vec-y end) y)
		     (< (vec-z end) z))
	  (error "bbox is bigger than array"))
	(let ((widths (v+ (v- end start) (v 1d0 1d0 1d0))))
	  (unless (and (= (floor (vec-x widths)) xx)
		       (= (floor (vec-y widths)) yy)
		       (= (floor (vec-z widths)) zz))
	    (error "size of BBOX isn't the same as size of small array B"))
	  (let ((sx (floor (vec-x start)))
		(sy (floor (vec-y start)))
		(sz (floor (vec-z start))))
	   (do-box (k j i 0 zz 0 yy 0 xx)
	     (setf (aref a (+ sz k) (+ sy j) (+ sx i))
		   (aref b k j i)))
	   a))))))

#+nil
(let* ((empty (make-array (list 4 4 4) :element-type '(unsigned-byte 8)))
       (a (make-array (list 4 4 4) 
		      :element-type '(unsigned-byte 8)
		      :initial-contents
		      '(((0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0))
			((0 0 0 0)
			 (0 1 0 0)
			 (0 0 1 0)
			 (0 0 0 0))
			((0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0))
			((0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0)))))
       (box (find-bbox3-ub8 a))
       (ex (extract-bbox3-ub8 a box)))
  (replace-bbox3-ub8 empty ex box))

(defun mean-realpart (a)
  "Calculate the average value over all the samples in volume A."
  (declare ((simple-array (complex double-float) *) a)
	   (values double-float &optional))
  (let* ((a1 (sb-ext:array-storage-vector a))
	 (sum 0d0)
	 (n (length a1)))
    (dotimes (i n)
      (incf sum (realpart (aref a1 i))))
    (/ sum n)))