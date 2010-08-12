;; calling nvidias cufft from sbcl
;; 2010-08-12 kielhorn.martin@googlemail.com

(defpackage :cuda-fft
   (:use :cl :sb-alien :sb-c-call)
   (:export #:ft3-csf
	    #:ift3-csf))
(in-package :cuda-fft)

(declaim (optimize (speed 2) (debug 3) (safety 3)))
 
(load-shared-object "/usr/local/cuda/lib64/libcudart.so")
(load-shared-object "/usr/local/cuda/lib64/libcufft.so")


(define-alien-type cufft-handle unsigned-int)
(define-alien-type cuda-device-ptr unsigned-int)
(define-alien-type cuda-error int) ;; enum, 0 is success
(define-alien-type cufft-result int) ;; enum, 0 is success
(define-alien-type cufft-type int) ;; enum, c2c is #x29
(define-alien-type cuda-memcpy-kind 
    (enum nil 
	  host->host
	  host->device
	  device->host
	  device->device))

(defconstant +host->device+ 1)
(defconstant +device->host+ 2)
(defconstant +cufft-c2c+ #x29) ;; compex single-float
(defconstant +cufft-z2z+ #x69) ;; complex double-float
(defconstant +cufft-forward+ -1)
(defconstant +cufft-inverse+ 1)
(define-alien-type size-t unsigned-long)
(define-alien-type cufft-complex single-float) ;; there is no complex
					       ;; support in sb-alien

(define-alien-routine ("cudaMalloc" cuda-malloc)
    cuda-error
  (device-pointer cuda-device-ptr :out)
  (size size-t))

(define-alien-routine ("cufftPlan2d" cufft-plan-2d)
    cufft-result
  (plan cufft-handle :out)
  (nx int)
  (ny int)
  (type cufft-type))

(define-alien-routine ("cufftPlan3d" cufft-plan-3d)
    cufft-result
  (plan cufft-handle :out)
  (nx int)
  (ny int)
  (nz int)
  (type cufft-type))

(define-alien-routine ("cufftExecC2C" cufft-exec-c2c)
    cufft-result
  (plan cufft-handle)
  (in-data (* cufft-complex))
  (out-data (* cufft-complex))
  (direction int))

(define-alien-routine ("cufftDestroy" cufft-destroy)
    cufft-result
  (plan cufft-handle))

(defun cu-plan (x y z)
  (multiple-value-bind (result plan)
      (cufft-plan-3d x y z +cufft-c2c+)
    (unless (eq 0 result)
      (error "cu-plan error: ~a" result))
    plan))

(defun cu-malloc-csf (n)
  (declare (fixnum n))
  (let ((complex-single-float-size (* 4 2)))
    (multiple-value-bind (result device-ptr)
	(cuda-malloc (* complex-single-float-size n))
     (unless (eq 0 result)
       (error "cuda-malloc error: ~a" result))
     device-ptr)))

(define-alien-routine ("cudaFree" cuda-free)
    cuda-error
  (device-pointer cuda-device-ptr :copy))

(define-alien-routine ("cudaMemcpy" cuda-memcpy)
    cuda-error
  (dst (* t))
  (src (* t))
  (count size-t)
  (kind cuda-memcpy-kind))

;; same semantics as ft3 wrapper to fftw3, input array isn't modified 
;; ft{2,3}-{csf,cdf}
(defun ft3-csf (in &key (forward t))
  (declare ((simple-array (complex single-float) 3) in)
	   (boolean forward)
	   (values (simple-array (complex single-float) 3) &optional))
  (let ((dims (array-dimensions in)))
    (destructuring-bind (z y x)
	dims
      (let* ((out (make-array dims :element-type '(complex single-float)))
	     (out1 (sb-ext:array-storage-vector out))
	     (in1 (sb-ext:array-storage-vector in))
	     (n (length in1))
	     ;; allocate array on device
	     (device (cu-malloc-csf (length in1)))
	     (complex-single-float-size (* 4 2)))
	;; copy data to device
	(cuda-memcpy (sb-sys:int-sap device) 
		     (sb-sys:vector-sap in1)
		     (* n complex-single-float-size) 
		     'host->device)
	;; plan and execute in-place transform on device
	(let ((plan (cu-plan x y z)))
	  (cufft-exec-c2c plan 
			  (sb-sys:int-sap device)
			  (sb-sys:int-sap device)
			  (if forward 
			      +cufft-forward+
			      +cufft-inverse+))
	  (cufft-destroy plan))
	;; copy result back
	(cuda-memcpy (sb-sys:vector-sap in1)
		     (sb-sys:int-sap device) 
		(* n complex-single-float-size) 'device->host)
	;; deallocate array on device
	(cuda-free device)
	;; normalize if forward
	(when forward 
	 (let* ((1/n (/ 1s0 n)))
	   (dotimes (i n)
	     (setf (aref out1 i) (* 1/n (aref out1 i))))))
	in))))

(defmacro ift3-csf (in)
  `(ft3-csf-cuda ,in :forward nil))


;; to test the fouriertransform run the following progn.

#+nil
(progn
  ;; for writing several type conversion functions
  ;; will have a name like convert3-ub8/cdf-complex
  ;; the dimension is looped over 1, 2 and 3
  (defmacro def-convert (in-type out-type &optional (function '#'identity)
			 (short-function-name nil))
    `(progn
       ,@(loop for dim from 1 upto 3 collect
	   (let ((short-image-types
		  '(((complex double-float) . cdf)
		    ((complex single-float) . csf) 
		    (double-float . df)
		    (single-float . sf)
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

  ;; I only define what I need here, a lot more are possible:
  (def-convert (unsigned-byte 8) (complex single-float)
    #'(lambda (c) (complex (* 1s0 c))) complex)
  (def-convert (complex single-float) single-float #'abs)

  ;; will have a name like normalize3-csf/ub8-abs. this function would
  ;; convert a complex single-float volume into unsigned-byte 8,
  ;; applying the function abs and squeezing all values into 0..255
  (defmacro def-normalize-csf (function)
  `(progn
    ,@(loop for dim from 2 upto 3 collect
     `(defun ,(intern (format nil "NORMALIZE~d-CSF/UB8-~a" dim function)) (a)
	(declare ((simple-array (complex single-float) ,dim) a)
		 (values (simple-array (unsigned-byte 8) ,dim) &optional))
	(let* ((res (make-array (array-dimensions a)
				:element-type '(unsigned-byte 8)))
	       (res1 (sb-ext:array-storage-vector res))
	       (b (,(intern (format nil "CONVERT~d-CSF/SF-~a" dim function)) a))
	       (b1 (sb-ext:array-storage-vector b))
	       (ma (reduce #'max b1))
	       (mi (reduce #'min b1))
	       (s (if (< (abs (- mi ma)) 1s-10)
		      0s0
		      (/ 255s0 (- ma mi)))))
	  (dotimes (i (length b1))
	    (setf (aref res1 i)
		  (floor (* s (- (aref b1 i) mi)))))
	  res)))))

  (def-normalize-csf abs)
  
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
  
  ;; extract an xz slice from a volume
  (defun cross-section-xz-csf (a &optional (y (floor (array-dimension a 1) 2)))
    (declare ((simple-array (complex single-float) 3) a)
	     (fixnum y)
	     (values (simple-array (complex single-float) 2)
		     &optional))
    (destructuring-bind (z y-size x)
	(array-dimensions a)
      (unless (<= 0 y (1- y-size))
	(error "Y is out of bounds."))
      (let ((b (make-array (list z x)
			   :element-type `(complex single-float))))
	(do-rectangle (j i 0 z 0 x)
	  (setf (aref b j i)
		(aref a j y i)))
	b)))

  ;; store a 2d unsigned-byte image into a file
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
  
  ;; draw a sphere into a volume
  (defun draw-sphere-ub8 (radius z y x)
    (declare (double-float radius)
	   (fixnum z y x)
	   (values (simple-array (unsigned-byte 8) 3)
		   &optional))
  (let ((sphere (make-array (list z y x)
			    :element-type '(unsigned-byte 8))))
    (let ((xh (floor x 2))
	  (yh (floor y 2))
	  (zh (floor z 2))
	  (radius2 (* radius radius)))
     (do-box (k j i 0 z 0 y 0 x)
       (let ((r2 (+ (expt (* 1d0 (- i xh)) 2)
		    (expt (* 1d0 (- j yh)) 2)
		    (expt (* 1d0 (- k zh)) 2))))
	 (setf (aref sphere k j i)
	       (if (< r2 radius2)
		   1 0)))))
    sphere))
  
  (time
   (let* ((nx 256)
	  (ny nx)
	  (nz ny)
	  (a (convert3-ub8/csf-complex
	      (draw-sphere-ub8 20d0 nz ny nx))))
     (write-pgm "cufft.pgm" 
		(normalize2-csf/ub8-abs
		 (cross-section-xz-csf (ft3-csf a)))))))

