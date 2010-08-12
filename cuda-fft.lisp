;; calling nvidias cufft from sbcl
;; I use debian5 64bit
;; the graphics card is:
;; 02:00.0 VGA compatible controller: nVidia Corporation G94 [GeForce 9600 GT] (rev a1)
;; and the Cuda Toolkit 3.1
;; http://developer.nvidia.com/object/cuda_3_1_downloads.html
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

#+nil
(progn
  #.(require :vol)
  (time
   (let* ((nx 256)
	  (ny nx)
	  (nz ny)
	  (a (vol:convert3-ub8/csf-complex
	      (vol:draw-sphere-ub8 20d0 nz ny nx))))
     (vol:write-pgm "cufft.pgm" 
		(vol:normalize2-csf/ub8-abs
		 (vol:cross-section-xz-csf (ft3-csf a)))))))

