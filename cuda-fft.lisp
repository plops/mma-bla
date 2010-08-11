#.(progn 
    (require :alexandria)
    (require :vector)
    (require :vol))
(defpackage :cuda-fft
   (:use :cl :sb-alien :sb-c-call))
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
(defconstant +cufft-c2c+ #x29)
(defconstant +cufft-forward+ -1)
(defconstant +cufft-inverse+ 1)
(define-alien-type size-t unsigned-long)
(define-alien-type cufft-complex single-float) ;; there is no complex
					       ;; support in sb-alien

(define-alien-routine ("cudaMalloc" cuda-malloc)
    cuda-error
  (device-pointer cuda-device-ptr :out)
  (size size-t))

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

(defun cu-malloc-sf (n)
  (declare (fixnum n))
  (multiple-value-bind (result device-ptr)
      (cuda-malloc (* 4 2 n))
    (unless (eq 0 result)
      (error "cuda-malloc error: ~a" result))
    device-ptr))

(define-alien-routine ("cudaFree" cuda-free)
    cuda-error
  (device-pointer cuda-device-ptr :copy))

(define-alien-routine ("cudaMemcpy" cuda-memcpy)
    cuda-error
  (dst (* t))
  (src (* t))
  (count size-t)
  (kind cuda-memcpy-kind))

#+nil
(let* ((nx 120)
       (ny nx)
       (nz ny)
       (dims (list nz ny nx))
       (a (vol:convert3-ub8/csf-complex
	   (vol:draw-sphere-ub8 20d0 nz ny nx)))
       (a1 (sb-ext:array-storage-vector a))
       (device (cu-malloc-sf (length a1))))
  (cuda-memcpy (sb-sys:int-sap device) 
	       (sb-sys:vector-sap a1)
	       (* (length a1) 4 2) 'host->device)
  (let ((plan (cu-plan nx ny nz)))
    (time (cufft-exec-c2c plan (sb-sys:int-sap device)
		     (sb-sys:int-sap device) +cufft-forward+))
    (cufft-destroy plan))
  (cuda-memcpy (sb-sys:vector-sap a1)
	       (sb-sys:int-sap device) 
	       (* (length a1) 4 2) 'device->host)
  (vol:write-pgm "/home/martin/tmp/cufft.pgm" 
		 (vol:normalize2-csf/ub8-abs
		  (vol:cross-section-xz-csf a)))
  (cuda-free device))
