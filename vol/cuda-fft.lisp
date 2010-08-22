;; calling nvidias cufft from sbcl
;; I use debian5 64bit
;; the graphics card is:
;; 02:00.0 VGA compatible controller: nVidia Corporation G94 [GeForce 9600 GT] (rev a1)
;; and the Cuda Toolkit 3.1
;; http://developer.nvidia.com/object/cuda_3_1_downloads.html
;; 2010-08-12 kielhorn.martin@googlemail.com

(in-package :vol)

(declaim (optimize (speed 2) (debug 3) (safety 3)))
 

(progn
  (load-shared-object "/usr/local/cuda/lib64/libcudart.so")
  (load-shared-object "/usr/local/cuda/lib64/libcufft.so")
  
  (define-alien-type cufft-handle unsigned-int)
  (define-alien-type cuda-device-ptr unsigned-int)
  (define-alien-type cuda-error int)  ;; enum, 0 is success
  (define-alien-type cufft-result int) ;; enum, 0 is success
  (define-alien-type cufft-type int)   ;; enum, c2c is #x29
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
  (define-alien-type cufft-complex single-float) ;; there is no
						 ;; complex support in
						 ;; sb-alien
  
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
  
  (define-alien-routine ("cudaFree" cuda-free)
    cuda-error
  (device-pointer cuda-device-ptr))

  (define-alien-routine ("cudaMemcpy" cuda-memcpy)
      cuda-error
    (dst (* t))
    (src (* t))
    (count size-t)
    (kind cuda-memcpy-kind)))

(defun cu-plan (x y &optional z)
  (multiple-value-bind (result plan)
      (if z
	  (cufft-plan-3d x y z +cufft-c2c+)
	  (cufft-plan-2d x y +cufft-c2c+))
    (unless (eq 0 result)
      (error "cu-plan error: ~a" result))
    plan))


(defun cu-malloc (n)
  (declare (fixnum n))
  (multiple-value-bind (result device-ptr)
      (cuda-malloc n)
    (unless (eq 0 result)
      (error "cuda-malloc error: ~a" result))
    device-ptr))


;; same semantics as ft3 wrapper to fftw3, input array isn't modified 
;; ft{2,3}-csf
(def-generator (ft (rank type))
  `(defun ,name (in &key (forward t))
     (declare ((simple-array ,long-type ,rank) in)
	      (boolean forward)
	      (values (simple-array ,long-type ,rank) &optional))
     (let* ((dims (array-dimensions in))
	    (out (make-array dims :element-type ',long-type))
	    (out1 (sb-ext:array-storage-vector out))
	    (in1 (sb-ext:array-storage-vector in))
	    (n (length in1))	    
	    (float-size ,(ecase type
				(csf '4)
				(cdf '8)))
	    ;; allocate array on device
	    (complex-size (* float-size 2))
	    (n-bytes (* n complex-size))
	    (device (cu-malloc n-bytes))
	    (dev-sap (sb-sys:int-sap device)))
       ;; copy data to device
       (assert (= 0 (cuda-memcpy dev-sap
				 (sb-sys:vector-sap in1)
				 n-bytes
				 'host->device)))
       ;; plan and execute in-place transform on device
       (let ((plan ,(ecase rank
			   (2 `(destructuring-bind (y x) dims
				 (cu-plan y x)))
			   (3 `(destructuring-bind (z y x) dims
				 (cu-plan z y x))))))
	 (assert (= 0 (cufft-exec-c2c plan 
				      dev-sap
				      dev-sap
				      (if forward +cufft-forward+ +cufft-inverse+))))
	 (assert (= 0 (cufft-destroy plan))))
       ;; copy result back
       (assert (= 0 (cuda-memcpy (sb-sys:vector-sap out1)
				 dev-sap
				 n-bytes 'device->host)))
       ;; deallocate array on device
       (assert (= 0 (cuda-free device)))
       
       ;; normalize so that a=ift(ft(a)), divide by n when forward
       (if forward
	   (s* (/ ,(coerce 1 (ecase type
			       (csf 'single-float)
			       (cdf 'double-float)))
		  (array-total-size out))
	       out)
	   out))))

#+nil
(def-ft-rank-type 2 csf)


(defmacro def-ft-functions (ranks types)
  (let* ((specifics nil)
	 (cases nil)
	 (name (format-symbol "ft")))
    (loop for rank in ranks do
	 (loop for type in types do
	      (let ((def-name (format-symbol "def-~a-rank-type" name))
		    (specific-name (format-symbol "~a-~a-~a" name rank type)))
		(push `(,def-name ,rank ,type) specifics)
		(push `((simple-array ,(get-long-type type) ,rank)
			(,specific-name a :forward forward))
		      cases))))
    (store-new-function name)
    `(progn ,@specifics
	    (defun ,name (a &key forward)
	       (etypecase a
		 ,@cases)))))

(def-ft-functions (2 3) (csf))

(defmacro ift (in)
  `(ft ,in :forward nil))

#+nil
(let ((a (make-array (list 12 12 12) :element-type '(complex single-float))))
  (ift (ft a))
  nil)

#+nil
(write-pgm "/home/martin/tmp/fftw.pgm"
 (normalize-2-csf/ub8-abs
	    (cross-section-xz 
	     (let ((a (ft-3-csf (draw-sphere-csf 12.0 34 206 296)))
		   (b (ft-3-csf (fftshift (draw-sphere-csf 5.0 34 206 296)))))
	       (ift (.* a b))))))