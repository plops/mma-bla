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
  (device-pointer cuda-device-ptr :copy))

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


(defun cu-malloc-csf (n)
  (declare (fixnum n))
  (let ((complex-single-float-size (* 4 2)))
    (multiple-value-bind (result device-ptr)
	(cuda-malloc (* complex-single-float-size n))
      (unless (eq 0 result)
       (error "cuda-malloc error: ~a" result))
      device-ptr)))


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
	    ;; allocate array on device
	    (device (cu-malloc-csf (length in1)))
	    (complex-size (* ,(ecase type
				     (csf '4)
				     (cdf '8)) 2)))
       ;; copy data to device
       (cuda-memcpy (sb-sys:int-sap device) 
		    (sb-sys:vector-sap in1)
		    (* n complex-size) 
		    'host->device)
       ;; plan and execute in-place transform on device
       (let ((plan ,(ecase rank
			   (2 `(destructuring-bind (y x) dims
				 (cu-plan x y)))
			   (3 `(destructuring-bind (z y x) dims
				 (cu-plan x y z))))))
	 (cufft-exec-c2c plan 
			 (sb-sys:int-sap device)
			 (sb-sys:int-sap device)
			 (if forward +cufft-forward+ +cufft-inverse+))
	 (cufft-destroy plan))
       ;; copy result back
       (cuda-memcpy (sb-sys:vector-sap out1)
		    (sb-sys:int-sap device) 
		    (* n complex-size) 'device->host)
       ;; deallocate array on device
       (cuda-free device)
       ;; normalize if forward
       (when forward 
	 (let* ((1/n (/ 1s0 n)))
	   (dotimes (i n)
	     (setf (aref out1 i) (* 1/n (aref out1 i))))))
       out)))


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