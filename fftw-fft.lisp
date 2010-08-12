(declaim (optimize (speed 2) (debug 3) (safety 3)))
#.(require :vol)
(defpackage :fftw-fft
  (:use :cl :sb-alien :sb-c-call)
  (:export #:ft3-cdf
		 #:ift3-cdf
		 #:ift2-cdf
		 #:ft2-cdf))
(in-package :fftw-fft)

(load-shared-object "/usr/lib/libfftw3.so")

;; multithreading for fftw is just a matter of two initializing
;; function calls, see:
;; http://www.fftw.org/fftw3_doc/Usage-of-Multi_002dthreaded-FFTW.html#Usage-of-Multi_002dthreaded-FFTW
;; but it didn't seem to get faster
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
    (plan-with-nthreads 4)))

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

(defun ft3-cdf (in &key (forward t))
  (declare ((simple-array (complex double-float) 3) in)
	   (boolean forward)
	   (values (simple-array (complex double-float) 3) &optional))
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
	   (vol:do-box (k j i 0 z 0 y 0 x)
	     (setf (aref out k j i) (* 1/n (aref out k j i))))))
       out))))

(defmacro ift3-cdf (in)
  `(ft3 ,in :forward nil))

(defun ft2-cdf (in &key (forward t))
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
	    (vol:do-rectangle (j i 0 y 0 x)
	      (setf (aref out j i) (* 1/n (aref out j i))))))
	out))))

(defmacro ift2-cdf (in)
  `(ft2 ,in :forward nil))

#+nil
(progn
  (time
   (let* ((nx 256)
	  (ny nx)
	  (nz ny)
	  (a (vol:convert3-ub8/cdf-complex
	      (vol:draw-sphere-ub8 20d0 nz ny nx))))
     (vol:write-pgm "/home/martin/tmp/fftwf.pgm" 
		(vol:normalize2-cdf/ub8-abs
		 (vol:cross-section-xz-cdf (ft3-cdf a)))))))