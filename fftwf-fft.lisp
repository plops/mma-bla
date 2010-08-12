(declaim (optimize (speed 2) (debug 3) (safety 3)))
#.(require :vol)
(defpackage :fftwf-fft
  (:use :cl :sb-alien :sb-c-call)
  (:export #:ft3-csf
	   #:ift3-csf
	   #:ift2-csf
	   #:ft2-csf))
(in-package :fftwf-fft)

(load-shared-object "/usr/lib/libfftw3f.so")

;; multithreading for fftw is just a matter of two initializing
;; function calls, see:
;; http://www.fftw.org/fftw3_doc/Usage-of-Multi_002dthreaded-FFTW.html#Usage-of-Multi_002dthreaded-FFTW
;; but it didn't seem to get faster
#+nil
(progn
  (load-shared-object "/usr/lib/libfftw3f_threads.so")
  
  (define-alien-routine ("fftwf_init_threads" init-threads)
      int)
  (define-alien-routine ("fftwf_plan_with_nthreads" plan-with-nthreads)
      void
    (nthreads int))
  
  (defun init-ft ()
    (init-threads)
    (plan-with-nthreads 4)))

;; to clean up completely call void fftw_cleanup_threads(void)


(define-alien-routine ("fftwf_execute" execute)
    void
  (plan (* int)))
 
(defconstant +forward+ 1)
(defconstant +backward+ -1)
(defconstant +estimate+ (ash 1 6))

 
(define-alien-routine ("fftwf_plan_dft_3d" plan-dft-3d)
    (* int)
  (n0 int)
  (n1 int)
  (n2 int)
  (in (* double-float))
  (out (* double-float))
  (sign int)
  (flags unsigned-int))

(define-alien-routine ("fftwf_plan_dft_2d" plan-dft-2d)
    (* int)
  (n0 int)
  (n1 int)
  (in (* double-float))
  (out (* double-float))
  (sign int)
  (flags unsigned-int))

(defun ft3-csf (in &key (forward t))
  (declare ((simple-array (complex single-float) 3) in)
	   (boolean forward)
	   (values (simple-array (complex single-float) 3) &optional))
  (let ((dims (array-dimensions in)))
   (destructuring-bind (z y x)
       dims
     (let* ((out (make-array dims :element-type '(complex single-float))))
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

(defmacro ift3-csf (in)
  `(ft3 ,in :forward nil))

(defun ft2-csf (in &key (forward t))
  (declare ((simple-array (complex single-float) 2) in)
	   (boolean forward)
	   (values (simple-array (complex single-float) 2) &optional))
  (let ((dims (array-dimensions in)))
    (destructuring-bind (y x)
	dims
      (let* ((out (make-array dims :element-type '(complex single-float))))
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

(defmacro ift2-csf (in)
  `(ft2 ,in :forward nil))
