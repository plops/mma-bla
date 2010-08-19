(declaim (optimize (speed 2) (debug 3) (safety 3)))
(in-package :vol)

(defconstant +forward+ 1)
(defconstant +backward+ -1)
(defconstant +estimate+ (ash 1 6))

(defmacro def-ffi (&optional (library "libfftw3.so") (prefix "fftw"))
  `(progn
     (load-shared-object ,(format nil "~a" library))
     (define-alien-routine (,(format nil "~a_execute" prefix) 
			     ,(vol:format-symbol "~a-execute" prefix))
	 void
       (plan (* int)))
     ,@(loop for rank in '(1 2 3) collect
	    `(define-alien-routine 
		 (,(format nil "~a_plan_dft_~ad" prefix rank) 
		   ,(format-symbol "~a-plan-dft-~ad" prefix rank))
		 (* int)
	       ,@(loop for i below rank collect
		      `(,(format-symbol "n~d" i) int))
	       (in (* double-float))
	       (out (* double-float))
	       (sign int)
	       (flags unsigned-int)))))

;; define the foreign functions
;; fftw{f}-execute, fftw{f}-plan-dft-{1,2,3}d
(def-ffi "libfftw3.so.3" "fftw")
(def-ffi "libfftw3f.so.3" "fftwf")

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



(def-generator (ft (rank type)) 
  (let* ((prefix (ecase type
		  (cdf 'fftw)
		  (csf 'fftwf)))
	 (rlong-type (ecase type
		       (csf (get-long-type 'sf))
		       (cdf (get-long-type 'df))))
	 (plan (format-symbol "~a-plan-dft-~ad" prefix rank))
	 (execute (format-symbol "~a-execute" prefix)))
    `(defun ,name (in &key (forward t))
       (declare ((simple-array ,long-type ,rank) in)
		(boolean forward)
	       (values (simple-array ,long-type ,rank) &optional))
      (let* ((dims (array-dimensions in))
	     (out (make-array dims :element-type ',long-type))
	     (in-sap (sb-sys:vector-sap 
		      (sb-ext:array-storage-vector in)))
	     (out-sap (sb-sys:vector-sap 
		       (sb-ext:array-storage-vector out)))
	     (dir (if forward +forward+ +backward+)))
	(sb-sys:with-pinned-objects (in out)
	  ,(ecase rank
		  (1 `(destructuring-bind (x) dims
			(let ((p (,plan x in-sap out-sap
					dir +estimate+)))
			  (,execute p))))
		  (2 `(destructuring-bind (y x) dims
			(let ((p (,plan y x in-sap out-sap
					dir +estimate+)))
			  (,execute p))))
		  (3 `(destructuring-bind (z y x) dims
			(let ((p (,plan z y x in-sap out-sap
					dir +estimate+)))
			  (,execute p))))))
	;; normalize so that a=ift(ft(a)), after forward transform
	;; divide by n
	(if forward
	    (s* (/ ,(coerce 1 rlong-type)
		   (array-total-size out))
		out)
	    out)))))

#+nil
(def-ft-rank-type 1 csf)

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
		 ,@cases
		 (t (error "The given type can't be handled with a generic ~a function." ',name)))))))

(def-ft-functions (1 2 3) (csf cdf))

(defmacro ift (in)
  (alexandria:with-gensyms (input)
    (let ((input in))
      `(ft ,input :forward nil))))

#+nil
(progn
  (time
   (let* ((nx 128)
	  (ny nx)
	  (nz ny)
	  (a (convert-3-ub8/csf-mul
	      (draw-sphere-ub8 20.0 nz ny nx))))
     (write-pgm "/home/martin/tmp/fftw.pgm" 
		(normalize-2-csf/ub8-abs
		 (cross-section-xz-csf (ft a)))))))