(in-package :vol)

;; for writing several type conversion functions
;; will have a name like convert3-ub8/cdf-complex
;; the rank is looped over 1, 2 and 3

(def-generator (convert (rank type out_type func short_func))
  (let ((long-out-type (get-long-type out_type))
	;; override the name that is constructed by def-generator
	(name (format-symbol "convert-~a-~a/~a-~a" 
			     rank type out_type
			     short_func)))
    (store-new-function name)
    `(defun ,name (a)
       (declare ((simple-array ,long-type ,rank) a)
		(values (simple-array ,long-out-type ,rank) &optional))
       (let* ((result (make-array (array-dimensions a)
				  :element-type (quote ,long-out-type)))
	      (result1 (sb-ext:array-storage-vector result))
	      (a1 (sb-ext:array-storage-vector a))
	      (n (length a1)))
	 (dotimes (i n)
	   (setf (aref result1 i)
		 (funcall ,func (aref a1 i))))
	 result))))

#+nil
(def-convert-rank-type-out_type-func-short_func
    1 sf df #'(lambda (x) (* 1d0 x)) coerce)
#+nil
(def-convert-rank-type-out_type-func-short_func
    1 ub8 csf #'(lambda (x) (complex (* 1d0 x))) complex)
#+nil
(convert-1-sf/df-coerce
 (make-array 4 :element-type 'single-float))


(defmacro def-convert-functions ()
  (labels ( ;; create a spec like this: ub8 sf (* 1s0 x) mul
	   ;; down can be used to convert double into float
	   (def (in-type out-type &optional (fun t))
	       `(,in-type 
		 ,out-type 
		 #'(lambda (x)
		     ,(ecase fun
			     (:floor `(floor x))
			     (:coerce `(coerce x ,(get-long-type out-type)))
			     (t `(* ,(coerce 1 (get-long-type out-type)) x))))
		 ,(ecase fun
			 (:floor 'floor)
			 (:coerce 'coerce)
			 (t 'mul))))
	   ;; create downconversions from complex types like
	   ;; cdf df #'realpart realpart
	   (def-comps (in-type out-type functions &optional (fun t))
	     (loop for func in functions collect
		  `(,in-type 
		    ,out-type 
		    ,(ecase fun 
		      (:floor `#'(lambda (x) (floor (funcall #',func x))))
		      (t `#',func)) 
		    ,func))))
    ;; an element of spec looks like this: (ub8 sf #'(lambda(x) (* 1s0 x))
    ;; mul) the first two cols define input and output types, then
    ;; comes a function that does this conversion followed by a short
    ;; name describing the function. this name is attached to the
    ;; convert function
    (let ((specs `(;; upconvert from ub8 into sf and similar
		   ,(def 'ub8 'sf)
		    ,(def 'ub8 'df)
		    ,(def 'ub8 'csf)
		    ,(def 'ub8 'cdf)

		    ,(def 'sf 'df)
		    ,(def 'sf 'csf)
		    ,(def 'sf 'cdf)

		    ,(def 'df 'cdf)

		    ;; upconvert complex to complex
		    ,(def 'csf 'cdf)
		    
		    ;; downconvert from double into single
		    ,(def 'df 'sf :coerce)
		    ,(def 'cdf 'csf :coerce)
		    
		    ;; downconvert from float into bytes
		    ,(def 'sf 'ub8 :floor)
		    ,(def 'df 'ub8 :floor)
		    
		    ;; convert from complex into real
		    ,@(def-comps 'csf 'sf '(realpart imagpart abs phase))
		    ,@(def-comps 'cdf 'df '(realpart imagpart abs phase))

		    ;; complex into real and conversion into fixed
		    ,@(def-comps 'csf 'ub8 '(realpart imagpart abs phase) :floor)
		    ,@(def-comps 'cdf 'ub8 '(realpart imagpart abs phase) :floor)
		    ))
	  (result nil))
      (loop for rank in '(1 2 3) do
	   (loop for spec in specs do
		(destructuring-bind (in out fun name)
		    spec
		 (push `(def-convert-rank-type-out_type-func-short_func
			    ,rank ,in ,out ,fun ,name)
		       result))))
      `(progn ,@result))))

(def-convert-functions)

#+nil
(convert-3-cdf/ub8-realpart
 (make-array (list 3 3 3) :element-type '(complex double-float)))

#+nil
(convert-3-csf/cdf-mul
 (make-array (list 3 3 3) :element-type '(complex single-float)))

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
		      zero
		      (/ 255d0 (- ma mi)))))
	  (dotimes (i (length b1))
	    (setf (aref res1 i)
		  (floor (* s (- (aref b1 i) mi)))))
	  res)))))

(def-normalize-cdf abs)
(def-normalize-cdf realpart)
(def-normalize-cdf imagpart)
(def-normalize-cdf phase)

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
(def-normalize-csf realpart)
(def-normalize-csf imagpart)
(def-normalize-csf phase)


(defmacro def-general-normalize ()
  (let ((cases nil)
	(types '((df my-float floor)
		 (sf single-float floor)
		 (csf (complex single-float) realpart)
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
	,@cases
	(t (error "normalize can't handle this type."))))))

(def-general-normalize)

#+nil
(normalize->ub8 (make-array (list 12 23) :element-type '(complex my-float)))

(defmacro def-normalize-df (dim function)
  `(defun ,(intern (format nil "NORMALIZE~d-DF/UB8-~a" dim function)) (a)
     (declare ((simple-array my-float ,dim) a)
	      (values (simple-array (unsigned-byte 8) ,dim) &optional))
     (let* ((res (make-array (array-dimensions a)
			     :element-type '(unsigned-byte 8)))
	    (res1 (sb-ext:array-storage-vector res))
	    (b1 (sb-ext:array-storage-vector a))
	    (ma (reduce #'max b1))
	    (mi (reduce #'min b1))
	    (s (if (< (abs (- mi ma)) 1d-12) 
		   zero
		   (/ 255d0 (- ma mi)))))
       (dotimes (i (length b1))
	 (setf (aref res1 i)
	       (floor (* s (- (aref b1 i) mi)))))
       res)))

(def-normalize-df 3 floor)
(def-normalize-df 2 floor)

(defun normalize2-df/df (a)
  (declare ((simple-array my-float 2) a)
	   (values (simple-array my-float 2) &optional))
  (let* ((res (make-array (array-dimensions a)
			  :element-type 'my-float))
	 (res1 (sb-ext:array-storage-vector res))
	 (a1 (sb-ext:array-storage-vector a))
	 (ma (reduce #'max a1))
	 (mi (reduce #'min a1))
	 (s (if (eq mi ma)
		zero
		(/ one (- ma mi)))))
    (dotimes (i (length a1))
      (setf (aref res1 i)
	    (* s (- (aref a1 i) mi))))
    res))

(defun normalize-ub8 (a)
  (declare ((simple-array * *) a)
	   (values (simple-array (unsigned-byte 8) *) &optional))
  (etypecase a
    ((simple-array (complex my-float) 2) (normalize2-cdf/ub8-realpart a))
    ((simple-array (complex my-float) 3) (normalize3-cdf/ub8-realpart a))
    ((simple-array my-float 2) (normalize2-df/ub8-floor a))
    ((simple-array my-float 3) (normalize3-df/ub8-floor a))))

#+nil
(let ((a (make-array (list 3 4 5)
		     :element-type '(complex my-float))))
  (setf (aref a 1 1 1) (complex one one))
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
				    (let* ((pat "CONVERT" 
					      #+nil "NORMALI"
					     )
					   (lpat (length pat)))
				      (when (< lpat (length x))
					(string= pat x 
						 :end1 lpat
						 :end2 lpat))))
				(mapcar #'(lambda (x) 
					    (format nil "~a" x)) res))
     do (format t "#:~a~%" (string-downcase s))))
