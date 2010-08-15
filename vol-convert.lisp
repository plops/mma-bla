;; define a bunch of functions to convert an {1,2,3}-dimensional array
;; from one type into another type. 
(in-package :vol)

;; for writing several type conversion functions will have a name like
;; convert-3-ub8/cdf-mul the rank is looped over 1, 2 and 3. here the
;; suffix mul indicates that type (up-)conversion is forced by a
;; multiplication. other suffixes are floor for conversion into fixed
;; type and coerce. coerce seemed to be quite slow last time i
;; tried. thats why i prefer the multiplication for most conversions.

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
			     (:coerce `(coerce x ',(get-long-type out-type)))
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
		    
		    ,(def 'fix 'sf)
		    ,(def 'fix 'df)
		    ,(def 'fix 'csf)
		    ,(def 'fix 'cdf)
		    

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
		    ,(def 'sf 'fix :floor)
		    ,(def 'df 'fix :floor)

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


;; converting complex into real with some function and converting into
;; out_type, the name of the functions will be like:
;; normalize-3-cdf/ub8-realpart. the function is evaluated into an
;; intermediate real array (either double or float depending on the
;; input type) and then normalized into the result. float results are
;; in 0..1 and ub8 results in 0..255.
(def-generator (normalize-complex (rank type out_type func short_func))
  (let ((long-out-type (get-long-type out_type))
	;; override the name that is constructed by def-generator
	(name (format-symbol "normalize-~a-~a/~a-~a" 
			     rank type out_type short_func))
	(intermed-type (ecase type
			 (cdf 'double-float)
			 (csf 'single-float))))
    (store-new-function name)
    `(defun ,name (a)
       (declare ((simple-array ,long-type ,rank) a)
		(values (simple-array ,long-out-type ,rank) &optional))
       (let* ((result (make-array (array-dimensions a)
				  :element-type ',long-out-type))
	      (result1 (sb-ext:array-storage-vector result))
	      (a1 (sb-ext:array-storage-vector a))
	      (n (length a1))
	      (intermediate1
	       (make-array n :element-type ',intermed-type)))
	 (dotimes (i n)
	   (setf (aref intermediate1 i) (funcall ,func (aref a1 i))))
	 (let* ((ma (reduce #'max intermediate1))
		(mi (reduce #'min intermediate1))
		(s (if (= ma mi)
		       (coerce 0 ',intermed-type)
		       (/ (- ma mi)))))
	   (dotimes (i n)
	     (let ((v (* s (- (aref intermediate1 i) mi))))
	      (setf (aref result1 i) ,(if (eq 'ub8 out_type)
					  `(floor (* 255 v))
					  `(* (coerce 1 ',long-out-type) v)))))
	 result)))))

#+nil
(def-normalize-complex-rank-type-out_type-func-short_func
    1 csf ub8 #'realpart realpart)

(defmacro def-normalize-complex-functions (ranks out-types funcs)
  (let ((result nil))
    (loop for rank in ranks do
	 (loop for type in '(csf cdf) do
	      (loop for otype in out-types do
		   (loop for func in funcs do
			(push `(def-normalize-complex-rank-type-out_type-func-short_func ,rank ,type ,otype #',func ,func)
			      result)))))
    `(progn ,@result)))

(def-normalize-complex-functions
    (1 2 3) (ub8 sf df) (realpart imagpart phase abs))

#+nil
(normalize-1-csf/ub8-realpart 
 (make-array 3 :element-type '(complex single-float)
	     :initial-contents '(#C(1s0 0s0) #C(2s0 0s0) #C(3s0 0s0))))

#+nil
(normalize-1-csf/df-phase
 (make-array 3 :element-type '(complex single-float)
	     :initial-contents '(#C(1s0 .2s0) #C(2s0 1s0) #C(3s0 0s0))))

;; normalize real arrays, name like: normalize-2-sf/ub8
(def-generator (normalize (rank type out_type))
  (let ((long-out-type (get-long-type out_type))
	;; override the name that is constructed by def-generator
	(name (format-symbol "normalize-~a-~a/~a" 
			     rank type out_type)))
    (store-new-function name)
    `(defun ,name (a)
       (declare ((simple-array ,long-type ,rank) a)
		(values (simple-array ,long-out-type ,rank) &optional))
       (let* ((result (make-array (array-dimensions a)
				  :element-type ',long-out-type))
	      (result1 (sb-ext:array-storage-vector result))
	      (a1 (sb-ext:array-storage-vector a))
	      (n (length a1))
	      (ma (reduce #'max a1))
	      (mi (reduce #'min a1))
	      (s (if (= ma mi)
		     (coerce 0 ',(get-long-type type))
		     (/ (- ma mi)))))
	 (dotimes (i n)
	   (let ((v (* s (- (aref a1 i) mi))))
	     (setf (aref result1 i) ,(if (eq 'ub8 out_type)
					 `(floor (* 255 v))
					 `(* (coerce 1 ',long-out-type) v)))))
	 result))))
#+nil
(def-normalize-rank-type-out_type 1 df ub8)
#+nil
(normalize-1-df/ub8 (make-array 3 :element-type 'double-float
				:initial-contents '(1d0 2d0 3d0)))

(defmacro def-normalize-functions (ranks in-types out-types)
  (let ((result nil))
    (loop for rank in ranks do
	 (loop for type in in-types do
	      (loop for otype in out-types do
		   (push `(def-normalize-rank-type-out_type ,rank ,type ,otype)
			 result))))
    `(progn ,@result)))

(def-normalize-functions (1 2 3) (ub8 sf df) (ub8 sf df))

#+nil
(normalize-1-sf/sf (make-array 3 :element-type 'single-float
				:initial-contents '(1s0 2s0 3s0)))
#+nil
(normalize-1-sf/ub8 (make-array 3 :element-type 'single-float
				:initial-contents '(1s0 2s0 3s0)))
#+nil
(normalize-1-ub8/sf (make-array 3 :element-type '(unsigned-byte 8)
				:initial-contents '(1 2 3)))
#+nil
(normalize-1-ub8/ub8 (make-array 3 :element-type '(unsigned-byte 8)
				:initial-contents '(1 2 3)))

#+nil ;; find the names of all the functions that were defined by the
      ;; above macros, for exporting
;; 2010-08-14 I don't think I need this anymore
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
