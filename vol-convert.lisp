(in-package :vol)

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
		    (my-float . df)
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

(def-convert (unsigned-byte 8) (complex my-float)
		  #'(lambda (c) (complex (* one c))) complex)
(def-convert (unsigned-byte 8) (complex single-float)
		  #'(lambda (c) (complex (* 1s0 c))) complex)
(def-convert my-float (complex my-float)
	     #'(lambda (d) (complex d)) complex)
(def-convert my-float (unsigned-byte 8) #'floor)
(def-convert (complex my-float) my-float #'realpart)
(def-convert (complex my-float) my-float #'imagpart)
(def-convert (complex my-float) my-float #'abs)
(def-convert (complex my-float) my-float #'phase)

(def-convert (complex single-float) single-float #'realpart)
(def-convert (complex single-float) single-float #'imagpart)
(def-convert (complex single-float) single-float #'abs)
(def-convert (complex single-float) single-float #'phase)

(def-convert (complex my-float) (unsigned-byte 8)
	     #'(lambda (z) (floor (realpart z)))
	     realpart)
(def-convert (complex my-float) (unsigned-byte 8)
	     #'(lambda (z) (floor (imagpart z)))
	     imagpart)
(def-convert (complex my-float) (unsigned-byte 8)
	     #'(lambda (z) (floor (phase z)))
	     phase)
(def-convert (complex my-float) (unsigned-byte 8)
	     #'(lambda (z) (floor (abs z)))
	     abs)

(def-convert (complex single-float) (complex my-float)
  #'(lambda (z) (coerce z '(complex my-float)))
  coerce)

#+nil
(convert3-cdf/ub8-realpart
 (make-array (list 3 3 3) :element-type '(complex my-float)))

#+nil
(convert3-csf/cdf-coerce
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
