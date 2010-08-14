;; some operations that are defined between and on n-arrays,
;; e.g. point-wise addition or averaging
(in-package :vol)

;; the functions .* .+ ./ .- will do a point-wise operation on an
;; n-array. here I define specific functions like: .*-2-df. Both
;; volumes A and B must have the same dimensions or B must be smaller
;; in all dimensions. In the latter case a vec-i has to be supplied in
;; B-START to define the relative position of B inside A.

(def-generator (point-wise (op rank type))
  (let ((name (format-symbol ".~a-~a-~a" op rank type)))
    (store-new-function name)
    `(defun ,name (a b &optional (b-start (make-vec-i)))
       (declare ((simple-array ,long-type ,rank) a b)
		(vec-i b-start)
		(values (simple-array ,long-type ,rank) &optional))
       (let ((result (make-array (array-dimensions b)
				 :element-type ',long-type)))
	 ,(ecase rank
	    (1 `(destructuring-bind (x)
		   (array-dimensions b)
		 (let ((sx (vec-i-x b-start)))
		   (do-region ((i) (x))
		     (setf (aref result i)
			   (+ (aref a (+ i sx))
			      (aref b i)))))))
	    (2 `(destructuring-bind (y x)
		   (array-dimensions b)
		 (let ((sx (vec-i-x b-start))
		       (sy (vec-i-y b-start)))
		   (do-region ((j i) (y x))
		     (setf (aref result j i)
			   (+ (aref a (+ j sy) (+ i sx))
			      (aref b j i)))))))
	    (3 `(destructuring-bind (z y x)
		   (array-dimensions b)
		 (let ((sx (vec-i-x b-start))
		       (sy (vec-i-y b-start))
		       (sz (vec-i-z b-start)))
		   (do-region ((k j i) (z y x))
		     (setf (aref result k j i)
			 (+ (aref a (+ k sz) (+ j sy) (+ i sx))
			    (aref b k j i))))))))
	 result))))
#+nil
(def-point-wise-op-rank-type * 1 sf)
#+nil
(def-point-wise-op-rank-type * 1 cdf)
#+nil
(.*-1-sf
 (make-array 3
	     :element-type 'single-float
	     :initial-contents '(1s0 2s0 3s0))
 (make-array 3
	     :element-type 'single-float
	     :initial-contents '(2s0 2s0 3s0)))

(defmacro def-point-wise-functions (ops ranks types)
  (let ((specific-funcs nil)
	(generic-funcs nil))
    (loop for rank in ranks do
	 (loop for type in types do
	      (loop for op in ops do
		   (push `(def-point-wise-op-rank-type ,op ,rank ,type)
			 specific-funcs))))
    (loop for op in ops do
	 (let ((cases nil))
	   (loop for rank in ranks do
		(loop for type in types do
		     (push `((simple-array ,(get-long-type type) ,rank)
			     (,(format-symbol  ".~a-~a-~a" op rank type) 
			       a b b-start))
			   cases)))
	   (let ((name (format-symbol ".~a" op)))
	     (store-new-function name)
	    (push `(defun ,name (a b &optional (b-start (make-vec-i)))
		       (etypecase a
			 ,@cases
			 (t (error "The given type can't be handled with a generic
		 point-wise function."))))  
		  generic-funcs))))
    `(progn ,@specific-funcs
	    ,@generic-funcs)))

(def-point-wise-functions (+ - * /) (1 2 3) (ub8 sf df csf cdf))

#+nil
(.* (make-array 3 :element-type 'single-float
		:initial-contents '(1s0 2s0 3s0))
    (make-array 3 :element-type 'single-float
		:initial-contents '(2s0 2s0 3s0)))

;; now I define multiplication and addition with a scalar. I can't
;; remember that I needed those, therefore the slower non-generic
;; implementation.
(defun s* (s vol)
  (let* ((a (sb-ext:array-storage-vector vol))
	 (n (length a)))
    (dotimes (i n)
      (setf (aref a i) (* s (aref a i)))))
  vol)
(defun s+ (s vol)
  (let* ((a (sb-ext:array-storage-vector vol))
	 (n (length a)))
    (dotimes (i n)
      (setf (aref a i) (+ s (aref a i)))))
  vol)

(def-generator (mean (rank type))
  `(defun ,name (a)
     "Calculate the average value over all the samples in array A."
     (declare ((simple-array ,long-type ,rank) a)
	      (values ,long-type &optional))
     (let* ((a1 (sb-ext:array-storage-vector a))
	    (sum (coerce 0 ',long-type))
	    (n (length a1)))
       (declare (,(if (eq type 'ub8)
		      'integer
		      'long-type) sum))
       (dotimes (i n)
	 (incf sum (aref a1 i)))
       (/ sum n))))
#+nil
(def-mean-rank-type 1 sf)
#+nil
(mean-1-sf
 (make-array 3 :element-type 'single-float
	     :initial-contents '(2s0 2s0 3s0)))

(defmacro def-mean-functions (ranks types)
  (let ((result nil)
	(cases nil))
    (loop for rank in ranks do
	 (loop for type in types do
	      (push `(def-mean-rank-type ,rank ,type)
		    result)))
    (loop for rank in ranks do
	 (loop for type in types do
	      (push `((simple-array ,(get-long-type type) ,rank)
		      (,(format-symbol  "mean-~a-~a" rank type) 
			a))
		    cases)))
    (store-new-function "mean")
    `(progn ,@result
	    (defun mean (a)
	     (etypecase a
	       ,@cases
	       (t (error "The given type can't be handled with a generic
		 averaging function mean.")))))))

(def-mean-functions (1 2 3) (ub8 sf df csf cdf))

#+nil
(mean
 (make-array 4 :element-type 'single-float
	     :initial-contents '(2s0 2s0 3s0 3s0)))