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
(.*-1-sf
 (make-array 3
	     :element-type 'single-float
	     :initial-contents '(1s0 2s0 3s0))
 (make-array 3
	     :element-type 'single-float
	     :initial-contents '(2s0 2s0 3s0)))

(defmacro def-point-wise-functions (ops ranks types)
  (let ((result nil))
    (loop for rank in ranks do
	(loop for type in types do
	     (loop for op in ops do
		  (push `(def-point-wise-op ,op ,rank ,type)
			result))))
    `(progn ,@result)))

(def-point-wise-functions (+ - * /) (1 2 3) (ub8 sf df csf cdf))

(defun .*2 (vola volb)
  (declare ((simple-array (complex my-float) 2) vola volb)
	   (values (simple-array (complex my-float) 2) &optional))
  (let ((result (make-array (array-dimensions vola)
			    :element-type (array-element-type vola))))
   (destructuring-bind (y x)
       (array-dimensions vola)
     (do-rectangle (j i 0 y 0 x)
       (setf (aref result j i)
	     (* (aref vola j i)
		(aref volb j i)))))
   result))

(defun .+2 (vola volb)
  (declare ((simple-array (complex my-float) 2) vola volb)
	   (values (simple-array (complex my-float) 2) &optional))
  (let ((result (make-array (array-dimensions vola)
			    :element-type (array-element-type vola))))
   (destructuring-bind (y x)
       (array-dimensions vola)
     (do-rectangle (j i 0 y 0 x)
       (setf (aref result j i)
	     (+ (aref vola j i)
		(aref volb j i)))))
   result))

(defun .* (vola volb &optional volb-start)
  "Elementwise multiplication of VOLA and VOLB. Both volumes must have
the same dimensions or VOLB must be smaller in all dimensions. In the
latter case a vec-i has to be supplied in VOLB-START to define the
relative position of VOLB inside VOLA."
  (declare ((simple-array (complex my-float) 3) vola volb)
	   ((or null vec-i) volb-start)
	   (values (simple-array (complex my-float) 3) &optional))
  (let ((result (make-array (array-dimensions volb)
			    :element-type '(complex my-float))))
   (destructuring-bind (z y x)
       (array-dimensions vola)
     (destructuring-bind (zz yy xx)
	 (array-dimensions volb)
       (if volb-start
	   ;; fill the result with volb multiplied by the
	   ;; corresponding values from the bigger vola
	   (let ((sx (vec-i-x volb-start))
		 (sy (vec-i-y volb-start))
		 (sz (vec-i-z volb-start)))
	     (unless (and (<= zz (+ z sz))
			  (<= yy (+ y sy))
			  (<= xx (+ x sx)))
	       (error "VOLB isn't contained in VOLA when shifted by VOLB-START. ~a" 
		      (list zz (+ z sz))))
	     (do-box (k j i 0 zz 0 yy 0 xx)
	       (setf (aref result k j i)
		     (* (aref volb k j i)
			(aref vola (+ k sz) (+ j sy) (+ i sx))))))
	   (progn 
	     (unless (and (= z zz) (= y yy) (= x xx))
	       (error "volumes don't have the same size, maybe you can supply a start vector."))
	     (do-box (k j i 0 z 0 y 0 x)
	       (setf (aref result k j i)
		     (* (aref vola k j i)
			(aref volb k j i))))))))
   result))

(defun .+ (vola volb &optional (volb-start (make-vec-i)))
  (declare ((simple-array (complex my-float) 3) vola volb)
	   (vec-i volb-start)
	   (values (simple-array (complex my-float) 3) &optional))
  (let ((result (make-array (array-dimensions volb)
			    :element-type '(complex my-float))))
    (destructuring-bind (z y x)
	(array-dimensions volb)
      (let ((sx (vec-i-x volb-start))
	    (sy (vec-i-y volb-start))
	    (sz (vec-i-z volb-start)))
	(do-box (k j i 0 z 0 y 0 x)
	  (setf (aref result k j i)
		(+ (aref vola (+ k sz) (+ j sy) (+ i sx))
		   (aref volb k j i))))))
    result))

(defun .- (vola volb &optional (volb-start (make-vec-i)))
  (declare ((simple-array (complex my-float) 3) vola volb)
	   (vec-i volb-start)
	   (values (simple-array (complex my-float) 3) &optional))
  (let ((result (make-array (array-dimensions volb)
			    :element-type '(complex my-float))))
    (destructuring-bind (z y x)
	(array-dimensions volb)
      (let ((sx (vec-i-x volb-start))
	    (sy (vec-i-y volb-start))
	    (sz (vec-i-z volb-start)))
	(do-box (k j i 0 z 0 y 0 x)
	  (setf (aref result k j i)
		(- (aref vola (+ k sz) (+ j sy) (+ i sx))
		   (aref volb k j i))))))
    result))

(declaim (ftype (function (my-float (simple-array (complex my-float) 3))
			  (values (simple-array (complex my-float) 3) &optional))
		s*))
(defun s* (s vol)
  (let* ((a (sb-ext:array-storage-vector vol))
	 (n (length a)))
    (dotimes (i n)
      (setf (aref a i) (* s (aref a i)))))
  vol)

(defun s*2 (s vol)
  (declare (my-float s)
	   ((simple-array (complex my-float) 2) vol)
	   (values (simple-array (complex my-float) 2) &optional))
  (let* ((a (sb-ext:array-storage-vector vol))
	 (n (length a)))
    (dotimes (i n)
      (setf (aref a i) (* s (aref a i)))))
  vol)


(defun mean-realpart (a)
  "Calculate the average value over all the samples in volume A."
  (declare ((simple-array (complex my-float) *) a)
	   (values my-float &optional))
  (let* ((a1 (sb-ext:array-storage-vector a))
	 (sum zero)
	 (n (length a1)))
    (dotimes (i n)
      (incf sum (realpart (aref a1 i))))
    (/ sum n)))