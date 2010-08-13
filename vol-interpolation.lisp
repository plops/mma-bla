(in-package :vol)

;; pbourke linearly interpolating points within a box
(def-generator (interpolate (rank type))
  (let ((params (ecase rank
		  (1 `(xx))
		  (2 `(yy xx))
		  (3 `(zz yy xx)))))
    `(defun ,name (vol ,@params)
       "Linear interpolation of a value."
      (declare (inline) 
	       ((simple-array ,long-type ,rank) vol)
	       (single-float ,@params)
	       (values ,long-type &optional))
      (let ((zero ,(coerce 0 long-type))
	    (one ,(coerce 1 long-type)))
	,(ecase rank
		(1 `(multiple-value-bind (i x)
			(floor xx)
		      (labels ((ref (a)
				 (let ((x (+ i a)))
				   (if (array-in-bounds-p vol x)
				       (aref vol x)
				       zero))))
			(let ((o (ref 0))
			      (i (ref 1))
			      (a (- one x)))
			  (+ (* o a)
			     (* i x))))))
		(2 `(multiple-value-bind (i x)
			(floor xx)
		      (multiple-value-bind (j y)
			  (floor yy)
			(labels ((ref (a b)
				   (let ((x (+ i a))
					 (y (+ j b)))
				     (if (array-in-bounds-p vol y x)
					 (aref vol y x)
					 zero))))
			  (let ((oo (ref 0 0))
				(io (ref 1 0))
				(oi (ref 0 1))
				(ii (ref 1 1))
				(a (- one x))
				(b (- one y)))
			    (+ (* oo a b)
			       (* io x b)
			       (* oi a y)
			       (* ii x y)))))))
		(3 `(multiple-value-bind (i x)
			(floor xx)
		      (multiple-value-bind (j y)
			  (floor yy)
			(multiple-value-bind (k z)
			    (floor zz)
			  (labels ((ref (a b c)
				     (let ((x (+ i a))
					   (y (+ j b))
					   (z (+ k c)))
				       (if (array-in-bounds-p vol z y x)
					   (aref vol z y x)
					   zero))))
			    (let ((ooo (ref 0 0 0))
				  (ioo (ref 1 0 0))
				  (oio (ref 0 1 0))
				  (ooi (ref 0 0 1))
				  (iio (ref 1 1 0))
				  (ioi (ref 1 0 1))
				  (oii (ref 0 1 1))
				  (iii (ref 1 1 1))
				  (a (- one x))
				  (b (- one y))
				  (c (- one z)))
			      (+ (* ooo a b c)
				 (* ioo x b c)
				 (* oio a y c)
				 (* ooi a b z)
				 (* iio x y c)
				 (* ioi x b z)
				 (* oii a y z)
				 (* iii x y z)))))))))))))
#+nil
(def-interpolate-rank-type 1 sf)
#+nil
(let ((a (make-array 3
		     :element-type 'single-float 
		     :initial-contents '(1.0 2.0 3.0))))
 (interpolate-1-sf a 1.5))

(defmacro def-interpolate-functions (ranks types)
  (let ((functions nil)
	(cases nil))
    (loop for rank in ranks do
	 (loop for type in types do
	      (push `(def-interpolate-rank-type ,rank ,type)
		    functions)))
    (loop for rank in ranks do
		       (loop for type in types do
			    (let ((long-type (get-long-type type)))
			      (push `((simple-array ,long-type ,rank)
				      (,(format-symbol 
					 "interpolate-~a-~a" rank type)
					a ,@(subseq '(x y z) 0 rank)))
				    cases))))
    `(progn ,@functions
	    (defun interpolate (a x &optional y z)
	      (declare ((or null single-float) x y z))
	      (etypecase a
		,@cases
	 (t (error "type not supported.")))))))

(def-interpolate-functions (1 2 3) (sf df csf cdf))

#+nil
(let* ((n 3)
       (a (make-array (list n n n) 
		      :element-type 'single-float))
       (a1 (sb-ext:array-storage-vector a)))
  (dotimes (i (length a1))
    (setf (aref a1 i) (* 1s0 i)))
  (interpolate-3-sf a 1.2s0 1.3s0 1.2s0)
  (interpolate a 1.2s0 1.3s0 1.2s0))


(def-generator (resample-half (rank type))
  `(defun ,name (vol)
     (declare ((simple-array ,long-type ,rank) vol)
	      (values (simple-array ,long-type ,rank) &optional))
     (let ((dims (array-dimensions vol)))
       ,(ecase 
	 rank
	 (1 `(destructuring-bind (x)
		 dims
	       (let* ((xx (floor x 2))
		      (small (make-array (list xx)
					:element-type (quote ,long-type))))
		 (do-region ((i) (xx))
		   (setf (aref small i)
			 (aref vol (* i 2))))
		 small)))
	 (2 `(destructuring-bind (y x)
		 dims
	       (let* ((xx (floor x 2))
		      (yy (floor y 2))
		      (small (make-array (list yy xx)
					 :element-type (quote ,long-type))))
		(do-region ((j i) (yy xx))
		  (setf (aref small j i)
			(aref vol (* j 2) (* i 2))))
		small)))
	 (3 `(destructuring-bind (z y x)
		 dims
	       (let* ((xx (floor x 2))
		      (yy (floor y 2))
		      (zz (floor z 2))
		      (small (make-array (list zz yy xx)
					 :element-type (quote ,long-type))))
		(do-region ((k j i) (zz yy xx))
		  (setf (aref small k j i)
			(aref vol (* k 2) (* j 2) (* i 2))))
		small)))))))
#+nil
(def-resample-half-rank-type 1 ub8)

(defmacro def-resample-half-functions (ranks types)
  (let ((result nil))
    (loop for rank in ranks do
	 (loop for type in types do
	     (push `(def-resample-half-rank-type ,rank ,type)
		   result)))
    `(progn ,@result)))


(def-resample-half-functions (1 2 3) (ub8 sf df cdf csf))

#+nil
(resample-half-1-ub8 
 (let ((l '(1 2 3 4 5 6)))
   (make-array (length l) :element-type '(unsigned-byte 8)
	       :initial-contents l)))



;; The following function is meant to resample an angular PSF that has
;; been calculated with too much resolution. The important thing is
;; that the central sample stays central. Consider a 1D array with n
;; elements and stepsize dx: The length of the array will be
;; X=n*dx. There are c=center(n) samples right of the center and b
;; samples (with b+c=n) left of the center. The new step size dx'
;; suggests that c'=(floor c*dx dx') samples are needed right of the
;; center and analogously b' samples left of the center. The size of
;; the new array is n'=b'+c'. The float coordinate x' for the new
;; sample is (i+b')*dx' where i runs from -b' below c'. The central
;; sample is i=0. This following function implements this scheme for 3
;; dimensions.
(def-generator (resample (rank type))
  (let ((par-deltas (ecase rank (1 `(dx)) (2 `(dx dy)) (3 `(dx dy dz))))
	(par-deltas^ (ecase rank (1 `(dx^)) (2 `(dx^ dy^)) (3 `(dx^ dy^ dz^)))))
 `(defun ,name (vol ,@par-deltas ,@par-deltas^)
    "Resample by linear interpolation. Ensure that the
  former center stays in the center."
    (declare ((simple-array ,long-type ,rank) vol)
	     (single-float ,@par-deltas ,@par-deltas^)
	     (values (simple-array ,long-type ,rank) &optional))
    (labels ((center (n) ;; give the index of the central sample
	       (declare (fixnum n)
			(values fixnum &optional))
	       (let ((c (if (evenp n)
			    (ceiling n 2)
			    (floor n 2))))
		 c))
	     (dims (n d d^) ;; calculate new array size from old one and deltas
	       (declare (fixnum n)
			(my-float d d^)
			(values fixnum fixnum fixnum &optional))
	       (let* ((c (center n))
		      (b (- n c))
		      (c^ (floor (* c d) d^))
		      (b^ (floor (* b d) d^))
		      (n^ (+ c^ b^)))
		 (values b^ c^ n^))))
      (macrolet ((coord (i dir) ;; convert index into float coordinate
		   `(* (/ ,(format-symbol "D~a^" dir)
			   ,(format-symbol "D~a" dir))
			(+ ,i ,(format-symbol "B~a" dir)))))
	,(ecase rank
		(1
		 `(destructuring-bind (x)
		      (array-dimensions vol)
		    (multiple-value-bind (bx cx nx)
			(dims x dx dx^)
		      (let ((result (make-array (list nx)
						:element-type (quote ,long-type))))
			(do-region ((i) (cx) ((- bx)))
			  (let ((xx (coord i x)))
			   (setf (aref result (+ bx i))
				 (,(format-symbol "interpolate-~a-~a" rank type) vol xx))))
			result))))
		(2
		`(destructuring-bind (y x)
		     (array-dimensions vol)
		   (multiple-value-bind (bx cx nx)
		       (dims x dx dx^)
		     (multiple-value-bind (by cy ny)
			 (dims y dy dy^)
		       (let ((result (make-array (list ny nx)
						 :element-type (quote ,long-type))))
			 (do-region ((j i) (cy cx) ((- by) (- bx)))
			   (let ((xx (coord i x))
				 (yy (coord j y)))
			     (setf (aref result (+ by j) (+ bx i))
				   (,(format-symbol "interpolate-~a-~a" rank type) vol xx yy))))
			 result)))))
	       (3
		`(destructuring-bind (z y x)
		     (array-dimensions vol)
		   (multiple-value-bind (bx cx nx)
		       (dims x dx dx^)
		     (multiple-value-bind (by cy ny)
			 (dims y dy dy^)
		       (multiple-value-bind (bz cz nz)
			   (dims z dz dz^)
			 (let ((result (make-array (list nz ny nx)
						   :element-type (quote ,long-type))))
			   (do-region ((k j i) (cz cy cx) ((- bz) (- by) (- bx)))
			     (let ((xx (coord i x))
				   (yy (coord j y))
				   (zz (coord k z)))
			       (setf (aref result (+ bz k) (+ by j) (+ bx i))
				     (,(format-symbol "interpolate-~a-~a" rank type) vol xx yy zz))))
			   result))))))))))))
#+nil
(def-resample-rank-type 1 sf)
#+nil
(let ((a (make-array 3 :element-type 'single-float :initial-contents '(1s0 2s0 3s0))))
 (resample-1-sf a 1.0 .5))

(defmacro def-resample-functions (ranks types)
  (let ((result nil))
    (loop for rank in ranks do
	 (loop for type in types do
	      (push `(def-resample-rank-type ,rank ,type)
		    result)))
    `(progn ,@result)))

(def-resample-functions (1 2 3) (sf df csf cdf))

#+nil
(time
 (let* ((s0 (convert3-ub8/cdf-complex (draw-sphere-ub8 12d0 41 239 232)))
	(dr 160d0)
	(dz 300d0)
	(ddr 200d0)
	(ddz 1000d0)
	(s1 (resample3-cdf s0 dr dr dz ddr ddr ddz))
	(s2 (resample3-cdf s1 ddr ddr ddz dr dr dz)))
   (save-stack-ub8 "/home/martin/tmp/s0/" (normalize3-cdf/ub8-realpart s0))
   (save-stack-ub8 "/home/martin/tmp/s1/" (normalize3-cdf/ub8-realpart s1))
   (save-stack-ub8 "/home/martin/tmp/s2/" (normalize3-cdf/ub8-realpart s2))
   (save-stack-ub8 "/home/martin/tmp/s2-s0" (normalize3-cdf/ub8-realpart (.- s0 s2)))))


(def-generator (cross-section-xz (type))
  `(defun ,name (a &optional (y (floor (array-dimension a 1) 2)))
     (declare ((simple-array ,long-type 3) a)
	      (fixnum y)
	      (values (simple-array ,long-type 2)
		      &optional))
     (destructuring-bind (z y-size x)
	 (array-dimensions a)
       (unless (<= 0 y (1- y-size))
	 (error "Y is out of bounds."))
       (let ((b (make-array (list z x)
			    :element-type (quote ,long-type))))
	 (do-region ((j i) (z x))
	   (setf (aref b j i)
		 (aref a j y i)))
	 b))))

#+nil
(def-cross-section-xz-type sf)

#+nil
(let ((a (make-array (list 2 2 2) :element-type 'single-float)))
 (cross-section-xz-sf a))

(defmacro def-cross-section-xz-functions (types)
  `(progn
     ,@(loop for type in types collect
	    `(def-cross-section-xz-type ,type))
     (store-new-function (format-symbol "cross-section-xz"))
     (defun cross-section-xz (a &optional (y (floor (array-dimension a 1) 2)))
       (declare (fixnum y))
       (etypecase a
	 ,@(loop for type in types collect
		(let ((long-type (get-long-type type)))
		       `((simple-array ,long-type 3) (,(format-symbol 
							 "cross-section-xz-~a" type) a y))))
	 (t (error "type not supported."))))))

(def-cross-section-xz-functions (ub8 sf df csf cdf))

#+nil
(let ((a (make-array (list 2 2 2) :element-type 'single-float)))
 (cross-section-xz a))

(def-generator (decimate-xy (type))
 `(defun ,name (dx vol)
   "Increase transversal sampling distance by odd integer factor DX."
   (declare (fixnum dx)
	    ((simple-array ,long-type 3) vol)
	    (values (simple-array ,long-type 3) &optional))
   (unless (eq (mod dx 2) 1)
     (error "Factor DX has to be odd."))
   (destructuring-bind (z y x)
       (array-dimensions vol)
     (let* ((dx2 (* dx dx))
	    (nx (floor x dx))
	    (ny (floor y dx))
	    (result (make-array (list z ny nx)
				:element-type (quote ,long-type))))
       (do-region ((k j i) (nx ny z))
	 (let ((sum 0))
	   (do-region ((jj ii) (dx dx))
	     (incf sum (aref vol
			     k
			     (+ (* dx j) jj)
			     (+ (* dx i) ii))))
	   (setf (aref result k j i) (floor sum dx2))))
       result))))

(defmacro def-decimate-xz-functions (types)
  `(progn
     ,@(loop for type in types collect
	    `(def-decimate-xy-type ,type))))

(def-decimate-xz-functions (ub8 sf df csf cdf))

;; for dx=5:
;; 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3
;; x x x q x
;;           x o x x x
;;                     x x x x x
;;                               x x x x x
;;                                         x x x x ?
;; dxh=2, n=24
;; output size floor(n,dx)=4
;; position of q: ii=3, i=0: dx*i+ii=5*0+3=3
;; position of o: ii=1, i=1: dx*i+ii=5+1=6

#+nil
(decimate-xy-ub8 5 (make-array (list 2 10 10)
			       :element-type '(unsigned-byte 8)))