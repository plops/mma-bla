(in-package :vol)

(declaim (ftype (function ((simple-array (unsigned-byte 8) 2)
			   fixnum fixnum)
			  (values (unsigned-byte 8) &optional))
		aref2-zero-ub8))
(defun aref2-zero-ub8 (a j i)
  "Like AREF but return zero if subscripts J and I point outside of
the array bounds."
  (if (array-in-bounds-p a j i)
      (aref a j i)
      0))

(declaim (ftype (function ((simple-array my-float 2)
			   fixnum fixnum)
			  (values my-float &optional))
		aref2-zero-df))
(defun aref2-zero-df (a j i)
  "Like AREF but return zero if subscripts J and I point outside of
the array bounds."
  (if (array-in-bounds-p a j i)
      (aref a j i)
      zero))

(defun aref2-zero-cdf (a j i)
  "Like AREF but return zero if subscripts J and I point outside of
the array bounds."
  (declare ((simple-array (complex double-float) 2) a)
	   (fixnum j i)
	   (values (complex double-float) &optional))
  (if (array-in-bounds-p a j i)
      (aref a j i)
      (complex 0d0)))

(defun aref2-zero-csf (a j i)
  "Like AREF but return zero if subscripts J and I point outside of
the array bounds."
  (declare ((simple-array (complex single-float) 2) a)
	   (fixnum j i)
	   (values (complex single-float) &optional))
  (if (array-in-bounds-p a j i)
      (aref a j i)
      (complex 0s0)))

(defmacro def-interp1 ()
  (let ((types '((ub8 (unsigned-byte 8) my-float)
		 (sf single-float single-float)
		 (csf (complex single-float) (complex single-float))
		 (df double-float double-float)
		 (cdf (complex double-float) (complex double-float)))))
   `(progn
      ,@(loop for ty in types collect
	     (destructuring-bind (short long retour)
		 ty
	       (let ((fun (intern (format nil "INTERP1-~A" short))))
		 `(progn 
		   (declaim (inline ,fun))
		   (defun ,fun (a b xi)
		    "Interpolate between values A and B. Returns A for
  xi=0 and B for xi=1."
		    (declare (,long a b)
			     (my-float xi)
			     (values ,retour &optional))
		    (+ (* (- one xi) a) (* xi b))))))))))

(def-interp1)

(declaim (inline interp1))
(defun interp1 (a b xi)
  (declare ((or (unsigned-byte 8)
		my-float
		(complex my-float)) a b)
	   (my-float xi)
	   (values (or my-float (complex my-float)) &optional))
  (etypecase a
    ((unsigned-byte 8) (interp1-ub8 a b xi))
    (single-float (interp1-sf a b xi))
    (double-float (interp1-df a b xi))
    ((complex single-float) (interp1-csf a b xi))
    ((complex double-float) (interp1-cdf a b xi))))

(declaim (ftype (function ((simple-array (unsigned-byte 8) 2)
			   my-float my-float)
			  (values my-float &optional))
		interpolate2-ub8))
(defun interpolate2-ub8 (img x y) 
  "Bilinear interpolation on an image IMG. The coordinates X and Y can
be floating point values. If they point outside of IMG 0 is returned."
  (multiple-value-bind (i ix)
      (floor x)
    (multiple-value-bind (j jx)
	(floor y)
      ;; values on grid points, top left is i,j and stored in a
      ;; |
      ;;-a-b
      ;; |
      ;; c d
      (let ((a (aref img i j))
	    (b (aref2-zero-ub8 img (1+ i) j))
	    (c (aref2-zero-ub8 img i (1+ j)))
	    (d (aref2-zero-ub8 img (1+ i) (1+ j))))
	;; now interpolate verticals
	;; a  b
	;; |  |
	;; e  f
	;; |  |
	;; c  d
	(let ((e (interp1-ub8 a c jx))
	      (f (interp1-ub8 b d jx)))
	  ;; and now horizontal
	  ;; e - g - f
	  (let ((g (interp1 e f ix)))
	    g))))))

(declaim (ftype (function ((simple-array my-float 2)
			   my-float my-float)
			  (values my-float &optional))
		interpolate2-df))
(defun interpolate2-df (img x y) 
  "Bilinear interpolation on an image IMG. The coordinates X and Y can
be floating point values. If they point outside of IMG 0 is returned."
  (multiple-value-bind (i ix)
      (floor x)
    (multiple-value-bind (j jx)
	(floor y)
      (let ((a (aref img i j))
	    (b (aref2-zero-df img (1+ i) j))
	    (c (aref2-zero-df img i (1+ j)))
	    (d (aref2-zero-df img (1+ i) (1+ j))))
	(let ((e (interp1-df a c jx))
	      (f (interp1-df b d jx)))
	  (let ((g (interp1-df e f ix)))
	    g))))))

(defun interpolate2-cdf (img x y) 
  "Bilinear interpolation on an image IMG. The coordinates X and Y can
be floating point values. If they point outside of IMG 0 is returned."
  (declare ((simple-array (complex double-float) 2) img)
	   (double-float x y)
	   (values (complex double-float) &optional))
  (multiple-value-bind (i ix)
      (floor x)
    (multiple-value-bind (j jx)
	(floor y)
      (let ((a (aref img i j))
	    (b (aref2-zero-cdf img (1+ i) j))
	    (c (aref2-zero-cdf img i (1+ j)))
	    (d (aref2-zero-cdf img (1+ i) (1+ j))))
	(let ((e (interp1-cdf a c jx))
	      (f (interp1-cdf b d jx)))
	  (let ((g (interp1-cdf e f ix)))
	    g))))))


(declaim (ftype (function ((simple-array * 2) my-float my-float)
			  (values (or double-float
				      (complex double-float)) &optional))
		interpolate2))
(defun interpolate2 (img x y)
  "Bilinear interpolation on an image IMG. The coordinates X and Y can
be floating point values. If they point outside of IMG 0 is returned."
  (etypecase img
    ((simple-array (unsigned-byte 8) 2) (interpolate2-ub8 img x y))
    ((simple-array my-float 2) (interpolate2-df img x y))
    ((simple-array (complex my-float) 2) (interpolate2-cdf img x y))))

#+nil
(let ((a (make-array (list 2 2) :element-type '(unsigned-byte 8)
		     :initial-contents '((1 2) (2 3)))))
  (interpolate2 a half .2d0)) 

;; pbourke linearly interpolating points within a box
(defun interpolate3-cdf (vol xx yy zz)
  "Trilinear interpolation of a value from a volumetric stack."
  (declare (inline) 
   ((simple-array (complex my-float) 3) vol)
	   (my-float xx yy zz)
	   (values (complex my-float) &optional))
  (multiple-value-bind (i x)
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
			 (complex zero)))))
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
	      (* iii x y z))))))))
#+nil
(let* ((n 3)
       (a (make-array (list n n n) 
		      :element-type '(complex my-float)))
       (a1 (sb-ext:array-storage-vector a)))
  (dotimes (i (length a1))
    (setf (aref a1 i) (complex (* one i))))
  (interpolate3-cdf a 1.2d0 1.3d0 1.2d0))


(defmacro def-resample-half-rk-type (rank short-type)
  (let ((name (format-symbol "resample-half~d-~a" rank short-type))
	(long-type (get-long-type short)))
    `(defun ,name (vol)
       (declare ((simple-array (complex double-float) 3) vol)
		(values (simple-array (complex double-float) 3) &optional))
       (let ((dims (array-dimensions vol)))
	(destructuring-bind (z y x)
	    dims
	  (let* ((xx (floor x 2))
		 (yy (floor y 2))
		 (zz (floor z 2))
		 (small (make-array (list zz yy xx)
				    :element-type ')))
	    (do-region ((k j i) (zz yy xx))
	      (setf (aref small k j i)
		    (aref vol (* k 2) (* j 2) (* i 2))))
	    small))))))

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
(defun resample3-cdf (vol dx dy dz dx^ dy^ dz^)
  "Resample a volume by trilinear interpolation. Ensure that the
  former center stays in the center."
  (declare ((simple-array (complex my-float) 3) vol)
	   (my-float dx dy dz dx^ dy^ dz^)
	   (values (simple-array (complex my-float) 3) &optional))
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
    (destructuring-bind (z y x)
	(array-dimensions vol)
      (multiple-value-bind (bx cx nx)
	  (dims x dx dx^)
	(multiple-value-bind (by cy ny)
	    (dims y dy dy^)
	  (multiple-value-bind (bz cz nz)
	      (dims z dz dz^)
	    (let ((result (make-array (list nz ny nx)
				      :element-type '(complex my-float))))
	      (macrolet ((coord (i dir) ;; convert index into float coordinate
			   `(* (/ ,(intern (format nil "D~a^" dir))
				  ,(intern (format nil "D~a" dir)))
			       (+ ,i ,(intern (format nil "B~a" dir))))))
	       (loop for k from (- bz) below cz do
		    (let ((zz (coord k z)))
		      (loop for j from (- by) below cy do
			   (let ((yy (coord j y)))
			     (loop for i from (- bx) below cx do
				  (let ((xx (coord i x)))
				    (setf (aref result (+ bz k) (+ by j) (+ bx i))
					  (interpolate3-cdf vol xx yy zz)))))))))
	     result)))))))
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
       (typecase a
	 ,@(loop for type in types collect
		     (let ((long-type (get-long-type type)))
		       `((simple-array ,long-type 3) (,(format-symbol 
							 "cross-section-xz-~a" type) a y))))
	 (t (error "type not supported."))))))

(def-cross-section-xz-functions (ub8 sf df csf cdf))

(defun decimate-xy-ub8 (dx vol)
  "Increase transversal sampling distance by odd integer factor DX."
  (declare (fixnum dx)
	   ((simple-array (unsigned-byte 8) 3) vol)
	   (values (simple-array (unsigned-byte 8) 3) &optional))
  (unless (eq (mod dx 2) 1)
    (error "Factor DX has to be odd."))
  (destructuring-bind (z y x)
      (array-dimensions vol)
    (let* ((dx2 (* dx dx))
	   (nx (floor x dx))
	   (ny (floor y dx))
	    (result (make-array (list z ny nx)
				:element-type '(unsigned-byte 8))))
      (do-box (k j i 0 z 0 ny 0 nx)
	(let ((sum 0))
	  (do-rectangle (jj ii 0 dx 0 dx)
	    (incf sum (aref vol
			    k
			    (+ (* dx j) jj)
			    (+ (* dx i) ii))))
	  (setf (aref result k j i) (floor sum dx2))))
      result)))
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
