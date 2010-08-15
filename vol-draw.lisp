(require :vol)
(in-package :vol)

;; A circular window in the center of the BFP with radius Rap gives
;; rise to rays up to the angle beta into sample space. The radius of
;; the focal sphere is n*f. Therefore one can write
;; sin(beta)=Rap/(n*f). Changing illumination direction of the grating
;; will shear the intensity image. In order to generate an image of
;; limited coherence one has to convolve each plane with a disk. The
;; radius of the disk is: Rd(z)=z*sin(beta) with defocus z. 
;; Eliminate sin(beta):
;; Rd(z)=abs(z*Rap/(n*f))
;; for a 63x objective we get f=2.61, with n=1.515 

(def-generator (draw-disk (type))
  `(defun ,name (radius y x)
     (declare (single-float radius)
	      (fixnum y x)
	      (values (simple-array ,long-type 2) &optional))
     (let ((result (make-array (list y x) :element-type ',long-type))
	   (xh (floor x 2))
	   (yh (floor y 2))
	   (r2 (* radius radius)))
       (do-region ((j i) (y x))
	 (let* ((ii (- i xh))
		(jj (- j yh))
		(rr2 (+ (* ii ii) (* jj jj))))
	   (when (< rr2 r2)
	     (setf (aref result j i) (* ,(case type
					       (ub8 255)
					       (otherwise (coerce 1 long-type))))))))
       result)))

#+nil
(def-draw-disk-type ub8)

(defmacro def-draw-disk-functions (types)
  (let* ((specifics nil)
	 (name (format-symbol "draw-disk")))
    (loop for type in types do
	 (let ((def-name (format-symbol "def-~a-type" name)))
	   (push `(,def-name ,type) specifics)))
    `(progn ,@specifics)))

(def-draw-disk-functions (ub8 sf df csf cdf))

#+nil
(write-pgm "/home/martin/tmp/disk.pgm"
	   (draw-disk-ub8 33.0 100 100))

#+nil
(write-pgm "/home/martin/tmp/disk.pgm"
	   (normalize2-cdf/ub8-realpart
	    (fftshift2 (convolve2-circ
			(draw-disk 12d0 100 200)
			(draw-disk 12d0 100 200)))))

(sb-alien:define-alien-routine ("j1" bessel-j1-df)
    sb-alien:double
  (arg sb-alien:double))

(sb-alien:define-alien-routine ("j1f" bessel-j1-sf)
    sb-alien:float
  (arg sb-alien:float))

;; isi.ssl.berkeley.edu/~tatebe/whitepapers/FT%20of%20Uniform%20Disk.pdf
(def-generator (draw-uniform-disk-precise (type))
  (let* ((rtype (ecase type
		 (csf 'sf)
		 (cdf 'df)))
	(rlong-type (get-long-type rtype)))
    `(defun ,name (radius y x)
      (declare (single-float radius)
	       (fixnum y x)
	       (values (simple-array ,long-type 2) &optional))
      (let ((a (make-array (list y x)
			   :element-type ',long-type))
	    (xh (floor x 2))
	    (yh (floor y 2))
	    (one ,(coerce 1 rlong-type))
	    (tiny ,(coerce 1e-12 rlong-type)))
	(do-region ((j i) (y x))
	  (let* ((xx (/ (* one (- i xh)) x))
		 (yy (/ (* one (- j yh)) y))
		 (f (sqrt (+ (* xx xx) (* yy yy)))))
	    (setf (aref a j i) (if (< f tiny)
				   (complex (* ,(coerce pi rlong-type) radius))
				   (complex (/ (,(ecase type
							(csf `bessel-j1-sf)
							(cdf `bessel-j1-df))
						 (* ,(coerce (* 2 pi) rlong-type)
						    f radius))
					       f))))))
	(fftshift (ift (fftshift a)))))))

(def-draw-uniform-disk-precise-type csf)
(def-draw-uniform-disk-precise-type cdf)

(def-generator (draw-unit-energy-disk-precise (type))
  `(defun ,name (radius y x)
     (declare (single-float radius)
	      (fixnum y x)
	      (values (simple-array ,long-type 2) &optional))
     (let* ((disk (,(format-symbol "draw-uniform-disk-precise-~a" type)
		    radius y x))
	    (sum (reduce #'+ (sb-ext:array-storage-vector disk))))
       (s* (/ (realpart sum)) disk))))

(def-draw-unit-energy-disk-precise-type csf)
(def-draw-unit-energy-disk-precise-type cdf)

#+NIL
(write-pgm "/home/martin/tmp/disk.pgm"
	   (normalize-2-csf/ub8-realpart
	    (draw-unit-energy-disk-precise-csf 12.3 300 300)))

(defun draw-sphere-ub8 (radius z y x)
  (declare (single-float radius)
	   (fixnum z y x)
	   (values (simple-array (unsigned-byte 8) 3)
		   &optional))
  (let ((sphere (make-array (list z y x)
			    :element-type '(unsigned-byte 8))))
    (let ((xh (floor x 2))
	  (yh (floor y 2))
	  (zh (floor z 2))
	  (radius2 (* radius radius)))
      (labels ((sq (x) (* x x)))
       (do-region ((k j i) (z y x))
	 (let ((r2 (+ (sq (* 1s0 (- i xh)))
		      (sq (* 1s0 (- j yh)))
		      (sq (* 1s0 (- k zh))))))
	   (setf (aref sphere k j i)
		 (if (< r2 radius2)
		     1 0))))))
    sphere))
#+nil
(count-non-zero-ub8 (draw-sphere-ub8 one 41 58 70))
#+nil
(let ((a (draw-sphere-ub8 one
			  4 4 4
			  ;;3 3 3
			  ;;41 58 70
			  ))
      (res ()))
 (destructuring-bind (z y x)
     (array-dimensions a)
   (do-box (k j i 0 z 0 y 0 x)
     (when (eq 1 (aref a k j i))
       (setf res (list k j i))))
   res))

(defun draw-oval-ub8 (radius z y x)
  (declare (my-float radius)
	   (fixnum z y x)
	   (values (simple-array (unsigned-byte 8) 3)
		   &optional))
  (let ((sphere (make-array (list z y x)
			    :element-type '(unsigned-byte 8)))
	(scale-z (coerce 5d0 'my-float)))
    (do-box (k j i 0 z 0 y 0 x)
      (let ((r (sqrt (+ (square (- i (* half x)))
			(square (- j (* half y)))
			(square (* scale-z (- k (* half z))))))))
	(setf (aref sphere k j i)
	      (if (< r radius)
		  1
		  0))))
    sphere))
