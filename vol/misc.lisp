(in-package :vol)

(defun clamp (a)
  (declare (integer a)
	   (values (unsigned-byte 8) &optional))
  (when (< a 0)
    (return-from clamp 0))
  (when (< 255 a)
    (return-from clamp 255))
  a)

(defun count-non-zero-ub8 (vol)
  (declare ((simple-array (unsigned-byte 8) 3) vol)
	   (values fixnum &optional))
  (let* ((sum 0)
	 (vol1 (sb-ext:array-storage-vector vol)))
    (dotimes (i (length vol1))
      (when (< 0 (aref vol1 i))
	(incf sum)))
    sum))

(defun histogram (img &optional (bins 30))
  (declare ((array (unsigned-byte 8) 1) img)
	   (fixnum bins)
	   (values (simple-array fixnum 1) (unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 8)
		 &optional))
  (let* ((maxval (aref img 0))
	 (minval maxval)
	 (w (length img)))
    (dotimes (i w)
      (let ((v (aref img i)))
	(when (< v minval)
	  (setf minval v))
	(when (< maxval v)
	  (setf maxval v))))
    (let* ((len (1+ (- maxval minval)))
	   (result (make-array len :element-type 'fixnum))
	   (binsm (min (- maxval minval) bins)))
      (when (eq maxval minval)
	#+nil (error "data is too boring.")
	(return-from histogram (values result minval maxval binsm)))
      (dotimes (i w)
	(incf (aref result (floor (* binsm
				     (- (aref img i)
					minval))
				  (- maxval minval)))))
      (values result minval maxval binsm))))

(defun square (x)
  (declare (double-float x)
	   (values double-float &optional))
  (* x x))
 
;; chernov/book.pdf p. 20
(defun linear-regression (y &optional
			  (x (let* ((n (length y)))
			       (make-array 
				n
				:element-type 'double-float
				:initial-contents 
				(loop for i below n collect (* 1d0 i))))))
  "Linear regression of the values in Y with the function y=a*x+b. If
X isn't supplied its assumed to be 0,1,2, ... . Returned are the
fitting parameters A and B and their errors DELTA_A and DELTA_B."
  (declare ((array double-float 1) y x)
	   (values double-float double-float double-float double-float &optional))
  (let* ((n (length y))
	 (xmean (/ (loop for xi across x sum xi) n))
	 (ymean (/ (loop for xi across y sum xi) n))
	 (sxx (loop for xi across x sum (square (- xi xmean))))
	#+nil (syy (loop for xi across y sum (square (- xi ymean))))
	 (sxy (loop for i below n sum (* (- (aref x i) xmean)
					 (- (aref y i) ymean))))
	 (bhat (/ sxy sxx))
	 (ahat (- ymean (* bhat xmean)))
	 (var (/ (loop for i below n sum (square (- (aref y i) ahat
						       (* bhat (aref x i)))))
		    (- n 2)))
	 (vara (* var (+ (/ (square xmean)
			    sxx)
			 (/ n))))
	 (varb (/ var sxx)))
    (values ahat bhat (sqrt vara) (sqrt varb))))
 
#+nil
(linear-regression (let* ((ll (list one 2.0one 3d0 4d0)))
		     (make-array (length ll)
		      :element-type 'double-float
		      :initial-contents ll))) 
