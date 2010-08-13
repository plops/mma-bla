;; functions that are somehow related to the fouriertransform

(in-package :vol)

(defmacro def-fftshift-rk-type (rank short-type)
  (let ((name (format-symbol "fftshift~d-~a" rank short-type))
	(long-type (get-long-type short-type)))
    (push name *macro-generated-functions*)
    `(defun ,name (in)
      (declare ((simple-array ,long-type ,rank) in)
	       (values (simple-array ,long-type ,rank) &optional))
      (let ((out (make-array (array-dimensions in)
			     :element-type ',long-type)))
	,(ecase rank
		(1 `(destructuring-bind (x)
			(array-dimensions in)
		     (let ((xx (floor x 2)))
		       (do-region ((i) (x))
			 (let ((ii (mod (+ i xx) x)))
			   (setf (aref out i)
				 (aref in ii)))))))
		(2 `(destructuring-bind (y x)
		       (array-dimensions in)
		     (let ((xx (floor x 2))
			   (yy (floor y 2)))
		       (do-region ((j i) (y x))
			 (let ((ii (mod (+ i xx) x))
			       (jj (mod (+ j yy) y)))
			   (setf (aref out j i)
				 (aref in jj ii)))))))
		(3 `(destructuring-bind (z y x)
			(array-dimensions in)
		      (let ((xx (floor x 2))
			    (yy (floor y 2))
			    (zz (floor z 2)))
			(do-region ((k j i) (z y x))
			  (let ((ii (mod (+ i xx) x))
				(jj (mod (+ j yy) y))
				(kk (mod (+ k zz) z)))
			    (setf (aref out k j i)
				  (aref in kk jj ii))))))))
	out))))

#+nil
(def-fftshift-rk-type 3 sf)

(defmacro def-fftshifts (ranks types)
  (let ((result nil))
    (loop for rank in ranks do
	(loop for type in types do
	     (push `(def-fftshift-rk-type ,rank ,type)
		   result)))
    `(progn ,@result)))

(def-fftshifts (1 2 3) (cdf csf))

#+nil
(let* ((ls '(1 2 3 4 5 6 7 8 9))
       (a (make-array (length ls)
		      :element-type '(complex single-float)
		      :initial-contents (mapcar
					 #'(lambda (z) (coerce z 
							 '(complex single-float)))
					 ls))))
  (fftshift1-csf a))

#+nil
(time 
 (let ((a (make-array (list 128 128 128)
		      :element-type '(complex single-float))))
   (fftshift3-csf a)
   nil))