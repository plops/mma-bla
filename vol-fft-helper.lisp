(let ((type-names '(((complex double-float) . cdf)
		    ((complex single-float) . csf)
		    (double-float . df)
		    (single-float . sf))))
  (defun get-short-type (long-type)
    (cdr (assoc long-type type-names)))
  (defun get-long-type (short-type)
    (car (rassoc short-type type-names))))

#+nil
(get-long-type 'df)

(defmacro do-region ((indices end &optional (start '(0 0 0))) &body body)
  "Write intertwined loops to traverse a vector, an image or a volume."
  (unless (and (= (length indices)
		  (length end)))
    (error "Number of indices and interval-ends are not equal."))
  (labels ((rec (ind end start acc) ;; several loops
             (if (null end)
                 acc
                 (rec (cdr ind) (cdr end) (cdr start)
                      `((loop for ,(car ind) from ,(car start) 
                           below ,(car end) do ,@acc))))))
    (first (rec (reverse indices) ;; first index is outermost loop
		(reverse end)
		(reverse start) body))))
#+nil
(let ((sum 0))
  (do-region ((k j i) (4 4 5))
    (incf sum (+ k j i)))
  sum)

(defmacro format-symbol (fmt &rest rest)
  `(intern (string-upcase (format nil ,fmt ,@rest))))

(defmacro letn (vecs binds &body body)
  `(symbol-macrolet ,vecs
     (let ,binds
       ,@body)))

(defparameter *macro-generated-functions* nil)

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