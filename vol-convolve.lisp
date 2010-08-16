;; 2d and 3d convolutions
(in-package :vol)

(def-generator (convolve-circ (rank type))
  `(defun ,name (vola volb)
     (declare ((simple-array ,long-type ,rank) vola volb)
	    (values (simple-array ,long-type ,rank) &optional))
     (let* ((da (array-dimensions vola))
	    (db (array-dimensions volb))
	    (compare-ab (map 'list #'(lambda (x y) (eq x y)) da db)))
       (when (some #'null compare-ab)
	 (error
	  ,(format
	    nil
	    "~a expects both input arrays to have the same dimensions." name)))
       (ift (s* (* ,(coerce 1 (get-long-type (ecase type (csf 'sf) (cdf 'df))))
		   (reduce #'* da)) 
		(.* (ft vola) (ft volb)))))))

(defmacro def-convolve-circ-functions (ranks types)
  (let* ((specifics nil)
	 (cases nil)
	 (name (format-symbol "convolve-circ")))
    (loop for rank in ranks do
	 (loop for type in types do
	      (let ((def-name (format-symbol "def-~a-rank-type" name))
		    (specific-name (format-symbol "~a-~a-~a" name rank type)))
		(push `(,def-name ,rank ,type) specifics)
		(push `((simple-array ,(get-long-type type) ,rank)
			(,specific-name a b))
		      cases))))
    (store-new-function name)
    `(progn ,@specifics
	    (defun ,name (a b)
	       (etypecase a
		 ,@cases
		 (t (error "The given type can't be handled with a generic ~a function." ',name)))))))

(def-convolve-circ-functions (2 3) (csf cdf))





(defun front (i) ;; extra size needed to accommodate kernel overlap
		 ;; there is a difference between even and odd kernels
  (declare (fixnum i)
	   (values fixnum &optional))
  (max 0
       (if (evenp i)
	   (floor i 2)
	   (1- (floor i 2)))))

;; volb is the kernel
(def-generator (convolve-nocrop (rank type))
  `(defun ,name (vola volb)
   "Convolve VOLA with VOLB. We consider VOLB as the convolution
kernel. Returns (values result vec). RESULT is an arrays that is as
big as necessary to accommodate the convolution and VEC contains the
relative coordinates to find the original sample positions of array
VOLA in RESULT."
   (declare ((simple-array ,long-type ,rank) vola volb)
	    (values (simple-array ,long-type ,rank) vec-i &optional))
   ,(ecase 
     rank
     (2 `(destructuring-bind (ya xa) (array-dimensions vola)
	   (destructuring-bind (yb xb) (array-dimensions volb)
	     (let* ((biga (make-array (list (+ ya yb) (+ xa xb))
				      :element-type ',long-type))
		    (bigb (make-array (array-dimensions biga)
				      :element-type ',long-type))
		    (fyb (front yb))
		    (fxb (front xb))
		    (fya (front ya))
		    (fxa (front xa))
		    (start (make-vec-i :x fxb :y fyb)))
	       (do-region ((j i) (ya xa))
		 (setf (aref biga (+ j fyb) (+ i fxb))
		       (aref vola j i)))
	       (do-region ((j i) (yb xb))
		 (setf (aref bigb (+ j fya) (+ i fxa))
		       (aref volb j i)))
	       (values (convolve-circ biga (fftshift bigb)) start)))))
     (3 `(destructuring-bind (za ya xa) (array-dimensions vola)
	   (destructuring-bind (zb yb xb) (array-dimensions volb)
	     (let* ((biga (make-array (list (+ za zb) (+ ya yb) (+ xa xb))
				      :element-type ',long-type))
		    (bigb (make-array (array-dimensions biga)
				      :element-type ',long-type))
		    (fzb (front zb))
		    (fyb (front yb))
		    (fxb (front xb))
		    (fza (front za))
		    (fya (front ya))
		    (fxa (front xa))
		    (start (make-vec-i :x fxb :y fyb :z fzb)))
	       (do-region ((k j i) (za ya xa))
		 (setf (aref biga (+ k fzb) (+ j fyb) (+ i fxb))
		       (aref vola k j i)))
	       (do-region ((k j i) (zb yb xb))
		 (setf (aref bigb (+ k fza) (+ j fya) (+ i fxa))
		       (aref volb k j i)))
	       (values (convolve-circ biga (fftshift bigb)) start))))))))

(defmacro def-convolve-nocrop-functions (ranks types)
  (let* ((specifics nil)
	 (cases nil)
	 (name (format-symbol "convolve-nocrop")))
    (loop for rank in ranks do
	 (loop for type in types do
	      (let ((def-name (format-symbol "def-~a-rank-type" name))
		    (specific-name (format-symbol "~a-~a-~a" name rank type)))
		(push `(,def-name ,rank ,type) specifics)
		(push `((simple-array ,(get-long-type type) ,rank)
			(,specific-name a b))
		      cases))))
    (store-new-function name)
    `(progn ,@specifics
	    (defun ,name (a b)
	       (etypecase a
		 ,@cases
		 (t (error "The given type can't be handled with a generic ~a function." ',name)))))))

(def-convolve-nocrop-functions (2 3) (csf cdf))

(defun convolve (vola volb)
  (multiple-value-bind (conv start)
      (convolve-nocrop vola volb)
    (let ((s (convert-1-fix/df-mul start))
	  (b (convert-1-fix/df-mul 
	      (make-array 3 :element-type 'fixnum
			  :initial-contents
			  (reverse (array-dimensions volb))))))
      (extract-bbox  conv 
		     (make-bbox :start s
				:end (v+ s b))))))

#+nil
(let ((a (make-array (list 20 40 30)
		     :element-type '(complex single-float)))
      (b (make-array (list 10 20 30)
		     :element-type '(complex single-float))))
  (convolve a b)
  nil)
