;; 2d and 3d convolutions
(require :vol)
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
       (let ((c (s* (* ,(coerce 1 (get-long-type (ecase type (csf 'sf) (cdf 'df))))
		       (reduce #'* da)) 
		    (.* (ft vola) (ft volb)))))
	 (ift c)))))

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
(defun convolve3-nocrop (vola volb)
  "Convolve VOLA with VOLB. We consider VOLB as the convolution
kernel. Returns (values result vec). RESULT is an arrays that is as
big as necessary to accommodate the convolution and VEC contains the
relative coordinates to find the original sample positions of array
VOLA in RESULT."
    (declare ((simple-array (complex my-float) 3) vola volb)
	   (values (simple-array (complex my-float) 3) vec-i &optional))
  (destructuring-bind (za ya xa)
      (array-dimensions vola)
    (destructuring-bind (zb yb xb)
	(array-dimensions volb)
      (let* ((biga (make-array (list (+ za zb)
				     (+ ya yb)
				     (+ xa xb))
			       :element-type '(complex my-float)))
	     (bigb (make-array (array-dimensions biga)
			       :element-type '(complex my-float)))
	     (fzb (front zb))
	     (fyb (front yb))
	     (fxb (front xb))
	     (fza (front za))
	     (fya (front ya))
	     (fxa (front xa))
	     (start (make-vec-i :x fxb :y fyb :z fzb)))
	(do-box (k j i 0 za 0 ya 0 xa)
	  (setf (aref biga (+ k fzb) (+ j fyb) (+ i fxb))
		(aref vola k j i)))
	(do-box (k j i 0 zb 0 yb 0 xb)
	  (setf (aref bigb (+ k fza) (+ j fya) (+ i fxa))
		(aref volb k j i)))
	(values (convolve3-circ biga (fftshift3 bigb))
		start)))))

(defun convolve3 (vola volb)
  (destructuring-bind (za ya xa)
      (array-dimensions vola)
    (multiple-value-bind (conv start)
	(convolve3-nocrop vola volb)
      (let* ((result (make-array (array-dimensions vola)
				 :element-type '(complex my-float)))
	     (oz (vec-i-z start))
	     (oy (vec-i-y start))
	     (ox (vec-i-x start)))
	(do-box (k j i 0 za 0 ya 0 xa)
	  (setf (aref result k j i)
	       (aref conv (+ k oz) (+ j oy) (+ i ox))))
	result))))

#+nil
(let ((a (make-array (list 100 200 300)
		     :element-type '(complex my-float)))
      (b (make-array (list 10 200 30)
		     :element-type '(complex my-float))))
  (convolve3 a b)
  nil)
