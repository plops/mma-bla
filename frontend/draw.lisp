(in-package :frontend)

(defun draw-spheres (radius centers z y x)
  "put points into the centers of nuclei and convolve a sphere around each"
  (declare (single-float radius)
	   ((simple-array vec-i 1) centers)
	   (fixnum z y x)
	   (values (simple-array (complex my-float) 3) &optional))
  (let* ((dims (list z y x))
	 (points (make-array dims
			     :element-type '(complex my-float)))
	 (n (length centers)))
    (dotimes (i n)
      (let ((c (aref centers i)))
	(setf (aref points
		    (* 5 (vec-i-z c))
		    (vec-i-y c)
		    (vec-i-x c))
	      (complex +one+))))
    (convolve-circ points 
		   (fftshift (draw-sphere-csf radius z y x)))))

(defun draw-ovals (radius centers z y x)
  (declare (single-float radius)
	   ((simple-array vec-i 1) centers)
	   (fixnum z y x)
	   (values (simple-array (complex my-float) 3) &optional))
  (let* ((dims (list z y x))
	 (points (make-array dims
			     :element-type '(complex my-float)))
	 (n (length centers)))
    (dotimes (i n)
      (let ((c (aref centers i)))
	(setf (aref points
		    (vec-i-z c)
		    (vec-i-y c)
		    (vec-i-x c))
	      (complex +one+))))
    (convolve-circ points (fftshift (draw-oval-csf radius z y x)))))

(defun draw-indexed-ovals (radius centers z y x)
  "The first oval contains the value 1 the second 2, ..."
  (declare (single-float radius)
	   ((simple-array vec-i 1) centers)
	   (fixnum z y x)
	   (values (simple-array (complex my-float) 3) &optional))
  (let* ((dims (list z y x))
	 (points (make-array dims
			     :element-type '(complex my-float)))
	 (n (length centers)))
    (dotimes (i n)
      (let ((c (aref centers i)))
	(setf (aref points
		    (vec-i-z c)
		    (vec-i-y c)
		    (vec-i-x c))
	      (complex (+ +one+ i)))))
    (convolve-circ points
		   (fftshift (draw-oval-csf radius z y x)))))


;; to find centers of cells do a convolution with a sphere
(defun find-centers ()
  (declare (values (simple-array vec-i 1)
		   (simple-array my-float 1)
		   cons &optional))
  (let* ((stack-byte (read-stack "/home/martin/tmp/xa*.pgm"))
	 (dims (array-dimensions stack-byte))
	 (stack (make-array dims :element-type '(complex my-float))))
    (destructuring-bind (z y x) dims
      (do-region ((k j i) (z y x))
	(setf (aref stack k j i) (complex (+ (* #.(coerce .43745 'my-float) k)
					     (aref stack-byte k j i)))))
      ;; find centers of cells by convolving with sphere, actually an
      ;; oval because the z resolution is smaller than the transversal
      (let* ((conv (convolve-circ 
		    stack 
		    (fftshift
		     (#.(cond 
			  ((subtypep 'my-float 'single-float) 'draw-oval-csf)
			  ((subtypep 'my-float 'double-float) 'draw-oval-cdf))
			12.0 z y x))))
	     (cv (convert conv 'sf 'realpart))
	     (centers nil)
	     (center-heights nil))
	(do-region ((k j i) ((- z 3) (- y 1) (- x 1)) (6 1 1))
	  (macrolet ((c (a b c)
		       `(aref cv (+ k ,a) (+ j ,b) (+ i ,c))))
	    (let ((v (c 0 0 0)))
	      (when (and (< (c 0 0 -1) v)
			 (< (c 0 0 1) v)
			 (< (c 0 -1 0) v)
			 (< (c 0 1 0) v)
			 (< (c -1 0 0) v)
			 (< (c 1 0 0) v))
		(push (make-vec-i :z k :y j :x i) centers)
		(push v center-heights)))))
	(let ((c (make-array (length centers)
			     :element-type 'vec-i
			     :initial-contents centers))
	      (ch (make-array (length center-heights)
			      :element-type 'my-float
			      :initial-contents center-heights)))
	  (values c ch dims))))))

#+nil
(sb-ext:gc :full t)
#+nil
(time
 (progn 
   (format t "~a~%" (multiple-value-list (find-centers)))
   nil))
