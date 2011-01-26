(defun read-ics3 (name slice stack &key (x 291) (y 354) (z 41) (d 91))
  "Read a slice of Jean-Yves worm stack video."
  (declare (string name)
	   (integer slice)
	   (integer stack))
  (assert (< -1 slice z))
  (assert (< -1 stack d))
  (let ((pos
	 (with-open-file (s name :direction :input)
	   (do ((l (read s) (read s)))
	       ((eq l 'end)))
	   (file-position s))))
    (with-open-file (s name :direction :input
		       :element-type '(unsigned-byte 16))
      ;; FIXME: why do I need -170 offset?
      (file-position s (+ (- 170) pos 
			  (* stack z y x 2)
			  (* slice y x 2)))
      (let* ((a (make-array (list y x)
			    :element-type '(unsigned-byte 16)))
	     (a1 (make-array (* y x)
			     :element-type '(unsigned-byte 16)
			     :displaced-to a)))
	(read-sequence a1 s)
	a))))
#+nil
(progn
  (defparameter edc
    (read-ics3 "/media/disk/cele.ics" 32 43))
  (write-pgm (normalize-ub8 edc) "/dev/shm/o.pgm"))


(defun amax (ar)
  (declare (array ar))
  (let* ((n (array-total-size ar))
	 (a1 (make-array n :element-type (array-element-type ar)
			   :displaced-to ar)))
   (loop for i from 0 below n maximize (aref a1 i))))

(defun amin (ar)
  (declare (array ar))
  (let* ((n (array-total-size ar))
	 (a1 (make-array n :element-type (array-element-type ar)
			   :displaced-to ar)))
   (loop for i from 0 below n minimize (aref a1 i))))

#+nil
(list (amin edc) (amax edc))

(defun clamp (a)
  (declare (integer a)
	   (values (unsigned-byte 8) &optional))
  (cond ((< a 0) 0)
	((< 255 a) 255)
	(t a)))

(defun normalize-ub8 (ar)
  (declare (array ar)
	   (values array &optional))
  (let* ((n (array-total-size ar))
	 (a1 (make-array n 
			 :element-type (array-element-type ar)
			 :displaced-to ar))
	 (b (make-array (array-dimensions ar)
			:element-type '(unsigned-byte 8)))
	 (b1 (make-array n :element-type '(unsigned-byte 8)
			   :displaced-to b))
	 (ma (amax ar))
	 (mi (amin ar))
	 (s (/ 255s0 (- ma mi))))
   (dotimes (i n)
     (setf (aref b1 i)
	   (floor (* s (- (aref a1 i) mi)))))
   b))



(defun a- (a b)
  (declare (array a b)
	   (values array &optional))
  (assert (equal (array-dimensions a)
		 (array-dimensions b)))
  (let* ((n (array-total-size a))
	 (a1 (make-array n
			 :element-type (array-element-type a)
			 :displaced-to a))
	 (b1 (make-array n :element-type (array-element-type b)
			   :displaced-to b))
	 (c (make-array (array-dimensions a)
			:element-type (array-element-type b)))
	 (c1 (make-array n :element-type (array-element-type b)
			   :displaced-to c)))
   (dotimes (i n)
     (setf (aref c1 i)
	   (if (or (= 0s0 (aref a1 i))
		   (= 0s0 (aref b1 i)))
	       0s0
	       (- (aref a1 i) (aref b1 i)))))
   c))

#+nil
(normalize-ub8 edc)

(defun write-pgm (img filename)
  (declare (string filename)
	   ((array (unsigned-byte 8) 2) img)
	   (values null &optional))
  (destructuring-bind (h w) (array-dimensions img)

    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%255~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :append)
      (let ((data-1d (make-array 
                      (* h w)
                      :element-type '(unsigned-byte 8)
                      :displaced-to img)))
        (write-sequence data-1d s)))
    nil))

#+nil
(write-pgm (normalize-ub8 edc) "/dev/shm/o.pgm")

(defun gauss (n)
  (declare ((integer 0 100) n)
	   (values array &optional))
  (let* ((n1 (1+ n))
	 (a (make-array n1 :element-type 'single-float)))
    (dotimes (i n1)
      (setf (aref a i)
	    (let ((q (/ (* 3s0 i) n)))
	     (exp (* -.5 q q)))))
    (let ((scale (/ (let ((sum (aref a 0)))
		      (loop for i from 1 below n1 do
			   (incf sum (* 2s0 (aref a i))))
		      sum))))
      (dotimes (i n1)
	(setf (aref a i)
	      (* scale (aref a i)))))
    a))

(defun convolve1-x (img kernel)
  (declare ((array single-float 1) kernel)
	   (array img)
	   (values array &optional))
  (destructuring-bind (y x) (array-dimensions img)
    (let ((n (1- (length kernel)))
	  (res (make-array (array-dimensions img)
			   :element-type 'single-float)))
      (dotimes (j y)
	(loop for i from n below (- x n) do
	     (let ((sum (* (aref kernel 0) (aref img j i))))
	       (dotimes (k n)
		 (incf sum
		       (+ (* (aref kernel (1+ k)) (aref img j (+ i k 1)))
			  (* (aref kernel (1+ k)) (aref img j (- i k -1))))))
	       (setf (aref res j i) sum))))
      res)))

#+nil
(write-pgm
 (normalize-ub8
  (convolve1-y edc (gauss 10)))
 "/dev/shm/o.pgm")

(defun convolve1-y (img kernel)
  (declare ((array single-float 1) kernel)
	   (array img)
	   (values array &optional))
  (destructuring-bind (y x) (array-dimensions img)
    (let ((n (1- (length kernel)))
	  (res (make-array (array-dimensions img)
			   :element-type 'single-float)))
      (dotimes (i x)
	(loop for j from n below (- y n) do
	     (let ((sum (* (aref kernel 0) (aref img j i))))
	       (dotimes (k n)
		 (incf sum
		       (+ (* (aref kernel (1+ k)) (aref img (+ j k 1) i))
			  (* (aref kernel (1+ k)) (aref img (- j k -1) i)))))
	       (setf (aref res j i) sum))))
      res)))

(defun convolve1 (img kernel)
  (convolve1-x (convolve1-y img kernel) kernel))

#+nil
(time 
 (let* ((bk (gauss 30))
	(sk (gauss 37))
	(big (convolve1 edc bk))
	(sma (convolve1 edc sk))
	(d (a- big sma)))
   
   (write-pgm
    (normalize-ub8 d)
    "/dev/shm/o.pgm")))