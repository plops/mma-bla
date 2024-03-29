(require :vol)
(in-package :run-gui)
#+nil
(defparameter *calib-bright* *line*)
#+nil
(defparameter *calib-dark* *line*)
#+nil
(defparameter *calib-spot* *line*)

(defun replace-zero-with-one (img)
  (declare (type (simple-array single-float 2) img))
  (destructuring-bind (y x) (array-dimensions img)
    (vol:do-region ((j i) (y x))
      (setf (aref img j i) (if (< (abs (aref img j i)) 1e-4)
			       1s0
			       (aref img j i)))))
  img)

(defun select-bigger (img value)
  (declare (type (simple-array single-float 2) img)
	   (type single-float value))
  (destructuring-bind (y x) (array-dimensions img)
    (vol:do-region ((j i) (y x))
      (setf (aref img j i) (if (< (aref img j i) value)
			       0s0
			       (aref img j i)))))
  img)

#+nil
(let ((b (vol:convert-2-ub16/sf-mul *calib-bright*))
      (d (vol:convert-2-ub16/sf-mul *calib-dark*))
      (s (vol:convert-2-ub16/sf-mul *calib-spot*)))
  (vol:write-pgm "/dev/shm/o.pgm" (vol:normalize-2-sf/ub8
				   (vol:./ (select-bigger (vol:.- s d) 12)
					   (replace-zero-with-one (vol:.- b d))))))
(defun clamp-ub8 (img)
  (declare (type (simple-array single-float 2) img))
  (let ((img1 (sb-ext:array-storage-vector img)))
    (dotimes (i (length img1))
      (setf (aref img1 i)
	    (max 0s0 (min 255s0 (aref img1 i)))))
    img))

(defun copy-image (img)
  (declare (type (simple-array (unsigned-byte 16) 2) img))
  (let* ((r (make-array (array-dimensions img)
			:element-type '(unsigned-byte 16)))
	 (r1 (sb-ext:array-storage-vector r))
	 (i1 (sb-ext:array-storage-vector img)))
    (dotimes (i (length r1))
      (setf (aref r1 i) (aref i1 i)))
    r))
#+nil
(let* ((i 0))
  (dolist (e *scan-result*)
    (destructuring-bind ((ii j radius r g b img-type) img cap captime pres prestime) e
      (when t ; (eq img-type :scan)
	(format t "~a~%" (list img-type ii j))
	(vol:write-pgm (format nil "/dev/shm/o~3,'0d.pgm" (incf i))
		      (vol:convert-2-sf/ub8-floor
		       (clamp-ub8 (vol::s* .2 (vol:convert-2-ub16/sf-mul
					       (second e))))))))))


(defun accum-img (type)
  (let* ((a (make-array (array-dimensions (second (first *scan-result*)))
		       :element-type 'single-float))
	(a1 (sb-ext:array-storage-vector a))
	(count 0))
   (dolist (e *scan-result*)
     (destructuring-bind ((i j radius r g b img-type) img cap captime pres prestime) e
       (when (eq img-type type)
	 (incf count)
	 (let ((img1 (sb-ext:array-storage-vector img)))
	   (dotimes (i (length a1))
	     (incf (aref a1 i) (aref img1 i)))))))
   (vol::s* (/ 1s0 count) a)))


(defun find-maxima (img)
  (declare (type (simple-array single-float 2) img))
  (let ((res nil))
    (destructuring-bind (y x) (array-dimensions img)
      (vol:do-region ((j i) ((1- y) (1- x)) (1 1))
	(let ((v (aref img j i)))
	  (macrolet ((c (y x)
		       `(< (aref img (+ ,y j) (+ ,x i)) v)))
	    (when (and (c 0 1) (c 1 0) 
		       (c 1 1) (c 0 -1)
		       (c -1 0) (c -1 -1)
		       (c -1 1) (c 1 -1))
	      (push (list i j v) res))))))
    res))

#+Nil
(sort 
 (find-maxima (vol:convert-2-csf/sf-realpart *blob*))
 #'>
 :key #'third)
(defvar *scan-pos* nil)
#+nil
(with-open-file (s "/home/martin/d0609/scan-pos-time.lisp" :direction :output
		   :if-does-not-exist :create :if-exists :supersede)
 (write *scan-pos* :stream s))

(defun print-maxima (scan-pos &optional (lcos-to-cam t))
  (format t "load(minpack)$~%q:-1$~%g(s,p,tx,ty):=[~%")
  (let ((n (length scan-pos)))
   (dotimes (i n) 
     (let ((e (elt scan-pos i)))
       (destructuring-bind (lx ly (x y intens)) e
	 (let ((s "s*(cos(p)*~a+q*sin(p)*~a)+tx-~a,s*(-sin(p)*~a+q*cos(p)*~a)+ty-~a")) 
	   (if lcos-to-cam
	       (format t s x y lx x y ly)
	       (format t s lx ly x lx ly y)))
	 (format t (if (= i (1- n))
		       "]$~%minpack_lsquares(g(s,p,x,y),[s,p,x,y],[0.88,-3.1,1200,-20]);~%"
		       ",~%")))))))

#+nil
(print-maxima *scan-pos* t) 

#+nil
(defparameter *sscan* (sort  *scan-pos* #'> :key #'(lambda (e) (third (third e)))))
#+nil
(defparameter *scan-pos* *scan-pos2*)
#+nil
(defparameter *ascan* (subseq (sort  *scan-pos* #'> :key #'(lambda (e) (third (third e))))
			      0 20))


#||
load(minpack)$
q:-1;
g(s,p,tx,ty):=[   ]$
minpack_lsquares(
  g(s,p,x,y),
  [s,p,x,y],
  [0.88,-3.1,1200,-20]);

||#
(defparameter *presentation-time* nil)
(defvar *scan* nil)
(defvar *scan-result* nil)
(defvar *old-e* nil)

#+nil
(check (start-acquisition))
#+nil
(check (abort-acquisition))
#+nl
(clara:status)
#+nil
(check (free-internal-memory))

#+nil
(defun draw-screen ()
  (when *t8*
      (gl:with-pushed-matrix
	(let ((tex (make-instance 'gui::texture :data *t8*)))
	  (destructuring-bind (h w) (array-dimensions *t8*)
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)
		      :wt 1s0 :ht 1s0))
	  (gui:destroy tex))))
  (when *scan*  
    (let ((e (pop *scan*)))
      (destructuring-bind (i j radius r g b type) e
	(format t "prepare image type ~a%~%" type)
	(when *old-e*
	  (sleep .1)
	  (capture)
	  (push (list *old-e* (copy-image *line*)
		      :capture-time (multiple-value-list (common-lisp-user::get-time-of-day))
		      :presentation-time *presentation-time*)
		*scan-result*))
	(gl:with-pushed-matrix
	  (gl:translate 0 1024 0)
	  (%gl:color-3ub r g b)
	  (draw-disk i j radius)
	  (setf *presentation-time* (multiple-value-list (common-lisp-user::get-time-of-day))))
	(setf *old-e* e)))))

#+nil
(defvar *scan* nil)
#+nil
(progn
  (defparameter *scan* nil)
  (defparameter *old-e* nil)
  (defparameter *scan-result* nil)
  (check (start-acquisition))
  (let ((scanr nil))
    (dotimes (i 10)
      (push (list 0s0 0s0 0s0  0 0 0 :dark) scanr))
    (dotimes (i 10)
     (push (list 500s0 500s0 1000s0 #b10 11 0 :bright) scanr))
   (let ((d 2))
    (dotimes (i (* d 6))
      (dotimes (j (* d 5))
	(push (list (* (floor 50 d) (+ (* d 6) i)) 
		    (* (floor 50 d) (+ (* d 5) j)) 
		    3s0 #b11111110 255 255 :scan) scanr))))
    (setf *scan* (reverse scanr))))

#+nil
(check (abort-acquisition))
#+nil
(status)
#+nil
(defparameter *dark* (accum-img :dark))
#+nil
(defparameter *bright* (accum-img :bright))
#+nil
(defparameter *blob*
  (let ((a (make-array (array-dimensions *bright*)
		       :element-type '(complex single-float))))
    (destructuring-bind (y x) (array-dimensions a)
      (vol:do-region ((j i) (y x))
	(let* ((ii (* (- i (floor x 2)) (/ 1s0 x)))
	       (jj (* (- j (floor y 2)) (/ 1s0 x)))
	       (r2 (+ (* ii ii) (* jj jj))))
	  (setf (aref a j i) (complex (exp (* -.9e4 r2)))))))
    a))
#+nil
(vol:write-pgm "/dev/shm/blob.pgm" (vol:normalize-2-csf/ub8-realpart *blob*))

#+nil
(progn
  (defparameter *dark* (accum-img :dark))
  (defparameter *bright* (accum-img :bright))
  (defparameter *scan-pos* nil)
  (let ((diff (replace-zero-with-one (vol:.- *bright* *dark*)))
	(i 0))
    (dolist (e *scan-result*)
      (destructuring-bind ((ii j radius r g b type) img cap captime pres prestime) e
	(when (eq type :scan)
	  (let* ((blurred-point (vol:convert-2-csf/sf-realpart
				 (vol:convolve-circ-2-csf 
				  (vol:convert-2-sf/csf-mul 
				   (vol:./ (select-bigger (vol:.- (vol:convert-2-ub16/sf-mul (second e))
								  *dark*)
							  20s0)
					   diff))
				  *blob*)))
		 (maximum (first (sort (find-maxima blurred-point)
				       #'>
				       :key #'third))))
	    (push (list ii j maximum) *scan-pos*)
	    (vol:write-pgm (format nil "/dev/shm/o~3,'0d.pgm" (1- (incf i)))
			   (vol:normalize-2-sf/ub8 blurred-point))))))))
