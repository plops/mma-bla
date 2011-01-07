#.(require :frontend)
#.(require :import)
#.(require :vol)
#.(require :bresenham)
(defpackage :run-ics
  (:use :cl))
(in-package :run-ics)
(declaim (optimize (speed 1) (safety 3) (debug 3)))

(defparameter *initial-nuclear-diameter* 21s0)
(defparameter *noise-threshold* .02s0)
(defparameter *min-drop* .3s0)
(defparameter *max-ray-change* 1.5s0)
(defparameter *min-ray-change* .333s0)
;; comparing filtered image
;; martin@desktop-big:/dev/shm/st> montage -geometry 291 -tile 2 `for i in *.pgm ;do echo ../a/$i $i;done|tr '\12' ' '` ../o.pgm

;; diameters and noise thresholds for different timepoints
#||
0  25
1  16     (use 21 diameter)
4  34 .1
5  37 .1
20 28 .02
34 29 .02
50 21
67 22
90 22
||#

#+nil
(defparameter icd
  (vol:convert-3-ub16/csf-mul 
   (import:read-ics4-stack "/home/martin/20101225/cele.ics" 34))) 

#+nil
(vol:write-pgm "/dev/shm/o.pgm"
 (vol:normalize-2-csf/ub8-realpart  (vol:cross-section-xz-csf icd)))
#+nil
(vol:write-pgm "/dev/shm/o.pgm"
 (vol:normalize-2-csf/ub8-realpart (vol:cross-section-xz-csf icf)))

#+nil
(vol:save-stack-ub8 "/dev/shm/a" (vol:normalize-3-csf/ub8-realpart icd))

(defun nuclear-seeds (v)
  "Find 3D maxima in 26 neighbourhood. Return list height, list z y x."
  (destructuring-bind (z y x) (array-dimensions v)
    (let ((res ()))
      (vol:with-arrays (v)
       (vol:do-region ((k j i) ((1- z) (1- y) (1- x)) (1 1 1))
	 (let ((c (v k j i)))
	   (macrolet ((q (l m n)
			`(< (v (+ k ,l) (+ j ,m) (+ i ,n)) c)))
	    (when (and (q 0 0 1) (q 0 0 -1)
		       (q 0 1 0) (q 0 -1 0)
		       (q 1 0 0) (q -1 0 0)
		       (q 1 1 0) (q 1 -1 0) (q -1 1 0)
		       (q 1 0 1) (q 1 0 -1) (q -1 0 1)
		       (q 0 1 1) (q 0 1 -1) (q 0 -1 1)
		       (q -1 1 1) (q 1 -1 1) (q 1 1 -1)
		       (q -1 -1 1) (q -1 1 -1) (q 1 -1 -1)
		       (q 1 1 1) (q -1 -1 -1))
	      (push (list c (list k j i)) res))))))
      res)))

#+nil
(biggest-part
 (point-list-sort (nuclear-seeds icf)) .92)

(defun mark-nuclear-seeds (vol)
  (let ((res (make-array (array-dimensions vol)
			 :element-type (array-element-type vol)))
	(points (biggest-part
		 (point-list-sort (nuclear-seeds vol)) .92)))
    (destructuring-bind (z y x) (array-dimensions vol)
      (vol:do-region ((k j i) (z y x))
	(setf (aref res k j i) (aref vol k j i))))
    (dolist (p points)
      (destructuring-bind (e (z y x)) p
	(setf (aref res z y x) 0s0)))
    res))

(defun mark-max (vol)
  (destructuring-bind (z y x) (array-dimensions vol)
    (let ((res (make-array (array-dimensions vol)
			   :element-type (array-element-type vol))))
      (dotimes (k z)
	(vol:do-region ((j i) ((1- y) (1- x)) (1 1))
	  (macrolet ((q (n m)
		       `(< (aref vol k (+ ,n j) (+ ,m i)) e)))
	    (let* ((e (aref vol k j i)))
	      (setf (aref res k j i)
		    (* (abs e) (if (and (q 0 1) (q 0 -1)
					(q 1 0) (q -1 0)
					(q 1 1) (q 1 -1)
					(q -1 1) (q -1 -1))
				   0s0 1s0)))))))
      res)))

(defun mark-max-slice (vol k)
  (destructuring-bind (z y x) (array-dimensions vol)
    (let ((points ()))
      (vol:do-region ((j i) ((1- y) (1- x)) (1 1))
	(macrolet ((q (n m)
		     `(< (aref vol k (+ ,n j) (+ ,m i)) e)))
	 (let* ((e (aref vol k j i)))
	   (when (and (q 0 1) (q 0 -1)
		      (q 1 0) (q -1 0)
		      (q 1 1) (q 1 -1)
		      (q -1 1) (q -1 -1))
	     (push (list e (list j i)) points)))))
      points)))



#+nil
(time
 (defparameter *kern*
   (destructuring-bind (z y x) (array-dimensions icd)
     (let* ((kern (make-array (array-dimensions icd)
			      :element-type '(complex single-float)))
	    (sigma (/ *initial-nuclear-diameter* 2.3548)) ;; convert FWHM to stddev
	    (sigma-b (* 1.6s0 sigma))
	    (f (expt (* 2s0 (coerce pi 'single-float)) 3/2))
	    (s (/ (* sigma sigma sigma f)))
	    (s-b (/ (* sigma-b sigma-b sigma-b f))))
       (vol:do-region ((k j i) (z y x))
	 (setf (aref kern k j i) 
	       (complex (let* ((ii (- i (floor x 2)))
					(jj (- j (floor y 2)))
					(kk (- k (floor z 2)))
			       (r2 (+ (* ii ii) (* jj jj) (* 5s0 5s0 kk kk)))) 
			  (-
			   (* s (exp (/ (- r2)
					(* 2s0 sigma sigma))))
			   (* s-b (exp (/ (- r2)
				    (* 2s0 sigma-b sigma-b))))
			   )))))
       (let ((sum 0s0))
	 (vol:do-region ((k j i) (z y x))
	   (incf sum (aref kern k j i)))
	 (vol::s* (/ 1e6 sum) kern))))))

#+nil
(time
 (progn
   (vol:save-stack-ub8 "/dev/shm/a" (vol:normalize-3-csf/ub8-realpart icd))
   (defparameter icf
	  (vol:convert-3-csf/sf-realpart (vol:convolve-circ-3-csf icd *kern*)))
   (defparameter *nuclear-center-set*
     (nuclear-seeds icf))
   (vol:save-stack-ub8 "/dev/shm/st"
		    (vol:normalize-3-sf/ub8 
		     (mark-max icf)))))
#+nil ;; mark 2d maxima
(vol:save-stack-ub8 "/dev/shm/st"
		    (vol:normalize-3-sf/ub8 
		     (mark-max icf)))
#+nil ;; mark significant 3d maxima (nuclear seeds)
(vol:save-stack-ub8 "/dev/shm/st"
		    (vol:normalize-3-sf/ub8 
		     (mark-nuclear-seeds icf)))

(defun point-list-histogram (points &optional (n 30))
  (let* ((points (point-list-sort points))
	 (mi (first (first (last points))))
	 (ma (first (first points)))
	 (s (/ n (- ma mi)))
	 (dups (mapcar #'(lambda (x) (floor (* s (- (first x) mi)))) 
		       points))
	 (hist (make-array (1+ n) :element-type 'fixnum)))
    (dolist (d dups)
      (incf (aref hist d)))
    (values hist n mi ma)))
#+nil
(point-list-histogram *nuclear-center-set*)

(defun print-histogram (hist n mi ma)
  (dotimes (i n)
    (format t "~6,3@f [~3S] " 
	    (+ (* i (/ (- ma mi) n))
	       mi)
	    (aref hist i))
    (dotimes (j (aref hist i))
      (format t "*"))
    (format t "~%")))

#+nil
(multiple-value-call #'print-histogram
  (point-list-histogram *nuclear-center-set* 20))


(defun point-list-sort (points)
  (sort points #'> :key #'first))

(defun biggest-part (seq &optional (frac .5s0))
  (remove-if #'(lambda (x) (< x (* frac (first (first seq)))))
	      seq :key #'first))

(defun point-list-bigger (seq value)
  (remove-if #'(lambda (x) (< x value))
	      seq :key #'first))



(defun nuc-candidates (vol k)
  (point-list-bigger (mark-max-slice vol k)
		     *noise-threshold*))
#+nil
(nuc-candidates icf 17)

(defun clamp (a mi ma)
  (cond ((< a mi) mi)
	((< ma a) ma)
	(t a)))

(defun nuc-min (vol expected-radius candidate angle k)
  "Go downhill in a direction and find zero or minimum."
  (destructuring-bind (az ay ax) (array-dimensions vol)
   (destructuring-bind (e (y x)) candidate
     (let ((mi e)
	   (zz (* 1s0 k)))
       (dotimes (i (floor (* 4 *max-ray-change* expected-radius)))
	 (let* ((r (* .5s0 i))
		(xx (+ x (* r (cos angle))))
		(yy (+ y (* r (sin angle))))
		(v (vol:interpolate-3-sf vol
					 (clamp zz 0s0 (* 1s0 (1- az)))
					 (clamp yy 0s0 (* 1s0 (1- ay)))
					 (clamp xx 0s0 (* 1s0 (1- ax))))))
	   (if (<= v mi)
	       (setf mi v)
	       (progn (format t "found minimum~%")
		      (return-from nuc-min (list r (list y x) (list yy xx)))))
	   (when (< v (* *min-drop* e))
	     (return-from nuc-min (list r (list y x) (list yy xx))))))))))



(defun point-list-median (points)
  "Return median length and position of median vector in list."
  (let* ((n (length points))
	 (index (loop for i below n collect i)))
    (values-list (nth (floor n 2)
		      (sort (mapcar #'(lambda (x y) (list (first x) y)) 
				    points index) #'< :key #'first)))))

(defun valid-lengths (points)
  (multiple-value-bind (reference-length ref-pos) (point-list-median points)
    (let ((n (length points))
	  (res (list (elt points ref-pos))))
      (dotimes (i n)
	(let ((cur (elt points (mod (1+ i) n))))
	  (destructuring-bind (r c s) cur
	    (when (< (* *min-ray-change* reference-length) 
		     r 
		     (* *max-ray-change* reference-length))
	      (push cur res)
	      (setf reference-length r)))))
      res)))
#+nil
(length (valid-lengths
  (loop for a below 16 collect
       (nuc-min icf 15s0 
		(first (nuc-candidates icf 17))
		(/ (* a 2 (coerce pi 'single-float)) 16)
		17))))

#+nil
(destructuring-bind (az ay ax) (array-dimensions icf)
   (let ((vol (vol:normalize-3-sf/ub8 
	       (mark-max icf)))
	 (pif (coerce pi 'single-float)))
     (loop for k from 1 below 39  do
	  (let* ((cands (nuc-candidates icf k))
		 (np 16))
	    (dolist (cand cands)
	      (let ((points
		     (valid-lengths
		      (remove-if #'null
				 (loop for a below np collect
				      (nuc-min icf 15s0 
					       cand
					       (/ (* pif a 2) np)
					       k))))))
		(dolist (p points)
		  (destructuring-bind (r c s) p
		    (format t "~a~%" r)
		    (setf (aref vol 
				k
				(floor (clamp (first s) 0 (1- ay)))
				(floor (clamp (second s) 0 (1- ax))))
			  128)))))))
     (vol:save-stack-ub8 "/dev/shm/st" vol)))
