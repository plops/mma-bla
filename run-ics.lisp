#.(require :frontend)
#.(require :import)
#.(require :vol)
#.(require :bresenham)
(defpackage :run-ics
  (:use :cl))
(in-package :run-ics)
(declaim (optimize (speed 1) (safety 3) (debug 3)))

#+nil
(defparameter icd
  (vol:convert-3-ub16/csf-mul 
   (import:read-ics4-stack "/media/disk/cele.ics" 35))) 

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
	    (sigma (/ 17s0 2.3548)) ;; convert FWHM to stddev
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
       kern))))
#+nil
(time
 (defparameter icf
   (vol:convert-3-csf/sf-realpart (vol:convolve-circ-3-csf icd *kern*))))
#+nil ;; mark 2d maxima
(vol:save-stack-ub8 "/dev/shm/st"
		    (vol:normalize-3-sf/ub8 
		     (mark-max icf)))
#+nil ;; mark significant 3d maxima (nuclear seeds)
(vol:save-stack-ub8 "/dev/shm/st"
		    (vol:normalize-3-sf/ub8 
		     (mark-nuclear-seeds icf)))
(defun point-list-sort (points)
  (sort points #'> :key #'first))
(defun biggest-part (seq &optional (frac .5s0))
  (remove-if #'(lambda (x) (< x (* frac (first (first seq)))))
	      seq :key #'first))

(defun nuc-candidates (vol k)
 (let ((sorted (point-list-sort (mark-max-slice vol k))))
   (biggest-part sorted .45s0)))
#+nil
(nuc-candidates icf 17)

(defparameter *min-drop* .3s0)
(defparameter *max-ray-change* 1.5s0)
(defparameter *min-ray-change* .333s0)

(defun nuc-min (vol expected-radius candidate angle k)
 (destructuring-bind (e (y x)) candidate
   (let ((mi e)
	 (zz (* 1s0 k)))
     (dotimes (i (floor (* 4 *max-ray-change* expected-radius)))
       (let* ((r (* .5s0 i))
	      (xx (+ x (* r (cos angle))))
	      (yy (+ y (* r (sin angle))))
	      (v (vol:interpolate-3-sf vol zz yy xx)))
	 (if (<= v mi)
	     (setf mi v)
	     (progn (format t "found minimum~%")
		    (return-from nuc-min (list r (list y x) (list yy xx)))))
	 (when (< v (* *min-drop* e))
	   (return-from nuc-min (list r (list y x) (list yy xx))))))
     mi)))



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
(let ((vol (vol:normalize-3-sf/ub8 
		(mark-max icf))))
  (loop for k from 17 below 19 do
    (let* ((cands (nuc-candidates icf k))
	   (np 16))
     (dolist (cand cands)
       (let ((points
	      (valid-lengths
	       (loop for a below np collect
		    (nuc-min icf 15s0 
			     cand
			     (/ (* a 2 (coerce pi 'single-float)) np)
			     k)))))
	 (dolist (p points)
	   (destructuring-bind (r c s) p
	     (setf (aref vol k (floor (first s)) (floor (second s)))
		   128)))))))
  (vol:save-stack-ub8 "/dev/shm/st" vol))
