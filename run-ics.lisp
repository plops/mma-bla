#.(require :frontend)
#.(require :import)
#.(require :vol)

(defpackage :run-ics
  (:use :cl))
(in-package :run-ics)
(declaim (optimize (speed 1) (safety 3) (debug 3)))

#+nil
(defparameter icd
 (import:read-ics4-stack "/media/disk/cele.ics" 43)) 

#+nil
(vol:write-pgm "/dev/shm/o.pgm"
 (vol:normalize-2-sf/ub8  (vol:cross-section-xz-sf (vol:convert-3-ub16/sf-mul icd))))
#+nil
(vol:write-pgm "/dev/shm/o.pgm"
 (vol:normalize-2-csf/ub8-realpart (vol:cross-section-xz-csf icf)))
#+nil
(defparameter a (vol:convert-3-ub16/csf-mul icd))

#+nil
(vol:save-stack-ub8 "/dev/shm/a" (vol:normalize-3-csf/ub8-realpart a))
#+nil
(vol:save-stack-ub8 "/dev/shm/st" (vol:normalize-3-sf/ub8 (mark-max (vol:convert-3-csf/sf-realpart icf))))
#+nil 
(defparameter *q* (vol:convert-3-csf/sf-realpart icf))
(defun mark-max (vol)
  (destructuring-bind (z y x) (array-dimensions vol)
    (let ((res (make-array (array-dimensions vol)
			   :element-type (array-element-type vol))))
     (dotimes (k z)
       (vol:do-region ((j i) ((1- y) (1- x)) (1 1))
	 (let* ((a (aref vol k (1+ j) i))
		(b (aref vol k j (1+ i)))
		(c (aref vol k (1- j) i))
		(d (aref vol k j (1- i)))
		(e (aref vol k j i)))
	   (setf (aref res k j i)
		 (* (abs e) (if (and (< a e)
				 (< b e)
				 (< c e)
				 (< d e))
			    0s0
			    1s0))))))
     res)))



#+nil
(time
 (defparameter *kern*
   (destructuring-bind (z y x) (array-dimensions a)
     (let* ((kern (make-array (array-dimensions a)
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
   (vol:convolve-circ-3-csf a *kern*)))