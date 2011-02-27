;; add phase images to obtain a widefield image (approximation)
(require :asdf)
(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf asdf:*central-registry*
	'("../../0102/woropt-cyb-0628/"
	  "../../0131/rayt/"
	  "../../0126/bead-eval/"))
  (require :vol))

(defpackage :compare-illum-modes
  (:use :cl :vol))
(in-package :compare-illum-modes)

(defparameter *out-dir* "/dev/shm/")
(defmacro out (str &rest rest)
  `(concatenate 'string *out-dir* (format nil ,str ,@rest)))

(defparameter *data-dir* "/home/martin/d0216/6/")
(defmacro dir (str &rest rest)
  `(concatenate 'string *data-dir* (format nil ,str ,@rest)))

(defun correct-clara-byte-mess (img)
  (declare (type (simple-array (unsigned-byte 16) 2) img))
  (let ((a (make-array (array-dimensions img) :element-type '(unsigned-byte 16))))
   (destructuring-bind (y x) (array-dimensions img)
     (vol:do-region ((j i) (y (1- x)))
       (setf (aref a j i)
	     (+ (* 256 (ldb (byte 8 0) (aref img j (1+ i))))
		(ldb (byte 8 8) (aref img j i))))))
   a))

#+Nil
(defparameter *st* 
 (let ((stack ()))
   (dotimes (i 12)
     (let ((phases 
	    (directory
	     (dir "phase-~3,'0d-000-00?.pgm" i))))
       (push 
	(loop for e in phases collect
	     (correct-clara-byte-mess (read-pgm e)))
	stack)))
   (reverse stack)))

#+nil
(defparameter *ph-st*
 (destructuring-bind (y x) (array-dimensions (first (first *st*)))
   (let* ((slices (length *st*)) 
	  (stack (make-array (list (* 4 slices) y x)
			       :element-type '(unsigned-byte 16))))
     (dotimes (slice slices)
       (dotimes (phase 4)
	 (let ((a (elt (elt *st* slice) phase)))
	   (vol:do-region ((j i) (y x))
	     (setf (aref stack (+ phase (* 4 slice)) j i)
		   (aref a j i))))))
     stack)))
#+nil
(vol:save-stack-ub8 "/dev/shm/phase"
		    (vol:normalize-3-sf/ub8 (vol:convert-3-ub16/sf-mul *ph-st*)))

#+nil
(defparameter *wf-st*
 (destructuring-bind (y x) (array-dimensions (first (first *st*)))
   (let* ((slices (length *st*)) 
	  (wf-stack (make-array (list slices y x)
			       :element-type '(unsigned-byte 16))))
     (dotimes (slice slices)
       (dotimes (phase 4)
	 (let ((a (elt (elt *st* slice) phase)))
	   (vol:do-region ((j i) (y x))
	     (incf (aref wf-stack slice j i)
		   (aref a j i))))))
     wf-stack)))

#+nil
(vol:save-stack-ub8 "/dev/shm/wf"
		    (vol:normalize-3-sf/ub8 (vol:convert-3-ub16/sf-mul *wf-st*)))


#+Nil
(defparameter *sec-st*
 (destructuring-bind (y x) (array-dimensions (first (first *st*)))
   (let* ((slices (length *st*)) 
	  (stack (make-array (list slices y x)
			       :element-type '(unsigned-byte 16))))
     (dotimes (slice slices)
       (let ((a (elt (elt *st* slice) 0))
	     (b (elt (elt *st* slice) 1))
	     (c (elt (elt *st* slice) 2))
	     (d (elt (elt *st* slice) 3)))
	 (vol:do-region ((j i) (y (1- x)))
	   (setf (aref stack slice j i)
		 (- (max (aref a j i) (aref b j i) (aref c j i) (aref d j i))
		    (min (aref a j i) (aref b j i) (aref c j i) (aref d j i)))))))
     stack)))

#+nil
(vol:save-stack-ub8 "/dev/shm/sec"
		    (vol:normalize-3-sf/ub8 (vol:convert-3-ub16/sf-mul *sec-st*)))

(defun list->stack (ls)
  (destructuring-bind (y x) (array-dimensions (first ls))
   (let ((a (make-array (list (length ls) y x) :element-type '(unsigned-byte 16))))
     (dotimes (k (length ls))
       (let ((q (elt ls k)))
	(do-region ((j i) (y x))
	  (setf (aref a k j i) (aref q j i)))))
     a)))

#+nil
(defparameter *clem* 
  (let ((nuc (directory (dir "snap-full-illum-000-*.pgm"))))
    (list->stack
     (loop for e in nuc collect
	  (correct-clara-byte-mess (read-pgm e))))))

#+nil
(vol:save-stack-ub8 "/dev/shm/clem"
		    (vol:normalize-3-sf/ub8 (vol:convert-3-ub16/sf-mul *clem*)))


#+nil
(defparameter *ang* 
  (let ((nuc (directory (dir "snap-angle-illum-000-*.pgm"))))
    (list->stack
     (loop for e in nuc collect
	  (correct-clara-byte-mess (read-pgm e))))))

#+nil
(vol:save-stack-ub8 "/dev/shm/ang"
		    (vol:normalize-3-sf/ub8 (vol:convert-3-ub16/sf-mul *ang*)))