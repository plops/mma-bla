;; add phase images to obtain a widefield image (approximation)
(require :asdf)
(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf asdf:*central-registry*
	'("../../0102/woropt-cyb-0628/"
	  "../../0131/rayt/"
	  "../../0126/bead-eval/"))
  (require :vol))

(defparameter *data-dir* "/dev/shm/")
(defmacro dir (str &rest rest)
  `(concatenate 'string *data-dir* (format nil ,str ,@rest)))

(defparameter *st* 
 (let ((stack ()))
   (dotimes (i 12)
     (let ((phases 
	    (directory
	     (format nil 
		     "/home/martin/d0216/2/phase-~3,'0d-000-00?.pgm" i))))
       (push 
	(loop for e in phases collect
	     (vol:read-pgm e))
	stack)))
   stack))

(defparameter *wf-st*
 (destructuring-bind (y x) (array-dimensions (first (first *st*)))
   (let* ((slices (length *st*)) 
	  (wf-stack (make-array (list slices y x)
			       :element-type 'single-float)))
     (dotimes (slice slices)
       (dotimes (phase 4)
	 (let ((a (elt (elt *st* slice) phase)))
	  (vol:do-region ((j i) (y (1- x)))
	    (let ((v (aref a j i)))
	      (incf (aref wf-stack slice j i) (+ (* 256s0 (ldb (byte 8 0) (aref a j (1+ i))))
						 (ldb (byte 8 8) (aref a j i)))))))))
     wf-stack)))

(vol:save-stack-ub8 "/dev/shm/o"
 (vol:normalize-3-sf/ub8 *wf-st*))
