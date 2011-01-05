(in-package :import)
(defun read-ics4-slice (name slice stack &key (x 291) (y 354) (z 41) (d 91))
  "Read a slice of Jean-Yves worm stack video."
  (declare ((or pathname string) name)
	   (integer slice)
	   (integer stack)
	   (values (array (unsigned-byte 16) 2) &optional))
  (assert (< -1 slice z))
  (assert (< -1 stack d))
  (let ((pos
	 (with-open-file (s name :direction :input)
	   (do ((l (read s) (read s)))
	       ((string= "END" (unless (numberp l) (string l)))))
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

(defun read-ics4-stack (name stack &key (x 291) (y 354) (z 41) (d 91))
  "Read a stack of Jean-Yves worm stack video."
  (declare (string name)
	   (integer stack)
	   (values (array (unsigned-byte 16) 3) &optional))
  (assert (< -1 stack d))
  (let ((pos
	 (with-open-file (s name :direction :input)
	   (do ((l (read s) (read s)))
	       ((string= "END" (unless (numberp l) (string l)))))
	   (file-position s))))
    (with-open-file (s name :direction :input
		       :element-type '(unsigned-byte 16))
      ;; FIXME: why do I need -170 offset?
      (file-position s (+ (- 170) pos 
			  (* stack z y x 2)))
      (let* ((a (make-array (list z y x)
			    :element-type '(unsigned-byte 16)))
	     (a1 (make-array (* z y x)
			     :element-type '(unsigned-byte 16)
			     :displaced-to a)))
	(read-sequence a1 s)
	a))))
#+nil
(defparameter edc
    (read-ics4-slice "/media/disk/cele.ics" 32 43))
