#.(load "ipms-ffi.lisp")
 
(defpackage :ipms
  (:use :cl :ipms-ffi))
(in-package :ipms)
 
(defun init ()
  (register-board #x0036344B00800803
		  "192.168.0.2"
		  "255.255.255.0"
		  "0.0.0.0" 
		  4001)
  (set-local-interface "192.168.0.1"
		       4001)
  (connect)
  (load-configuration "/home/martin/linux-mma2/boardini/800803.ini")
  (set-voltage +volt-pixel+ 17.5s0))
 
(declaim (ftype (function ((simple-array (unsigned-byte 16) (* *)))
			  (values null &optional)) write-data))
(defun write-data (buf)
  "Write a 256x256 unsigned-short buffer to the device."
  (sb-sys:with-pinned-objects (buf)
    (write-matrix-data 1 3
		       (sb-sys:vector-sap 
			(sb-ext:array-storage-vector 
			 buf))
		       (* 256 256)))
  nil)
 
(defun draw (&key (r-small 0.0) (r-big 1.0))
  (declare (single-float r-small r-big)
	   (values null &optional))
  (let* ((n 256)
	 (nh (floor n 2))
	 (1/n (/ 1.0 n))
	 (buf (make-array (list n n) 
			:element-type '(unsigned-byte 16))))
    (declare (type (simple-array (unsigned-byte 16) (* *))))
    (dotimes (j n)
      (dotimes (i n)
	(let* ((x (* 2.0 1/n (- i nh)))
	       (y (* 2.0 1/n (- j nh)))
	       (r (sqrt (+ (* x x) (* y y)))))
	  (setf (aref buf i j) 
		(if (< r-small r r-big)
		    0
		    #xffff)))))
    (write-data buf)
    nil))
 
 
(defun status ()
  (multiple-value-bind (status error)
      (read-status)
    (format t "status ~a~%" (list status error))))
 
(defun run (&key (r .5) (dr (- 1.0 r)))
  (draw :r-small (- r dr) :r-big (+ r dr))
  (set-picture-sequence 1 1 0)
  (set-power-on)
  (set-start-mma)
  (status))
 
 
(defun end ()
  (set-stop-mma)
  (set-power-off))

;; init has to return 0
#+nil
(progn
 (init))
#+nil
(time
 (status))

#+nil
(time
 (loop for r from 0.0 upto 1.0 by .02 do
      (let ((dr .1))
	(format t "~a~%" (list r))
	(sleep 1)
	(draw :r-small (- r dr) :r-big (+ r dr))
	(set-picture-sequence 1 1 0))))

#+nil
(time
 (run :r .7))

#+nil
(end)

#+nil
(disconnect)