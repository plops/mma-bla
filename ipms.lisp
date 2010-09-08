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
  (set-voltage +volt-pixel+ 16s0))
 
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
 
(defun draw ()
  (let* ((n 256)
	 (buf (make-array (list n n) 
			:element-type '(unsigned-byte 16))))
    (declare (type (simple-array (unsigned-byte 16) (* *))))
    (dotimes (j n)
      (dotimes (i (/ n 2))
	(setf (aref buf i j)
	      0))
      (dotimes (i (/ n 2))
	(setf (aref buf (+ i (/ n 2)) j)
	      #xffff)))
    (write-data buf)))
 
 
(defun status ()
  (multiple-value-bind (status error)
      (read-status)
    (format t "status ~a~%" (list status error))))
 
(defun run ()
  (draw)
  (set-picture-sequence 1 1 0)
  (set-power-on)
  (set-start-mma)
  (status))
 
 
(defun end ()
  (set-stop-mma)
  (set-power-off))

#+nil
(progn
 (init))
#+nil
(status)

#+nil
(draw)

#+nil
(run)

#+nil
(end)