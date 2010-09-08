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
  (unless (= 0 (connect))
    (error "Library couldn't connect to board."))
  (load-configuration "/home/martin/linux-mma2/boardini/800803.ini")
  (set-voltage +volt-pixel+ 17.5s0))

(defun write-data (buf &key (pic-number 1))
  "Write a 256x256 unsigned-short buffer to the device."
  (declare ((simple-array (unsigned-byte 16) 2) buf)
	   (values null &optional))
  (sb-sys:with-pinned-objects (buf)
    (write-matrix-data pic-number 3
		       (sb-sys:vector-sap 
			(sb-ext:array-storage-vector 
			 buf))
		       (* 256 256)))
  nil)
 
(defun draw (&key (r-small 0.0) (r-big 1.0) (pic-number 1))
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
    (write-data buf :pic-number pic-number)
    nil))
 
(defun parse-bits (value bits)
  (let ((result nil))
    (loop for (name x) in bits do
	 (when (logand value x)
	   (push name result)))
    result))

(defun parse-status-bits (value)
  (parse-bits value
	      '((peltier-on #x1000)
		(power-on #x4000)
		(start-matrix #x8000)
		(smart-adr-on #x10000)
		(extern-start-en #x40000))))

#+nil
(parse-status-bits 392342) 

(defun parse-error-bits (value)
  (parse-bits value
	      '((mirror-voltage #x01)
		(module-error #x02)
		(calibration-error #x04)
		(temperature-alert #x10)
		(matrix-ready-error #x20)
		(channel-overflow #x40)
		(ram-test-error #x100)
		(supply-error #x200)
		(config-error #x400))))
#+nil
(parse-error-bits 99939)

(defun status ()
  (multiple-value-bind (retval status error)
      (read-status)
    (unless (= 0 retval)
      (format t "read-status didn't return 0."))
    (let ((lerror (parse-error-bits error)))
     (if lerror
       (format t "error(s) ~a detected, status: ~a, retval: ~a~%"
	       (list error lerror) (parse-status-bits status) retval)
       (format t "status ~a~%" (parse-status-bits status))))))
 
#+nil
(status)

(defun update-picture (&key (r .5) (dr (- 1.0 r)))
  (draw :r-small (- r dr) :r-big (+ r dr))
  (set-picture-sequence 1 1 0))
 
(defun begin ()
  (set-power-on)
  (set-start-mma)
  (sleep 1) 
  (status))
 
(defun end ()
  (set-stop-mma)
  (set-power-off))

(defun load-pictures (&key (n 12) (dr .02) (ready-out-needed 0))
  (dotimes (i n)
    (let ((r (/ .2 n)))
      (format t "~a~%" `(picture ,i / ,n))
      (draw :pic-number (1+ i)
	    :r-small (- r dr)
	    :r-big (+ r dr))))
  (dotimes (i n)
    (set-picture-sequence (1+ i) 
			  (if (< i (- n 1))
			      0
			      1)
			  ready-out-needed)))

#+nil
(time
 (progn
   (init)
   (load-pictures :n 4)
   (begin)))

#+nil
(time
 (progn
   (set-stop-mma)
   (load-pictures :n 3)
   (begin)))

#+nil
(progn
  (end)
  (disconnect))
