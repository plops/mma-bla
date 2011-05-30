(defpackage :mma
  (:use :cl :ipms-ffi)
  (:export
   #:init 
   #:uninit
   #:standby
   #:wake-up
   #:write-data
   #:select-pictures
   #:load-disk-raster
   #:load-concentric-circles
   #:draw-disk
   #:status
   #:set-nominal-deflection-nm
   #:get-nominal-deflection-nm
   #:wait-until-network-is-available
   ))

(in-package :mma)
 
(defun set-extern-trigger (&optional (on t))
  (if on
      (unless (= 0 (enable-extern-start))
	(break "enable-extern-start didn't return 0."))
      (unless (= 0 (disable-extern-start))
	(break "disable-extern-start didn't return 0."))))

(defun check-network ()
  "Returns empty string when there is no connection to control board."
  (with-output-to-string (stream)
    (sb-ext:run-program 
     "/bin/bash"
     (list "-c"
	   "netstat -anp 2> /dev/null|grep 192.168.0.2:4002") 
    :output stream)))
#+nil
(equal "" (check-network))

(defun wait-until-network-is-available ()
  (loop
     for i below 30 
     until (equal "" (check-network))
     do
       (format t "control board already connected, waiting ~d/30~%" i)
       (sleep 2)))


(defun write-data (buf &key (pic-number 1))
  "Write a 256x256 unsigned-short buffer to the device."
  (declare ((simple-array (unsigned-byte 16) (256 256)) buf)
	   (values null &optional))
  (let ((buf1 (sb-ext:array-storage-vector buf)))
    (sb-sys:with-pinned-objects (buf)
      (write-matrix-data pic-number 3 (sb-sys:vector-sap buf1) 
			 (* 2 (length buf1)))))
  nil)

(defun draw-ring (&key (r-small 0.0) (r-big 1.0) (pic-number 1))
  (declare (single-float r-small r-big)
	   (fixnum pic-number)
	   (values null &optional))
  (let* ((n 256)
	 (nh (floor n 2))
	 (1/n (/ 1.0 n))
	 (buf (make-array (list n n) 
			  :element-type '(unsigned-byte 16))))
    (declare (type (simple-array (unsigned-byte 16) 2) buf))
    (dotimes (j n)
      (dotimes (i n)
	(let* ((x (* 2.0 1/n (- i nh)))
	       (y (* 2.0 1/n (- j nh)))
	       (r (sqrt (+ (* x x) (* y y)))))
	  (setf (aref buf i j) 
		(if (< r-small r r-big) 0 #xffff)))))
    (write-data buf :pic-number pic-number)
    nil))

(defun draw-disk (&key (cx 0) (cy 0) (radius .1) (pic-number 1) (value 0))
  (declare (single-float radius)
	   (fixnum cx cy pic-number) 
	   (values (simple-array (unsigned-byte 16) 2) &optional))
  (let* ((n 256)
	 (nh (floor n 2))
	 (1/n (/ 1.0 n))
	 (buf (make-array (list n n) 
			  :element-type '(unsigned-byte 16))))
    (declare (type (simple-array (unsigned-byte 16) 2) buf))
    (dotimes (j n)
      (dotimes (i n)
	(let* ((x (* 2.0 1/n (- i nh cx)))
	       (y (* 2.0 1/n (- j nh cy)))
	       (r (sqrt (+ (* x x) (* y y)))))
	  (setf (aref buf i j) 
		(if (< r radius) value #xffff)))))
    (write-data buf :pic-number pic-number)
    buf))

(defun draw-grating (&key (pic-number 1))
  (declare (values (simple-array (unsigned-byte 16) 2) &optional))
  (let* ((n 256)
	 (buf (make-array (list n n) 
			  :element-type '(unsigned-byte 16))))
    (declare (type (simple-array (unsigned-byte 16) 2) buf))
    (dotimes (j n)
      (dotimes (i n)
	(setf (aref buf i j) 
	      (if (= 0 (mod i 2)) 0 #xffff))))
    (write-data buf :pic-number pic-number)
    buf))

(defun parse-bits (value bits)
  (declare (fixnum value))
  (let ((result nil))
    (loop for (name x) in bits do
	 (when (= x (logand value x))
	   (push name result)))
    result))

(defun parse-status-bits (value)
  (parse-bits 
   value '((peltier-on #x1000) (power-on #x4000) (start-matrix #x8000)
	   (smart-adr-on #x10000) (extern-start-en #x40000))))

(defun parse-error-bits (value)
  (parse-bits value
	      '((mirror-voltage #x01) (module-error #x02) (calibration-error #x04)
		(temperature-alert #x10) (matrix-ready-error #x20) (channel-overflow #x40)
		(ram-test-error #x100) (supply-error #x200) (config-error #x400))))

(defun status ()
  (multiple-value-bind (retval status error) (read-status)
    (unless (= 0 retval)
      (error "read-status didn't return 0 but ~d.~%" retval))
    (if (/= 0 error)
	(format t "error: ~a~%error-bits:~%~a~% status-bits:~%~a~%retval: ~a~%"
		error (parse-error-bits error)
		(parse-status-bits status)
		retval)
	(format t "status-bits ~a~%" (parse-status-bits status)))
    (parse-status-bits status)))

(defun select-pictures (start &key (n 1) (ready-out-needed nil))
  (dotimes (i n)
    (set-picture-sequence (+ 1 start i)
			  (if (< i (- n 1)) 0 1)
			  (if ready-out-needed 1 0))))

(defun load-concentric-disks (&key (n 12) (ready-out-needed t))
  (let ((result nil))
    (dotimes (i n)
      (let ((r (/ (* 1.0 (1+ i)) n)))
	(format t "~a~%" `(picture ,i / ,n))
	(push (draw-disk :cx 0 :cy 0
			 :pic-number (1+ i)
			 :radius r)
	     result)))
    (select-pictures 0 :n n :ready-out-needed ready-out-needed)
    (reverse result)))

(defun load-disk-raster (&key (n 12))
  (declare (values cons &optional))
  (let ((shift (if (evenp n) 
		   (floor 256 (* 2 n))
		   0))
	(result nil))
   (dotimes (j n)
     (dotimes (i n)
       (let ((x (floor (* 256 (- i (floor n 2)) (/ 1.0 n))))
	     (y (floor (* 256 (- j (floor n 2)) (/ 1.0 n)))))
	 (push (draw-disk-cal :cx (+ x shift)
			      :cy (+ y shift)
			      :r-big (/ 1s0 n)
			      :pic-number (1+ (+ i (* n j))))
	       result))))
   (select-pictures 0 :n (* n n))
   (reverse result)))

(defun set-nominal-deflection-nm (&optional (value 118.25s0))
  (declare (single-float value))
  (ipms-ffi:set-parameter 1001 value 4))

#+nil
(set-nominal-deflection-nm 10s0)

(defun get-nominal-deflection-nm ()
  (get-parameter 1001 4))

#+nil
(get-nominal-deflection-nm)

