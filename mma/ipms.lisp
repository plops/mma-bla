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

(defmacro check (cmd)
  `(progn
     (format t "running ~a~%" ',cmd)
     (unless (= 0 ,cmd)
      (error "Error while running ~a." ',cmd))))
 
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
      (check
       (write-matrix-data pic-number 3 (sb-sys:vector-sap buf1) 
			  (* 2 (length buf1))))))
  nil)

(defun fill-constant (value &key (pic-number 1))
  (declare (type (unsigned-byte 16) value)
	   (type (integer 1 1023) pic-number))
  (let ((a (make-array (list 256 256) 
		       :element-type '(unsigned-byte 16)
		       :initial-element value)))
    (write-data a :pic-number pic-number)
    (check (set-picture-sequence pic-number
				 pic-number

				 1))))
#+nil
(fill-constant 0)

#+nil
(progn
 (check (register-board #x0036344B00800803 "192.168.0.2"
		  "255.255.255.0" "0.0.0.0" 4001))
 (check (set-local-interface "192.168.0.1" 4001))
 (check (connect)))
#+nil
(let* ((s (format nil #+nil "/home/martin/24811567.cal" "/home/martin/cyberpower-mit/mma-essentials-0209/VC2481_15_67_2011-02-01_0-250nm_Rand7_Typ1.cal"))
       (a (make-array (1+ (length s)) :element-type '(unsigned-byte 8))))
  (dotimes (i (length s)) 
    (setf (aref a i) (char-code (char s i))))
  (sb-sys:with-pinned-objects (a)
    (load-calibration-data (sb-alien:sap-alien (sb-sys:vector-sap a)
					       (sb-alien:c-string)))))
(sb-alien:define-alien-routine ("SLM_SaveConfiguration" save-configuration)
    sb-alien:int
  "Save a board configuration and setup board with values."
  (filepath sb-alien:c-string))
#+nil
(save-configuration "saveconf")
#+nil 
(sb-posix:getcwd)
#+nil
(load-configuration "/home/martin/3ini")
#+nil
(set-power-off)
#+nil
(disconnect)
#+nil
(status)
#+nil
(init)
(defun init (&key
	     (id #x0036344B00800803)
	     (board-ip "192.168.0.2")
	     (local-ip "192.168.0.1")
	     (netmask "255.255.255.0")
	     (gateway "0.0.0.0")
	     (port 4001)
	     (config "/home/martin/cyberpower-mit/mma-essentials-0209/800803_dmdl6_20110215.ini")
	     (calibration "/home/martin/mma-essentials-0209/VC2481_15_67_2011-02-01_0-250nm_Rand7_Typ1.cal")
	     (width-ms (- 16s0 .522)))
  (check (register-board id
			 board-ip
			 netmask
			 gateway
			 port))
  (check (set-local-interface local-ip port))
  (check (connect))
  (sb-sys:with-pinned-objects (config)
   (check (load-configuration config)))
  (check (load-calibration-data calibration))
  (check (set-deflection-phase .0 (* 1000 width-ms)))
  (check (set-voltage +volt-frame-f+ 20.0s0))
  (check (set-voltage +volt-frame-l+ 20.0s0))

  (let ((delay 20s0))
   (check (set-extern-ready delay (- (* 1000 width-ms)
				     delay))))
  (check (enable-extern-start))
  (check (set-cycle-time (+ .01 (* 2 width-ms))))
  (check (set-power-on))
  (fill-constant 4095)
  (set-nominal-deflection-nm (/ 473.0 4))
  (unless (= 0 (set-start-mma))
    (error "set-start-mma failed."))
  (sleep .1) ;; It takes a while until the board can tell if there is
	     ;; a matrix or not. Wait duration should be .1 .. 1s,
	     ;; increase the delay with the matrix detached until
	     ;; read-status returns the appropriate error.
  (status))

#+nil
(init)

(defun standby ()
  (check (set-stop-mma))
  (check (set-power-off)))

(defun wake-up ()
  (check (set-power-on))
  (check (set-start-mma)))

(defun uninit ()
  (standby)
  (check (disconnect)))

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

