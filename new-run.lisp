(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :mma))

(in-package :mma)
;; ifconfig eth1 192.168.0.1

(defmacro check (cmd)
  `(progn
     (format t "running ~a~%" ',cmd)
     (unless (= 0 ,cmd)
      (error "Error while running ~a status: ~a" ',cmd
	     (status)))))

(declaim (optimize (debug 3) (safety 3) (speed 1)))

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
  (check (load-configuration config))
  (check (load-calibration-data calibration))
  (check (set-deflection-phase .0 (* 1000 width-ms)))
  (set-voltage +volt-frame-f+ 20.0s0)
  (set-voltage +volt-frame-l+ 20.0s0)

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

(defun standby ()
  (check (set-stop-mma))
  (check (set-power-off)))

(defun wake-up ()
  (check (set-power-on))
  (check (set-start-mma)))

(defun uninit ()
  (standby)
  (check (disconnect)))


#+nil
(time (init)) ;; takes 6.3s
#+nil
(uninit)

#+nil
(register-board #x0036344B00800803
		"192.168.0.2"
		"255.255.255.0"
		"0.0.0.0" 
		4001)

#+nil
(set-local-interface "192.168.0.1"
		     4001)

#+nil
(connect)

#+nil
(plain-status)

#+nil
(load-configuration "/home/martin/3ini")


#+nil
(load-calibration-data 
 "/home/martin/cyberpower-mit/mma-essentials-0209/VC2481_15_67_2011-02-01_0-250nm_Rand7_Typ1.cal")


#+nil
(set-extern-ready (+ 20s0 16s0)
		  (- 16300s0 20s0)) ;; should start 20us later than deflection


#+nil
(set-deflection-phase 16s0 16300s0)

#+nil
(mma::set-cycle-time 90s0)

#+nil
(mma:set-nominal-deflection-nm 118.25)

#+nil
(let ((cmd "STM#DBE " ))
  (ipms-ffi::service-command "SEND#SRV" cmd (length cmd)))

#+nil
(set-power-on)

#+nil
(progn
 (load-white :pic-number 1)
 nil)

#+nil
(begin)


#+nil
(mma::plain-status)
#+nil
(mma:status)

#+nil
(mma::set-stop-mma)

#+nil
(mma::set-extern-trigger t)

#+nil
(mma::set-start-mma)

#+nil
(mma:select-pictures 0 :n 1 :ready-out-needed t)

#+nil
(let* ((n 256)
       (m (make-array (list n n) :element-type '(unsigned-byte 12))))
  (dotimes (j n)
    (dotimes (i n)
      (setf (aref m j i) (*  #+nil  (* (mod i 2) #+nil (mod j 2)) 4095))))
  (mma:draw-array-cal m :pic-number 1)
  nil)

#+nil
(mma::set-stop-mma)
#+nil
(mma::set-power-off)
#+nil
(disconnect)

