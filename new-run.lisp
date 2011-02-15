(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :mma))

(in-package :mma)

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
(set-extern-ready 16s0 16300s0)


#+nil
(set-deflection-phase 16s0 16300s0)

#+nil
(mma::set-cycle-time 90s0)

#+nil
(mma:set-nominal-deflection-nm 118.25)


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
      (setf (aref m j i) (* 0    (* (mod i 2) #+nil (mod j 2)) 4095))))
  (mma:draw-array-cal m :pic-number 1)
  nil)

#+nil
(mma::set-stop-mma)
#+nil
(mma::set-power-off)
#+nil
(disconnect)

