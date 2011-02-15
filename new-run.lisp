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
 "/home/martin/cyberpower-mit/mma-essentials-0209/VC2610_13_61_2010-12-02_Rand-5_0-250nm_Typ1.cal")


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
(let* ((n 256)
       (m (make-array (list n n) :element-type '(unsigned-byte 12))))
  (dotimes (j n)
    (dotimes (i n)
      (setf (aref m j i) (* #+nil (* (mod i 2) (mod j 2)) 4095))))
  (mma:draw-array-cal m :pic-number 1)
  nil)

#+nil
(mma::set-stop-mma)
#+nil
(mma::set-power-off)
#+nil
(disconnect)
