(require :clara)
(defpackage :run
  (:use :cl :clara))
(in-package :clara)


(check 
  (set-current-camera
   (val2 (get-camera-handle 
	  (1- (val2 (get-available-cameras)))))))

(check
  (initialize "/usr/local/etc/andor"))

(get-detector)
(cooler-on)
(cooler-off)
(check (set-temperature (val3 (get-temperature-range))))
(get-temperature-f)

(trigger-mode :internal)

(set-exposure-time .01)

(read-mode :image)

(acquisition-mode :single-scan)


;; set-accumulation-cycle-time
;; aet-number-accumulations (that's just in memory)

(set-image 1 1
	   1 1392
	   1 1040)

(start-acquisition)
(abort-acquisition)

(lookup-error (val2 (get-status)))

(progn
 (defparameter *q*
   (let* ((w 1392)
	  (h 1040)
	  (img (make-array (list h w) :element-type '(unsigned-byte 16)))
	  (img1 (sb-ext:array-storage-vector img))
	  (sap (sb-sys:vector-sap img1)))
     (sb-sys:with-pinned-objects (img)
       (get-acquired-data16 sap (length img1)))
     img))
 nil)

(save-as-sif "/dev/shm/o.sif")

(check
 (free-internal-memory))

;; set-high-capacity
;; set-output-amplifier
;; is-amplifier-available

;; (is-pre-amp-gain-available 0 0)

#+nil
(save-camera-specs "/home/martin/0517/clara-e.cap")


