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
(set-ad-channel 1)

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

(let ((s *bl*))
  
  (subseq s 0 
	  (position #\Space
		    s)))



(require :cl-utilities) ;; I need this to split a string into tokens

(defun read-sif (fn)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-open-file (s fn :direction :input)
    (let ((head-lines
	   (loop for i below 32 collect 
		(read-line s))))
      (destructuring-bind (a0 a1 h w a4 a5 a6 a7)
	  (cl-utilities:split-sequence #\Space (elt head-lines 29))
	(declare (ignore a0 a1 a4 a5 a6 a7))
	(let* ((img (make-array (list (read-from-string h)
				      (read-from-string w))
				:element-type 'single-float))
	       (img1 (sb-ext:array-storage-vector img))
	       (pos (file-position s)))
	  (declare (type (simple-array (unsigned-byte 32) 2) img))
	  (with-open-file (s fn :element-type '(unsigned-byte 32))
	    (file-position s pos)
	    (read-sequence img1 s))
	  img))))))


(defparameter *bla*
 (read-sif "/dev/shm/o.sif"))

(uninit)