(require :asdf)
(push "/home/martin/0505/mma/" asdf:*central-registry*)
(require :clara)
(defpackage :run-clara
  (:use :cl :clara))
(in-package :run-clara)

(progn
  (check 
    (set-current-camera
     (val2 (get-camera-handle 
	    (1- (val2 (get-available-cameras)))))))
  
  (check
    (initialize "/usr/local/etc/andor"))
  (trigger-mode :external)
  (set-exposure-time .0121)
  (read-mode :image)
  (acquisition-mode :single-scan)
  (check
    (set-image 1 1
	       1 1392
	       1 1040))
  (set-ad-channel 1))
(set-ad-channel 1)
(get-temperature-range)

(get-detector)
(cooler-on)
(cooler-off)
(check (set-temperature (val2 (get-temperature-range))))
(get-temperature-f)
(uninit)
(check
 (set-exposure-time  .01521))

#+nil
(set-shutter 1 1 0 0)

(clara:status)


(trigger-mode :internal)
(trigger-mode :external)

(clara:uninit)



;; set-accumulation-cycle-time
;; aet-number-accumulations (that's just in memory)



(defparameter *blub*
  (let* ((w 1392)
	 (h 1040)
	 (img (make-array (list h w)
			  :element-type '(unsigned-byte 16)))
	 (img1 (sb-ext:array-storage-vector img))
	 (sap (sb-sys:vector-sap img1)))
    (progn
      (start-acquisition)
      (loop while (not (eq 'clara::DRV_IDLE
			   (lookup-error (val2 (get-status)))))
	 do
	   (sleep .01))
      (sb-sys:with-pinned-objects (img)
	(get-acquired-data16 sap (length img1)))
      (check
	(free-internal-memory)))
   img))

(reduce #'max (sb-ext:array-storage-vector *blub*))

(lookup-error (val2 (get-status)))
(check (abort-acquisition))



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



;; set-high-capacity
;; set-output-amplifier
;; is-amplifier-available

;; (is-pre-amp-gain-available 0 0)

#+nil
(save-camera-specs "/home/martin/0517/clara-e.cap")

(require :cl-utilities) ;; I need this to split a string into tokens
(require :sb-vector-io) ;; to read single-float values fast




(defun read-sif (fn)
  (with-open-file (s fn)
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
	  (with-open-file (s fn :element-type '(unsigned-byte 8))
	    (file-position s pos)
	    (vector-io:read-vector-data img1 s))
	  img)))))

(prog1 nil
 (defparameter *bla*
   (read-sif "/dev/shm/o.sif")))

(defun sum (a b)
  (destructuring-bind (h w) (array-dimensions a)
    (let ((sum 0)) 
      (dotimes (j h)
       (dotimes (i w)
	 (incf sum (expt  (- (aref a j i)
			     (aref b j i)) 2))))
      sum)))
(sum *q* *bla*)

#+nil
(uninit)