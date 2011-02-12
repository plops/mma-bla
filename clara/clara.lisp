(in-package :clara)
 
(defmacro check (&body body)
  `(let ((ret ,@body))
     (unless (eq drv-success ret)
       (error "~a didn't succeed. The error is ~a." ',@body (lookup-error ret)))
     t))

(defmacro val2 (fn)
  `(multiple-value-bind (a b)
       ,fn
     (check a)
     b))

(defmacro val3 (fn)
  `(multiple-value-bind (a b c)
       ,fn
     (declare (ignorable b))
     (check a)
     c))

(defun init-kinetic ()
  (let* ((cams (val2 (get-available-cameras)))
	 (handle (val2 (get-camera-handle (1- cams)))))
    (check (set-current-camera handle))
    (check (initialize "/usr/local/etc/andor"))
    (check (set-acquisition-mode 1)) ;; single scan
 
    ;; set vertical speed to max
    (let ((index (val2 (get-number-vs-speeds)))
	  (stemp 0d0)
	  (number 0))
      (dotimes (i index)
	(let ((speed (val2 (get-vs-speed i))))
	  (when (< stemp speed)
	    (setf stemp speed
		  number i))))
      (format t "~a~%" (list 'vs-speed number stemp))
      (check (set-vs-speed number)))
 
    ;; set horizontal speed to max
    (let ((index (val2 (get-number-hs-speeds 0 0)))
	  (stemp 0d0)
	  (number 0))
      (dotimes (i index)
	(let ((speed (val2 (get-hs-speed 0 0 i))))
	  (when (< stemp speed)
	    (setf stemp speed
		  number i))))
      (format t "~a~%" (list 'hs-speed number stemp))
      (check (set-hs-speed 0 number)))
    nil))

(defun acquire-kinetic (&key (exp-s .016s0) (cycle-time-s 2s0) (num-frames 10)
			(width 32) (height 32))
  (let ((image (make-array (list height width) :element-type '(unsigned-byte 16))))
   (check (set-acquisition-mode 5)) ;; run till abort
   (check (set-read-mode 4))	    ;; image mode
   (multiple-value-bind (a xdim ydim)
       (get-detector)
     (check a)
     (format t "~a~%" (list 'image xdim ydim))
     (let* ((xh (floor xdim 2))
	    (wh (floor width 2))
	    (xmin (- xh wh))
	    (xmax (+ xmin width))
	    (yh (floor ydim 2))
	    (hh (floor height 2))
	    (ymin (- yh hh))
	    (ymax (+ ymin height)))
       (check (set-image 1 1 (1+ xmin) xmax (1+ ymin) ymax))))
   (check (set-exposure-time exp-s))
   (check (set-number-accumulations 1))
   (check (set-kinetic-cycle-time cycle-time-s))
 
  ;; report desired and actual settings
   (multiple-value-bind (ret fexp faccum fkinetic)
       (get-acquisition-timings)
     (check ret)
     (format t "~a~%" 
	     (list (list 'desired 'exp exp-s 
			 'cycle cycle-time-s
			 'num num-frames)
		   (list 'actual 'exp fexp 
			 'kinetic-cycle fkinetic
			 'accum faccum))))
 
  ;; start acquiring
   (let ((startt (get-internal-real-time)))
     (check (start-acquisition))
     (multiple-value-bind (ret acc series-init)
	 (get-acquisition-progress)
       (declare (ignorable acc))
       (check ret)
       ;; leave loop when series has changed
       (loop (unless (eq series-init (val3 (get-acquisition-progress)))
	       (return)))
       (let ((timeprev (get-internal-real-time))
	     (ret 0)
	     (acc 0s0)
	     (series 0)
	     (seriesprev 0)
	     (frame-counter 0))
	 (loop 
	    (format t "~a~%" (list 'status (val2 (get-status))))
	    (multiple-value-setq (ret acc series)
	      (get-acquisition-progress))
	    (format t "~a~%" (list 'series series 'delta (- (get-internal-real-time)
							    timeprev)))
	    (check ret)
	    (when (< seriesprev series)
	      ;; new frame arrived use get-moste-recent-image16 to collect pixels
	      (sb-sys:with-pinned-objects (image)
		(check (get-most-recent-image16 (sb-sys:vector-sap (sb-ext:array-storage-vector image))
					  (* height width))))
 
	      ;; report time since previous frame
	      (format t "frame ~d captured after ~a ms~%" (incf frame-counter)
		      (- (get-internal-real-time)  timeprev))
	      (setf seriesprev series
		    timeprev (get-internal-real-time)))
	    (sleep .1d0)
	    ;; abort when enough frames
	    (when (eq (- series series-init) num-frames)
	      (return)))
	 (let ((deltat (- (get-internal-real-time) startt)))
	   (format t "~a~%" (list 'kinetic-finish 'total-time deltat
				  'effective-interval (/ (* 1d0 deltat) num-frames))))
	 (unless (eq 0 series)
	   (check (abort-acquisition))))))
   image))

;; use this if a defined number of images should be captured with
;; equal time intervals

#+nil
(time (init-kinetic))

#+nil
(time
 (defparameter *im*
   (acquire-kinetic :cycle-time-s .1s0)))

#+nil ;; if the series takes to long abort like this:
(check (abort-acquisition))

#+nil ;; close the camera, reopening will calibrate adc again (waiting 8s)
(check (shutdown))

(defvar *w* 32)
(defvar *h* 32) 
(defvar *adc-calibrated* nil)
(defvar *displayed-images* 0)
(defvar *start-series* nil)
(defparameter *im* nil)

(defun init (&key (width 512) (height 512) (xpos 0) (ypos 0) (exposure-s 1s0)
	     (fast-adc t) (external-trigger nil))
  (when *adc-calibrated*
    (when (is-acquiring-p)
      (clara:stop)))
  (init-single-scan :exposure-s exposure-s :width width :height height
		    :xpos xpos :ypos ypos
		    :fast-adc fast-adc :external-trigger external-trigger)
  (status))

(defun init-single-scan (&key (width 32) (height 32) (xpos 0) (ypos 0) (exposure-s 1s0)
			 (fast-adc t) (external-trigger nil))
  "Prepare camera, to capture single frames. Use start-acquisition to acquire a new frame."
  (unless *adc-calibrated* 
    (let* ((cams (val2 (get-available-cameras)))
	   (handle (val2 (get-camera-handle (1- cams)))))
      (check (set-current-camera handle)))
    (check (initialize "/usr/local/etc/andor"))
    (setf *adc-calibrated* t))
  (check (set-read-mode 4)) ;; 0 vertical bin, 1 multitrack, 2 randomtrack, 3 singletrack, 4 image 
  (check (set-acquisition-mode 1)) ;; 1 sglscan, 2 accum, 3 kinetics, 4 fast kin, 5 run-till-abort
  (check (set-exposure-time exposure-s))
  (check (set-ad-channel (if fast-adc 1 0)))
  (check (set-output-amplifier 0))
  (check (set-hs-speed 0 0))
  (check (set-trigger-mode (if external-trigger 6 0))) ;; 0 int, 1 ext, 6 ext start, 10 software
  
  (multiple-value-bind (e xdim ydim)
      (get-detector)
    (check e)
    (format t "~a~%" (list 'image xdim ydim))
    (let* ((xh (floor xdim 2))
	   (wh (floor width 2))
	   (xmin (- xh wh))
	   (xmax (+ xmin width))
	   (yh (floor ydim 2))
	   (hh (floor height 2))
	   (ymin (- yh hh))
	   (ymax (+ ymin height))
	   (hbin 1)
	   (vbin 1)
	   (hstart (+ xpos (1+ xmin)))
	   (hend (+ xpos xmax))
	   (vstart (+ ypos (1+ ymin)))
	   (vend (+ ypos ymax)))
      (check (set-image hbin vbin hstart hend vstart vend))
      (format t "set-image ~a~%" (list hbin vbin hstart hend vstart vend))))
  (setf *w* width
	*h* height)
  (check (prepare-acquisition))
  (multiple-value-bind (ret exp acc kin)
      (get-acquisition-timings)
    (check ret)
    (format t
	    "size: ~dx~d exposure time: ~fs~%accumulation time: ~fs~%kinetic cycle time: ~fs~%"
	    *w* *h* exp acc kin)))

#+nil
(init-single-scan :exposure-s .0163s0 :external-trigger t)
#+nil
(start-acquisition)
#+nil
(setf *im*
      (let* ((img (make-array (list *w* *h*) :element-type '(unsigned-byte 16)))
	     (img1 (sb-ext:array-storage-vector img)))
	(sb-sys:with-pinned-objects (img1)
	  (let ((ret (get-most-recent-image16 (sb-sys:vector-sap img1)
					      (* *w* *h*))))
	    (if (eq ret drv-no-new-data)
		(format t "no new data~%")
		(check ret))))
	img))

(defun init-run-till-abort (&key (width 32) (height 32) (xpos 0) (ypos 0) (exposure-s 1s0)
			    (fast-adc t) (external-trigger nil))
  (unless *adc-calibrated* 
    (let* ((cams (val2 (get-available-cameras)))
	   (handle (val2 (get-camera-handle (1- cams)))))
      (check (set-current-camera handle)))
    (check (initialize "/usr/local/etc/andor"))
    (setf *adc-calibrated* t))
  (check (set-read-mode 4)) ;; 0 vertical bin, 1 multitrack, 2 randomtrack, 3 singletrack, 4 image 
  (check (set-acquisition-mode 5)) ;; 1 sglscan, 2 accum, 3 kinetics, 4 fast kin, 5 run-till-abort
  (check (set-kinetic-cycle-time 0f0))
  
  (check (set-exposure-time exposure-s))
  (check (set-ad-channel (if fast-adc 1 0)))
   (check (set-output-amplifier 0))
   (check (set-hs-speed 0 0))
   (check (set-frame-transfer-mode 1))
   (check (set-trigger-mode (if external-trigger 1 0))) ;; 0 int, 1 ext, 10 software
   
   (multiple-value-bind (e xdim ydim)
       (get-detector)
     (check e)
     (format t "~a~%" (list 'image xdim ydim))
     (let* ((xh (floor xdim 2))
	    (wh (floor width 2))
	    (xmin (- xh wh))
	    (xmax (+ xmin width))
	    (yh (floor ydim 2))
	    (hh (floor height 2))
	    (ymin (- yh hh))
	    (ymax (+ ymin height)))
       (check (set-image 1 1 (+ xpos (1+ xmin)) (+ xpos xmax)
			 (+ ypos (1+ ymin)) (+ ypos ymax)))))
   (setf *w* width
	 *h* height)
   (multiple-value-bind (ret exp acc kin)
       (get-acquisition-timings)
     (check ret)
     (format t
	     "size: ~dx~d exposure time: ~fs   accumulation time: ~fs   kinetic cycle time: ~fs~%"
	     *w* *h* exp acc kin)))


(defun is-idle-p ()
  (multiple-value-bind (ret state)
      (get-status)
    (unless (eq drv-success ret)
      (error "can't get status, it returns ~a."
	     (lookup-error ret)))
    (eq state drv-idle)))
 

(defun init-fast (&key (exposure-s .016s0 exposure-s-p) 
		  (width 1392) (height 1040) (x 0) (y 0) (fast-adc t)
		  (external-trigger nil))
  (init-run-till-abort :exposure-s exposure-s 
		       :width width :height height
		       :external-trigger external-trigger
		       :xpos x :ypos y
		       :fast-adc fast-adc)
 
  (when exposure-s-p
    (unless (is-idle-p)
      (check (abort-acquisition)))
    (check (set-exposure-time exposure-s)))
  
  (when (is-idle-p)
    (check (start-acquisition)))
 
  (setf *displayed-images* 0)
  (setf *start-series* (val3 (get-acquisition-progress))))
 
(defun is-acquiring-p ()
  (multiple-value-bind (ret state)
      (get-status)
    (unless (eq drv-success ret)
      (error "can't get status, it returns ~a."
	     (lookup-error ret)))
    (eq state drv-acquiring)))

(defun snap-single-image ()
  (check (start-acquisition))
  (loop while (clara::is-acquiring-p) 
    ;; I decided to poll. WaitForAcquisition never returns 
    do (sleep .05s0))
  (setf *im*
	(let* ((img (make-array (list *h* *w*) :element-type '(unsigned-byte 16)))
	       (img1 (sb-ext:array-storage-vector img)))
	  (sb-sys:with-pinned-objects (img1)
	    (let ((ret (get-most-recent-image16 (sb-sys:vector-sap img1)
						(* *w* *h*))))
	      (if (eq ret drv-no-new-data)
		  (return-from snap-single-image *displayed-images*)
		  (check ret))))
	  img))
  (incf *displayed-images*))


(defun wait-for-image-and-copy ()
  (when (is-acquiring-p)
   (check (wait-for-acquisition)))
  (setf *im*
	(let* ((img (make-array (list *w* *h*) :element-type '(unsigned-byte 16)))
	       (img1 (sb-ext:array-storage-vector img)))
	  (sb-sys:with-pinned-objects (img1)
	    (let ((ret (get-most-recent-image16 (sb-sys:vector-sap img1)
						(* *w* *h*))))
	      (if (eq ret drv-no-new-data)
		  (return-from wait-for-image-and-copy *displayed-images*)
		  (check ret))))
	  img))
  (incf *displayed-images*)
  #+nil (check (free-internal-memory)))

(defun status ()
  (multiple-value-bind (e status)
      (get-status)
    (list (lookup-error e) (lookup-error status))))

(defun stop ()
  (check (abort-acquisition)))

(defun uninit ()
  (setf *adc-calibrated* nil
	*im* nil)
  (check (shutdown)))

;; use the following procedure to obtain images: call init-fast with
;; exposure and image size you want the first call will take a while
;; (8 seconds) later calls don't call initialize again when
;; *adc-calibrate*=t. then call status to see if the camera is idle or
;; acquiring. call wait-for-image-and-copy to get image data. it
;; either returns the oldest image or waits until an image has been
;; captured. the data is copied into *im*. to change exposure time or
;; image size call stop and call init-fast again. to close the camera
;; call uninit. init-fast will take again 8s to start it up.

#+nil
(time (init-fast :exposure-s .016s0 :width 320 :height 240))
#+nil 
(status)
#+nil
(progn
  (dotimes (i 10)
    (wait-for-image-and-copy))
  (stop))
#+nil
(uninit)
