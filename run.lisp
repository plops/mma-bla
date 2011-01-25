#.(require :gui) ;; make sure DISPLAY is exported
#.(require :clara)
#.(require :mma)
#.(require :focus)

#+nil ;; fill two screens
(sb-thread:make-thread 
 #'(lambda ()
     (gui:with-gui ((* 1280 2) 1024 0 0)
       (draw-screen)))
 :name "display")

;;; Clara CAMERA
#+nil
(clara:init :exposure-s .0163s0 
	    :fast-adc nil
	    :external-trigger t
	    :xpos -290 :ypos 100)
;; continuously capture new images
(defparameter *capture-thread* nil)
(defun start-capture-thread ()
  (unless *capture-thread*
   (setf *capture-thread*
	 (sb-thread:make-thread
	  #'(lambda ()
	      (loop
		 (clara:snap-single-image)))
	  :name "capture"))))

(defun stop-capture-thread ()
  (let ((is-running *capture-thread*))
   (when is-running
     (sb-thread:terminate-thread *capture-thread*)
     (setf *capture-thread* nil))
   is-running))
#+nil
(start-capture-thread)
#+nil
(stop-capture-thread)
#+nil
(clara:stop)
#+nil
(clara:status)
#+nil
(clara:snap-single-image)
#+nil
(clara:uninit)
#+nil
clara:*im*

;;; MMA OVER NETWORK (run sudo ifconfig eth1 192.168.0.1)
;; oscilloscope luvcview -d /dev/video1 -s 320x240 -i 5
;; mma image    luvcview -d /dev/video0 -s 320x240 -i 5
;; /usr/local/bin/uvcdynctrl -v -s "Exposure (Absolute)" 10000

#+nil
(mma:init)
#+nil
(mma:status)
#+nil
(mma::set-start-mma)
#+nil
(mma::set-stop-mma)
#+nil
(progn
  (mma::set-stop-mma)
  (mma::set-extern-trigger t)
  (mma::set-deflection-phase 16s0 16300s0)
  (mma:begin))
(defvar *mma-contents* nil)
(defvar *mma-select* 0)
#+nil
(mma::select-pictures 1 :n 1 :ready-out-needed t)

#+nil
(mma::draw-ring-cal :pic-number 1)
#+nil
(time
 (progn
   (mma::end)
   #+nil (dotimes (i 4090)
	   (mma::draw-random-cal :pic-number (1+ i)))
   #+nil (mma:load-white :radius 1.0 :pic-number 1)
   #+nil (mma:load-concentric-circles :n 12)
   #+nil (setf *mma-contents* (mma::load-concentric-disks :n 12))
   #+nil (setf *mma-contents* (mma::load-concentric-circles :dr .1 :n 12))
   (setf *mma-contents* (mma:load-disks2 :n 5))
   (append *mma-contents* (list (mma::draw-disk-cal :pic-number 26)
				(mma::draw-disk-cal :pic-number 27 :value 0)))
   #+nil(mma::draw-grating)
   (mma:begin)))
#+nil
(mma:uninit)
#+nil
(mma::reset)

;;; FOCUS STAGE OVER SERIAL (look for pl2303 converter in dmesg)
#+nil
(focus:connect)
#+nil
(focus:get-position)
#+nil ;; move away from sample
(focus:set-position (- (focus:get-position) .25))
#+nil ;; move further into sample
(focus:set-position (+ .25 (focus:get-position)))
#+nil
(focus:disconnect)

(defun clamp-u16 (a)
  (declare (values (unsigned-byte 16) &optional))
  (let ((ma #.(1- (expt 2 16))))
   (cond ((< a 0) 0)
	 ((< ma a) ma)
	 (t a))))

(defvar *section-im* nil)
(defvar *widefield-im* nil)
(defvar *unfocused-im* nil)
(defvar *dark-im* nil)
(defvar *stack* nil)

;; OBTAIN
#+nil
(obtain-sectioned-slice :accumulate 1)


;;; DRAW INTO OPENGL WINDOW (for LCOS and camera view)
(let* ((white-width 2)
       (phases-x 12)
       (phases-y 1)
       (a (make-array (* phases-x phases-y white-width) 
		      :element-type '(unsigned-byte 8)))
       (phase 0)
       
       ;(im-scale 100s0) (im-offset 1.86s0)
       (im-scale 4000s0) (im-offset 0.0s0)
       )

  (defun change-phase (p)
    (setf phase p
	  a (gui:grating->texture (gui:grating-stack phases-y phases-x)
				  p :h white-width :w white-width))) 

  (change-phase 0)

  (defun draw-disk-fan (&key (radius 1.0) (n 17))
    (gl:with-pushed-matrix
      (gl:scale radius radius radius)
      (gl:with-primitive :triangle-fan
	(gl:vertex 0.0 0.0)
	(dotimes (i n)
	  (let ((phi (* i (/ (* 2.0 (coerce pi 'single-float)) n))))
	    (gl:vertex (cos phi) (sin phi))))
	(gl:vertex 1.0 0.0))))

  (defun obtain-sectioned-slice (&key (accumulate 1))
    (let ((capture-was-running (stop-capture-thread)))
      (destructuring-bind (w h) (array-dimensions clara:*im*)
	(let ((phase-im (make-array (list phases-y phases-x w h) 
				    :element-type '(signed-byte 64)))
	      (widefield-im (make-array (list w h) 
					:element-type '(signed-byte 64))))
	  (setf *section-im* (make-array (list w h) 
					 :element-type '(unsigned-byte 16))
		*widefield-im* (make-array (list w h) 
					   :element-type '(unsigned-byte 16)))
	  ;; capture the phase images, accumulate if requested, update
	  ;; widefield image as well
	 (dotimes (py phases-y)
	   (dotimes (px phases-x)
	     (change-phase (+ px (* phases-x py)))
	     (format t "~a~%" (list 'px px 'py py))
	     (sleep 1/60)
	     (clara:snap-single-image)
	     (dotimes (a accumulate)
	       (dotimes (j h)
		 (dotimes (i w)
		   (let ((v (aref clara:*im* i j)))
		     (incf (aref phase-im py px i j) v)
		     (incf (aref widefield-im i j) v)
		     (setf (aref *widefield-im* i j)
			   (clamp-u16 (floor (aref widefield-im i j) 2)))))))))
	 ;; final widefield image normalize to full 16bit range
	 (let ((ma 0)
	       (mi (1- (expt 2 64))))
	   (dotimes (j h)
	     (dotimes (i w)
	       (let ((v (aref widefield-im i j)))
		 (setf ma (max v ma)
		       mi (min v mi)))))
	   (dotimes (j h)
	     (dotimes (i w)
	       (setf (aref *widefield-im* i j) (floor (- (aref widefield-im i j) mi)
						      (/ (- ma mi) (1- (expt 2 16)))))))
	   (format t "values in accumulated widefield image: ~a~%" (list mi ma))
	   ;; min-max reconstruction
	   (dotimes (j h)
	     (dotimes (i w)
	       (let* ((v (aref phase-im 0 0 i j))
		      (ma v)
		      (mi v))
		 (dotimes (py phases-y)
		   (dotimes (px phases-x)
		     (let ((v (aref phase-im py px i j)))
		       (setf mi (min mi v)
			     ma (max ma v)))))
		 (setf (aref *section-im* i j) (clamp-u16 (- ma mi))))))

	   ;; subtract section image from accumulated widefield image
	   (setf *unfocused-im* (make-array (list w h) :element-type '(unsigned-byte 16)))
	   (dotimes (j h)
	     (dotimes (i w)
	       (setf (aref *unfocused-im* i j)
		     (clamp-u16 (- (aref *widefield-im* i j) 
				   (floor (aref *section-im* i j) 
					  ;; uses max and min from widefield
					  (/ (- ma mi) (* 20 (1- (expt 2 16))))))))))))
	(format t "maximum value in section ~a~%"
		(reduce #'max (sb-ext:array-storage-vector *section-im*))))
      (when capture-was-running
	(start-capture-thread))))
  
  (defun obtain-stack (&key (n 30) (dz .1s0) (accumulate 1))
    (let ((center (focus:get-position))
	  (result nil))
      (dotimes (k n)
	(let ((z (+ center (* dz (- k (floor n 2))))))
	  (format t "slice ~f ~d/~d~%" z (1+ k) n)
	  (focus:set-position z)
	  (sleep .1)
	  (obtain-sectioned-slice :accumulate accumulate)
	  (push (list z *section-im*) result)))
      (setf *stack* (reverse result))))

  (defun draw-screen ()
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer-bit)
    ;; draw raw camera image on the left
    (gl:with-pushed-matrix
      (gl:scale .5 .5 1s0)
      (when clara:*im*
	(let ((tex (make-instance 'gui::texture :data clara:*im* 
				  :scale im-scale :offset im-offset)))
	  (destructuring-bind (w h) (array-dimensions clara:*im*)
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	  (gui:destroy tex)))
      ;; draw SECTIONed image next to it
      (gl:with-pushed-matrix
	(when *section-im*
	  (let ((tex (make-instance 'gui::texture :data *section-im*
				    :scale 4000s0 :offset 0.03s0)))
	    (destructuring-bind (w h) (array-dimensions *section-im*)
	      (gl:translate w 0 0)
	      (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	    (gui:destroy tex)))
	;; draw accumulated widefield image below
	(when *widefield-im*
	  (let ((tex (make-instance 'gui::texture :data *widefield-im*
				    :scale 5s0 :offset 0.0s0
				    )))
	    (destructuring-bind (w h) (array-dimensions *widefield-im*)
	      (gl:translate 0 h 0)
	      (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	    (gui:destroy tex))))
      ;; draw an image with only out of focus light in the lower left
      (gl:with-pushed-matrix
	(when *unfocused-im*
	  (let ((tex (make-instance 'gui::texture :data *unfocused-im*
				    :scale 10s0 :offset 0.0s0)))
	    (destructuring-bind (w h) (array-dimensions *section-im*)
	      (gl:translate 0 h 0)
	      (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	    (gui:destroy tex))))
      ;; draw the image that is displayed on the mma
      #+nil (when *mma-contents*
	      (gl:with-pushed-matrix
		(let ((p (elt *mma-contents* *mma-select*))) 
		  (let ((tex (make-instance 'gui::texture :data p
					    :scale 1s0 :offset 0.0s0)))
		    (destructuring-bind (w h) (array-dimensions p)
		      (gl:translate 0 (* 2 h) 0)
		      (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
		    (gui:destroy tex)))))
      )    
    ;; draw grating for sectioning on the very right
    (gl:translate (+ 280 400) 0 0)
    (unless *dark-im*
     (let ((repetition 900f0))
       (gui::with-grating (g a)
	 (gui:draw g 
		   :w (* repetition white-width phases-x)
		   :h (* repetition white-width phases-y)
		   :wt repetition
		   :ht repetition))))

    ;; FAN
    ;#+nil
    (gl:with-pushed-matrix
      (gl:color 1 1 1)
      (gl:translate (+ 1000 400 -175.0) 535.0 0.0)
      (draw-disk-fan :radius 20.0))

    #+nil
    (gl:with-primitive :lines
      (gl:color 1 0 0) (gl:vertex 0 0) (gl:vertex (* white-width phases 3) 0 0) ;; x axis red
      ;; show ruling with a pixel spacing of white-width
      (dotimes (i (1+ (* phases 3)))
	(let ((x (* white-width i))
	      (y (if (/= 0 (mod i phases)) -3 -5)))
	  (gl:vertex x 0)
	  (gl:vertex x y)))
      (gl:color 0 1 0) (gl:vertex 0 1) (gl:vertex 0 100 0) ;; y axis green
      (gl:color 0 0 1) (gl:vertex 0 0 1) (gl:vertex 0 0 100))))

(defun select-disk (q)
  (setf *mma-select* q)
  (mma:select-pictures q :n 1 :ready-out-needed t))

#+nil
(select-disk 24)

#+nil
(sb-thread:make-thread 
 #'(lambda ()
     (loop
	(dotimes (i 26)
	(select-disk i)
	(sleep .5))))
 :name "switch-angle")

(defun average-img (img)
  (let ((sum 0))
   (destructuring-bind (w h) (array-dimensions img)
     (dotimes (j h)
       (dotimes (i w)
	 (incf sum (aref img i j))))
     (/ sum (* 1s0 w h)))))

(defun integrate-slices (stack)
  (loop for s in stack collect
    (destructuring-bind (z slice) s
      (list z
	    (average-img slice)))))

(defparameter *stack-integral* nil)


(defun write-pgm (filename img)
  (declare (simple-string filename)
           ((array (unsigned-byte 8) 2) img)
           (values null &optional))
  (destructuring-bind (h w)
      (array-dimensions img)
    (declare ((integer 0 65535) w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%255~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :append)
      (write-sequence (sb-ext:array-storage-vector img) s))
    nil))

(defun write-pgm16 (filename img)
  (declare (simple-string filename)
           ((array (unsigned-byte 16) 2) img)
           (values null &optional))
  (destructuring-bind (h w) (array-dimensions img)
    (declare ((integer 0 65535) w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%65535~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 16)
                       :direction :output
                       :if-exists :append)
      (write-sequence (sb-ext:array-storage-vector img) s))
    nil))

;; save the bfp scan as a mosaic
(defmacro save-bfp-mosaic (filename image)
  `(destructuring-bind (w h) (array-dimensions (third (elt *mma-scan* 0)))
    (let* ((n 7) 
	  (mosaic (make-array (list (* w n) (* h n)) :element-type '(unsigned-byte 8))))
      (dolist (e *mma-scan*)
	(destructuring-bind (ii jj wide section unfocus) e
	  (let ((ma (reduce #'max (sb-ext:array-storage-vector wide)))
		(mi (reduce #'min (sb-ext:array-storage-vector wide))))
	    (dotimes (j h)
	      (dotimes (i w)
		(setf (aref mosaic (+ (* ii w) i) (+ (* jj h) j))
		      (floor (- (aref ,image i j) mi)
			     (/ (- ma mi) 255))))))))
     (write-pgm (format nil "~a-~a.pgm" ,filename ',image) mosaic))))
#+nil
(progn
 (save-bfp-mosaic "/home/martin/tmp/mosaic7" wide)
 (save-bfp-mosaic "/home/martin/tmp/mosaic7" section)
 (save-bfp-mosaic "/home/martin/tmp/mosaic7" unfocus))

