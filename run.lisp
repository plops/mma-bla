#.(require :gui) ;; make sure DISPLAY is exported
#.(require :clara)
#.(require :mma)
#.(require :focus)
;;; Clara CAMERA
#+nil
(progn
  (when clara::*adc-calibrated*
   (when (clara::is-acquiring-p)
     (clara:stop)))
  (clara:init-fast :exposure-s .0163s0 :width 256 :height 256 
		   :x -38 :y 26 :fast-adc t :external-trigger t)
  (clara:wait-for-image-and-copy)
  (clara:status)
  (format t "initialized~%"))
#+nil
(clara:stop)
#+nil
(clara:start-acquisition)
#+nil
(clara:status)
#+nil
(clara:uninit)
#+nil
clara:*im*


;;; MMA OVER NETWORK (run sudo ifconfig eth1 192.168.0.1)
;; oscilloscope luvcview -d /dev/video1 -s 320x240 -i 5
;; mma image    luvcview -d /dev/video0 -s 320x240 -i 5
#+nil
(mma:init)
#+nil
(mma:status)
#+nil
(mma::set-start-mma)
#+nil
(progn
  (mma::set-stop-mma)
  (mma::set-extern-trigger t)
  (mma:begin))
(defvar *mma-contents* nil)
(defvar *mma-select* 0)
#+nil
(mma::select-pictures 15 :n 1 :ready-out-needed t)
#+nil
(progn
  (mma::end)
   (mma:load-white :radius 1.0 :pic-number 1)
  #+nil (mma:load-concentric-circles :n 12)
  #+nil (setf *mma-contents* (mma::load-concentric-disks :n 12))
  #+nil (setf *mma-contents* (mma::load-concentric-circles :dr .1 :n 12))
  #+nil (setf *mma-contents* (mma:load-disks2 :n 4))
  #+nil(mma::draw-grating)
  (mma:begin))
#+nil
(mma:uninit)


;;; FOCUS STAGE OVER SERIAL (look for pl2303 converter in dmesg)
;; dmesg|grep pl2303|grep ttyUSB|tail -n1|sed s+.*ttyUSB+/dev/ttyUSB+g
(defun run-shell (command)
  (with-output-to-string (stream)
    (sb-ext:run-program "/bin/bash" (list "-c" command)
			:input nil
			:output stream)))

(defun find-zeiss-usb-adapter ()
  (let ((port (run-shell "dmesg|grep pl2303|grep ttyUSB|tail -n1|sed s+.*ttyUSB+/dev/ttyUSB+g|tr -d '\\n'")))
    (if (string-equal "" port)
	(error "dmesg output doesn't contain ttyUSB assignment. This can happen when the system ran a long time. You could reattach the USB adapter that is connected to the microscope.")
	port)))



#+nil
(focus:connect (find-zeiss-usb-adapter))
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
(defvar *dark-im* t)
(defvar *stack* nil)

;;; DRAW INTO OPENGL WINDOW (for LCOS and camera view)
(let* ((white-width 4)
       (phases 12)
       (colors 3) 
       (a (make-array (* colors phases white-width) :element-type '(unsigned-byte 8)))
       (phase 0)
       
       ;(im-scale 40s0) (im-offset 0.56s0)
       (im-scale 20s0) (im-offset 0.0s0)
       )

  (defun change-phase (p)
    (setf phase p
	  a (make-array (* colors phases white-width) :element-type '(unsigned-byte 8)))
    (let ((offset (* phase white-width colors)))
     (dotimes (i white-width)
       (setf (aref a (+ offset (+ 0 (* colors i)))) #b01010100 ;; disable first bit plane
	     (aref a (+ offset (+ 1 (* colors i)))) #b01010101 ;; show only every other bit plane
	     (aref a (+ offset (+ 2 (* colors i)))) #b01010101)))) 

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
    (when clara:*im*
      (destructuring-bind (w h) (array-dimensions clara:*im*)
	(let ((phase-im (make-array (list phases w h) :element-type '(signed-byte 64)))
	      (widefield-im (make-array (list w h) :element-type '(signed-byte 64))))
	  (setf *section-im* (make-array (list w h) :element-type '(unsigned-byte 16))
		*widefield-im* (make-array (list w h) :element-type '(unsigned-byte 16)))
	  ;; capture the phase images, accumulate if requested, update
	  ;; widefield image as well
	  (dotimes (p phases)
	    (change-phase p)
	    (sleep .1)
	    (clara:wait-for-image-and-copy)
	    (dotimes (a accumulate)
	      (dotimes (j h)
		(dotimes (i w)
		  (let ((v (aref clara:*im* i j)))
		    (incf (aref phase-im p i j) v)
		    (incf (aref widefield-im i j) v)
		    (setf (aref *widefield-im* i j)
			  (clamp-u16 (floor (aref widefield-im i j) 2))))))))
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
		(let* ((v (aref phase-im 0 i j))
		       (ma v)
		       (mi v))
		  (loop for p from 1 below phases do
		       (let ((v (aref phase-im p i j)))
			 (setf mi (min mi v)
			       ma (max ma v))))
		  (setf (aref *section-im* i j) (clamp-u16 (- ma mi))))))
	  ;; sqrt reconstruction
	    #+nil
	    (let ((square-im (make-array (list w h) :element-type '(signed-byte 64))))
	      (dotimes (j h)
		(dotimes (i w)
		  (dotimes (p phases)
		    (let ((q (- (aref phase-im phase i j) 
				(aref phase-im (mod (+ p (floor phases 2)) phases) i j))))
		      (incf (aref square-im i j) 
			    (* q q))))
		  (setf (aref *section-im* i j) (floor (sqrt (aref square-im i j)))))))
	    ;; subtract section image from accumulated widefield image
	    (setf *unfocused-im* (make-array (list w h) :element-type '(unsigned-byte 16)))
	    (dotimes (j h)
	      (dotimes (i w)
		(setf (aref *unfocused-im* i j)
		      (clamp-u16 (- (aref *widefield-im* i j) 
				    (floor (aref *section-im* i j) ;; uses max and min from widefield
					   (/ (- ma mi) (* 20 (1- (expt 2 16)))))))))))))
      (format t "maximum value in section ~a~%"
	      (reduce #'max (sb-ext:array-storage-vector *section-im*)))))
  
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
    (when clara:*im*
      (clara:wait-for-image-and-copy)
      (let ((tex (make-instance 'gui::texture :data clara:*im* 
				:scale im-scale :offset im-offset
			;;	:scale 40s0 :offset 0.76s0
				)))
       (destructuring-bind (w h) (array-dimensions clara:*im*)
	 (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
       (gui:destroy tex)))
    ;; draw SECTIONed image next to it
    (gl:with-pushed-matrix
      (when *section-im*
	(let ((tex (make-instance 'gui::texture :data *section-im*
				  :scale 80s0 :offset 0.0s0)))
	  (destructuring-bind (w h) (array-dimensions *section-im*)
	    (gl:translate w 0 0)
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	  (gui:destroy tex)))
      ;; draw accumulated widefield image below
      (when *widefield-im*
	(let ((tex (make-instance 'gui::texture :data *widefield-im*
				  :scale 1s0 :offset 0.0s0
				  )))
	  (destructuring-bind (w h) (array-dimensions *widefield-im*)
	    (gl:translate 0 h 0)
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	  (gui:destroy tex))))
    ;; draw an image with only out of focus light in the lower left
    (gl:with-pushed-matrix
      (when *unfocused-im*
	(let ((tex (make-instance 'gui::texture :data *unfocused-im*
				  :scale 1s0 :offset 0.0s0)))
	  (destructuring-bind (w h) (array-dimensions *section-im*)
	    (gl:translate 0 h 0)
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	  (gui:destroy tex))))
    ;; draw the image that is displayed on the mma
    (when *mma-contents*
      (gl:with-pushed-matrix
	(let ((p (elt *mma-contents* *mma-select*))) 
	  (let ((tex (make-instance 'gui::texture :data p
				    :scale 1s0 :offset 0.0s0)))
	    (destructuring-bind (w h) (array-dimensions p)
	      (gl:translate 0 (* 2 h) 0)
	      (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	    (gui:destroy tex)))))
    
    ;; draw grating for sectioning on the very right
    (gl:translate 400 0 0)
    (unless *dark-im*
     (let ((repetition 100f0))
       (gui::with-grating (g a)
	 (gui:draw g :w (* repetition white-width phases) :h 900.0 :wt repetition))))

    #+nil
    (gl:with-pushed-matrix
      (let ((r (/ #xff 255.0))
	    (g (/ #xff 255.0))
	    (b (/ #xff 255.0)))
       (gl:color r g b))
      (gl:translate (+ 400 -150.0) 530.0 0.0)
     (draw-disk-fan :radius 4.0))

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
#+nil
(progn
  (setf *dark-im* nil)
  (sb-thread:join-thread 
   (sb-thread:make-thread #'(lambda () (obtain-sectioned-slice :accumulate 12))))
  (setf *dark-im* t))

(defun select-disk (q)
  (setf *mma-select* q)
  (mma:select-pictures q :n 1 :ready-out-needed t))

#+nil
(setf *dark-im* nil)
#+nil
(setf *dark-im* t)

#+nil
(select-disk 7)

#+nil
(sb-thread:make-thread #'(lambda () (obtain-stack :dz .3 :accumulate 12)))

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


(defparameter *stack05* *stack*)
(defparameter *stack11* *stack*)
(defparameter *stack11-4* *stack*)
(defparameter *stack11-6* *stack*)
(defparameter *stack11-10* *stack*)
(defparameter *stack11-10-disk7* *stack*)

#+nil
(setf *stack-integral*
 (integrate-slices *stack*))
#+nil
(with-open-file (s "/home/martin/tmp/11_5.dat" :if-does-not-exist :create
		   :if-exists :supersede :direction :output)
  (dolist (e *stack-integral*)
   (format s "~f ~f~%" (first e) (second e))))

#+nil
(let* ((x 1)
       (y 2)
       (n 5)
       (q (+ x (* n y))))
  (setf *mma-select* q)
  (mma:select-pictures q :n 1 :ready-out-needed t))



;; scan through different mma positions and capture slices
#+nil
(defparameter *mma-scan*
  (let ((n 5)
	(result nil))
    (dotimes (y n)
      (dotimes (x n)
	(let ((q (+ x (* n y))))
	 (setf *mma-select* q)
	 (mma:select-pictures q :ready-out-needed t))
	(sb-thread:join-thread 
	 (sb-thread:make-thread 
	  #'(lambda () (obtain-sectioned-slice :accumulate 1))))
	(push (list x y *widefield-im* *section-im* *unfocused-im*) result)))
    result))

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

;; gui window on normal screen
#+nil
(sb-thread:make-thread 
 #'(lambda ()
     (gui:with-gui (1280 1024 0 0)
       (draw-screen))))


;; gui window on right screen (lcos)
#+nil
(sb-thread:make-thread 
 #'(lambda ()
     (gui:with-gui (1400 (+ 256 512) (- 1280 512) 100)
       (draw-screen))))
