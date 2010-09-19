#.(require :gui)
#.(require :clara)
#.(require :mma)
#.(require :focus)
;;; CLARA CAMERA
#+nil
(progn
  (clara:init-fast :exposure-s 0.016s0 :width 256 :height 256 
		   :x -50 :y 80 :fast-adc t :external-trigger t)
  (clara:wait-for-image-and-copy)
  (clara:status))
#+nil
(clara:stop)
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
#+nil
(progn
  (mma::end)
  #+nil (mma:load-white :radius .1 :pic-number 8)
  #+nil (mma:load-concentric-circles :n 12)
  (mma:load-disks2 :n 7)
  (mma:begin))
#+nil
(mma:uninit)
#+nil
(let ((x 3)
      (y 1)
      (n 7))
 (mma:select-pictures (+ x (* n y)) :ready-out-needed t))

;;; FOCUS STAGE OVER SERIAL (look for pl2303 converter in dmesg)
#+nil
(focus:connect)
#+nil
(focus:get-position)
#+nil ;; move away from sample
(focus:set-position (1- (focus:get-position))) 
#+nil ;; move further into sample
(focus:set-position (+ 1 (focus:get-position)))
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

(loop for i below 7 collect
     (list i (mod (+ i (floor 3 2) ) 3)))

;;; DRAW INTO OPENGL WINDOW (for LCOS and camera view)
(let* ((white-width 2)
       (phases 12)
       (colors 3) 
       (a (make-array (* colors phases white-width) :element-type '(unsigned-byte 8)))
       (phase 0)
       
       (im-scale 600s0)
       (im-offset .09s0))

  (defun change-phase (p)
    (setf phase p
	  a (make-array (* colors phases white-width) :element-type '(unsigned-byte 8)))
    (let ((offset (* phase white-width colors)))
     (dotimes (i white-width)
       (setf (aref a (+ offset (+ 0 (* colors i)))) #b01010100 ;; disable first bit plane
	     (aref a (+ offset (+ 1 (* colors i)))) #b01010101 ;; show only every other bit plane
	     (aref a (+ offset (+ 2 (* colors i)))) #b01010101)))) 

  (change-phase 0)

  (defun obtain-sectioned-slice (&key (accumulate 1))
    (when clara:*im*
      (destructuring-bind (w h) (array-dimensions clara:*im*)
	(let ((phase-im (make-array (list phases w h) :element-type '(signed-byte 64)))
	      (widefield-im (make-array (list w h) :element-type '(signed-byte 64))))
	  (setf *section-im* (make-array (list w h) :element-type '(unsigned-byte 16))
		*widefield-im* (make-array (list w h) :element-type '(unsigned-byte 16)))
	  ;; capture the phase images, accumulate if requested, update widefield image as well
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
		    (setf (aref *widefield-im* i j) (clamp-u16 (floor (aref widefield-im i j) 2))))))))
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
	 (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))))
    ;; draw sectioned image next to it
    (gl:with-pushed-matrix
      (gl:translate 256 0 0)
      (when *section-im*
	(let ((tex (make-instance 'gui::texture :data *section-im*
				  :scale 100s0 :offset 0.0s0)))
	  (destructuring-bind (w h) (array-dimensions *section-im*)
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))))
      ;; draw accumulated widefield image below
      (gl:translate 0 256 0)
      (when *widefield-im*
	(let ((tex (make-instance 'gui::texture :data *widefield-im*
				  :scale 1s0 :offset 0.0s0
				  )))
	  (destructuring-bind (w h) (array-dimensions *widefield-im*)
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h))))))
    ;; draw an image with only out of focus light in the lower left
    (gl:with-pushed-matrix
      (gl:translate 0 256 0)
      (when *unfocused-im*
	(let ((tex (make-instance 'gui::texture :data *unfocused-im*
				  :scale 1s0 :offset 0.0s0)))
	  (destructuring-bind (w h) (array-dimensions *section-im*)
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h))))))
    ;; draw grating for sectioning on the very right
    (gl:translate 800 0 0)
    (let ((repetition 100f0))
      (gui::with-grating (g a)
	(gui:draw g :w (* repetition white-width phases) :h 300.0 :wt repetition)))
    
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
(sb-thread:make-thread #'(lambda () (obtain-sectioned-slice :accumulate 100)))

;; scan through different mma positions and capture slices
(defparameter *mma-scan*
  (let ((n 7)
	(result nil))
    (dotimes (y n)
      (dotimes (x n)
	(mma:select-pictures (+ x (* n y)) :ready-out-needed t)
	(sb-thread:join-thread 
	 (sb-thread:make-thread 
	  #'(lambda () (obtain-sectioned-slice :accumulate 100))))
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
(progn
 (save-bfp-mosaic "/home/martin/tmp/mosaic7" wide)
 (save-bfp-mosaic "/home/martin/tmp/mosaic7" section)
 (save-bfp-mosaic "/home/martin/tmp/mosaic7" unfocus))

#+nil
(gui:with-gui (1400 512)
  (draw-screen))
