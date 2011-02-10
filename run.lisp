#.(require :gui) ;; make sure DISPLAY is exported
#.(require :clara)
#.(require :mma)
#.(require :focus)
(push "../../0131/rayt/" asdf:*central-registry*)
;#.(require :rayt)



(deftype num () `single-float)
(deftype vec () `(simple-array num (3)))
(deftype mat () `(simple-array num (3 3)))

(declaim (inline vx vy vz))
(defun vx (v) (declare (type vec v)) (the num (aref v 0)))
(defun vy (v) (declare (type vec v)) (the num (aref v 1)))
(defun vz (v) (declare (type vec v)) (the num (aref v 2)))

(defun v (&optional (x 0s0) (y 0s0) (z 0s0))
  (the vec (make-array 3 :element-type 'num :initial-contents (list (float x) 
								    (float y)
								    (float z)))))

#+nil
(vx (v 1 2 34))

(defun m (a b c d e f g h i)
  (declare (type num a b c d e f g h i))
  (the mat 
    (make-array '(3 3)
		:element-type 'num
		:initial-contents (list (list a b c)
					(list d e f)
					(list g h i)))))

(defun m* (matrix vect)
  "Multiply MATRIX with VECT."
  (declare (type mat matrix) (type vec vect))
  (let ((res (v)))
    (dotimes (i 3)
      (dotimes (j 3)
        (incf (aref res i)
              (* (aref matrix i j) (aref vect j)))))
    res))


(defun .s (s a)
  "scalar multiplication"
  (declare (type num s)
	   (type vec a))
  (let ((r (v)))
    (dotimes (i (length a))
      (setf (aref r i) (* s (aref a i))))
    r))

(defun cam->lcos (cam)
  (declare (type vec cam))
  (let* ((slcos (m* (m -1.0437 0.02797 1339.98
		       .001000 1.05462 -6.178
		       2.64e-6 2.48e-5 .986)
		    cam)))
    (the vec (.s (/ (vz slcos)) slcos))))

#+nil
(cam->lcos (v 0 0 1))


#+nil ;; FILL two screens
(sb-thread:make-thread 
 #'(lambda ()
     (sb-ext:run-program "/usr/bin/xset"
			 '("-dpms" "s" "off"))
     (gui:with-gui ((+ 1280 1366) 1024 -1 -1)
       (draw-screen)))
 :name "display")



(defparameter *exposure-time-s* .0163s0)
;;; Clara CAMERA
#+nil
(progn
  (setf *exposure-time-s* (* .0163s0 1))
 (clara:init :exposure-s  *exposure-time-s*
	     :fast-adc t
	     :external-trigger t
	     ;:xpos -290 :ypos 100
	     ;:width 128 :height 128
	     :width 1392 :height 1040
	     ))
;; continuously capture new images
(defparameter *capture-thread* nil)
(defun start-capture-thread (&optional (delay nil))
  (unless *capture-thread*
   (setf *capture-thread*
	 (sb-thread:make-thread
	  #'(lambda ()
	      (loop
		 (when delay
		  (sleep delay))
		 (clara:snap-single-image)))
	  :name "capture"))))

(defun stop-capture-thread ()
  (let ((is-running *capture-thread*))
   (when is-running
     (sb-thread:terminate-thread *capture-thread*)
     (setf *capture-thread* nil))
   is-running))
#+nil
(start-capture-thread 0s0)
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
(progn
 (mma:init)
 (mma:set-nominal-deflection-nm 118.25))
#+nil
(mma:set-nominal-deflection-nm 118.25)
#+nil
(mma:status)
#+nil
(mma::set-power-off)
#+nil
(mma:get-nominal-deflection-nm)
#+nil
(mma::set-start-mma)
#+nil
(mma::set-stop-mma)
#+nil
(progn
  (mma::set-stop-mma)
  (mma::set-extern-trigger t)
  (let* ((e-ms (* 1000 *exposure-time-s*))
	(e-us (* 1000 e-ms)))
    (mma::set-deflection-phase 0s0 e-us)
    (mma::set-cycle-time (* 2 e-ms)))
  (mma:begin))
#+nil
(mma::get-cycle-time)
#+nil

(defvar *mma-contents* nil)
(defvar *mma-select* 0)

(defparameter *mma-dark* 26)
(defparameter *mma-bright* 25)
#+nil
(mma::select-pictures 0 :n 9 :ready-out-needed t)
#+nil
(select-disk *mma-bright*)
#+nil
(dotimes (j 12)
 (let ((n 30)
       (dr/2 .025s0))
   (dotimes (i n) 
     (let ((r (+ dr/2 (* (- 1s0 (* 2 dr/2))
			 (/ (* 1s0 i) n)))))
       (mma::draw-disk-cal :r-small (- r dr/2) :r-big (+ r dr/2) :pic-number 5))
     #+nil (sleep .1))))

#+nil ;; draw some arbitrary data
(let* ((n 256)
       (m (make-array (list n n) :element-type '(unsigned-byte 12))))
  (dotimes (j n)
    (dotimes (i n)
      (setf (aref m j i) (* 4095 (mod j 2)))))
  (mma:draw-array-cal m :pic-number 5)
  nil)
#+nil
(select-disk 4)

(defvar *mma-contents* nil)
#+nil
(time
 (progn
   (mma::end)
   #+nil (dotimes (i 4090)
	   (mma::draw-random-cal :pic-number (1+ i)))
   #+nil (mma:load-white :radius 1.0 :pic-number 1)
   #+nil (mma:load-concentric-circles :n 12)
   #+nil (setf *mma-contents* (mma::load-concentric-disks :n 12))
   #+nil (setf *mma-contents* (mma::draw-ring-cal :r-small .3 :r-big .8))
   #+nil (setf *mma-contents* (mma::load-concentric-circles :dr .1 :n 12))
   (let ((n 3))
     (setf *mma-contents* (mma:load-disks2 :n n)
	   *mma-bright* (* n n)
	   *mma-dark* (+ (* n n) 1))
     (append *mma-contents* 
	     (list (mma::draw-disk-cal :pic-number (1+ *mma-bright*))
		   (mma::draw-disk-cal :pic-number (1+ *mma-dark*) 
				       :value 0))))
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


(defvar *section-im* nil)
(defvar *widefield-im* nil)
(defvar *unfocused-im* nil)
(defvar *dark-im* nil)
(defvar *bright-im* nil)
(defvar *stack* nil)

(defparameter *data-dir* "/home/martin/d0210/")

(defmacro dir (&rest rest)
  `(concatenate 'string *data-dir* ,@rest))

(defun  obtain-image (&optional (mma-image *mma-bright*))
  (select-disk mma-image)
  (setf *dark-im* nil)
  (setf *bright-im* t)
  (sleep .1)
  (clara:snap-single-image)
  (write-pgm16 (dir (format nil "snap~3,'0d.pgm" mma-image)) 
	       clara:*im*)
  (setf *bright-im* nil
	*dark-im* nil)
  (select-disk *mma-bright*))

(defun clamp-u16 (a)
  (declare (values (unsigned-byte 16) &optional))
  (let ((ma #.(1- (expt 2 16))))
   (cond ((< a 0) 0)
	 ((< ma a) ma)
	 (t a))))

;; capture image with angular illumination
#+nil
(progn
  (setf *bright-im* t
	*dark-im* nil)
  (sleep .1)
  (dotimes (i *mma-dark*)
   (obtain-image i)
   (sleep 1))
 (setf *bright-im* nil
       *dark-im* t)
 (obtain-image *mma-dark*))

;; store MMA images
#+nil
(let ((i 0))
 (dolist (e *mma-contents*)
   (let ((r (make-array (butlast (array-dimensions e))
			:element-type '(unsigned-byte 8))))
     (destructuring-bind (y x) (array-dimensions r)
       (dotimes  (j y)
	 (dotimes (i x)
	   (setf (aref r j i) (aref e j i 0)))))
    (write-pgm (dir (format nil "mma~3,'0d.pgm" i)) r))
   (incf i)))

#+nil
(obtain-image)



;; OBTAIN
#+nil
(obtain-sectioned-slice)
#+nil
(progn
 (obtain-stack :n 20 :offset 3 :dz 2s0)
 nil)
(declaim (optimize (speed 1) (debug 3) (safety 3)))
;;; DRAW INTO OPENGL WINDOW (for LCOS and camera view)
(let* ((white-width 2)
       (phases-x 4)
       (phases-y 1)
       (a (make-array (* phases-x phases-y white-width) 
		      :element-type '(unsigned-byte 8)))
       (phase 0)
       
       ;(im-scale 100s0) (im-offset 1.86s0)
       (im-scale 20s0) (im-offset 0.019s0)
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

  (defun obtain-sectioned-slice (&key (mma-image *mma-bright*)
				 (accumulate 1) (z 0))
    (let ((capture-was-running (stop-capture-thread)))
      (destructuring-bind (w h) (array-dimensions clara:*im*)
	
	(let ((dark-im (make-array (list w h)
				   :element-type '(unsigned-byte 16))))
	  ;; capture a dark image
	  (setf *dark-im* t) ;; dark LCOS
	  (select-disk *mma-dark*)
	  (sleep 1/60)
	  (clara:snap-single-image)
	  (dotimes (j h)
	    (dotimes (i w)
	      (setf (aref dark-im i j) (aref clara:*im* i j))))
	  (write-pgm16 (dir (format nil "~3,'0d-0-dark.pgm" z))
		       dark-im)
	  ;; capture bright widefield image
	  (select-disk *mma-bright*)
	  (setf *dark-im* nil)
	  (setf *bright-im* t)
	  (sleep 1/60)
	  (clara:snap-single-image)
	  (dotimes (j h)
	    (dotimes (i w)
	      (setf (aref dark-im i j) (aref clara:*im* i j))))
	  (write-pgm16 (dir (format nil "~3,'0d-0-bright.pgm" z))
		       dark-im)
	  (setf *bright-im* nil
		*dark-im* nil)
	  (select-disk mma-image)
	  (sleep 1/60))
	(format t "dark and bright image captured~%")
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
	     (format t "~a~%" (list 'z z "phase" 'px px 'py py))
	     (sleep .5)
	     (clara:snap-single-image)
	     (dotimes (a accumulate)
	       (dotimes (j h)
		 (dotimes (i w)
		   (let ((v (aref clara:*im* i j)))
		     (incf (aref phase-im py px i j) v)
		     (incf (aref widefield-im i j) v)
		     (setf (aref *widefield-im* i j)
			   (clamp-u16 (floor (aref widefield-im i j) 2)))))))
	     (write-pgm16 (dir (format nil "~3,'0d-1-phase~3,'0d-~3,'0d.pgm"
				       z py px))
			  clara:*im*)))
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
	       (setf (aref *widefield-im* i j) 
		     (floor (- (aref widefield-im i j) mi)
			    (/ (- ma mi) (1- (expt 2 16)))))))
	   (format t "values in accumulated widefield image: ~a~%" (list mi ma))
	   ;; min-max reconstruction
	   (progn
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
	    (write-pgm16 
	     (dir (format nil "~3,'0d-2-section.pgm" z))
	     *section-im*))
	   
	   ;; subtract section image from accumulated widefield image
	   (setf *unfocused-im* (make-array
				 (list w h)
				 :element-type '(unsigned-byte 16)))
	   (dotimes (j h)
	     (dotimes (i w)
	       (setf (aref *unfocused-im* i j)
		     (clamp-u16 
		      (- (aref *widefield-im* i j) 
			 (floor (aref *section-im* i j) 
				;; uses max and min from widefield
				(/ (- ma mi) (* 20 (1- (expt 2 16))))))))))))
	(format t "maximum value in section ~a~%"
		(reduce #'max (sb-ext:array-storage-vector *section-im*))))
      (when capture-was-running
	(start-capture-thread))))
  
  (defun obtain-stack (&key (n 30) (dz .1s0) (offset (floor n 2)) (accumulate 1))
    (declare (ignore accumulate))
    (let ((center (focus:get-position))
	  (result nil))
      (dotimes (k n)
	(let ((z (+ center (* dz (- k offset)))))
	  (format t "slice ~f ~d/~d~%" z (1+ k) n)
	  (focus:set-position z)
	  (sleep .1)
	  (obtain-sectioned-slice :z k)
	  (push (list z *section-im*) result)))
      (focus:set-position center)
      (setf *stack* (reverse result))))

  (defun draw-screen ()
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer-bit)
    ;; BACK draw raw camera image on the left
    (gl:with-pushed-matrix
      
      (gl:scale .25 .25 1s0)
      (gl:translate 0 900 0)
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
				    :scale 3000s0 :offset 0.0005s0)))
	    (destructuring-bind (w h) (array-dimensions *section-im*)
	      (gl:translate w 0 0)
	      (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	    (gui:destroy tex)))
	;; draw accumulated widefield image below
	(when *widefield-im*
	  (let ((tex (make-instance 'gui::texture :data *widefield-im*
				    :scale 7s0 :offset 0.3s0
				    )))
	    (destructuring-bind (w h) (array-dimensions *widefield-im*)
	      (gl:translate 0 h 0)
	      (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	    (gui:destroy tex))))
      ;; draw an image with only out of focus light in the lower left
      #+nil(gl:with-pushed-matrix
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
    (gl:with-pushed-matrix 
      (gl:translate (+ 280 400) 0 0)
      (unless *dark-im*
	(let ((repetition 900f0))
	  (gui::with-grating (g a)
	    (gui:draw g 
		      :w (* repetition white-width phases-x)
		      :h (* repetition white-width phases-y)
		      :wt repetition
		      :ht repetition)))))

    ;; FAN
    (when *bright-im*
     (gl:with-pushed-matrix
       (gl:translate 1366 0 0)
       (gl:color 1 1 1)
       (let ((c (cam->lcos (v  (/ 1392 2) (+ 120 (/ 1040 2)) 1))))
	 (gl:translate (vx c) (vy c) 0.0))
       (draw-disk-fan :radius 10.0)))

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
(select-disk 4)

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
      (format s "P5~%~D ~D~%255~%" h w))
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
      (format s "P5~%~D ~D~%65535~%" h w))
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
