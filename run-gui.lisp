#.(progn
    (sb-posix:setenv "DISPLAY" ":0" 1)
    (sb-posix:setenv "__GL_SYNC_TO_VBLANK" "1" 1)
    (sb-ext:run-program "/usr/bin/xset" '("s" "off"))
    (sb-ext:run-program "/usr/bin/xset" '("-dpms"))

    (setf asdf:*central-registry* (list "/home/martin/0505/mma/"))
    (ql:quickload "cl-opengl")
    (require :gui)
   ; (require :andor3)
    (require :clara)
   ; (require :mma)
    (require :focus)) 
(defpackage :run-gui
	(:use :cl :gl #-clara :clara
	      ))
(in-package :run-gui)

#+nil
(focus:connect)
#+nil
(focus:get-position)
#+nil
(focus:set-position
 (+ (focus:get-position) .2))
#+nil
(capture)
(defvar *mma-chan* nil)
(defvar *binary-fifo* nil)

#+NIL
(sb-ext:run-program "/home/martin/0505/mma/libmma/reset" '()
		    :wait nil)
#+nil
(progn
  (defparameter *mma-chan*
    (sb-ext:run-program "/home/martin/0505/mma/libmma/mma-cmd" '()
			:output :stream
			:input :stream
			:wait nil))
  
  (sb-thread:make-thread 
   #'(lambda ()
       (unwind-protect
	   (with-open-stream (s (sb-ext:process-output *mma-chan*))
	     (loop for line = (read-line s nil nil)
		while line do
		  (format t "read: ~a~%" line)
		  (finish-output)))
	(sb-ext:process-close *mma-chan*)))
   :name "mma-cmd-reader")
  (defparameter *binary-fifo*
    (open "/home/martin/0505/mma/binary_fifo" 
	  :direction :output
	  :if-exists :supersede
	  :if-does-not-exist :error
	  :element-type '(unsigned-byte 16))))

(defun send-binary (img)
      (declare (type (simple-array (unsigned-byte 16) (256 256)) img))
      (let* ((s (sb-ext:process-input *mma-chan*))
	     (n (* 2 (array-total-size img)))
	     (img1 (sb-ext:array-storage-vector img)))
	(write-line (format nil "load ~a" n) s)
	(finish-output s)
	(write-sequence img1 *binary-fifo*)
	(finish-output *binary-fifo*)))

(defparameter *mma-img*
  (let* ((n 256)
	 (a (make-array (list n n)
		       :element-type '(unsigned-byte 16))))
    (dotimes (i 256)
      (dotimes (j 256)
	(let* ((x (- i (floor n 2)))
	       (y (- j (floor n 2)))
	       (r2 (+ (* x x) (* y y))))
	  (setf (aref a j i) (if (< r2 (expt 45 2)) #+nil (= 0 (mod (+ i j) 2))
				 4095
				 90)))))
    a))
#+nil
(send-binary *mma-img*)

(defun mma (cmd)
  (let ((s (sb-ext:process-input *mma-chan*)))
    (format s "~a~%" cmd)
    (finish-output s)))
#+nil
(mma "white")
#+nil
(mma "black")
#+nil
(mma "set-cycle-time 330")
#+nil
(mma "img")
#+nil
(mma "stop")
#+nil
(mma "off")
#+nil
(mma "on")
#+nil
(mma "start")
#+nil
(mma "frame-voltage 15.0 15.0") ;; 15V should tilt ca. 120nm
#+nil
(mma "splat 128 128 21")
#+nil
(mma "quit")

(defun mma-polar (r phi d)
  (mma (format nil "splat ~a ~a ~a" 
	       (+ 128 (* r (cos phi)))
	       (+ 128 (* r (sin phi)))
	       d)))

#+nil
(mma-polar 20s0 1.2s0 22s0)
#+nil
(progn
 (defparameter *mma-size* 20s0)
 (defparameter *mma-radius* 80s0)
 (defparameter *mma-do-run* t))
#+nil
(sb-thread:make-thread #'(lambda ()
			  (loop while *mma-do-run* do
			       (loop for p from 0 below 360 by 30 do
				    (sleep .06)
				    (mma-polar *mma-radius* (* 3.1415 (/ 180) p) *mma-size*))))
		       :name "mma-update")


#+nil
(mma "set-cycle-time 285")
#+nil
(mma "stop")
#+nil
(mma "off")
#+nil
(mma "start")
#+nil
(mma "quit")
(defun sum (img)
  (destructuring-bind (h w) (array-dimensions img)
    (let ((sum 0))
      (dotimes (j h)
	(dotimes (i w)
	  (incf sum (aref img j i))))
      sum)))



#+nil
(start-acquisition)
#+nil
(let ((fac 2))
  (start-acquisition)
  (defparameter *scan* nil)
  (loop for j below 256 by fac do
       (loop for i below 256 by fac do
	    (mma (format nil "splat ~a ~a 7" i j))
	    (sleep .07)
	    (capture)
	    (let ((s (list i j (sum *line*))))
	      (format t "~a~%" s)
	      (push s *scan*))))
  (abort-acquisition))
#+nil
(progn
  (check (start-acquisition))
  (capture)
  (check (abort-acquisition)))

#+nil
(require :vol)
#+nil
(vol:write-pgm "/dev/shm/o.pgm"
 (let* ((d *scan*)
	(fac 2)
	(ma (reduce #'max d :key #'third))
	(mi (reduce #'min d :key #'third))
	(b (make-array (list (/ 256 fac) (/ 256 fac))
		       :element-type '(unsigned-byte 8)))
	(k 0))
   (format t "~a ~a~%" ma mi)
   (dolist (e d)
     (destructuring-bind (i j val) e
       (let ((ii (mod k (/ 256 fac)))
	     (jj (floor k (/ 256 fac))))
	 (incf k)
	 (setf (aref b ii jj)
	       (max 0 (min 255 (floor (* 255 (/ (- val mi)
					       (- ma mi))))))))))
   b))

(defun mma-spot (i j &key (kernel 3))
  (let ((b (make-array (list 256 256)
		       :element-type '(unsigned-byte 16)
		       :initial-element 120)))
    (loop for y from (- kernel) upto kernel do
	 (loop for x from (- kernel) upto kernel do
	      (let ((yy (+ j y))
		    (xx (+ i x)))
		(when (and (<= 0 xx 255)
			   (<= 0 yy 255))
		  (setf (aref b yy xx) 2000)))))
    b))


#+nil (defmacro with-lcos-to-cam (&body body)
  `(let* ((s 1.129781s0)
	  (sx  s)
	  (sy  (- s))
	  (phi 0.0 #+nil 1.3154879)
	  (cp (cos phi))
	  (sp (sqrt (- 1s0 (* cp cp))))
	  (tx 800.0)
	  (ty 1198.154s0)
	  (a (make-array (list 4 4) :element-type 'single-float
			 :initial-contents
			 (list (list (* sx cp)    (* sy sp)  .0  tx)
			       (list (* -1 sx sp) (* sy cp)  .0  ty)
			       (list .0     .0   1.0  .0)
			       (list .0     .0    .0 1.0)))))
     (gl:with-pushed-matrix
       (gl:load-transpose-matrix (sb-ext:array-storage-vector a))
       ,@body)))

#+nil
(defmacro with-cam-to-lcos ((&optional (x 0s0) (y 0s0)) &body body)
  `(let* ((s .8274659387376514)
	  (sx  s)
	  (sy  (- s))
	  (phi 1.4)
	  (cp (cos phi))
	  (sp (sqrt (- 1s0 (* cp cp))))
	  (tx 783.23854s0)
	  (ty 1198.40181879s0)
	  (a (make-array (list 4 4) :element-type 'single-float
			 :initial-contents
			 (list (list (* sx cp)    (* sy sp)  .0  (+ ,x tx))
			       (list (* -1 sx sp) (* sy cp)  .0  (+ ,y ty))
			       (list .0     .0   1.0  .0)
			       (list .0     .0    .0 1.0)))))
     (gl:with-pushed-matrix
       (gl:load-transpose-matrix (sb-ext:array-storage-vector a))
       ,@body)))

(defun load-cam-to-lcos-matrix (&optional (x 0s0) (y 0s0))
  (let* ((s 0.828333873909549)
	 (sx  s)
	 (sy  (- s))
	 (phi -3.101722728951688)
	 (sp (sin phi))
	 (cp (cos phi))
	 (tx 608.4330743004457)
	 (ty 168.9188383630887)
	 (a (make-array (list 4 4) :element-type 'single-float
			 :initial-contents
			 (list (list (* sx cp)    (* sy sp)  .0  (+ x tx))
			       (list (* -1 sx sp) (* sy cp)  .0  (+ y ty))
			       (list .0     .0   1.0  .0)
			       (list .0     .0    .0 1.0)))))
    (gl:load-transpose-matrix (sb-ext:array-storage-vector a))))



(defun draw-circle (x y r)
  (gl:with-primitive :line-loop
   (let ((n 37))
     (loop for i from 0 below n do
	  (let ((arg (* i (/ n) 2 (coerce pi 'single-float)))) 
	    (gl:vertex (+ x (* r (cos arg))) (+ y (* r (sin arg)))))))))

(defun draw-disk (x y r)
  (gl:with-primitive :triangle-fan
   (let ((n 38))
     (gl:vertex x y)
     (loop for i from 0 below n do
	  (let ((arg (* i (/ (1- n)) 2 (coerce pi 'single-float)))) 
	    (gl:vertex (+ x (* r (cos arg))) (+ y (* r (sin arg)))))))))

(defparameter *do-capture* nil)
(defparameter *do-capture* t)
#+nil
(progn
  (sb-thread:make-thread 
  #'(lambda () 
      (start-acquisition)
      (loop while *do-capture* do
	   ;(sleep .01)
	   (obtain-section))
      (abort-acquisition)
      (free-internal-memory))
  :name "capture-section"))
#+nil
(progn
  
 (sb-thread:make-thread 
  #'(lambda () 
      (start-acquisition)
      (loop while *do-capture* do
	   (capture)
	   #+nil
	   (sleep .01))
      (abort-acquisition)
      (free-internal-memory))
  :name "capture"))

(progn
 (defparameter *t8* nil)
 (defparameter *t9* nil)
 (defparameter *phase-im* nil)
 (defparameter *sec* nil)
 (defparameter *dark* nil)
 (defparameter *white* nil)
 (defparameter *line* nil))
#+nil
(change-capture-size (+ 380 513) (+ 64 513) 980 650)
#+nil
(change-capture-size 1 1 1392 1040 nil)
#+nil
(change-capture-size 1 1 432 412 nil)
#+nil
(change-target 840 470 200 :ril 210s0)
#+nil
(change-phase 1)
(let* ((px 220s0) (py 230s0) (pr 230s0)
       (px-ill px) (py-ill py) (pr-ill pr)
       (w 432)
       (h 412)
       (x 1)
       (y 1)
       (crop-mode t)
       (new-size t)
       
       (white-width 8)
       (phases-x 4)
       (phases-y 1)
       (grating (make-array (* phases-x phases-y white-width)
			    :element-type '(unsigned-byte 8)))
       (phase 0)
       (count 0)
       (phase-ims (list nil nil nil nil)))
  (defun change-phase (p)
    (setf phase p)
    (when p
      (setf grating (gui:grating->texture (gui:grating-stack phases-y phases-x)
					  p :h white-width :w white-width)))
    nil)
  (change-phase 0)
  (defun change-target (x y r &key (xil x) (yil y) (ril r))
    (setf px x
	  py y
	  pr r
	  px-ill xil
	  py-ill yil
	  pr-ill ril)
    (change-capture-size (max 1 (+ 1 px (- r))) 
			 (max 1 (+ 1 py (- r)))
			 (min 1392 (+ px r))
			 (min 1040 (+ py r))))
  (defun change-capture-size (xx yy ww hh &optional crop)
    (setf w ww
	  h hh
	  x xx
	  y yy
	  new-size t
	  crop-mode crop))
  #+nil(change-capture-size 1 1 1392 1040)
 
  (defun draw-screen ()
    (incf count)
    (gl:clear-color 0 0 0 1)
    
    (gl:clear :color-buffer-bit)
    ;(sleep (/ 60))
    (gl:line-width 1)
    (gl:color 0 1 1)
    
    (when *t8*
      (gl:with-pushed-matrix
	(gl:translate (- x 1) (- y 1) 0)
	(let ((tex (make-instance 'gui::texture :data *t8*)))
	  (destructuring-bind (h w) (array-dimensions *t8*)
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)
		      :wt 1s0 :ht 1s0))
	  (gui:destroy tex))))
    (format t "~a~%" phase-ims)
    (when (elt phase-ims 0)
      (gl:with-pushed-matrix
	(gl:translate 0 0 0)
	(let ((tex (make-instance 'gui::texture :data (elt phase-ims 0))))
	  (destructuring-bind (h w) (array-dimensions (elt phase-ims 0))
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)
		      :wt 1s0 :ht 1s0))
	  (gui:destroy tex))))
    (when *t9*
      (gl:with-pushed-matrix
	(gl:translate (+ 30 w (- x 1)) (- y 1) 0)
	(let ((tex (make-instance 'gui::texture :data *t9*)))
	  (destructuring-bind (h w) (array-dimensions *t9*)
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)
		      :wt 1s0 :ht 1s0))
	  (gui:destroy tex))))
    (gl:color 1 .4 0)
    (gl:line-width 2)
    (draw-circle px py pr)
     #+nil(dotimes (i 6) 
	(dotimes (j 3)
	  (draw-circle (+ (* 50 i) px-ill) 
		       (+ (* 50 j) py-ill) 
		       (* .5 pr-ill))))
    (gl:with-pushed-matrix
      (%gl:color-3ub  #b11111110 255  255)
      (load-cam-to-lcos-matrix 0s0 1024s0)
      #-nil
      (when phase
	(gl:with-pushed-matrix 
	 (gl:translate -40 -40 0)
	 (let ((repetition 19s0))
	   (gui::with-grating (g grating)
	     (gui::draw-grating g
				:w (* repetition white-width phases-x)
				:h (* 6 repetition white-width phases-y)
				:wt repetition
				:ht repetition)))))
      #+nil(when (= 1 (mod count 2))
       (draw-disk px-ill py-ill pr-ill))
       #+nil(dotimes (i 6) 
	(dotimes (j 3)
	  (draw-disk (+ (* 50 i) px-ill) 
		     (+ (* 50 j) py-ill) 
		     (* .5 pr-ill))))))
  
  (defun capture ()
    #-clara (when new-size
	      (unless (eq 'clara::DRV_IDLE
			  (lookup-error (val2 (get-status))))
		(abort-acquisition))
	      (if crop-mode
		  (check (clara::set-isolated-crop-mode 1 h w 1 1))
		  (check (set-image 1 1 x w y h)))
	      (start-acquisition)
	      (setf new-size nil))
    #+andor3 (defparameter *line*
      (multiple-value-bind (ptr img) (andor3::wait-buffer)
	(let* ((cpy (make-array (array-dimensions img)
				:element-type '(unsigned-byte 16)))
	       (img1 (sb-ext:array-storage-vector img))
	       (cpy1 (sb-ext:array-storage-vector cpy)))
	  (dotimes (i (length cpy1))
	    (setf (aref cpy1 i) (aref img1 i)))
	  (andor3::requeue-buffer ptr)
	  cpy)))
    #-clara (defparameter *line*
     (let* ((img (make-array (list (- h (1- y)) (- w (1- x)))
			     :element-type '(unsigned-byte 16)))
	    (img1 (sb-ext:array-storage-vector img))
	    (sap (sb-sys:vector-sap img1)))
       (progn
	#+nil (start-acquisition)
	 #+nil (loop while (not (eq 'clara::DRV_IDLE
				    (lookup-error (val2 (get-status)))))
	    do
	    (sleep .003))
	(check (wait-for-acquisition)) 
	(format t "imgs ~a~%" (multiple-value-list (get-total-number-images-acquired)))
	 (sb-sys:with-pinned-objects (img)
	    (check (get-most-recent-image16 sap (length img1)))
	   #+nil (check (get-acquired-data16 sap (length img1))))
	#+nil (abort-acquisition)
        #+nil(check
	   (free-internal-memory))
	 )
       img))
   (defparameter *t8*
     (when (and *line*			;*dark* *white* *line*
		)
       (let* ((b (make-array (array-dimensions *line*)
			     :element-type '(unsigned-byte 8)))
	      (b1 (sb-ext:array-storage-vector b))
					;(d1 (sb-ext:array-storage-vector *dark*))
					;(w1 (sb-ext:array-storage-vector *white*))
	      (l1 (sb-ext:array-storage-vector *line*))
	      )
	 (destructuring-bind (h w) (array-dimensions *line*)
	   (declare (ignorable h))

	   (dotimes (i (length b1))
	     (let ((v (if (< 1 (aref l1 i))
			  (min 255 (max 0 (floor (- (aref l1 i) 0) 
						 1)))
			  (let ((yy (floor i w))
				(xx (mod i w)))
			    (cond ((or (= 0 (mod (+ y yy) 500))
				       (= 0 (mod (+ x xx) 500)))
				   255)
				  ((or (= 0 (mod (+ y yy) 100))
				       (= 0 (mod (+ x xx) 100)))
				   80)
				  (t 0))))))
	       (setf (aref b1 i) v))))
	 b))))
  (defun obtain-section ()
    (let ((phase-im (make-array (list phases-y phases-x h w)
				:element-type '(unsigned-byte 16)))
	  (sec (make-array (list h w)
			   :element-type '(unsigned-byte 16))))
      (dotimes (py phases-y)
	(dotimes (px phases-x)
	  (change-phase (+ px (* phases-x py)))
	  (sleep .05)
	  (capture)
	  (setf (elt phase-ims px) *line*)
	  (dotimes (j h)
	    (dotimes (i w)
	      (setf (aref phase-im py px j i) (aref *line* j i))))))
      (defparameter *phase-im* phase-im)
      (dotimes (j h)
	(dotimes (i w)
	  (let* ((v (aref phase-im 0 0 j i))
		 (mi v)
		 (ma v))
	    (dotimes (py phases-y)
	      (dotimes (px phases-x)
		(setf mi (min mi (aref phase-im py px j i))
		      ma (max ma (aref phase-im py px j i)))))
	    (setf (aref sec j i) (- ma mi)))))
      (setf *sec* sec)
      (defparameter *t9*
	(let* ((b (make-array (list h w)
			      :element-type '(unsigned-byte 8)))
	       (b1 (sb-ext:array-storage-vector b))
	       (s1 (sb-ext:array-storage-vector sec)))
	  (dotimes (i (length b1))
	    (setf (aref b1 i) (min 255 (max 0
					    (floor (aref s1 i)
						   1)))))
	  b))
      (change-phase nil)))
  (defun obtain-sectioned-stack (&key (n (* 2 23)) (depth 23s0))
  (when *line*
    (destructuring-bind (h w) (array-dimensions *line*)
      (let* ((v (make-array (list n h w)
			    :element-type '(unsigned-byte 16)))
	     (vp (make-array (list (* n phases-x) h w)
			    :element-type '(unsigned-byte 16)))
	     (z0 (focus:get-position)))
	(setf *do-capture* nil)
	(start-acquisition)
	(dotimes (k n)
	  (focus:set-position (+ z0 (* k (/ depth n))))
	  (sleep .02)
	  (obtain-section)
	  (dotimes (j h)
	    (dotimes (i w)
	      (setf (aref v k j i) (aref *sec* j i))
	      (dotimes (p phases-x)
		(setf (aref vp (+ (* phases-x k) p) j i) (aref *phase-im* 0 p j i))))))
	(defparameter *vol* v)
	(defparameter *volp* vp)
	(focus:set-position z0)
	(setf *do-capture* t)
	(abort-acquisition))))))


#+nil
(time (obtain-sectioned-stack))
#+nil
(focus:get-position)
#+nil
(change-phase nil)

#+nil
(progn
  (push "/home/martin/0215/0126/bead-eval/" asdf:*central-registry*)
  (push "/home/martin/0215/0102/woropt-cyb-0628/" asdf:*central-registry*)
  (require :bead-eval)
  (require :vol))
#+nil
(vol:save-stack-ub8 "/dev/shm/op/" 
		    (vol:normalize-3-sf/ub8 (vol:convert-3-ub16/sf-mul *volp*)))
#+nil
(defparameter *g3* (bead-eval:make-gauss3 (vol:convert-3-ub16/sf-mul *vol*) :sigma-x-pixel 3.1s0))
#+nil
(defparameter *bvol* (vol:convert-3-csf/sf-realpart
		    (vol:convolve-circ-3-csf *g3*
					     (vol:convert-3-ub16/csf-mul *vol*))))
#+nil
(vol:write-pgm "/dev/shm/c.pgm" 
	       (vol:normalize-2-sf/ub8
		(vol:cross-section-xz *bvol*)))
#+nil
(let ((l (run-ics::nuclear-seeds *bvol*)))
  (multiple-value-bind (hist n mi ma)
      (run-ics::point-list-histogram l)
    (run-ics::print-histogram hist n (* 1s10 mi) (* 1s10 ma))
    (terpri)
    (setf *num-points* (reduce #'+ (subseq hist 5)))))

#+nil
(capture)
#+nil
(progn (start-acquisition)
       (obtain-section)
       (abort-acquisition))
#+nil
(let ((a (sb-ext:array-storage-vector *sec*)))
  (reduce #'max a))
#+NIL
(status)
#+nil
(clara::prepare-acquisition)
#+nil
(abort-acquisition)
#+nil
(lookup-error (val2 (get-status)))
#+nil
(sb-thread:make-thread 
 #'(lambda ()
     (gui:with-gui (1280 (* 2 1024))
       (draw-screen)))
 :name "display-gui")
#+nil
(gui::get-frame-rate)