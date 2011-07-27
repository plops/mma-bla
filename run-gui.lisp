(eval-when (:compile-toplevel)
  (progn
    (sb-posix:setenv "DISPLAY" ":0" 1)
    (sb-posix:setenv "__GL_SYNC_TO_VBLANK" "1" 1)
    (sb-ext:run-program "/usr/bin/xset" '("s" "off"))
    (sb-ext:run-program "/usr/bin/xset" '("-dpms"))

    (setf asdf:*central-registry* (list "/home/martin/0505/mma/"))
    (ql:quickload "cl-opengl")))
(eval-when (:compile-toplevel)
  (require :gui)
  ;; (require :andor3)
  (require :clara)
  ;; (require :mma)
  (require :focus)
  (require :sb-concurrency)
  ;; (require :cl-glut)
  ) 
(defpackage :run-gui
  (:use :cl :gl #-clara :clara))
(in-package :run-gui)



#+nil
(focus:connect "/dev/ttyUSB0")
#+nil
(focus:get-position)
#+nil
(focus:set-position
 (+ (focus:get-position) .4s0))
#+nil
(capture)
(defvar *mma-chan* nil)
(defvar *binary-fifo* nil)


#+NIL
(sb-ext:run-program "/home/martin/0505/mma/libmma/reset" '()
		    :wait nil)
#+nil
(progn
  (sb-thread:make-thread 
   #'(lambda ()
       (defparameter *mma-chan*
	 (sb-ext:run-program "/home/martin/0505/mma/libmma/mma-cmd" '()
			     :output :stream
			     :input :stream
			     :wait nil))
       (sb-ext:process-wait *mma-chan*))
   :name "mma-waiting-father")
  (sleep .3)
  (sb-thread:make-thread 
   #'(lambda ()
       (unwind-protect
	   (with-open-stream (s (sb-ext:process-output *mma-chan*))
	     (loop for line = (read-line s nil nil)
		while line do
		  (format t "mma read: ~a~%" line)
		  (finish-output)))
	(sb-ext:process-close *mma-chan*)))
   :name "mma-cmd-reader")
  (defparameter *binary-fifo*
    (open "/home/martin/0505/mma/binary_fifo" 
	  :direction :output
	  :if-exists :supersede
	  :if-does-not-exist :error
	  :element-type '(unsigned-byte 16)))
  (send-binary *mma-img*)
  (mma "set-cycle-time 33.27"))

(defun send-binary (img)
      (declare (type (simple-array (unsigned-byte 16) (256 256)) img))
      (let* ((s (sb-ext:process-input *mma-chan*))
	     (n (* 2 (array-total-size img)))
	     (img1 (sb-ext:array-storage-vector img)))
	(write-line (format nil "load ~a" n) s)
	(finish-output s)
	(write-sequence img1 *binary-fifo*)
	(finish-output *binary-fifo*)))


(defparameter *mma-imgs*
 (let ((res nil)
       (n 20))
   (dotimes (k n)
     (let* ((arg (* 2 pi (/ k n)))
	    (n 256)
	    (a (make-array (list n n)
			   :element-type '(unsigned-byte 16))))
       (dotimes (i 256)
	 (dotimes (j 256)
	   (let* ((x (+ (- i (floor n 2)) (* 90 (cos arg))))
		  (y (+ (- j (floor n 2)) (* 90 (sin arg))))
		  (r2 (+ (* x x) (* y y))))
	     (setf (aref a j i) (if t #+nil (< r2 (expt 45 2)) 
				    #+nil(= 0 (mod (+ i j) 2))
				    (min 4095 (floor (* 4095 (exp (* -.0004 r2)))))
				    90)))))
       (push a res)))
   (reverse res)))

(defparameter *mma-img*
  (let* ((n 256)
	 (a (make-array (list n n)
		       :element-type '(unsigned-byte 16))))
    (dotimes (i 256)
      (dotimes (j 256)
	(let* ((x (- i (floor n 2)))
	       (y (- j (floor n 2)))
	       (r2 (+ (* x x) (* y y))))
	  (setf (aref a j i) (if t #+nil (< r2 (expt 45 2)) 
				 #+nil(= 0 (mod (+ i j) 2))
				 (min 4095 (floor (* 4095 (exp (* -.0004 r2)))))
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
(mma "set-cycle-time 33")
#+nil
(mma "set-cycle-time 33.27")
#+nil
(mma "set-cycle-time 193.5")
#+nil
(mma "deflection 118.5")
#+nil
(mma "help")
#+nil ;; ANGLE
(let ((n (length *mma-imgs*))
      (i 0))
  (dolist (e *mma-imgs*)
    (send-binary e)
    (mma (format nil "img ~a" (1+ i)))
    (mma (format nil "set-picture-sequence ~a ~a 1" (1+ i) (if (= i (1- n)) 1 0)))
    (incf i)))
#+nil
(let ((i 0)
      (n 1))
 (mma (format nil "set-picture-sequence ~a ~a 1" (1+ i) (if (= i (1- n)) 1 0))))

#+nil
(mma "stop")
#+nil
(mma "off")
#+nil
(mma "on")
#+nil
(mma "get-temperature")
#+nil
(mma "start")
#+nil
(mma "frame-voltage 15.0 15.0") ;; 15V should tilt ca. 120nm
#+nil
(mma "splat 128 128 26")
#+nil
(mma "quit")
#+nil
(clara::uninit )
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

#+nil
(defun draw-circle (x y r)
  (gl:with-primitive :line-loop
   (let ((n 37))
     (loop for i from 0 below n do
	  (let ((arg (* i (/ n) 2 (coerce pi 'single-float)))) 
	    (gl:vertex (+ x (* r (cos arg))) (+ y (* r (sin arg)))))))))
#+nil
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
	   (capture))
      (abort-acquisition)
      (free-internal-memory))
  :name "capture"))
#+nil
(clara::abort-acquisition)
#+nil
(setf sb-ext:*after-gc-hooks*
      (list #'(lambda () 
                (format t " ~a~%" 
                        (/ sb-ext:*gc-run-time*
                           (* 1s0 internal-time-units-per-second)))
                (setf sb-ext:*gc-run-time* 0))))

#+nil
(sb-thread:make-thread 
 #'(lambda ()  ;; CAPTURE
     
     (progn
       (setf *line* (sb-concurrency:make-queue :name 'picture-fifo))
       
       (clara::prepare-acquisition)

       (progn ;; display a disk ontop of in-focus bead
	 (dotimes (i (length *img-array*))
	   (dotimes (j 2)
	     (lcos "qdisk 225 225 80")
	     (lcos "qswap")))

	 ;; start LCOS
	 (lcos "toggle-queue 1"))

       (let ((n (length *mma-imgs*))) ;; start MMA
	 (dotimes (i n)
	   (mma (format nil "set-picture-sequence ~a ~a 1"
			(1+ i) (if (= i (1- n)) 
				   1 
				   0)))))

       (start-acquisition) ;; start camera
       ;; the first captured frame doesn't have any lcos image
       ;; it can be used as a dark frame

       (let ((count 0))
	 (format t "count: ~a~%" count)
	 (loop while (and *do-capture*
			  (< count (length *img-array*))) do
	      (capture)
	      (loop for i below (sb-concurrency:queue-count *line*) do
		   (setf (aref *img-array* count)
			 (sb-concurrency:dequeue *line*))
		   (incf count))))
       (abort-acquisition)
       (free-internal-memory)))
 :name "capture")



(defun transpose-ub16 (img)
  "Transpose the image that comes from the Andor camera."
  (declare (type (simple-array (unsigned-byte 16) 2) img)
	   (values (simple-array (unsigned-byte 16) 2) &optional))
  (let ((dim (array-dimensions img)))
   (destructuring-bind (y x) dim
     (let ((img1 (sb-ext:array-storage-vector img))
	   (r (make-array (list x y) :element-type '(unsigned-byte 16))))
       (vol:do-region ((j i) (y x))
	 (setf (aref r i j) (aref img1 (+ (* y i) j))))
       r))))

(defun maxima (img)
  (destructuring-bind (y x) (array-dimensions img)
    (let ((points ()))
      (vol:do-region ((j i) ((1- y) (1- x)) (1 1))
        (macrolet ((q (n m)
                     `(< (aref img (+ ,n j) (+ ,m i)) e)))
         (let* ((e (aref img j i)))
           (when (and (q 0 1) (q 0 -1)
                      (q 1 0) (q -1 0)
                      (q 1 1) (q 1 -1)
                      (q -1 1) (q -1 -1))
             (push (list e (list j i)) points)))))
      points)))

(defun locate-beads (img)
  (let ((in (transpose-ub16 img)))
    (destructuring-bind (y x) (array-dimensions in)
      (let ((b (vol:normalize-2-csf/ub8-realpart 
		(vol:convolve-circ
		 (vol:draw-disk-csf 9s0 y x)
		 (vol:convert-2-ub16/csf-mul in)))))
	(let ((points
	       (subseq (sort (maxima b) #'(lambda (x y) (< (first y) (first x))))
		       0 4)))
	  (dolist (p points)
	   (destructuring-bind (h (y x)) p
	     (declare (ignore h))
	     (vol:do-region ((j i) ((+ y 2) (+ x 2)) ((- y 2) (- x 2)))
	       (setf (aref in j i) 500))))
	 (vol:write-pgm "/dev/shm/01beads.pgm" 
			(vol:normalize-2-sf/ub8 (vol:convert-2-ub16/sf-mul in)))
	 points)))))


#+nil
(sb-thread:make-thread 
 #'(lambda ()  ;; CAPTUREADAPT
     
     (let ((img-array (make-array 2)))
       (setf *line* (sb-concurrency:make-queue :name 'picture-fifo))
       
       (clara::prepare-acquisition)
       
       (progn ;; display a disk ontop of in-focus bead
	 (dotimes (i (1- (length img-array)))
	   (dotimes (j 2)
	     (lcos "qdisk 225 225 200")
	     (lcos "qswap")))
	 
	 ;; start LCOS
	 (lcos "toggle-queue 1"))

       (start-acquisition) ;; start camera
       ;; the first captured frame doesn't have any lcos image
       ;; it can be used as a dark frame

       (let ((count 0))
	 (format t "count: ~a~%" count)
	 (loop while (and *do-capture*
			  (< count (length img-array))) do
	      (capture)
	      (loop for i below (sb-concurrency:queue-count *line*) do
		   (setf (aref img-array count)
			 (sb-concurrency:dequeue *line*))
		   (incf count)))
	 (abort-acquisition)
	 (free-internal-memory))

       (defparameter *bla* img-array)
       
       (let* ((points (locate-beads (aref img-array 1)))
	      (bead-img (make-array (1+ (length points)))))
	 (dolist (p points)
	   (destructuring-bind (h (y x)) p
	     (declare (ignore h))
	     (dotimes (i 2)
	       (lcos (format nil "qdisk ~d ~d 80" y x))
	       (lcos "qswap"))))
	 
	 
	 (let ((count 0))
	   (clara::prepare-acquisition)
	   (lcos "toggle-queue 1")
	   (start-acquisition) 
	   (format t "count: ~a~%" count)
	   (loop while (and *do-capture*
			    (< count (length bead-img))) do
		(capture)
		(loop for i below (sb-concurrency:queue-count *line*) do
		     (setf (aref bead-img count)
			 (sb-concurrency:dequeue *line*))
		     (incf count)))
	   (abort-acquisition)
	   (free-internal-memory))
	 (dotimes (i (length bead-img))
	   (vol::write-pgm-transposed (format nil "/dev/shm/02_beadnr~3,'0d.pgm" i)
			  (vol:normalize-2-sf/ub8 
			   (vol:convert-2-ub16/sf-mul (aref bead-img i))))))))
 :name "capture")







#+nil
(progn ;; reset mma and lcos and start camera
  ;; capture n frames and save into /dev/shm
  (sb-thread:make-thread 
   #'(lambda () 
       (let ((n (* 100 (length *mma-imgs*))))
	 (setf *line* (sb-concurrency:make-queue :name 'picture-fifo))
	 (start-acquisition)
	 
	 (dotimes (i n)
	   (mma (format nil "set-picture-sequence ~a ~a 1" 
			(1+ i) ;; images start with 1
			(if (= i (1- n)) 1 0) ;; last image (n-1)th has parameter 1
			)))
	 (dotimes (j 1)
	   (dotimes (i (* 2 n)) 
	     (lcos (format nil "qnumber ~a" i))
	     (lcos "qswap")))
	 (let ((count 0))
	   (loop while (< count n) do
		(capture)
		(dotimes (i (sb-concurrency:queue-count *line*))
		  (setf (aref *img-array* count)
			(sb-concurrency:dequeue *line*))
		  (incf count))))
	 (abort-acquisition)
	 (free-internal-memory)
	 (format t "capture finished!~%")
	 (dotimes (i n) 
	   (vol::write-pgm-transposed 
	    (format nil "/dev/shm/~4,'0d.pgm" i)
	    (vol:normalize-2-sf/ub8
	     (vol:convert-2-ub16/sf-mul
	      (aref *img-array* i)))))))
   :name "capture"))
#+nil
(require :vol)


(progn
  (defparameter *t8* nil)
  (defparameter *t9* nil)
  (defparameter *phase-im* nil)
  (defparameter *sec* nil)
  (defparameter *dark* nil)
  (defparameter *white* nil)
  (defparameter *line* (sb-concurrency:make-queue :name 'picture-fifo)))

#+nil
(change-capture-size (+ 380 513) (+ 64 513) 980 650)
#+nil
(change-capture-size 1 1 1392 1040 nil)
#+nil
(change-capture-size 1 1 432 412 t)
#+nil
(change-capture-size 1 1 512 512 t)
#+nil
(change-capture-size 1 1 256 256 t)
#+nil
(change-target 840 470 200 :ril 210s0)
#+nil
(change-phase 1)
(defparameter *img-array* (make-array (* 100)))
#+nil
(require :vol)
#+nil 
(time ;; STORE
 (let* ((max-threads 4)
	(w (/ (length *img-array*)
	      max-threads)))
   (let ((thr (loop for p below max-threads collect
		   (sb-thread:make-thread
		    (lambda ()
		      (sb-sys:without-gcing
		       (dotimes (i w)
			 (let* ((p (read-from-string (sb-thread:thread-name sb-thread:*current-thread*))) 
				(ii (+ i (* w p))))
			   (when (arrayp (aref *img-array* ii))
			     (vol::write-pgm-transposed 
			      (format nil "/dev/shm/~4,'0d.pgm" ii)
			      (vol:normalize-2-sf/ub8
			       (vol:convert-2-ub16/sf-mul
				(aref *img-array* ii)))))))))
		    :name (format nil "~a" p)))))
     (mapcar #'sb-thread:join-thread thr))))

#+nil
(room)

(let* ((count 0)
       (h 412)
       (w 432)
       (px-ill 220s0)
       (py-ill 230s0)
       (pr-ill 230s0)
       (img-circ (make-array (list 141 #+nil run-clara::*circ-buf-size* h w)
			     :element-type '(unsigned-byte 16))))
  ;(gui::reset-frame-count)
  (defun draw-screen ()
    ;(gl:draw-buffer :back)
    ;(clear-color .1 0 0 1)
    (gl:clear :color-buffer-bit)
    (let ((c (sb-concurrency:queue-count *line*)))
     (unless (or (= 0 c) (= 1 c))
       (format t "*~a*" c)))
    (loop for e below (sb-concurrency:queue-count *line*) do
	 (let ((e (sb-concurrency:dequeue *line*))
	       (p (mod count 5)))
	  (when e
	     (gl:with-pushed-matrix
	       (let* ((tex (make-instance 'gui::texture16 :data e
					  :scale 402s0 :offset 0.0077s0
					  )))
		(destructuring-bind (h w) (array-dimensions e)
		  ;; current image
		  (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)
			    :wt (* h 1s0) :ht (* w 1s0))
		  
		  (with-pushed-matrix 
		    (gl:translate (- 1024 550 (* .25 w (floor p))) 420 0)
		    (gl:scale .25 .25 .25)
		    ;; small copies of earlier images
		    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)
			      :wt (* h 1s0) :ht (* w 1s0))))
		(gui:destroy tex)))))
	 (incf count))
    (when (and *mma-imgs* (first *mma-imgs*))
      (let ((cnt 0))
       (dolist (e *mma-imgs*)
	 (with-pushed-matrix
	   ;; mma image
	   (gl:scale .25 .25 .25)
	   (gl:translate (* cnt 256) 2100 0)
	   (incf cnt)
	   (let ((tex (make-instance 'gui::texture16 :data e
				     :scale 16s0 :offset 0s0)))
	     (destructuring-bind (h w) (array-dimensions e)
	       (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)
			 :wt (* h 1s0) :ht (* w 1s0)))
	     (gui:destroy tex)))))))
  #+nil
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
	#+nil (loop while (eq 'clara::DRV_IDLE
			(lookup-error (val2 (get-status))))
	   do
	     (sleep .003))
	(check (wait-for-acquisition)) 
	(format t "imgs ~a~%" (list (mod count 4) (floor count 2) (val2 (get-total-number-images-acquired))))
	 (sb-sys:with-pinned-objects (img)
	    (check (get-most-recent-image16 sap (length img1)))
	   #+nil (check (get-acquired-data16 sap (length img1))))
	#+nil (abort-acquisition)
        #+nil(check
	   (free-internal-memory))
	 )
       img))
   #+nil8 (defparameter *t8*
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
						 11)))
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

  (defun capture ()
    (let* ((img1 (sb-ext:array-storage-vector img-circ))
	   (sap (sb-sys:vector-sap img1)))
      (destructuring-bind (z y x) (array-dimensions img-circ)
	(declare (ignorable z))
	(check (wait-for-acquisition)) 
	(multiple-value-bind (ret-num-avail first last)
	    (clara::get-number-new-images)
	  (check ret-num-avail)
	  (let ((n (- last first)))
	    (format t "~a" (1+ n))
	    (finish-output)
	    (sb-sys:with-pinned-objects (img-circ)
	      (multiple-value-bind (ret-get16 validfirst validlast)
		  (clara::get-images16 first last sap (* (1+ n) y x))
		(check ret-get16)
		(unless (and 
			 (= validlast last)
			 (= validfirst first))
		  (break "couldn't get as many images as expected ~a"
			 (list first last :valid validfirst validlast)))))
	    (dotimes (k (1+ n))
	      (let ((a (make-array (list y x) 
				   :element-type '(unsigned-byte 16))))
		(dotimes (j y)
		  (dotimes (i x)
		    (setf (aref a j i)
			  (aref img-circ k j i))))
		(sb-concurrency:enqueue a *line*))))))))
  #+nil
  (defun obtain-section ()
    (let ((phase-im (make-array (list phases-y phases-x h w)
				:element-type '(unsigned-byte 16)))
	  (sec (make-array (list h w)
			   :element-type '(unsigned-byte 16))))
      (dotimes (py phases-y)
	(dotimes (px phases-x)
	  (change-phase (+ px (* phases-x py)))
	  ;(sleep .05)
	  (capture)
	  (setf (elt phase-ims px) *line*)
	  (dotimes (j h)
	    (dotimes (i w)
	      (setf (aref phase-im py px j i) (aref *line* j i))))))
      #+nil8 (defparameter *phase-im* phase-im)
      #+nil8 (dotimes (j h)
	(dotimes (i w)
	  (let* ((v (aref phase-im 0 0 j i))
		 (mi v)
		 (ma v))
	    (dotimes (py phases-y)
	      (dotimes (px phases-x)
		(setf mi (min mi (aref phase-im py px j i))
		      ma (max ma (aref phase-im py px j i)))))
	    (setf (aref sec j i) (- ma mi)))))
      #+nil8 (setf *sec* sec)
      #+nil8 (defparameter *t9*
	(let* ((b (make-array (list h w)
			      :element-type '(unsigned-byte 8)))
	       (b1 (sb-ext:array-storage-vector b))
	       (s1 (sb-ext:array-storage-vector sec)))
	  (dotimes (i (length b1))
	    (setf (aref b1 i) (min 255 (max 0
					    (floor (aref s1 i)
						   11)))))
	  b))
      (change-phase nil)))
  #+nil
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
	  ;(sleep .02)
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
(let ((x 700)
      (y 100))
  (sb-posix:setenv "DISPLAY" ":0" 1)
  (sb-thread:make-thread
   #'(lambda ()
       (gui:with-gui ((- 1280 x) 700 x y)
	 (draw-screen)))
   :name "camera-display"))

(defparameter *lcos-chan* nil)
(defun lcos (cmd)
  (let ((s (sb-ext:process-input *lcos-chan*)))
    (format s "~a~%" cmd)
    (finish-output s)
    (force-output s)))


#+nil
(progn
  (sb-posix:setenv "DISPLAY" ":0" 1)
  (sb-posix:setenv "__GL_SYNC_TO_VBLANK" "1" 1)
  (sb-thread:make-thread
   #'(lambda () 
       (setf *lcos-chan*
	     (sb-ext:run-program "/home/martin/0505/mma/glfw-server/glfw" '("1280" "1024")
				 :output :stream
				 :input :stream
				 :wait nil))
       (sb-ext:process-wait *lcos-chan*))
   :name "glfw-waiting-father")
  
  (sb-thread:make-thread 
   #'(lambda ()
       (unwind-protect
           (with-open-stream (s (sb-ext:process-output *lcos-chan*))
             (loop for line = (read-line s nil nil)
                while line do
                  (format t "lcos read: ~a~%" line)
                  (finish-output)))
         (sb-ext:process-close *lcos-chan*)))
   :name "lcos-cmd-reader"))
#+nil
(lcos "toggle-stripes 1")
#+NIL
(lcos "toggle-stripes 0")
#+nil
(lcos "toggle-queue 0")
#+nil
(lcos "toggle-queue 1")
#+nil
(lcos "quit")
#+nil
(lcos "help")
#+nil
(lcos (format nil "toggle-notify-mma ~d"
	      (sb-ext:process-pid *mma-chan*)))

#+nil ;; turn lcos white
(dotimes (j 1)
  (dotimes (i 1000)
    (lcos "qdisk 200 225 200")
    (lcos "qswap")
    (lcos "qdisk 200 225 200")
    (lcos "qswap"))
  (sleep .4)
  (lcos "toggle-queue 1"))
#+nil
(let ((a (random 100)))
  (dotimes (i 300)
    (let* ((arg (* 2 pi (/ (mod i 10) 10)))
	   (r 140)
	   (c (+ 225 (* r (cos arg))))
	   (s (+ 225 (* r (sin arg)))))
      (lcos (format nil "qline 225 225 ~f ~f" c s)))
    (let ((x 100) (xx 350)
	  (y 90) (yy 350))
      (lcos (format nil "qline ~f ~f ~f ~f" x y xx y))
      (lcos (format nil "qline ~f ~f ~f ~f" x y x yy))
      (lcos (format nil "qline ~f ~f ~f ~f" x yy xx yy))
      (lcos (format nil "qline ~f ~f ~f ~f" xx y xx yy)))
    (lcos (format nil "qnumber ~f" i))
    (lcos "qswap")))

#+nil
(dotimes (i 300)
    (lcos (format nil "qnumber ~a" i))
    (lcos "qswap"))
#+nil
(progn
  (let ((n (length *mma-imgs*)))
  (dotimes (i n)
    (mma (format nil "set-picture-sequence ~a ~a 1" (1+ i) (if (= i (1- n)) 1 0)))))
 (lcos "toggle-queue 1"))