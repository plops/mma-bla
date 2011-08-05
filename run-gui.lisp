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
  (require :vol)
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



(unless focus::*fd*
 (focus:connect "/dev/ttyUSB0"))
#+nil
(focus:get-position)
#+nil
(focus:set-position
 (+ (focus:get-position) .4s0))

(defvar *mma-chan* nil)
(defvar *binary-fifo* nil)

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
  (mma "set-cycle-time 33.27")
  (send-binary *mma-img*))

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
	  (setf (aref a j i) 4059 #+nil (if t #+nil (< r2 (expt 45 2)) 
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
(mma "set-cycle-time 33.27")
#+nil
(mma "deflection 118.5")
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
(mma "get-temperature")
#+nil
(mma "frame-voltage 15.0 15.0") ;; 15V should tilt ca. 120nm
#+nil
(mma "quit")
#+nil
(clara::uninit)

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

(defparameter *do-capture* nil)
(defparameter *do-capture* t)
(defparameter *do-display-queue* nil)
(defparameter *do-display-queue* t)
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

#+nil ;; turn lcos white
(let ((phases 3))
 (dotimes (j 1)
   (dotimes (i 300)
     (dotimes (k 2)
       #+nil(lcos (format nil "qgrating-disk 425 325 200 ~d ~d 4" 
		     (mod i phases) phases))
       ;;(draw-grating-disk 200 225 380 :phase (mod i 3)))
       (lcos "qdisk 200 225 280")
       (sleep .001)
       (lcos "qswap")))
   (sleep .4)
   (lcos "toggle-queue 1")))


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
	       (when (and (<= 0 i (1- x))
			  (<= 0 j (1- y)))
		 (setf (aref in j i) 500)))))
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
	       (lcos (format nil "qdisk ~d ~d 30" x y))
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

(defun make-slice (z-position type)
  (list z-position type))

(defparameter *sequence* nil)


(defun plan-stack (&key (slices 1) lcos-seq (start-pos 0) (dz 1)
		   (frame-period (/ 60)) (start-time 0d0)
		   (stage-settle-duration 20) (lcos-lag 0))
  (let ((res nil)
        (pos start-pos)
        (time (+ start-time frame-period)))
    (push (list :type :capture
                :start 0
                :end 15
                :content :dark)
          res)
    (loop for k below slices do
         (let ((cam nil))
          (push (list :type :lcos-display
                      :pos pos
                      :lcos-seq
                      (let ((lcos nil))
                        (loop for e in lcos-seq do
                           ;; lcos displays each frame twice
                             (push (list :start time 
                                         :end (+ time 15)
                                         :content e) 
                                   lcos)
                             (incf time frame-period)
                             (push (list :start time
                                         :end (+ time 15)
                                         :content e)
                                   lcos)
                           ;; one of the frames is captured by the camera
                             (push (list :type :capture
                                         :start time
                                         :end (+ time 15)
                                         :content e)
                                   cam)
			   ;; for this frame the MMA was white
			     (push (list :type :mma
                                         :start time
                                         :end (+ time 15)
                                         :content e)
                                   cam)
                             (incf time frame-period))
                        (reverse lcos)))
                res)
          (loop for e in (reverse cam) do
               (push e res))
          (incf pos dz)
          (unless (= k (- slices 1))
           (push (list :type :stage-move
                       :start (+ time frame-period) ;; move stage in one of the dark images
                       :end (+ time frame-period stage-settle-duration (- lcos-lag))
                       :pos pos)
                 res))))
    (reverse res)))


(defun extract-moves (ls)
  (mapcar #'(lambda (x) (getf x :start)) 
          (remove-if-not #'(lambda (x) (eq :stage-move (getf x :type)))
                         ls)))

(defparameter *stack-state* nil)


(defmacro ss (sym)
  "Access an entry in the stack state"
  `(getf *stack-state* ,sym))

(defun prepare-stack-acquisition ()
  (when (ss :wait-move-thread) 
    (close-move-thread))
  (let* ((start-pos (focus:get-position))
	 (seq (plan-stack :slices 21 :lcos-seq '(:dark 0 1 2)
			  :frame-period (/ 1000 60)
			  :start-pos start-pos :dz 1))
	 (moves (extract-moves seq)))
    (setf *stack-state* nil)
     (setf (ss :seq) seq
	   (ss :moves) moves ;; planned moves
	   (ss :real-moves) nil ;; times when actual stage movements occured
	   (ss :start) 0
	   (ss :phases) 3
	   (ss :do-wait-move) t
	   (ss :wait-move-thread) nil
	   (ss :start-position) start-pos
	   (ss :set-start-at-next-swap-buffer) nil)))


(prepare-stack-acquisition)


(defun close-move-thread ()
  (let ((thread (ss :wait-move-thread)))
    (when thread
      (setf (ss :do-wait-move) nil)
      (handler-case (sb-thread:join-thread thread)
	(sb-thread:join-thread-error () (format t "Note: no thread woke up")))
      (setf (ss :do-wait-move) t))))

(defun previous-move (time ls)
  (first (last (remove-if #'(lambda (x) (< time x)) ls))))

(defun next-move (time ls)
  (first (remove-if #'(lambda (x) (< x time)) ls)))

#+nil
(next-move 320 (ss :moves))

(defun next-position (time)
  (getf (first (remove-if #'(lambda (x) (< (getf x :start) time))
			  (mapcar #'(lambda (x) (let ((y nil)) 
					     (setf (getf y :start) (getf x :start)
						   (getf y :pos) (getf x :pos))
					     y)) 
				  (remove-if-not #'(lambda (x) (eq :stage-move (getf x :type)))
						 (ss :seq)))))
	:pos))
#+nil
(next-position 304)

(defun clear-real-moves ()
  (prog1
      (ss :real-moves)
    (setf (ss :real-moves) nil)))

#+nil
(clear-real-moves)

(defun move-stage-fun ()
  (loop while (ss :do-wait-move) do
       (let ((start (ss :start)))
	 (if (= start 0)
	     (sleep (/ 2 1000))
	     (let* ((time (- (get-internal-real-time) start))
		    (next (or (next-move time (ss :moves))
			      (progn 
				(push time (ss :real-moves))
				(setf (ss :real-moves) (reverse (ss :real-moves)))
				(focus:set-position (ss :start-position))
				(return-from move-stage-fun
				  (+ 1 time)))))
		    (diff (- next time)))
	    (when (< 2 diff)
	      (push time (ss :real-moves))
	      (let ((npos (next-position time)))
		(format t "~a~%" (list diff time next npos))
		(focus:set-position npos))
	      (sleep (/ diff 1000))))))))

(defun start-move-thread ()
    (close-move-thread)
    (setf (ss :wait-move-thread)
	  (sb-thread:make-thread #'(lambda () (move-stage-fun))
				 :name "stage-mover")))

(defun get-lcos-sequence ()
 (let ((res nil))
   (dolist (es (mapcar #'(lambda (x) (mapcar #'(lambda (y) (getf y :content))
					(getf x :lcos-seq)))
		       (remove-if-not #'(lambda (x) (eq :lcos-display (getf x :type))) (ss :seq))))
 
     (dolist (e es)
       (push e res)))
   (reverse res)))

(defun get-capture-sequence ()
 (remove-if-not #'(lambda (x) (eq :capture (getf x :type))) (ss :seq)))

(defparameter *img-time* nil)

#+nil
(let ((show-on-screen t))
  (unless show-on-screen 
    (setf *do-capture* nil)
    (setf *do-display-queue* nil)
    (sleep .1)
    (setf *do-capture* t)
    (clara::abort-acquisition )
    (clara::prepare-acquisition))
  
  (setf *line* (sb-concurrency:make-queue :name 'picture-fifo))

  (prepare-stack-acquisition)
  (let ((phases (ss :phases)))
    (dolist (e (get-lcos-sequence))     
      (unless (eq e :dark)
	(lcos (format nil "qgrating-disk 425 325 200 ~d ~d 4" 
		      e phases))
	(sleep .001))
      (lcos "qswap")
      (sleep .001))
   (sleep .2))

  (let ((img-array (make-array (length (get-capture-sequence))))
	(img-time (make-array (length (get-capture-sequence)))))
    (lcos "toggle-queue 1")
    (setf (ss :set-start-at-next-swap-buffer) t)
    (unless show-on-screen (start-acquisition)) ;; start camera
    
    (start-move-thread)
    
    (unless show-on-screen (let ((count 0))
			     (loop while (and *do-capture*
					      (< count (length img-array))) do
				  (capture)
				  (loop for i below (sb-concurrency:queue-count *line*) do
				       (setf (aref img-array count) (sb-concurrency:dequeue *line*)
					     (aref img-time count) (get-internal-real-time))
				       (incf count)))
			     (abort-acquisition)
			     (free-internal-memory))
	    (defparameter *img-array* img-array)
	    (defparameter *img-time* img-time)))

  (setf *do-display-queue* t))

#+nil ;; store images
(dotimes (i (length *img-array*))
  (vol::write-pgm-transposed 
   (format nil "/dev/shm/o~4,'0d.pgm" i)
   (vol:normalize-2-sf/ub8
    (vol:convert-2-ub16/sf-mul (aref *img-array* i)))))

(defun draw-moves ()
  (flet ((vline (x)
	   (with-primitive :lines
	     (vertex x 0)
	     (vertex x 80))))
   (with-pushed-matrix 
     (scale .19 1 1)
     (color 1 1 1)
     (vline (- (get-internal-real-time) (ss :start)))
     (color .3 1 .3)
     
     (dolist (e (ss :real-moves))
       (vline e))
     
     (when *img-time*
       (color .9 .2 .2)
       (dotimes (i (length *img-time*))
	 (vline (- (aref *img-time* i) (ss :start)))))

     (translate 0 20 0)
     (dolist (e (ss :seq))
       (case (getf e :type)
	 (:capture
	  (apply #'color (case (getf e :content)
			   (:dark (list .4 .4 .4))
			   (0 (list .9 .1 .1))
			   (1 (list 0 .7 0))
			   (2 (list .2 .2 1))))
	  (rect (getf e :start) 0 (getf e :end) 20))
	 (:stage-move
	  (color 1 1 1)
	  (rect (getf e :start) 21 (getf e :end) 41))))
)))

#+nil 
(sb-thread:make-thread 
 #'(lambda ()  ;; CAPTURESECTION
     (clara::abort-acquisition )
     (setf *do-display-queue* nil)

     (let* ((phases 3)
	    (slices 100)
	    (start-position (focus:get-position))
	    (sequence nil) ;; this will contain the state of each image
	    ;; the first image is a dark image
	    (img-number (+ 1 (* slices (+ 1 phases))))
	    (img-array (make-array img-number))
	    (img-time (make-array img-number)))
       (flet ((slice-z (i)
		(+ (* 20 (/ i (1- slices))) 
		   start-position)))
	   (setf *line* (sb-concurrency:make-queue :name 'picture-fifo))
	 
	 (clara::prepare-acquisition)
	 
	 (push (make-slice start-position :dark) sequence)
	 
	 (dotimes (j slices)
	   (push (make-slice (slice-z j) :dark) sequence)
	   (dotimes (k 2)  
	     ;; dark image for each slice (to give stage time to settle)
	     (lcos "qswap"))
	   (dotimes (i phases)
	     (push (make-slice (slice-z j) i) sequence)
	     (dotimes (k 2)
	       #+nil
	       (lcos "qdisk 225 225 200")
	      
	       (lcos (format nil "qgrating-disk 425 325 200 ~d ~d 4" 
			     (mod i phases) phases))
	       (sleep .001)
	       (lcos "qswap")
	       (sleep .001))))
	 (sleep .4)
	 (lcos "toggle-queue 1")

	 (setf *sequence* (reverse sequence))
	 
	 (defparameter *move-time* (make-array (1- slices)))

	 (sb-thread:make-thread 
	  #'(lambda ()
	      ;; the stage doesn't need to be moved for the first slice
	      (sleep (* (+ 2.5 phases) .033298)) 
	      ;; wait for two dark images and the first phase images to finish
	      ;; also wait one frame of the dark images
	      (dotimes (i (1- slices))
		(focus:set-position (slice-z i))
		(setf (aref *move-time* i) (get-internal-real-time))
		(sleep (* (1+ phases) (* .98 .033298)))))
	  :name "focus-mover")       


	 (start-acquisition) ;; start camera
	 ;; the first captured frame doesn't have any lcos image
	 ;; it can be used as a dark frame

	 (let ((count 0))
	   (loop while (and *do-capture*
			    (< count (length img-array))) do
		(capture)
		(loop for i below (sb-concurrency:queue-count *line*) do
		     (setf (aref img-array count) (sb-concurrency:dequeue *line*)
			   (aref img-time count) (get-internal-real-time))
		     (incf count)))
	   (abort-acquisition)
	   (free-internal-memory)))

       (defparameter *bla* img-array)
       (defparameter *bla-time* img-time)
       (focus:set-position start-position)       
       
       #+nil
       (dotimes (i (length img-array))
	 (vol::write-pgm-transposed (format nil "/dev/shm/01_~3,'0d.pgm" i)
				    (vol:normalize-2-sf/ub8 
				     (vol:convert-2-ub16/sf-mul 
				      (aref img-array i)))))
       #+nil (section-array img-array))
     (setf *do-display-queue* t))
 :name "capture")

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
    ;;(gl:draw-buffer :back)
    ;(clear-color .1 0 0 1)
    (gl:clear :color-buffer-bit)
    
    (let ((c (sb-concurrency:queue-count *line*)))
     (unless (or (= 0 c) (= 1 c))
       (format t "*~a*" c)))
    (when *do-display-queue*
     (loop for e below (sb-concurrency:queue-count *line*) do
	  (let ((e (sb-concurrency:dequeue *line*))
		(p (mod count 5)))
	    (when e
	      (gl:with-pushed-matrix
		(let* ((tex (make-instance 'gui::texture16 :data e
					   :scale 102s0 :offset 0.0077s0
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
	  (incf count)))
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
	     (gui:destroy tex))))))
    (draw-moves)
    (when (ss :set-start-at-next-swap-buffer)
      (setf (ss :set-start-at-next-swap-buffer) nil
	    (ss :start) (get-internal-real-time))))

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
		(sb-concurrency:enqueue a *line*)))))))))

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
(let ((x 700)
      (y 100))
  (progn
    (setf gui::*kill-window* t)
    (sleep .1)
    (setf gui::*kill-window* nil))
  (sb-posix:setenv "DISPLAY" ":0" 1)
  (sb-thread:make-thread
   #'(lambda ()
       (gui:with-gui ((- 1280 x) 700 x y)
	 (draw-screen)))
   :name "camera-display"))
#+nil
(progn
    (setf gui::*kill-window* t)
    (sleep .1)
    (setf gui::*kill-window* nil))


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


;; echo quit > /proc/`ps aux|grep er/glfw|grep -v grep |awk '{print $2}'`/fd/0
;; echo quit > /proc/`ps aux|grep mma-cmd|grep -v grep |awk '{print $2}'`/fd/0