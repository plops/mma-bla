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
  (mma:load-white :radius 1.0 :pic-number 8)
  #+nil (mma:load-concentric-circles :n 12)
  #+nil (mma:load-disks2 :n 3)
  (mma:begin))
#+nil
(mma:uninit)
#+nil
(mma:select-pictures 7 :ready-out-needed t)

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

(defvar *section-im* nil)

;;; DRAW INTO OPENGL WINDOW (for LCOS and camera view)
(let* ((white-width 10)
       (phases 3)
       (colors 3) 
       (a (make-array (* colors phases white-width) :element-type '(unsigned-byte 8)))
       (phase 2)
       
       (im-scale 60s0)
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

  (defun obtain-sectioned-slice ()
    (when clara:*im*
      (destructuring-bind (w h) (array-dimensions clara:*im*)
	(let ((phase-im (make-array (list phases w h) :element-type '(signed-byte 16)))
	      #+nil (square-im (make-array (list w h) :element-type '(signed-byte 32))))
	  (dotimes (p phases)
	   (change-phase p)
	   (sleep .5)
	   (clara:wait-for-image-and-copy)
	   (dotimes (j h)
	     (dotimes (i w)
	       (setf (aref phase-im p i j) (aref clara:*im* i j)))))
	  (dotimes (j h)
	    (dotimes (i w)
	      (let* ((v (aref phase-im 0 i j))
		     (ma v)
		     (mi v))
		(loop for p from 1 below phases do
		     (let ((v (aref phase-im 0 i j)))
		       (setf mi (min mi v)
			     ma (max ma v)))
		     #+nil (let ((q (- (aref phase-im phase i j) 
				       (aref phase-im (mod (1+ phase) phases) i j))))
			     (incf (aref square-im i j) 
				   (* q q))))
		(setf (aref *section-im* i j) (- ma mi)))
	      #+nil (setf (aref *section-im* i j) (floor (sqrt (aref square-im i j))))))))))

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
				 :scale 300s0 :offset 0s0
				 )))
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
(sb-thread:make-thread #'(lambda () (obtain-sectioned-slice)))


#+nil
(gui:with-gui (1400 300)
  (draw-screen))
