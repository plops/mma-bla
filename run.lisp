#.(require :gui)
#.(require :clara)
#.(require :mma)
#.(require :focus)


;;; CLARA CAMERA
#+nil
(progn
  (clara:init-fast :exposure-s 0.016s0 :width 300 :height 256 
		   :x -100 :y 80 :fast-adc t :external-trigger t)
  (clara:wait-for-image-and-copy)
  (clara:status))
#+nil
(clara:status)
#+nil
(clara:stop)
#+nil
(clara:uninit)
#+nil
clara:*im*


;;; MMA OVER NETWORK (run ifconfig eth1 192.168.0.1)
#+nil
(progn
  (mma:init))
#+nil
(progn
  (mma::end)
  (mma:load-white :radius .3 :pic-number 8)
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
#+nil
(focus:set-position (1+ (focus:get-position)))
#+nil
(focus:disconnect)

;;; DRAW INTO OPENGL WINDOW (for LCOS and camera view)
(let* ((white-width 6)
       (phases 3)
       (colors 3) 
       (a (make-array (* colors phases white-width) :element-type '(unsigned-byte 8)))
       (phase 2)
       (offset (* phase white-width colors)))
  (dotimes (i white-width)
    (setf (aref a (+ offset (+ 0 (* colors i)))) #b01010100 ;; disable first bit plane
	  (aref a (+ offset (+ 1 (* colors i)))) #b01010101 ;; show only every other bit plane
	  (aref a (+ offset (+ 2 (* colors i)))) #b01010101)) 
  (defun draw-screen ()
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer-bit)
    (when clara:*im*
      (clara:wait-for-image-and-copy)
      (let ((tex (make-instance 'gui::texture :data clara:*im* 
				:scale 120s0 :offset .1s0
			;;	:scale 40s0 :offset 0.76s0
				)))
       (destructuring-bind (w h) (array-dimensions clara:*im*)
	 (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))))
    (gl:translate 800 0 0)
    (let ((repetition 40f0))
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
(gui:with-gui (1200 300)
  (draw-screen))
