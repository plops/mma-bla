#.(require :gui)

(let* ((white-width 21)
       (phases 3)
       (colors 3) 
       (a (make-array (* colors phases white-width) :element-type '(unsigned-byte 8)))
       (phase 0)
       (offset (* phase white-width colors)))
  (dotimes (i white-width)
    (setf (aref a (+ offset (+ 0 (* colors i)))) #b00101010 ;; disable first bit plane
	  (aref a (+ offset (+ 1 (* colors i)))) #b10101010
	  (aref a (+ offset (+ 2 (* colors i)))) #b10101010)) 
  (defun draw ()
    (gl:clear-color .4 .2 0 1)
    (gl:clear :color-buffer-bit)
    (gl:translate 12 23 0)
    (let ((repetition 7f0))
      (gui::with-grating (g a)
	(gui::draw g :w (* repetition white-width phases) :h 128.0 :wt repetition)))
    (gl:color 0 0 0)
    (gl:with-primitive :lines
      (gl:color 1 0 0) (gl:vertex 0 0) (gl:vertex (* white-width 9) 0 0) ;; x axis red
      ;; show ruling with a pixel spacing of white-width
      (dotimes (i 10)
	(let ((x (* white-width i))
	      (y (if (/= 0 (mod i 3)) -3 -5)))
	  (gl:vertex x 0)
	  (gl:vertex x y)))
      (gl:color 0 1 0) (gl:vertex 0 1) (gl:vertex 0 100 0) ;; y axis green
      (gl:color 0 0 1) (gl:vertex 0 0 1) (gl:vertex 0 0 100))))

#+nil
(gui:with-gui
  (draw))