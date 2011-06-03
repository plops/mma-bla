#.(progn
    (sb-posix:setenv "DISPLAY" ":0" 1)
    (sb-ext:run-program "/usr/bin/xset" '("s" "off"))
    (sb-ext:run-program "/usr/bin/xset" '("-dpms"))

    (setf asdf:*central-registry* (list "/home/martin/0505/mma/"))
    (ql:quickload "cl-opengl")
    (require :gui)
    (require :clara)
   ; (require :mma)
    (require :focus)) 
(defpackage :run-gui
	(:use :cl :clara :gl))
(in-package :run-gui)

#+nil
(mma::parse-status-bits #x4c000)

#+nil
(focus:connect)
#+nil
(focus:get-position)
#+nil
(focus:set-position
 (+ (focus:get-position) -10.))


#+nil
(mma::set-stop-mma)
#+nil
(mma::set-start-mma)
#+nil
(mma:write-data (mma-spot 128 128 :kernel 7))
#+nil
(mma:write-data (mma-white))
#+nil
(mma:status)
#+nil
(mma::set-power-off)
#+nil
(mma::disconnect)
#+nil
(capture)

(defun sum (img)
  (destructuring-bind (h w) (array-dimensions img)
    (let ((sum 0))
      (dotimes (j h)
	(dotimes (i w)
	  (incf sum (aref img j i))))
      sum)))


#+nil
(defparameter *scan5*
 (let ((res ()))
   (loop for j below 256 by 4 do
	(loop for i below 256 by 4 do
	     (mma:write-data (mma-spot i j :kernel 7))
	     (capture)
	     (let ((s (list i j (sum *line*))))
	       (format t "~a~%" s)
	       (push s  res))))
   res))
#+nil
(vol:write-pgm "/dev/shm/o.pgm"
 (let* ((d *scan5*)
	(ma (* .1 (reduce #'max d :key #'third)))
       (mi (reduce #'min d :key #'third))
       (b (make-array (list (/ 128 8) (/ 128 8))
		      :element-type '(unsigned-byte 8))))
   (dolist (e d)
     (destructuring-bind (i j val) e
       (setf (aref b (floor j 8) (floor i 8))
	     (max 0 (min 255 (floor (* 255 (/ (- val mi)
					(- ma mi)))))))))
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


(defmacro with-lcos-to-cam (&body body)
  `(let* ((s 1.129781s0)
	  (sx  s)
	  (sy  (- s))
	  (phi 1.3154879)
	  (cp (cos phi))
	  (sp (sqrt (- 1s0 (* cp cp))))
	  (tx 1086.606s0)
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

(defmacro with-cam-to-lcos ((&optional (x 0s0) (y 0s0)) &body body)
  `(let* ((s .885090144449)
	  (sx  s)
	  (sy  (- s))
	  (phi 1.3154879)
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

(defun draw-circle (x y r)
  (gl:with-primitive :line-loop
   (let ((n 37))
     (loop for i from 0 below n do
	  (let ((arg (* i (/ n) 2 (coerce pi 'single-float)))) 
	    (gl:vertex (+ x (* r (cos arg))) (+ y (* r (sin arg)))))))))

(defun draw-disk (x y r)
  (gl:with-primitive :triangle-fan
   (let ((n 37))
     (gl:vertex x y)
     (loop for i from 0 below n do
	  (let ((arg (* i (/ (1- n)) 2 (coerce pi 'single-float)))) 
	    (gl:vertex (+ x (* r (cos arg))) (+ y (* r (sin arg)))))))))

(defparameter *do-capture* t)

#+nil
(sb-thread:make-thread 
 #'(lambda () 
     (loop while *do-capture* do
	(capture)
	#+nil
	(sleep .01)))
 :name "capture")

(progn
 (defparameter *t8* nil)
 (defparameter *dark* nil)
 (defparameter *white* nil)
 (defparameter *line* nil))
#+nil
(change-capture-size (+ 380 513) (+ 64 513) 980 650)
#+nil
(change-target 865 630 12 :ril 200s0)
(let* ((px 900s0) (py 600s0) (pr 300s0)
       (px-ill px) (py-ill py) (pr-ill pr)
       (w 1392)
       (h 1040)
       (x 1)
       (y 1)
       (new-size nil))
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
  (defun change-capture-size (xx yy ww hh)
    (setf w ww
	  h hh
	  x xx
	  y yy
	  new-size t))
  (change-capture-size 1 1 1392 1040)
  
  (defun draw-screen ()
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer-bit)
    (sleep (/ 60))
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
    (gl:color 1 .4 0)
    (gl:line-width 4)
    (draw-circle px py pr)
    (gl:with-pushed-matrix
      (%gl:color-3ub #b00111100 #+nil #b01111111 0 0 ;255 255
		     )
      (gl:translate 0 1024 0)
      (with-cam-to-lcos (0 1024)
	(draw-disk px-ill py-ill pr-ill))))

  (defun capture ()
    (when new-size
      (check
	(set-image 1 1 x w y h))
      (setf new-size nil))
   (defparameter *line*
     (let* ((img (make-array (list (- h (1- y)) (- w (1- x)))
			     :element-type '(unsigned-byte 16)))
	    (img1 (sb-ext:array-storage-vector img))
	    (sap (sb-sys:vector-sap img1)))
       (progn
	 (start-acquisition)
	 (loop while (not (eq 'clara::DRV_IDLE
			      (lookup-error (val2 (get-status)))))
	    do
	    (sleep .003))
	 (sb-sys:with-pinned-objects (img)
	   (get-acquired-data16 sap (length img1)))
	 (check
	   (free-internal-memory)))
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
	   (dotimes (i (length b1))
	     (let ((v (if t		;(< 800 (aref w1 i)) 
			  (min 255 
			       (max 0 
				    (floor (aref l1 i) 50)
				    #+nil (floor (* 255 (- (aref l1 i) 
							   (aref d1 i)))
						 (- (aref w1 i)
						    (aref d1 i)))))
			  0)))
	       (if (< 1 v)
		   (setf (aref b1 i) 
			 v)
		   (let ((yy (floor i w))
			 (xx (mod i w)))
		     (cond ((or (= 0 (mod (+ y yy) 500))
				(= 0 (mod (+ x xx) 500)))
			    (setf (aref b1 i) 255))
			   ((or (= 0 (mod (+ y yy) 100))
				(= 0 (mod (+ x xx) 100)))
			    (setf (aref b1 i) 80))))))))
	 b)))))
#+nil
(capture)




#+nil
(sb-thread:make-thread 
 #'(lambda ()
     (gui:with-gui (1280 (* 2 1024))
       (draw-screen)))
 :name "display-gui")

