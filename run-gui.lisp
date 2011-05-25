(sb-posix:setenv "DISPLAY" ":0" 1)

(setf asdf:*central-registry* (list "/home/martin/0505/mma/"))
(ql:quickload "cl-opengl")
(require :gui)
(require :clara)
(defpackage :run-gui
  (:use :cl :clara :gl))
(in-package :run-gui)


#+nil
(let* ((a1 (sb-ext:array-storage-vector *t8*))
       (mi (reduce #'min a1))
       (ma (reduce #'max a1)))
  (list mi ma))

#+nil
(defparameter *t8*
  (let* ((dat *diff*)
	 (b (make-array (array-dimensions dat)
			:element-type '(unsigned-byte 8)))
	(b1 (sb-ext:array-storage-vector b))
	(a1 (sb-ext:array-storage-vector dat))
	(mi 0s0)
	(ma .0009s0))
   (dotimes (i (length b1))
     (setf (aref b1 i) (min 255 (max 0 (floor (* 255 (- (aref a1 i) mi))
					      (- ma mi) )))))
   b))
#+nil
(defparameter *fit*
  (let* ((a (multiple-value-call #'gauss::create 200 200
				 (gauss::do-fit *cut*)))
	 (s (gauss::estimate-amplitude *cut*))
	 (a1 (sb-ext:array-storage-vector a)))
    (dotimes (i (length a1))
      (setf (aref a1 i) (* s (aref a1 i))))
    a))
#+nil
(gauss::do-fit *cut*)
#+nil
(defparameter *diff*
  (let* ((e (make-array (array-dimensions *cut*) :element-type 'single-float))
	 (e1 (sb-ext:array-storage-vector e))
	 (a1 (sb-ext:array-storage-vector *fit*))
	 (c1 (sb-ext:array-storage-vector *cut*)))
    (dotimes (i (length a1))
      (setf (aref e1 i) (expt (- (aref a1 i) (aref c1 i)) 2)))
    e))
#+nil
(defparameter *fit*
  (gauss::create-default 200 :x 82s0 :y 102s0 :sxx (expt 12s0 2)))
#+nil
(let* ((w 200)
       (h 200)
       (x 600)
       (y 400)
       (a (make-array (list h w)
		      :element-type 'single-float)))
  (dotimes (jj h)
    (dotimes (ii w)
      (let* ((i (+ ii x))
	     (j (+ jj y))
	     (v (if (< 1000 (aref *white* j i)) 
		   (/ (* 1s0 (- (aref *line* j i) 
				(aref *dark* j i)))
		      (- (aref *white* j i)
			 (aref *dark* j i)))
		   0s0)))
	(setf (aref a jj ii) (if (< .05 v) v 0s0)))))
 (defparameter *cut* a))



#+nil
(sb-thread:make-thread 
 #'(lambda () 
     (loop
      (capture)
	#+nil
      (sleep .01)))
 :name "capture")


(defparameter *dark* nil)
(defparameter *white* nil)
(defun capture ()
 (defparameter *line*
   (let* ((w 1392)
	  (h 1040)
	  (img (make-array (list h w)
			   :element-type '(unsigned-byte 16)))
	  (img1 (sb-ext:array-storage-vector img))
	  (sap (sb-sys:vector-sap img1)))
     (progn
       (start-acquisition)
       (loop while (not (eq 'clara::DRV_IDLE
			    (lookup-error (val2 (get-status)))))
	  do
	  (sleep .01))
       (sb-sys:with-pinned-objects (img)
	 (get-acquired-data16 sap (length img1)))
       (check
	 (free-internal-memory)))
     img))
 (defparameter *t8*
   (when (and *line* ;*dark* *white* *line*
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
	  (let ((v (if t ;(< 800 (aref w1 i)) 
		       (min 255 
			    (max 0 
				 (floor (aref l1 i) 1.5)
				 #+nil (floor (* 255 (- (aref l1 i) 
						   (aref d1 i)))
					(- (aref w1 i)
					   (aref d1 i)))))
		       0)))
	     (if (< 12 v)
		(setf (aref b1 i) 
		      v)
		(let ((y (floor i w))
		      (x (mod i w)))
		  (cond ((or (= 0 (mod y 500))
			     (= 0 (mod x 500)))
			 (setf (aref b1 i) 255))
			((or (= 0 (mod y 100))
			     (= 0 (mod x 100)))
			 (setf (aref b1 i) 80))))))))
      b))))
#+nil
(capture)

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
  (declare (type single-float x y r))
  (gl:with-primitive :line-loop
   (let ((n 37))
     (loop for i from 0 below n do
	  (let ((arg (* i (/ n) 2 (coerce pi 'single-float)))) 
	    (gl:vertex (+ x (* r (cos arg))) (+ y (* r (sin arg)))))))))

(defun draw-disk (x y r)
  (declare (type single-float x y r))
  (gl:with-primitive :triangle-fan
   (let ((n 37))
     (gl:vertex x y)
     (loop for i from 0 below n do
	  (let ((arg (* i (/ (1- n)) 2 (coerce pi 'single-float)))) 
	    (gl:vertex (+ x (* r (cos arg))) (+ y (* r (sin arg)))))))))

(defparameter *t8* nil)
(let ((a 3))
  #+nil(defun draw-screen ()
	 (gl:clear-color 0 0 0 1)
    )
 #-nil (defun draw-screen ()
   (gl:clear-color 0 0 0 1)
   (gl:clear :color-buffer-bit)
   (sleep .1)
   (gl:line-width 1)
   (gl:color 0 1 1)
   (when *t8*
    (let ((tex (make-instance 'gui::texture :data *t8*)))
      (destructuring-bind (h w) (array-dimensions *t8*)
	(gui:draw tex :w (* 1s0 w) :h (* 1s0 h)
		  :wt 1s0 :ht 1s0))
      (gui:destroy tex)))
   #+nil (gl:color 0 0 0)
   #+nil (gl:rect (+ 1280 0) 0 (+ 1280 1280) 1024)
   (gl:color 1 1 1)
   #+nil(gl:with-pushed-matrix
     (gl:translate 0 1024 0)
    (let ((d 7))
      (dotimes (l 17)
	(dotimes (k 16)
	  (multiple-value-bind (x y)
	      (lcos->camera (vec (+ 340 (* k 20s0)) (+ (* l 20) 600s0) 1s0))
	    (gl:rect (+ 1280 x (- d)) (+ y (- d)) (+ 1280 x d) (+ y d)))))))
   #+nil (gl:with-pushed-matrix 
     (gl:translate 0 0 0)
     (loop for j from 200 below 450 by 50 do
	  (loop for i from 200 below 700 by 50 do
	       
	       (gl:with-pushed-matrix 
		 (multiple-value-bind (x y)
		     (lcos->camera (vec (* 1s0 i) (* 1s0 j) 1s0))
		   (gl:translate x y 0))
		 (let ((x 10))
		   (gl:with-primitive :line-loop
		     (gl:vertex (- x) (- x))
		     (gl:vertex x (- x))
		     (gl:vertex x x)
		     (gl:vertex (- x) x)))))))
   (let ((n0 3) (n 21)
	 (m0 1) (m 29)
	 (r 250s0)
	 (radii 4)
	 (px 623s0) (py 595s0) (pr 50s0))
     ;; optimization by hand:
     ;; start with sx=sy=s=1, phi=0, tx=0, ty=0
     ;; shift big spot ontop of another by changing tx, ty
     ;; flip sign of sy=-s if it needs flipping
     ;; find rotation phi
     ;; adjust scale s
     (gl:color 1 0 0)
     (gl:line-width 3)
     (draw-circle px py pr)
     (with-lcos-to-cam
       

       #+nil (loop for i from n0 below n do
	 (loop for j from m0 below m do
	   (gl:point-size (if (and (= i 0) (= j 0))
			      6 6))
	   (gl:with-primitive :points
	     (gl:vertex (* 50 i) (* 50 j)))))
       #+nil(dotimes (i radii)
	 (draw-circle 440s0 350s0 (* (/ radii) (+ 1 i) r))))
     (gl:line-width 5)
     (gl:with-pushed-matrix
       (gl:color 1 1 1)
       (gl:translate 0 1024 0)
       (with-cam-to-lcos (0 1024)

	 (draw-disk px py pr))
       #+nil(dotimes (i radii)
	 (draw-circle 440s0 350s0 (* (/ radii) (+ 1 i) r)))
       #+nil (loop for i from n0 below n do
	(loop for j from m0 below m do
	  (gl:point-size (if (and (= i 0) (= j 0))
			     4 4))
	  (gl:with-primitives :points
	    (gl:vertex (* 50 i) (* 50 j)))))))
  #+nil (gl:with-pushed-matrix 
     (gl:translate 0 1024 0)
     
     (loop for j from 200 below 450 by 50 do
	  (loop for i from 200 below 700 by 50 do
	       (gl:with-pushed-matrix 
		 (gl:translate i j 0)
		 (let ((x 20))
		   (gl:with-primitive :line-loop
		     (gl:vertex (- x) (- x))
		     (gl:vertex x (- x))
		     (gl:vertex x x)
		     (gl:vertex (- x) x)))))))))



#+nil
(sb-thread:make-thread 
 #'(lambda ()
     (gui:with-gui (1280 (* 2 1024))
       (draw-screen)))
 :name "display-gui")

