(in-package :gui)

(defclass fenster (window)
  ((cursor-position :accessor cursor-position 
		    :initform (make-array 2 :element-type 'fixnum)
		    :type (simple-array fixnum (2)))
   (draw-func :accessor draw-func
	      :initarg :draw-func
	      :initform #'(lambda ()   
			    (with-primitive :lines
			      (color 1 0 0) (vertex 0 0 0) (vertex 1 0 0)
			      (color 0 1 0) (vertex 0 0 0) (vertex 0 1 0)
			      (color 0 0 1) (vertex 0 0 0) (vertex 0 0 1)))
	      :type function)))

(defmethod set-view ((w fenster) &key (2d t))
      (load-identity)
      (viewport 0 0 (width w) (height w))
      (matrix-mode :projection)
      (load-identity)
      (if 2d
	  (ortho 0 (width w) (height w) 0 -1 1)
	  (progn (glu:perspective 40 (/ (width w) (height w)) 3 100)
		 (glu:look-at 20 30 -5
			      0 0 0
			      0 0 1)))
      (matrix-mode :modelview)
      (load-identity))

(defun current-time ()
  (multiple-value-bind (sec usec)
      (sb-ext:get-time-of-day)
    (+ sec (/ usec 1000000))))
#+nil
(current-time)

(let* ((start 0)
       (end 0)
       (count-max 10)
       (count count-max)
       (frame-rate 0))
  (defun measure-frame-rate ()
    (when (= 0 count)
      (setf end (current-time)
	    frame-rate (/ (* 1s0 count-max)
			  (- end start))
	    count count-max
	    start (current-time)))
    (decf count))
  (defun get-frame-rate ()
    frame-rate))


(defmethod display ((w fenster))
  (clear :color-buffer-bit :depth-buffer-bit)
  (load-identity)
  
  (funcall (draw-func w))
  
  (measure-frame-rate)
  (swap-buffers)
  (post-redisplay))

(defmethod reshape ((w fenster) x y)
  (setf (width w) x
	(height w) y)
  (set-view w))

(defmethod display-window :before ((w fenster))
  (set-view w))

(defmethod passive-motion ((w fenster) x y)
  (setf (aref (cursor-position w) 0) x
	(aref (cursor-position w) 1) (- (height w) y)))

(defmethod keyboard ((w fenster) key x y)
  (case key
    (#\Esc (destroy-current-window))))

(defmacro with-gui ((w &optional (h w) (x 0) (y 0)) &body body)
  `(display-window 
    (make-instance 'gui:fenster
		   :mode '(:double :rgb :depth)
		   :width ,w :height ,h
		   :pos-x ,x :pos-y ,y 
		   :draw-func #'(lambda ()
				  ,@body))))
