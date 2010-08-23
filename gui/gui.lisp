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

(defmethod set-view ((w fenster) &optional (2d nil))
  (load-identity)
  (viewport 0 0 (width w) (height w))
  (matrix-mode :projection)
  (load-identity)
  (if 2d
      (ortho 0 (width w) (height w) 0 -1 1)
      (progn
	(glu:perspective 30 (/ (width w) (height w)) .01 100)
	(glu:look-at 20 30 10
		 0 0 0
		 0 0 1)))
  (matrix-mode :modelview)
  (load-identity))

(defmethod reshape ((w fenster) x y)
  (setf (width w) x
	(height w) y)
  (set-view w))

(defmethod display-window :before ((w fenster))
  (set-view w))

(defmethod display ((w fenster))
  (clear :color-buffer-bit :depth-buffer-bit)
  (load-identity)
  
  (funcall (draw-func w))

  (swap-buffers)
  (sleep (/ 30))
  (post-redisplay))

(defmethod passive-motion ((w fenster) x y)
  (setf (aref (cursor-position w) 0) x
	(aref (cursor-position w) 1) (- (height w) y)))

(defmethod keyboard ((w fenster) key x y)
  (case key
    (#\Esc (destroy-current-window))))

(defmacro with-gui (&body body)
  `(display-window 
    (make-instance 'gui:fenster
		   :mode '(:double :rgb :depth)
		   :draw-func #'(lambda ()
				  ,@body))))


