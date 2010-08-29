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

(let ((view-center (list (v))))
  (defun set-view-center (vec-list)
    "This function can be called from outside an OpenGL context. The
supplied list will be used to set the viewpoint with glLookAt. It is
consumed from the front."
    (declare (cons vec-list))
    (assert (equal (type-of (car vec-list))
		   (type-of (v))))
    (setf view-center vec-list))
  
    (defmethod set-view ((w fenster) &optional (2d nil) (view-center (v)))
      (load-identity)
      (viewport 0 0 (width w) (height w))
      (matrix-mode :projection)
      (load-identity)
      (if 2d
	  (ortho 0 (width w) (height w) 0 -1 1)
	(let ((x (vec-x view-center))
	      (y (vec-y view-center))
	      (z (vec-z view-center)))
	  (glu:perspective 30 (/ (width w) (height w)) .01 100)
	  (glu:look-at 20 30 10
		       x y z
		       0 0 1)))
      (matrix-mode :modelview)
      (load-identity))
    
    (defmethod ensure-uptodate-view-center ((w fenster))
      (if (cdr view-center)
	  (set-vew w nil (pop view-center))
	  (set-view w nil (car view-center))))
    (defmethod display ((w fenster))
      (ensure-uptodate-view-center w)
      (clear :color-buffer-bit :depth-buffer-bit)
      (load-identity)
      
      (funcall (draw-func w))
      
      (swap-buffers)
      (sleep (/ 20))
      (post-redisplay)))

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

(defmacro with-gui (&body body)
  `(display-window 
    (make-instance 'gui:fenster
		   :mode '(:double :rgb :depth)
		   :draw-func #'(lambda ()
				  ,@body))))


