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

(defmethod set-view ((w fenster) &key (2d nil) (view-center (v))
		     (fov 30d0))
      (load-identity)
      (viewport 0 0 (width w) (height w))
      (matrix-mode :projection)
      (load-identity)
      (if 2d
	  (ortho 0 (width w) (height w) 0 -1 1)
	(let ((x (vec-x view-center))
	      (y (vec-y view-center))
	      (z (vec-z view-center)))
	  (glu:perspective fov (/ (width w) (height w)) .01 100)
	  (glu:look-at 20 30 10
		       x y z
		       0 0 1)))
      (matrix-mode :modelview)
      (load-identity))

(let ((field-of-view (list 30d0))
      (view-center (list (v))))
  (defun update-view (&key (center-list view-center)
		      (fov-list field-of-view))
    "This function can be called from outside an OpenGL context. The
supplied lists will be used to set the viewpoint with glLookAt. It is
consumed from the front."
    (declare (cons center-list fov-list))
    (assert (equal (type-of (car center-list))
		   (type-of (v))))
    (assert (numberp (car fov-list)))
    (setf view-center center-list
	  field-of-view fov-list))

  (defmethod ensure-uptodate-view ((w fenster))
    (labels ((pop-until-last (l)
	       (if (cdr l)
		   (pop l)
		   (car l))))
      (set-view w :view-center (pop-until-last view-center)
		:fov (pop-until-last field-of-view))))
  
  (defmethod display ((w fenster))
    (ensure-uptodate-view w)
    (clear :color-buffer-bit :depth-buffer-bit)
    (load-identity)
    
    (funcall (draw-func w))
    
    (swap-buffers)
    (sleep (/ 20))
    (post-redisplay)))

(defmethod reshape ((w fenster) x y)
  (setf (width w) x
	(height w) y)
  (ensure-uptodate-view w))

(defmethod display-window :before ((w fenster))
  (ensure-uptodate-view w))

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


