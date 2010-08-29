(in-package :frontend)

(defclass sphere-model (raytrace:sphere-algebraic-model)
  ((spheres :accessor spheres :initarg :index-spheres
	    :initform (make-array (list 0 0 0)
				  :element-type '(complex psf:my-float))
	    :type (simple-array (complex psf:my-float) 3))))

(defmethod initialize-instance :after ((model sphere-model) 
				       &key (filename-glob "/home/martin/tmp/xa*.pgm") (radius-pixels 12d0))
  (let ((radius-sf (coerce radius-pixels 'single-float)))
   (with-slots ((dx raytrace::dx)
		(dy raytrace::dy)
		(dz raytrace::dz)
		(immersion-index raytrace::immersion-index)
		(dimensions raytrace::dimensions)
		(centers raytrace::centers)
		(radii-mm raytrace::radii-mm)
		(centers-mm raytrace::centers-mm) spheres) model
     (unless centers ;; read from files if centers aren't given
       (let* ((stack-byte (read-stack filename-glob))
	      (dims (array-dimensions stack-byte))
	      (stack (make-array dims :element-type '(complex my-float))))
	 (destructuring-bind (z y x) dims
	   (do-region ((k j i) (z y x))
	     (setf (aref stack k j i) (complex (+ (* #.(coerce .43745 'my-float) k)
						  (aref stack-byte k j i)))))
	   ;; find centers of cells by convolving with sphere, actually an
	   ;; oval because the z resolution is smaller than the transversal
	   (let* ((conv (convolve-circ 
			 stack 
			 (fftshift
			  (#.(cond ((subtypep 'my-float 'single-float) 'draw-oval-csf)
				   ((subtypep 'my-float 'double-float) 'draw-oval-cdf))
			     radius-sf z y x))))
		  (cv (convert conv 'sf 'realpart))
		  (rcenters nil))
	     (do-region ((k j i) ((- z 3) (- y 1) (- x 1)) (6 1 1))
	       (macrolet ((c (a b c)
			    `(aref cv (+ k ,a) (+ j ,b) (+ i ,c))))
		 (let ((v (c 0 0 0)))
		   (when (and (< (c  0  0 -1) v) (< (c  0  0  1) v)
			      (< (c  0 -1  0) v) (< (c  0  1  0) v)
			      (< (c -1  0  0) v) (< (c  1  0  0) v))
		     (push (make-vec-i :z k :y j :x i) rcenters)))))
	     (setf centers (nreverse rcenters)
		   dimensions dims)))))
     (destructuring-bind (z y x) dimensions
       (setf radii-mm (loop for i below (length centers) collect
			   (* 1d-3 immersion-index dx radius-pixels))
	     centers-mm (mapcar #'(lambda (x) (let ((s (* 1d-3 immersion-index)))
						(make-vec (* s dx (vec-i-x x))
							  (* s dy (vec-i-y x))
							  (* s dz (vec-i-z x)))))
				centers)
	     spheres (draw-ovals radius-sf centers z y x))))))

(defmethod print-object ((model sphere-model) stream)
  (with-slots (dimensions centers dx dy dz) model
   (format stream "<sphere-model ~dx~dx~d ~3,1fx~3,1fx~3,1f um^3 ~d nuclei>" 
	   (elt dimensions 2) (elt dimensions 1) (elt dimensions 0)
	   (* dx (elt dimensions 2)) (* dy (elt dimensions 1)) (* dz (elt dimensions 0))
	   (length centers))))

#+nil
(make-instance 'sphere-model)

(defclass sphere-model-angular (sphere-model)
  (;; each nucleus drawn with its index+1
   (index-spheres :accessor index-spheres :initarg :index-spheres
		  :initform (make-array (list 0 0 0) :element-type 'my-float)
		  :type (simple-array my-float 3))))

(defmethod initialize-instance :after ((model sphere-model-angular) &key)
  (with-slots (dimensions centers index-spheres) model
   (destructuring-bind (z y x) dimensions
     (setf index-spheres (draw-indexed-ovals 12s0 centers z y x)))))

(defmethod print-object ((model sphere-model-angular) stream)
  (with-slots (dimensions centers dx dy dz) model
   (format stream "<sphere-model-angular ~dx~dx~d ~3,1fx~3,1fx~3,1f um^3 ~d nuclei>" 
	   (elt dimensions 2) (elt dimensions 1) (elt dimensions 0)
	   (* dx (elt dimensions 2)) (* dy (elt dimensions 1)) (* dz (elt dimensions 0))
	   (length centers))))

#+nil
(make-instance 'sphere-model-angular)

(defun make-test-model ()
  (declare (values sphere-model-angular &optional))
  (let* ((centers nil)
	 (nx 10)
	 (ny 7)
	 (z 20)
	 (dx 30))
    (do-region ((i j) (nx ny))
      (let ((x (+ (floor dx 2) (* dx i)))
	    (y (+ (floor dx 2) (* dx j))))
	(unless (and (= i 4) (= j 3))
	  (push (make-vec-i :x x :y y :z z)
		centers))))
    (push (make-vec-i :x 130 :y 100 :z 10) centers)
    (make-instance 'sphere-model-angular
		   :dimensions '(34 206 296)
		   :centers centers)))


#+nil
(make-test-model)