(in-package :gui)

;; 2d texture for 8bit camera image
(defclass texture ()
  ((object :accessor object :initarg :object :initform 0 :type fixnum)))

(defmethod initialize-instance :after ((tex texture) &key data 
				       ;(offset .3s0) (scale 30s0)
				       )
  (declare ((simple-array (unsigned-byte 8) 2) data))
  (let ((target :texture-2d))
    (with-slots (object) tex
      (setf object (first (gen-textures 1)))
      (bind-texture target object)
      (tex-parameter target :texture-min-filter :linear)
      (tex-parameter target :texture-mag-filter :linear)
      (destructuring-bind (h w) (array-dimensions data)
	(sb-sys:with-pinned-objects (data)
	  (let* ((data1 (sb-ext:array-storage-vector data))
		 (data-sap (sb-sys:vector-sap data1)))
	    (with-pushed-matrix 
	      (tex-image-2d target 0 
			    :luminance w h 0 :luminance
			    :unsigned-byte data-sap))))))))

(defmethod destroy ((tex texture))
  (delete-textures (list (object tex))))

(defmethod bind ((tex texture))
  (bind-texture :texture-2d (object tex)))

(defmethod update ((tex texture) &key data)
  (declare ((simple-array (unsigned-byte 16) 2) data))
  (bind tex)
  (destructuring-bind (w h) (array-dimensions data)
    (let* ((data1 (sb-ext:array-storage-vector data))
	   (data-sap (sb-sys:vector-sap data1)))
      (tex-sub-image-2d :texture-2d 0 0 0 w h
			:luminance :unsigned-short data-sap))))

(defmethod draw ((self texture) &key
		 (x 0f0) (y 0f0) 
		 (w 1920f0) (h 1080f0)
		 (wt w) (ht h))
  (declare (single-float x y w h wt ht))
  (let ((target :texture-2d))
    (with-slots ((obj object)) self
      (bind self)
      (enable target)
      (color 1 1 1)
      (let ((q 1 #+nil(/ h w)))
	(with-primitive :quads
	  (tex-coord 0 0)(vertex x y)
	  (tex-coord wt 0)(vertex w y)
	  (tex-coord wt ht)(vertex w (* q h))
	  (tex-coord 0 ht)(vertex x (* q h))))
      (disable target))))


(defmacro def-vec-funcs (&rest names)
  `(progn
     ,@(loop for name in names collect
	    (let ((name-v (alexandria:format-symbol :gui "~a-V" name)))
	      `(defun ,name-v (vec)
		 (declare (vec vec)
			 (values null &optional))
		(,name (vec-x vec) (vec-y vec) (vec-z vec))
		nil)))))

(def-vec-funcs vertex tex-coord translate normal scale)

(defvar circle-points
  (let* ((n 37)
         (ps (make-array (+ n 2) :element-type 'vec
                         :initial-element (v))))
    (declare (fixnum n)
             ((simple-array vec 1) ps))
    (setf (aref ps 0) (v))
    (dotimes (i n)
      (let ((arg (* 2d0 pi i (/ 1d0 n))))
        (declare ((double-float 0d0 6.3d0) arg))
        (setf (aref ps (1+ i)) (make-vec (cos arg) (sin arg)))))
    (setf (aref ps (1+ n)) (aref ps 1))
    ps))

(declaim (type (simple-array vec 1) circle-points))

(defun draw-circle ()
  "Draw circle with radius 1."
  (dotimes (i (length circle-points))
    (vertex-v (aref circle-points i)))
  nil)

#+nil
(defmethod draw ((disk lens:disk))
  (declare (values null &optional))
  (with-slots ((c lens::center) (r lens::radius)) disk
   (with-pushed-matrix
     (translate-v c)
     (scale r r r)
     (with-primitive :triangle-fan
       (draw-circle)))))

(defclass texture-luminance-ub8 ()
  ((dimensions :accessor dimensions :initarg :dimensions :initform '(0 0 0)
	       :type cons)
   (object :accessor object :initarg :object :initform 0 :type fixnum)
   (target :accessor target :initarg :target 
	   :initform :texture-3d :type fixnum)))

(defmethod initialize-instance :after ((tex texture-luminance-ub8) &key data)
  (let ((rank (etypecase data
		((simple-array (unsigned-byte 8) 2) 2)
		((simple-array (unsigned-byte 8) 3) 3))))
    (with-slots (dimensions object target) tex
      (setf object (first (gen-textures 1))
	    dimensions (array-dimensions data)
	    target (ecase rank
		     (2 :texture-rectangle-nv)
		     (3 :texture-3d)))
      (bind-texture target object)
      (tex-parameter target :texture-min-filter :linear)
      (tex-parameter target :texture-mag-filter :linear)
      (sb-sys:with-pinned-objects (data)
	(let* ((data1 (sb-ext:array-storage-vector data))
	       (data-sap (sb-sys:vector-sap data1)))
	  (ecase rank
	    (2 (tex-image-2d target 0 :luminance 
			     (array-dimension data 1) ;; x
			     (array-dimension data 0) 0 :luminance
			     :unsigned-byte data-sap))
	    (3 (tex-image-3d target 0 :luminance
			     (array-dimension data 2) ;; x
			     (array-dimension data 1) ;; y
			     (array-dimension data 0) 0 :luminance
			     :unsigned-byte data-sap))))))))

(defmethod draw-xz ((tex texture-luminance-ub8) x+ x- z+ z-
		    &key (y 0d0) (ty 0d0))
  "Draw a quad with a texture. If the texture is 3d, an xz-plane at y
  is selected. The parameter x+, x-, z+, z- and y define vertex
  positions of the quad. In case of a 3d texture ty choses the y
  texture coordinate with value from 0 .. 1."
  (with-slots (target dimensions) tex
    (bind tex)
    (gl:enable target)
    (let* ((texcoords 
	    (ecase target
	      (:texture-rectangle-nv
	       (destructuring-bind (yy xx) dimensions
		 (let ((y (* 1d0 yy)) 
		       (x (* 1d0 xx)))
		   (list (make-vec x y)     (make-vec 0d0 y)
			 (make-vec 0d0 0d0) (make-vec x 0d0)))))
	      (:texture-3d (list (make-vec 1d0 ty 1d0) (make-vec 0d0 ty 1d0)
				 (make-vec 0d0 ty 0d0) (make-vec 1d0 ty 0d0)))))
	   (vertexs (list (make-vec x+ y z-)
			  (make-vec x- y z-)
			  (make-vec x- y z+)
			  (make-vec x+ y z+))))
      (gl:with-primitive :quads
	(loop for v in vertexs and c in texcoords do
	   (tex-coord-v c) (vertex-v v))))
    (gl:disable target)))



(defun draw-axes ()
  (gl:line-width 3)
  (gl:with-primitive :lines
    (gl:color 1 0 0 1) (gl:vertex 0 0 0) (gl:vertex 1 0 0)
    (gl:color 0 1 0 1) (gl:vertex 0 0 0) (gl:vertex 0 1 0)
    (gl:color 0 0 1 1) (gl:vertex 0 0 0) (gl:vertex 0 0 1)))


(defun draw-wire-box (start end)
  (gl:with-pushed-matrix
    (translate-v start)
    (scale-v (v- end start))
    (gl:translate .5 .5 .5)
    (glut:wire-cube 1)))

