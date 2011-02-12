(in-package :gui)

;; 2d texture for 16bit camera image
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




(defun draw-axes ()
  (gl:line-width 3)
  (gl:with-primitive :lines
    (gl:color 1 0 0 1) (gl:vertex 0 0 0) (gl:vertex 1 0 0)
    (gl:color 0 1 0 1) (gl:vertex 0 0 0) (gl:vertex 0 1 0)
    (gl:color 0 0 1 1) (gl:vertex 0 0 0) (gl:vertex 0 0 1)))

