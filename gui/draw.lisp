(in-package :gui)

(defclass grating ()
  ((object :accessor object :initarg :object :initform 0 :type fixnum)))

(defmethod initialize-instance :after ((tex grating) &key data)
  (declare ((simple-array (unsigned-byte 8) 1) data))
  (let ((target :texture-2d))
   (with-slots (object) tex
     (setf object (first (gen-textures 1)))
     (bind-texture target object)
     (tex-parameter target :texture-min-filter :nearest)
     (tex-parameter target :texture-mag-filter :nearest)
     (tex-parameter target :texture-wrap-s :repeat)
     (tex-parameter target :texture-wrap-t :repeat)
     (sb-sys:with-pinned-objects (data)
       (let* ((data1 (sb-ext:array-storage-vector data))
	      (data-sap (sb-sys:vector-sap data1)))
	 (tex-image-2d target 0 :rgb (floor (length data) 3) 1 0 :rgb
		       :unsigned-byte data-sap))))))

(defmethod destroy ((tex grating))
  (delete-textures (list (object tex))))

(defmacro with-grating ((grating data) &body body)
  `(let ((,grating (make-instance 'gui::grating :data ,data)))
     ,@body
     (gui::destroy ,grating)))

(defmethod bind-tex ((tex grating))
  (bind-texture :texture-2d (object tex)))

(defmethod draw ((self grating) &key
		 (x 0f0) (y 0f0) 
		 (w 1920f0) (h 1080f0)
		 (wt 1f0) (ht 1f0))
  (declare (single-float x y w h))
  (let ((target :texture-2d))
   (with-slots ((obj object)) self
     (bind-tex self)
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

