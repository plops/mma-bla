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
     (sb-sys:with-pinned-objects (data)
       (let* ((data1 (sb-ext:array-storage-vector data))
	      (data-sap (sb-sys:vector-sap data1)))
	 (tex-image-2d target 0 :rgb (length data) 1 0 :rgb
		       :unsigned-byte data-sap))))))

(defmethod destroy ((tex grating))
  (delete-textures (list (object tex))))

(defmethod bind-tex ((tex grating))
  (bind-texture (target tex) (object tex)))

(defmethod draw ((self bild) &optional
		 (x 0f0) (y 0f0) 
		 (ww 1920f0) (hh 1080f0))
  (declare (single-float x y ww hh))
  (with-slots ((w width)
               (h height)
               (obj texture-object)
               (target texture-target))
      self
    (bind-tex grating)
    (enable target)
    (color 1 1 1)
    (let ((q 1 #+nil(/ h w)))
     (with-primitive :quads
       (tex-coord 0 0)(vertex x y)
       (tex-coord w 0)(vertex ww y)
       (tex-coord w h)(vertex ww (* q hh))
       (tex-coord 0 h)(vertex x (* q hh))))
    (disable target)))

(defun draw-axes ()
  (gl:line-width 3)
  (gl:with-primitive :lines
    (gl:color 1 0 0 1) (gl:vertex 0 0 0) (gl:vertex 1 0 0)
    (gl:color 0 1 0 1) (gl:vertex 0 0 0) (gl:vertex 0 1 0)
    (gl:color 0 0 1 1) (gl:vertex 0 0 0) (gl:vertex 0 0 1)))

