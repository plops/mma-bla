(in-package :gui)

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

(defmethod destroy ((tex texture-luminance-ub8))
  (delete-textures (list (object tex))))

(defmethod bind-tex ((tex texture-luminance-ub8))
  (bind-texture (target tex) (object tex)))

(defun draw-axes ()
  (gl:line-width 3)
  (gl:with-primitive :lines
    (gl:color 1 0 0 1) (gl:vertex 0 0 0) (gl:vertex 1 0 0)
    (gl:color 0 1 0 1) (gl:vertex 0 0 0) (gl:vertex 0 1 0)
    (gl:color 0 0 1 1) (gl:vertex 0 0 0) (gl:vertex 0 0 1)))

