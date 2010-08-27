(in-package :gui)

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

(defmethod draw ((disk lens:disk))
  (declare (values null &optional))
  (with-slots ((c lens::center) (r lens::radius)) disk
   (with-pushed-matrix
     (translate-v c)
     (scale r r r)
     (with-primitive :triangle-fan
       (draw-circle)))))

(defclass texture-3-luminance-ub8 ()
  ((dimensions :accessor dimensions :initarg :dimensions :initform '(0 0 0)
	       :type cons)
   (object :accessor object :initarg :object :initform 0 :type fixnum)
   (target :accessor target :initarg :target 
	   :initform :texture-3d :type fixnum)))

(defmethod initialize-instance :after ((tex texture-3-luminance-ub8) &key data)
  (declare ((simple-array (unsigned-byte 8) 3) data))
  (with-slots (dimensions object target) tex
    (setf object (first (gen-textures 1))
	  dimensions (array-dimensions data))
    (destructuring-bind (z y x) dimensions
      (bind-texture target object)
      (tex-parameter target :texture-min-filter :linear)
      (tex-parameter target :texture-mag-filter :linear)
      (sb-sys:with-pinned-objects (data)
	(let* ((data1 (sb-ext:array-storage-vector data))
	       (data-sap (sb-sys:vector-sap data1)))
	 (tex-image-3d target 0 :luminance x y z 0 :luminance
		       :unsigned-byte data-sap))))))

(defmethod destroy ((tex texture-3-luminance-ub8))
  (delete-textures (list (object tex))))

(defmethod bind-tex ((tex texture-3-luminance-ub8))
  (bind-texture (target tex) (object tex)))

(defun draw-axes ()
  (gl:line-width 3)
  (gl:with-primitive :lines
    (gl:color 1 0 0 1) (gl:vertex 0 0 0) (gl:vertex 1 0 0)
    (gl:color 0 1 0 1) (gl:vertex 0 0 0) (gl:vertex 0 1 0)
    (gl:color 0 0 1 1) (gl:vertex 0 0 0) (gl:vertex 0 0 1)))


