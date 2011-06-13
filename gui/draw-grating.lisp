(in-package :gui)

(defun grating-stack (h &optional (w h))
  (let* ((z (* w h))
         (a (make-array (list z h w)
                        :element-type 'boolean
                        :initial-element nil)))
    (dotimes (k z)
      (let ((i (mod k w))
            (j (floor k w)))
        (setf (aref a k j i) t)))
    a))
#+nil
(grating-stack 3 3)

(defun grating->texture (grating-stack k &key (h 1) (w h) (value #xff))
  (destructuring-bind (z y x) (array-dimensions grating-stack)
    (assert (< k z))
    (let ((a (make-array (list (* y h) (* x w))
                        :element-type '(unsigned-byte 8))))
      (dotimes (j y)
        (dotimes (i x)
          (when (aref grating-stack k j i)
            (dotimes (jj h)
              (dotimes (ii w)
                (setf (aref a (+ (* h j) jj) (+ (* w i) ii)) value))))))
      a)))

#+nil
(grating->texture
 (grating-stack 3 3) 
 4 :w 2 :h 2 :value 1)

;; display the grating as a repetitive 2d texture
(defclass grating ()
  ((object :accessor object :initarg :object :initform 0 :type fixnum)))

(defmethod initialize-instance :after ((tex grating) &key data)
  (declare ((simple-array (unsigned-byte 8) 2) data))
  (destructuring-bind (y x) (array-dimensions data)
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
	   (tex-image-2d target 0 :luminance x y 0 :luminance
			 :unsigned-byte data-sap)))))))

(defmethod destroy ((tex grating))
  (delete-textures (list (object tex))))

(defmacro with-grating ((grating data) &body body)
  `(let ((,grating (make-instance 'gui::grating :data ,data)))
     ,@body
     (gui::destroy ,grating)))

(defmethod bind ((tex grating))
  (bind-texture :texture-2d (object tex)))


(defmethod draw-grating ((self grating) &key
		 (x 0f0) (y 0f0) 
		 (w 1920f0) (h 1080f0)
		 (wt 1f0) (ht 1f0))
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
