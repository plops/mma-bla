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

(defun draw-disk (center radius)
  (declare (vec center)
	   (double-float radius)
	   (values null &optional))
  (with-pushed-matrix
    (translate-v center)
    (scale radius radius radius)
    (with-primitive :triangle-fan
      (draw-circle))))
