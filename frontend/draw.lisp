(in-package :frontend)

(defun draw-spheres (radius centers z y x)
  "put points into the centers of nuclei and convolve a sphere around each"
  (declare (single-float radius)
	   (cons centers)
	   (fixnum z y x)
	   (values (simple-array (complex my-float) 3) &optional))
  (assert (equal (type-of (make-vec-i)) (type-of (first centers))))
  (let* ((dims (list z y x))
	 (points (make-array dims
			     :element-type '(complex my-float))))
    (dolist (c centers)
      (setf (aref points (* 5 (vec-i-z c)) (vec-i-y c) (vec-i-x c))
	    (complex +one+)))
    (convolve-circ points (draw-sphere-csf radius z y x))))

(defun draw-ovals (radius centers z y x)
  (declare (single-float radius)
	   (cons centers)
	   (fixnum z y x)
	   (values (simple-array (complex my-float) 3) &optional))
  (assert (equal (type-of (make-vec-i)) (type-of (first centers))))
  (let* ((dims (list z y x))
	 (points (make-array dims
			     :element-type '(complex my-float))))
    (dolist (c centers)
      (setf (aref points (vec-i-z c) (vec-i-y c) (vec-i-x c))
	    (complex +one+)))
    (convolve-circ points (draw-oval-csf radius z y x))))


(defun draw-indexed-ovals (radius centers z y x)
  "The first oval contains the value 1 the second 2, ..."
  (declare (single-float radius)
	   (cons centers)
	   (fixnum z y x)
	   (values (simple-array (complex my-float) 3) &optional))
  (assert (equal (type-of (make-vec-i)) (type-of (first centers))))
  (let* ((dims (list z y x))
	 (points (make-array dims
			     :element-type '(complex my-float)))
	 (i 0))
    (dolist (c centers)
      (incf i)
      (setf (aref points (vec-i-z c) (vec-i-y c) (vec-i-x c))
	    (complex (+ +one+ i))))
    (convolve-circ points (draw-oval-csf radius z y x))))


