#.(progn (require :asdf)
	 (require :vector)
	 (require :vol)
	 (require :psf))

(defpackage :deconvolve
  (:use :cl :vector :vol))
(in-package :deconvolve)

(defun draw-sphere (vol radius)
  (declare ((simple-array (complex double-float) 3) vol)
	   (double-float radius)
	   (values (simple-array (complex double-float) 3) &optional))
  (destructuring-bind (z y x)
     (array-dimensions vol)
   (let ((xh (floor x 2))
	 (yh (floor y 2))
	 (zh (floor z 2))
	 (radius2 (* radius radius)))
     (do-box (k j i 0 z 0 y 0 x)
       (let* ((a (- i xh))
	      (b (- j yh))
	      (c (- k zh))
	      (r2 (+ (* a a) (* b b) (* c c))))
	 (when (< r2 radius2)
	   (setf (aref vol k j i)
		 (complex 1d0)))))))
  vol)

(defun add-noise (vol avg)
  (declare ((simple-array (complex double-float) 3) vol)
	   (double-float avg)
	   (values (simple-array (complex double-float) 3) &optional))
  (destructuring-bind (z y x)
      (array-dimensions vol)
    (do-box (k j i 0 z 0 y 0 x)
      (setf (aref vol k j i)
	    (+ (aref vol k j i)
	       (random avg)))))
  vol)

#+nil
(time
 (let* ((dx 1d0)
	(dz dx)
	(x 32)
	(y 32)
	(z 32)
	(psf (psf:intensity-psf z y x (* dx x) (* dz z)
				:integrand-evaluations 100))
	(sphere (make-array (list 32 32 32)
			    :element-type '(complex double-float))))

   (defparameter *sphere* sphere)
   (draw-sphere sphere 4.2d0)
   (defparameter *psf* psf)
   
   
   (write-pgm (normalize-img (cross-section-xz psf))
	      "/home/martin/tmp/deconv000-psf.pgm")
   (write-pgm (normalize-img (cross-section-xz sphere))
	      "/home/martin/tmp/deconv001-sphere.pgm")

   (let ((sphere-x-psf (convolve3 sphere psf)))
   ;;  (add-noise sphere-x-psf 100d0)
     (defparameter *sphere-x-psf* sphere-x-psf)

     (write-pgm (normalize-img (cross-section-xz sphere-x-psf))
		"/home/martin/tmp/deconv002-sphere-x-psf.pgm"))))

(defun divide-minus-1 (result mi ei)
  (declare ((simple-array (complex double-float) 3) result mi ei)
	   (values (simple-array (complex double-float) 3) &optional))
  (destructuring-bind (z y x)
      (array-dimensions mi)
    (do-box (k j i 0 z 0 y 0 x)
      (setf (aref result k j i)
	    (complex (- (/ (realpart (aref mi k j i))
			   (realpart (aref ei k j i)))
			1d0)))))
  result)

(defun multiply-add (pl q fl)
  (declare ((simple-array (complex double-float) 3) fl pl)
	   (double-float q)
	   (values (simple-array (complex double-float) 3) &optional))
  (destructuring-bind (z y x)
      (array-dimensions pl)
    (do-box (k j i 0 z 0 y 0 x)
      (incf (aref pl k j i)
	    (complex (* q
			(realpart (aref fl k j i))
			(realpart (aref pl k j i)))))))
  pl)

#+nil
(time
 (let* ((p *sphere* #+nil (make-array (array-dimensions *sphere-x-psf*)
		       :element-type '(complex double-float)
		       :initial-element (complex 1d0)))
	(qi (make-array (array-dimensions *sphere-x-psf*)
			:element-type '(complex double-float)
			:initial-element (complex 1d0)))
	(qs '(1 2 1 4 1 8 1 4 1 16 1 2 1 16 1 2 1
	      16 1 2 1 16 1 2 1 16 1 2 1 16 1 2 1
	      16 1 2 1 16 1 2 1 16 1 2 1 16 1 2 1
	      16 1 2 1 16 1 2 1 16 1 2 1 16 1 2 1
	      16 1 2 1 16 1 2 1 16 1 2 1 16 1 2 1
	      16 1 2 1 16 1 2 1 16 1 2 1 16 1 2 1
	      16 1 2 1 16 1 2 1 16 1 2 1 16 1 2 1
	      16 1 2 1 16 1 2 1 16 1 2 1 16 1 2 1
	      16 1 2 1 16 1 2 1 16 1 2 1 16 1 2 1
	      16 1 2 1 16 1 2 1 16 1 2 1 16 1 2 1
	      16 1 2 1 16 1 2 1 16 1 2 1 16 1 2 1
	      1 1 1 1 1 1
	      ))
	(count 2))
   (loop for i in qs do
	(incf count)
	(let ((ei (convolve3 p *psf*)))
	  (divide-minus-1 qi *sphere-x-psf* ei)
	  (write-pgm (normalize-img (cross-section-xz qi))
		     (format nil "/home/martin/tmp/deconv~3,'0d-qi.pgm" count))
	  (let ((fl (convolve3 qi *psf*)))
	    (incf count)
	    (write-pgm (normalize-img (cross-section-xz fl))
		       (format nil "/home/martin/tmp/deconv~3,'0d-fl.pgm" count))
	    (multiply-add p 1d0 fl)
	    (incf count)
	    (write-pgm (normalize-img (cross-section-xz p))
		   (format nil "/home/martin/tmp/deconv~3,'0d-pl.pgm" count)))))))