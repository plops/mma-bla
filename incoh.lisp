;; incoherent illumination

;; \int |(e^{i r1 r2} C2) \otimes a|^2 C1 d^2r1

#.(require :vol)
#.(require :psf)

(defpackage :incoherent
  (:use :cl :vol))
(in-package :incoherent)

(defun abs^2 (z)
  (declare ((complex single-float) z)
	   (values single-float &optional))
  (realpart (* z (conjugate z))))

(let* ((n2 10)
       (c2 (let ((d3 (csf 1 n2 n2))
		 (d2 (draw-disk-csf 4.0 n2 n2)))
	     (do-region ((j i) (n2 n2))
	       (setf (aref d3 0 j i) (aref d2 j i)))
	     d3)))
  (multiple-value-bind (ex ey ez dz dx)
      (psf:electric-field-psf 30 100 100 :integrand-evaluations 201)
    (format t "~a~%" (list dz dx))
    (let* ((cx (convolve-nocrop c2 ex))
	   (cy (convolve-nocrop c2 ey))
	   (cz (convolve-nocrop c2 ez))
	   (abs2 (make-array (array-dimensions cx) :element-type 'single-float)))
      (destructuring-bind (z y x) (array-dimensions cx)
       (do-region ((k j i) (z y x))
	 (setf (aref abs2 k j i) (sqrt (+ (abs^2 (aref cx k j i))
				     (abs^2 (aref cy k j i))
				     (abs^2 (aref cz k j i)))))))
      (write-pgm "/dev/shm/o.pgm"
		 (normalize-2-sf/ub8
		  (cross-section-xz abs2))))))
