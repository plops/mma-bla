;; incoherent illumination

;; \int |(e^{i r1 r2} C2) \otimes a|^2 C1 d^2r1

#.(require :vol)
#.(require :psf)

(defpackage :incoherent
  (:use :cl :vol))
(in-package :incoherent)

(multiple-value-bind (ex ey ez)
    (psf:electric-field-psf 30 100 100)
  (write-pgm "/dev/shm/o.pgm" (normalize-2-csf/ub8-abs (cross-section-xz (fftshift (ft ex))))))

(let* ((n2 10)
       (c2 (let ((d3 (csf 1 n2 n2))
		 (d2 (draw-disk-csf 4.0 n2 n2)))
	     (do-region ((j i) (n2 n2))
	       (setf (aref d3 0 j i) (aref d2 j i)))
	     d3))
       (a (csf 2 n2 n2)))
  (convolve-nocrop c2 a))
