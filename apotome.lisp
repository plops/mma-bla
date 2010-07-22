#+nil 
(time (let* ((dx .1d0)
	     (dz .1d0)
	     (x 100)
	     (y x)
	     (z 100))
	(multiple-value-bind (ex ey ez)
       (psf:electric-field-psf z y x (* dz z) (* dx x) :integrand-evaluations 200)
	  (defparameter *kex* (fftshift3 (ft3 ex)))
	  (defparameter *key* (fftshift3 (ft3 ey)))
	  (defparameter *kez* (fftshift3 (ft3 ez)))
	  nil)))
#+Nil
(progn
 (save-stack-ub8 "/home/martin/tmp/kex" (normalize-vol *kex*))
 (save-stack-ub8 "/home/martin/tmp/key" (normalize-vol *key*))
 (save-stack-ub8 "/home/martin/tmp/kez" (normalize-vol *kez*))
 )
#+NIL
(write-pgm (normalize-img (cross-section-xz *kex*))
	   "/home/martin/tmp/kex.pgm")
#+nil
(time
 (destructuring-bind (z y x)
     (array-dimensions *kex*)
   (let* ((grat (make-array (list z y x) :element-type '(complex double-float))))
     (let ((k (floor z 2)))
       (do-rectangle (j i 0 y 0 x)
	 (setf (aref grat k j i) (complex (* 1d0 (mod i 8))))))
     (defparameter *kgrat* (fftshift3 (ft3 grat)))
     nil)))
#+nil
(save-stack-ub8 "/home/martin/tmp/kgrat" (normalize-vol *kgrat*))
#+nil ;; store laser grating
(destructuring-bind (z y x)
    (array-dimensions *kex*)
 (let* ((ex (ift3 (fftshift3 (.* *kgrat* *kex*))))
	(ey (ift3 (fftshift3 (.* *kgrat* *key*))))
	(ez (ift3 (fftshift3 (.* *kgrat* *kez*))))
	(intens (make-array (array-dimensions ex)
			    :element-type '(complex double-float))))
   (do-box (k j i 0 z 0 y 0 x)
     (setf (aref intens k j i)
	   (+ (* (conjugate (aref ex k j i)) (aref ex k j i))
	      (* (conjugate (aref ey k j i)) (aref ey k j i))
	      (* (conjugate (aref ez k j i)) (aref ez k j i)))))
   (defparameter *intens* intens)
   (save-stack-ub8 "/home/martin/tmp/intens-grat" (normalize-vol intens)))) 
#+nil ;; store laser psf
(destructuring-bind (z y x)
    (array-dimensions *kex*)
 (let* ((ex (ift3 (fftshift3 *kex*)))
	(ey (ift3 (fftshift3 *key*)))
	(ez (ift3 (fftshift3 *kez*)))
	(intens (make-array (array-dimensions ex)
			    :element-type '(complex double-float))))
   (do-box (k j i 0 z 0 y 0 x)
     (setf (aref intens k j i)
	   (+ (* (conjugate (aref ex k j i)) (aref ex k j i))
	      (* (conjugate (aref ey k j i)) (aref ey k j i))
	      (* (conjugate (aref ez k j i)) (aref ez k j i)))))
   (defparameter *intens-psf-laser* intens)
   (save-stack-ub8 "/home/martin/tmp/intens-psf-laser" (normalize-vol intens))))
#+nil
(write-pgm 
 (normalize-img (cross-section-xz *intens-psf-laser*))
 "/home/martin/tmp/intens-psf-laser.pgm")

#+nil
(time
 (save-stack-ub8 "/home/martin/tmp/kintens-grat" 
		 (normalize-vol (fftshift3 (ft3 *intens*))
				:function #'(lambda (z) (let ((v (abs z)))
							  (if (< v 1d-12)
							      -10d0
							      (log v)))))))

;; A circular window in the center of the BFP with radius Rap gives
;; rise to rays up to the angle beta into sample space. The radius of
;; the focal sphere is n*f. Therefore one can write
;; sin(beta)=Rap/(n*f). Changing illumination direction of the grating
;; will shear the intensity image. In order to generate an image of
;; limited coherence one has to convolve each plane with a disk. The
;; radius of the disk is: Rd(z)=z*sin(beta) with defocus z. 
;; Eliminate sin(beta):
;; Rd(z)=abs(z*Rap/(n*f))
;; for a 63x objective we get f=2.61, with n=1.515 

(defun draw-disk (radius y x)
  (declare (double-float radius)
	   (fixnum y x)
	   (values (simple-array (complex double-float) 2) &optional))
  (let ((result (make-array (list y x) :element-type '(complex double-float)))
	(xh (floor x 2))
	(yh (floor y 2))
	(r2 (* radius radius)))
    (do-rectangle (j i 0 y 0 x)
      (let* ((ii (- i xh))
	     (jj (- j yh))
	     (rr2 (+ (* ii ii) (* jj jj))))
	(when (< rr2 r2)
	  (setf (aref result j i) (complex (/ r2))))))
    result))
#+nil
(write-pgm (normalize-img
	    (fftshift2 (convolve2-circ
			(draw-disk 12d0 100 200)
			(draw-disk 12d0 100 200)))) "/home/martin/tmp/disk.pgm")

(sb-alien:define-alien-routine ("j1" bessel-j1)
    sb-alien:double
  (arg sb-alien:double))

;; isi.ssl.berkeley.edu/~tatebe/whitepapers/FT%20of%20Uniform%20Disk.pdf
(defun draw-disk-precise (radius y x)
  (declare (double-float radius)
	   (fixnum y x)
	   (values (simple-array (complex double-float) 2) &optional))
  (let ((a (make-array (list y x)
		       :element-type '(complex double-float)))
	(xh (floor x 2))
	(yh (floor y 2)))
    (do-rectangle (j i 0 y 0 x)
      (let* ((xx (/ (* 1d0 (- i xh)) x))
	     (yy (/ (* 1d0 (- j yh)) y))
	     (f (sqrt (+ (* xx xx) (* yy yy)))))
	(setf (aref a j i) (if (< f 1d-12)
			       (complex (* pi radius))
			       (complex (/ (bessel-j1 (* 2d0 pi f radius))
					   f))))))
    (fftshift2 (ift2 a))))

#+NIL
(write-pgm (normalize-img (draw-disk-precise 12.3d0 300 300))
	   "/home/martin/tmp/disk.pgm")

#+nil
(time
 (destructuring-bind (z y x)
     (array-dimensions *intens*)
   (let ((n 1.515d0)
	 (f (lens:focal-length-from-magnification 63d0))
	 (Rap 2d0)
	 (dz .21d0)
	 (result (make-array (array-dimensions *intens*)
			     :element-type '(complex double-float))))
     (dotimes (k z)
       (unless (eq k (floor z 2))
	(let* ((Rd-pixels (abs (* (- k (floor z 2)) dz (/ Rap (* n f)))))
	       (disk (draw-disk-precise Rd-pixels y x))
	       (img (make-array (list y x) :element-type '(complex double-float))))
	  (write-pgm (convert-img (s*2 .01d0 disk))
		     (format nil "/home/martin/tmp/disk/disk~3,'0d.pgm" k))
	  (do-rectangle (j i 0 y 0 x)
	    (setf (aref img j i) (aref *intens* k j i)))
	  (let ((conv (fftshift2 (convolve2-circ img disk))))
	    (do-rectangle (j i 0 y 0 x)
	      (setf (aref result k j i) (aref conv j i)))))))
     (defparameter *intens-disk* result)
     (save-stack-ub8 "/home/martin/tmp/intens-grat-conv" (normalize-vol result))
     (save-stack-ub8 "/home/martin/tmp/kintens-grat-conv"
		     (normalize-vol (ft3 result)
				    :function #'(lambda (z) (let ((v (abs z)))
							      (if (< v 1d-12)
								  -10d0
								  (log v)))))))))