#.(require :gui)
(in-package :run)

#+nil
(let* ((k 25)
       (nuc (first (get-visible-nuclei k)))
       (vol (get-lcos-volume k nuc)))
  (format t "~a~%" `(nuc ,nuc))
  (write-section "/home/martin/tmp/angular-0lcos-cut.pgm" vol)
  (save-stack-ub8 "/home/martin/tmp/angular-0lcos" (normalize-3-csf/ub8-realpart vol)))

;; FIXME: are these coordinates in mm or relative positions for a bfp-radius of 1?
;; I think the latter, but I'm not sure.
#+nil
(time
 (let*((dx .2d0)
       (dz 1d0)
       (r 100)
       (z (array-dimension *spheres* 0))
       (psf (angular-psf :x r :y r :z (* 2 z)
		:pixel-size-x dx :pixel-size-z dz
		:window-radius *bfp-window-radius*
		:window-x -.714d0
		:window-y .16d0
		:initialize t
		:debug t
		:integrand-evaluations 400)))
   (save-stack-ub8 "/home/martin/tmp/psf" (normalize3-cdf/ub8-realpart psf))
   nil))


;; calculate the excitation one nucleus
#+bla
(defun calc-light-field (k nucleus)
  (declare (fixnum k nucleus))
  (let* ((result nil)
	 (lcos (get-lcos-volume k nucleus))
	 (bfp-pos (find-optimal-bfp-window-center nucleus))
	 (psf (let ((dx .2d0)
		    (dz 1d0)
		    (r 100)
		    (z (array-dimension *spheres* 0)))
		(multiple-value-bind (h ddx ddz)
		    (angular-intensity-psf-minimal-resolution
				 :x-um (* r dx) :y-um (* r dx) :z-um (* 2 z dz)
				 :window-radius *bfp-window-radius*
				 :window-x (vec2-x bfp-pos)
				 :window-y (vec2-y bfp-pos)
				 :initialize t
				 :debug t
				 :integrand-evaluations 1000)
		  (resample-3-cdf h ddx ddx ddz dx dx dz)))))
    (format t "~a~%" `(bfp-pos ,bfp-pos))
    (write-section (format nil "/home/martin/tmp/angular-1expsf-cut-~3,'0d.pgm" nucleus) psf)

    (multiple-value-bind (conv conv-start)
	(convolve-nocrop lcos psf)
      ;; light distribution in sample
      (defparameter *angular-light-field* conv)
      (defparameter *angular-light-field-start* conv-start)
      (write-section (format nil "/home/martin/tmp/angular-2light-cut-~3,'0d.pgm" nucleus)
		     conv
		     (vec-i-y (aref *centers* nucleus)))
      (save-stack-ub8 (format nil "/home/martin/tmp/angular-2light-~3,'0d/" nucleus)
		      (normalize-3-cdf/ub8-realpart conv))
      ;; multiply fluorophore concentration with light distribution
      (let ((excite (.* conv *spheres* conv-start)))
	(defparameter *excite* excite)
	(write-section (format nil "/home/martin/tmp/angular-3excite-cut-~3,'0d.pgm" nucleus)
		       excite
		       (vec-i-y (aref *centers* nucleus)))
	(save-stack-ub8 (format nil "/home/martin/tmp/angular-3excite-~3,'0d/" nucleus)
			(normalize-3-cdf/ub8-realpart excite))
	(destructuring-bind (z y x)
	    (array-dimensions excite)
	  (declare (ignorable z))
	  (let* ((in-focus (extract-bbox-3-cdf excite
					      (make-bbox :start (v 0d0 0d0 (* 1d0 k))
							 :end (v (* 1d0 (1- x))
								 (* 1d0 (1- y))
								 (* 1d0 k))))))
	    (save-stack-ub8 "/home/martin/tmp/angular-4in-focus/"
			    (normalize-3-cdf/ub8-realpart in-focus))
	    (let*((mplane (mean (convert-3-cdf/df-realpart in-focus)))
		  (mvol (mean (convert-3-cdf/df-realpart excite)))
		  (gamma (/ mplane mvol)))
	      (push (list mplane mvol gamma) result)
	      (debug-out mplane mvol gamma)
	      (format t "plane-result ~f ~f ~f~%" mplane mvol gamma))
	    result))))))

#+nil
(time
 (progn
  (with-open-file (*standard-output* "/home/martin/tmp/angular-stack.log"
				     :direction :output
				     :if-exists :supersede
				     :if-does-not-exist :create)
    (let ((result nil))
      (dotimes (k (array-dimension *spheres* 0))
	(let* ((nucs (get-visible-nuclei k)))
	  (loop for nuc in nucs do
	       (format t "~a~%" (list 'doing k nucs))
	       (push (list k nuc (calc-light-field k nuc)) result))))
      (defparameter *scan-result* result)))
  (with-open-file (s "/home/martin/tmp/angular-stack-struc.lisp"
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (write *scan-result* :stream s))))





#+nil
(let ((vol (make-array dims :element-type '(unsigned-byte 8))))
  (loop for i in '(4.0d-3 -.2d-3) do
   (draw-ray-into-vol i 0d0 .99d0 .0d0 vol)
   #+nil(draw-ray-into-vol i 0d0 -.99d0 .0d0 vol)
   (draw-ray-into-vol i 0d0 0d0 .99d0 vol)
   (draw-ray-into-vol i 0d0 0d0 -.99d0 vol))

  (save-stack-ub8 "/home/martin/tmp/line"
		  vol))


#+nil
(time
 (let* ((n 100)
	(a (make-array (list n n) :element-type '(unsigned-byte 8)))
	(nn (length *spheres-c-r*))
	(mosaicx (ceiling (sqrt nn)))
	(mosaic (make-array (list (* n mosaicx) (* n mosaicx))
			    :element-type '(unsigned-byte 8))))
   (with-open-file (*standard-output* "/dev/shm/a"
				      :direction :output
				      :if-exists :supersede)
     (dotimes (*nucleus-index* 1 nn)
       (dotimes (i 10)
	 (tagbody again
	    (multiple-value-bind (min point)
		(simplex-anneal:anneal (simplex-anneal:make-simplex
					(make-vec2 :x -1d0 :y -1d0) 1d0)
				       #'merit-function
				       ;; set temperature bigger than the
				       ;; maxima in the bfp but smaller
				       ;; than border-value
				       :start-temperature 2.4d0
				       :eps/m .02d0
				       :itmax 1000
				       :ftol 1d-3
				       :params )
	      (unless (<= min 100d0)
		(go again))
	      (let* ((x (aref point 0))
		     (y (aref point 1))
		     (ix (floor (* n (+ x 1)) 2))
		     (iy (floor (* n (+ y 1)) 2))
		     (mx (mod *nucleus-index* mosaicx))
		     (my (floor *nucleus-index* mosaicx)))
		(incf (aref mosaic (+ (* n my) iy) (+ (* n mx) ix)))
		(format t "min ~a~%" (list min ix iy))))))))
   (write-pgm "/home/martin/tmp/scan-mosaic-max.pgm" mosaic)))



