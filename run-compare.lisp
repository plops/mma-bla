#.(require :asdf)
#.(require :frontend)

(in-package :frontend)

(defparameter *measured-centers*
  '((6 79 177)
    (12 125 111)
    (13 101 135)
    (13 155 89)
    (11 219 120)
    (3 212 168)) ;; make sure the illuminated one is the last one to be pushed
  "List of coordinates of nuclei centers. In the form z y x. dx=100nm, dz=2um")

(defun make-model-from-centers (measured-centers)
  (declare (values sphere-model-angular &optional))
  (let* ((centers nil))
    (dolist (e measured-centers)
      (destructuring-bind (z y x) e
	(push (make-vec-i :x x :y y :z z)
	      centers)))
    (make-instance 'sphere-model-angular
		   :dimensions '(20 300 300)
		   :dx .1d0
		   :dy .1d0
		   :dz 2d0
		   :centers centers
		   :radius 40s0
		   :scale-z 5s0 ; FIXME: this should be expressed with dx and dz
		   )))

(defmacro tmp (&rest rest)
  `(concatenate 'string "/home/martin/tmp/a0130/" ,@rest))

#+nil
(progn
 (defparameter *model* (make-model-from-centers *measured-centers*))
 (write-pgm (tmp "model-cut.pgm")
	   (normalize-2-csf/ub8-realpart 
	    (cross-section-xz (spheres *model*) 111)
	    ))
 (save-stack-ub8 (tmp "model")
		 (normalize-3-csf/ub8-realpart (spheres *model*))))

#+nil
(let* ((n 4)
       (shift (if (evenp n) (/ 1d0 n) 0))
       (direction 0)
       (i (floor direction n))
       (j (mod direction n))
       (x (- (* 1d0 (/ i n)) .5d0))
       (y (- (* 1d0 (/ j n)) .5d0))
       (radius (/ 1d0 n)))
  (format t "~a~%" (list x y shift radius))
 (defun draw-all ()
   (let ((r .25d0))
    (draw *model*
	  :nucleus 0 ;; target nucleus to shoot rays into
	  :win-x/r (- (- .99d0 r))	;(+ x shift)
	  :win-y/r 0d0			;(+ y shift)
	  :win-r/r r		;radius
	  :nr-ffp 2
	  :nr-bfp 2
	  :nr-theta 12))))

#+nil
(with-gui (draw-all))

#+nil ;; this way you see the model
(update-scale 100 10)
#+nil ;; in this mode you see the bfp
(update-scale 2.8 10)

#+nil 
(let* ((obj (lens:make-objective))
       (n (lens::immersion-index obj))
       (f (lens::focal-length obj))
       (nucleus-position (elt (centers-mm *model*) 0))
       (center (make-vec (vec-x nucleus-position)
			 (vec-y nucleus-position))))
  (update-view-center nucleus-position)
  (update-scale 100 20))

#+nil
(time
 (let* ((n 100)
	(nn 6)
	(mosaicx (ceiling (sqrt nn)))
	(mosaic (make-array (list (* n mosaicx) (* n mosaicx))
			    :element-type 'double-float))
	(obj (lens:make-objective :center (v) :normal (v 0 0 1)))
	(nucleus 0)
	(positions (sample-circles 3 7 5)))
   (with-open-file (str (tmp "scan-mosaic.txt") :direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
    (dotimes (nuc nn) ;; vary window radius
      (with-slots ((c lens::center)
		   (ri lens::immersion-index)
		   (f lens::focal-length)) obj
	(let* ((window-radius (* nuc (/ .30d0 nn)))
	       (z-plane-mm (vec-z (elt (raytrace::centers-mm *model*) nucleus)))
	       (vals-min 1d80)
	       (vals-max -1d80)) 
	  (setf c (make-vec 0d0 0d0 (+ (- (* ri f)) z-plane-mm)))
	  (let* ((params (list obj *model* nucleus window-radius positions))
		 (px (* n (mod nuc mosaicx)))
		 (py (* n (floor nuc mosaicx))))
	    (do-region ((j i) (n n))
	      (let* ((x (- (* 2d0 (/ i n)) 1d0))
		     (y (- (* 2d0 (/ j n)) 1d0))
		     (v (merit-function (make-vec2 :x x :y y)
					params
					:border-value .001d0)))
		(setf (aref mosaic (+ py j) (+ px i)) v)
		(if (< v vals-min)
		    (setf vals-min v)
		    (when (< vals-max v)
		      (setf vals-max v))))))
	  (format str "min ~2,6f max ~2,6f win-r ~2,3f~%"
		  vals-min vals-max
		  window-radius)
	  (format t "min ~2,6f max ~2,6f win-r ~2,3f~%"
		  vals-min vals-max
		  window-radius)))))
   (write-pgm (tmp "scan-mosaic.pgm") (normalize-2-df/ub8 mosaic))))