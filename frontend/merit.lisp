(in-package :frontend)

(defun merit-function (vec2 params)
  (declare ((simple-array double-float (2)) vec2)
	   (cons params)
	   (values double-float &optional))
  (destructuring-bind (objective model nucleus-index window-radius)
      params
   (let* ((border-value 0d0) ;; value to return when outside of bfp
	  ;; this has to be considerably bigger than the maxima on the bfp
	  (border-width window-radius) ;; in this range to the
	  ;; border of the bfp
	  ;; enforce bad merit
	  ;; function
	  (sum 0d0)
	  (radius (norm2 vec2)))
     (if (< radius (- .99d0 border-width))
	 ;; inside
	 (loop for dirs in '((:right :left)
			     (:top :bottom)) do
	      (loop for dir in dirs do
		   (loop for bfp-dir in dirs do
			(incf sum
			      (illuminate-ray objective
					      model
					      nucleus-index dir
					      (vec2-x vec2) (vec2-y vec2)
					      window-radius bfp-dir)))))
	 ;; in the border-width or outside of bfp
	 (incf sum border-value))
     sum)))

(defun find-optimal-bfp-window-center (nucleus params)
  (declare (fixnum nucleus)
	   (cons params)
	   (values vec2 &optional))
  (setf (elt params 2) nucleus)
  (loop
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
				:params params)
       (when (< min 100d0)
	 (return-from find-optimal-bfp-window-center point)))))

#+nil
(find-optimal-bfp-window-center 0)
