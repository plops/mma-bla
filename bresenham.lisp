(defpackage :bresenham
  (:use :cl :vol :vector)
  (:export #:scan-convert-line3))

(in-package :bresenham)

;; 1986 kaufman
(defun scan-convert-line3x (x1 y1 z1 x2 y2 z2 vol)
  (declare (fixnum x1 y1 z1 x2 y2 z2)
	   ((simple-array (unsigned-byte 8) 3) vol)
	   (values (simple-array (unsigned-byte 8) 3) &optional))
  ;; x2 - x1 has to be the biggest difference between endpoint
  ;; coordinates
  (let* ((x x1)
	 (delx (- x2 x1))
	 ;; initialization for y
	 (y y1)
	 (ddy (- y2 y1))
	 (dely (abs ddy))
	 (ysign (signum ddy))
	 (dy (- (* 2 dely) delx)) ;; decision variable along y
	 (yinc1 (* 2 dely)) ;; increment along y for dy<0
	 (yinc2 (* 2 (- dely delx))) ;; inc along y for dy>=0
	 ;; same initialization for z
	 (z z1)
	 (ddz (- z2 z1))
	 (delz (abs ddz))
	 (zsign (signum ddz))
	 (dz (- (* 2 delz) delx))
	 (zinc1 (* 2 delz))
	 (zinc2 (* 2 (- delz delx))))
    (when (<= delx 0)
	(error "x2 is <= x1."))
    (setf (aref vol z y x) 255)
    (loop while (< x x2) do
	 (incf x) ;; step in x
	 (if (< dy 0) ;; then no change in y
	     (incf dy yinc1) ;; update dy
	     (progn
	       (incf dy yinc2) ;; update dy, and
	       (incf y ysign)));; increment/decrement y

	 (if (< dz 0)
	     (incf dz zinc1)
	     (progn
	       (incf dz zinc2)
	       (incf z zsign)))
	 (setf (aref vol z y x) 255)))
  vol)
;; start from scan-convert-line3x and replace x->$, y->x, $->y
(defun scan-convert-line3y (x1 y1 z1 x2 y2 z2 vol)
  (declare (fixnum x1 y1 z1 x2 y2 z2)
	   ((simple-array (unsigned-byte 8) 3) vol)
	   (values (simple-array (unsigned-byte 8) 3) &optional))
  (let* ((y y1)
	 (dely (- y2 y1))
	 (x x1)
	 (ddx (- x2 x1))
	 (delx (abs ddx))
	 (xsign (signum ddx))
	 (dx (- (* 2 delx) dely))
	 (xinc1 (* 2 delx))
	 (xinc2 (* 2 (- delx dely)))
	 (z z1)
	 (ddz (- z2 z1))
	 (delz (abs ddz))
	 (zsign (signum ddz))
	 (dz (- (* 2 delz) dely))
	 (zinc1 (* 2 delz))
	 (zinc2 (* 2 (- delz dely))))
    (when (<= dely 0)
	(error "y2 is <= y1."))
    (setf (aref vol z y x) 255)
    (loop while (< y y2) do
	 (incf y)
	 (if (< dx 0)
	     (incf dx xinc1)
	     (progn
	       (incf dx xinc2)
	       (incf x xsign)))
	 (if (< dz 0)
	     (incf dz zinc1)
	     (progn
	       (incf dz zinc2)
	       (incf z zsign)))
	 (setf (aref vol z y x) 255)))
  vol)
;; replace x->$, z->x, $->z
(defun scan-convert-line3z (x1 y1 z1 x2 y2 z2 vol)
  (declare (fixnum x1 y1 z1 x2 y2 z2)
	   ((simple-array (unsigned-byte 8) 3) vol)
	   (values (simple-array (unsigned-byte 8) 3) &optional))
  (let* ((z z1)
	 (delz (- z2 z1))
	 (y y1)
	 (ddy (- y2 y1))
	 (dely (abs ddy))
	 (ysign (signum ddy))
	 (dy (- (* 2 dely) delz))
	 (yinc1 (* 2 dely))
	 (yinc2 (* 2 (- dely delz)))
	 (x x1)
	 (ddx (- x2 x1))
	 (delx (abs ddx))
	 (xsign (signum ddx))
	 (dx (- (* 2 delx) delz))
	 (xinc1 (* 2 delx))
	 (xinc2 (* 2 (- delx delz))))
    (when (<= delz 0)
	(error "z2 is <= z1."))
    (setf (aref vol z y x) 255)
    (loop while (< z z2) do
	 (incf z)
	 (if (< dy 0)
	     (incf dy yinc1)
	     (progn
	       (incf dy yinc2)
	       (incf y ysign)))

	 (if (< dx 0)
	     (incf dx xinc1)
	     (progn
	       (incf dx xinc2)
	       (incf x xsign)))
	 (setf (aref vol z y x) 255)))
  vol)

(defun scan-convert-line3 (start end vol)
  (declare (vec-i start end)
	   ((simple-array (unsigned-byte 8) 3) vol)
	   (values (simple-array (unsigned-byte 8) 3) &optional))
  (let* ((diff (v--i end start))
	 (ls (list (list (vec-i-x diff) 2)
		   (list (vec-i-y diff) 1)
		   (list (vec-i-z diff) 0)))
	 (diffa (mapcar #'(lambda (e) (list (abs (first e))
				       (second e))) ls))
	 ;; find the direction with the biggest difference
	 (sorted-diff-a (sort diffa #'> :key #'car))
	 (main-direction (second (first sorted-diff-a))) ;; 2 corresponds to x, 1->y, 0->z
	 ;; find the order in which to deliver the points
	 (main-diff (aref diff main-direction))
	 ;; we have to swap the points when main-diff is negative
	 (swap-points? (< main-diff 0))
	 ;; create the function name to dispatch to
	 (function (ecase main-direction
		     (2 #'scan-convert-line3x)
		     (1 #'scan-convert-line3y)
		     (0 #'scan-convert-line3z))))
    (when (eq 0 main-diff)
      (error "start and end point are the same."))
    (if swap-points?
	(funcall function
		 (vec-i-x end)
		 (vec-i-y end)
		 (vec-i-z end)
		 (vec-i-x start)
		 (vec-i-y start)
		 (vec-i-z start)
		 vol)
	(funcall function
		 (vec-i-x start)
		 (vec-i-y start)
		 (vec-i-z start)
		 (vec-i-x end)
		 (vec-i-y end)
		 (vec-i-z end)
		 vol))))


#+nil
(time
 (let ((vol (make-array (list 128 128 128) :element-type '(unsigned-byte 8))))
   (save-stack-ub8 "/home/martin/tmp/line"
		   (scan-convert-line3 (make-vec-i :x 0 :y 0 :z 0)
				       (make-vec-i :x 120 :y 127 :z 127)
				       vol))))
