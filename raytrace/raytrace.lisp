(defpackage :raytrace
  (:export #:quadratic-roots
	   #:ray-sphere-intersection-length
	   #:ray-spheres-intersection
	   #:sphere)
  (:use :cl :vector))

(in-package :raytrace)

(declaim (optimize (speed 2) (safety 3) (debug 3)))

(define-condition one-solution () ())
(define-condition no-solution () ())

(defun quadratic-roots (a b c)
  (declare (double-float a b c)
	   (values double-float double-float &optional))
  "Find the two roots of ax^2+bx+c=0 and return them as multiple
  values."
  ;; see numerical recipes sec 5.6, p. 251 on how to avoid roundoff
  ;; error
  (let ((det2 (- (* b b) (* 4 a c))))
    (unless (<= 0d0 det2)
      (error 'no-solution))
    (let* ((pdet2 det2)
	   (q (* .5d0 (+ b (* (signum b) (sqrt pdet2)))))
	   (aa (abs a))
	   (aq (abs q)))
      (declare ((double-float 0d0) pdet2))
      (cond ((and (< aq 1d-12) (< aa 1d-12)) (error 'no-solution))
	    ((or (< aq 1d-12) (< aa 1d-12)) (error 'one-solution))
	    (t (values (/ q a) (/ c q)))))))

#+nil ;; two solution
(quadratic-roots 1d0 2d0 -3d0)
#+nil ;; one solution
(quadratic-roots 0d0 1d0 0d0)
#+nil ;; no solution
(quadratic-roots 0d0 -0d0 1d0)

(defclass sphere ()
  ((center :accessor center :initarg :center :initform (v) :type vec)
   (radius :accessor radius :initarg :radius :initform 1d0 :type double-float)))


(defmethod print-object ((sphere sphere) stream)
  (with-slots (center radius) sphere
    (format stream "#<sphere radius: ~4f center: <~4f ~4f ~4f>>" 
	    radius (vec-x center) (vec-y center) (vec-z center))))

(defmethod ray-sphere-intersection-length ((ray ray) (sphere sphere))
  (declare (values double-float &optional))
  ;; (c-x)^2=r^2 defines the sphere, substitute x with the rays p+alpha a,
  ;; the raydirection should have length 1, solve the quadratic equation,
  ;; the distance between the two solutions is the distance that the ray
  ;; travelled through the sphere
  (check-direction-norm ray)
  (destructuring-bind (start direction) ray
    (destructuring-bind (center radius) sphere
      (let* ((l (v- center start))
	     (c (- (v. l l) (* radius radius)))
	     (b (* -2d0 (v. l direction))))
       (handler-case
	   (multiple-value-bind (x1 x2)
	       (quadratic-roots 1d0 b c)
	     (abs (- x1 x2)))
	 (no-solution () 0d0)
	 (one-solution () 0d0))))))

#+nil
(ray-sphere-intersection-length (v 0d0 .1d0 -12d0) (v 0d0 0d0 1d0) (v) 3d0)

(defmethod ray-spheres-intersection ((ray ray) spheres illuminated-sphere-index)
  (declare ((simple-array sphere 1) spheres)
	   (fixnum illuminated-sphere-index)
	   (values double-float &optional))
  (let ((sum 0d0))
    (dotimes (i (length spheres))
      (unless (eq i illuminated-sphere-index)
	(incf sum (ray-sphere-intersection-length ray
						  (aref spheres i)))))
    sum))





