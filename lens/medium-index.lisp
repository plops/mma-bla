#+nil
(require :lens)
(in-package :lens)

;; simulate slab with different material and thickness

(defparameter *obj*
 (let* ((mag 63d0)
	(f (focal-length-from-magnification mag))
	(ri 1.515d0)
	(na 1.46d0))
   (make-objective :numerical-aperture na
		   :immersion-index ri
		   :magnification mag
		   :center (make-vec 0d0 0d0 f)
		   :normal (v 0d0 0d0 -1d0))))



(defclass plane-interface (plane)
  ((index-before :accessor index-before :initarg :index-before
		 :initform (alexandria:required-argument "index-before")
		 :type double-float)
   (index-after :accessor index-after :initarg :index-after
		 :initform (alexandria:required-argument "index-after")
		 :type double-float)))

(defparameter *pl*
  (make-instance 'plane-interface :index-before 1.515d0
		 :index-after 1.33d0
		 :normal (v 0d0 0d0 -1d0)
		 :center (v 0d0 0d0 -.1d0)))

(defparameter *ray*
  (get-ray-behind-objective *obj* .05d0 0d0 .1d0 0d0))

#+nil
(intersect *ray* *sl*)


(defmethod refract ((ray ray) (plane-interface plane-interface))
  (let ((i (intersect ray plane-interface)))
   (with-slots (index-before index-after normal center) plane-interface
     (with-slots ((start vector::start) (direction vector::direction)) ray
       (let* ((kn1 (v* normal (v. direction normal)))
	      (kt1 (v- direction kn1))
	      (eta (/ index-after index-before))
	      (k1^2 (v. direction direction)) ;; could be left out, as direction has length 1
	      (rat (- (* eta eta k1^2) (v. kt1 kt1)))
	      (kn2 (if (< 0 rat) ;; is it total internal reflection?
		       (return-from refract (make-instance 'ray :direction (v- kt1 kn1)
							   :start i))
		       (v* normal (sqrt rat)))))
	 (make-instance 'ray :direction (normalize (v+ kt1 kn2)) :start i))))))

#+nil
(refract *ray* *pl*)


(with-open-file (s "/dev/shm/model.asy" :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (macrolet ((asy (str &rest rest)
		 `(progn
		    (format s ,str ,@rest)
		    (terpri s))))
      (flet ((coord (v)
	       (format nil "(~f,~f)"
		       (vec-x v) (vec-z v))))
	;(asy "import three;~%import grid3;")
	(asy "size(300,300);")
	(let ((qs .9d0))
	 (loop for i from (- qs) upto qs by (/ qs 10) do
	      (multiple-value-bind (ray in) (get-ray-behind-objective *obj* .05d0 0d0 i 0d0)
		(let* ((out (refract ray *pl*)))
		  (with-slots ((s0 vector::start) (d0 (vector::direction))) in
		    (with-slots ((s1 vector::start) (d1 vector::direction)) ray
		      (with-slots ((s2 vector::start) (d2 vector::direction)) out
			(asy "draw((0,1)--(0,0)--(1,0));")
			#+nil (asy "draw(~a--~a--~a);" 
				   (coord s1)
				   (coord s2)
				   (coord (v+ s2 (v* (normalize d2) 10d0))))
			(asy "draw(~a--~a--~a);"
			     (coord (elt lens::*refract-res* 0))
			     (coord (elt lens::*refract-res* 1))
			     (let ((f (focal-length *obj*)))
			      (coord (v+  (make-vec 0d0 0d0 (- (focal-length *obj*)))
					  (elt lens::*refract-res* 2)))))))))))))))
