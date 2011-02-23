#+nil
(require :lens)
(in-package :lens)

;; simulate slab with different material and thickness
;; this should introduce spherical aberrations, high angles
;; will not hit the area as indicated by the LCoS image
 
(defparameter *obj*
 (let* ((mag 63d0)
	(f (focal-length-from-magnification mag))
	(ri 1.515d0)
	(na 1.46d0))
   (make-objective :numerical-aperture na
		   :immersion-index ri
		   :magnification mag
		   :center (make-vec 0d0 0d0 (* -1 ri f))
		   :normal (v 0d0 0d0 1d0))))


(defparameter *lens*
 (let* ((mag 63d0)
	(f (focal-length-from-magnification mag))
	(ri 1.515d0)
	(na 1.46d0))
   (make-instance 'lens
		  :focal-length f :radius (* na f)
		  :center (make-vec 0d0 0d0  (- f)))))

(defclass plane-interface (plane)
  ((index-before :accessor index-before :initarg :index-before
		 :initform (alexandria:required-argument "index-before")
		 :type double-float)
   (index-after :accessor index-after :initarg :index-after
		 :initform (alexandria:required-argument "index-after")
		 :type double-float)))

(defparameter *pl*
  (make-instance 'plane-interface :index-before 1.515d0
		 :index-after 1.515d0
		 :normal (v 0d0 0d0 -1d0)
		 :center (v 0d0 0d0 -.1d0)))

(defparameter *ray*
  (get-ray-behind-objective *obj* .05d0 0d0 .1d0 0d0))

#+nil
(intersect *ray* *pl*)
#+nil
(refract *ray* *lens*)

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


(defun extract (symb ls)
 (second (assoc symb ls)))

(defun coord (v)
  (format nil "(~f,~f)" (vec-x v) (vec-z v)))

(defun coord-e (symb ls)
  (coord (extract symb ls)))

(defmacro with-extracted (names ls &body body)
  `(let (,@(loop for e in names collect `(,e (extract ',e ,ls))))
     ,@body))

#+nil
(with-extracted (e i) lens::*refract-res*
  (format t "~a~%" e))

(progn
 (defun run ()
   (with-open-file (model-stream "/dev/shm/model.asy" :direction :output
				 :if-exists :supersede
				 :if-does-not-exist :create)
     (macrolet ((asy (str &rest rest)
		  `(progn
		     (format model-stream ,str ,@rest)
		     (terpri model-stream))))
       (asy "size(300,300);")
       (let ((qs .9d0))
	 (setf lens::*refract-res* nil)
	 (loop for j in '(-.1d0 .1d0) do
	      (loop for i from (- qs) upto qs by (/ qs 3) do
		   (multiple-value-bind (ray in) 
		       (get-ray-behind-objective *obj* j 0d0 i 0d0)
		     (refract ray *pl*)
		     (push `(in ,in) (first lens::*refract-res*))
		     (push `(out ,ray) (first lens::*refract-res*)))))
	 (dolist (e lens::*refract-res*)
	   (with-extracted (start i ro ru a r s in out) e
	     (let ((sphere (v+ i s)))
	       (asy "draw(~a--~a--~a);" 
		    (coord (vector::start in))
		    (coord (vector::start out))
		    (coord (v+ (vector::start out)
			       (v* (vector::direction out) 10s0 )))))))
	 (terpri)))))
 (run))


#+nil
(run)