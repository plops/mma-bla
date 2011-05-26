(in-package :lens)

(defclass plane ()
  ((normal :accessor normal :initarg :normal :initform #.(v 0 0 1) :type vec)
   (center :accessor center :initarg :center :initform #.(v 0 0 0) :type vec)))

(defclass disk (plane)
  ((radius :accessor radius :initarg :radius :initform 1d0 :type double-float)))

(defclass lens (disk)
  ((focal-length :accessor focal-length
		 :initarg :focal-length
		 :initform 1d0
		 :type double-float)))

(defclass objective (lens)
  ;; set the lens-radius to something bigger (maybe twice bfp-radius)
  ;; to prevent spurious RAY-LOST when dispatching to lens refract
  ((immersion-index :accessor immersion-index :initarg :immersion-index
                    :initform (alexandria:required-argument)
                    :type double-float)
   (numerical-aperture :accessor numerical-aperture :initarg :numerical-aperture
		       :initform (alexandria:required-argument)
		       :type double-float)
   (bfp-radius :accessor bfp-radius :initarg :bfp-radius
	       :initform (alexandria:required-argument)
	       :type double-float)))

(defmethod print-object ((objective objective) stream)
  (with-slots (immersion-index numerical-aperture focal-length bfp-radius) objective
    (format stream "#<objective ~3,1fx f: ~2,2f na: ~f n: ~f bfp-radius: ~2,3f>"
	    (/ 164.5 focal-length) focal-length 
	    numerical-aperture immersion-index bfp-radius)))
