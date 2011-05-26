(in-package :raytrace)

;; the following class contains only the part of the model, that is
;; necessary to implement the intersection-length methods. 

(defclass sphere-algebraic-model ()
  (;; pixel dimensions in um
   (dx :accessor dx :initarg :dx :initform .2d0 :type double-float)
   (dy :accessor dy :initarg :dy :initform .2d0 :type double-float)
   (dz :accessor dz :initarg :dz :initform 1d0 :type double-float)
   (immersion-index :accessor immersion-index :initarg :immersion-index
		    :initform 1.515d0 :type double-float)
   ;; size of the input stack
   (dimensions :accessor dimensions :initarg :dimensions 
	       :initform nil :type cons)
   ;; integral center coordinates of the nuclei, vec-i
   (centers :accessor centers :initarg :centers :initform nil :type cons)
   ;; radii of the nuclei in mm, double-float
   (radii-mm :accessor radii-mm :initarg :radii-mm :initform nil :type cons)
   ;; center positions of the nuclei in mm, double-float
   (centers-mm :accessor centers-mm :initarg :centers-mm :initform nil :type cons)))