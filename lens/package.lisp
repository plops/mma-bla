(defpackage :lens
  (:use :cl :vector)
  (:export #:intersect
	   #:refract
	   #:plane
	   #:disk
	   #:lens
	   #:objective
	   #:back-focal-plane-radius
	   #:focal-length-from-magnification
	   #:make-objective
	   #:find-inverse-ray-angle))