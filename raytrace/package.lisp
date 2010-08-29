(defpackage :raytrace
  (:use :cl :vector)
  (:export #:quadratic-roots
	   #:ray-sphere-intersection-length
	   #:ray-spheres-intersection
	   #:sphere
	   #:sphere-algebraic-model
	   #:dx
	   #:dy
	   #:dz
	   #:immersion-index
	   #:dimensions
	   #:centers
	   #:radii-mm
	   #:centers-mm))