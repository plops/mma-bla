#.(require :alexandria)
#.(require :vector)
#.(require :cuda-fft)



(in-package :vol)

(deftype my-float ()
    'single-float)

(defconstant zero (coerce .0d0 'my-float))
(defconstant half (coerce .5d0 'my-float))
(defconstant one (coerce 1d0 'my-float))
(defconstant two (coerce 2d0 'my-float))
(defconstant my-pi (coerce pi 'my-float))

#+nil (declaim (optimize (speed 3) (debug 1) (safety 1)))
(declaim (optimize (speed 2) (debug 3) (safety 3)))
 
;; C-standard "row-major" order, so that the last dimension has the
;; fastest-varying index in the array.
