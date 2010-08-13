#.(require :alexandria)
#.(require :vector)
#.(require :cuda-fft)

(in-package :vol)

#+nil (declaim (optimize (speed 3) (debug 1) (safety 1)))
(declaim (optimize (speed 2) (debug 3) (safety 3)))

