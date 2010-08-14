(declaim (optimize (speed 2) (debug 3) (safety 3)))
;#.(require :alexandria)
;#.(require :vector)

;; only in the lab
#+x86-64 (require :cuda-fft)


(defpackage :vol
  (:use :cl :sb-alien :sb-c-call :vector 
	#+x86-64 :cuda-fft)
  (:export

    #:read-pgm
    #:write-pgm
    #:histogram
    #:read-stack
    #:linear-regression
    #:clamp
       
    #:do-region
    #:with-slice
   
    #:save-stack-ub8


    #:resample-half
    #:cross-section-xz

    #:count-non-zero-ub8
    
    #:bbox
    #:make-bbox
    #:bbox-start
    #:bbox-end
    
    #:mean

    #:with-arrays

    #:draw-disk
    #:draw-unit-intensity-disk-precise
    #:draw-unit-energy-disk-precise
    #:draw-sphere-ub8
    #:draw-oval-ub8))

;; for i in `cat vol.lisp|grep defconst|cut -d " " -f 2`;do echo \#:$i ;done