(declaim (optimize (speed 2) (debug 3) (safety 3)))

#.(require :alexandria)
#.(require :vector)

;; only in the lab
#+x86-64 (require :cuda-fft)


(defpackage :vol
  (:use :cl :sb-alien :sb-c-call :vector :cuda-fft)
  (:export
   #:fftshift2
   #:convolve2-circ

    #:fftshift3
    #:read-pgm
    #:write-pgm
    #:histogram
    #:get-linear-array
    #:read-stack
    #:square
    #:linear-regression
    #:clamp
    #:interp1
    #:interpolate2

    #:+forward+
    #:+backward+
    #:+estimate+
   
    #:do-rectangle
    #:do-box
    #:with-slice
   
    #:save-stack
    #:save-stack-ub8
    #:.*
    #:.+
    #:s*
    #:.*2
    #:.+2
    #:s*2
    #:convolve3-circ
    #:convolve3-nocrop
    #:convolve3

    #:resample-half
    #:cross-section-xz

    #:convert3-cdf/df-imagpart
    #:convert3-df/ub8-floor
    #:convert2-ub8/cdf-complex
    #:convert2-cdf/df-realpart
    #:convert2-cdf/df-imagpart
    #:convert2-df/cdf-complex
    #:convert3-ub8/cdf-complex
    #:convert2-cdf/ub8-realpart
    #:convert3-cdf/df-phase
    #:convert2-cdf/ub8-abs
    #:convert3-cdf/df-realpart
    #:convert3-cdf/df-imagpart
    #:convert3-df/cdf-complex
    #:convert2-cdf/ub8-phase
    #:convert3-cdf/df-abs
    #:convert2-cdf/df-imagpart
    #:convert3-cdf/ub8-abs
    #:convert2-cdf/df-phase
    #:convert2-cdf/df-abs
    #:convert3-cdf/ub8-phase
    #:convert2-df/ub8-floor
    #:convert3-cdf/ub8-realpart
    #:convert3-cdf/ub8-imagpart

    #:convert3-ub8/csf-complex
    #:convert2-ub8/csf-complex
    #:convert1-ub8/csf-complex

    #:convert3-csf/cdf-coerce
    #:convert2-csf/cdf-coerce
    #:convert1-csf/cdf-coerce

    #:normalize2-cdf/ub8-phase
    #:normalize3-cdf/ub8-phase
    #:normalize2-cdf/ub8-realpart
    #:normalize2-cdf/ub8-imagpart
    #:normalize2-df/ub8-floor
    #:normalize2-cdf/ub8-abs
    #:normalize3-df/ub8-realpart
    #:normalize3-cdf/ub8-abs
    #:normalize3-cdf/ub8-realpart
    #:normalize3-cdf/ub8-imagpart
    #:normalize3-df/ub8-floor
    #:normalize-ub8

    #:normalize2-csf/ub8-realpart
    #:normalize2-sf/ub8-floor
    #:normalize2-csf/ub8-imagpart
    #:normalize3-sf/ub8-floor
    #:normalize1-df/ub8-floor
    #:normalize3-csf/ub8-abs
    #:normalize3-csf/ub8-realpart
    #:normalize3-csf/ub8-imagpart
    #:normalize1-sf/ub8-floor
    #:normalize3-csf/ub8-phase
    #:normalize2-csf/ub8-abs
    #:normalize2-csf/ub8-phase
    #:normalize1-cdf/ub8-realpart
    #:normalize1-csf/ub8-realpart

    #:count-non-zero-ub8
    #:decimate-xy-ub8
    
    #:bbox
    #:make-bbox
    #:bbox-start
    #:bbox-end
    #:extract-bbox2-ub8
    #:replace-bbox2-ub8
    #:find-bbox2-ub8
    #:find-bbox3-ub8
    #:extract-bbox3-ub8
    #:extract-bbox3-cdf
    #:extract-bbox3-df
    #:replace-bbox3-ub8
    
    #:mean-realpart
    #:normalize2-df/df
    #:with-arrays
    #:normalize->ub8
    #:draw-disk
    #:draw-unit-intensity-disk-precise
    #:draw-unit-energy-disk-precise
    #:draw-sphere-ub8
    #:draw-oval-ub8
    #:resample3-cdf
    #:interpolate3-cdf
    #:.-
    #:cross-section-xz-csf
    #:cross-section-xz-cdf))

;; for i in `cat vol.lisp|grep defconst|cut -d " " -f 2`;do echo \#:$i ;done