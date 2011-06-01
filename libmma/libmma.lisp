(defpackage :libmma
  (:use :cl :sb-alien :sb-c-call)
  (:export #:init
	   #:uninit
	   #:upload-image
	   #:status
	   #:set-cycle-time))
(in-package :libmma)
(defparameter *library* "/home/martin/0505/mma-c/libmma.so") 
(load-shared-object *library*)

#+nil
(unload-shared-object *library*)

(define-alien-routine "upload_image" 
    int
  (buf (* unsigned-char)))
 
(define-alien-routine "set_cycle_time"
    int
  (time-ms float))

(define-alien-routine "init"
    int)

(define-alien-routine "uninit"
    int)

(define-alien-routine "status"
    int
  (stat unsigned-int :out)
  (err unsigned-int :out))

#+nil
(init)
#+nil
(set-cycle-time 150s0)
#+nil
(uninit)
#+nil
(status)