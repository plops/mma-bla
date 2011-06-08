(defpackage :libmma
  (:use :cl :sb-alien :sb-c-call)
  (:export #:init
	   #:uninit
	   #:image
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

(defun image (img)
  (declare (type (simple-array (unsigned-byte 16) 2) img))
  (destructuring-bind (h w) (array-dimensions img)
    (assert (= 256 w h))
    (let ((img1 (sb-ext:array-storage-vector img)))
      (sb-sys:with-pinned-objects (img)
	(upload-image (sb-sys:vector-sap img1)))))
  nil) 


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
(let ((a (make-array (list 256 256)
		     :element-type '(unsigned-byte 16))))
  (image a))
#+nil
(uninit)
#+nil
(status)