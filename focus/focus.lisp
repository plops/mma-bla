(in-package :focus)

(defvar *stream* nil)
(defvar *fd* nil)

(defun run-shell (command)
  (with-output-to-string (stream)
    (sb-ext:run-program "/bin/bash" (list "-c" command)
			:input nil
			:output stream)))

(defun find-zeiss-usb-adapter ()
  (let ((port (run-shell "dmesg|grep pl2303|grep ttyUSB|tail -n1|sed s+.*ttyUSB+/dev/ttyUSB+g|tr -d '\\n'")))
    (if (string-equal "" port)
	(error "dmesg output doesn't contain ttyUSB assignment. This can happen when the system ran a long time. You could reattach the USB adapter that is connected to the microscope.")
	port)))


(defun connect (&optional (devicename (find-zeiss-usb-adapter)))
  (multiple-value-bind (s fd)
      (open-serial devicename)
    (defparameter *stream* s)
    (defparameter *fd* fd)))
#+nil
(connect)

(defun disconnect ()
  (close-serial *fd*)
  (setf *stream* nil))

#+nil
(disconnect)
 
#+nil
(serial-recv-length *fd*)
 
#+nil ;; do cat /dev/ttyUSB1 in some terminal, or use read-response below
(progn
  (format *stream* "HPTv0~a" #\Return)
  (finish-output *stream*))
 
#+nil
(progn
  (format *stream* "FPZp~a" #\Return)
  (finish-output *stream*))
 
#+nil
(read-response *fd* *stream*)
 
#+nil
(response->pos-um (read-response *fd* *stream*))
 
#+nil
(close-serial *fd2*)
 
#+nil
(time
 (response->pos-um (talk-zeiss *fd2* *s2* "FPZp")))
 
#+nil ;; measure the time it takes until the full response has arrived
(progn
 (format *s2* "FPZp~a" #\Return)
 (finish-output *s2*)
 (dotimes (i 10)
   (sleep .01d0)
   (format t "~a~%" (list i (serial-recv-length *fd2*))))
 (read-response *fd2* *s2*))
 
(defconstant +step-size+ .025s0 "Distance of one z step in micrometer.")
 
(defun response->pos-um (answer)
  (declare (string answer)
	   (values single-float &optional))
  (if (equal "PF" (subseq answer 0 2))
    (let* ((uval (the fixnum (read-from-string
			      (format nil "#x~a" (subseq answer 2)))))
	   (val (if (eq 0 (logand uval #x800000))
		    uval ;; positive
		    (- uval #xffffff 1))))
      (* +step-size+ val))
    (error "unexpected answer on serial port.")))
 
;; some tricks with two's complement here!  be sure to generate a
;; 24bit signed number consecutive application of pos-um->request and
;; response->pos-um should be the identity (if you don't consider the
;; prefix "PF" that response->pos-um expects)

(defun pos-um->request (pos-um)
  (declare (single-float pos-um)
	   (values string &optional))
  (format nil "~6,'0X"
	  (let ((val (round pos-um +step-size+)))
	    (if (< val 0)
		(+ #xffffff val 1)
		val))))
 
(defun get-position ()
  (declare (values single-float &optional))
  (response->pos-um (talk-zeiss *fd* *stream* "FPZp")))
 
(defun set-position (position-um)
  (declare (single-float position-um))
  (write-zeiss *stream*
	       (format nil "FPZT~a" (pos-um->request position-um))))
 
#+nil
(format nil "FPZT~a" (pos-um->request -8.0d0))
 
#+nil
(defparameter current-pos (get-position *fd* *stream*))
#+nil
(format t "pos: ~a~%" (get-position *fd2* *s2*))
#+nil
(time (format t "response ~a~%"
	      (set-position *s2* (+ current-pos 0.7d0))))
 
#+nil
(progn
  (set-position *s2* (+ current-pos 135d0))
  (dotimes (i 20)
    (format t "pos ~a~%" (list i (get-position *fd2* *s2*)))))
 
#+nil
(loop for i below 100 do
     (sleep .1)
     (format t "~a~%" (response->pos-um (talk-zeiss "FPZp"))))
