(require :asdf)
#.(push "/home/martin/0505/mma/" asdf:*central-registry*)
(require :andor3)


(defconstant +handle-system+ 1)
(defconstant +success+ 0)


(defmacro with-wide-string ((s string) &body body)
  (let ((s32 (gensym)))
    `(let* ((,s32 (andor3::char->wide-char ,string))
	    (,s (sb-sys:vector-sap ,s32)))
       (sb-sys:with-pinned-objects (,s32)
	 ,@body))))

(defun get-device-count ()
  (with-wide-string (s "DeviceCount")
      (multiple-value-bind (a b)
	  (andor3::%get-int +handle-system+ s)
	(assert (= a +success+))
	b)))
#+nil
(get-device-count)

(defun camera-open (&optional (index 0))
 (multiple-value-bind (a b)
     (andor3::%open index)
   (assert (= a +success+))
   b))

(defparameter *c* nil)
#+nil
(defparameter *c* (camera-open))

(defun get-serial-number ()
  (let* ((n 64)
	 (ret (make-array n :element-type '(unsigned-byte 32)))
	 (sret (sb-sys:vector-sap ret)))
    (sb-sys:with-pinned-objects (ret)
     (with-wide-string (s "SerialNumber")
       (assert (= +success+ (andor3::%get-string *c* s sret n)))))
    (andor3::wide-char->char ret)))

#+nil
(get-serial-number)

(defun set-exposure (exp_ms)
  (declare (type double-float exp_ms)
	   (values double-float &optional))
  (with-wide-string (s "ExposureTime")
    (assert (= +success+
	       (andor3::%set-float *c* s exp_ms)))
    (multiple-value-bind (a b) (andor3::%get-float *c* s)
      (assert (= +success+ a))
      b)))
#+nil
(set-exposure .0163d0)

(defun get-int (feature)
  (with-wide-string (s feature)
    (multiple-value-bind (a b)
	(andor3::%get-int *c* s)
      (assert (= +success+ a))
      b)))
#+nil
(get-int "ImageSizeBytes")
#+nil
(get-int "AOIHeight")
#+nil
(get-int "AOIWidth")



(defparameter *buf*
 (let* ((w (get-int "AOIWidth")) 
	(h (get-int "AOIHeight"))
	(img (make-array (list h w)
			 :element-type '(unsigned-byte 16)))
	(img1 (sb-ext:array-storage-vector img))
	(sap (sb-sys:vector-sap img1))
	(int (sb-sys:sap-int sap)))
   (unless (= 0 (mod int 8))
     (break "Buffer must be aligned on am 8 byte boundary."))
   (sb-sys:with-pinned-objects (img)
    (let ((ret (andor3::%queue-buffer *c* sap (* 2 (length img1)))))
      (unless (= +success+ ret)
	(break "Error queue-buffer: ~a." (andor3::lookup-error ret)))))
   (list int img)))

(defun command (cmd)
  (with-wide-string (s cmd)
    (assert (= +success+ (andor3::%command *c* s)))))
#+nil
(command "AcquisitionStart")

(defparameter *blba*
 (multiple-value-list
  (andor3::%wait-buffer *c* 10000)))

(= *buf* (second *blba*))

#+nil
(andor3::%initialise-library)
#+nil
(andor3::%finalise-library)