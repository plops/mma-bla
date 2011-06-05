(require :asdf)
(push "/home/martin/0505/mma/" asdf:*central-registry*)
(require :andor3)

(andor3::%initialise-library)
(defconstant +handle-system+ 1)
(defconstant +success+ 0)

(defun string->wchar_t (s)
   "Convert lisp string into zero terminated uint32 array."
   (let ((a (make-array (1+ (length s))
			:element-type '(unsigned-byte 32))))
     (dotimes (i (length s))
       (setf (aref a i) (char-code (char s i))))
     a))
#+nil
(string->wchar_t "test")


(defmacro with-wide-string ((s string) &body body)
  (let ((s32 (gensym)))
    `(let* ((,s32 (andor3::string->wchar_t ,string))
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

(defparameter *c* (camera-open))

(defun get-serial-number ()
  (let* ((n 64)
	 (ret (make-array n :element-type '(unsigned-byte 32)))
	 (sret (sb-sys:vector-sap ret)))
    (sb-sys:with-pinned-objects (ret)
     (with-wide-string (s "SerialNumber")
       (multiple-value-bind (a b)
	   (andor3::%get-string *c* s sret n)
	 (assert (= a +success+)))
       ret))))

(andor3::%finalise-library)