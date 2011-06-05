(require :asdf)
#.(progn
    (setf asdf:*central-registry* 
	  (union '("/home/martin/0505/mma/") asdf:*central-registry*))
    (require :andor3))


(defparameter *pic-buffer-hash* (make-hash-table))


(defconstant +handle-system+ 1)
(defconstant +success+ 0)


(defmacro with-wide-strings (decls &body body)
  (let* ((n (length decls))
	 (s32 (loop for i below n collect (gensym))))
    `(let* (,@(loop for i below n collect
		   (destructuring-bind (s string) (elt decls i)
		       `(,(elt s32 i) 
			  (andor3::char->wide-char ,string))))
	    ,@(loop for i below n collect
		   (destructuring-bind (s string) (elt decls i)
		       `(,s (sb-sys:vector-sap ,(elt s32 i))))))
       (sb-sys:with-pinned-objects ,s32
	 ,@body))))

(defun get-device-count ()
  (with-wide-strings ((s "DeviceCount"))
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


(defun get-serial-number ()
  (let* ((n 64)
	 (ret (make-array n :element-type '(unsigned-byte 32)))
	 (sret (sb-sys:vector-sap ret)))
    (sb-sys:with-pinned-objects (ret)
     (with-wide-strings ((s "SerialNumber"))
       (assert (= +success+ (andor3::%get-string *c* s sret n)))))
    (andor3::wide-char->char ret)))

#+nil
(get-serial-number)

(defun set-exposure (exp_ms)
  (declare (type double-float exp_ms)
	   (values double-float &optional))
  (with-wide-strings ((s "ExposureTime"))
    (assert (= +success+
	       (andor3::%set-float *c* s exp_ms)))
    (multiple-value-bind (a b) (andor3::%get-float *c* s)
      (assert (= +success+ a))
      b)))
#+nil
(set-exposure .0163d0)

(defun get-int (feature)
  (with-wide-strings ((s feature))
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

(defun set-recommended-roi528 ()
  (set-int "AOIWidth" 528)
  (set-int "AOIHeight" 512)
  (set-int "AOITop" 825)
  (set-int "AOILeft" 1033))

(defun set-int (feature value)
  (declare (type (unsigned-byte 64) value))
  (with-wide-strings ((s feature))
    (assert (= +success+ (andor3::%set-int *c* s value)))
    value))


(defun queue-buffer ()
  (declare (values (unsigned-byte 64)
		   (simple-array (unsigned-byte 16) 2) &optional))
  (let* ((w (get-int "AOIWidth")) 
	 (h (get-int "AOIHeight"))
	 (img (make-array (list h w)
			  :element-type '(unsigned-byte 16)))
	 (img1 (sb-ext:array-storage-vector img))
	 (sap (sb-sys:vector-sap img1))
	 (ptr (sb-sys:sap-int sap)))
    (unless (= 0 (mod ptr 8))
      (break "Buffer must be aligned on am 8 byte boundary."))
    (sb-sys:with-pinned-objects (img)
      (let ((ret (andor3::%queue-buffer *c* sap (* 2 (length img1)))))
	(unless (= +success+ ret)
	  (break "Error queue-buffer: ~a." (andor3::lookup-error ret)))))
    (values ptr img)))

(defun queue-buffer-into-hash ()
  (multiple-value-bind (ptr img) (queue-buffer) 
    ;; store into hash table with ptr as key
    (setf (gethash ptr *pic-buffer-hash*)
	  img)))

(defun wait-buffer ()
  "Wait for a buffer to be available in the capture queue and return
the corresponding image from the hash table."
  (multiple-value-bind (ret ptr bytes) (andor3::%wait-buffer *c* 10000)
    (unless (= +success+ ret)
      (break "Error in wait-buffer: ~a." (andor3::lookup-error ret)))
    (gethash ptr *pic-buffer-hash*)))

(defun flush ()
  "Remove buffers from the two queues. This needs to be called when
  the region of interest changes."
  (assert (= +success+ (andor3::%flush *c*)))
  (setf *pic-buffer-hash* (make-hash-table)))


(defun command (cmd)
  (with-wide-strings ((s cmd))
    (assert (= +success+ (andor3::%command *c* s)))))

#+nil
(assert (= +success+
	 (andor3::%close *c*)))

(defun get-enum-index (feature)
  (with-wide-strings ((s feature))
    (multiple-value-bind (a b) (andor3::%get-enum-index *c* s)
      (assert (= +success+ a))
      b)))
#+nil
(get-enum-index "TriggerMode")

(defun get-enum-string-by-index (feature index)
  (with-wide-strings ((s feature))
   (let* ((n 512)
	  (a (make-array (1+ n) :element-type '(unsigned-byte 32))))
     (assert (= +success+
		(andor3::%get-enum-string-by-index
		 *c* s index (sb-sys:vector-sap a) n)))
     (andor3::wide-char->char a))))

#+nil
(get-enum-string-by-index "TriggerMode" 6)

(defun get-enum-string (feature)
  (get-enum-string-by-index feature
			    (get-enum-index feature)))
#+nil
(get-enum-string "TriggerMode")

(defun set-enum-string (feature value)
  (declare (type string feature value))
  (with-wide-strings ((s feature)
		      (v value))
    (assert (= +success+
	       (andor3::%set-enum-string *c* s v))))
  (get-enum-string feature))
#+nil
(set-enum-string "TriggerMode" "External")


#+nil
(get-enum-string "ElectronicShutteringMode")

#+nil
(set-enum-string "ElectronicShutteringMode" "Global")

(defun init ()
  (set-enum-string "ElectronicShutteringMode" "Global")
 #+nil (write-enum-string "TriggerMode" "External"))

#+nil
(andor3::%initialise-library)
#+nil
(defparameter *c* (camera-open))
#+nil
(set-recommended-roi528)
#+nil
(progn 
  (queue-buffer-into-hash)
  nil)
#+nil
(command "AcquisitionStart")
#+nil
(defparameter *blasd* (wait-buffer))



#+nil
(command "AcquisitionStop")


#+nil
(flush)
#+nil
(assert (= +success+
	 (andor3::%finalise-library)))