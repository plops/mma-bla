;; macros to handle volumes

(in-package :vol)

(defmacro with-arrays (arrays &body body)
  "Provides a corresponding accessor for each array as a local macro,
so that (ARRAY ...) corresponds to (AREF ARRAY ...)."
  `(macrolet ,(mapcar (lambda (array)
                        `(,array (&rest indices) `(aref ,',array ,@indices)))
                      arrays)
     ,@body))

(defmacro with-linear-arrays (arrays &body body)
  "Exposes the linear array for each of the supplied arrays. When a
and b are multidimensional arrays you can use (with-arrays (a
b) (setf (a1 0) (b1 324))) to access their storage vectors."
  (let* ((arrays1 (mapcar #'(lambda (x) 
			      (unless (symbolp x)
			       (error "only symbols are allowed in with1."))
			      (intern (format nil "~a1" x)))
			  arrays)))
   `(let (,@(mapcar #'(lambda (x y) `(,x (sb-ext:array-storage-vector ,y)))
		    arrays1 arrays))
      (with-arrays ,arrays1
	,@body))))

(defmacro do-region ((indices end &optional (start '(0 0 0))) &body body)
  "Write intertwined loops to traverse a vector, an image or a volume."
  (unless (and (= (length indices)
		  (length end)))
    (error "Number of indices and interval-ends are not equal."))
  (labels ((rec (ind end start acc) ;; several loops
             (if (null end)
                 acc
                 (rec (cdr ind) (cdr end) (cdr start)
                      `((loop for ,(car ind) from ,(car start) 
                           below ,(car end) do ,@acc))))))
    (first (rec (reverse indices) ;; first index is outermost loop
		(reverse end)
		(reverse start) body))))
#+nil
(let ((sum 0))
  (do-region ((k j i) (4 4 5))
    (incf sum (+ k j i)))
  sum)


(defmacro with-slice ((slice-array array slice-nr) &body body)
  "Returns SLICE-NRth slice of ARRAY as the 2D SLICE-ARRAY."
  (alexandria:with-gensyms (x y z)
    `(destructuring-bind (,z ,y ,x)
	 (array-dimensions ,array)
       (when (or (< ,slice-nr 0) (<= ,z ,slice-nr))
	 (error "slice-nr=~d out of range [0,~d]" ,slice-nr (1- ,z)))
       (let* ((,slice-array (make-array (list ,y ,x)
					:element-type '(unsigned-byte 8)
					:displaced-to ,array
					:displaced-index-offset (* ,slice-nr ,x ,y))))
	 ,@body))))