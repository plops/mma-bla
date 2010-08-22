;; utilities that are used in vol to write macros
;; mostly its used to write functions for different ranks or types

(in-package :vol)


(let ((type-names '(((complex double-float) . cdf)
		    ((complex single-float) . csf)
		    (double-float . df)
		    (single-float . sf)
		    (fixnum . fix)
		    ((signed-byte 16) . sb16)
		    ((unsigned-byte 16) . ub16)
		    ((unsigned-byte 8) . ub8))))
  (defun get-short-type (long-type)
    (cdr (assoc long-type type-names :test #'equal)))
  (defun get-long-type (short-type)
    (car (rassoc short-type type-names))))

#+nil
(get-long-type 'df)

(defmacro format-symbol (fmt &rest rest)
  `(intern (string-upcase (format nil ,fmt ,@rest))))

(defparameter *macro-generated-functions* nil)
(defun store-new-function (name)
  (push name *macro-generated-functions*))

;; macro that defines another macro that in turn will define functions
;; handling different ranks and types. should be called like this:
;; (def-generator (fftshift rank type) ... ). the name of the new
;; macro will be def-fftshift-rank-type and when it is invoked the
;; generated function name will be pushed into
;; *macro-generated-functions*. this can be used to build the list of
;; exported symbols. if spec contains the symbol type, than a
;; long-type will be generated. the variable name should be used to
;; define the name of the function. here is an example: 

;; (def-generator (fftshift (rank type))
;;   `(defun ,name (in)
;;      (declare ((simple-array ,long-type ,rank) in))))

;; now we can interpolate into a function with the following call

;; (def-fftshift-rank-type 2 sf)

;; the new function looks like this:

;; (DEFUN DEF-FFTSHIFT-2-SF (IN)
;;   (DECLARE ((SIMPLE-ARRAY SINGLE-FLOAT 2) IN)))

;; when a new macro is needed to generate a lot of functions (and
;; maybe one dispatch function) it should be called def-...-functions.

(defmacro def-generator ((name spec &key override-name) &body body)
  (let ((macro-name (format-symbol "def-~a~{-~a~}" name spec))
	(function-fmt (let ((result (format nil "~a" name)))
			(dotimes (i (length spec)) 
			  (setf result (concatenate 'string result (format nil "-~~a"))))
			result)))
    `(defmacro ,macro-name ,spec
       (let ((name (format-symbol ,function-fmt ,@spec))
	     ,(when (member 'type spec)
		    `(long-type (get-long-type type))))
	 ,(unless override-name `(store-new-function name))
	 ,@body))))

#+nil
(def-generator (fftshift (rank type))
  `(defun ,name (in)
     (declare ((simple-array ,long-type ,rank) in))))
#+nil
(def-fftshift-rank-type 2 sf)
#+nil
(fftshift-2-sf )

;; given two lists the following macros get the outer product, a 2d
;; matrix spanning all the combinations. everything is pushed into a
;; 1d stack whose value can be altered by the optional parameter
;; return-sexpr. for use in macros progn is a useful return-sexpr.

(defmacro def-outer-cross (la lb func &optional return-sexpr)
  (let ((result nil))
    (loop for a in la do
	 (loop for b in lb do
	      (push `(,func ,a ,b)
		    result)))
    (if return-sexpr
	`(,return-sexpr ,@result)
	`(,@result))))
#+nil
(def-outer-cross (1 2 3) (sf df csf cdf) def-fftshift-rank-type progn)
#+nil
(def-outer-cross (1 2 3) (1 2 3) def)



