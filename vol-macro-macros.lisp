;; utilities that are used in vol to write macros
;; mostly its used to write functions for different ranks or types

(in-package :vol)


(let ((type-names '(((complex double-float) . cdf)
		    ((complex single-float) . csf)
		    (double-float . df)
		    (single-float . sf))))
  (defun get-short-type (long-type)
    (cdr (assoc long-type type-names)))
  (defun get-long-type (short-type)
    (car (rassoc short-type type-names))))

#+nil
(get-long-type 'df)

(defmacro format-symbol (fmt &rest rest)
  `(intern (string-upcase (format nil ,fmt ,@rest))))

(defparameter *macro-generated-functions* nil)