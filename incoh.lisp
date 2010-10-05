;; incoherent illumination

;; \int |(e^{i r1 r2} C2) \otimes a|^2 C1 d^2r1

#.(require :vol)

(defpackage :incoherent
  (:use :cl :vol))
(in-package :incoherent)

(defun csf (dims)
  (make-array dims :element-type '(complex single-float)))
