(defpackage :clara
  (:use :cl :sb-alien :sb-c-call)
  (:export
   #:init-fast
   #:wait-for-image-and-copy
   #:status
   #:stop
   #:uninit
   #:*im*))
