(defpackage :gui
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut)
  (:export #:fenster
	   #:with-gui))