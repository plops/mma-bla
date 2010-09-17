(defpackage :gui
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut)
  (:export #:fenster
	   #:with-gui
	   #:texture-luminance-ub8
	   #:destroy
	   #:bind-tex
	   #:draw-axes
	   #:draw-wire-box))
