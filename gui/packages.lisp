(defpackage :gui
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut :vector)
  (:export #:fenster
	   #:with-gui
	   #:VERTEX-V
	   #:TEX-COORD-V
	   #:TRANSLATE-V
	   #:NORMAL-V
	   #:SCALE-V
	   #:texture-3-luminance-ub8
	   #:destroy
	   #:bind-tex
	   #:draw-axes
	   #:draw-xz))
