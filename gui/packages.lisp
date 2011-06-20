(defpackage :gui
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut :vector)
  (:export #:fenster
	   #:with-gui
	   #:grating
	   #:destroy
	   #:bind
	   #:draw
	   #:draw-axes
	   #:draw-wire-box
	   #:texture
	   #:grating-stack
	   #:grating->texture
	   
	   #:VERTEX-V
	   #:TEX-COORD-V
	   #:TRANSLATE-V
	   #:NORMAL-V
	   #:SCALE-V
	   #:texture-luminance-ub8
	   #:draw-xz
	   #:get-frame-rate
	   #:draw-number))
