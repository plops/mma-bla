(asdf:defsystem vol
  :depends-on (:alexandria :vector)
  :serial t
  :components ((:file "vol-package")
	       (:file "vol-macro-macros")
	       (:file "vol-macros")
	       (:file "vol-convert")
	       (:file "vol-interpolation")
	       (:file "vol-pgm")
	       (:file "vol-operators")
	       (:file "vol-misc")
	       (:file "vol-bbox")))
