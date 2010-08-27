(asdf:defsystem vol
  :depends-on (:alexandria :vector)
  :components ((:module "vol"
			:serial t
			:components
			((:file "package")
			 (:file "macro-macros")
			 (:file "macros")
			 (:file "convert")
			 (:file "interpolation")
			 (:file "pgm")
			 (:file "operators")
			 (:file "misc")
			 (:file "bbox")
			 (:file "fft-helper")
			 (:file "fftw-fft")
			 ;; (:file "cuda-fft")
			 (:file "draw")
			 (:file "convolve")))))
