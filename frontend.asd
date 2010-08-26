(asdf:defsystem frontend
  :depends-on (:alexandria :vector :vol :psf :simplex-anneal :raytrace :lens
			   :bresenham :gui)
  :components ((:module :sertial t
			:components ((:file "package")
				     (:file "draw")
				     (:file "angular-psf")))))
