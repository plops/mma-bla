(asdf:defsystem frontend
  :depends-on (:alexandria :vector :vol :psf :simplex-anneal :raytrace :lens
			   :bresenham :gui)
  :components ((:module "frontend" :serial t
			:components ((:file "package")
				     (:file "draw")
				     (:file "angular-psf")
				     (:file "model")
				     (:file "simulate-angular")
				     (:file "merit")))))
