(asdf:defsystem run
  :depends-on (:alexandria :vector :vol :psf :simplex-anneal :raytrace :lens
			   :bresenham)
  :serial t
  :components ((:file "run")))
