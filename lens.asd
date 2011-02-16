(asdf:defsystem lens 
  :depends-on (:alexandria :vector)
  :components ((:module "lens" :serial t
			:components ((:file "package")
				     (:file "objects")
				     (:file "helpers")
				     (:file "lens")))))
