(asdf:defsystem :otf
  :depends-on (:qng)
  :components ((:module "otf" :components ((:file "package")
					   (:file "otf")))))
