(asdf:defsystem raytrace
  :depends-on (:vector)
  :components
  ((:module "raytrace" :components ((:file "package")
				    (:file "model")
				    (:file "raytrace")))))
