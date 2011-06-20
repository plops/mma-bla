(asdf:defsystem clara
  :components ((:module "clara" :serial t :components ((:file "package")
						       (:file "ffi")
						       (:file "clara")
						       (:file "queue")))))
