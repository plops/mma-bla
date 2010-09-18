(asdf:defsystem mma
  :components ((:module "mma"
			:serial t
			:components ((:file "ffi")
				     (:file "ipms")))))