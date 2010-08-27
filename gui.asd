(asdf:defsystem gui
  :depends-on (:cl-opengl :cl-glut :cl-glu :alexandria :vector :lens)
  :components ((:module "gui"
			:serial t
			:components
			((:file "packages")
			 (:file "draw")
			 (:file "gui")))))