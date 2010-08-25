(asdf:defsystem gui
  :depends-on (:cl-opengl :cl-glut :cl-glu :alexandria :vector)
  :components ((:module "gui"
			:serial t
			:components
			((:file "packages")
			 (:file "draw")
			 (:file "gui")))))