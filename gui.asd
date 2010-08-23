(asdf:defsystem gui
  :depends-on (:cl-opengl :cl-glut :cl-glu)
  :components ((:module "gui"
			:serial t
			:components
			((:file "packages")
			 (:file "gui")))))