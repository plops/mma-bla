(asdf:defsystem acquisitor
  :depends-on (:cl-opengl :focus :sb-concurrency :clara)
  :components ((:module "acquisitor" :serial t :components ((:file "package")
							    (:file "planner")))))
