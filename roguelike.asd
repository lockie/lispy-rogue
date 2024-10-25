(defsystem "roguelike"
  :version "0.0.1"
  :author "Andrew Kravchuk"
  :license "MIT"
  :depends-on (#:alexandria
               
               #:cl-liballegro
               #:cl-liballegro-nuklear
               
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "main"))))
  :description "An Autumn Lisp Game Jam 2024 entry"
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"roguelike"
  :entry-point "roguelike:main")
