(defsystem "roguelike"
  :version "0.0.1"
  :author "Andrew Kravchuk"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-astar
               #:cl-fast-ecs
               #:cl-liballegro
               #:cl-liballegro-nuklear
               #:cl-tiled
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "common")
                 (:file "sprites")
                 (:file "fov")
                 (:file "map")
                 (:file "characters")
                 (:file "melee")
                 (:file "enemies")
                 (:file "player")
                 (:file "main"))))
  :description "An Autumn Lisp Game Jam 2024 entry"
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"roguelike"
  :entry-point "roguelike:main")
