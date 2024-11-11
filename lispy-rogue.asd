(defsystem "lispy-rogue"
  :version "0.0.2"
  :author "Andrew Kravchuk"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-astar
               #:cl-fast-ecs
               #:cl-liballegro
               #:cl-liballegro-nuklear
               #:cl-liballegro-nuklear/declarative
               #:cl-ppcre
               #:cl-tiled
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "common")
                 (:file "sprites")
                 (:file "fov")
                 (:file "characters")
                 (:file "melee")
                 (:file "ranged")
                 (:file "enemies")
                 (:file "fights")
                 (:file "player")
                 (:file "items")
                 (:file "equipment")
                 (:file "ui")
                 (:file "map")
                 (:file "targeting")
                 (:file "main"))))
  :description "An Autumn Lisp Game Jam 2024 entry"
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"lispy-rogue"
  :entry-point "lispy-rogue:main")
