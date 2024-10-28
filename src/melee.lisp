(in-package #:roguelike)


(ecs:defcomponent melee
  (range (1+ +tile-size+) :type single-float)
  (duration 0.0 :type single-float)
  (accuracy 0.0 :type single-float)
  (min-damage 0.0 :type single-float)
  (max-damage 0.0 :type single-float))
