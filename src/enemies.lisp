(in-package #:roguelike)


(ecs:defcomponent enemy
  (vision-range 0.0 :type single-float))

(defun attack-range (entity)
  ;; TODO ranged
  (melee-range entity))

(ecs:defsystem chase-player
  (:components-ro (position enemy)
   :components-rw (character)
   :with ((player-x player-y) := (with-position () (player-entity 1)
                                   (values x y)))
   :enable *turn*)
  (let ((attack-range (attack-range entity)))
    (when (and (and (approx-equal position-x player-x enemy-vision-range)
                    (approx-equal position-y player-y enemy-vision-range))
               (not (and (approx-equal position-x player-x attack-range)
                         (approx-equal position-y player-y attack-range))))
      (let+ (((&values target-x target-y) (nearest-tile position-x position-y
                                                        player-x player-y)))
        (setf character-target-x target-x
              character-target-y target-y)))))

(defun make-enemy-object (sprite x y)
  (let ((object (make-sprite-object sprite x y)))
    (make-character object :speed 50.0)
    (make-enemy object :vision-range 100.0)
    (make-melee object :min-damage 1.0 :max-damage 10.0)))
