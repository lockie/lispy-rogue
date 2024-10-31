(in-package #:roguelike)


(ecs:defcomponent enemy)

(defun attack-range (entity)
  (cond
    ((has-melee-p entity)
     (melee-range entity))

    ((has-ranged-p entity)
     (ranged-range entity))))

(ecs:defsystem chase-player
  (:components-ro (position enemy health)
   :components-rw (character)
   :with ((player player-x player-y) := (let ((player (player-entity 1)))
                                          (with-position () player
                                            (values player x y))))
   :enable (and *turn* (has-health-p player)))
  (let ((attack-range (attack-range entity)))
    (cond
      ((and (approx-equal position-x player-x attack-range)
            (approx-equal position-y player-y attack-range))

       (attack entity player))

      ((and (approx-equal position-x player-x character-vision-range)
            (approx-equal position-y player-y character-vision-range))

       (let+ (((&values target-x target-y)
               (nearest-tile position-x position-y player-x player-y)))
         (setf character-target-x target-x
               character-target-y target-y))))))

(defun make-melee-enemy-object (sprite name x y)
  (let ((object (make-sprite-object sprite x y)))
    (make-character object :name name :speed 50.0 :vision-range 100.0)
    (make-enemy object)
    (make-health object :max 20)
    (make-defense object :evasion 10.0 :dodge 15.0 :block-chance 0.1 :armor 5.0)
    (make-melee object :min-damage 5.0 :max-damage 10.0 :accuracy 25.0 :duration 0.3)
    object))

(defun make-ranged-enemy-object (sprite name x y)
  (let ((object (make-sprite-object sprite x y)))
    (make-character object :name name :speed 50.0 :vision-range 100.0)
    (make-enemy object)
    (make-health object :max 20)
    (make-defense object :evasion 10.0 :dodge 20.0 :block-chance 0.05 :armor 1.0)
    (make-ranged object :range 85.0 :min-damage 10.0 :max-damage 15.0 :accuracy 30.0 :duration 0.4)
    object))
