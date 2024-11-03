(in-package #:roguelike)


(ecs:defcomponent enemy
  (level 0 :type fixnum)
  (xp 0 :type fixnum))

(ecs:defsystem chase-player
  (:components-ro (position tile enemy health offense)
   :components-rw (character)
   :with ((player player-x player-y) := (let ((player (player-entity 1)))
                                          (with-position () player
                                            (values player x y))))
   :enable (and *turn* (has-health-p player)))
  (cond
    ((and (approx-equal position-x player-x offense-range)
          (approx-equal position-y player-y offense-range))

     (attack entity player))

    ((and (approx-equal position-x player-x character-vision-range)
          (approx-equal position-y player-y character-vision-range)
          (not (and (has-path-p entity)
                    (approx-equal (path-destination-x entity) player-x
                                  (* 2 +tile-size+))
                    (approx-equal (path-destination-y entity) player-y
                                  (* 2 +tile-size+)))))
     (with-tile (player-col player-row) player
       (let+ (((&values target-x target-y)
               ;; TODO check LoS for ranged?
               (nearest-tile tile-col tile-row player-col player-row)))
         (when (blocked entity target-x target-y)
           (setf target-x player-col
                 target-y player-row))
         (find-path tile-col tile-row target-x target-y
                    :entity entity :player player))))))

(defun make-melee-enemy-object (sprite name x y)
  (let ((object (make-sprite-object sprite x y)))
    (make-character object :name name :base-speed 50.0 :vision-range 100.0)
    (make-enemy object :xp 10)
    (make-health object :base-max 20)
    (make-defense object :base-evasion 10.0 :base-block-chance 0.1 :base-armor 5.0)
    (make-offense object :range (* 1.5 +tile-size+) :min-damage 5.0 :max-damage 10.0 :accuracy 25.0 :duration 0.3)
    (make-melee object)
    object))

(defun make-ranged-enemy-object (sprite name x y)
  (let ((object (make-sprite-object sprite x y)))
    (make-character object :name name :base-speed 50.0 :vision-range 100.0)
    (make-enemy object :xp 20)
    (make-health object :base-max 20)
    (make-defense object :base-evasion 10.0 :base-block-chance 0.05 :base-armor 1.0)
    (make-offense object :range 85.0 :min-damage 10.0 :max-damage 15.0 :accuracy 30.0 :duration 0.6)
    (make-ranged object)
    object))
