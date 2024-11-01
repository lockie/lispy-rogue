(in-package #:roguelike)


(defun attack (attacker target)
  (unless (has-attack-p attacker) ;; souls-liek rules baby
    (cond
      ((has-melee-p attacker)
       (log-message "~@(~a~) ~a at ~a."
                    (character-name attacker) (verb "swing" attacker)
                    (character-name target))
       (make-attack attacker :target target))

      ((has-ranged-p attacker)
       (log-message "~@(~a~) ~a the bowstring, aiming at ~a."
                    (character-name attacker) (verb "pull" attacker)
                    (character-name target))
       (make-attack attacker :target target)))))

(defun damage (target value)
  (with-health () target
    (decf points value)))

(defun area-damage (col row value)
  (loop :for x :from (max 0.0 (- col +tile-size+))
        :to (min +world-width+ (+ col +tile-size+)) :by +tile-size+
        :nconcing
           (loop :for y :from (max 0.0 (- row +tile-size+))
                 :to (min +world-height+ (+ row +tile-size+)) :by +tile-size+
                 :for tiles := (tiles (a*:encode-float-coordinates x y))
                 :for character := (loop :for tile :in tiles
                                         :when (has-character-p tile)
                                         :return tile)
                 :when character :do (damage character value)
                 :when character :collect character)))

(defun maybe-hit (attacker target accuracy min-damage max-damage)
  (with-defense () target
    (let ((attacker-name (character-name attacker))
          (target-name (character-name target)))
      (if (> accuracy evasion)
          (if (< block-chance (random 1.0))
              (let* ((damage (random-from-range min-damage max-damage))
                     (reduction (/ armor (+ armor (* 10.0 damage))))
                     (damage-dealt (floor (* damage (- 1.0 reduction)))))
                (log-message "~@(~a~) ~a ~a physical damage to ~a."
                                 attacker-name (verb "deal" attacker)
                                 damage-dealt target-name)
                (damage target damage-dealt))
              (log-message "~@(~a~) ~a an attack by ~a."
                           target-name (verb "block" target)
                           attacker-name))
          (log-message "~@(~a~) ~a ~a." attacker-name (verb "miss" attacker)
                       target-name)))))

(ecs:defsystem perform-melee-attacks
  (:components-rw (attack)
   :components-ro (melee character offense tile)
   :enable *turn*
   :arguments ((dt single-float)))
  (incf attack-elapsed dt)
  (when (>= attack-elapsed offense-duration)
    (with-tile (target-col target-row) attack-target
      (if (and (approx-equal tile-col target-col offense-range)
               (approx-equal tile-row target-row offense-range)
               (has-health-p attack-target))
          (maybe-hit entity attack-target offense-accuracy
                     offense-min-damage offense-max-damage)
          (log-message "~@(~a~) only ~a through the air."
                       character-name (verb "slice" entity))))
    (delete-attack entity)))

(ecs:defsystem perform-ranged-attacks
  (:components-rw (attack)
   :components-ro (ranged character offense tile)
   :enable *turn*
   :arguments ((dt single-float)))
  (incf attack-elapsed dt)
  (when (>= attack-elapsed offense-duration)
    (with-tile (target-col target-row) attack-target
      (if (and (approx-equal tile-col target-col offense-range)
               (approx-equal tile-row target-row offense-range))
          (when-let ((object (object/firing-line tile-col tile-row
                                                 target-col target-row)))
            (if (has-map-tile-p object)
                (log-message "The arrow from ~a deflects off the wall."
                             character-name)
                (maybe-hit entity object offense-accuracy
                           offense-min-damage offense-max-damage))))
      (log-message "The arrow from ~a sticks into the ground."
                   character-name))
    (delete-attack entity)))

(ecs:defsystem demise-characters
  (:components-ro (health sprite)
   :components-rw (character)
   :when (not (plusp health-points)))
  (log-message "~@(~a~) ~a." character-name (verb "die" entity))
  (delete-health entity)
  (when (has-attack-p entity)
    (delete-attack entity))
  (setf character-name (if (has-player-p entity)
                           "your body"
                           (format nil "the corpse of ~a" character-name)))
  (change-sprite
   entity
   (format-symbol :keyword "~a-CORPSE"
                  (first (split "-" (string sprite-name) :limit 2)))))
