(in-package #:roguelike)


(ecs:defcomponent health
  (max 0 :type fixnum)
  (points max :type fixnum))

(defun attack (attacker target)
  (unless (has-attack-p attacker) ;; souls-liek rules baby
    (log-message "~@(~a~) ~a at ~a."
                 (character-name attacker) (verb "swing" attacker)
                 (character-name target))
    (make-attack attacker :target target)))

(defun damage (target value)
  (with-health () target
    (decf points value)))

(defun maybe-hit (attacker accuracy min-damage max-damage)
  (with-attack () attacker
    (with-defense () target
      (let ((attacker-name (character-name attacker))
            (target-name (character-name target)))
        (if (> accuracy evasion)
            (if (< (rating->chance dodge) (random 1.0))
                (if (< block-chance (random 1.0))
                    (let* ((damage (random-from-range min-damage max-damage))
                           (reduction (/ armor (+ armor (* 10.0 damage))))
                           (damage-dealt (floor (* damage (- 1.0 reduction)))))
                      (log-message "~@(~a~) ~a ~a melee damage to ~a."
                                   attacker-name (verb "deal" attacker)
                                   damage-dealt target-name)
                      (damage target damage-dealt))
                    (log-message "~@(~a~) ~a an attack by ~a."
                                 target-name (verb "block" target)
                                 attacker-name))
                (log-message "~@(~a~) ~a an attack by ~a."
                             target-name (verb "dodge" target)
                             attacker-name))
            (log-message "~@(~a~) ~a ~a." attacker-name (verb "miss" attacker)
                         target-name))))))

(ecs:defsystem perform-melee-attacks
  (:components-rw (melee attack character position)
   :enable *turn*
   :arguments ((dt single-float)))
  (incf attack-elapsed dt)
  (when (>= attack-elapsed melee-duration)
    (with-position (target-x target-y) attack-target
      (if (and (approx-equal position-x target-x melee-range)
               (approx-equal position-y target-y melee-range)
               (has-health-p attack-target))
          (maybe-hit entity melee-accuracy melee-min-damage melee-max-damage)
          (log-message "~@(~a~) only ~a through the air."
                       character-name (verb "slice" entity))))
    (delete-attack entity)))

(ecs:defsystem demise-characters
  (:components-ro (health character sprite)
   :when (not (plusp health-points)))
  (log-message "~@(~a~) ~a." character-name (verb "die" entity))
  (delete-health entity)
  (change-sprite entity (format-symbol :keyword "~a-CORPSE" sprite-name)))
