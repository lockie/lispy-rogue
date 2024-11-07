(in-package #:lispy-rogue)


(ecs:defcomponent item
  (name "" :type simple-string)
  (level 0 :type fixnum)
  (owner -1 :type ecs:entity :index items))

(ecs:defcomponent item-health-potion
  (points 0 :type fixnum))

(ecs:defcomponent item-fireball-scroll
  (mana-cost 0 :type fixnum)
  (damage 0 :type fixnum))

(ecs:defcomponent item-cripple-scroll
  (mana-cost 0 :type fixnum)
  (damage 0 :type fixnum))

(ecs:defsystem draw-item-sprites
  (:components-ro (tile sprite item)
   :when (and (not (ecs:entity-valid-p item-owner))
              (lit tile-col tile-row)
              (not (blocked -1 tile-col tile-row)))
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (al:draw-bitmap sprite-bitmap tile-col tile-row 0))

(declaim (type boolean *pickup-key-pressed*))
(defparameter *pickup-key-pressed* nil)

(ecs:defsystem pick-item
  (:components-ro (player health tile)
   :when (plusp health-points)
   :enable (and (not *message-log-focused*)
                (not *inventory-shown*)
                (not *targeting*)
                (not *levelup-shown*)
                (not *help-shown*)
                (not *won*))
   :arguments ((keyboard-state cffi:foreign-pointer)))
  (if (keys-down keyboard-state :G :comma)
      (unless *pickup-key-pressed*
        (setf *pickup-key-pressed* t)
        (if-let (item (loop :for tile
                            :in (tiles
                                 (a*:encode-float-coordinates tile-col
                                                              tile-row))
                            :when (and (has-item-p tile)
                                       (not (ecs:entity-valid-p
                                             (item-owner tile))))
                            :return tile))
          (if (length= +inventory-keys+ (items entity))
              (log-message "You're overburdened.")
              (progn
                (setf (item-owner item) entity
                      (parent-entity item) entity)
                (log-message "You pick up ~a." (item-name item))))
          (log-message "There is nothing to pick up here.")))
      (setf *pickup-key-pressed* nil)))

(defun use-item (item x y)
  (let ((player (player-entity 1)))
    (when (has-attack-p player)
      (delete-attack player))
    (cond
      ((and (has-equipped-p item)
          (has-ranged-p player) x y)
     (if-let (target-character (live-character-at x y))
       (progn
         (if (= target-character player)
             (log-message "You see nothing to shoot at.")
             (if (and (approx-equal x (tile-col player) (offense-range player))
                      (approx-equal y (tile-row player) (offense-range player)))
                 (progn
                   (attack player target-character)
                   (setf *turn* t))
                 (log-message "~@(~a~) is too far away for an attack."
                              (character-name target-character)))))
       (log-message "You see nothing to shoot at.")))

      ((has-equipment-p item)
       (toggle-equipped item))

      ((has-item-health-potion-p item)
       (let ((potion-points (item-health-potion-points item)))
         (with-health () player
           (if (= points max)
               (log-message "You are already at full health.")
               (block do-drink
                 (setf points (min (+ points potion-points) max))
                 (ecs:delete-entity item)
                 (log-message "You drink health potion, restoring ~a points."
                              potion-points))))))

      ((has-item-fireball-scroll-p item)
       (if (and x y)
           (if (>= (mana-points player) (item-fireball-scroll-mana-cost item))
               (loop :initially (decf (mana-points player)
                                      (item-fireball-scroll-mana-cost item))
                                (log-message "The fireball explodes.")
                     :with damage := (item-fireball-scroll-damage item)
                     :for character :in (area-damage x y damage)
                     :do (log-message "~@(~a~) ~a burned for ~a damage."
                                      (character-name character)
                                   (verb "get" character) damage)
                     :finally (ecs:delete-entity item))
               (log-message "You need ~a mana to cast fireball."
                            (item-fireball-scroll-mana-cost item)))
           (start-targeting item)))

      ((has-item-cripple-scroll-p item)
       (if (and x y)
           (if (>= (mana-points player) (item-cripple-scroll-mana-cost item))
               (if-let (target-character (live-character-at x y))
                 (with-character () target-character
                   (if (/= target-character player)
                       (progn
                         (setf speed (/ speed 2.0))
                         (with-health () target-character
                           (decf points (item-cripple-scroll-damage item))
                           (log-message "~@(~a~) gets crippled for ~a damage."
                                        (character-name target-character)
                                        (item-cripple-scroll-damage item)))
                         (ecs:delete-entity item))
                       (log-message "You see nothing to cast cripple at.")))
                 (log-message "You see nothing to cast cripple at."))
               (log-message "You need ~a mana to cast cripple."
                            (item-fireball-scroll-mana-cost item)))
           (start-targeting item)))

      (t
       (log-message "You don't know how to use ~a." (item-name item))))))

(defun make-health-potion (level points x y)
  (let ((object (make-sprite-object :health-potion x y)))
    (make-item object :name "the health potion" :level level)
    (make-item-health-potion object :points points)
    object))

(defun make-fireball-scroll (level damage x y)
  (let ((object (make-sprite-object :scroll x y)))
    (make-item object :name "the fireball scroll" :level level)
    (make-item-fireball-scroll object :damage damage :mana-cost 20)
    object))

(defun make-cripple-scroll (level damage x y)
  (let ((object (make-sprite-object :scroll x y)))
    (make-item object :name "the cripple scroll" :level level)
    (make-item-cripple-scroll object :damage damage :mana-cost 20)
    object))
