(in-package #:roguelike)


(ecs:defcomponent item
  (name "" :type simple-string)
  (owner -1 :type ecs:entity :index items))

(ecs:defcomponent item-health-potion
  (points 0 :type fixnum))

(ecs:defcomponent item-fireball-scroll
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
   :enable (and (not *message-log-focused*)
                (not *inventory-shown*)
                (not *targeting*)))
  (when (plusp health-points)
    (al:with-current-keyboard-state keyboard-state
      (if (keys-down keyboard-state :G :comma)
          (block key-pressed
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
                (progn (setf (item-owner item) entity
                             (parent-entity item) entity)
                       (log-message "You pick up ~a." (item-name item)))
                (log-message "There is nothing to pick up."))))
          (setf *pickup-key-pressed* nil)))))

(defun use-item (item x y)
  (cond
    ((has-item-health-potion-p item)
     (let ((potion-points (item-health-potion-points item)))
       (with-health () (player-entity 1)
         (if (= points max)
             (log-message "You are already at full health.")
             (block do-drink
               (setf points (min (+ points potion-points) max))
               (ecs:delete-entity item)
               (log-message "You drink health potion, restoring ~a points."
                            potion-points))))))

    ((has-item-fireball-scroll-p item)
     (if (and x y)
         (loop :initially (log-message "The fireball explodes.")
               :with damage := (item-fireball-scroll-damage item)
               :for character :in (area-damage x y damage)
               :do (log-message "~@(~a~) ~a burned for ~a damage."
                                (character-name character)
                                (verb "get" character) damage)
               :finally (ecs:delete-entity item))
         (start-targeting item)))

    (t
     (log-message "You don't know how to use ~a." (item-name item)))))

(defun make-health-potion (points x y)
  (let ((object (make-sprite-object :health-potion x y)))
    (make-item object :name "the health potion")
    (make-item-health-potion object :points points)
    object))

(defun make-fireball-scroll (damage x y)
  (let ((object (make-sprite-object :scroll x y)))
    (make-item object :name "the fireball scroll")
    (make-item-fireball-scroll object :damage damage)
    object))
