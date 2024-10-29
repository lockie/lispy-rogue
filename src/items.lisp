(in-package #:roguelike)


(ecs:defcomponent item
  (name "" :type simple-string)
  (owner -1 :type ecs:entity :index items))

(ecs:defcomponent item-health-potion
  (points 0 :type fixnum))

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
   :enable (not *message-log-focused*))
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
                (progn (setf (item-owner item) entity)
                       (log-message "You pick up ~a." (item-name item)))
                (log-message "There's nothing to pick up."))))
          (setf *pickup-key-pressed* nil)))))

(defun make-health-potion (points x y)
  (let ((object (make-sprite-object :health-potion x y)))
    (make-item object :name "the health potion")
    (make-item-health-potion object :points points)))
