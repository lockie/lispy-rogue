(in-package #:roguelike)


(ecs:defcomponent player
  (player 1 :type bit :index player-entity :unique t))

(ecs:defcomponent wait
  (elapsed 0.0 :type single-float))

(defmacro keys-down (state &rest keys)
  `(or ,@(mapcar (lambda (key) `(al:key-down ,state ,key)) keys)))

(declaim (type boolean *move-key-pressed*))
(defparameter *move-key-pressed* nil)

(ecs:defsystem control-player
  (:components-ro (player health position tile)
   :components-rw (character)
   :enable (and (not *message-log-focused*)
                (not *inventory-shown*)
                (not *throw-window-shown*)
                (not *targeting*))
   :after (move-characters))
  (when (plusp health-points)
    (let ((target-x nil) (target-y nil))
      (al:with-current-keyboard-state keyboard-state
        (let ((dx 0) (dy 0) (wait 0))
          ;; TODO https://roguebasin.com/index.php/Preferred_Key_Controls
          (when (keys-down keyboard-state :up    :W :K) (setf dy -1.0))
          (when (keys-down keyboard-state :down  :S :J) (setf dy +1.0))
          (when (keys-down keyboard-state :left  :A :H) (setf dx -1.0))
          (when (keys-down keyboard-state :right :D :L) (setf dx +1.0))
          (when (keys-down keyboard-state :space :R)    (setf wait 1))

          (if (and (zerop dx) (zerop dy) (zerop wait))
              (setf *move-key-pressed* nil)
              (unless *move-key-pressed*
                (when (and (zerop wait) (has-wait-p entity))
                  (delete-wait entity))
                (if (plusp wait)
                    (progn
                      (log-message "You stand still.")
                      (assign-wait entity))
                    (setf target-x (clamp (+ tile-col (* dx +tile-size+))
                                          0.0 (- +world-width+ +tile-size+))
                          target-y (clamp (+ tile-row (* dy +tile-size+))
                                          0.0 (- +world-height+ +tile-size+))))
                (setf *turn* t
                      *move-key-pressed* t)))))
      (al:with-current-mouse-state mouse-state
        (when (= 1 (mouse-state-buttons mouse-state))
          (setf target-x (round/tile-size
                          (- (mouse-state-x mouse-state) (/ +tile-size+ 2)))
                target-y (round/tile-size
                          (- (mouse-state-y mouse-state) (/ +tile-size+ 2)))
                *turn* t)))
      (when (and target-x target-y
                 (lit target-x target-y))
        (if-let (target-character (live-character-at target-x target-y))
          (unless (= target-character entity)
            ;; TODO check range
            (attack entity target-character))
          (setf character-target-x target-x
                character-target-y target-y))))))

(ecs:defsystem wait-turn
  (:components-ro (player)
   :components-rw (wait)
   :arguments ((dt single-float))
   :enable *turn*)
  (incf wait-elapsed dt)
  (when (>= wait-elapsed 1.0)
    (delete-wait entity)
    (setf *turn* nil)))

(ecs:defsystem stop-turn
  (:components-ro (player character position)
   :components-no (attack wait)
   :enable *turn*)
  (when (and (= position-x character-target-x)
             (= position-y character-target-y))
    (setf *turn* nil)))

(declaim (type fixnum *player-position-hash*))
(defvar *player-position-hash* -1)

(ecs:defsystem recalculate-fov
  (:components-ro (player character tile))
  (when (/= tile-hash *player-position-hash*)
    (setf *player-position-hash* tile-hash)
    (recalculate-fov tile-col tile-row character-vision-range)))

(defun make-player-object (x y)
  (let ((object (make-sprite-object :hero x y)))
    (make-character object :name "you" :speed 150.0 :vision-range 100.0)
    (make-player object)
    (make-health object :max 100)
    (make-defense object :evasion 10.0 :dodge 10.0 :block-chance 0.1 :armor 10.0)
    (make-melee object :min-damage 10.0 :max-damage 15.0 :accuracy 20.0 :duration 0.4)))
