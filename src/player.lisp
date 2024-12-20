(in-package #:lispy-rogue)


(ecs:defcomponent wait
  (elapsed 0.0 :type single-float))

(defmacro keys-down (state &rest keys)
  `(or ,@(mapcar (lambda (key) `(al:key-down ,state ,key)) keys)))

(declaim (type boolean *move-key-pressed* *mouse-clicked*))
(defparameter *move-key-pressed* nil)
(defparameter *mouse-clicked* nil)

(ecs:defsystem control-player
  (:components-ro (player health offense position tile)
   :components-rw (character)
   :when (plusp health-points)
   :enable (and (not *message-log-focused*)
                (not *inventory-shown*)
                (not *throw-window-shown*)
                (not *targeting*)
                (not *levelup-shown*)
                (not *help-shown*)
                (not *won*))
   :after (move-characters)
   :arguments ((keyboard-state cffi:foreign-pointer)))
  (let ((target-x nil) (target-y nil)
        (dx 0.0) (dy 0.0) (wait 0))
    (declare (type single-float dx dy))
    (when (keys-down keyboard-state :up    :W :K :pad-8) (setf dy -1.0))
    (when (keys-down keyboard-state :down  :S :J :pad-2) (setf dy +1.0))
    (when (keys-down keyboard-state :left  :A :H :pad-4) (setf dx -1.0))
    (when (keys-down keyboard-state :right :D :L :pad-6) (setf dx +1.0))
    (when (keys-down keyboard-state :Q :Y :pad-7) (setf dx -1.0 dy -1.0))
    (when (keys-down keyboard-state :E :U :pad-9) (setf dx +1.0 dy -1.0))
    (when (keys-down keyboard-state :Z :B :pad-1) (setf dx -1.0 dy +1.0))
    (when (keys-down keyboard-state :C :N :pad-3) (setf dx +1.0 dy +1.0))
    (when (keys-down keyboard-state :space :R)    (setf wait 1))

    (if (and (zerop dx) (zerop dy) (zerop wait))
        (setf *move-key-pressed* nil)
        (unless *move-key-pressed*
          (when (and (zerop wait) (has-wait-p entity))
            (delete-wait entity))
          (if (plusp wait)
              (progn
                (log-message "You stand still.")
                (assign-wait entity)
                (setf *turn* t))
              (progn
                (setf target-x (clamp (+ tile-col (* dx +tile-size+)) 0.0
                                      (- +world-width+ +tile-size+))
                      target-y (clamp (+ tile-row (* dy +tile-size+)) 0.0
                                      (- +world-height+ +tile-size+)))
                (unless (live-character-at target-x target-y)
                  (log-message "You take a step ~a."
                               (cond ((and (plusp  dx) (plusp  dy))
                                      "southeast")
                                     ((and (plusp  dx) (minusp dy))
                                      "northeast")
                                     ((and (minusp dx) (plusp  dy))
                                      "southwest")
                                     ((and (minusp dx) (minusp dy))
                                      "northwest")
                                     ((plusp  dx)
                                      "east")
                                     ((minusp dx)
                                      "west")
                                     ((plusp  dy)
                                      "south")
                                     ((minusp dy)
                                      "north"))))))
          (setf *move-key-pressed* t)))
    (al:with-current-mouse-state mouse-state
      (if (= 1 (mouse-state-buttons mouse-state))
          (unless *mouse-clicked*
            (setf target-x (round/tile-size
                            (- (mouse-state-x mouse-state) (/ +tile-size+ 2)))
                  target-y (round/tile-size
                            (- (mouse-state-y mouse-state) (/ +tile-size+ 2)))
                  *mouse-clicked* t)
            (unless (live-character-at target-x target-y)
              (log-message "You take a few steps.")))
          (setf *mouse-clicked* nil)))
    (when (and target-x target-y
               (lit target-x target-y))
      (setf *turn* t)
      (if-let (target-character (live-character-at target-x target-y))
        (unless (= target-character entity)
          (if (and (approx-equal target-x tile-col offense-range)
                   (approx-equal target-y tile-row offense-range))
              (attack entity target-character)
              (log-message "~@(~a~) is too far away for an attack."
                           (character-name target-character))))
        (setf character-target-x target-x
              character-target-y target-y)))))

(declaim (type boolean *fire-key-pressed*))
(defparameter *fire-key-pressed* nil)

(ecs:defsystem target-ranged
  (:components-ro (player ranged health)
   :enable (and (not *message-log-focused*)
                (not *inventory-shown*)
                (not *throw-window-shown*)
                (not *targeting*)
                (not *help-shown*)
                (not *won*))
   :when (plusp health-points)
   :arguments ((keyboard-state cffi:foreign-pointer)))
  (if (al:key-down keyboard-state :F)
      (unless *fire-key-pressed*
        (setf *fire-key-pressed* t
              *targeting-key-pressed* t)
        (start-targeting (equipped :weapon)))
      (setf *fire-key-pressed* nil)))

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
    (make-character object :name "you"
                           :base-speed 100.0
                           :base-vision-range 90.0)
    (make-player object)
    (make-stats object :base-str 1 :base-dex 1 :base-int 1)
    (make-health object :base-max 100)
    (make-mana object :base-max 20)
    (make-defense object
                  :base-evasion 10.0
                  :base-block-chance 0.25
                  :base-armor 10.0)
    (make-offense object
                  :base-min-damage 1.0
                  :base-max-damage 10.0
                  :base-accuracy 10.0
                  :base-duration 0.4
                  :range (* 1.5 +tile-size+))
    (make-melee object)
    object))

(defun make-starting-equipment (player)
  (let ((starting-dagger (make-sprite-object :dagger 0.0 0.0)))
    (make-item starting-dagger :name "the dull dagger" :owner player)
    (make-equipment starting-dagger :slot :weapon :type :dagger)
    (make-bonus starting-dagger :accuracy-mult 0.5 :min-dmg-flat 5)
    (make-equipped starting-dagger :slot :weapon)
    (make-parent starting-dagger :entity player))
  (let ((starting-bow (make-sprite-object :bow 0.0 0.0)))
    (make-item starting-bow :name "the short bow" :owner player)
    (make-equipment starting-bow :slot :weapon :type :bow)
    (make-bonus starting-bow :accuracy-mult 0.5 :min-dmg-flat 5)
    (make-parent starting-bow :entity player))
  (recalculate-combat-parameters player))

(defun next-level-xp (current-level)
  (* 3 current-level current-level))

(ecs:defsystem level-up
  (:components-ro (health)
   :components-rw (player)
   :enable (not *levelup-shown*)
   :when (plusp health-points))
  (when (>= player-xp (next-level-xp player-level))
    (incf player-level)
    (log-message "You reach level ~a." player-level)
    (setf *turn* nil
          *levelup-shown* t)))

(ecs:defsystem do-levelup
  (:components-ro (player health)
   :components-rw (stats)
   :when (plusp health-points)
   :enable *levelup-shown*
   :arguments ((keyboard-state cffi:foreign-pointer)
               (ui-context cffi:foreign-pointer)))
  (when-let (stat (levelup ui-context keyboard-state))
    (setf *levelup-shown* nil)
    (case stat
      (0 (incf stats-base-str))
      (1 (incf stats-base-dex))
      (2 (incf stats-base-int)))
    (recalculate-combat-parameters entity)))

(ecs:defsystem regen-mana
  (:components-ro (player health)
   :components-rw (mana)
   :arguments ((dt single-float))
   :when (plusp health-points)
   :enable *turn*)
  (incf mana-regen-elapsed dt)
  (when (> mana-regen-elapsed 2.0)
    (when (< mana-points mana-max)
      (log-message "You regenerate mana.")
      (setf mana-regen-elapsed 0.0)
      (incf mana-points))))
