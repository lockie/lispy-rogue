(in-package #:roguelike)


(ecs:defcomponent player
  (player 1 :type bit :index player-entity :unique t))

(defmacro keys-down (state &rest keys)
  `(or ,@(mapcar (lambda (key) `(al:key-down ,state ,key)) keys)))

(declaim (type boolean *move-key-pressed*))
(defparameter *move-key-pressed* nil)

(ecs:defsystem control-player
  (:components-ro (player health position tile)
   :components-rw (character)
   :enable (not *message-log-focused*)
   :after (move-characters))
  (when (plusp health-points)
   (al:with-current-keyboard-state keyboard-state
     (let ((dx 0) (dy 0))
       (when (keys-down keyboard-state :up    :W :K) (setf dy -1.0))
       (when (keys-down keyboard-state :down  :S :J) (setf dy +1.0))
       (when (keys-down keyboard-state :left  :A :H) (setf dx -1.0))
       (when (keys-down keyboard-state :right :D :L) (setf dx +1.0))

       (if (and (zerop dx) (zerop dy))
           (setf *move-key-pressed* nil)
           (unless *move-key-pressed*
             (let ((target-x (clamp (+ tile-col (* dx +tile-size+))
                                    0.0 (- +world-width+ +tile-size+)))
                   (target-y (clamp (+ tile-row (* dy +tile-size+))
                                    0.0 (- +world-height+ +tile-size+))))
               (if-let (target-character (live-character-at target-x target-y))
                 (attack entity target-character)
                 (setf character-target-x target-x
                       character-target-y target-y))
               (setf *turn* t
                     *move-key-pressed* t))))))))

(ecs:defsystem stop-turn
  (:components-ro (player character position)
   :components-no (attack))
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
