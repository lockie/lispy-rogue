(in-package #:roguelike)


(ecs:defcomponent player
  (player 1 :type bit :index player-entity :unique t))

(defmacro keys-down (state &rest keys)
  `(or ,@(mapcar (lambda (key) `(al:key-down ,state ,key)) keys)))

(ecs:defsystem control-player
  (:components-ro (player position tile)
   :components-rw (character)
   :after (move-characters))
  (al:with-current-keyboard-state keyboard-state
    (let ((dx 0) (dy 0))
      (when (keys-down keyboard-state :up    :W :K) (setf dy -1.0))
      (when (keys-down keyboard-state :down  :S :J) (setf dy +1.0))
      (when (keys-down keyboard-state :left  :A :H) (setf dx -1.0))
      (when (keys-down keyboard-state :right :D :L) (setf dx +1.0))

      (unless (and (zerop dx) (zerop dy))
        (setf character-target-x (clamp (+ tile-col (* dx +tile-size+))
                                        0.0 (- +world-width+ +tile-size+))
              character-target-y (clamp (+ tile-row (* dy +tile-size+))
                                        0.0 (- +world-height+ +tile-size+))
              *turn* t)))))

(ecs:defsystem stop-turn
  (:components-ro (player character position))
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
    (make-character object :speed 50.0 :vision-range 100.0)
    (make-player object)))
