(in-package #:roguelike)


(declaim (type ecs:entity *targeted-item*))
(defparameter *targeted-item* -1)

(declaim (type single-float *target-x* *target-y*))
(defparameter *target-x* single-float-nan)
(defparameter *target-y* single-float-nan)

(defun start-targeting (item)
  (with-tile () (player-entity 1)
    (setf *targeting* t
          *turn* nil
          *targeted-item* item
          *target-x* col
          *target-y* row)
    (al:set-mouse-xy (al:get-current-display)
                     (floor (+ col (/ +tile-size+ 2)))
                     (floor (+ row (/ +tile-size+ 2))))))

(declaim (type boolean *targeting-key-pressed*))
(defparameter *targeting-key-pressed* nil)

(define-constant +targeting-color+ '(al::r 1.0 al::g 1.0 al::b 1.0 al::a 1.0)
  :test #'equal)

(ecs:defsystem do-targeting
  (:components-ro (player)
   :after (draw-character-sprites draw-item-sprites draw-fow stop-turn)
   :enable (and *targeting*
                (not *message-log-focused*)
                (not *levelup-shown*)
                (not *help-shown*)))
  (flet ((finish-targeting (&key cancel)
           (unless cancel
             (if (lit *target-x* *target-y*)
                 (use-item *targeted-item* *target-x* *target-y*)
                 (log-message "You see nothing to aim at.")))
           (setf *targeting* nil
                 *targeted-item* -1
                 *target-x* single-float-nan
                 *target-y* single-float-nan)
           (return-from ecs:current-entity)))
    (al:with-current-keyboard-state keyboard-state
      (let ((dx 0) (dy 0) (finish nil) (cancel nil))
        (when (keys-down keyboard-state :enter :F)
          (setf finish t))
        (when (keys-down keyboard-state :escape)
          (setf cancel t))
        (when (keys-down keyboard-state :up    :W :K :pad-8) (setf dy -1.0))
        (when (keys-down keyboard-state :down  :S :J :pad-2) (setf dy +1.0))
        (when (keys-down keyboard-state :left  :A :H :pad-4) (setf dx -1.0))
        (when (keys-down keyboard-state :right :D :L :pad-6) (setf dx +1.0))
        (when (keys-down keyboard-state :Q :Y :pad-7) (setf dx -1.0 dy -1.0))
        (when (keys-down keyboard-state :E :U :pad-9) (setf dx +1.0 dy -1.0))
        (when (keys-down keyboard-state :Z :B :pad-1) (setf dx -1.0 dy +1.0))
        (when (keys-down keyboard-state :C :N :pad-3) (setf dx +1.0 dy +1.0))
        (if (and (zerop dx) (zerop dy) (not finish) (not cancel))
            (setf *targeting-key-pressed* nil)
            (unless *targeting-key-pressed*
              (setf  *target-x* (clamp (+ *target-x* (* dx +tile-size+))
                                       0.0 (- +world-width+ +tile-size+))
                     *target-y* (clamp (+ *target-y* (* dy +tile-size+))
                                       0.0 (- +world-height+ +tile-size+))
                     *targeting-key-pressed* t)
              (al:set-mouse-xy (al:get-current-display)
                               (floor (+ *target-x* (/ +tile-size+ 2)))
                               (floor (+ *target-y* (/ +tile-size+ 2))))
              (when finish
                (finish-targeting))
              (when cancel
                (finish-targeting :cancel t))))))
    (al:with-current-mouse-state mouse-state
      (setf *target-x*
            (round/tile-size (- (mouse-state-x mouse-state) (/ +tile-size+ 2)))
            *target-y*
            (round/tile-size (- (mouse-state-y mouse-state) (/ +tile-size+ 2))))
      (case (mouse-state-buttons mouse-state)
        (1 (setf *mouse-clicked* t) (finish-targeting))
        (2 (setf *mouse-clicked* t) (finish-targeting :cancel t))))
    (al:draw-rectangle *target-x* *target-y*
                       (+ *target-x* +tile-size+) (+ *target-y* +tile-size+)
                       +targeting-color+ 1.0)))
