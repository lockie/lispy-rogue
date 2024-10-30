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
          *target-y* row)))

(declaim (type boolean *targeting-key-pressed*))
(defparameter *targeting-key-pressed* nil)

(define-constant +targeting-color+ '(al::r 1.0 al::g 1.0 al::b 1.0 al::a 1.0)
  :test #'equal)

(ecs:defsystem do-targeting
  (:components-ro (player)
   :after (draw-character-sprites draw-item-sprites draw-fow)
   :enable (and *targeting* (not *message-log-focused*)))
  (flet ((finish-targeting (&key cancel)
           (unless cancel
             (use-item *targeted-item* *target-x* *target-y*))
           (setf *targeting* nil
                 *targeted-item* -1
                 *target-x* single-float-nan
                 *target-y* single-float-nan)
           (return-from ecs:current-entity)))
    (al:with-current-keyboard-state keyboard-state
      (let ((dx 0) (dy 0))
        (when (keys-down keyboard-state :enter :F)
          (finish-targeting))
        (when (keys-down keyboard-state :escape)
          (finish-targeting :cancel t))
        ;; TODO https://roguebasin.com/index.php/Preferred_Key_Controls
        (when (keys-down keyboard-state :up    :W :K) (setf dy -1.0))
        (when (keys-down keyboard-state :down  :S :J) (setf dy +1.0))
        (when (keys-down keyboard-state :left  :A :H) (setf dx -1.0))
        (when (keys-down keyboard-state :right :D :L) (setf dx +1.0))
        (if (and (zerop dx) (zerop dy))
            (setf *targeting-key-pressed* nil)
            (unless *targeting-key-pressed*
              (setf  *target-x* (clamp (+ *target-x* (* dx +tile-size+))
                                       0.0 (- +world-width+ +tile-size+))
                     *target-y* (clamp (+ *target-y* (* dy +tile-size+))
                                       0.0 (- +world-height+ +tile-size+))
                     *targeting-key-pressed* t)
              (al:set-mouse-xy (al:get-current-display)
                               (floor (+ *target-x* (/ +tile-size+ 2)))
                               (floor (+ *target-y* (/ +tile-size+ 2))))))))
    (al:with-current-mouse-state mouse-state
      (setf *target-x*
            (round/tile-size (- (mouse-state-x mouse-state) (/ +tile-size+ 2)))
            *target-y*
            (round/tile-size (- (mouse-state-y mouse-state) (/ +tile-size+ 2))))
      (case (mouse-state-buttons mouse-state)
        (1 (finish-targeting))
        (2 (finish-targeting :cancel t))))
    (al:draw-rectangle *target-x* *target-y*
                       (+ *target-x* +tile-size+) (+ *target-y* +tile-size+)
                       +targeting-color+ 1.0)))
