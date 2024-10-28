(in-package #:roguelike)


(ecs:defsystem draw-character-sprites
  (:components-ro (tile sprite character)
   :when (or (has-player-p entity) (lit tile-col tile-row))
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (al:draw-bitmap sprite-bitmap tile-col tile-row 0))

(ecs:defsystem move-characters
  (:components-rw (position tile character)
   :arguments ((dt single-float))
   :enable *turn*)
  (when (or (float-nan-p character-target-x)
            (float-nan-p character-target-y))
    (setf character-target-x position-x
          character-target-y position-y))
  (if (and (approx-equal position-x character-target-x)
           (approx-equal position-y character-target-y))
      (setf character-target-x position-x
            character-target-y position-y)
      (let* ((angle (atan (- character-target-y position-y)
                          (- character-target-x position-x)))
             (dx (* character-speed dt (cos angle)))
             (dy (* character-speed dt (sin angle)))
             (new-x (+ position-x dx))
             (new-y (+ position-y dy)))
        (if (blocked entity new-x new-y)
            (setf character-target-x position-x
                  character-target-y position-y)
            (setf position-x new-x
                  position-y new-y
                  tile-col (round/tile-size position-x)
                  tile-row (round/tile-size position-y)
                  tile-hash (a*:encode-float-coordinates tile-col tile-row))))))
