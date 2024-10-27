(in-package #:roguelike)


(ecs:defcomponent character
  (speed 0.0 :type single-float)
  (target-x single-float-nan :type single-float)
  (target-y single-float-nan :type single-float))

(ecs:defsystem draw-map-sprites
  (:components-ro (tile sprite)
   :components-no (character)
   :after (set-tile)
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (al:draw-bitmap sprite-bitmap tile-col tile-row 0))

(ecs:defsystem draw-character-sprites
  (:components-ro (tile sprite character)
   :after (set-tile draw-map-sprites)
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (al:draw-bitmap sprite-bitmap tile-col tile-row 0))

(ecs:defsystem move-characters
  (:components-rw (position character)
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
        (if (blocked new-x new-y)
            (setf character-target-x position-x
                  character-target-y position-y)
            (setf position-x new-x
                  position-y new-y)))))
