(in-package #:lispy-rogue)


(ecs:defcomponent path-point
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (traveller -1 :type ecs:entity :index path-points))

(ecs:defcomponent path
  (destination-x 0.0 :type single-float)
  (destination-y 0.0 :type single-float))

(ecs:defsystem follow-path
  (:components-ro (path position)
   :components-rw (character)
   :enable *turn*)
  (if-let (first-point (first (path-points entity :count 1)))
    (with-path-point (point-x point-y) first-point
      (if (and (approx-equal position-x point-x)
               (approx-equal position-y point-y))
          (ecs:delete-entity first-point)
          (setf character-target-x point-x
                character-target-y point-y)))
    (delete-path entity)))

;; TODO dont draw corpse if there's character on top?
(ecs:defsystem draw-character-sprites
  (:components-ro (tile sprite character)
   :when (or (has-player-p entity) (lit tile-col tile-row))
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (al:draw-bitmap sprite-bitmap tile-col tile-row 0))

(ecs:defsystem move-characters
  (:components-rw (position tile character health)
   :components-no (attack)
   :when (plusp health-points)
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

(a*:define-path-finder find-path (entity player)
    (:variables ((world-width  (floor +world-width+  +tile-size+))
                 (world-height (floor +world-height+ +tile-size+)))
     :world-size (* world-width world-height)
     :indexer (a*:make-row-major-indexer world-width :node-width  +tile-size+
                                                     :node-height +tile-size+)
     :goal-reached-p a*:goal-reached-exact-p
     :neighbour-enumerator (a*:make-8-directions-enumerator
                            :node-width  +tile-size+
                            :node-height +tile-size+
                            :max-x +world-width+
                            :max-y +world-height+)
     :exact-cost (lambda (x1 y1 x2 y2)
                   (if (or (blocked player x2 y2)
                           (and (/= x1 x2)
                                (/= y1 y2)
                                (or (blocked player x1 y2)
                                    (blocked player x2 y1))))
                       most-positive-single-float
                       0.0))
     :heuristic-cost (a*:make-octile-distance-heuristic)
     :path-initiator (lambda (length)
                       (declare (ignorable length))
                       (when (has-path-p entity)
                         (dolist (point (path-points entity))
                           (ecs:delete-entity point)))
                       (assign-path entity :destination-x goal-x
                                           :destination-y goal-y))
     :path-processor (lambda (x y)
                       (ecs:make-object
                        `((:path-point :x ,x :y ,y :traveller ,entity)
                          (:parent :entity ,entity))))))
