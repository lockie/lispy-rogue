(in-package #:roguelike)


(ecs:defsystem draw-map-sprites
  (:components-ro (tile sprite)
   :components-rw (view)
   :components-no (character)
   :before (draw-character-sprites)
   :when (or (plusp view-lit) (plusp view-explored))
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (when (zerop view-explored)
    (setf view-explored 1))
  (al:draw-bitmap sprite-bitmap tile-col tile-row 0))

(define-constant fog '(al::r 0.0 al::g 0.0 al::b 0.0 al::a 0.7) :test #'equalp)

(ecs:defsystem draw-fow
  (:components-ro (tile view)
   :after (draw-map-sprites)
   :when (and (not (plusp view-lit)) (plusp view-explored)))
  (al:draw-filled-rectangle tile-col
                            tile-row
                            (+ tile-col +tile-size+)
                            (+ tile-row +tile-size+)
                            fog))

(define-constant +room-min-size+ (* 5  +tile-size+))
(define-constant +room-max-size+ (* 10 +tile-size+))
(define-constant +room-max-monsters+ 3)
(define-constant +max-rooms+ 30)

(defstruct rect
  (x1 0.0 :type single-float)
  (y1 0.0 :type single-float)
  (x2 0.0 :type single-float)
  (y2 0.0 :type single-float))

(defun make-rect* (x y w h)
  (make-rect :x1 x
             :y1 y
             :x2 (+ x w)
             :y2 (+ y h)))

(defun center (rect)
  (values
   (/ (+ (rect-x1 rect) (rect-x2 rect)) 2.0)
   (/ (+ (rect-y1 rect) (rect-y2 rect)) 2.0)))

(defun intersect (rect1 rect2)
  (and (<= (rect-x1 rect1) (rect-x2 rect2))
       (>= (rect-x2 rect1) (rect-x1 rect2))
       (<= (rect-y1 rect1) (rect-y2 rect2))
       (>= (rect-y2 rect1) (rect-y1 rect2))))


(defun blocked (entity x y)
  (loop :for tile :of-type ecs:entity :in (tiles (a*:encode-float-coordinates
                                                  (round/tile-size x)
                                                  (round/tile-size y)))
        :thereis (or (and (has-map-tile-p tile)
                          (plusp (map-tile-blocks tile)))
                     (and (/= tile entity)
                          (has-character-p tile)
                          (has-health-p tile)))))

(defun live-character-at (x y)
  (loop :for tile :of-type ecs:entity :in (tiles (a*:encode-float-coordinates
                                                  (round/tile-size x)
                                                  (round/tile-size y)))
        :when (and (has-character-p tile)
                   (has-health-p tile))
           :return tile))

(defun place-objects (x1 y1 x2 y2)
  (dotimes (_ (random (1+ +room-max-monsters+)))
    (let ((x (random-from-range (+ x1 +tile-size+) (- x2 +tile-size+)))
          (y (random-from-range (+ y1 +tile-size+) (- y2 +tile-size+))))
      (unless (blocked -1 x y)
        (make-enemy-object :goblin-warrior "the goblin" x y)))))

(defun make-room (x1 y1 x2 y2 &key first)
  (loop
    :for x :of-type single-float
      :from (+ x1 +tile-size+) :below x2 :by +tile-size+
    :do (loop
          :for y :of-type single-float
            :from (+ y1 +tile-size+) :below y2 :by +tile-size+
          :do (let ((tile (first (tiles (a*:encode-float-coordinates x y)))))
                (with-map-tile () tile
                  (setf blocks 0
                        obscures 0))
                (change-sprite tile :floor)))
    :finally (unless first (place-objects x1 y1 x2 y2))))

(defun make-horizontal-tunnel (x1 x2 y)
  (loop
    :for x :of-type single-float
      :from (min x1 x2) :to (max x1 x2) :by +tile-size+
    :do (let ((tile (first (tiles (a*:encode-float-coordinates x y)))))
          (with-map-tile () tile
            (setf blocks 0
                  obscures 0))
          (change-sprite tile :floor))))

(defun make-vertical-tunnel (y1 y2 x)
  (loop
    :for y :of-type single-float
      :from (min y1 y2) :to (max y1 y2) :by +tile-size+
    :do (let ((tile (first (tiles (a*:encode-float-coordinates x y)))))
          (with-map-tile () tile
            (setf blocks 0
                  obscures 0))
          (change-sprite tile :floor))))

(defun make-map ()
  (loop :for x :of-type single-float
          :from 0.0 :below +world-width+ :by +tile-size+
        :do (loop :for y :of-type single-float
                    :from 0.0 :below +world-height+ :by +tile-size+
                  :do (let ((object (make-sprite-object :wall x y)))
                        (make-map-tile object :blocks 1)
                        (make-view object))))
  (loop
    :with rooms := nil
    :with player-x := 0.0
    :with player-y := 0.0
    :for i :from 0 :below +max-rooms+
    :for w := (round/tile-size
               (random-from-range +room-min-size+ +room-max-size+))
    :for h := (round/tile-size
               (random-from-range +room-min-size+ +room-max-size+))
    :for x := (round/tile-size
               (random-from-range 0 (- +world-width+  w +tile-size+)))
    :for y := (round/tile-size
               (random-from-range 0 (- +world-height+ h +tile-size+)))
    :for room := (make-rect* x y w h)
    :for intersects := (loop :for r :in rooms :thereis (intersect room r))
    :unless intersects
      :do (make-room (rect-x1 room) (rect-y1 room)
                     (rect-x2 room) (rect-y2 room)
                     :first (not rooms))
          (let+ (((&values center-x center-y) (center room))
                 (new-x (round/tile-size center-x))
                 (new-y (round/tile-size center-y)))
            (if rooms
                (let+ ((previous-room (first rooms))
                       ((&values previous-x previous-y) (center previous-room))
                       (prev-x (round/tile-size previous-x))
                       (prev-y (round/tile-size previous-y)))
                  (cond ((zerop (random 2))
                         (make-horizontal-tunnel prev-x new-x prev-y)
                         (make-vertical-tunnel   prev-y new-y new-x))
                        (t
                         (make-vertical-tunnel   prev-y new-y prev-x)
                         (make-horizontal-tunnel prev-x new-x new-y))))
                (setf player-x new-x
                      player-y new-y))
            (push room rooms))
    :finally (make-player-object player-x player-y)))
