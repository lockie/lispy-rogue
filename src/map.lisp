(in-package #:lispy-rogue)


(ecs:defcomponent level
  (number 0 :type fixnum))

(ecs:defcomponent stairs
  (current-level -1 :type ecs:entity))

(defun make-exit-from (level x y)
  (loop :for tile :in (tiles (a*:encode-float-coordinates x y))
        :when (has-map-tile-p tile)
        :do (change-sprite tile :stairs)
            (make-stairs tile :current-level level)
            (return)))

(ecs:defsystem draw-map-sprites
  (:components-ro (tile sprite)
   :components-rw (view)
   :components-no (character)
   :before (draw-character-sprites draw-item-sprites)
   :when (or (plusp view-lit) (plusp view-explored))
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (when (zerop view-explored)
    (setf view-explored 1))
  (al:draw-bitmap sprite-bitmap tile-col tile-row 0))

(ecs:defsystem draw-fow
  (:components-ro (tile view)
   :after (draw-map-sprites)
   :when (and (not (plusp view-lit)) (plusp view-explored))
   :with (fog := (sprite-bitmap (sprite-prefab :fog))))
  (al:draw-bitmap fog tile-col tile-row 0))

(define-constant +room-min-size+ (* 5  +tile-size+))
(define-constant +room-max-size+ (* 10 +tile-size+))
(define-constant +room-max-monsters+ 3)
(define-constant +room-max-items+ 2)
(define-constant +max-rooms+ 30)

(defstruct rect
  (x1 0.0 :type single-float)
  (y1 0.0 :type single-float)
  (x2 0.0 :type single-float)
  (y2 0.0 :type single-float))

(declaim
 (ftype (function (single-float single-float single-float single-float) rect)
        make-rect*))
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

(declaim
 (ftype (function (ecs:entity single-float single-float) (or ecs:entity null))
        blocked))
(defun blocked (entity x y)
  (loop :for tile :of-type ecs:entity :in (tiles (a*:encode-float-coordinates
                                                  (round/tile-size x)
                                                  (round/tile-size y)))
        :when (or (and (has-map-tile-p tile)
                       (plusp (map-tile-blocks tile)))
                  (and (/= tile entity)
                       (has-character-p tile)
                       (has-health-p tile)))
        :return tile))

(declaim
 (ftype (function (single-float single-float) (or ecs:entity null)) has-object))
(defun has-object (x y)
  (loop :for tile :of-type ecs:entity :in (tiles (a*:encode-float-coordinates
                                                  (round/tile-size x)
                                                  (round/tile-size y)))
        :when (or (and (has-map-tile-p tile)
                       (plusp (map-tile-blocks tile)))
                  (and (has-character-p tile)
                       (has-health-p tile))
                  (has-item-p tile))
          :return tile))

(declaim (ftype (function (single-float single-float) (or ecs:entity null))
                live-character-at))
(defun live-character-at (x y)
  (loop :for tile :of-type ecs:entity :in (tiles (a*:encode-float-coordinates
                                                  (round/tile-size x)
                                                  (round/tile-size y)))
        :when (and (has-character-p tile)
                   (has-health-p tile))
           :return tile))

(defvar *floor-tile*)
(defvar *wall-tile1*)
(defvar *wall-tile2*)

(define-weighted-random-generator random-item
  (0.5 :health-potion)
  (0.1 :fireball-scroll)
  (0.2 :cripple-scroll)
  (0.2 :equipment))

(declaim (ftype (function (ecs:entity
                           single-float single-float single-float single-float))
                place-objects))
(defun place-objects (level x1 y1 x2 y2)
  (let ((level-number (level-number level)))
    (declare (type (integer 0 10) level-number))
    (dotimes (_ (random (1+ +room-max-monsters+)))
      (let ((x (random-from-range (+ x1 +tile-size+) (- x2 +tile-size+)))
            (y (random-from-range (+ y1 +tile-size+) (- y2 +tile-size+))))
        (unless (blocked -1 x y)
          (make-parent (make-enemy-object level-number x y) :entity level))))
    (dotimes (_ (random (1+ +room-max-items+)))
      (let ((x (random-from-range (+ x1 +tile-size+) (- x2 +tile-size+)))
            (y (random-from-range (+ y1 +tile-size+) (- y2 +tile-size+))))
        (unless (has-object x y)
          (make-parent
           (ecase (random-item)
             (:health-potion
              (make-health-potion level-number
                                  (random-from-range (* 10 level-number)
                                                     (* 15 level-number))
                                  x y))
             (:fireball-scroll
              (make-fireball-scroll level-number
                                    (random-from-range (* 3 level-number)
                                                       (* 5 level-number))
                                    x y))
             (:cripple-scroll
              (make-cripple-scroll level-number
                                   (random-from-range (* 1 level-number)
                                                      (* 3 level-number))
                                   x y))
             (:equipment
              (make-equipment-item level-number x y))))
         :entity level)))))

(defun make-room (level x1 y1 x2 y2 &key first)
  (declare (type single-float x1 y1 x2 y2))
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
                (change-sprite tile *floor-tile*)))
    :finally (unless first (place-objects level x1 y1 x2 y2))))

(defun make-horizontal-tunnel (x1 x2 y)
  (declare (type single-float x1 y x2))
  (loop
    :for x :of-type single-float
      :from (min x1 x2) :to (max x1 x2) :by +tile-size+
    :do (let ((tile (first (tiles (a*:encode-float-coordinates x y)))))
          (with-map-tile () tile
            (setf blocks 0
                  obscures 0))
          (change-sprite tile *floor-tile*))))

(defun make-vertical-tunnel (y1 y2 x)
  (declare (type single-float x y1 y2))
  (loop
    :for y :of-type single-float
      :from (min y1 y2) :to (max y1 y2) :by +tile-size+
    :do (let ((tile (first (tiles (a*:encode-float-coordinates x y)))))
          (with-map-tile () tile
            (setf blocks 0
                  obscures 0))
          (change-sprite tile *floor-tile*))))

(defun make-map (level-number)
  (loop
    :with level := (ecs:make-object `((:level :number ,level-number)))
    :with rooms := nil
    :with player-x := 0.0
    :with player-y := 0.0
    :with *floor-tile* := (format-symbol :keyword "FLOOR~a" (random 29))
    :with wall-tile := (random 6)
    :with *wall-tile1* := (format-symbol :keyword "WALL~a0" wall-tile)
    :with *wall-tile2* := (format-symbol :keyword "WALL~a1" wall-tile)
    :initially (delete-tile (player-entity 1)) ;; HACK
               (loop
                 :for x :of-type single-float
                 :from 0.0 :below +world-width+ :by +tile-size+
                 :do (loop :for y :of-type single-float
                           :from 0.0 :below +world-height+ :by +tile-size+
                           :do (let ((object (make-sprite-object
                                              (if (zerop (random 2))
                                                  *wall-tile1* *wall-tile2*)
                                              x y)))
                                 (make-parent object :entity level)
                                 (make-map-tile object :blocks 1)
                                 (make-view object))))
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
      :do (make-room level
                     (rect-x1 room) (rect-y1 room)
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
    :finally (let+ (((&values cx cy) (center (first rooms))))
               (make-exit-from level
                               (round/tile-size cx)
                               (round/tile-size cy)))
             (let ((player (player-entity 1)))
               (assign-position player :x player-x :y player-y)
               (assign-tile player :col player-x :row player-y)
               (with-character () player
                 (setf target-x player-x
                       target-y player-y))
               (setf *player-position-hash* -1))
             (setf *current-map* level)
             (return level)))


(declaim (type boolean *stairs-key-pressed*))
(defparameter *stairs-key-pressed* nil)

(ecs:defsystem switch-level
  (:components-ro (player health tile)
   :when (plusp health-points)
   :enable (and (not *message-log-focused*)
                (not *inventory-shown*)
                (not *throw-window-shown*)
                (not *targeting*)
                (not *levelup-shown*)
                (not *help-shown*)
                (not *won*))
   :arguments ((keyboard-state cffi:foreign-pointer)))
  (if (and (al:key-down keyboard-state :fullstop)
           (or (al:key-down keyboard-state :lshift)
               (al:key-down keyboard-state :rshift)))
      (unless *stairs-key-pressed*
        (if-let (stairs (find-if #'has-stairs-p
                                 (tiles (a*:encode-float-coordinates
                                         tile-col tile-row))))
          (with-stairs () stairs
            (let* ((current-level-number (level-number current-level))
                   (new-level (1+ current-level-number)))
              (when (= new-level 11)
                (setf *won* t
                      *turn* nil)
                (return-from ecs:current-entity))
              (ecs:delete-entity current-level)
              (make-map new-level)
              (setf *message-log* "")
              (log-message "You descend to the ~:r level." new-level)))
          (log-message "There is no stairs here."))
        (setf *turn* nil
              *stairs-key-pressed* t))
      (setf *stairs-key-pressed* nil)))
