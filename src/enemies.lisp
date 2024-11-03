(in-package #:roguelike)


(ecs:defcomponent enemy
  (level 0 :type fixnum)
  (xp 0 :type fixnum))

(ecs:defsystem chase-player
  (:components-ro (position tile enemy health offense)
   :components-rw (character)
   :with ((player player-x player-y) := (let ((player (player-entity 1)))
                                          (with-position () player
                                            (values player x y))))
   :enable (and *turn* (has-health-p player)))
  (cond
    ((and (approx-equal position-x player-x offense-range)
          (approx-equal position-y player-y offense-range))

     (attack entity player))

    ((and (approx-equal position-x player-x character-vision-range)
          (approx-equal position-y player-y character-vision-range)
          (not (and (has-path-p entity)
                    (approx-equal (path-destination-x entity) player-x
                                  (* 2 +tile-size+))
                    (approx-equal (path-destination-y entity) player-y
                                  (* 2 +tile-size+)))))
     (with-tile (player-col player-row) player
       (let+ (((&values target-x target-y)
               ;; TODO check LoS for ranged?
               (nearest-tile tile-col tile-row player-col player-row)))
         (when (blocked entity target-x target-y)
           (setf target-x player-col
                 target-y player-row))
         (find-path tile-col tile-row target-x target-y
                    :entity entity :player player))))))

(define-weighted-random-generator random-low-level-enemy
  (0.10 :goblin-archer)
  (0.25 :goblin-warrior)
  (0.20 :kobold-warrior)
  (0.20 :lizard-warrior)
  (0.05 :skeleton-archer)
  (0.15 :skeleton-warrior)
  (0.05 :troll-warrior))

(define-weighted-random-generator random-high-level-enemy
  (0.042 :demon-archer)
  (0.021 :demon-crusher)
  (0.062 :demon-warrior)
  (0.125 :goblin-crusher)
  (0.055 :kobold-archer)
  (0.035 :kobold-crusher)
  (0.085 :kobold-warrior)
  (0.035 :lizard-archer)
  (0.090 :lizard-warrior)
  (0.050 :oni-warrior)
  (0.050 :skeleton-archer)
  (0.021 :skeleton-crusher)
  (0.079 :skeleton-warrior)
  (0.042 :troll-crusher)
  (0.083 :troll-warrior)
  (0.042 :wisp-archer)
  (0.083 :wisp-warrior))

(defun %make-enemy-object (sprite mlvl x y
                           &key xp speed vision range hp evasion (block 0.0) armor
                                min-dmg max-dmg accuracy duration)
  (let ((object (make-sprite-object sprite x y)))
    (make-character object
                    :name (format nil "the ~{~(~a~)~^ ~}"
                                  (split "-" (string sprite) :limit 2))
                    :base-speed speed
                    :base-vision-range vision)
    (make-enemy object :xp xp :level mlvl)
    (make-health object :base-max hp)
    (make-defense object :base-evasion evasion
                         :base-block-chance block
                         :base-armor armor)
    (make-offense object :range range
                         :min-damage min-dmg
                         :max-damage max-dmg
                         :accuracy accuracy
                         :duration duration)
    (if (< range (* 2 +tile-size+))
        (make-melee object)
        (make-ranged object))
    object))

(define-constant +melee-range+ (* 1.5 +tile-size+))

(defun make-enemy-object (mlvl x y)
  (case (if (>= mlvl 5) (random-high-level-enemy) (random-low-level-enemy))
    (:demon-archer
     (%make-enemy-object :demon-archer mlvl x y
                         :xp 12 :speed 120.0 :vision 150.0 :range 120.0
                         :hp 50 :evasion (* mlvl 4.0) :armor (* mlvl 1.0)
                         :min-dmg (+ mlvl 25.0) :max-dmg (+ mlvl 30.0)
                         :accuracy (* mlvl 2.0) :duration 0.8))
    (:demon-crusher
     (%make-enemy-object :demon-crusher mlvl x y
                         :xp 12 :speed 120.0 :vision 150.0 :range +melee-range+
                         :hp 50 :evasion (* mlvl 4.0) :armor (* mlvl 1.0)
                         :min-dmg (+ mlvl 25.0) :max-dmg (+ mlvl 30.0)
                         :accuracy (* mlvl 2.0) :duration 0.8))
    (:demon-warrior
     (%make-enemy-object :demon-warrior mlvl x y
                         :xp 10 :speed 150.0 :vision 150.0 :range +melee-range+
                         :hp 50 :evasion (* mlvl 4.0) :armor (* mlvl 1.0)
                         :min-dmg (+ mlvl 20.0) :max-dmg (+ mlvl 30.0)
                         :accuracy (* mlvl 1.0) :duration 0.4))
    (:goblin-archer
     (%make-enemy-object :goblin-archer mlvl x y
                         :xp 3 :speed 75.0 :vision 100.0 :range 85.0
                         :hp 20 :evasion (* mlvl 2.0) :armor (* mlvl 1.0)
                         :min-dmg (+ mlvl 2.0) :max-dmg (+ mlvl 5.0)
                         :accuracy (* mlvl 5.0) :duration 0.8))
    (:goblin-crusher
     (%make-enemy-object :goblin-crusher mlvl x y
                         :xp 3 :speed 75.0 :vision 100.0 :range +melee-range+
                         :hp 20 :evasion (* mlvl 2.0) :armor (* mlvl 3.0)
                         :min-dmg (+ mlvl 2.0) :max-dmg (+ mlvl 5.0)
                         :accuracy (* mlvl 5.0) :duration 0.8))
    (:goblin-warrior
     (%make-enemy-object :goblin-warrior mlvl x y
                         :xp 1 :speed 120.0 :vision 100.0 :range +melee-range+
                         :hp 20 :evasion (* mlvl 2.0) :armor (* mlvl 2.0)
                         :min-dmg (+ mlvl 1.0) :max-dmg (+ mlvl 5.0)
                         :accuracy (* mlvl 2.0) :duration 0.4))
    (:kobold-archer
     (%make-enemy-object :kobold-archer mlvl x y
                         :xp 4 :speed 120.0 :vision 150.0 :range 120.0
                         :hp 20 :evasion (* mlvl 2.0) :armor (* mlvl 1.0)
                         :min-dmg (+ mlvl 2.0) :max-dmg (+ mlvl 5.0)
                         :accuracy (* mlvl 5.0) :duration 0.6))
    (:kobold-crusher
     (%make-enemy-object :kobold-crusher mlvl x y
                         :xp 4 :speed 120.0 :vision 150.0 :range +melee-range+
                         :hp 20 :evasion (* mlvl 2.0) :armor (* mlvl 3.0)
                         :block 0.30
                         :min-dmg (+ mlvl 2.0) :max-dmg (+ mlvl 5.0)
                         :accuracy (* mlvl 3.0) :duration 0.6))
    (:kobold-warrior
     (%make-enemy-object :kobold-warrior mlvl x y
                         :xp 2 :speed 150.0 :vision 150.0 :range +melee-range+
                         :hp 20 :evasion (* mlvl 2.0) :armor (* mlvl 2.0)
                         :min-dmg (+ mlvl 1.0) :max-dmg (+ mlvl 5.0)
                         :accuracy (* mlvl 2.0) :duration 0.3))
    (:lizard-archer
     (%make-enemy-object :lizard-archer mlvl x y
                         :xp 6 :speed 175.0 :vision 150.0 :range 100.0
                         :hp 30 :evasion (* mlvl 4.0) :armor (* mlvl 1.0)
                         :min-dmg (+ mlvl 4.0) :max-dmg (+ mlvl 7.0)
                         :accuracy (* mlvl 5.0) :duration 0.5))
    (:lizard-warrior
     (%make-enemy-object :lizard-warrior mlvl x y
                         :xp 4 :speed 200.0 :vision 150.0 :range +melee-range+
                         :hp 30 :evasion (* mlvl 4.0) :armor (* mlvl 1.0)
                         :min-dmg (+ mlvl 2.0) :max-dmg (+ mlvl 7.0)
                         :accuracy (* mlvl 3.0) :duration 0.2))
    (:oni-warrior
     (%make-enemy-object :oni-warrior mlvl x y
                         :xp 25 :speed 175.0 :vision 150.0 :range +melee-range+
                         :hp 50 :evasion (* mlvl 5.0) :armor (* mlvl 3.0)
                         :block 0.45
                         :min-dmg (+ mlvl 25.0) :max-dmg (+ mlvl 45.0)
                         :accuracy (* mlvl 5.0) :duration 0.3))
    (:skeleton-archer
     (%make-enemy-object :skeleton-archer mlvl x y
                         :xp 4 :speed 30.0 :vision 75.0 :range 60.0
                         :hp 15 :evasion (* mlvl 2.0) :armor (* mlvl 1.0)
                         :min-dmg (+ mlvl 5.0) :max-dmg (+ mlvl 7.0)
                         :accuracy (* mlvl 5.0) :duration 1.0))
    (:skeleton-crusher
     (%make-enemy-object :skeleton-crusher mlvl x y
                         :xp 4 :speed 30.0 :vision 75.0 :range +melee-range+
                         :hp 15 :evasion (* mlvl 2.0) :armor (* mlvl 1.0)
                         :block 0.25
                         :min-dmg (+ mlvl 5.0) :max-dmg (+ mlvl 7.0)
                         :accuracy (* mlvl 5.0) :duration 1.0))
    (:skeleton-warrior
     (%make-enemy-object :skeleton-warrior mlvl x y
                         :xp 2 :speed 50.0 :vision 75.0 :range +melee-range+
                         :hp 15 :evasion (* mlvl 2.0) :armor (* mlvl 1.0)
                         :min-dmg (+ mlvl 2.0) :max-dmg (+ mlvl 7.0)
                         :accuracy (* mlvl 2.0) :duration 0.6))
    (:troll-crusher
     (%make-enemy-object :troll-crusher mlvl x y
                         :xp 12 :speed 50.0 :vision 75.0 :range +melee-range+
                         :hp 40 :evasion (* mlvl 2.0) :armor (* mlvl 3.0)
                         :block 0.30
                         :min-dmg (+ mlvl 20.0) :max-dmg (+ mlvl 25.0)
                         :accuracy (* mlvl 3.0) :duration 1.0))
    (:troll-warrior
     (%make-enemy-object :troll-warrior mlvl x y
                         :xp 10 :speed 75.0 :vision 75.0 :range +melee-range+
                         :hp 40 :evasion (* mlvl 2.0) :armor (* mlvl 3.0)
                         :min-dmg (+ mlvl 15.0) :max-dmg (+ mlvl 25.0)
                         :accuracy (* mlvl 2.0) :duration 0.5))
    (:wisp-archer
     (%make-enemy-object :wisp-archer mlvl x y
                         :xp 20 :speed 120.0 :vision 120.0 :range 100.0
                         :hp 50 :evasion (* mlvl 5.0) :armor 0.0
                         :min-dmg (+ mlvl 25.0) :max-dmg (+ mlvl 30.0)
                         :accuracy (* mlvl 5.0) :duration 0.8))
    (:wisp-warrior
     (%make-enemy-object :wisp-warrior mlvl x y
                         :xp 15 :speed 120.0 :vision 120.0 :range +melee-range+
                         :hp 50 :evasion (* mlvl 5.0) :armor 0.0 :block 0.35
                         :min-dmg (+ mlvl 20.0) :max-dmg (+ mlvl 30.0)
                         :accuracy (* mlvl 4.0) :duration 0.4))))
