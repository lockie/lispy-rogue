(in-package #:roguelike)


(cffi:defcallback %edit-filter :int ((edit :pointer) (char :unsigned-int))
  (declare (ignore edit char))
  0)

(ui:defwindow message-log ()
    (:x 0 :y +world-height+
     :w +world-width+ :h (- +window-height+ +world-height+)
     :styles ((:item-color :window-fixed-background :r 0 :g 0 :b 0)
              (:item-color :edit-normal :r 0 :g 0 :b 0)))
  (ui:layout-space (:height (- +window-height+ +world-height+ +ui-font-size+ 6)
                    :format :dynamic)
    (ui:layout-space-push :x 0.02 :y 0.0 :w 0.97 :h 1.0)
    (setf *message-log-focused*
          (/= 2 (ui:edit
                 (format nil "~{~a~^~%~}" *message-log*)
                 :flags (:multiline :no-horizontal-scroll :selectable :clipboard)
                 :filter (cffi:callback %edit-filter))))))

(defun describe-tile (x y)
  (let* ((x* (round/tile-size x))
         (y* (round/tile-size y))
         (hash (a*:encode-float-coordinates x* y*))
         (tiles (tiles hash))
         (description nil))
    (dolist (tile (reverse tiles))
      (cond
        ((has-view-p tile)
         (when (zerop (view-lit tile))
           (return-from describe-tile nil))
         (unless description
           (setf description (if (plusp (map-tile-blocks tile))
                                 "the wall"
                                 "the floor"))))

        ((and (has-item-p tile)
              (not (ecs:entity-valid-p (item-owner tile))))
         (setf description (item-name tile)))

        ((has-stairs-p tile)
         (setf description "the stairs to the next level."))

        ((has-enemy-p tile)
         (when (or (has-health-p tile) (not description))
           (setf description (character-name tile))))))
    description))

(ui:defwindow info ()
    (:x +world-width+ :y 0
     :w (- +window-width+ +world-width+) :h +window-height+
     :styles ((:item-color :window-fixed-background :r 0 :g 0 :b 0)
              (:item-color :edit-normal :r 0 :g 0 :b 0)
              (:color :edit-border-color :r 0 :g 0 :b 0)))
  "Bleh."
  (ui:layout-space (:height (/ +window-height+ 2) :format :dynamic)
    (ui:layout-space-push :x 0.02 :y 0 :w 1.0 :h 0.1)
    ;; TODO tooltips?
    (let ((player (player-entity 1)))
      (with-health () player
        (ui:label (format nil "HP  ~3d / ~3d" points max)))
      (ui:layout-space-push :x 0.02 :y 0.07 :w 1.0 :h 0.1)
      (with-mana () player
        (ui:label (format nil "MP  ~3d / ~3d" points max)))
      (with-player () player
        (ui:layout-space-push :x 0.02 :y 0.14 :w 1.0 :h 0.1)
        (ui:label (format nil "LVL ~2d  XP ~4d / ~4d"
                          level xp (next-level-xp level))))
      (with-stats () player
        (ui:layout-space-push :x 0.02 :y 0.21 :w 1.0 :h 0.1)
        (ui:label (format nil "STR ~2d  DEX ~2d  INT ~2d" str dex int)))
      (with-character () player
        (ui:layout-space-push :x 0.02 :y 0.28 :w 1.0 :h 0.1)
        (ui:label (format nil "movement speed ~7d" (round speed))))
      (with-defense () player
        (ui:layout-space-push :x 0.02 :y 0.35 :w 1.0 :h 0.1)
        (ui:label (format nil "evasion ~14d" (round evasion)))
        (ui:layout-space-push :x 0.02 :y 0.42 :w 1.0 :h 0.1)
        (ui:label (format nil "block chance ~8d%" (round (* block-chance 100))))
        (ui:layout-space-push :x 0.02 :y 0.49 :w 1.0 :h 0.1)
        (ui:label (format nil "armor ~16d" (round armor))))
      (with-offense () player
        (ui:layout-space-push :x 0.02 :y 0.56 :w 1.0 :h 0.1)
        (ui:label (format nil "attack range ~9d" (floor range +tile-size+)))
        (ui:layout-space-push :x 0.02 :y 0.63 :w 1.0 :h 0.1)
        (ui:label (format nil "attack speed ~7,1f/s" (/ 1 duration)))
        (ui:layout-space-push :x 0.02 :y 0.70 :w 1.0 :h 0.1)
        (ui:label (format nil "accuracy ~13d" (round accuracy)))
        (ui:layout-space-push :x 0.02 :y 0.77 :w 1.0 :h 0.1)
        (ui:label (format nil "min damage ~11d" (round min-damage)))
        (ui:layout-space-push :x 0.02 :y 0.84 :w 1.0 :h 0.1)
        (ui:label (format nil "max damage ~11d" (round max-damage))))
      (ui:layout-space-push :x 0.02 :y 1.0 :w 0.9 :h 0.9)
      (if (or *inventory-shown* *throw-window-shown*)
          (when (ecs:entity-valid-p *hovered-item*)
            (ui:label-wrap (format nil "You see ~a from level ~a."
                                   (item-name *hovered-item*)
                                   (item-level *hovered-item*)))
            (ui:layout-space-push :x 0.02 :y 1.15 :w 1.0 :h 1.0)
            (ui:edit (describe-equipment *hovered-item*)
                     :flags (:multiline :no-horizontal-scroll :read-only)))
          (unless *levelup-shown*
            (al:with-current-mouse-state mouse-state
              (let ((x (- (mouse-state-x mouse-state) (/ +tile-size+ 2)))
                    (y (- (mouse-state-y mouse-state) (/ +tile-size+ 2))))
                (when-let (description (describe-tile x y))
                  (ui:label-wrap (format nil "You see ~a." description))
                  (when-let (enemy (live-character-at x y))
                    (when (and (/= enemy player)
                               (>= (stats-int player) (enemy-level enemy)))
                      (ui:layout-space-push :x 0.02 :y 1.15 :w 1.1 :h 1.0)
                      (ui:edit
                       (format nil "~@{~a~^~%~}"
                               (format nil "HP  ~3d / ~3d"
                                       (health-points enemy) (health-max enemy))
                               (format nil "movement speed ~7d"
                                       (round (character-speed enemy)))
                               (format nil "evasion ~14d"
                                       (round (defense-evasion enemy)))
                               (format nil "block chance ~8d%"
                                       (round (* (defense-block-chance enemy) 100)))
                               (format nil "armor ~16d"
                                       (round (defense-armor enemy)))
                               (format nil "attack range ~9d"
                                       (floor (offense-range enemy) +tile-size+))
                               (format nil "attack speed ~7,1f/s"
                                       (/ 1 (offense-duration enemy)))
                               (format nil "accuracy ~13d"
                                       (round (offense-accuracy enemy)))
                               (format nil "min damage ~11d"
                                       (round (offense-min-damage enemy)))
                               (format nil "max damage ~11d"
                                       (round (offense-max-damage enemy))))
                       :flags (:multiline :no-horizontal-scroll :read-only))))))))))))

(define-constant +inventory-keys+ '(:1 :2 :3 :4 :5 :6 :7 :8 :9 :0 :a :b :c)
  :test #'equal)

(defparameter *hovered-item* -1)

(ui:defwindow inventory (title items)
    (:title title
     :flags (border title)
     :x 280 :y 50 :w 400 :h 500
     :styles ((:item-color :window-fixed-background :r 0 :g 0 :b 0)
              (:item-color :window-header-active :r 0 :g 0 :b 0)
              (:item-color :window-header-normal :r 0 :g 0 :b 0)
              (:item-color :selectable-normal :r 0 :g 0 :b 0)))
  (ui:layout-row-static :height +ui-font-size+ :item-width 375 :columns 1)
  (al:with-current-keyboard-state keyboard-state
    (cffi:with-foreign-object (selected :int (length +inventory-keys+))
      (dotimes (i (length +inventory-keys+))
        (setf (cffi:mem-aref selected :int i) 0))
      (loop :for item :in (stable-sort items (lambda (a b) (string< (item-name a)
                                                               (item-name b))))
            :for key :in +inventory-keys+
            :for i :of-type fixnum :from 0
            :do (ui:with-context ctx
                  (when (plusp (the fixnum (nk:widget-is-hovered ctx)))
                    (setf *hovered-item* item)))
                (ui:selectable-label
                 (format nil "(~(~a~)) ~a ~:[~;[equipped]~]" key
                         (subseq (item-name item) 4)
                         (has-equipped-p item))
                 (cffi:inc-pointer selected (* i (cffi:foreign-type-size :int))))
                (when (or (plusp (cffi:mem-aref selected :int i))
                          (al:key-down keyboard-state key))
                  (return-from inventory item)))))
  nil)

(declaim (type boolean *inventory-key-pressed* *inventory-shown*))
(defparameter *inventory-key-pressed* nil)
(defparameter *inventory-shown* nil)

(declaim (type boolean *throw-key-pressed* *throw-window-shown*))
(defparameter *throw-key-pressed* nil)
(defparameter *throw-window-shown* nil)

(ecs:defsystem rummage-inventory
  (:components-ro (player health)
   :when (plusp health-points)
   :enable (and (not *message-log-focused*)
                (not *throw-window-shown*)
                (not *targeting*)
                (not *levelup-shown*)
                (not *help-shown*)
                (not *won*))
   :arguments ((ui-context cffi:foreign-pointer)))
  (al:with-current-keyboard-state keyboard-state
    (if (al:key-down keyboard-state :I)
        (unless *inventory-key-pressed*
          (setf *hovered-item* -1
                *inventory-key-pressed* t
                *inventory-shown* (not *inventory-shown*)
                *turn* (not *inventory-shown*)))
        (setf *inventory-key-pressed* nil))
    (when (and *inventory-shown* (al:key-down keyboard-state :escape))
      (setf *inventory-shown* nil
            *turn* t)))
  (when *inventory-shown*
    (when-let (selected-item
               (inventory ui-context "Inventory" (items entity)))
      (use-item selected-item nil nil)
      (setf *inventory-shown* nil
            *turn* t))))

(ecs:defsystem throw-away-item
  (:components-ro (player health position tile)
   :when (plusp health-points)
   :enable (and (not *message-log-focused*)
                (not *inventory-shown*)
                (not *targeting*)
                (not *levelup-shown*)
                (not *help-shown*)
                (not *won*))
   :arguments ((ui-context cffi:foreign-pointer)))
  (al:with-current-keyboard-state keyboard-state
    (if (al:key-down keyboard-state :T)
        (unless *throw-key-pressed*
          (setf *throw-key-pressed* t
                *throw-window-shown* (not *throw-window-shown*)
                *turn* (not *throw-window-shown*)))
        (setf *throw-key-pressed* nil))
    (when (and *throw-window-shown* (al:key-down keyboard-state :escape))
      (setf *throw-window-shown* nil
            *turn* t))
    (when *throw-window-shown*
      (when-let (selected-item
                 (inventory ui-context "Throw away item" (items entity)))
        (log-message "You throw away ~a." (item-name selected-item))
        (setf (item-owner selected-item) -1
              (parent-entity selected-item) *current-map*
              (position-x selected-item) position-x
              (position-y selected-item) position-y
              (tile-col  selected-item) tile-col
              (tile-row  selected-item) tile-row
              (tile-hash selected-item) tile-hash)
        (when (has-equipped-p selected-item)
          (delete-equipped selected-item))
        (setf *throw-window-shown* nil
              *turn* t)))))

(ui:defwindow congratulations ()
    (:title "Congratulations!"
     :flags (border title)
     :x 0 :y 0 :w +window-width+ :h +window-height+
     :styles ((:vec2 :window-header-label-padding :x 540 :y 20)
              (:item-color :window-fixed-background :r 0 :g 0 :b 0)
              (:item-color :window-header-active :r 0 :g 0 :b 0)
              (:item-color :window-header-normal :r 0 :g 0 :b 0)))
  (ui:layout-space (:height (/ +window-height+ 2) :format :dynamic)
    (ui:layout-space-push :x 0.1 :y 0.1 :w 0.97 :h 0.1)
    (ui:label "You beat the game! Now you are a true lispy rogue ^_^")))

(define-constant +help-text+
  '("Beat the game by descending to level 11 without dying."
    ""
    "                                            CONTROLS"
    "q w e    y k u    7 8 9"
    " \\|/      \\|/      \\|/"
    "a- -d    h- -l    4- -6"
    " /|\\      /|\\      /|\\"
    "z s c    b j n    1 2 3"
    ""
    "Use any of WASD, vi-keys, numpad or just arrows for movement."
    ""
    "f         start targeting ranged weapon (use the movement keys above to move the target)"
    "f, enter  pick a target when in targeting mode"
    ">         move down the stairs"
    "g         grab the item from the floor"
    "i         show inventory window (use items by pressing correspoding keys 0..9, a, b, c)"
    "t         select an item to throw away (same)"
    "r, space  skip turn"
    "f1, ?     bring up this handy window"
    "escape    cancel current window or targeting mode")
  :test #'equal)

(ui:defwindow help ()
    (:title "HELP"
     :flags (border title)
     :x 0 :y 0 :w +window-width+ :h +window-height+
     :styles ((:vec2 :window-header-label-padding :x 615 :y 20)
              (:item-color :window-fixed-background :r 0 :g 0 :b 0)
              (:item-color :window-header-active :r 0 :g 0 :b 0)
              (:item-color :window-header-normal :r 0 :g 0 :b 0)))
  (ui:layout-space (:height +ui-font-size+ :format :dynamic)
    (loop :for string :of-type simple-string :in +help-text+
          :for i :from 0
          :do (ui:layout-space-push :x 0.02 :y i :w 1.0 :h 1.0)
              (ui:label string))))

(declaim (type boolean *help-key-pressed* *help-shown*))
(defparameter *help-key-pressed* nil)
(defparameter *help-shown* t)

(ecs:defsystem show-help
  (:components-ro (player)
   :enable (not *won*)
   :arguments ((ui-context cffi:foreign-pointer)))
  (al:with-current-keyboard-state keyboard-state
    (if (keys-down keyboard-state :F1 :slash)
        (unless *help-key-pressed*
          (setf *help-key-pressed* t
                *help-shown* (not *help-shown*))
          (when *help-shown*
            (setf *turn* nil)))
        (setf *help-key-pressed* nil))
    (when (and *help-shown* (al:key-down keyboard-state :escape))
      (setf *help-shown* nil))
    (when *help-shown*
      (help ui-context))))

(declaim (type boolean *levelup-shown* *levelup-key-pressed*))
(defparameter *levelup-shown* nil)
(defparameter *levelup-key-pressed* nil)

(define-constant +stat-descriptions+
  '("   affects HP, armor & damage"
    "   affects evasion & accuracy"
    "   affects mana and vision")
  :test #'equal)

(ui:defwindow levelup ()
    (:title "Choose stat to raise"
     :flags (border title)
     :x 280 :y 50 :w 400 :h 500
     :styles ((:item-color :window-fixed-background :r 0 :g 0 :b 0)
              (:item-color :window-header-active :r 0 :g 0 :b 0)
              (:item-color :window-header-normal :r 0 :g 0 :b 0)
              (:item-color :selectable-normal :r 0 :g 0 :b 0)))
  (ui:layout-row-static :height +ui-font-size+ :item-width 375 :columns 1)
  (al:with-current-keyboard-state keyboard-state
    (cffi:with-foreign-object (selected :int 3)
      (dotimes (i 3)
        (setf (cffi:mem-aref selected :int i) 0))
      (loop
        :for stat :in '("STR" "DEX" "INT")
        :for description :in +stat-descriptions+
        :for key :in '(:1 :2 :3)
        :for i :from 0
        :with key-pressed := nil
        :do (ui:selectable-label
             (format nil "(~(~a~)) ~a" key stat)
             (cffi:inc-pointer selected (* i (cffi:foreign-type-size :int))))
            (ui:label description)
            (when (al:key-down keyboard-state key)
              (setf key-pressed t))
            (when (or (plusp (cffi:mem-aref selected :int i))
                      (and (not *levelup-key-pressed*)
                           (al:key-down keyboard-state key)))
              (setf *levelup-key-pressed* key-pressed)
              (return-from levelup i))
        :finally (setf *levelup-key-pressed* key-pressed))))
  nil)
