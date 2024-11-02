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

        ((has-item-p tile)
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
  (ui:layout-space (:height (/ +window-height+ 2) :format :dynamic)
    (ui:layout-space-push :x 0.02 :y 0 :w 1.0 :h 0.1)
    ;; TODO tooltips?
    (let ((player (player-entity 1)))
      (with-health () player
        (ui:label (format nil "HP  ~3d / ~3d" points max)))
      (ui:layout-space-push :x 0.02 :y 0.07 :w 1.0 :h 0.1)
      (with-mana () player
        (ui:label (format nil "MP  ~3d / ~3d" points max)))
      (with-stats () player
        (ui:layout-space-push :x 0.02 :y 0.14 :w 1.0 :h 0.1)
        (ui:label (format nil "STR ~2d  DEX ~2d  INT ~2d" str dex int)))
      (with-character () player
        (ui:layout-space-push :x 0.02 :y 0.21 :w 1.0 :h 0.1)
        (ui:label (format nil "movement speed ~7d" (round speed))))
      (with-defense () player
        (ui:layout-space-push :x 0.02 :y 0.28 :w 1.0 :h 0.1)
        (ui:label (format nil "evasion ~14d" (round evasion)))
        (ui:layout-space-push :x 0.02 :y 0.35 :w 1.0 :h 0.1)
        (ui:label (format nil "block chance ~8d%" (round (* block-chance 100))))
        (ui:layout-space-push :x 0.02 :y 0.42 :w 1.0 :h 0.1)
        (ui:label (format nil "armor ~16d" (round armor))))
      (with-offense () player
        (ui:layout-space-push :x 0.02 :y 0.49 :w 1.0 :h 0.1)
        (ui:label (format nil "attack range ~9d" (floor range +tile-size+)))
        (ui:layout-space-push :x 0.02 :y 0.56 :w 1.0 :h 0.1)
        (ui:label (format nil "attack speed ~7,1f/s" (/ 1 duration)))
        (ui:layout-space-push :x 0.02 :y 0.63 :w 1.0 :h 0.1)
        (ui:label (format nil "accuracy ~13d" (round accuracy)))
        (ui:layout-space-push :x 0.02 :y 0.70 :w 1.0 :h 0.1)
        (ui:label (format nil "min damage ~11d" (round min-damage)))
        (ui:layout-space-push :x 0.02 :y 0.77 :w 1.0 :h 0.1)
        (ui:label (format nil "max damage ~11d" (round max-damage)))))
    (ui:layout-space-push :x 0.02 :y 1.0 :w 0.9 :h 0.9)
    (if (or *inventory-shown* *throw-window-shown*)
        (when (ecs:entity-valid-p *hovered-item*)
          (ui:label-wrap (format nil "You see ~a." (item-name *hovered-item*)))
          (ui:layout-space-push :x 0.02 :y 1.15 :w 1.0 :h 1.0)
          (ui:edit (describe-equipment *hovered-item*)
                   :flags (:multiline :no-horizontal-scroll :read-only)))
        (al:with-current-mouse-state mouse-state
          (when-let (description
                     (describe-tile
                      (- (mouse-state-x mouse-state) (/ +tile-size+ 2))
                      (- (mouse-state-y mouse-state) (/ +tile-size+ 2))))
            (ui:label-wrap (format nil "You see ~a." description)))))))

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
   :enable (and (not *message-log-focused*)
                (not *throw-window-shown*)
                (not *targeting*)
                (not *won*))
   :arguments ((ui-context cffi:foreign-pointer)))
  (when (plusp health-points)
    (al:with-current-keyboard-state keyboard-state
      (if (al:key-down keyboard-state :I)
          (block key-pressed
            (unless *inventory-key-pressed*
              (setf *hovered-item* -1
                    *inventory-key-pressed* t
                    *inventory-shown* (not *inventory-shown*)
                    *turn* (not *inventory-shown*))))
          (setf *inventory-key-pressed* nil))
      (when (and *inventory-shown* (al:key-down keyboard-state :escape))
        (setf *inventory-shown* nil
              *turn* t)))
    (when *inventory-shown*
      (when-let (selected-item
                 (inventory ui-context "Inventory" (items entity)))
        (use-item selected-item nil nil)
        (setf *inventory-shown* nil
              *turn* t)))))

(ecs:defsystem throw-away-item
  (:components-ro (player health position tile)
   :enable (and (not *message-log-focused*)
                (not *inventory-shown*)
                (not *targeting*)
                (not *won*))
   :arguments ((ui-context cffi:foreign-pointer)))
  (when (plusp health-points)
    (al:with-current-keyboard-state keyboard-state
      (if (al:key-down keyboard-state :T)
          (block key-pressed
            (unless *throw-key-pressed*
              (setf *throw-key-pressed* t
                    *throw-window-shown* (not *throw-window-shown*)
                    *turn* (not *throw-window-shown*))))
          (setf *throw-key-pressed* nil))
      (when (and *throw-window-shown* (al:key-down keyboard-state :escape))
        (setf *throw-window-shown* nil
              *turn* t))
      (when *throw-window-shown*
        (when-let (selected-item
                   (inventory ui-context "Throw away item" (items entity)))
          (log-message "You throw away ~a." (item-name selected-item))
          (setf (item-owner selected-item) -1
                (position-x selected-item) position-x
                (position-y selected-item) position-y
                (tile-col  selected-item) tile-col
                (tile-row  selected-item) tile-row
                (tile-hash selected-item) tile-hash)
          (setf *throw-window-shown* nil
                *turn* t))))))

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
