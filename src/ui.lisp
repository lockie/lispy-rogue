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
          (= 1 (ui:edit
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
     :styles ((:item-color :window-fixed-background :r 0 :g 0 :b 0)))
  (ui:layout-space (:height (/ +window-height+ 2) :format :dynamic)
    (ui:layout-space-push :x 0.02 :y 0 :w 1.0 :h 0.1)
    (with-health () (player-entity 1)
      (ui:label (format nil "HP: ~a / ~a" points max)))
    (al:with-current-mouse-state mouse-state
      (when-let (description
                 (describe-tile
                  (- (mouse-state-x mouse-state) (/ +tile-size+ 2))
                  (- (mouse-state-y mouse-state) (/ +tile-size+ 2))))
        (ui:layout-space-push :x 0.02 :y 1.0 :w 1.0 :h 1.0)
        (ui:label-wrap (format nil "You see ~a." description))))))

(define-constant +inventory-keys+ '(:1 :2 :3 :4 :5 :6 :7 :8 :9 :0 :a :b :c)
  :test #'equal)

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
      (loop :for item :in items
            :for key :in +inventory-keys+
            :for i :of-type fixnum :from 0
            :do (ui:selectable-label
                 (format nil "(~(~a~)) ~a" key (item-name item))
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
                (not *targeting*))
   :arguments ((ui-context cffi:foreign-pointer)))
  (when (plusp health-points)
    (al:with-current-keyboard-state keyboard-state
      (if (al:key-down keyboard-state :I)
          (block key-pressed
            (unless *inventory-key-pressed*
              (setf *inventory-key-pressed* t
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
                (not *targeting*))
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
