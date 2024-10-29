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

(ui:defwindow stats ()
    (:x +world-width+ :y 0
     :w (- +window-width+ +world-width+) :h +window-height+
     :styles ((:item-color :window-fixed-background :r 0 :g 0 :b 0)))
  (ui:layout-space (:height +ui-font-size+ :format :dynamic)
    (ui:layout-space-push :x 0 :y 0 :w 1.0 :h 1.0)
    (with-health () (player-entity 1)
      (ui:label (format nil "HP: ~a / ~a" points max)))))
