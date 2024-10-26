(in-package #:roguelike)


(defun make-map ()
  (loop :for x :of-type single-float
        :from 0.0 :below +world-width+ :by +tile-size+
        :do (loop :for y :of-type single-float
                  :from 0.0 :below +world-height+ :by +tile-size+
                  :do (make-map-tile
                       (make-sprite-object :grass1 x y)))))
