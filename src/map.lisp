(in-package #:roguelike)


(defun blocked (x y)
  (loop :for tile :of-type ecs:entity :in (tiles (a*:encode-float-coordinates
                                                  (round/tile-size x)
                                                  (round/tile-size y)))
        :thereis (and (has-map-tile-p tile)
                      (plusp (map-tile-blocks tile)))))

(defun make-room (x1 y1 x2 y2)
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
                (change-sprite tile :floor)))))

(defun make-horizontal-tunnel (x1 x2 y)
  (loop
    :for x :of-type single-float
    :from (min x1 x2) :to (max x1 x2) :by +tile-size+
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
                  :do (make-map-tile
                       (make-sprite-object :wall x y)
                       :blocks 1)))
  (make-room 120.0 120.0 240.0 576.0)
  (make-room 360.0 360.0 576.0 576.0)
  (make-horizontal-tunnel 240.0 360.0 480.0)
  
  )
