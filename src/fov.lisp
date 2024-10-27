(in-package #:roguelike)


(ecs:defcomponent view
  (lit 0 :type bit :index fov))


(defun lit (x y)
  (loop :for tile :of-type ecs:entity :in (tiles (a*:encode-float-coordinates
                                                  (round/tile-size x)
                                                  (round/tile-size y)))
        :always (or (not (has-view-p tile))
                    (plusp (view-lit tile)))))

(defun set-lit (x y)
  (loop :for tile :of-type ecs:entity :in (tiles (a*:encode-float-coordinates
                                                  (round/tile-size x)
                                                  (round/tile-size y)))
        :when (has-view-p tile)
        :do (setf (view-lit tile) 1)
            (loop-finish)))

(defun obscures (x y)
  (loop :for tile :of-type ecs:entity :in (tiles (a*:encode-float-coordinates
                                                  (round/tile-size x)
                                                  (round/tile-size y)))
        :thereis (and (has-map-tile-p tile)
                      (plusp (map-tile-obscures tile)))))

;; based on https://roguebasin.com/index.php/Python_shadowcasting_implementation

(define-constant +mult+ #(( 24  0  0  24)
                          ( 0  24  24  0)
                          ( 0 -24  24  0)
                          (-24  0  0  24)
                          (-24  0  0 -24)
                          ( 0 -24 -24  0)
                          ( 0  24 -24  0)
                          ( 24  0  0 -24))
  :test #'equalp)

(defun cast-light (cx cy row start end radius xx xy yx yy)
  (declare (type fixnum row))
  (unless (< start end)
    (loop
      :with radius* := (/ (* radius radius) (* +tile-size+ +tile-size+)) 
      :with new-start
      :for j :of-type fixnum :from row :to (floor radius +tile-size+)
      :for dx :of-type fixnum := (- (1+ j))
      :for dy :of-type fixnum := (- j)
      :for obscured :of-type boolean := nil
      :do (loop :while (<= dx 0)
                :do (incf dx)
                    (let ((X (+ cx (* dx xx) (* dy xy)))
                          (Y (+ cy (* dx yx) (* dy yy)))
                          (lslope (/ (- dx 0.5) (+ dy 0.5)))
                          (rslope (/ (+ dx 0.5) (- dy 0.5))))
                      (cond ((< start rslope))
                            ((> end lslope)
                             (loop-finish))
                            (t
                             (when (>= radius* (+ (* dx dx) (* dy dy)))
                               (set-lit X Y))
                             (if obscured
                                 (if (obscures X Y)
                                     (setf new-start rslope)
                                     (setf obscured nil
                                           start new-start))
                                 (when (and (obscures X Y)
                                            (< j (/ radius +tile-size+)))
                                   (cast-light cx cy (1+ j) start lslope radius
                                               xx xy yx yy)
                                   (setf obscured t
                                         new-start rslope)))))))
      :never obscured)))

(defun recalculate-fov (player-x player-y radius)
  (dolist (v (fov 1))
    (setf (view-lit v) 0))
  (loop :for (xx xy yx yy) :across +mult+
        :do (cast-light player-x player-y 0 1.0 0.0 radius xx xy yx yy)))
