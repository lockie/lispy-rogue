(in-package #:lispy-rogue)


(ecs:defcomponent ranged)

(defstruct point
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defun object/firing-line (x1 y1 x2 y2)
  (let* ((x1* (floor x1 +tile-size+)) (x2* (floor x2 +tile-size+))
         (y1* (floor y1 +tile-size+)) (y2* (floor y2 +tile-size+))
         (dx  (- x2* x1*))
         (dy  (- y2* y1*))
         (steep (> (abs dy) (abs dx)))
         (swapped nil) (error 0) (ystep 0) (y 0) (points nil))
    (when steep
      (rotatef x1* y1*)
      (rotatef x2* y2*))
    (when (> x1* x2*)
      (rotatef x1* x2*)
      (rotatef y1* y2*)
      (setf swapped t))
    (setf dx (- x2* x1*)
          dy (- y2* y1*)
          error (floor dx 2)
          ystep (if (< y1* y2*) 1 -1)
          y y1*)
    (loop :for x :from x1* :to x2*
          :do (push (if steep
                        (make-point :x y :y x)
                        (make-point :x x :y y))
                    points)
              (decf error (abs dy))
              (when (< error 0)
                (incf y ystep)
                (incf error dx))
          :finally (unless swapped
                     (setf points (nreverse points)))
                   (return
                     (loop :for point :in (rest points)
                           :for point-x := (* (point-x point) +tile-size+)
                           :for point-y := (* (point-y point) +tile-size+)
                           :do (when-let (tile (blocked -1 point-x point-y))
                                 (return tile)))))))
