(in-package #:roguelike)


(define-constant +window-width+ 1280)
(define-constant +window-height+ 800)

(define-constant +tile-size+ 24.0)

(declaim (inline round/tile-size)
         (ftype (function (single-float) single-float) round/tile-size))
(defun round/tile-size (x)
  (* +tile-size+ (floor x +tile-size+)))

(ecs:defcomponent parent
    (entity -1 :type ecs:entity :index children))

(ecs:hook-up ecs:*entity-deleting-hook*
             (lambda (entity)
               (dolist (child (children entity))
                 (ecs:delete-entity child))))

(ecs:defcomponent position
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))

(ecs:defcomponent tile
  (col 0.0 :type single-float)
  (row 0.0 :type single-float)
  (hash (a*:encode-float-coordinates
         (round/tile-size col)
         (round/tile-size row))
        :type fixnum :index tiles))

(ecs:defsystem set-tile
  (:components-ro (position)
   :components-rw (tile))
  (setf tile-col (round/tile-size position-x)
        tile-row (round/tile-size position-y)
        tile-hash (a*:encode-float-coordinates tile-col tile-row)))

(declaim
 (inline approx-equal)
 (ftype (function (single-float single-float &optional single-float) boolean)
        approx-equal))
(defun approx-equal (a b &optional (epsilon 0.5))
  (< (abs (- a b)) epsilon))

(defvar *should-quit*)
