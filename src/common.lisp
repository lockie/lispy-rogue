(in-package #:roguelike)


(define-constant +window-width+ 1280)
(define-constant +window-height+ 800)

(define-constant +world-width+ 960.0)
(define-constant +world-height+ 600.0)

(define-constant +tile-size+ 24.0)

(declaim ;;(inline round/tile-size)
         (ftype (function (single-float) single-float) round/tile-size))
(defun round/tile-size (x)
  (* +tile-size+ (round x +tile-size+)))

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

(ecs:defcomponent map-tile
  (blocks 0 :type bit)
  (obscures blocks :type bit))

(ecs:defsystem set-tile
  (:components-ro (position)
   :components-no (map-tile)
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

(declaim ;;(inline nearest-tile)
         (ftype (function (single-float single-float single-float single-float)
                          (values single-float single-float))
                nearest-tile))
(defun nearest-tile (source-x source-y dest-x dest-y)
  (values
   (- dest-x (* +tile-size+ (signum (- dest-x source-x))))
   (- dest-y (* +tile-size+ (signum (- dest-y source-y))))))

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))

(declaim (type boolean *turn* *should-quit*))
(defvar *turn* nil)
(defvar *should-quit*)

