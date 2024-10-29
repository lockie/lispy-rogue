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

(ecs:defcomponent character
  (name "" :type simple-string)
  (vision-range 0.0 :type single-float)
  (speed 0.0 :type single-float)
  (target-x single-float-nan :type single-float)
  (target-y single-float-nan :type single-float))

(ecs:defcomponent attack
  (elapsed 0.0 :type single-float)
  (target -1 :type ecs:entity))

(ecs:defcomponent defense
  (evasion 0.0 :type single-float)
  (dodge 0.0 :type single-float)
  (block-chance 0.0 :type single-float)
  (armor 0.0 :type single-float))

(declaim
 (inline approx-equal)
 (ftype (function (single-float single-float &optional single-float) boolean)
        approx-equal))
(defun approx-equal (a b &optional (epsilon 0.5))
  (< (abs (- a b)) epsilon))

(declaim
 ;;(inline rating->chance)
 (ftype (function (single-float) single-float) rating->chance))
(defun rating->chance (value)
  (/ 1.0 (+ 1.0 (sqrt (max 0.0 value)))))

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

(defun verb (infinitive entity)
  (cond
    ((has-player-p entity)
     infinitive)

    ((or (uiop:string-suffix-p infinitive  "s")
         (uiop:string-suffix-p infinitive  "z")
         (uiop:string-suffix-p infinitive  "x")
         (uiop:string-suffix-p infinitive "sh")
         (uiop:string-suffix-p infinitive "ch"))
     (concatenate 'string infinitive "es"))

    ((and (uiop:string-suffix-p infinitive "y")
          (not (member (char infinitive (- (length infinitive) 2))
                       '(#\a #\e #\i #\o #\u)
                       :test #'char=)))
     (concatenate 'string (subseq infinitive 0 (1- (length infinitive))) "ies"))

    (t (concatenate 'string infinitive "s"))))

(declaim (type boolean *turn* *should-quit*))
(defvar *turn* nil)
(defvar *should-quit*)

(declaim (type list *message-log*))
(defparameter *message-log* nil)

(declaim (type boolean *message-log-focused*))
(defparameter *message-log-focused* nil)

(defun log-message (control &rest args)
  (push (apply #'format nil control args) *message-log*))
