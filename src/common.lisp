(in-package #:lispy-rogue)


(define-constant +window-width+ 1280)
(define-constant +window-height+ 800)

(define-constant +world-width+ 960.0)
(define-constant +world-height+ 600.0)

(define-constant +tile-size+ 24.0)

(define-constant +ui-font-size+ 28)

(define-constant +inventory-keys+ '(:1 :2 :3 :4 :5 :6 :7 :8 :9 :0 :a :b :c)
  :test #'equal)

(declaim (inline round/tile-size)
         (ftype (function (single-float) single-float) round/tile-size))
(defun round/tile-size (x)
  (* +tile-size+ (the fixnum (round x +tile-size+))))

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
  (base-vision-range 0.0 :type single-float)
  (vision-range base-vision-range :type single-float)
  (base-speed 0.0 :type single-float)
  (speed base-speed :type single-float)
  (target-x single-float-nan :type single-float)
  (target-y single-float-nan :type single-float))

(ecs:defcomponent player
  (xp 0 :type fixnum)
  (level 1 :type fixnum)
  (player 1 :type bit :index player-entity :unique t))

(ecs:defcomponent stats
  (base-str 0 :type fixnum)
  (base-dex 0 :type fixnum)
  (base-int 0 :type fixnum)
  (str base-str :type fixnum)
  (dex base-dex :type fixnum)
  (int base-int :type fixnum))

(ecs:defcomponent health
  (base-max 0 :type fixnum)
  (max base-max :type fixnum)
  (points max :type fixnum))

(ecs:defcomponent mana
  (base-max 0 :type fixnum)
  (max base-max :type fixnum)
  (points max :type fixnum)
  (regen-elapsed 0.0 :type single-float))

(ecs:defcomponent attack
  (elapsed 0.0 :type single-float)
  (target -1 :type ecs:entity))

(ecs:defcomponent defense
  (base-evasion 0.0 :type single-float)
  (base-block-chance 0.0 :type single-float)
  (base-armor 0.0 :type single-float)
  (evasion base-evasion :type single-float)
  (block-chance base-block-chance :type single-float)
  (armor base-armor :type single-float))

(ecs:defcomponent offense
  (range 0.0 :type single-float)
  (base-duration 0.0 :type single-float)
  (base-accuracy 0.0 :type single-float)
  (base-min-damage 0.0 :type single-float)
  (base-max-damage 0.0 :type single-float)
  (duration base-duration :type single-float)
  (accuracy base-accuracy :type single-float)
  (min-damage base-min-damage :type single-float)
  (max-damage base-max-damage :type single-float))

(defmacro define-weighted-random-generator (name &rest cases)
  `(defun ,name ()
     (cdr (assoc (random 1.0)
                 (load-time-value
                  ',(loop :for (weight value) :in cases
                          :summing weight :into total
                          :collecting (cons total value)
                          :finally (unless (= 1.0 total)
                                     (error "Weights do not add up to 1: ~a"
                                            total)))
                  t)
                 :test #'<))))

(declaim
 (inline approx-equal)
 (ftype (function (single-float single-float &optional single-float) boolean)
        approx-equal))
(defun approx-equal (a b &optional (epsilon 0.5))
  (< (abs (- a b)) epsilon))

(declaim
 (inline rating->chance)
 (ftype (function (single-float) single-float) rating->chance))
(defun rating->chance (value)
  (/ 1.0 (+ 1.0 (sqrt (max 0.0 value)))))

(declaim (inline nearest-tile)
         (ftype (function (single-float single-float single-float single-float)
                          (values single-float single-float))
                nearest-tile))
(defun nearest-tile (source-x source-y dest-x dest-y)
  (values
   (- dest-x (* +tile-size+ (signum (- dest-x source-x))))
   (- dest-y (* +tile-size+ (signum (- dest-y source-y))))))

(declaim (inline random-from-range))
(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))

(declaim (ftype (function (simple-string ecs:entity) simple-string) verb))
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

(declaim (type ecs:entity *current-map*))
(defvar *current-map*)

(declaim (type boolean *turn* *should-quit* *won*))
(defvar *turn* nil)
(defvar *should-quit*)
(defvar *won* nil)

(declaim (type simple-string *message-log*))
(defparameter *message-log* "")

(declaim (type boolean *message-log-focused*))
(defparameter *message-log-focused* nil)

(declaim (type boolean *targeting*))
(defparameter *targeting* nil)

(declaim (type boolean *help-key-pressed* *help-shown*))
(defparameter *help-key-pressed* nil)
(defparameter *help-shown* t)

(defparameter *hovered-item* -1)

(declaim (type boolean *inventory-key-pressed* *inventory-shown*))
(defparameter *inventory-key-pressed* nil)
(defparameter *inventory-shown* nil)

(declaim (type boolean *throw-key-pressed* *throw-window-shown*))
(defparameter *throw-key-pressed* nil)
(defparameter *throw-window-shown* nil)

(declaim (type boolean *levelup-shown* *levelup-key-pressed*))
(defparameter *levelup-shown* nil)
(defparameter *levelup-key-pressed* nil)

(declaim (type boolean *targeting-key-pressed*))
(defparameter *targeting-key-pressed* nil)

(define-constant newline (format nil "~%") :test #'string=)

(defun log-message (control &rest args)
  (setf *message-log* (concatenate 'string
                                   (apply #'format nil control args)
                                   newline
                                   *message-log*)))

(declaim (inline mouse-state-x mouse-state-y mouse-state-buttons))
(defun mouse-state-x (mouse-state)
  (cffi:foreign-slot-value mouse-state '(:struct al:mouse-state) 'al::x))
(defun mouse-state-y (mouse-state)
  (cffi:foreign-slot-value mouse-state '(:struct al:mouse-state) 'al::y))
(defun mouse-state-buttons (mouse-state)
  (cffi:foreign-slot-value mouse-state '(:struct al:mouse-state) 'al::buttons))
