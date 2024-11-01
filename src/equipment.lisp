(in-package #:roguelike)


(define-constant +equipment-slots+
    '(:armor :boots :gloves :helm :shield :weapon :amulet :ring)
  :test #'equal)

(ecs:defcomponent equipment
  (slot :|| :type keyword))

(ecs:defcomponent equipped
  (slot :|| :type keyword :index equipped :unique t))

(ecs:defcomponent bonus
  (str-flat 0 :type fixnum)
  (dex-flat 0 :type fixnum)
  (int-flat 0 :type fixnum)
  (str-mult 0.0 :type single-float)
  (dex-mult 0.0 :type single-float)
  (int-mult 0.0 :type single-float)
  (hp-flat 0 :type fixnum)
  (mp-flat 0 :type fixnum)
  (hp-mult 0.0 :type single-float)
  (mp-mult 0.0 :type single-float)
  (speed-flat 0.0 :type single-float)
  (speed-mult 0.0 :type single-float)
  (evasion-flat 0 :type fixnum)
  (block-chance-flat 0.0 :type single-float :documentation "dummy, always zero")
  (armor-flat 0 :type fixnum)
  (evasion-mult 0.0 :type single-float)
  (armor-mult 0.0 :type single-float)
  (block-chance-mult 0.0 :type single-float)
  (attack-speed-flat 0.0 :type single-float)
  (accuracy-flat 0 :type fixnum)
  (min-dmg-flat 0 :type fixnum)
  (max-dmg-flat 0 :type fixnum)
  (attack-speed-mult 0.0 :type single-float)
  (accuracy-mult 0.0 :type single-float)
  (min-dmg-mult 0.0 :type single-float)
  (max-dmg-mult 0.0 :type single-float))

(defun add-random-bonus (item ilvl)
  "Ugh."
  (with-bonus () item
    (case (random 25)
      (0  (incf str-flat          (random ilvl)))
      (1  (incf dex-flat          (random ilvl)))
      (2  (incf int-flat          (random ilvl)))
      (3  (incf str-mult          (random (* 0.1 ilvl))))
      (4  (incf dex-mult          (random (* 0.1 ilvl))))
      (5  (incf int-mult          (random (* 0.1 ilvl))))
      (6  (incf hp-flat           (random (* 10 ilvl))))
      (7  (incf mp-flat           (random (* 10 ilvl))))
      (8  (incf hp-mult           (random (* 0.1 ilvl))))
      (9  (incf mp-mult           (random (* 0.1 ilvl))))
      (10 (incf speed-flat        (random (* 10.0 ilvl))))
      (11 (incf speed-mult        (random (* 0.1 ilvl))))
      (12 (incf evasion-flat      (random (* 10 ilvl))))
      (13 (incf armor-flat        (random (* 10 ilvl))))
      (14 (incf evasion-mult      (random (* 0.1 ilvl))))
      (15 (incf block-chance-mult (random (- 1.0 (/ 1.0 (exp (* 0.1 ilvl)))))))
      (16 (incf armor-mult        (random (* 0.1 ilvl))))
      (17 (incf attack-speed-flat (random (* 1.0 ilvl))))
      (18 (incf accuracy-flat     (random (* 10 ilvl))))
      (19 (incf min-dmg-flat      (random (* 10 ilvl))))
      (20 (incf max-dmg-flat      (random (* 10 ilvl))))
      (21 (incf attack-speed-mult (random (* 0.1 ilvl))))
      (22 (incf accuracy-mult     (random (* 0.1 ilvl))))
      (23 (incf min-dmg-mult      (random (* 0.1 ilvl))))
      (24 (incf max-dmg-mult      (random (* 0.1 ilvl)))))))

(define-modify-macro mulf (&rest args) *)

(defmacro scale-parameter/items (param base-value &key int)
  `(loop :with mult :of-type single-float := 1.0
         :for item :in items
         :when (has-equipped-p item)
         :sum (,(symbolicate :bonus- param :-flat) item) :into flat
         :and :do (mulf mult (1+ (,(symbolicate :bonus- param :-mult) item)))
         :finally (return (,(if int 'floor 'identity)
                           (* (+ ,base-value flat) mult)))))

(defun recalculate-combat-parameters (character)
  (let ((items (items character)))
    (with-stats () character
      (setf str (scale-parameter/items str base-str :int t)
            dex (scale-parameter/items dex base-dex :int t)
            int (scale-parameter/items int base-int :int t))
      (with-health () character
        (setf max (scale-parameter/items hp (+ base-max (* str 10.0)) :int t)))
      (with-mana () character
        (setf max (scale-parameter/items mp (+ base-max (* int 10.0)) :int t)))
      (with-character () character
        (setf speed (scale-parameter/items speed base-speed)))
      (with-defense () character
        (setf evasion      (scale-parameter/items
                            evasion
                            (* base-evasion (1+ (/ dex 20.0))))
              block-chance (if (ecs:entity-valid-p
                                (equipped :shield :missing-error-p nil))
                               (min
                                (scale-parameter/items
                                 block-chance
                                 base-block-chance)
                                0.75)
                               0.0)
              armor        (scale-parameter/items
                            armor
                            (+ base-armor (* str 20.0)))))
      (with-offense () character
        (setf duration   (/ 1.0
                            (scale-parameter/items attack-speed
                                                   (/ 1.0 base-duration)))
              accuracy   (scale-parameter/items
                          accuracy
                          (+ base-accuracy (* dex 2.0)))
              ;; NOTE: scaling damage identically for both melee and ranged
              min-damage (scale-parameter/items
                          min-dmg
                          (+ base-min-damage (1+ (/ str 20.0))))
              max-damage (max
                          min-damage
                          (scale-parameter/items
                           max-dmg
                           (+ base-max-damage (1+ (/ str 20.0))))))))))

(define-weighted-random-generator random-equipment-slot
  (0.3  :armor)
  (0.1  :boots)
  (0.1  :gloves)
  (0.1  :helm)
  (0.15 :shield)
  (0.15 :weapon)
  (0.05 :amulet)
  (0.05 :ring))

(define-weighted-random-generator random-weapon-type
  (0.085 :sword)
  (0.085 :dagger)
  (0.085 :club)
  (0.085 :axe)
  (0.33  :bow)
  (0.165 :staff)
  (0.165 :wand))

(define-weighted-random-generator random-grade
  (0.8 :common)
  (0.15 :magic)
  (0.05 :rare))

(defun make-equipment-item (ilvl x y)
  (let* ((slot (random-equipment-slot))
         (grade (random-grade))
         (weapon-type (random-weapon-type))
         (object (make-sprite-object
                  (case slot
                    (:armor (format-symbol :keyword "ARMOR~a" (random 13)))
                    (:boots :boots)
                    (:gloves :gloves)
                    (:helm (format-symbol :keyword "HELM~a" (random 4)))
                    (:shield (format-symbol :keyword "SHIELD~a" (random 3)))
                    (:weapon weapon-type)
                    (:amulet (format-symbol :keyword "AMULET~a" (random 12)))
                    (:ring (format-symbol :keyword "RING~a" (random 7))))
                  x y)))
    (make-item object :name (format nil "the ~(~a~) ~(~a~)" grade
                                    (if (eq slot :weapon) weapon-type slot)))
    (make-equipment object :slot slot)
    (make-bonus object)
    (with-bonus () object
      ;; base characteristic
      (case slot
        (:armor
         (case (random 2)
           (0 (setf armor-flat (1+ (random (* 10 ilvl)))))
           (1 (setf evasion-flat (1+ (random (* 10 ilvl)))))))
        (:boots
         (setf speed-flat (1+ (random (* 10.0 ilvl)))))
        (:gloves
         (setf attack-speed-flat (random (* 0.1 ilvl))))
        (:helm
         (setf armor-flat (1+ (random (* 10 ilvl)))))
        (:shield
         (setf block-chance-mult (random (* 0.1 ilvl))))
        (:weapon
         ;; TODO magic
         (case weapon-type
           ((:sword :club :axe)
            (setf min-dmg-flat (1+ (random (* 10 ilvl)))
                  max-dmg-flat (1+ (random (* 10 ilvl)))))
           ((:dagger :bow)
            (setf accuracy-flat (1+ (random (* 10 ilvl))))))))
      ;; extra bonuses
      (case grade
        (:magic
         (add-random-bonus object ilvl))
        (:rare
         (dotimes (_ (+ 2 (min (random 4) (random 4))))
           (add-random-bonus object ilvl)))))
    object))

(defmacro %describe-slot (slot name)
  (let ((flat (uiop:string-suffix-p slot "FLAT")))
    `(unless (zerop ,slot)
       (push
        ,(if flat
             `(format nil "~@d ~a" (round ,slot) ,name)
             `(format nil "~@d% ~a" (round (* ,slot 100)) ,name)) 
        descriptions))))

(defun describe-equipment (item)
  (if (has-equipment-p item)
      (with-bonus () item
        (let ((descriptions nil))
          (declare (dynamic-extent descriptions))
          (%describe-slot str-flat "strength")
          (%describe-slot dex-flat "dexterity")
          (%describe-slot int-flat "intelligence")
          (%describe-slot str-mult "strength")
          (%describe-slot dex-mult "dexterity")
          (%describe-slot int-mult "intelligence")
          (%describe-slot hp-flat "health points")
          (%describe-slot mp-flat "mana points")
          (%describe-slot hp-mult "health points")
          (%describe-slot mp-mult "mana points")
          (%describe-slot speed-flat "movement speed")
          (%describe-slot speed-mult "movement speed")
          (%describe-slot evasion-flat "evasion")
          (%describe-slot armor-flat "armor")
          (%describe-slot evasion-mult "evasion")
          (%describe-slot armor-mult "armor")
          (%describe-slot block-chance-mult "block chance")
          (unless (zerop attack-speed-flat)
            (push
             (format nil "~,1@f attack speed" attack-speed-flat)
             descriptions))
          (%describe-slot accuracy-flat "accuracy")
          (%describe-slot min-dmg-flat "min damage")
          (%describe-slot max-dmg-flat "max damage")
          (%describe-slot attack-speed-mult "attack speed")
          (%describe-slot accuracy-mult "accuracy")
          (%describe-slot min-dmg-mult "min damage")
          (%describe-slot max-dmg-mult "max damage")
          (format nil "~{~a~%~}" descriptions)))
      ""))

(defun toggle-equipped (item)
  (let ((equipped (has-equipped-p item))
        (slot (equipment-slot item)))
    (if equipped
        (block unequip
          (delete-equipped item)
          (recalculate-combat-parameters (player-entity 1))
          (log-message "You unequip ~a." (item-name item)))
        (block equip
          (let ((currently-equipped (equipped slot :missing-error-p nil)))
            (when (ecs:entity-valid-p currently-equipped)
              (toggle-equipped currently-equipped)))
          (assign-equipped item :slot slot)
          (recalculate-combat-parameters (player-entity 1))
          (log-message "You equip ~a." (item-name item))))))
