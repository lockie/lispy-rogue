(in-package #:roguelike)


(ecs:defcomponent sprite-prefab
  (name :|| :type keyword :index sprite-prefab :unique t))

(ecs:defcomponent (sprite :finalize (lambda (entity &key bitmap)
                                      (when (has-sprite-prefab-p entity)
                                        (al:destroy-bitmap bitmap))))
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer))

(ecs:defsystem draw-sprites
  (:components-ro (tile sprite)
   :after (set-tile)
   :initially (al:hold-bitmap-drawing t)
   :finally (al:hold-bitmap-drawing nil))
  (al:draw-bitmap sprite-bitmap tile-col tile-row 0))

(defun load-bitmap (filename)
  (al:ensure-loaded #'al:load-bitmap (namestring filename)))

(defun load-sprites (filename)
  (let* ((tileset (tiled:load-tileset filename))
         (image-source (tiled:image-source (tiled:tileset-image tileset)))
         (image (load-bitmap image-source)))
    (dolist (tile (tiled:tileset-tiles tileset))
      (ecs:make-object
       `((:sprite-prefab :name ,(make-keyword
                                 (string-upcase
                                  (gethash "name" (tiled:properties tile)))))
         (:sprite :bitmap ,(al:create-sub-bitmap image
                                                 (tiled:tile-pixel-x tile)
                                                 (tiled:tile-pixel-y tile)
                                                 (floor +tile-size+)
                                                 (floor +tile-size+))))))))

(defun make-sprite-object (name x y)
  (let ((prefab (sprite-prefab name)))
    (ecs:make-object
     `((:sprite :bitmap ,(sprite-bitmap prefab))
       (:position :x ,x :y ,y)
       (:tile :col ,(* +tile-size+ (floor x +tile-size+))
              :row ,(* +tile-size+ (floor y +tile-size+)))))))

(defun change-sprite (object new-sprite)
  (setf (sprite-bitmap object)
        (sprite-bitmap (sprite-prefab new-sprite))))
