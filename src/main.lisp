(in-package #:lispy-rogue)

(define-constant +repl-update-interval+ 0.3d0)

(defvar *resources-path*
  (asdf:system-relative-pathname :lispy-rogue #P"Resources/"))

(deploy:define-hook (:boot set-resources-path) ()
  (setf *resources-path*
        (merge-pathnames #P"Resources/"
                         (uiop:pathname-parent-directory-pathname
                          (deploy:runtime-directory)))))

(define-constant +font-path+ "inconsolata.ttf" :test #'string=)
(define-constant +font-size+ 24)
(define-constant +ui-font-path+ "fantasque-sans-mono.ttf" :test #'string=)


(define-constant +config-path+ "../config.cfg" :test #'string=)

(defun init ()
  (setf *random-state* (make-random-state t))
  (ecs:bind-storage)
  (let ((soundtrack (al:create-sample-instance
                     (al:load-sample "dungeon.ogg"))))
    (al:attach-sample-instance-to-mixer soundtrack (al:get-default-mixer))
    (al:set-sample-instance-playmode soundtrack :loop)
    (al:set-sample-instance-playing soundtrack t))
  (load-sprites "urizen-tileset.tsx")
  (make-starting-equipment
   (make-player-object 0.0 0.0))
  (make-map 1)
  (setf *message-log* "")
  (log-message "You enter the dungeon."))

(declaim (type fixnum *fps*))
(defvar *fps* 0)

(defun update (dt keyboard-state ui-context)
  (unless (zerop dt)
    (setf *fps* (round 1 dt)))
  (if *won*
    (congratulations ui-context)
    (progn
      (message-log ui-context)
      (info ui-context)
      (ecs:run-systems :dt (float dt 0.0)
                       :keyboard-state keyboard-state
                       :ui-context ui-context))))

(defvar *font*)

(defun render ()
  (nk:allegro-render)
  #+nil (al:draw-text *font* (al:map-rgba 255 255 255 0) 0 0 0
                      (format nil "~d FPS" *fps*)))

(cffi:defcallback %main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (handler-bind
      ((error #'(lambda (e) (unless *debugger-hook*
                         (al:show-native-message-box
                          (cffi:null-pointer) "Hey guys"
                          "We got a big error here :("
                          (with-output-to-string (s)
                            (uiop:print-condition-backtrace e :stream s))
                          (cffi:null-pointer) :error)))))
    (uiop:chdir (setf *default-pathname-defaults* *resources-path*))
    (al:set-app-name "lispy-rogue")
    (unless (al:init)
      (error "Initializing liballegro failed"))
    (let ((config (al:load-config-file +config-path+)))
      (unless (cffi:null-pointer-p config)
        (al:merge-config-into (al:get-system-config) config)))
    (unless (al:init-primitives-addon)
      (error "Initializing primitives addon failed"))
    (unless (al:init-image-addon)
      (error "Initializing image addon failed"))
    (unless (al:init-font-addon)
      (error "Initializing liballegro font addon failed"))
    (unless (al:init-ttf-addon)
      (error "Initializing liballegro TTF addon failed"))
    (unless (al:install-audio)
      (error "Intializing audio addon failed"))
    (unless (al:init-acodec-addon)
      (error "Initializing audio codec addon failed"))
    (unless (al:restore-default-mixer)
      (error "Initializing default audio mixer failed"))
    (let ((display (al:create-display +window-width+ +window-height+))
          (event-queue (al:create-event-queue)))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (al:inhibit-screensaver t)
      (al:set-window-title display "Lispy Rogue")
      (al:register-event-source event-queue
                                (al:get-display-event-source display))
      (al:install-keyboard)
      (al:register-event-source event-queue
                                (al:get-keyboard-event-source))
      (al:install-mouse)
      (al:register-event-source event-queue
                                (al:get-mouse-event-source))
      (unwind-protect
           (cffi:with-foreign-objects
               ((event '(:union al:event))
                (keyboard-state '(:struct al::keyboard-state)))
             (init)
             (#+darwin trivial-main-thread:call-in-main-thread #-darwin funcall
              #'livesupport:setup-lisp-repl)
             (loop
               :named main-game-loop
               :with *font* := (al:ensure-loaded #'al:load-ttf-font
                                                 +font-path+
                                                 (- +font-size+) 0)
               :with ui-font := (al:ensure-loaded
                                 #'nk:allegro-font-create-from-file
                                 +ui-font-path+ (- +ui-font-size+) 0)
               :with ui-context := (nk:allegro-init ui-font display
                                                    +window-width+
                                                    +window-height+)
               :with ticks :of-type double-float := (al:get-time)
               :with last-repl-update :of-type double-float := ticks
               :with dt :of-type double-float := 0d0
               :while (loop
                        :named event-loop
                        :initially (nk:input-begin ui-context)
                        :while (al:get-next-event event-queue event)
                        :for type := (cffi:foreign-slot-value
                                      event '(:union al:event) 'al::type)
                        :do (nk:allegro-handle-event event)
                        :always (not (eq type :display-close))
                        :finally (nk:input-end ui-context))
               :do (let ((new-ticks (al:get-time)))
                     (setf dt (- new-ticks ticks)
                           ticks new-ticks))
                   (when (> (- ticks last-repl-update)
                            +repl-update-interval+)
                     (livesupport:update-repl-link)
                     (setf last-repl-update ticks))
                   (al:clear-to-color (al:map-rgb 0 0 0))
                   (al:get-keyboard-state keyboard-state)
                   (livesupport:continuable
                     (update dt keyboard-state ui-context)
                     (render))
                   (al:flip-display)
               :finally (nk:allegro-shutdown)
                        (nk:allegro-font-del ui-font)
                        (al:destroy-font *font*)))
        (al:inhibit-screensaver nil)
        (al:destroy-event-queue event-queue)
        (al:destroy-display display)
        (al:stop-samples)
        (al:uninstall-system)
        (al:uninstall-audio)
        (al:shutdown-ttf-addon)
        (al:shutdown-font-addon)
        (al:shutdown-image-addon))))
  0)

(defun main ()
  (#+darwin trivial-main-thread:with-body-in-main-thread #-darwin progn nil
    (float-features:with-float-traps-masked
        (:divide-by-zero :invalid :inexact :overflow :underflow)
      (al:run-main 0 (cffi:null-pointer) (cffi:callback %main)))))

