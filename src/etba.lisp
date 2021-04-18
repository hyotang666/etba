(in-package :cl-user)

(defpackage :etba
  (:use :cl)
  (:export)
  (:import-from :tovia #:main))

(in-package :etba)

(setq tovia:*title* "Eternal Battle")

(setq tovia:*scene* 'test)

;;;; SPECIALS

(defvar *player*)

;;;; TEXTURES

(defparameter *default-image-directory*
  (merge-pathnames "resources/images/"
                   (asdf:system-source-directory (asdf:find-system :etba))))

(defun image-pathname (name)
  (truename (merge-pathnames name *default-image-directory*)))

(macrolet ((def (name pathname &optional spritep &rest args)
             `(progn
               (fude-gl:defimage ,name (image-pathname ,pathname))
               (fude-gl:deftexture ,name :texture-2d
                 (fude-gl:tex-image-2d (fude-gl:image ,name))
                 :texture-min-filter :nearest
                 :texture-mag-filter :nearest)
               ,@(when spritep
                   `((tovia:defsprite ,name ,spritep
                       :unit 1/8
                       :texture (fude-gl:find-texture ,name)
                       :projection #'fude-gl:ortho
                       ,@args))))))
  (def :earth "backgrounds/earth.png")
  (def :romius "characters/romius.png" tovia:player)
  (def :hit "effects/hit.png")
  (def :mashroom "characters/mashroom.png" tovia:4-directional :response 8))

(tovia:defsprite :hit tovia:effect
  :unit 1/4
  :texture (fude-gl:find-texture :hit)
  :stepper (alexandria:circular-list '(0 3) '(1 3) '(2 3) nil)
  :timer 90
  :projection #'fude-gl:ortho)

(defun attack (subject win)
  (tovia:add
    (multiple-value-bind (x y)
        (tovia:front subject)
      (tovia:sprite :hit win :x x :y y :who subject))))

(defgeneric action (subject window)
  (:method (s w))
  (:method ((s tovia:4-directional) (win sdl2-ffi:sdl-window))
    (when (tovia:response? s)
      (if (oddp (random 2))
          (attack s win)
          (tovia:move s win :direction (tovia:target-direction s *player*)))))
  (:method ((player tovia:player) (win sdl2-ffi:sdl-window))
    (let ((tracker (tovia:tracker player)))
      (tovia:keypress-case
        (:f
         (if (tovia:key-down-p tracker #\f)
             (incf (tovia:current (tovia:key-tracker-time tracker)))
             (progn
              (attack player win)
              (tovia:update-keystate tracker #\f :down)
              (setf (tovia:current (tovia:key-tracker-time tracker)) 0))))
        (otherwise
         (cond
           ((tovia:key-down-p tracker #\f)
            (tovia:update-keystate tracker #\f :up))))))
    (tovia:move player win)))

;;;; SHADER
;; BACKGROUND-SHADER

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; DEFVERTICES need this eval-when.
  (defun make-map-offset (x y)
    (let ((result
           (make-array (list (* x y) 2)
                       :element-type 'single-float
                       :initial-element 0.0)))
      (loop :for i :below x
            :with w = (/ x)
            :do (loop :for j :below y
                      :with h = (/ y)
                      :do (setf (aref result (+ j (* i y)) 0)
                                  (float (- (* 2 i w) 1))
                                (aref result (+ j (* i y)) 1)
                                  (float (- (* 2 j h) 1)))))
      result)))

(fude-gl:defshader background-shader 330 (fude-gl:xy fude-gl:st fude-gl:offset)
  (:vertex ((coord :vec2))
    (declaim (ftype (function nil (values)) main))
    (defun main () "coord = st;" "gl_Position = vec4(xy + offset, 0.0, 1.0);"))
  (:fragment ((color :vec4) &uniform (tex :|sampler2D|))
    (declaim (ftype (function nil (values)) main))
    (defun main () "color = vec4(texture(tex, coord));")))

(fude-gl:defvertices tile
    (let ((w (float (* 2 (/ tovia:*width*))))
          (h (float (* 2 (/ (1- tovia:*height*))))))
      (coerce
        (vector 0.0 h 0.0 1.0 ; Top left
                0.0 0.0 0.0 0.0 ; Bottom left
                w h 1.0 1.0 ; Top right
                w 0.0 1.0 0.0) ; Bottom right
        '(array single-float (*))))
  :draw-mode :triangle-strip
  :shader 'background-shader
  :instances `((fude-gl:offset
                ,(make-map-offset tovia:*width* (1- tovia:*height*)))))

(defun status-bar (player win)
  (multiple-value-call #'gl:viewport 0 0 (sdl2:get-window-size win))
  (fude-gl:with-text-renderer (text :win win)
    (text (format nil "HP: ~S" (tovia:current (tovia:life player))) :x 0 :y
     tovia:*pixel-size* :scale tovia:*pixel-size*)))

(defun collision ()
  (quaspar:traverse tovia:*colliders*
                    (lambda (list)
                      (quaspar:do-unique-pair ((a b) list)
                        (when (tovia:collidep a b)
                          (tovia:react a b))))))

;;;; TRANSITIONS

(defun test (win)
  (uiop:nest
    (progn
     (setf *player* (tovia:add (tovia:sprite :romius win)))
     (multiple-value-bind (w h)
         (sdl2:get-window-size win)
       (tovia:add (tovia:sprite :mashroom win :x (/ w 2) :y (/ h 2)))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (multiple-value-bind (w h)
          (sdl2:get-window-size win)
        (gl:viewport 0 (tovia:boxel) w (- h (tovia:boxel))))
      (fude-gl:with-uniforms ((tex :unit 0))
          'background-shader
        (setf tex (fude-gl:find-texture :earth))
        (fude-gl:draw 'tile))
      (quaspar:do-lqtree (o tovia:*colliders*)
        (action o win))
      (collision)
      (quaspar:traverse tovia:*colliders*
                        (lambda (list) (mapc #'fude-gl:draw list)))
      (tovia:delete-lives)
      (when (tovia:deadp *player*)
        (signal 'tovia:sequence-transition :next 'game-over))
      (status-bar *player* win))))

(defun game-over (win)
  (fude-gl:with-text-renderer (text :win win)
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:keydown ()
        (signal 'tovia:sequence-transition :next 'test))
      (:idle ()
        (text "Game over" :x :center :y :center :scale tovia:*pixel-size*)
        (text "Push any key to retry." :x :center :y (* 3 (tovia:boxel)))
        (sdl2:gl-swap-window win)
        (sleep 0.5)))))