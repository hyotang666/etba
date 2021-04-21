(in-package :cl-user)

(defpackage :etba
  (:use :cl)
  (:export)
  (:import-from :tovia #:main))

(in-package :etba)

(setq tovia:*title* "Eternal Battle")

(setq tovia:*scene* 'start)

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
  (def :energy "effects/energy.png")
  (def :barrage "effects/barrage.png")
  (def :mashroom "characters/mashroom.png" tovia:npc :response 8))

(tovia:defsprite :hit tovia:effect
  :unit 1/4
  :texture (fude-gl:find-texture :hit)
  :stepper (alexandria:circular-list '(0 3) '(1 3) '(2 3) nil)
  :timer 90
  :projection #'fude-gl:ortho
  :effects (list (tovia:damager 10) (tovia:knock-backer 10)))

(tovia:defsprite :energy tovia:projectile
  :unit 1/8
  :texture (fude-gl:find-texture :energy)
  :timer 90
  :projection #'fude-gl:ortho
  :effects (list (tovia:damager 10) (tovia:knock-backer 10)))

(tovia:defsprite :barrage tovia:melee
  :unit 1/8
  :texture (fude-gl:find-texture :barrage)
  :stepper (alexandria:circular-list 0 1 2)
  :timer 180
  :projection #'fude-gl:ortho
  :life 40
  :effects (list (tovia:damager 10) (tovia:knock-backer 10)))

(defgeneric attack (subject win arm &rest args)
  (:method ((subject tovia:being) (win sdl2-ffi:sdl-window) arm &rest args)
    (tovia:add
      (multiple-value-bind (x y)
          (tovia:front subject)
        (apply #'tovia:sprite arm win :x x :y y :who subject :direction
               (tovia:last-direction subject) :allow-other-keys t args))))
  (:method :before ((subject tovia:being) (win sdl2-ffi:sdl-window)
                    (arm (eql :barrage)) &rest args)
    (declare (ignore args))
    (setf (tovia:coeff-of :response subject)
            (acons :stun (constantly nil) (tovia:coeff-of :response subject))))
  (:method :before ((subject tovia:being) (win sdl2-ffi:sdl-window)
                    (arm (eql :step-in-hit)) &rest args)
    (declare (ignore args))
    (setf (tovia:coeff-of :move subject)
            (acons :step-in (constantly (/ (tovia:boxel) 2))
                   (tovia:coeff-of :move subject)))
    (tovia:move subject win :direction (tovia:last-direction subject))
    (setf (tovia:coeff-of :move subject)
            (delete :step-in (tovia:coeff-of :move subject) :key #'car)))
  (:method ((subject tovia:being) (win sdl2-ffi:sdl-window)
            (arm (eql :step-in-hit)) &rest args)
    (apply #'call-next-method subject win :hit args))
  (:method :around ((subject tovia:player) (win sdl2-ffi:sdl-window)
                    (arm (eql :energy)) &rest args)
    (declare (ignore args))
    (let* ((tracker (tovia:tracker subject))
           (time (tovia:current (tovia:key-tracker-time tracker))))
      (setf (tovia:current (tovia:key-tracker-time tracker)) 0)
      (when (< 10 time)
        (call-next-method subject win :energy :life time)))))

(defgeneric action (subject window)
  (:method (s w))
  (:method ((s tovia:melee) (win sdl2-ffi:sdl-window))
    (when (<= (decf (tovia:current (tovia:life s))) 0)
      (setf (tovia:coeff-of :response (tovia:who s))
              (delete :stun (tovia:coeff-of :response (tovia:who s))
                      :key #'car))))
  (:method ((s tovia:projectile) (win sdl2-ffi:sdl-window))
    (decf (tovia:current (tovia:life s)))
    (tovia:move s win))
  (:method :around ((s tovia:being) (win sdl2-ffi:sdl-window))
    (when (tovia:apply-coeff (tovia:response? s) (tovia:coeff-of :response s))
      (call-next-method)))
  (:method ((s tovia:npc) (win sdl2-ffi:sdl-window))
    (ecase (random 10)
      (0 (attack s win :hit))
      (1 (attack s win :barrage))
      (2 (attack s win :energy))
      ((3 4 5 6 7 8 9)
       (tovia:move s win :direction (tovia:target-direction s *player*))))))

(defmethod action ((player tovia:player) (win sdl2-ffi:sdl-window))
  (let ((tracker (tovia:tracker player)))
    (tovia:keypress-case
      (:f
       (if (tovia:key-down-p tracker :f)
           (progn ; Keep on pressing.
            (incf (tovia:current (tovia:key-tracker-time tracker)))
            (tovia:move player win))
           (progn ; First time to press.
            (setf (tovia:current (tovia:key-tracker-time tracker))
                    (1- (tovia:current (tovia:key-tracker-time tracker)))
                  (tovia:coeff-of :move player)
                    (acons :charging (lambda (x) (round x 2))
                           (tovia:coeff-of :move player)))
            (cond
              ((tovia:command-input-p '(:f :f :f) tracker
                                      (tovia:discrete-time 0 0.2))
               (attack player win :barrage))
              ((tovia:command-input-p '(:f :f) tracker
                                      (tovia:discrete-time 0.3 0.4))
               (attack player win :step-in-hit))
              (t (attack player win :hit)))
            (setf (tovia:keystate tracker :f) :down))))
      (otherwise
       (cond
         ((tovia:key-down-p tracker :f)
          (setf (tovia:keystate tracker :f) :up
                (tovia:coeff-of :move player)
                  (delete :charging (tovia:coeff-of :move player) :key #'car))
          (attack player win :energy)))
       (tovia:move player win)))))

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
  (fude-gl:render-text
    (format nil "HP: ~S" (tovia:current (tovia:life player)))
    :x 0
    :y tovia:*pixel-size*
    :scale tovia:*pixel-size*
    :win win))

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
    (fude-gl:with-text (win))
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
      (status-bar *player* win)
      (when (tovia:deadp *player*)
        (signal 'tovia:sequence-transition :next #'game-over))
      (quaspar:do-lqtree (o tovia:*colliders*
                          (signal 'tovia:sequence-transition
                                  :next #'congratulations))
        (when
          ;; as enemy survives-p
          (and (not (typep o 'tovia:player)) (typep o 'tovia:being))
          (return nil))))))

(defun game-over (win)
  (fude-gl:with-text (win)
    (let ((init t))
      (sdl2:with-event-loop (:method :poll)
        (:quit ()
          t)
        (:keydown ()
          (signal 'tovia:sequence-transition :next #'start))
        (:idle ()
          (when init
            (fude-gl:render-text "Game over"
                                 :x :center
                                 :y :center
                                 :scale tovia:*pixel-size*
                                 :win win)
            (fude-gl:render-text "Push any key."
                                 :x :center
                                 :y (* 3 (tovia:boxel))
                                 :win win)
            (sdl2:gl-swap-window win)
            (setq init nil))
          (sleep 0.5))))))

(defun congratulations (win)
  (fude-gl:with-text (win)
    (let ((init t))
      (sdl2:with-event-loop (:method :poll)
        (:quit ()
          t)
        (:keydown ()
          (signal 'tovia:sequence-transition :next #'start))
        (:idle ()
          (when init
            (fude-gl:render-text "You win!"
                                 :x :center
                                 :y :center
                                 :scale tovia:*pixel-size*
                                 :win win)
            (fude-gl:render-text "Push any key."
                                 :x :center
                                 :y (* 3 (tovia:boxel))
                                 :win win)
            (sdl2:gl-swap-window win)
            (setq init nil))
          (sleep 0.5))))))

(defun start (win)
  (fude-gl:with-text (win)
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:keydown ()
        (signal 'tovia:sequence-transition :next #'test))
      (:idle ()
        (fude-gl:with-clear (win (:color-buffer-bit))
          (fude-gl:render-text "ETBA"
                               :x :center
                               :y :center
                               :scale tovia:*pixel-size*
                               :win win)
          (fude-gl:render-text "Push any key to play."
                               :x :center
                               :y (* 3 (tovia:boxel))
                               :win win))))))