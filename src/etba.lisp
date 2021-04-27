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

;;;; SOUNDS

(defparameter *default-sounds-directory*
  (merge-pathnames "resources/sounds/"
                   (asdf:system-source-directory (asdf:find-system :etba))))

(defun sound-pathname (name)
  (truename (merge-pathnames name *default-sounds-directory*)))

(macrolet ((def (name pathname)
             `(tovia:defsound ,name (sound-pathname ,pathname))))
  (def :swing-light "swing-light.wav")
  (def :swing-barrage "swing-barrage.wav")
  (def :swing-heavy "swing-heavy.wav")
  (def :hit "hit1.wav")
  (def :projectile "projectile.wav")
  (def :dash "dash.wav"))

;;;; TEXTURES

(defparameter *default-image-directory*
  (merge-pathnames "resources/images/"
                   (asdf:system-source-directory (asdf:find-system :etba))))

(defun image-pathname (name)
  (truename (merge-pathnames name *default-image-directory*)))

(defclass mashroom (tovia:npc) ())

(defparameter *damage* nil)

(defclass damage ()
  ((x :initarg :x :reader x)
   (y :initarg :y :accessor y)
   (life :initarg :life :reader tovia:life :type tovia:parameter)
   (damage :initarg :damage :reader damage :type string)))

(defmethod initialize-instance :after ((o damage) &key (life 20))
  (setf (slot-value o 'life) (tovia:make-parameter life)))

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
  (def :mashroom "characters/mashroom.png" mashroom :response 8)
  (def :preliminary "effects/preliminary.png"))

(tovia:defsprite :preliminary tovia:status-effect
  :unit 1/8
  :texture (fude-gl:find-texture :preliminary)
  :stepper (alexandria:circular-list '(0 0) '(1 0) '(2 0))
  :timer 64
  :projection #'fude-gl:ortho)

(tovia:defsprite :hit tovia:effect
  :unit 1/4
  :texture (fude-gl:find-texture :hit)
  :stepper (alexandria:circular-list '(0 3) '(1 3) '(2 3) nil)
  :timer 90
  :projection #'fude-gl:ortho
  :effects (list
             (tovia:damager 10
                            (lambda (phenomenon victim damage)
                              (let ((damage
                                     (if (tovia:find-coeff :step-in (tovia:coeff-of
                                                                      :move (tovia:who
                                                                              phenomenon)))
                                         (round damage 2/3)
                                         damage)))
                                (push
                                 (make-instance 'damage
                                                :x (quaspar:x victim)
                                                :y (quaspar:y victim)
                                                :damage (princ-to-string
                                                          damage))
                                 *damage*)
                                damage)))
             (tovia:knock-backer (tovia:boxel))))

(tovia:defsprite :energy tovia:projectile
  :unit 1/8
  :texture (fude-gl:find-texture :energy)
  :timer 90
  :projection #'fude-gl:ortho
  :effects (list
             (tovia:damager 10
                            (lambda (phenomenon victim damage)
                              (let ((damage
                                     (round
                                       (* damage
                                          (/
                                            (tovia:current
                                              (tovia:life phenomenon))
                                            (tovia:max-of
                                              (tovia:life phenomenon)))))))
                                (push
                                 (make-instance 'damage
                                                :x (quaspar:x victim)
                                                :y (quaspar:y victim)
                                                :damage (princ-to-string
                                                          damage))
                                 *damage*)
                                damage)))
             (tovia:knock-backer 10)))

(tovia:defsprite :barrage tovia:melee
  :unit 1/8
  :texture (fude-gl:find-texture :barrage)
  :stepper (alexandria:circular-list 0 1 2)
  :timer 180
  :projection #'fude-gl:ortho
  :life 40
  :effects (list (tovia:damager 5) (tovia:knock-backer 10)))

(defgeneric attack (subject win arm &rest args)
  (:method :around ((subject tovia:player) (win sdl2-ffi:sdl-window)
                    (arm (eql :energy)) &rest args)
    (declare (ignore args))
    (let* ((tracker (tovia:key-tracker subject))
           (time (tovia:current (tovia:key-tracker-time tracker))))
      (setf (tovia:current (tovia:key-tracker-time tracker)) 0)
      (when (< 10 time)
        (call-next-method subject win :energy :life time))))
  (:method :before ((subject tovia:being) (win sdl2-ffi:sdl-window)
                    (arm (eql :barrage)) &rest args)
    (declare (ignore args))
    (tovia:play :swing-barrage)
    (setf (tovia:coeff-of :response subject)
            (tovia:append-coeff (tovia:coeff-of :response subject)
                                :stun (constantly nil))))
  (:method :before ((subject tovia:being) (win sdl2-ffi:sdl-window)
                    (arm (eql :step-in-hit)) &rest args)
    (declare (ignore args))
    (tovia:play :swing-heavy)
    (setf (tovia:coeff-of :move subject)
            (tovia:append-coeff (tovia:coeff-of :move subject)
                                :step-in (constantly (/ (tovia:boxel) 2))))
    (tovia:move subject win :direction (tovia:last-direction subject)))
  (:method :before ((subject tovia:being) (win sdl2-ffi:sdl-window)
                    (arm (eql :energy)) &rest args)
    (declare (ignore args))
    (tovia:play :projectile))
  (:method :before ((subject tovia:being) (win sdl2-ffi:sdl-window)
                    (arm (eql :hit)) &rest args)
    (declare (ignore args))
    (tovia:play :swing-light))
  (:method ((subject tovia:being) (win sdl2-ffi:sdl-window)
            (arm (eql :step-in-hit)) &rest args)
    (apply #'call-next-method subject win :hit args))
  (:method ((subject tovia:being) (win sdl2-ffi:sdl-window) arm &rest args)
    (tovia:add
      (multiple-value-bind (x y)
          (tovia:front subject)
        (apply #'tovia:sprite arm win :x x :y y :who subject :direction
               (tovia:last-direction subject) :allow-other-keys t args)))))

(defgeneric action (subject window)
  (:method :around ((s tovia:being) (win sdl2-ffi:sdl-window))
    (when (tovia:apply-coeff (tovia:response? s) (tovia:coeff-of :response s))
      (if (tovia:reserved-actions s)
          (funcall (pop (tovia:reserved-actions s)) s win)
          (call-next-method))))
  (:method ((s tovia:melee) (win sdl2-ffi:sdl-window))
    (when (<= (decf (tovia:current (tovia:life s))) 0)
      (setf (tovia:coeff-of :response (tovia:who s))
              (delete :stun (tovia:coeff-of :response (tovia:who s))
                      :key #'car))))
  (:method ((s tovia:projectile) (win sdl2-ffi:sdl-window))
    (decf (tovia:current (tovia:life s)))
    (tovia:move s win))
  (:method ((s tovia:npc) (win sdl2-ffi:sdl-window))
    (ecase (random 10)
      (0 (attack s win :hit))
      (1 (attack s win :barrage))
      (2 (attack s win :energy))
      ((3 4 5 6 7 8 9)
       (tovia:move s win :direction (tovia:target-direction s *player*)))))
  (:method ((s mashroom) (win sdl2-ffi:sdl-window))
    (multiple-value-bind (see? distance)
        (tovia:in-sight-p s *player* (* (tovia:boxel) 4))
      (if (not see?)
          (setf (tovia:last-direction s)
                  (aref #(:s :n :w :e :nw :ne :sw :se) (random 8)))
          (progn
           (setf (tovia:last-direction s) (tovia:target-direction s *player*))
           (when (zerop (random 5))
             (let ((preliminary
                    (tovia:sprite :preliminary win
                                  :x (quaspar:x s)
                                  :y (quaspar:y s)
                                  :life 9)))
               (push preliminary (tovia:coeff-of :status-effect s))
               (uiop:appendf (tovia:reserved-actions s)
                             (list
                               (lambda (mash win)
                                 (declare (ignore mash win))
                                 (unless (find preliminary
                                               (tovia:coeff-of :status-effect s))
                                   (setf (tovia:reserved-actions s)
                                           (delete preliminary
                                                   (tovia:reserved-actions
                                                     s)))))
                               (lambda (mash win)
                                 (attack mash win
                                         (if (<= distance (* 2 (tovia:boxel)))
                                             :hit
                                             :energy)))))))))))
  (:method (s w)))

(defmethod action ((player tovia:player) (win sdl2-ffi:sdl-window))
  (if (tovia:find-coeff :step-in (tovia:coeff-of :move player))
      ;; Tiny stun.
      (setf (tovia:coeff-of :move player)
              (delete :step-in (tovia:coeff-of :move player) :key #'car))
      (let ((tracker (tovia:key-tracker player)))
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
                        (tovia:append-coeff (tovia:coeff-of :move player)
                                            :charging (lambda (x)
                                                        (round x 2))))
                (cond
                  ((tovia:command-input-p '(:f :f :f) tracker
                                          (tovia:discrete-time 0 0.2))
                   (attack player win :barrage))
                  ((tovia:command-input-p '(:f :f) tracker
                                          (tovia:discrete-time 0.3 0.5))
                   (attack player win :step-in-hit))
                  (t (attack player win :hit)))
                (setf (tovia:keystate tracker :f) :down))))
          (otherwise
           (cond
             ((tovia:key-down-p tracker :f)
              (setf (tovia:keystate tracker :f) :up
                    (tovia:coeff-of :move player)
                      (delete :charging (tovia:coeff-of :move player)
                              :key #'car))
              (attack player win :energy)))
           (when (tovia:key-down-p tracker :g)
             (setf (tovia:current
                     (tovia:life
                       (find guard (tovia:coeff-of :status-efffect player))))
                     0))
           (tovia:move player win))))))

(defmethod tovia:move :around ((o tovia:player) (win sdl2-ffi:sdl-window) &key)
  (let ((dash? (tovia:find-coeff :dush (tovia:coeff-of :move o))))
    (call-next-method)
    (when (and (null dash?) (tovia:find-coeff :dush (tovia:coeff-of :move o)))
      (tovia:play :dash))))

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

(let ((string
       (make-array 10 ; HP must 6 length.
                   :element-type 'character
                   :initial-element #\Nul
                   :fill-pointer t)))
  (defun status-bar (player win)
    (multiple-value-call #'gl:viewport 0 0 (sdl2:get-window-size win))
    (setf (fill-pointer string) 0)
    (format string (formatter "HP: ~S") (tovia:current (tovia:life player)))
    (fude-gl:render-text string
                         :x 0
                         :y tovia:*pixel-size*
                         :scale tovia:*pixel-size*)))

(defun collision ()
  (quaspar:traverse tovia:*colliders*
                    (lambda (list)
                      (quaspar:do-unique-pair ((a b) list)
                        (when (tovia:collidep a b)
                          (tovia:react a b))))))

(defmethod tovia:react :before ((s tovia:phenomenon) (o tovia:being))
  (unless (tovia:victimp o s)
    (tovia:play :hit)))

(defun damage-pop-up ()
  (loop :for d :in *damage*
        :when (< 0 (decf (tovia:current (tovia:life d))))
          :do (fude-gl:render-text (damage d)
                                   :x (x d)
                                   :y (incf (y d) tovia:*pixel-size*)
                                   :scale 2
                                   :alpha (float
                                            (/ (tovia:current (tovia:life d))
                                               (tovia:max-of (tovia:life d)))))
        :finally (setf *damage*
                         (delete-if
                           (lambda (d) (<= (tovia:current (tovia:life d)) 0))
                           *damage*))))

;;;; TRANSITIONS

(defun test (win)
  (uiop:nest
    (progn
     (setf *player* (tovia:add (tovia:sprite :romius win))
           *damage* nil)
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
      ;; action
      (quaspar:do-lqtree (o tovia:*colliders*)
        (action o win))
      ;; collision
      (collision)
      ;;;; draw
      ;; background
      (fude-gl:with-uniforms ((tex :unit 0))
          'background-shader
        (setf tex (fude-gl:find-texture :earth))
        (fude-gl:draw 'tile))
      ;; beings then effects.
      (let (effects)
        (quaspar:traverse tovia:*colliders*
                          (lambda (list)
                            (dolist (elt list)
                              (etypecase elt
                                (tovia:player) ; do-nothing.
                                (tovia:being (fude-gl:draw elt))
                                (tovia:phenomenon (push elt effects))))))
        (fude-gl:draw *player*)
        (mapc #'fude-gl:draw effects))
      (damage-pop-up)
      (status-bar *player* win)
      ;;;; cleanup.
      (tovia:delete-lives)
      ;; gameover?
      (when (tovia:deadp *player*)
        (signal 'tovia:sequence-transition :next #'game-over))
      ;; clear?
      (quaspar:do-lqtree (o tovia:*colliders*
                          (signal 'tovia:sequence-transition
                                  :next #'congratulations))
        (when
          ;; as enemy survives-p
          (and (not (typep o 'tovia:player)) (typep o 'tovia:being))
          (return nil))))))

(defun game-over (win)
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
        (sleep 0.5)))))

(defun congratulations (win)
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
        (sleep 0.5)))))

(defun start (win)
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
                             :win win)))))