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

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; TOVIA:DEFSOUND need this eval-when.
  (defparameter *default-sounds-directory*
    (merge-pathnames "resources/sounds/"
                     (asdf:system-source-directory (asdf:find-system :etba))))
  (defun sound-pathname (name)
    (truename (merge-pathnames name *default-sounds-directory*))))

(macrolet ((def (name pathname)
             `(tovia:defsound ,name (sound-pathname ,pathname))))
  (def :swing-light "swing-light.wav")
  (def :swing-barrage "swing-barrage.wav")
  (def :swing-heavy "swing-heavy.wav")
  (def :hit "hit1.wav")
  (def :projectile "projectile.wav")
  (def :dash "dash.wav")
  (def :guard "guard.wav")
  (def :backstep "backstep.wav")
  (def :shield-bash "shield-bash.wav")
  (def :spore "spore.wav"))

;;;; TEXTURES

(defparameter *default-image-directory*
  (merge-pathnames "resources/images/"
                   (asdf:system-source-directory (asdf:find-system :etba))))

(defun image-pathname (name)
  (truename (merge-pathnames name *default-image-directory*)))

(progn
 .
 #.(mapcar (lambda (name) `(defclass ,name (tovia:npc) ()))
           '(mashroom snail ameba rat mandrake snake beetle worm)))

(defclass wood-golem (tovia:npc) ((territory :reader territory)))

(defclass cat (tovia:npc) ((victim :initform nil :accessor victim)))

(defmethod initialize-instance :after ((o wood-golem) &key x y)
  (setf (slot-value o 'territory) (3d-vectors:vec2 x y)))

(defmethod quaspar:x ((o 3d-vectors:vec2)) (3d-vectors:vx o))

(defmethod quaspar:y ((o 3d-vectors:vec2)) (3d-vectors:vy o))

(defparameter *damage* nil)

(defclass damage ()
  ((x :initarg :x :reader x)
   (y :initarg :y :accessor y)
   (life :initarg :life :reader tovia:life :type tovia:parameter)
   (damage :initarg :damage :reader damage :type string)))

(defmethod initialize-instance :after ((o damage) &key (life 20))
  (setf (slot-value o 'life) (tovia:make-parameter life)))

(defclass key-tracker (tovia:key-tracker)
  ((last-shield-bash-time :initform (get-internal-real-time)
                          :accessor last-shield-bash-time
                          :type fixnum)))

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
  (def :magic-circle "backgrounds/magic-circle.png")
  (def :romius "characters/romius.png" tovia:player :key-tracker
   (make-instance 'key-tracker))
  (def :hit "effects/hit.png")
  (def :energy "effects/energy.png")
  (def :barrage "effects/barrage.png")
  (def :mashroom "characters/mashroom.png" mashroom :response 8 :reactions
   (list :spore (lambda (mashroom who)
                  (flet ((do-attack (s w)
                           (setf (tovia:last-direction s)
                                   (tovia:target-direction s who))
                           (attack s w :near-spore)))
                    (unless (tovia:action-reserved-p :attack mashroom)
                      (tovia:reserve-actions mashroom
                                             (cons :attack #'do-attack)))))))
  (def :preliminary "effects/preliminary.png")
  (def :guard "effects/guard.png")
  (def :snail "characters/snail.png" snail :response 8)
  (def :wood-golem "characters/wood-golem.png" wood-golem :response 32)
  (def :smoke "effects/smoke.png")
  (def :spore "effects/spore.png")
  (def :ameba "characters/ameba.png" ameba :response 8)
  (def :rat "characters/rat.png" rat :response 96)
  (def :mandrake "characters/mandrake.png" mandrake :response 32)
  (def :snake "characters/snake.png" snake :response 32)
  (def :beetle "characters/beetle.png" beetle :response 16)
  (def :cat "characters/cat.png" cat :response 64)
  (def :worm "characters/worm.png" worm :response 8))

(tovia:defsprite :magic-circle tovia:trigger
  :unit 1
  :texture (fude-gl:find-texture :magic-circle)
  :stepper (alexandria:circular-list '(0 0))
  :projection #'fude-gl:ortho)

(tovia:defsprite :preliminary tovia:status-effect
  :unit 1/8
  :texture (fude-gl:find-texture :preliminary)
  :stepper (alexandria:circular-list '(0 0) '(1 0) '(2 0))
  :timer 64
  :projection #'fude-gl:ortho)

(tovia:defsprite :guard tovia:guard-effect
  :unit 1/8
  :texture (fude-gl:find-texture :guard)
  :timer 64
  :projection #'fude-gl:ortho)

(defun hit-dir-coeff (phenomenon victim)
  (ecase (tovia:last-direction phenomenon)
    (:n
     #.(#0=(lambda (direction)
             `(ecase (tovia:last-direction victim)
                ,@(let* ((dirs (list :n :ne :e :se :s :sw :w :nw))
                         (position (position direction dirs)))
                    (loop :for dir :in dirs
                          :for pos = (position dir dirs)
                          :for diff
                               = (multiple-value-bind (a rem)
                                     (floor (abs (- position pos)) 4)
                                   (if (zerop a)
                                       rem
                                       (- 4 rem)))
                          :collect `(,dir
                                     ,(ecase diff
                                        (0 1)
                                        (1 3/4)
                                        (2 1/2)
                                        (3 1/4)
                                        (4 0)))))))
        :n))
    (:ne #.(#0# :ne))
    (:e #.(#0# :e))
    (:se #.(#0# :se))
    (:s #.(#0# :s))
    (:sw #.(#0# :sw))
    (:w #.(#0# :w))
    (:nw #.(#0# :nw))))

(tovia:defsprite :hit tovia:effect
  :unit 1/4
  :texture (fude-gl:find-texture :hit)
  :stepper (alexandria:circular-list '(0 3) '(1 3) '(2 3) nil)
  :timer 90
  :projection #'fude-gl:ortho
  :effects (list
             (tovia:damager 10
                            (lambda (phenomenon victim damage)
                              (let* ((guard
                                      (tovia:find-coeff :guard (tovia:coeff-of
                                                                 :status-effect victim)))
                                     (coeff (hit-dir-coeff phenomenon victim))
                                     (damage
                                      (round
                                        (cond (guard (* damage coeff))
                                              ((tovia:find-coeff :step-in (tovia:coeff-of
                                                                            :move (tovia:who
                                                                                    phenomenon)))
                                               (* (1+ coeff) damage 3/2))
                                              (t (* (1+ coeff) damage))))))
                                (push
                                 (make-instance 'damage
                                                :x (quaspar:x victim)
                                                :y (quaspar:y victim)
                                                :damage (if guard
                                                            (progn
                                                             (tovia:play
                                                               :guard)
                                                             "GUARD!")
                                                            (princ-to-string
                                                              damage)))
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
                              (let* ((guard
                                      (tovia:find-coeff :guard (tovia:coeff-of
                                                                 :status-effect victim)))
                                     (coeff (hit-dir-coeff phenomenon victim))
                                     (damage
                                      (round
                                        (if guard
                                            (* damage coeff)
                                            (* (1+ coeff)
                                               (* damage
                                                  (/
                                                    (tovia:current
                                                      (tovia:life phenomenon))
                                                    (tovia:max-of
                                                      (tovia:life
                                                        phenomenon)))))))))
                                (push
                                 (make-instance 'damage
                                                :x (quaspar:x victim)
                                                :y (quaspar:y victim)
                                                :damage (if guard
                                                            (progn
                                                             (tovia:play
                                                               :guard)
                                                             "GUARD!")
                                                            (princ-to-string
                                                              damage)))
                                 *damage*)
                                damage)))
             (tovia:knock-backer 10)))

(tovia:defsprite :smoke tovia:radiation
  :unit 1/8
  :texture (fude-gl:find-texture :smoke)
  :stepper (alexandria:circular-list '(0 0) '(1 0) '(2 0))
  :timer 90
  :projection #'fude-gl:ortho
  :effects (list (tovia:damager 5)))

(tovia:defsprite :barrage tovia:melee
  :unit 1/8
  :texture (fude-gl:find-texture :barrage)
  :stepper (alexandria:circular-list 0 1 2)
  :timer 180
  :projection #'fude-gl:ortho
  :life 40
  :effects (list (tovia:damager 5) (tovia:knock-backer 10)))

(tovia:defsprite :spore tovia:effect
  :unit 1/8
  :texture (fude-gl:find-texture :spore)
  :stepper (alexandria:circular-list '(0 0) '(1 0) '(2 0) nil)
  :projection #'fude-gl:ortho
  :timer 45
  :effects (list (tovia:damager 5)))

(defgeneric attack (subject win arm &rest args)
  (:method :around ((s tovia:npc) (win sdl2-ffi:sdl-window) arm &rest args)
    (declare (ignore args))
    (let ((preliminary
           (tovia:sprite :preliminary win
                         :x (quaspar:x s)
                         :y (quaspar:y s)
                         :life 9)))
      (setf (tovia:coeff-of :status-effect s)
              (tovia:append-coeff (tovia:coeff-of :status-effect s)
                                  :preliminary preliminary))
      (flet ((preliminary (subject win)
               (declare (ignore subject win))
               (unless (tovia:find-coeff :preliminary (tovia:coeff-of
                                                        :status-effect s))
                 (setf (tovia:reserved-actions s)
                         (delete preliminary (tovia:reserved-actions s)))))
             (attack! (subject win)
               (declare (ignore subject win))
               (call-next-method)))
        (tovia:reserve-actions s (cons :preliminary #'preliminary)
                               (cons :attack #'attack!)))))
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
  (:method :before ((s tovia:being) (win sdl2-ffi:sdl-window)
                    (arm (eql :spore)) &rest args)
    (declare (ignore args))
    (tovia:play :spore))
  (:method :before ((s tovia:being) (win sdl2-ffi:sdl-window)
                    (arm (eql :near-spore)) &rest args)
    (declare (ignore args))
    (tovia:play :spore))
  (:method ((subject tovia:being) (win sdl2-ffi:sdl-window)
            (arm (eql :step-in-hit)) &rest args)
    (apply #'call-next-method subject win :hit args))
  (:method ((subject tovia:being) (win sdl2-ffi:sdl-window) (arm (eql :spore))
            &rest args)
    (dolist (dir '(:n :s :w :e :nw :ne :sw :se))
      (tovia:add
        (apply #'tovia:sprite :smoke win :x (quaspar:x subject) :y
               (quaspar:y subject) :who subject :direction dir
               :allow-other-keys t args))))
  (:method ((subject tovia:being) (win sdl2-ffi:sdl-window)
            (arm (eql :near-spore)) &rest args)
    (let ((direction (tovia:last-direction subject)))
      (dolist (dir '(:n :s :w :e :nw :ne :sw :se))
        (setf (tovia:last-direction subject) dir)
        (multiple-value-bind (x y)
            (tovia:front subject)
          (tovia:add
            (apply #'tovia:sprite :spore win :x x :y y :who subject
                   :allow-other-keys t args)))
        (setf (tovia:last-direction subject) direction))))
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
          (tovia:do-reserved-action s win)
          (call-next-method))))
  (:method ((s tovia:melee) (win sdl2-ffi:sdl-window))
    (when (<= (decf (tovia:current (tovia:life s))) 0)
      (setf (tovia:coeff-of :response (tovia:who s))
              (tovia:delete-coeff :stun (tovia:coeff-of :response (tovia:who
                                                                    s))))))
  (:method ((s tovia:projectile) (win sdl2-ffi:sdl-window))
    (decf (tovia:current (tovia:life s)))
    (tovia:move s win))
  (:method ((s tovia:radiation) (win sdl2-ffi:sdl-window))
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
    (let ((wood
           (uiop:while-collecting (collect)
             (tovia:do-beings (b)
               (when (typep b 'wood-golem)
                 (collect (cons (tovia:distance s b) b)))))))
      (if wood
          (tovia:move s win
                      :direction (tovia:target-direction s
                                                         (tovia:nearest wood)))
          (setf (tovia:last-direction s)
                  (aref #(:s :n :w :e :nw :ne :sw :se) (random 8))))))
  (:method ((s snail) (win sdl2-ffi:sdl-window))
    (let ((targets
           (uiop:while-collecting (acc)
             (tovia:do-beings (e)
               (when (typep e '(or ameba mashroom wood-golem))
                 (acc (cons (tovia:distance s e) e)))))))
      (if (null targets)
          (tovia:walk-random s)
          (let ((nearest (tovia:nearest targets)))
            (if (not (tovia:in-sight-p s nearest (tovia:boxel)))
                (tovia:move s win
                            :direction (tovia:target-direction s nearest))
                (etypecase nearest
                  ((or mashroom wood-golem)
                   (setf (tovia:last-direction s)
                           (tovia:target-direction s nearest))
                   (attack s win :hit))
                  (ameba
                   (tovia:move s win
                               :direction (tovia:target-direction s
                                                                  nearest)))))))))
  (:method ((s ameba) (win sdl2-ffi:sdl-window))
    (let ((targets (tovia:in-sight-beings s (tovia:boxel))))
      (if targets
          (let ((nearest (tovia:nearest targets)))
            (setf (tovia:last-direction s) (tovia:target-direction s nearest))
            (attack s win :hit))
          (tovia:walk-random s))))
  (:method ((s rat) (win sdl2-ffi:sdl-window))
    (let ((beings (tovia:in-sight-beings s (tovia:boxel)))
          (dir #(:n :ne :e :se :s :sw :w :nw)))
      (cond
        (beings
         (setf (tovia:last-direction s)
                 (tovia:target-direction s (tovia:nearest beings)))
         (attack s win :hit))
        ((tovia:forwardablep s win (tovia:last-direction s))
         (tovia:move s win :direction (tovia:last-direction s)))
        ((zerop (random 16))
         (let ((index
                (+ (aref #(2 -2) (random 2))
                   (or (position (tovia:last-direction s) dir)
                       (error "Internal logical error.")))))
           (tovia:move s win
                       :direction (treat-as-circle:elt-as-circle dir
                                                                 index)))))))
  (:method ((s mandrake) (win sdl2-ffi:sdl-window))
    (let ((target (tovia:in-sight-beings s (* 4 (tovia:boxel)))))
      (if target
          (let ((nearest (tovia:nearest target)))
            (setf (tovia:last-direction s) (tovia:target-direction s nearest))
            (when (zerop (random 6))
              (attack s win :energy)))
          (tovia:walk-random s))))
  (:method ((s snake) (win sdl2-ffi:sdl-window))
    (let ((target
           (uiop:while-collecting (acc)
             (tovia:do-beings (b)
               (when (typep b '(or tovia:player rat snail))
                 (multiple-value-bind (see? distance)
                     (tovia:in-sight-p s b (* 5 (tovia:boxel)))
                   (when see?
                     (acc (cons distance b)))))))))
      (if target
          (let ((nearest (tovia:nearest target)))
            (if (tovia:in-sight-p s nearest (tovia:boxel))
                (when (zerop (random 5))
                  (attack s win :hit))
                (tovia:move s win
                            :direction (tovia:target-direction s nearest))))
          (tovia:walk-random s))))
  (:method ((s beetle) (win sdl2-ffi:sdl-window))
    (multiple-value-bind (target home)
        (uiop:while-collecting (snail wood)
          (tovia:do-beings (b)
            (multiple-value-bind (see? distance)
                (tovia:in-sight-p s b (* 3 (tovia:boxel)))
              (when see?
                (typecase b
                  (wood-golem (wood (cons distance b)))
                  (snail (snail (cons distance b))))))))
      (if target
          (let ((nearest (tovia:nearest target)))
            (if (tovia:in-sight-p s nearest (tovia:boxel))
                (when (zerop (random 8))
                  (setf (tovia:last-direction s)
                          (tovia:target-direction s nearest))
                  (attack s win :barrage))
                (tovia:move s win
                            :direction (tovia:target-direction s nearest))))
          (if home
              (tovia:move s win
                          :direction (tovia:target-direction s
                                                             (tovia:nearest
                                                               home)))
              (tovia:walk-random s)))))
  (:method ((s cat) (win sdl2-ffi:sdl-window))
    (if (victim s)
        (if (zerop (random 8))
            (progn
             (attack s win :hit)
             (when (<= (tovia:current (tovia:life (victim s))) 0)
               (setf (victim s) nil)))
            (tovia:move s win
                        :direction (tovia:target-direction s (victim s))))
        (let ((target
               (uiop:while-collecting (acc)
                 (tovia:do-beings (b)
                   (when (typep b '(or beetle snail rat snake))
                     (multiple-value-bind (see? distance)
                         (tovia:in-sight-p s b (* 3 (tovia:boxel)))
                       (when see?
                         (acc (cons distance b)))))))))
          (if target
              (let ((nearest (tovia:nearest target)))
                (setf (victim s) nearest)
                (tovia:move s win
                            :direction (tovia:target-direction s nearest)))
              (when (zerop (random 5))
                (tovia:walk-random s))))))
  (:method ((s worm) (win sdl2-ffi:sdl-window)) (tovia:walk-random s))
  (:method (s w)))

(defmacro defreact (&rest args)
  (let* ((body args)
         (key
          (when (keywordp (car body))
            (list (pop body)))))
    `(progn
      (defmethod tovia:react ,@key ,@body)
      (defmethod tovia:react ,@key ,(reverse (car body)) ,@(cdr body)))))

(defun pprint-defreact (stream exp)
  (pprint-logical-block (stream exp :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-indent :block 1 stream)
    (pprint-newline :miser stream)
    (let ((key? (pprint-pop)))
      (cond
        ((keywordp key?)
         (write key? :stream stream)
         (pprint-exit-if-list-exhausted)
         (write-char #\Space stream)
         (pprint-newline :miser stream)
         (write (pprint-pop) :stream stream)
         (pprint-exit-if-list-exhausted)
         (write-char #\Space stream)
         (pprint-newline :linear stream))
        (t
         (write key? :stream stream)
         (pprint-exit-if-list-exhausted)
         (write-char #\Space stream)
         (pprint-newline :linear stream))))
    (loop (write (pprint-pop) :stream stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline :linear stream))))

(set-pprint-dispatch '(cons (member defreact)) 'pprint-defreact)

(defreact :before ((s mashroom) (e tovia:phenomenon))
  (when (and (not (tovia:action-reserved-p :attack s))
             (not (eq s (tovia:who e))))
    (tovia:reserve-actions s
                           (cons :attack (lambda (s w) (attack s w :spore))))))

(defreact :before ((s rat) (e tovia:phenomenon))
  (when (and (not (tovia:action-reserved-p :run s)) (not (eq s (tovia:who e))))
    (tovia:reserve-actions s
                           (cons :run (lambda (s w)
                                        (tovia:move s w
                                                    :direction (tovia:last-direction
                                                                 e)))))))

(defmethod action ((s wood-golem) (win sdl2-ffi:sdl-window))
  (let* ((range 6)
         (invaders
          (uiop:while-collecting (acc)
            (quaspar:do-lqtree (e tovia:*colliders*)
              (when (and (not (eq e s)) (not (typep e 'tovia:phenomenon)))
                (multiple-value-bind (see? distance)
                    (tovia:in-sight-p (territory s) e (* range (tovia:boxel)))
                  (when see?
                    (acc (cons distance e)))))))))
    (if invaders
        (let ((invader
               (cdr
                 (reduce
                   (lambda (champ challenger)
                     (if (< (car champ) (car challenger))
                         champ
                         challenger))
                   invaders))))
          (if (tovia:in-sight-p s invader (* 1.5 (tovia:boxel)))
              (progn
               (setf (tovia:last-direction s)
                       (tovia:target-direction s invader))
               (attack s win :hit))
              (tovia:move s win
                          :direction (tovia:target-direction s invader))))
        (if (tovia:in-sight-p (territory s) s (* range (tovia:boxel)))
            (tovia:walk-random s)
            (tovia:move s win
                        :direction (tovia:target-direction s (territory s)))))))

(defgeneric key-action (key subject win))

(defmethod key-action
           ((key (eql :f)) (player tovia:player) (win sdl2-ffi:sdl-window))
  (let ((tracker (tovia:key-tracker player)))
    (if (tovia:key-down-p tracker :f)
        (progn ; Keep on pressing.
         (incf (tovia:current (tovia:key-tracker-time tracker)))
         (tovia:move player win))
        (progn ; First time to press.
         (setf (tovia:current (tovia:key-tracker-time tracker))
                 (1- (tovia:current (tovia:key-tracker-time tracker)))
               (tovia:coeff-of :move player)
                 (tovia:append-coeff (tovia:coeff-of :move player)
                                     :charging (lambda (x) (round x 2))))
         (cond
           ((tovia:command-input-p '(:f :f :f) tracker
                                   (tovia:discrete-time 0 0.2))
            (attack player win :barrage))
           ((tovia:command-input-p '(:f :f) tracker
                                   (tovia:discrete-time 0.3 0.5))
            (attack player win :step-in-hit))
           (t (attack player win :hit)))
         (setf (tovia:keystate tracker :f) :down)))))

(defun guard (player win)
  (let ((tracker (tovia:key-tracker player)))
    ;; NOTE: When coeff's life becomes 0,
    ;;       FUDE-GL:DRAW method deletes it.
    (setf (tovia:coeff-of :status-effect player)
            (tovia:append-coeff (tovia:coeff-of :status-effect player)
                                :guard (tovia:sprite :guard win
                                                     :x (quaspar:x player)
                                                     :y (quaspar:y player)
                                                     :direction (tovia:last-direction
                                                                  player))))
    (when (tovia:command-input-p '(:g :g) tracker (tovia:discrete-time 0 0.2))
      ;; Back-step
      (tovia:play :backstep)
      (let ((tovia:*coeffs*
             (acons :back-step (constantly (tovia:boxel)) tovia:*coeffs*)))
        (tovia:move player win
                    :direction (tovia:turn-direction
                                 (tovia:last-direction player))
                    :animate nil)))))

(defun guarding (player win)
  (let ((last-direction (tovia:last-direction player)))
    (let ((tovia:*coeffs* (acons :guard (constantly 0) tovia:*coeffs*)))
      (tovia:move player win))
    (if (eq last-direction (tovia:last-direction player))
        (incf ; To reset life.
              (tovia:current
                (tovia:life
                  (cdr
                    (tovia:find-coeff :guard (tovia:coeff-of :status-effect player))))))
        (progn
         (setf (tovia:current
                 (tovia:life
                   (cdr
                     (tovia:find-coeff :guard (tovia:coeff-of :status-effect player)))))
                 0)
         ;; add new shield with new direction.
         (setf (tovia:coeff-of :status-effect player)
                 (tovia:append-coeff (tovia:coeff-of :status-effect player)
                                     :guard (tovia:sprite :guard win
                                                          :x (quaspar:x player)
                                                          :y (quaspar:y player)
                                                          :direction (tovia:last-direction
                                                                       player))))))))

(defun shield-bash (player win)
  (when (<= #.(* internal-time-units-per-second 5)
            (- (get-internal-real-time)
               (last-shield-bash-time (tovia:key-tracker player))))
    (flet ((shield-bash (player victim)
             (let ((damage 10))
               (tovia:play :hit)
               (funcall (funcall (tovia:knock-backer (* 2 (tovia:boxel))) win)
                        player victim)
               (push
                (make-instance 'damage
                               :x (quaspar:x victim)
                               :y (quaspar:y victim)
                               :damage (princ-to-string damage))
                *damage*)
               (decf (tovia:current (tovia:life victim)) damage)))
           (cleanup (player win)
             (declare (ignore win))
             (tovia:rem-reaction :shield-bash player)))
      (let ((tovia:*coeffs*
             (acons :shield-bash (constantly (tovia:boxel)) tovia:*coeffs*)))
        (setf (last-shield-bash-time (tovia:key-tracker player))
                (get-internal-real-time))
        (tovia:play :shield-bash)
        (tovia:add-reaction :shield-bash #'shield-bash player)
        (tovia:reserve-actions player `(:shield-bash . ,#'cleanup))
        (tovia:move player win
                    :direction (tovia:last-direction player)
                    :animate nil)))))

(defmethod key-action
           ((key (eql :g)) (player tovia:player) (win sdl2-ffi:sdl-window))
  (let ((tracker (tovia:key-tracker player)))
    (cond
      ((not (tovia:key-down-p tracker :g)) ; N times to press g.
       (guard player win)
       (setf (tovia:keystate tracker :g) :down))
      ((not (tovia:keypressp :f)) ; Just keep on pressing g.
       (guarding player win)
       (setf (tovia:keystate tracker :f) :up))
      ((not (tovia:key-down-p tracker :f)) ; First time to press g+f.
       (shield-bash player win)
       (setf (tovia:keystate tracker :f) :down))
      (t ; keep on pressing g+f. Do nothing.
       nil))))

(defmethod action ((player tovia:player) (win sdl2-ffi:sdl-window))
  (if (tovia:find-coeff :step-in (tovia:coeff-of :move player))
      ;; Tiny stun.
      (setf (tovia:coeff-of :move player)
              (tovia:delete-coeff :step-in (tovia:coeff-of :move player)))
      (let ((tracker (tovia:key-tracker player)))
        (tovia:keypress-case
          (:g (key-action :g player win))
          (:f (key-action :f player win))
          (otherwise
           (cond
             ((tovia:key-down-p tracker :f)
              (setf (tovia:keystate tracker :f) :up
                    (tovia:coeff-of :move player)
                      (tovia:delete-coeff :charging (tovia:coeff-of :move player)))
              (attack player win :energy)))
           (when (tovia:key-down-p tracker :g)
             (setf (tovia:current
                     (tovia:life
                       (cdr
                         (tovia:find-coeff :guard (tovia:coeff-of
                                                    :status-effect player)))))
                     0
                   (tovia:keystate tracker :g) :up))
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
                                   :scale 1.5
                                   :alpha (float
                                            (/ (tovia:current (tovia:life d))
                                               (tovia:max-of (tovia:life d)))))
        :finally (setf *damage*
                         (delete-if
                           (lambda (d) (<= (tovia:current (tovia:life d)) 0))
                           *damage*))))

;;;; TRANSITIONS

(defun config-monsters (win)
  (multiple-value-bind (w h)
      (sdl2:get-window-size win)
    (let ((npcs
           #(:mashroom :snail :wood-golem :ameba :rat :mandrake :snake :beetle
             :cat :worm)))
      (dotimes (n (ceiling (tovia:pnd-random 10)))
        (tovia:add
          (tovia:sprite (aref npcs (random (length npcs))) win
                        :x (random (floor (- w (tovia:boxel))))
                        :y (random (floor (- h (tovia:boxel))))))))))

(defun draw (win)
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
                            ((or tovia:being tovia:trigger) (fude-gl:draw elt))
                            (tovia:phenomenon (push elt effects))))))
    (fude-gl:draw *player*)
    (mapc #'fude-gl:draw effects))
  (damage-pop-up)
  (status-bar *player* win))

(defun test (win)
  (uiop:nest
    (let (clear))
    (flet ((next-stage ()
             (unless clear
               (setf clear t)
               (multiple-value-bind (w h)
                   (sdl2:get-window-size win)
                 (tovia:add
                   (tovia:sprite :magic-circle win
                                 :x (random (floor (- w (tovia:boxel))))
                                 :y (random (floor (- h (tovia:boxel))))
                                 :effects (list
                                            (lambda (trigger being)
                                              (declare (ignore being))
                                              (setf clear nil
                                                    (tovia:current
                                                      (tovia:life trigger))
                                                      0)
                                              (config-monsters win)))))))))
      (setf *player* (tovia:add (tovia:sprite :romius win))
            *damage* nil)
      (config-monsters win))
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
      (draw win)
      ;;;; cleanup.
      (tovia:delete-lives)
      ;; gameover?
      (when (tovia:deadp *player*)
        (signal 'tovia:sequence-transition :next #'game-over))
      ;; clear?
      (quaspar:do-lqtree (o tovia:*colliders* (next-stage))
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