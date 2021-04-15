(in-package :cl-user)

(defpackage :etba
  (:use :cl)
  (:export)
  (:import-from :tovia #:main))

(in-package :etba)

(setq tovia:*title* "Eternal Battle")

(setq tovia:*scene* 'test)

;;;; TEXTURES

(defparameter *default-image-directory*
  (merge-pathnames "resources/images/"
                   (asdf:system-source-directory (asdf:find-system :etba))))

(defun image-pathname (name)
  (truename (merge-pathnames name *default-image-directory*)))

(macrolet ((def (name pathname &optional spritep)
             `(progn
               (fude-gl:defimage ,name (image-pathname ,pathname))
               (fude-gl:deftexture ,name :texture-2d
                 (fude-gl:tex-image-2d (fude-gl:image ,name))
                 :texture-min-filter :nearest
                 :texture-mag-filter :nearest)
               ,@(when spritep
                   `((tovia:defsprite ,name tovia:4-directional
                       :unit 1/8
                       :texture (fude-gl:find-texture ,name)
                       :projection #'fude-gl:ortho))))))
  (def :earth "backgrounds/earth.png")
  (def :romius "characters/romius.png" t)
  (def :mashroom "characters/mashroom.png"))

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
          (h (float (* 2 (/ tovia:*height*)))))
      (coerce
        (vector 0.0 h 0.0 1.0 ; Top left
                0.0 0.0 0.0 0.0 ; Bottom left
                w h 1.0 1.0 ; Top right
                w 0.0 1.0 0.0) ; Bottom right
        '(array single-float (*))))
  :draw-mode :triangle-strip
  :shader 'background-shader
  :instances `((fude-gl:offset
                ,(make-map-offset tovia:*width* tovia:*height*))))

;;;; TRANSITIONS

(defun test (win)
  (uiop:nest
    (let ((player (tovia:sprite :romius win))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:with-uniforms ((tex :unit 0))
          'background-shader
        (setf tex (fude-gl:find-texture :earth))
        (fude-gl:draw 'tile))
      (fude-gl:draw player))))