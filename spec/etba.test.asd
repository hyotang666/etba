; vim: ft=lisp et
(in-package :asdf)
(defsystem "etba.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "etba")
  :components
  ((:file "etba"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :etba args)))