(asdf:defsystem "cl-lisp2calc-tests"
  :depends-on ("cl-lisp2calc" "parachute")
  :serial t
  :components ((:module "tests"
                :around-compile (lambda (next)
                                  (proclaim '(optimize (debug 3)
                                              (safety 3)
                                              (speed 0)))
                                  (funcall next))
                :components ((:file "package-tests")
                             (:file "main-tests")
                             (:file "emacs-tests"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :lisp2calc-tests)))
