(asdf:defsystem "cl-lisp2calc-tests"
  :version "0.1.0"
  :author "occisn"
  :license "MIT"
  :description "Test suite for cl-lisp2calc."
  :depends-on ("cl-lisp2calc" "parachute")
  :serial t
  :components ((:module "tests"
                ;; Compilation policy for every file of the system, set in one place.
                ;;
                ;; Beware: PROCLAIM is global and permanent.  It is not undone when this
                ;; system has finished compiling, so the setting below stays in force for
                ;; the rest of the session and applies to every other system compiled
                ;; afterwards in the same image.
                :around-compile (lambda (next)
                                  (proclaim '(optimize (debug 3)
                                              (safety 3)
                                              (speed 0)))
                                  (funcall next))
                :components ((:file "package-tests")
                             (:file "main-tests")
                             (:file "emacs-tests"))))
  ;; PARACHUTE:TEST returns a result object that is true whether the suite
  ;; passed or not, so its status has to be inspected explicitly.  Without
  ;; this, (asdf:test-system "cl-lisp2calc") reports success on a failing
  ;; suite -- including on tests that error out before running, which
  ;; Parachute counts as neither passed nor failed.
  :perform (asdf:test-op (op c)
                         (declare (ignore op c))
                         (let ((result (uiop:symbol-call :parachute :test
                                                         :lisp2calc-tests)))
                           (unless (eq (uiop:symbol-call :parachute :status result)
                                       :passed)
                             (error "Test suite lisp2calc-tests failed.")))))
