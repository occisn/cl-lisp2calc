(asdf:defsystem "cl-lisp2calc"
  :version "0.1.0"
  :author "occisn"
  :maintainer "occisn"
  :license "MIT"
  :homepage "https://github.com/occisn/cl-lisp2calc"
  :description "Converts Common Lisp s-expressions into GNU Emacs Calc keyboard macro strings"
  :long-description "The output can be pasted into Emacs, highlighted, run through M-x read-kbd-macro, then executed in Calc with X."
  :serial t
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
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "cl-lisp2calc"))))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-lisp2calc-tests))))
