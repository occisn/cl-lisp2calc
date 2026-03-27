(asdf:defsystem "cl-lisp2calc"
  :name "cl-lisp2calc"
  :version "0.1.0"
  :author "occisn"
  :licence "MIT"
  :description "Converts Common Lisp s-expressions into GNU Emacs Calc keyboard macro strings"
  :long-description "The output can be pasted into Emacs, highlighted, run through M-x read-kbd-macro, then executed in Calc with X."
  :serial t
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                (safety 3)
                                (speed 0)))
                    (funcall next))
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "cl-lisp2calc"))))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-lisp2calc-tests))))
