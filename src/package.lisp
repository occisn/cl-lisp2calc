;;;; Package definition for the whole system.

(defpackage :lisp2calc
  (:nicknames #:l2c)
  (:use :cl)
  (:export #:convert)
  (:documentation "Converts Common Lisp s-expressions into GNU Emacs Calc
keyboard macro strings."))
