;;;; Package definition for the test suite.
;;;;
;;;; The test package does not :USE LISP2CALC.  CONVERT is the only exported
;;;; symbol it needs, and the tests reach the internal helpers they exercise
;;;; through the LISP2CALC:: prefix, which keeps the origin of every symbol
;;;; visible at the point of use.

(defpackage :lisp2calc-tests
  (:use :cl)
  (:import-from :lisp2calc
                #:convert)
  (:export #:*run-emacs-tests*
           #:*run-long-emacs-tests*
           #:*run-very-long-emacs-tests*
           #:*calc-runner-path*)
  (:documentation "Test suite for cl-lisp2calc."))
