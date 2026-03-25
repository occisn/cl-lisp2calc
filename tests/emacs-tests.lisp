(in-package :lisp2calc-tests)

;;; ===========================
;;; === CONFIGURATION ===
;;; ===========================

(defvar *run-emacs-tests* nil
  "When non-NIL, run integration tests that execute Calc macros in Emacs.
Set to T before running (asdf:test-system \"cl-lisp2calc\") to enable.")

(defvar *emacs-program* "emacs"
  "Path to the Emacs executable. Override on Windows, e.g.:
  (setf lisp2calc-tests::*emacs-program*
        \"C:/portable-programs/emacs-30.2/bin/emacs.exe\")")

(defvar *calc-runner-path*
  (merge-pathnames "elisp/calc-runner.el"
                   (asdf:system-source-directory "cl-lisp2calc"))
  "Absolute path to calc-runner.el used by integration tests.")

;;; ===========================
;;; === JSON HELPERS ===
;;; ===========================

(defun %json-extract-string (json key)
  "Extract the string value for KEY from a flat JSON object string.
Returns NIL if key not found or value is null."
  (let* ((pattern (format nil "\"~a\":" key))
         (pos (search pattern json)))
    (when pos
      (let* ((after-key (+ pos (length pattern)))
             (trimmed (string-left-trim " " (subseq json after-key))))
        (cond
          ;; null value
          ((and (>= (length trimmed) 4)
                (string= "null" (subseq trimmed 0 4)))
           nil)
          ;; string value
          ((char= #\" (char trimmed 0))
           (let ((end (position #\" trimmed :start 1)))
             (when end
               (subseq trimmed 1 end))))
          ;; numeric value (unquoted)
          (t (let ((end (position-if (lambda (c) (member c '(#\, #\} #\Space)))
                                     trimmed)))
               (subseq trimmed 0 end))))))))

(defun %json-extract-integer (json key)
  "Extract an integer value for KEY from a flat JSON object string."
  (let ((val (%json-extract-string json key)))
    (when val (parse-integer val))))

;;; ===========================
;;; === EMACS RUNNER ===
;;; ===========================

(defun run-in-emacs-calc (macro-string)
  "Execute MACRO-STRING in Emacs Calc via batch mode.
Returns an alist with keys :STATUS, :RESULT, :STACK-DEPTH, :ERROR."
  (let* ((calc-runner (namestring *calc-runner-path*))
         (output (uiop:run-program
                  (list *emacs-program* "--batch"
                        "-l" calc-runner
                        "-f" "calc-runner--main"
                        macro-string)
                  :output '(:string :stripped t)
                  :error-output :interactive
                  :ignore-error-status t)))
    (list :status (%json-extract-string output "status")
          :result (%json-extract-string output "result")
          :stack-depth (%json-extract-integer output "stack_depth")
          :error (%json-extract-string output "error"))))

(defun extract-and-run (code)
  "Convert CODE to a Calc macro string, execute it in Emacs, return the result alist."
  (let ((macro-string (extract-calc-output code)))
    (unless macro-string
      (error "extract-calc-output returned NIL for ~a" code))
    (run-in-emacs-calc macro-string)))

(defun %check-emacs-result (code expected-result)
  "Convert CODE, run in Emacs Calc, verify status=ok, stack_depth=1, result=EXPECTED-RESULT."
  (if (not *run-emacs-tests*)
      (parachute:skip "Emacs integration tests disabled (*run-emacs-tests* is NIL)")
      (let ((result (extract-and-run code)))
        (parachute:is string= "ok" (getf result :status)
                      "Status should be ok for ~a (error: ~a)" code (getf result :error))
        (parachute:is = 1 (getf result :stack-depth)
                      "Stack depth should be 1 for ~a" code)
        (parachute:is string= expected-result (getf result :result)
                      "Result mismatch for ~a" code))))

;;; ==========================================
;;; === G. EMACS INTEGRATION TESTS ===
;;; ==========================================

;;; --- Atoms ---

(parachute:define-test test-emacs-positive-number
  (%check-emacs-result 5 "5"))

(parachute:define-test test-emacs-zero
  (%check-emacs-result 0 "0"))

;;; --- Arithmetic ---

(parachute:define-test test-emacs-plus
  (%check-emacs-result '(+ 3 4) "7"))

(parachute:define-test test-emacs-mult
  (%check-emacs-result '(* 2 3) "6"))

(parachute:define-test test-emacs-minus-binary
  (%check-emacs-result '(- 5 3) "2"))

(parachute:define-test test-emacs-divide-binary
  (%check-emacs-result '(/ 10 2) "5"))

(parachute:define-test test-emacs-mod
  (%check-emacs-result '(mod 10 3) "1"))

(parachute:define-test test-emacs-min
  (%check-emacs-result '(min 3 5) "3"))

(parachute:define-test test-emacs-max
  (%check-emacs-result '(max 3 5) "5"))

(parachute:define-test test-emacs-lcm
  (%check-emacs-result '(lcm 4 6) "12"))

(parachute:define-test test-emacs-multi-arg-plus
  (%check-emacs-result '(+ 1 2 3 4) "10"))

(parachute:define-test test-emacs-multi-arg-minus
  (%check-emacs-result '(- 10 1 2 3) "4"))

(parachute:define-test test-emacs-multi-arg-divide
  (%check-emacs-result '(/ 120 2 3 4) "5"))

(parachute:define-test test-emacs-nested-arithmetic-1
  (%check-emacs-result '(+ (* 2 3) 4) "10"))

(parachute:define-test test-emacs-nested-arithmetic-2
  (%check-emacs-result '(* (+ 1 2) (+ 3 4)) "21"))

;;; --- Unary operations ---

(parachute:define-test test-emacs-unary-minus
  (%check-emacs-result '(let ((x 3)) (- x)) "-3"))

(parachute:define-test test-emacs-unary-divide
  (%check-emacs-result '(let ((x 4)) (/ x)) "0.25"))

;;; --- Variable binding & assignment ---

(parachute:define-test test-emacs-let-single
  (%check-emacs-result '(let ((x 5)) x) "5"))

(parachute:define-test test-emacs-let-multiple
  (%check-emacs-result '(let ((x 3) (y 4)) (+ x y)) "7"))

(parachute:define-test test-emacs-let-star
  (%check-emacs-result '(let* ((x 3) (y (+ x 1))) y) "4"))

(parachute:define-test test-emacs-setq
  (%check-emacs-result '(let ((x 3)) (setq x 5) x) "5"))

(parachute:define-test test-emacs-incf
  (%check-emacs-result '(let ((x 3)) (incf x) x) "4"))

(parachute:define-test test-emacs-decf
  (%check-emacs-result '(let ((x 5)) (decf x) x) "4"))

;;; --- Control flow ---

(parachute:define-test test-emacs-if
  (%check-emacs-result '(let ((x 3)) (if (= x 3) 1 0)) "1"))

(parachute:define-test test-emacs-when
  (%check-emacs-result '(let ((x 3)) (when (= x 3) (+ x 1))) "4"))

(parachute:define-test test-emacs-dotimes
  (%check-emacs-result '(let ((sum 0)) (dotimes (i 5) (setq sum (+ sum i))) sum) "10"))

(parachute:define-test test-emacs-while-<=
  (%check-emacs-result
   `(let ((x 0)) (lisp2calc::while (<= x 3) (incf x)) x) "4"))

(parachute:define-test test-emacs-while-<
  (%check-emacs-result
   `(let ((x 0)) (lisp2calc::while (< x 4) (incf x)) x) "4"))

(parachute:define-test test-emacs-while->=
  (%check-emacs-result
   `(let ((x 5)) (lisp2calc::while (>= x 2) (decf x)) x) "1"))

(parachute:define-test test-emacs-while->
  (%check-emacs-result
   `(let ((x 5)) (lisp2calc::while (> x 1) (decf x)) x) "1"))

;;; end
