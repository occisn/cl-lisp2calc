(in-package :lisp2calc-tests)

;;; ===========================
;;; === CONFIGURATION ===
;;; ===========================

(defvar *run-emacs-tests* t
  "When non-NIL, run integration tests that execute Calc macros in Emacs.
Set to T before running (asdf:test-system \"cl-lisp2calc\") to enable.")

(defvar *emacs-program*
  (or (probe-file "/usr/bin/emacs")
      (probe-file "/mnt/c/portable-programs/emacs-30.2/bin/emacs.exe")
      "C:/portable-programs/emacs-30.2/bin/emacs.exe")
  "Path to the Emacs executable. Tries Linux, WSL, then Windows paths.")

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

(defun %result-to-string (value)
  "Convert a CL value to a string comparable with Emacs Calc output.
Ratios are converted to floats (e.g. 1/4 → \"0.25\")."
  (etypecase value
    (ratio (format nil "~F" (float value)))
    (number (write-to-string value))
    (string value)))

(defun %check-emacs-result (code)
  "Eval CODE in CL, convert CODE to a Calc macro and run in Emacs, compare results."
  (if (not *run-emacs-tests*)
      (parachute:skip "Emacs integration tests disabled (*run-emacs-tests* is NIL)")
      (let* ((expected (%result-to-string (eval code)))
             (result (extract-and-run code)))
        (parachute:is string= "ok" (getf result :status)
                      "Status should be ok for ~a (error: ~a)" code (getf result :error))
        (parachute:is = 1 (getf result :stack-depth)
                      "Stack depth should be 1 for ~a" code)
        (parachute:is string= expected (getf result :result)
                      "Result mismatch for ~a: CL=~a, Calc=~a"
                      code expected (getf result :result)))))

;;; ==========================================
;;; === G. EMACS INTEGRATION TESTS ===
;;; ==========================================

;;; --- Atoms ---

(parachute:define-test test-emacs-positive-number
  (%check-emacs-result 5))

(parachute:define-test test-emacs-zero
  (%check-emacs-result 0))

;;; --- Arithmetic ---

(parachute:define-test test-emacs-plus
  (%check-emacs-result '(+ 3 4)))

(parachute:define-test test-emacs-mult
  (%check-emacs-result '(* 2 3)))

(parachute:define-test test-emacs-minus-binary
  (%check-emacs-result '(- 5 3)))

(parachute:define-test test-emacs-divide-binary
  (%check-emacs-result '(/ 10 2)))

(parachute:define-test test-emacs-mod
  (%check-emacs-result '(mod 10 3)))

(parachute:define-test test-emacs-min
  (%check-emacs-result '(min 3 5)))

(parachute:define-test test-emacs-max
  (%check-emacs-result '(max 3 5)))

(parachute:define-test test-emacs-lcm
  (%check-emacs-result '(lcm 4 6)))

(parachute:define-test test-emacs-multi-arg-plus
  (%check-emacs-result '(+ 1 2 3 4)))

(parachute:define-test test-emacs-multi-arg-minus
  (%check-emacs-result '(- 10 1 2 3)))

(parachute:define-test test-emacs-multi-arg-divide
  (%check-emacs-result '(/ 120 2 3 4)))

(parachute:define-test test-emacs-nested-arithmetic-1
  (%check-emacs-result '(+ (* 2 3) 4)))

(parachute:define-test test-emacs-nested-arithmetic-2
  (%check-emacs-result '(* (+ 1 2) (+ 3 4))))

;;; --- Unary operations ---

(parachute:define-test test-emacs-unary-minus
  (%check-emacs-result '(let ((x 3)) (- x))))

(parachute:define-test test-emacs-unary-divide
  (%check-emacs-result '(let ((x 4)) (/ x))))

;;; --- Variable binding & assignment ---

(parachute:define-test test-emacs-let-single
  (%check-emacs-result '(let ((x 5)) x)))

(parachute:define-test test-emacs-let-multiple
  (%check-emacs-result '(let ((x 3) (y 4)) (+ x y))))

(parachute:define-test test-emacs-let-star
  (%check-emacs-result '(let* ((x 3) (y (+ x 1))) y)))

(parachute:define-test test-emacs-setq
  (%check-emacs-result '(let ((x 3)) (setq x 5) x)))

(parachute:define-test test-emacs-setq-multiple
  (%check-emacs-result
   '(let ((x 1) (y 2)) (setq x 10 y 20) (+ x y))))

(parachute:define-test test-emacs-incf
  (%check-emacs-result '(let ((x 3)) (incf x) x)))

(parachute:define-test test-emacs-decf
  (%check-emacs-result '(let ((x 5)) (decf x) x)))

;;; --- Control flow ---

(parachute:define-test test-emacs-if
  (%check-emacs-result '(let ((x 3)) (if (= x 3) 1 0))))

(parachute:define-test test-emacs-when
  (%check-emacs-result '(let ((x 3)) (when (= x 3) (+ x 1)))))

(parachute:define-test test-emacs-dotimes
  (%check-emacs-result '(let ((sum 0)) (dotimes (i 5) (setq sum (+ sum i))) sum)))

(parachute:define-test test-emacs-while-<=
  (%check-emacs-result
   `(let ((x 0)) (l2c::while (<= x 3) (incf x)) x)))

(parachute:define-test test-emacs-while-<
  (%check-emacs-result
   `(let ((x 0)) (l2c::while (< x 4) (incf x)) x)))

(parachute:define-test test-emacs-while->=
  (%check-emacs-result
   `(let ((x 5)) (l2c::while (>= x 2) (decf x)) x)))

(parachute:define-test test-emacs-while->
  (%check-emacs-result
   `(let ((x 5)) (l2c::while (> x 1) (decf x)) x)))

;;; --- Applications ---

(defun %check-emacs-result-against (code expected-string)
  "Like %check-emacs-result, but also verify that both CL and Calc produce EXPECTED-STRING."
  (if (not *run-emacs-tests*)
      (parachute:skip "Emacs integration tests disabled (*run-emacs-tests* is NIL)")
      (let* ((cl-result (%result-to-string (eval code)))
             (result (extract-and-run code)))
        (parachute:is string= expected-string cl-result
                      "CL result mismatch for ~a: expected ~a, got ~a"
                      code expected-string cl-result)
        (parachute:is string= "ok" (getf result :status)
                      "Status should be ok for ~a (error: ~a)" code (getf result :error))
        (parachute:is = 1 (getf result :stack-depth)
                      "Stack depth should be 1 for ~a" code)
        (parachute:is string= expected-string (getf result :result)
                      "Calc result mismatch for ~a: expected ~a, got ~a"
                      code expected-string (getf result :result)))))

(parachute:define-test test-emacs-euler-1
  (%check-emacs-result-against
   '(let ((n 1000)
          (sum 0))
      (dotimes (i n)
        (when (or (= 0 (mod i 3)) (= 0 (mod i 5)))
          (incf sum i)))
      sum)
   "233168"))

(parachute:define-test test-emacs-euler-2
  (%check-emacs-result-against
   `(let ((n 4000000)
          (f1 0)
          (f2 1)
          (tmp 0)
          (sum 0))
      (l2c::while (<= f2 n)
        (when (= 0 (mod f2 2)) (incf sum f2))
        (setq tmp f1
              f1 f2
              f2 (+ tmp f2)))
      sum)
   "4613732"))

(parachute:define-test test-emacs-euler-3
  (%check-emacs-result-against
   `(lisp2calc::last-element (lisp2calc::prime-factorization 600851475143))
   "6857"))

;;; end
