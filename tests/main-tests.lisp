(in-package :lisp2calc-tests)

;;; ===============
;;; === HELPERS ===
;;; ===============

(defun extract-calc-output (code)
  "Call convert on CODE, capture stdout, and return just the calc output string."
  (let* ((full-output (with-output-to-string (*standard-output*)
                        (lisp2calc:convert code)))
         (output-prefix "output = ")
         (pos (search output-prefix full-output)))
    (when pos
      (let* ((start (+ pos (length output-prefix)))
             (end (position #\Newline full-output :start start)))
        (subseq full-output start (or end (length full-output)))))))

;;; ==============================
;;; === A. UTILITY FUNCTIONS ===
;;; ==============================

(parachute:define-test test-delete-nth
  (parachute:is equal '(b c) (lisp2calc::delete-nth 0 '(a b c)))
  (parachute:is equal '(a c) (lisp2calc::delete-nth 1 '(a b c)))
  (parachute:is equal '(a b) (lisp2calc::delete-nth 2 '(a b c)))
  (parachute:is equal nil (lisp2calc::delete-nth 0 '(a))))

(parachute:define-test test-replace-nth
  (parachute:is equal '(z b c) (lisp2calc::replace-nth 0 'z '(a b c)))
  (parachute:is equal '(a z c) (lisp2calc::replace-nth 1 'z '(a b c)))
  (parachute:is equal '(a b z) (lisp2calc::replace-nth 2 'z '(a b c))))

(parachute:define-test test-add-spaces
  ;; Two consecutive numbers => SPC
  (parachute:is string= "3 SPC 4" (lisp2calc::add-spaces '(3 4)))
  ;; Three consecutive numbers => two SPCs
  (parachute:is string= "3 SPC 4 SPC 5" (lisp2calc::add-spaces '(3 4 5)))
  ;; Number then operator => no SPC
  (parachute:is string= "3 +" (lisp2calc::add-spaces '(3 "+")))
  ;; Number then DEL => SPC
  (parachute:is string= "3 SPC DEL" (lisp2calc::add-spaces '(3 "DEL")))
  ;; Number then RET => SPC
  (parachute:is string= "3 SPC RET" (lisp2calc::add-spaces '(3 "RET")))
  ;; Number then C-u => SPC
  (parachute:is string= "3 SPC C-u" (lisp2calc::add-spaces '(3 "C-u")))
  ;; "n" then number (no preceding "f") => SPC
  (parachute:is string= "n SPC 3" (lisp2calc::add-spaces '("n" 3)))
  ;; "f" "n" then number => no SPC (min operation)
  (parachute:is string= "f n 3" (lisp2calc::add-spaces '("f" "n" 3)))
  ;; Single element
  (parachute:is string= "10" (lisp2calc::add-spaces '(10)))
  ;; Operator between numbers => no extra SPC
  (parachute:is string= "3 + 4" (lisp2calc::add-spaces '(3 "+" 4))))

;;; ============================
;;; === B. ATOM PROCESSING ===
;;; ============================

(parachute:define-test test-positive-number
  (parachute:is string= "5" (extract-calc-output 5)))

(parachute:define-test test-zero
  (parachute:is string= "0" (extract-calc-output 0)))

(parachute:define-test test-unary-minus-in-let
  ;; Standalone (- 5) errors due to check-stack-length bug; test inside let
  (parachute:is string= "3 SPC RET n M-DEL"
                (extract-calc-output '(let ((x 3)) (- x)))))

(parachute:define-test test-unary-divide-in-let
  ;; Standalone (/ 4) errors due to check-stack-length bug; test inside let
  (parachute:is string= "4 SPC RET & M-DEL"
                (extract-calc-output '(let ((x 4)) (/ x)))))

;;; ========================
;;; === C. ARITHMETIC ===
;;; ========================

(parachute:define-test test-plus
  (parachute:is string= "3 SPC 4 +" (extract-calc-output '(+ 3 4))))

(parachute:define-test test-mult
  (parachute:is string= "2 SPC 3 *" (extract-calc-output '(* 2 3))))

(parachute:define-test test-minus-binary
  (parachute:is string= "5 SPC 3 -" (extract-calc-output '(- 5 3))))

(parachute:define-test test-divide-binary
  (parachute:is string= "10 SPC 2 /" (extract-calc-output '(/ 10 2))))

(parachute:define-test test-mod
  (parachute:is string= "10 SPC 3 %" (extract-calc-output '(mod 10 3))))

(parachute:define-test test-min
  (parachute:is string= "3 SPC 5 f n" (extract-calc-output '(min 3 5))))

(parachute:define-test test-max
  (parachute:is string= "3 SPC 5 f x" (extract-calc-output '(max 3 5))))

(parachute:define-test test-lcm
  (parachute:is string= "4 SPC 6 k l" (extract-calc-output '(lcm 4 6))))

(parachute:define-test test-multi-arg-plus
  (parachute:is string= "1 SPC 2 SPC 3 SPC 4 + + +"
                (extract-calc-output '(+ 1 2 3 4))))

(parachute:define-test test-multi-arg-minus
  (parachute:is string= "10 SPC 1 SPC 2 SPC 3 + + -"
                (extract-calc-output '(- 10 1 2 3))))

(parachute:define-test test-multi-arg-divide
  (parachute:is string= "120 SPC 2 SPC 3 SPC 4 * * /"
                (extract-calc-output '(/ 120 2 3 4))))

(parachute:define-test test-nested-arithmetic
  (parachute:is string= "2 SPC 3 * 4 +"
                (extract-calc-output '(+ (* 2 3) 4)))
  (parachute:is string= "1 SPC 2 + 3 SPC 4 + *"
                (extract-calc-output '(* (+ 1 2) (+ 3 4)))))

;;; ==========================================
;;; === D. VARIABLE BINDING & ASSIGNMENT ===
;;; ==========================================

(parachute:define-test test-let-single
  (parachute:is string= "5 SPC RET M-DEL"
                (extract-calc-output '(let ((x 5)) x))))

(parachute:define-test test-let-multiple
  (parachute:is string= "3 SPC 4 C-j C-j + M-DEL M-DEL"
                (extract-calc-output '(let ((x 3) (y 4)) (+ x y)))))

(parachute:define-test test-let-star
  (parachute:is string= "3 SPC RET 1 + RET M-DEL M-DEL"
                (extract-calc-output '(let* ((x 3) (y (+ x 1))) y))))

(parachute:define-test test-setq
  (parachute:is string= "3 SPC 5 M-DEL C-u 1 TAB RET M-DEL"
                (extract-calc-output '(let ((x 3)) (setq x 5) x))))

(parachute:define-test test-incf
  (parachute:is string= "3 SPC RET 1 + M-DEL C-u 1 TAB RET M-DEL"
                (extract-calc-output '(let ((x 3)) (incf x) x))))

(parachute:define-test test-decf
  (parachute:is string= "5 SPC RET 1 - M-DEL C-u 1 TAB RET M-DEL"
                (extract-calc-output '(let ((x 5)) (decf x) x))))

;;; =========================
;;; === E. CONTROL FLOW ===
;;; =========================

(parachute:define-test test-progn
  (parachute:is string= "1 SPC 2 + 3 SPC 4 +"
                (extract-calc-output '(progn (+ 1 2) (+ 3 4)))))

(parachute:define-test test-if
  (parachute:is string= "3 SPC RET 3 a= Z[ 1 Z: 0 Z] DEL"
                (extract-calc-output '(let ((x 3)) (if (= x 3) 1 0)))))

(parachute:define-test test-when
  (parachute:is string= "3 SPC RET 3 a= Z[ RET 1 + Z: Z] DEL"
                (extract-calc-output '(let ((x 3)) (when (= x 3) (+ x 1))))))

(parachute:define-test test-dotimes
  (parachute:is string= "0 SPC 0 Z{ RET 5 SPC 1 - a> Z/ C-j C-j + C-u 3 M-DEL TAB RET 1 + M-DEL C-u 1 TAB Z} DEL RET M-DEL"
                (extract-calc-output '(let ((sum 0)) (dotimes (i 5) (setq sum (+ sum i))) sum))))

(parachute:define-test test-while-<=
  (parachute:is string=
                "0 Z{ RET 3 a> Z/ RET 1 + M-DEL C-u 1 TAB Z} RET M-DEL"
                (extract-calc-output
                 `(let ((x 0)) (lisp2calc::while (<= x 3) (incf x)) x))))

(parachute:define-test test-while-<
  (parachute:is string=
                "0 Z{ RET 4 SPC 1 - a> Z/ RET 1 + M-DEL C-u 1 TAB Z} RET M-DEL"
                (extract-calc-output
                 `(let ((x 0)) (lisp2calc::while (< x 4) (incf x)) x))))

(parachute:define-test test-while->=
  (parachute:is string=
                "5 Z{ 2 C-j a> Z/ RET 1 - M-DEL C-u 1 TAB Z} RET M-DEL"
                (extract-calc-output
                 `(let ((x 5)) (lisp2calc::while (>= x 2) (decf x)) x))))

(parachute:define-test test-while->
  (parachute:is string=
                "5 Z{ 1 C-j 1 - a> Z/ RET 1 - M-DEL C-u 1 TAB Z} RET M-DEL"
                (extract-calc-output
                 `(let ((x 5)) (lisp2calc::while (> x 1) (decf x)) x))))

;;; ===========================
;;; === F. ERROR HANDLING ===
;;; ===========================

(parachute:define-test test-unknown-operator
  (parachute:fail (lisp2calc:convert '(frobnicate 3))))

(parachute:define-test test-undefined-variable
  (parachute:fail (lisp2calc:convert 'x)))

(parachute:define-test test-wrong-arity
  (parachute:fail (lisp2calc:convert '(mod 1 2 3))))

;;; end
