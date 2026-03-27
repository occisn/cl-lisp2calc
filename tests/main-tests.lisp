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

(parachute:define-test test-let-multiple-bindings-error
  (parachute:fail (lisp2calc:convert '(let ((x 3) (y 4)) (+ x y)))))

(parachute:define-test test-let*-single
  (parachute:is string= "5 SPC RET M-DEL"
                (extract-calc-output '(let* ((x 5)) x))))

(parachute:define-test test-let*-multiple
  (parachute:is string= "3 SPC 4 C-j C-j + M-DEL M-DEL"
                (extract-calc-output '(let* ((x 3) (y 4)) (+ x y)))))

(parachute:define-test test-let*-cross-ref
  (parachute:is string= "3 SPC RET 1 + RET M-DEL M-DEL"
                (extract-calc-output '(let* ((x 3) (y (+ x 1))) y))))

(parachute:define-test test-setq
  (parachute:is string= "3 SPC 5 SPC M-DEL RET M-DEL"
                (extract-calc-output '(let ((x 3)) (setq x 5) x))))

(parachute:define-test test-incf
  (parachute:is string= "3 SPC RET 1 + M-DEL RET M-DEL"
                (extract-calc-output '(let ((x 3)) (incf x) x))))

(parachute:define-test test-decf
  (parachute:is string= "5 SPC RET 1 - M-DEL RET M-DEL"
                (extract-calc-output '(let ((x 5)) (decf x) x))))

(parachute:define-test test-setq-multiple
  (parachute:is string= "1 SPC 2 SPC 10 SPC C-u 3 M-DEL TAB 20 SPC M-DEL C-j C-j + M-DEL M-DEL"
                (extract-calc-output '(let* ((x 1) (y 2)) (setq x 10 y 20) (+ x y)))))

;;; =========================
;;; === E. CONTROL FLOW ===
;;; =========================

(parachute:define-test test-progn
  (parachute:is string= "1 SPC 2 + 3 SPC 4 +"
                (extract-calc-output '(progn (+ 1 2) (+ 3 4)))))

(parachute:define-test test-if
  (parachute:is string= "3 SPC RET 3 a= Z[ 1 Z: 0 Z] M-DEL"
                (extract-calc-output '(let ((x 3)) (if (= x 3) 1 0)))))

(parachute:define-test test-when
  (parachute:is string= "3 SPC RET 3 a= Z[ RET 1 + Z: 0 Z] M-DEL"
                (extract-calc-output '(let ((x 3)) (when (= x 3) (+ x 1))))))

(parachute:define-test test-dotimes
  (parachute:is string= "0 SPC 0 Z{ RET 5 SPC 1 - a> Z/ C-j C-j + C-u 3 M-DEL TAB RET 1 + M-DEL Z} DEL RET M-DEL"
                (extract-calc-output '(let ((sum 0)) (dotimes (i 5) (setq sum (+ sum i))) sum))))

(parachute:define-test test-while-<=
  (parachute:is string=
                "0 Z{ RET 3 a> Z/ RET 1 + M-DEL Z} RET M-DEL"
                (extract-calc-output
                 `(let ((x 0)) (lisp2calc::while (<= x 3) (incf x)) x))))

(parachute:define-test test-while-<
  (parachute:is string=
                "0 Z{ RET 4 SPC 1 - a> Z/ RET 1 + M-DEL Z} RET M-DEL"
                (extract-calc-output
                 `(let ((x 0)) (lisp2calc::while (< x 4) (incf x)) x))))

(parachute:define-test test-while->=
  (parachute:is string=
                "5 Z{ 2 C-j a> Z/ RET 1 - M-DEL Z} RET M-DEL"
                (extract-calc-output
                 `(let ((x 5)) (lisp2calc::while (>= x 2) (decf x)) x))))

(parachute:define-test test-while->
  (parachute:is string=
                "5 Z{ 1 C-j 1 - a> Z/ RET 1 - M-DEL Z} RET M-DEL"
                (extract-calc-output
                 `(let ((x 5)) (lisp2calc::while (> x 1) (decf x)) x))))

(parachute:define-test test-while-/=
  ;; (while (/= x target) body): loop while x ≠ target
  ;; x=1, target=5; increment x until x=5
  (parachute:is string=
                "1 SPC 5 Z{ C-j C-j a= Z/ C-j 1 + C-u 3 M-DEL TAB Z} C-j M-DEL M-DEL"
                (extract-calc-output
                 `(let* ((x 1) (target 5))
                    (lisp2calc::while (/= x target)
                      (incf x))
                    x))))

(parachute:define-test test-while-and
  ;; (while (and (= ...) (= ...)) body): loop while both conditions true
  ;; x=3,y=3; loop while x=3 AND y=3; body sets y=0 → exits after 1 iteration
  (parachute:is string=
                "3 SPC 3 Z{ C-j 3 a= Z[ RET 3 a= Z: 0 Z] 0 a= Z/ 0 SPC M-DEL Z} RET M-DEL M-DEL"
                (extract-calc-output
                 `(let* ((x 3) (y 3))
                    (lisp2calc::while (and (= x 3) (= y 3))
                      (setq y 0))
                    y))))

(parachute:define-test test-or
  ;; (or (= 1 1) (= 2 3)) → first true, short-circuits
  (parachute:is string= "1 SPC 1 a= Z[ 1 Z: 2 SPC 3 a= Z]"
                (extract-calc-output '(or (= 1 1) (= 2 3))))
  ;; (or (= 1 2) (= 3 3)) → first false, evaluates second
  (parachute:is string= "1 SPC 2 a= Z[ 1 Z: 3 SPC 3 a= Z]"
                (extract-calc-output '(or (= 1 2) (= 3 3))))
  ;; with variables
  (parachute:is string= "3 SPC RET 3 a= Z[ 1 Z: RET 5 a= Z] M-DEL"
                (extract-calc-output '(let ((x 3)) (or (= x 3) (= x 5)))))
  ;; "multiple of 3 or 5" with n=9 (mult of 3)
  (parachute:is string=
                "9 SPC 0 C-j 3 % a= Z[ 1 Z: 0 C-j 5 % a= Z] M-DEL"
                (extract-calc-output
                 '(let ((n 9)) (or (= 0 (mod n 3)) (= 0 (mod n 5))))))
  ;; "multiple of 3 or 5" inside when, n=9
  (parachute:is string=
                "9 SPC 0 C-j 3 % a= Z[ 1 Z: 0 C-j 5 % a= Z] Z[ 1 Z: 0 Z] M-DEL"
                (extract-calc-output
                 '(let ((n 9))
                    (when (or (= 0 (mod n 3)) (= 0 (mod n 5))) 1)))))

(parachute:define-test test-and
  ;; (and (= 1 1) (= 2 2)) → both true → 1
  (parachute:is string= "1 SPC 1 a= Z[ 2 SPC 2 a= Z: 0 Z]"
                (extract-calc-output '(and (= 1 1) (= 2 2))))
  ;; (and (= 1 2) (= 3 3)) → first false, short-circuits → 0
  (parachute:is string= "1 SPC 2 a= Z[ 3 SPC 3 a= Z: 0 Z]"
                (extract-calc-output '(and (= 1 2) (= 3 3))))
  ;; (and (= 1 1) (= 2 3)) → first true, second false → 0
  (parachute:is string= "1 SPC 1 a= Z[ 2 SPC 3 a= Z: 0 Z]"
                (extract-calc-output '(and (= 1 1) (= 2 3))))
  ;; with variables
  (parachute:is string= "3 SPC RET 3 a= Z[ RET 5 a= Z: 0 Z] M-DEL"
                (extract-calc-output '(let ((x 3)) (and (= x 3) (= x 5)))))
  ;; "multiple of 2 and 3" with n=6
  (parachute:is string=
                "6 SPC 0 C-j 2 % a= Z[ 0 C-j 3 % a= Z: 0 Z] M-DEL"
                (extract-calc-output
                 '(let ((n 6)) (and (= 0 (mod n 2)) (= 0 (mod n 3))))))
  ;; "multiple of 2 and 3" inside when, n=6
  (parachute:is string=
                "6 SPC 0 C-j 2 % a= Z[ 0 C-j 3 % a= Z: 0 Z] Z[ 1 Z: 0 Z] M-DEL"
                (extract-calc-output
                 '(let ((n 6))
                    (when (and (= 0 (mod n 2)) (= 0 (mod n 3))) 1)))))

;;; ===========================
;;; === F. ERROR HANDLING ===
;;; ===========================

(parachute:define-test test-unknown-operator
  (parachute:fail (lisp2calc:convert '(frobnicate 3))))

(parachute:define-test test-undefined-variable
  (parachute:fail (lisp2calc:convert 'x)))

(parachute:define-test test-wrong-arity
  (parachute:fail (lisp2calc:convert '(mod 1 2 3))))

;;; ====================================
;;; === G. ADDITIONAL COMMANDS ===
;;; ====================================

(parachute:define-test test-prime-factorization
  (parachute:is equal '(2) (lisp2calc::prime-factorization 2))
  (parachute:is equal '(3) (lisp2calc::prime-factorization 3))
  (parachute:is equal '(2 2) (lisp2calc::prime-factorization 4))
  (parachute:is equal '(2 3) (lisp2calc::prime-factorization 6))
  (parachute:is equal '(2 2 2) (lisp2calc::prime-factorization 8))
  (parachute:is equal '(2 2 3) (lisp2calc::prime-factorization 12))
  (parachute:is equal '(7) (lisp2calc::prime-factorization 7))
  (parachute:is equal '(2 3 5 7) (lisp2calc::prime-factorization 210))
  (parachute:is equal '(2 2 2 2 2 2 2 2 2 2) (lisp2calc::prime-factorization 1024))
  (parachute:is equal '(101) (lisp2calc::prime-factorization 101)))

(parachute:define-test test-convert-prime-factorization
  ;; literal number
  (parachute:is string= "12 k f"
                (extract-calc-output `(lisp2calc::prime-factorization 12)))
  ;; with variable
  (parachute:is string= "210 SPC RET k f M-DEL"
                (extract-calc-output `(let ((n 210)) (lisp2calc::prime-factorization n)))))

(parachute:define-test test-convert-last-element
  ;; literal number
  (parachute:is string= "12 v v v r 1"
                (extract-calc-output `(lisp2calc::last-element 12)))
  ;; composed with prime-factorization
  (parachute:is string= "12 k f v v v r 1"
                (extract-calc-output `(lisp2calc::last-element (lisp2calc::prime-factorization 12)))))

;;; end
