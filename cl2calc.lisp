(defpackage cl2calc
  (:use :cl ))

(in-package :cl2calc)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defparameter +nil+ nil)


;;; ====================
;;; === BASIC MACROS ===
;;; ====================

(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))

;;; =================================
;;; === BASIC OPERATIONS ON LISTS ===
;;; =================================

(defun delete-nth (n lst)
  "Return a list which is the original LST without its N-th element. Not destructive.
(v1 as of 2025-05-17, from cl-utils repository of 'occisn' GitHub)"
  (loop for elt in lst
        for i from 0
        unless (= i n) collect elt))

(defun replace-nth (n new-value lst)
  "Return a list which is the original LST where N-th element is replaced by NEW-VALUE. Not destructive.
(v1 as of 2025-05-18, from cl-utils repository of 'occisn' GitHub)"
  (loop for elt in lst
        for i from 0
        when (= i n) collect new-value
        unless (= i n) collect elt))

;;; ===========
;;; === XXX ===
;;; ===========

(defmacro pop-and-check-from-stack (stack operation-name)
  "Pop the first element of STACK and checks that it is not associated with a variable, otherwise throw an error.
OPERATION-NAME contains the name of the operation which calls this function (to be displayed in the error message)."
  `(let ((newest-elt (pop ,stack)))
    (when (not (equal newest-elt 'NIL)) 
      (error "Operation '~a' pops the newest element out of the stack, but it is associated with a variable: ~a" ,operation-name newest-elt))))

(defun check-stack-length (stack minimal-length operation-name)
  "Check that the length of STACK is >= MINIMAL-LENGTH, otherwise throw an error.
OPERATION-NAME contains the name of the operation which calls this function (to be displayed in the error message)."
  (when (< (length stack) 2)
    (error "Not enough elements (should be >= ~a) in the stack to apply '~a'; stack = ~a" minimal-length operation-name stack))
  )

;;; ============================================
;;; === BASIC OPERATIONS ON OUTPUT-AND-STACK ===
;;; ============================================

(defun add-to-output (output-and-stack elt)
  "Add ELT to the output part of OUTPUT-AND-STACK, and return the new output-and-stack."
  (let ((output (car output-and-stack))
        (stack (cdr output-and-stack)))
    (cons (cons elt output) stack)))

(defun append-to-output (output-and-stack lst)
  "Add ELT to the output part of OUTPUT-AND-STACK, and return the new output-and-stack."
  (let ((output (car output-and-stack))
        (stack (cdr output-and-stack)))
    (cons (append lst output) stack)))

(defun delete-newest-element-on-stack (output-and-stack)
  "Delete newest element on stack of OUTPUT-AND-STACK (but keep output unchanged), and return the new output-and-stack."
  (let ((output (car output-and-stack))
        (stack (cdr output-and-stack)))
    (cons output (cdr stack))))

;;; =============================
;;; === OPERATIONS PROCESSING ===
;;; =============================

(defun process-let (output-and-stack terms)
  "Convert a 'let' with terms TERMS (bindings and body) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."

  (unless (>= (length terms) 2)
    (error "Not enough terms for a 'let' within ~a" terms))
  (let ((bindings (car terms))
        (body (cdr terms)))

    ;; (1) Process bindings:
    (dolist (binding bindings)
      (let* ((symbol0 (car binding))
             (value-exp (cadr binding))
             (output-and-stack2 (process-atom-or-sexp output-and-stack value-exp))
             (output2 (car output-and-stack2))
             (stack2 (cdr output-and-stack2))
             (stack3 (cons symbol0 (cdr stack2))))
        (setq output-and-stack (cons output2 stack3)))) ; dolist

    ;; (2) Process body:
    (setq output-and-stack
          (process-sexp output-and-stack (cons 'progn body)))

    ;; (3) Delete bindings from stack:
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))
      (dolist (binding (reverse bindings))
        (let* ((symbol0 (car binding))
               (place-of-symbol-in-stack
                 (position symbol0 stack)))
          (when (null place-of-symbol-in-stack)
            (error "(let) Variable ~a supposed to be deleted not found in stack" symbol0))
          ;; Add relevant output, for instance C-u 5 M-DEL:
          (setq output (append
                        (cond ((= 0 place-of-symbol-in-stack)
                               (list "DEL"))
                              ((= 1 place-of-symbol-in-stack)
                               (list "M-DEL"))
                              (t (reverse (list "C-u" (+ 1 place-of-symbol-in-stack) "M-DEL"))))
                        output))
          (setq stack (delete-nth place-of-symbol-in-stack stack))))
      (cons output stack))))

(defun process-setq (output-and-stack symbol value-exp)
  "Convert a 'setq' with terms SYMBOL and VALUE-EXP taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let* ((output-and-stack2 (process-atom-or-sexp output-and-stack value-exp))
         (output2 (car output-and-stack2))
         (stack2 (cdr output-and-stack2))
         (place-of-symbol-in-stack (position symbol stack2)))
    (when (null place-of-symbol-in-stack)
      (error "(setq) Variable ~a not found in stack" symbol))
    (when (= 0 place-of-symbol-in-stack)
      (error "(setq) Variable ~a found in stack in position 0" symbol))
    ;; 4: C-u 4 M-DEL C-u 3 TAB
    (let* ((instrA (cond ;; (= 0 place-of-symbol-in-stack) is not possible 
                     ((= 1 place-of-symbol-in-stack)
                      (list "M-DEL"))
                     (t (reverse (list "C-u" (+ 1 place-of-symbol-in-stack) "M-DEL")))))
           (instrB (cond ;; (= 1 place-of-symbol-in-stack) ==> do nothing
                     ((= 2 place-of-symbol-in-stack)
                      (list "TAB"))
                     (t (reverse (list "C-u" place-of-symbol-in-stack "TAB")))))
           (output3 (append
                     instrB
                     instrA
                     output2)))
      (pop-and-check-from-stack stack2 "setq")
      
      (cons output3 stack2))))

(defun process-incf (output-and-stack symbol)
  "Convert a 'incf' with symbol SYMBOL taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-atom-or-sexp output-and-stack `(setq ,symbol (+ ,symbol 1))))

(defun process-decf (output-and-stack symbol)
  "Convert a 'decf' with symbol SYMBOL taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-atom-or-sexp output-and-stack `(setq ,symbol (- ,symbol 1))))

(defun process-progn (output-and-stack terms)
  "Convert a 'progn' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (dolist (term terms)
    (setq output-and-stack
          (process-atom-or-sexp output-and-stack term)))
  output-and-stack)

(defun process-while-<= (output-and-stack term1 term2 body)
  "Convert a (while (<= term1 term2) body)' with terms TERM1, TERM2 and BODY, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.

Caution: body shall not increase stack size!"

  (let* ((initial-output (car output-and-stack))
         (initial-stack (cdr output-and-stack))
         (body (cons 'progn body))
         (body-output-in-context (car (process-atom-or-sexp output-and-stack body)))
         (body-output (subseq body-output-in-context 0 (- (length body-output-in-context) (length initial-output))))
         (final-output nil))

    ;; (1) Express final output:
    (setq output-and-stack (add-to-output output-and-stack "Z{"))
    (setq output-and-stack (process-atom-or-sexp output-and-stack term1))
    (setq output-and-stack (process-atom-or-sexp output-and-stack term2))
    (setq output-and-stack (delete-newest-element-on-stack output-and-stack))
    (setq output-and-stack (delete-newest-element-on-stack output-and-stack))
    (setq output-and-stack (add-to-output output-and-stack "a>"))
    (setq output-and-stack (add-to-output output-and-stack "Z/"))
    (setq output-and-stack (append-to-output output-and-stack body-output))
    (setq output-and-stack (add-to-output output-and-stack "Z}"))
    (setq final-output (car output-and-stack))

    (cons final-output initial-stack)))

(defun process-while (output-and-stack terms)
  "Convert a '(while (...) ...)' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((comparison-sexp (car terms))
        (body (cdr terms)))
    (unless (= 3 (length comparison-sexp))
      (error "(while) Malformed comparison: ~a" comparison-sexp))
    (let ((comparison-operator (car comparison-sexp))
          (term1 (cadr comparison-sexp))
          (term2 (caddr comparison-sexp)))
      (cond ((equal '<= comparison-operator)
             (process-while-<= output-and-stack term1 term2 body))
            ((equal '< comparison-operator)
             (process-while-<= output-and-stack term1 `(- ,term2 1) body))
            ((equal '>= comparison-operator)
             (process-while-<= output-and-stack term2 term1 body))
            ((equal '> comparison-operator)
             (process-while-<= output-and-stack term2 `(- ,term1 1) body))
            (t (error "(while) Comparison operator not recognized: ~a" comparison-operator))))))

(defun process-if-= (output-and-stack term1 term2 then-body else-body)
  "Convert a (if (= term1 term2) then-body else-body)' with terms TERM1, TERM2, THEN-BODY and ELSE-BODY, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."

  (when (null else-body)
    (setq else-body '(progn)))
  
  (let* ((initial-output (car output-and-stack))
         (then-body-output-in-context (car (process-atom-or-sexp output-and-stack then-body)))
         (then-body-output (subseq then-body-output-in-context 0 (- (length then-body-output-in-context) (length initial-output))))
         (else-body-output-in-context (car (process-atom-or-sexp output-and-stack else-body)))
         (else-body-output (subseq else-body-output-in-context 0 (- (length else-body-output-in-context) (length initial-output))))
         (final-output nil)
         (final-stack nil))

    (setq output-and-stack (process-atom-or-sexp output-and-stack term1))
    (setq output-and-stack (process-atom-or-sexp output-and-stack term2))
    (setq output-and-stack (add-to-output output-and-stack "a="))
    (pop-and-check-from-stack (cdr output-and-stack) "if-=")
    (pop-and-check-from-stack (cdr output-and-stack) "if-=")
    (setq output-and-stack (add-to-output output-and-stack "Z["))
    (setq output-and-stack (append-to-output output-and-stack then-body-output))
    (setq final-stack (cdr output-and-stack))
    (setq output-and-stack (add-to-output output-and-stack "Z:"))
    (setq output-and-stack (append-to-output output-and-stack else-body-output))
    (setq output-and-stack (add-to-output output-and-stack "Z]"))
    (setq final-output (car output-and-stack))

    (cons final-output final-stack)))

(defun process-if (output-and-stack terms)
  "Convert a '(if (...) ...)' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((control-sexp (car terms))
        (then-body (cadr terms))
        (else-body (caddr terms)))
    (unless (= 3 (length control-sexp))
      (error "(if) Malformed control form: ~a" control-sexp))
    (let ((comparison-operator (car control-sexp))
          (term1 (cadr control-sexp))
          (term2 (caddr control-sexp)))
      (cond ((equal '= comparison-operator)
             (process-if-= output-and-stack term1 term2 then-body else-body))
            (t (error "(if) Comparison operator not recognized: ~a" comparison-operator))))))

(defun process-when (output-and-stack terms)
  "Convert a '(when (...) ...)' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((control-sexp (car terms))
        (body (cdr terms)))
    (unless (= 3 (length control-sexp))
      (error "(when) Malformed control form: ~a" control-sexp))
    (let ((comparison-operator (car control-sexp))
          (term1 (cadr control-sexp))
          (term2 (caddr control-sexp)))
      (cond ((equal '= comparison-operator)
             (process-if-= output-and-stack term1 term2 `(progn ,@body) `(progn)))
            (t (error "(when) Comparison operator not recognized: ~a" comparison-operator))))))

(defun process-dotimes (output-and-stack terms)
  "Convert a '(dotimes (i 4) ...)' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((iteration-sexp (car terms))
        (body (cdr terms)))
    (unless (= 2 (length iteration-sexp))
      (error "(dotimes) Malformed iteration term: ~a" iteration-sexp))
    (let ((symbol (car iteration-sexp))
          (max-value (cadr iteration-sexp)))
      (process-atom-or-sexp output-and-stack
                            `(let ((,symbol 0))
                               (while (< ,symbol ,max-value)
                                 ,@body
                                 (incf ,symbol)))))))

(defun process-unary-operation (output-and-stack term calc-instructions-list operation-name)
  "Convert an unary operation (negate, inverse...) with only one TERM, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.
OPERATION-NAME contains the name of the operation (for error message).
CALC-INSTRUCTIONS-LIST contains the list of related calc instructions."
  
  ;; (1) Add term to the stack:
  (setq output-and-stack
        (process-atom-or-sexp output-and-stack term))
  
  ;; (2) Apply the operation:
  (let ((output (car output-and-stack))
        (stack (cdr output-and-stack)))
    (check-stack-length stack 1 operation-name)
    (pop-and-check-from-stack stack operation-name)
    
    (cons
     (append (reverse calc-instructions-list) output)
     (cons 'NIL stack))))

(defun process-unary-minus (output-and-stack term)
  "Convert a '-' with only one TERM, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-unary-operation output-and-stack term '("n") "unary-minus"))

(defun process-unary-divide (output-and-stack term)
  "Convert a '/' with only one TERM, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-unary-operation output-and-stack term '("&") "unary-divide"))

(defun process-binary-operation (output-and-stack terms calc-instructions-list operation-name)
  "Convert a binary operation (min, max) with a list of two terms TERMS, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.
OPERATION-NAME contains the name of the operation (for error message).
CALC-INSTRUCTIONS-LIST contains the list of related calc instructions."
  (let ((nb-of-terms (length terms)))
    (unless (= nb-of-terms 2)
      (error "Not the right numbers of terms to apply '~a' in ~a" operation-name terms))

    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))

    ;; (2) Apply operation:
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))
      (check-stack-length stack 2 operation-name)
      (pop-and-check-from-stack stack operation-name)
      (pop-and-check-from-stack stack operation-name)
      (setq stack (cons 'NIL stack))
      (setq output (append (reverse calc-instructions-list) output))

      (cons output stack))))

(defun process-mod (output-and-stack terms)
  "Convert a 'mod' with terms TERMS (only 2 accepted) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-binary-operation output-and-stack terms '("%") "mod"))

(defun process-min (output-and-stack terms)
  "Convert a 'min' with terms TERMS (only 2 accepted) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-binary-operation output-and-stack terms '("f" "n") "min"))

(defun process-max (output-and-stack terms)
  "Convert a 'max' with terms TERMS (only 2 accepted) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-binary-operation output-and-stack terms '("f" "x") "max"))

(defun process-lcm (output-and-stack terms)
  "Convert a 'lcm' with terms TERMS (only 2 accepted) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-binary-operation output-and-stack terms '("k" "l") "lcm"))

(defun process-multiple-arguments-operation (output-and-stack terms calc-instructions-list operation-name)
  "Convert an operation with multiple arguments (+, *) with a list of terms TERMS, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.
OPERATION-NAME contains the name of the operation (for error message).
CALC-INSTRUCTIONS-LIST contains the list of related calc instructions."
  (let ((nb-of-terms (length terms)))
    (when (< nb-of-terms 2)
      (error "Not enough terms to apply '~a' in terms ~a" operation-name terms))

    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))

    ;; (2) Apply the operation once or more:
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))
      (dotimes (i (- nb-of-terms 1))
        (check-stack-length stack 2 operation-name)
        (pop-and-check-from-stack stack operation-name)
        (pop-and-check-from-stack stack operation-name)
        (setq stack (cons 'NIL stack))
        (setq output (append calc-instructions-list output)))

      (cons output stack))))

(defun process-plus (output-and-stack terms)
  "Convert a '+' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-multiple-arguments-operation output-and-stack terms '("+") "+"))

(defun process-mult (output-and-stack terms)
  "Convert a '*' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-multiple-arguments-operation output-and-stack terms '("*") "*"))

(defun process-minus (output-and-stack terms)
  "Convert a '-' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((nb-of-terms (length terms)))
    (when (< nb-of-terms 2)
      (error "Not enough terms to apply - in terms ~a" terms))

    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))
    
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))

      ;; (2) Apply '+' once or more:
      (dotimes (i (- nb-of-terms 2))
        (check-stack-length stack 2 "+ within -")
        (pop-and-check-from-stack stack "+ within -")
        (pop-and-check-from-stack stack "+ within -")
        (setq stack (cons 'NIL stack))
        (setq output (cons "+" output))) ; end of dotimes

      ;; (3) Apply final '-':
      (check-stack-length stack 2 "-")
      (pop-and-check-from-stack stack "-")
      (pop-and-check-from-stack stack "-")
      (setq stack (cons 'NIL stack))
      (setq output (cons "-" output))
      
      (cons output stack))))

(defun process-divide (output-and-stack terms)
  "Convert a '/' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((nb-of-terms (length terms)))
    (when (< nb-of-terms 2)
      (error "Not enough terms to apply / in terms ~a" terms))

    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))
    
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))

      ;; (2) Apply '*' once or more:
      (dotimes (i (- nb-of-terms 2))
        (check-stack-length stack 2 "* within /")
        (pop-and-check-from-stack stack "* within /")
        (pop-and-check-from-stack stack "* within /")
        (setq stack (cons 'NIL stack))
        (setq output (cons "*" output))) ; end of dotimes

      ;; (3) Apply final '/':
      (check-stack-length stack 2 "/")
      (pop-and-check-from-stack stack "/")
      (pop-and-check-from-stack stack "/")
      (setq stack (cons 'NIL stack))
      (setq output (cons "/" output))
      
      (cons output stack))))

(defun process-sexp (output-and-stack sexp)
  "Convert a sexp SEXP taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((operator (car sexp)))
    (cond ((equal 'progn operator)
           (process-progn output-and-stack (cdr sexp)))
          ((equal 'mod operator)
           (process-mod output-and-stack (cdr sexp)))
          ((equal 'min operator)
           (process-min output-and-stack (cdr sexp)))
          ((equal 'max operator)
           (process-max output-and-stack (cdr sexp)))
          ((equal 'lcm operator)
           (process-lcm output-and-stack (cdr sexp)))
          ((equal '+ operator)
           (process-plus output-and-stack (cdr sexp)))
          ((equal '* operator)
           (process-mult output-and-stack (cdr sexp)))
          ((and (equal '- operator) (= 1 (length (cdr sexp))))
           (process-unary-minus output-and-stack (car (cdr sexp))))
          ((and (equal '- operator) (<= 2 (length (cdr sexp))))
           (process-minus output-and-stack (cdr sexp)))
          ((and (equal '/ operator) (= 1 (length (cdr sexp))))
           (process-unary-divide output-and-stack (car (cdr sexp))))
          ((and (equal '/ operator) (<= 2 (length (cdr sexp))))
           (process-divide output-and-stack (cdr sexp)))
          ((or (equal 'let operator) (equal 'let* operator))
           (process-let output-and-stack (cdr sexp)))
          ((equal 'while operator) ; (string= "WHILE" (symbol-name operator))
           (process-while output-and-stack (cdr sexp)))
          ((equal 'when operator)
           (process-when output-and-stack (cdr sexp)))
          ((equal 'if operator)
           (process-if output-and-stack (cdr sexp)))
          ((equal 'incf operator)
           (process-incf output-and-stack (car (cdr sexp))))
          ((equal 'decf operator)
           (process-decf output-and-stack (car (cdr sexp))))
          ((equal 'setq operator)
           (process-setq output-and-stack (car (cdr sexp)) (cadr (cdr sexp))))
          ((equal 'dotimes operator)
           (process-dotimes output-and-stack (cdr sexp)))
          (t (error "Operator not recognized: ~a" operator)))))

(defun process-positive-number (output-and-stack number)
  "Convert a positive number NUMBER taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let* ((output (car output-and-stack))
         (stack (cdr output-and-stack)))
    (cons
     ;; new output:
     (cons number output)
     ;; new stack:
     (cons 'NIL stack))))

(defun process-number (output-and-stack number)
  "Convert a number NUMBER taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (if (>= number 0)
      (process-positive-number output-and-stack number)
      (let ((number2 (- number)))
        (process-sexp output-and-stack `(- ,number2)))))

(defun process-variable (output-and-stack symbol)
  "Convert a variable SYMBOL (for intance I) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let* ((output (car output-and-stack))
         (stack (cdr output-and-stack))
         (place-of-symbol-in-stack (position symbol stack :test #'equal)))

    (when (null place-of-symbol-in-stack)
      (error "Variable ~a not found in stack" symbol))

    (incf place-of-symbol-in-stack)

    (cons
     ;; new output:
     (append (cond ((= 1 place-of-symbol-in-stack)
                    (list "RET"))
                   ((= 2 place-of-symbol-in-stack)
                    (list "C-j"))
                   (t (reverse (list "C-u" place-of-symbol-in-stack "C-j"))))
             output)
     ;; new stack
     (cons 'NIL stack))))

(defun process-atom-or-sexp (output-and-stack atom-or-sexp)
  "Convert an atom or sexp ATOM-OR-SEXP taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (cond ((numberp atom-or-sexp)
         (process-number output-and-stack atom-or-sexp))
        ((symbolp atom-or-sexp)
         (process-variable output-and-stack atom-or-sexp))
        ((listp atom-or-sexp)
         (process-sexp output-and-stack atom-or-sexp))
        (t (error "Neither number, symbol nor list: ~a" atom-or-sexp))))

;;; ============
;;; === MAIN ===
;;; ============

(defun add-spaces (output)
  "Add necessary 'SPC' between numbers in OUTPUT, and return an updated output as a string.
Also do it between number and DEL.
And 'n followed number'
For instance: (3 4) --> '3 SPC 4'
              (3 DEL) --> '3 SPC DEL'"

  (let ((output2 nil))
    (dolist (elt output)
      (let ((last-elt (car output2))
            (last-last-elt (cadr output2)))
        (when
            (or
             (and (numberp elt) (numberp last-elt))
             (and (equal "DEL" elt) (numberp last-elt))
             (and (equal "RET" elt) (numberp last-elt))
             (and (equal "C-u" elt) (numberp last-elt))
             (and (numberp elt) (equal "n" last-elt) (not (equal "f" last-last-elt))))
          (push "SPC" output2))         ; when
        (push elt output2)))
    (let ((output3 (format nil "~a" (car output2))))
      (loop for elt in (cdr output2)
            do (setq output3 (format nil "~a ~a" elt output3)))
      output3)))

(defun convert (code)
  "Convert CODE."
  (let* ((stack1 nil)
         (output1 nil)
         (output-and-stack1 (cons output1 stack1))
         (output-and-stack2 (process-atom-or-sexp output-and-stack1 code))
         (output2 (car output-and-stack2))
         (output3 (reverse output2))
         (output4 (add-spaces output3)))
    (format t "~%")
    (format t "code = ~a~%" code)
    (format t "final stack (newest first) = ~a~%" (cdr output-and-stack2))
    (format t "output = ~a~%" output4)))


(convert
 '(let ((i 4)) (+ i 5)))

;; PE 1 :

(convert
 '(let ((n 1000)
        (res 0))
   (dotimes (i n)
     (when (= 0 (* (mod i 3) (mod i 5)))
       (setq res (+ res i))))
   res))

;; 233168

;; PE 5

(convert
  '(let ((n 20)
        (res 1))
    (dotimes (i n)
          (setq res (lcm res (+ i 1))))
    res))

;; 232792560

;; PE 6:

(convert
 '(let ((n 100) (res 0))
   (dotimes (i (+ n 1))
     (setq res (+ res i)))
   (setq res (* res res))
   (dotimes (i (+ n 1))
     (setq res (- res (* i i))))
   res))

;; 25164150

;; PE 9:

(convert
 '(let ((n 1000)
        ;;(nb-solutions 0)
        (res -1))
   (let ((c n))
     (while (>= c 3)
       (let* ((bmax (min (- c 1) (- n c 1)))
              (bmin (max 2 (/ (- n c) 2)))
              (b bmax))
         (while (>= b bmin)
           (let ((a (- n b c)))
             (when (= (* c c) (+ (* a a) (* b b)))
               ;;(incf nb-solutions)
               (setq res (* a b c))))
           (setq b (- b 1))))
       (setq c (- c 1))))
   res))

;; 31875000


;;; end
