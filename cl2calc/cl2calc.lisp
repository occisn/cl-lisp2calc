(defpackage cl2calc
  (:use :cl ))

(in-package :cl2calc)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

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
             (stack3 (cons (cons (car (car stack2)) symbol0) (cdr stack2))))
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
                 (position symbol0 stack :key #'cdr)))
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

  ;; let's take the example of (setq i 6)
  ;; with stack:
  ;; 3: 5 = i
  ;; 2: y
  ;; 1: x
  
  (let* ((output-and-stack2 (process-atom-or-sexp output-and-stack value-exp))
         (output2 (car output-and-stack2))
         (stack2 (cdr output-and-stack2))

         ;; stack2 :
         ;; 4: 5 = i
         ;; 3: y
         ;; 2: x
         ;; 1: 6
         
         (place-of-symbol-in-stack (position symbol stack2 :key #'cdr))
         ;; = 3, which corresponds to 4 on stack
         )
    (when (null place-of-symbol-in-stack)
      (error "(setq) Variable ~a not found in stack" symbol))
    (when (= 0 place-of-symbol-in-stack)
      (error "(setq) Variable ~a found in stack in position 0" symbol))
    ;; 4: C-u 4 M-DEL C-u 3 TAB
    (let* ((instrA (cond ;; (= 0 place-of-symbol-in-stack) is not possible 
                     ((= 1 place-of-symbol-in-stack)
                      (list "M-DEL"))
                     (t (reverse (list "C-u" (+ 1 place-of-symbol-in-stack) "M-DEL")))))
           ;; stack:
           ;; 3: y
           ;; 2: x
           ;; 1: 6
           (instrB (cond ;; (= 1 place-of-symbol-in-stack) ==> do nothing
                     ((= 2 place-of-symbol-in-stack)
                      (list "TAB"))
                     (t (reverse (list "C-u" place-of-symbol-in-stack "TAB")))))
           (output3 (append
                     instrB
                     instrA
                     output2))
           (value3 (car (pop stack2)))
           (stack3 (replace-nth (- place-of-symbol-in-stack 1) (cons value3 symbol) stack2)))
      (cons output3 stack3))))

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
         (final-output nil)
         (final-stack nil))

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

    ;; (2) Express final stack:
    (setq output-and-stack (cons nil initial-stack))
    (loop
      (setq output-and-stack (process-atom-or-sexp output-and-stack term1))
      (setq output-and-stack (process-atom-or-sexp output-and-stack term2))
      (let* ((term2-value (car (pop (cdr output-and-stack))))
             (term1-value (car (pop (cdr output-and-stack)))))
        (when (> term1-value term2-value)
          (return))
        (setq output-and-stack (process-atom-or-sexp output-and-stack body))))
    (setq final-stack (cdr output-and-stack))
    
    (cons final-output final-stack)))

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
            (t (error "(while) Compararison operator not recognized: ~a" comparison-operator))))))

(defun process-if-= (output-and-stack term1 term2 then-body else-body)
  "Convert a (if (= term1 term2) then-body else-body)' with terms TERM1, TERM2, THEN-BODY and ELSE-BODY, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."

  (when (null else-body)
    (setq else-body '(progn)))
  
  (let* ((initial-output (car output-and-stack))
         (initial-stack (cdr output-and-stack))
         (then-body-output-in-context (car (process-atom-or-sexp output-and-stack then-body)))
         (then-body-output (subseq then-body-output-in-context 0 (- (length then-body-output-in-context) (length initial-output))))
         (else-body-output-in-context (car (process-atom-or-sexp output-and-stack else-body)))
         (else-body-output (subseq else-body-output-in-context 0 (- (length else-body-output-in-context) (length initial-output))))
         (final-output nil)
         (final-stack nil))

    ;; (1) Express final output:
    (setq output-and-stack (process-atom-or-sexp output-and-stack term1))
    (setq output-and-stack (process-atom-or-sexp output-and-stack term2))
    (setq output-and-stack (add-to-output output-and-stack "a="))
    (setq output-and-stack (add-to-output output-and-stack "Z["))
    (setq output-and-stack (append-to-output output-and-stack then-body-output))
    (setq output-and-stack (add-to-output output-and-stack "Z:"))
    (setq output-and-stack (append-to-output output-and-stack else-body-output))
    (setq output-and-stack (add-to-output output-and-stack "Z]"))
    (setq final-output (car output-and-stack))

    ;; (2) Express final stack:
    (setq output-and-stack (cons nil initial-stack))
    ;; (format t "~%(if) initial-stack = ~a~%~%" initial-stack)
    ;; (format t "~%(if) term1 = ~a~%~%" term1)
    (setq output-and-stack (process-atom-or-sexp output-and-stack term1))
    (setq output-and-stack (process-atom-or-sexp output-and-stack term2))
    (let* ((term2-value (car (pop (cdr output-and-stack))))
           (term1-value (car (pop (cdr output-and-stack)))))
      (if (= term1-value term2-value)
          (setq output-and-stack (process-atom-or-sexp output-and-stack then-body))
          (setq output-and-stack (process-atom-or-sexp output-and-stack else-body))))
    (setq final-stack (cdr output-and-stack))
    
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
            (t (error "(if) Compararison operator not recognized: ~a" comparison-operator))))))

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
            (t (error "(when) Compararison operator not recognized: ~a" comparison-operator))))))

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

(defun process-unary-minus (output-and-stack term)
  "Convert a '-' with only one TERM, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."

  ;; (1) Add term to the stack:
  (setq output-and-stack
        (process-atom-or-sexp output-and-stack term))

  ;; (2) Apply '-'
  (let ((output (car output-and-stack))
        (stack (cdr output-and-stack)))
    (when (< (length stack) 1)
      (error "Not enough element in the stack to apply unary '-'; stack = ~a" stack)) ; normally not possible
    (let* ((first-term (car (pop stack)))
           (result (- first-term)))
      (setq output (cons "n" output))
      (setq stack (cons (cons result nil) stack)))
    (cons output stack)))

(defun process-unary-divide (output-and-stack term)
  "Convert a '/' with only one TERM, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."

  ;; (1) Add term to the stack:
  (setq output-and-stack
            (process-atom-or-sexp output-and-stack term))

  ;; (2) Apply '/'
  (let ((output (car output-and-stack))
        (stack (cdr output-and-stack)))
    (when (< (length stack) 1)
      (error "Not enough element in the stack to apply unary '/'; stack = ~a" stack)) ; normally not possible
    (let* ((first-term (car (pop stack)))
           (result (/ first-term)))
      (setq output (cons "&" output))
      (setq stack (cons (cons result nil) stack)))
    (cons output stack)))

(defun process-mod (output-and-stack terms)
  "Convert a 'mod' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((nb-of-terms (length terms)))
    (unless (= nb-of-terms 2)
      (error "Not the right numbers of terms to apply 'mod' in ~a" terms))

    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))

    ;; (2) Apply 'mod':
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))
      (when (< (length stack) 2)
        (error "Not enough elements in the stack to apply 'mod'; stack = ~a" stack))
      (let* ((first-term (car (pop stack)))
             (second-term (car (pop stack)))
             (result (mod second-term first-term)))
        (setq output (cons "%" output))
        (setq stack (cons (cons result nil) stack)))
      (cons output stack))))

(defun process-min (output-and-stack terms)
  "Convert a 'mod' with terms TERMS (only 2 accepted) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((nb-of-terms (length terms)))
    (unless (= nb-of-terms 2)
      (error "Not the right numbers of terms to apply 'min' in ~a" terms))

    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))

    ;; (2) Apply 'min':
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))
      (when (< (length stack) 2)
        (error "Not enough elements in the stack to apply 'mod'; stack = ~a" stack))
      (let* ((first-term (car (pop stack)))
             (second-term (car (pop stack)))
             (result (min second-term first-term)))
        (setq output (append (reverse '("f" "n")) output))
        (setq stack (cons (cons result nil) stack)))
      (cons output stack))))

(defun process-max (output-and-stack terms)
  "Convert a 'mod' with terms TERMS (only 2 accepted) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((nb-of-terms (length terms)))
    (unless (= nb-of-terms 2)
      (error "Not the right numbers of terms to apply 'max' in ~a" terms))

    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))

    ;; (2) Apply 'max':
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))
      (when (< (length stack) 2)
        (error "Not enough elements in the stack to apply 'mod'; stack = ~a" stack))
      (let* ((first-term (car (pop stack)))
             (second-term (car (pop stack)))
             (result (max second-term first-term)))
        (setq output (append (reverse '("f" "x")) output))
        (setq stack (cons (cons result nil) stack)))
      (cons output stack))))

(defun process-plus (output-and-stack terms)
  "Convert a '+' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((nb-of-terms (length terms)))
    (when (< nb-of-terms 2)
      (error "Not enough terms to apply + in terms ~a" terms))
    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))
    ;; (2) Apply '+' once or more:
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))
      (dotimes (i (- nb-of-terms 1))
        (when (< (length stack) 2)
          (error "Not enough elements in the stack to apply '+'; stack = ~a" stack))
        (let* ((first-term (car (pop stack)))
               (second-term (car (pop stack)))
               (result (+ first-term second-term)))
          (setq output (cons "+" output))
          (setq stack (cons (cons result nil) stack))))
      (cons output stack))))

(defun process-minus (output-and-stack terms)
  "Convert a '-' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((nb-of-terms (length terms)))
    (when (< nb-of-terms 2)
      (error "Not enough terms to apply - in terms ~a" terms))
    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))
    ;; (2) Apply '+' once or more:
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))
      (dotimes (i (- nb-of-terms 2))
        (when (< (length stack) 2)
          (error "Not enough elements in the stack to apply '+' within '-'; stack = ~a" stack))
        (let* ((first-term (car (pop stack)))
               (second-term (car (pop stack)))
               (result (+ first-term second-term)))
          (setq output (cons "+" output))
          (setq stack (cons (cons result nil) stack)))) ; dotimes
      (when (< (length stack) 2)
        (error "Not enough elements in the stack to apply '-'; stack = ~a" stack))
      (let* ((first-term (car (pop stack)))
             (second-term (car (pop stack)))
             (result (- second-term first-term )))
        (setq output (cons "-" output))
        (setq stack (cons (cons result nil) stack)))
      
      (cons output stack))))

(defun process-mult (output-and-stack terms)
  "Convert a '*' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((nb-of-terms (length terms)))
    (when (< nb-of-terms 2)
      (error "Not enough terms to apply * in terms ~a" terms))
    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))
    ;; (2) Apply '*' once or more:
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))
      (dotimes (i (- nb-of-terms 1))
        (when (< (length stack) 2)
          (error "Not enough elements in the stack to apply '*'; stack = ~a" stack))
        (let* ((first-term (car (pop stack)))
               (second-term (car (pop stack)))
               (result (* first-term second-term)))
          (setq output (cons "*" output))
          (setq stack (cons (cons result nil) stack))))
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
    ;; (2) Apply '*' once or more:
    (let ((output (car output-and-stack))
          (stack (cdr output-and-stack)))
      (dotimes (i (- nb-of-terms 2))
        (when (< (length stack) 2)
          (error "Not enough elements in the stack to apply '*' within '/'; stack = ~a" stack))
        (let* ((first-term (car (pop stack)))
               (second-term (car (pop stack)))
               (result (* first-term second-term)))
          (setq output (cons "*" output))
          (setq stack (cons (cons result nil) stack)))) ; dotimes
      (when (< (length stack) 2)
        (error "Not enough elements in the stack to apply '/'; stack = ~a" stack))
      (let* ((first-term (car (pop stack)))
             (second-term (car (pop stack)))
             (result (/ second-term first-term )))
        (setq output (cons "/" output))
        (setq stack (cons (cons result nil) stack)))
      
      (cons output stack))))

(defun process-sexp (output-and-stack sexp)
  "Convert a sexp SEXP taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((operator (car sexp)))
    (cond ((equal 'PROGN operator)
           (process-progn output-and-stack (cdr sexp)))
          ((equal 'mod operator)
           (process-mod output-and-stack (cdr sexp)))
          ((equal 'min operator)
           (process-min output-and-stack (cdr sexp)))
          ((equal 'max operator)
           (process-max output-and-stack (cdr sexp)))
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
          ((or (equal 'while operator))
           (process-while output-and-stack (cdr sexp)))
          ((or (equal 'when operator))
           (process-when output-and-stack (cdr sexp)))
          ((or (equal 'if operator))
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
         (stack (cdr output-and-stack))
         (output2 (cons number output))
         (stack2 (cons (cons number nil) stack)))
    (cons output2 stack2)))

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

         ;; (1) Locate variable in the stack:
         (tmp
           (loop for elt in stack
                 for i from 1
                 when (equal symbol (cdr elt)) return (cons i (car elt))))
         (place-of-symbol-in-stack (car tmp)))
    (when (null tmp)
      (error "Variable ~a not found in stack" symbol))

    ;; (2) Add the value as the newest element of the stack:
    (let* ((value (cdr tmp))
           ;; for instance: C-u 5 C-j
           (output2 (append (cond ((= 1 place-of-symbol-in-stack)
                                   (list "RET"))
                                  ((= 2 place-of-symbol-in-stack)
                                   (list "C-j"))
                                  (t (list "C-j" place-of-symbol-in-stack "C-u")))
                            output))
           (stack2 (cons (cons value nil) stack)))
      
      (cons output2 stack2))))

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

;; PE 6 :

(convert
 '(let ((n 100) (res 0))
   (dotimes (i (+ n 1))
     (setq res (+ res i)))
   (setq res (* res res))
   (dotimes (i (+ n 1))
     (setq res (- res (* i i))))
   res))

;; PE 9

(when nil
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
     res)))

;; 31875000
