(in-package :lisp2calc)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

;;; ===================================
;;; === ADDITIONAL NON-CL OPERATORS ===
;;; ===================================

(defmacro while (condition &body body)
  `(loop while ,condition
         do (progn ,@body)))

(defun prime-factorization (n)
  "Return the prime factors of fixnum N as a flat list, with multiplicity.
N is supposed to be an integer >= 2."
  (declare (type fixnum n))
  (let ((result '()))
    (flet ((divide-out (factor)
             (loop while (zerop (mod n factor))
                   do (push factor result)
                      (setq n (the fixnum (floor n factor))))))
      (divide-out 2)
      (loop for f of-type fixnum from 3 by 2
            while (<= (the fixnum (* f f)) n)
            do (divide-out f))
      (when (> n 1) (push n result)))
    (nreverse result)))

(defun last-element (lst)
  "Returns the last element of list LST."
  (car (last lst)))

;;; =================================
;;; === BASIC OPERATIONS ON LISTS ===
;;; =================================

(defun delete-nth (n lst)
  "Return a list which is the original LST without its N-th element. Not destructive."
  (loop for elt in lst
        for i from 0
        unless (= i n) collect elt))

(defun replace-nth (n new-value lst)
  "Return a list which is the original LST where N-th element is replaced by NEW-VALUE. Not destructive."
  (loop for elt in lst
        for i from 0
        when (= i n) collect new-value
        unless (= i n) collect elt))


;;; =================================
;;; === BASIC OPERATIONS ON STACK ===
;;; =================================

(defmacro pop-and-check-from-stack (stack operation-name)
  "Pop the first element of STACK and checks that it is not associated with a variable, otherwise throw an error.
OPERATION-NAME contains the name of the operation which calls this function (to be displayed in the error message)."
  `(let ((newest-elt (pop ,stack)))
     (when (not (equal newest-elt 'NIL))
       (error "Operation '~a' pops the newest element out of the stack, but it is associated with a variable: ~a" ,operation-name newest-elt))))

(defun check-stack-length (stack minimal-length operation-name)
  "Check that the length of STACK is >= MINIMAL-LENGTH, otherwise throw an error.
OPERATION-NAME contains the name of the operation which calls this function (to be displayed in the error message)."
  (when (< (length stack) minimal-length)
    (error "Not enough elements (should be >= ~a) in the stack to apply '~a'; stack = ~a" minimal-length operation-name stack)))


;;; ============================================
;;; === BASIC OPERATIONS ON OUTPUT-AND-STACK ===
;;; ============================================

(defun output-of (output-and-stack)
  "Extract output from OUTPUT-AND-STACK."
  (car output-and-stack))

(defun stack-of (output-and-stack)
  "Extract stack from OUTPUT-AND-STACK."
  (cdr output-and-stack))

(defun build-output-and-stack-from (output stack)
  "Build output-and-stack from OUTPUT and STACK"
  (cons output stack))

(defun add-to-output (output-and-stack elt)
  "Add ELT to the output part of OUTPUT-AND-STACK, and return the new output-and-stack."
  (build-output-and-stack-from (cons elt (output-of output-and-stack))
                               (stack-of output-and-stack)))

(defun append-to-output (output-and-stack lst)
  "Append LST to the output part of OUTPUT-AND-STACK, and return the new output-and-stack."
  (build-output-and-stack-from (append lst (output-of output-and-stack))
                               (stack-of output-and-stack)))

(defmacro pop-and-check-from-stack-of (output-and-stack operation-name)
  "Pop the newest element from the stack of OUTPUT-AND-STACK and check that it is not associated with a variable.
OPERATION-NAME contains the name of the operation (for error message).
Wrapper around pop-and-check-from-stack for use directly on an output-and-stack cons cell."
  `(pop-and-check-from-stack (cdr ,output-and-stack) ,operation-name))

(defun delete-newest-element-on-stack (output-and-stack)
  "Delete newest element on stack of OUTPUT-AND-STACK (but keep output unchanged), and return the new output-and-stack."
  (build-output-and-stack-from (output-of output-and-stack)
                               (cdr (stack-of output-and-stack))))


;;; =============================
;;; === OPERATIONS PROCESSING ===
;;; =============================

;;; === Non-CL operators ===

(defun process-prime-factorization (output-and-stack terms)
  "Convert a (prime-factorization n) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.
Emits Calc's 'k f' command."
  (unless (= 1 (length terms))
    (error "(prime-factorization) requires exactly 1 argument, got: ~a" terms))
  (process-unary-operation output-and-stack (car terms) '("k" "f") "prime-factorization"))

(defun process-last-element (output-and-stack terms)
  "Convert a (last-element lst) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.
Equivalent to (car (last lst)). Emits Calc's 'v v v r 1' (reverse vector, extract first element)."
  (unless (= 1 (length terms))
    (error "(last-element) requires exactly 1 argument, got: ~a" terms))
  (process-unary-operation output-and-stack (car terms) '("v" "v" "v" "r" 1) "last-element"))

;;; === Variable binding & assignment ===

(defun process-let (output-and-stack terms)
  "Convert a 'let' with terms TERMS (exactly one binding and body) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.
Only one binding is allowed; use 'let*' for multiple bindings."

  ;; Example: (let ((x 3)) (+ x 1))
  ;;   (1) Push 3 → stack: [x=3]
  ;;   (2) Body (+ x 1): RET (copy x) + "1" + "+" → result on top
  ;;       stack: [NIL, x=3]
  ;;   (3) Clean up x (pos 1 → M-DEL)
  ;;   Output: "3 SPC RET 1 + M-DEL"

  (unless (>= (length terms) 2)
    (error "Not enough terms for a 'let' within ~a" terms))
  (let ((bindings (car terms))
        (body (cdr terms)))
    (unless (= 1 (length bindings))
      (error "'let' accepts exactly one binding; use 'let*' for multiple bindings: ~a" bindings))

    ;; (1) Process bindings: evaluate each binding's value expression, then
    ;; replace the anonymous NIL marker on top of the stack with the variable name.
    ;; This allows later references to find the variable by name via POSITION.
    (dolist (binding bindings)
      (let* ((symbol0 (car binding))
             (value-exp (cadr binding))
             (output-and-stack2 (process-atom-or-sexp output-and-stack value-exp)))
        (setq output-and-stack
              (build-output-and-stack-from (output-of output-and-stack2)
                                           (cons symbol0 (cdr (stack-of output-and-stack2)))))))

    ;; (2) Process body (wrapped in progn to handle multiple body forms):
    (setq output-and-stack
          (process-sexp output-and-stack (cons 'progn body)))

    ;; (3) Clean up: delete each binding from the Calc stack in reverse order.
    ;; Reverse order ensures stack positions remain valid as we delete from top.
    ;; DEL = delete top element, M-DEL = delete 2nd element,
    ;; C-u N M-DEL = delete N-th element (1-indexed in Calc).
    (let ((output (output-of output-and-stack))
          (stack (stack-of output-and-stack)))
      (dolist (binding (reverse bindings))
        (let* ((symbol0 (car binding))
               (place-of-symbol-in-stack
                (position symbol0 stack)))
          (when (null place-of-symbol-in-stack)
            (error "(let) Variable ~a supposed to be deleted not found in stack" symbol0))
          (setq output (append
                        (cond ((= 0 place-of-symbol-in-stack)
                               (list "DEL"))
                              ((= 1 place-of-symbol-in-stack)
                               (list "M-DEL"))
                              (t (reverse (list "C-u" (+ 1 place-of-symbol-in-stack) "M-DEL"))))
                        output))
          (setq stack (delete-nth place-of-symbol-in-stack stack))))
      (build-output-and-stack-from output stack))))

(defun process-let* (output-and-stack terms)
  "Convert a 'let*' with terms TERMS (bindings and body) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.
Bindings are processed sequentially: each binding can reference earlier bindings."

  ;; Example: (let* ((x 3) (y (+ x 1))) y)
  ;;   (1) Push 3 → stack: [x=3]; evaluate (+ x 1) → stack: [y=4, x=3]
  ;;   (2) Body y: RET (copy y) → stack: [NIL, y=4, x=3]
  ;;   (3) Clean up y (pos 1 → M-DEL), then x (pos 1 → M-DEL)
  ;;   Output: "3 SPC RET 1 + RET M-DEL M-DEL"

  (unless (>= (length terms) 2)
    (error "Not enough terms for a 'let*' within ~a" terms))
  (let ((bindings (car terms))
        (body (cdr terms)))

    ;; (1) Process bindings: evaluate each binding's value expression, then
    ;; replace the anonymous NIL marker on top of the stack with the variable name.
    ;; This allows later references to find the variable by name via POSITION.
    (dolist (binding bindings)
      (let* ((symbol0 (car binding))
             (value-exp (cadr binding))
             (output-and-stack2 (process-atom-or-sexp output-and-stack value-exp)))
        (setq output-and-stack
              (build-output-and-stack-from (output-of output-and-stack2)
                                           (cons symbol0 (cdr (stack-of output-and-stack2)))))))

    ;; (2) Process body (wrapped in progn to handle multiple body forms):
    (setq output-and-stack
          (process-sexp output-and-stack (cons 'progn body)))

    ;; (3) Clean up: delete each binding from the Calc stack in reverse order.
    ;; Reverse order ensures stack positions remain valid as we delete from top.
    ;; DEL = delete top element, M-DEL = delete 2nd element,
    ;; C-u N M-DEL = delete N-th element (1-indexed in Calc).
    (let ((output (output-of output-and-stack))
          (stack (stack-of output-and-stack)))
      (dolist (binding (reverse bindings))
        (let* ((symbol0 (car binding))
               (place-of-symbol-in-stack
                (position symbol0 stack)))
          (when (null place-of-symbol-in-stack)
            (error "(let*) Variable ~a supposed to be deleted not found in stack" symbol0))
          (setq output (append
                        (cond ((= 0 place-of-symbol-in-stack)
                               (list "DEL"))
                              ((= 1 place-of-symbol-in-stack)
                               (list "M-DEL"))
                              (t (reverse (list "C-u" (+ 1 place-of-symbol-in-stack) "M-DEL"))))
                        output))
          (setq stack (delete-nth place-of-symbol-in-stack stack))))
      (build-output-and-stack-from output stack))))

(defun process-setq (output-and-stack symbol value-exp)
  "Convert a 'setq' with terms SYMBOL and VALUE-EXP taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack. Accept also multiple assignments."
  ;; Example: (let ((x 3)) (setq x 7) x)
  ;;   After let pushes 3, stack is [x=3]. Then (setq x 7):
  ;;   - Push 7 → stack: [NIL=7, x=3]  (x is at position 1)
  ;;   - instrA: M-DEL (delete old x at position 1)  → stack: [NIL=7]
  ;;   - instrB: nil (position was 1, new value is already in place)
  ;;   Output for setq: "7 M-DEL"
  ;;
  ;; Strategy: evaluate the new value (pushes result on top of stack), then:
  ;;   instrA: delete the old value at the variable's position (M-DEL / C-u N M-DEL)
  ;;   instrB: move the new value (currently on top) down to where the variable was (TAB / C-u N TAB)
  ;; After instrA, the variable's position shifts by -1 since the new value is above it.
  (let* ((output-and-stack2 (process-atom-or-sexp output-and-stack value-exp))
         (output2 (output-of output-and-stack2))
         (stack2 (stack-of output-and-stack2))
         (place-of-symbol-in-stack (position symbol stack2)))
    (when (null place-of-symbol-in-stack)
      (error "(setq) Variable ~a not found in stack" symbol))
    (when (= 0 place-of-symbol-in-stack)
      (error "(setq) Variable ~a found in stack in position 0" symbol))
    (let* (;; instrA: delete the old variable value from the stack
           (instrA (cond ;; position 0 is the new value itself, so never happens
                     ((= 1 place-of-symbol-in-stack)
                      (list "M-DEL"))
                     (t (reverse (list "C-u" (+ 1 place-of-symbol-in-stack) "M-DEL")))))
           ;; instrB: move the new value (now on top) down to the variable's slot
           ;; When position=1, the new value is already in the right place (no move needed)
           (instrB (cond ((= 1 place-of-symbol-in-stack)
                      nil)
                     ((= 2 place-of-symbol-in-stack)
                      (list "TAB"))
                     (t (reverse (list "C-u" place-of-symbol-in-stack "TAB")))))
           (output3 (append
                     instrB
                     instrA
                     output2)))
      ;; Remove the anonymous top-of-stack entry (the evaluated new value)
      (pop-and-check-from-stack stack2 "setq")

      (build-output-and-stack-from output3 stack2))))

(defun process-incf (output-and-stack symbol &optional (delta 1))
  "Convert a 'incf' with symbol SYMBOL and optional DELTA taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  ;; Desugared: (incf x) → (setq x (+ x 1)), (incf x d) → (setq x (+ x d))
  (process-atom-or-sexp output-and-stack `(setq ,symbol (+ ,symbol ,delta))))

(defun process-decf (output-and-stack symbol &optional (delta 1))
  "Convert a 'decf' with symbol SYMBOL and optional DELTA taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  ;; Desugared: (decf x) → (setq x (- x 1)), (decf x d) → (setq x (- x d))
  (process-atom-or-sexp output-and-stack `(setq ,symbol (- ,symbol ,delta))))

;;; === Control flow ===

(defun process-progn (output-and-stack terms)
  "Convert a 'progn' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  ;; Example: (progn (+ 1 2) (+ 3 4)) → processes each form sequentially,
  ;; threading the output-and-stack through. Final result is the last form's value.
  (dolist (term terms)
    (setq output-and-stack
          (process-atom-or-sexp output-and-stack term)))
  output-and-stack)

(defun process-while-<= (output-and-stack term1 term2 body)
  "Convert a (while (<= term1 term2) body)' with terms TERM1, TERM2 and BODY, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.

Caution: body shall not increase stack size!"

  ;; Example: (let ((i 0)) (while (<= i 5) (incf i)))
  ;;   Produces: Z{ <push i> <push 5> a> Z/ <body: incf i> Z}
  ;;   i.e. "Z{ RET SPC 5 a> Z/ RET SPC 1 + M-DEL Z}"
  ;;   Loop runs while i <= 5; a> tests i > 5, Z/ breaks when that's nonzero.
  ;;
  ;; Calc loop structure:  Z{ <condition> Z/ <body> Z}
  ;;   Z{ = start loop, Z} = end loop, Z/ = break if top-of-stack is nonzero
  ;; Condition: push term1 and term2, then a> (greater-than test) gives nonzero to break when term1 > term2.

  (let* ((initial-output (output-of output-and-stack))
         (initial-stack (stack-of output-and-stack))
         (body (cons 'progn body))
         ;; Pre-compute the body's Calc instructions by running it in isolation
         ;; on the current state, then extracting only the new instructions
         (body-output-in-context (output-of (process-atom-or-sexp output-and-stack body)))
         (body-output (subseq body-output-in-context 0 (- (length body-output-in-context) (length initial-output))))
         (final-output nil))

    ;; Build the loop: Z{ → condition → Z/ (break if true) → body → Z}
    (setq output-and-stack (add-to-output output-and-stack "Z{"))
    (setq output-and-stack (process-atom-or-sexp output-and-stack term1))
    (setq output-and-stack (process-atom-or-sexp output-and-stack term2))
    ;; Clean up the two condition values from our internal stack tracking
    (setq output-and-stack (delete-newest-element-on-stack output-and-stack))
    (setq output-and-stack (delete-newest-element-on-stack output-and-stack))
    ;; a> = test if term1 > term2 (nonzero = exit loop via Z/)
    (setq output-and-stack (add-to-output output-and-stack "a>"))
    (setq output-and-stack (add-to-output output-and-stack "Z/"))
    (setq output-and-stack (append-to-output output-and-stack body-output))
    (setq output-and-stack (add-to-output output-and-stack "Z}"))
    (setq final-output (output-of output-and-stack))

    ;; Restore initial stack: the loop doesn't change net stack size
    (build-output-and-stack-from final-output initial-stack)))

(defun process-while (output-and-stack terms)
  "Convert a '(while (...) ...)' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  ;; All comparison variants are reduced to the <= primitive (process-while-<=):
  ;;   (< a b)  → (<= a (- b 1))    [integers only]
  ;;   (>= a b) → (<= b a)           [swap operands]
  ;;   (> a b)  → (<= b (- a 1))     [swap + adjust]
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

;;; === Conditionals & boolean ===

(defun process-if-= (output-and-stack term1 term2 then-body else-body)
  "Convert a (if (= term1 term2) then-body else-body)' with terms TERM1, TERM2, THEN-BODY and ELSE-BODY, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."

  ;; Example: (if (= x 0) 42 99)
  ;;   Produces: <push x> <push 0> a= Z[ 42 Z: 99 Z]
  ;;   i.e. "RET SPC 0 a= Z[ 42 Z: 99 Z]"
  ;;   If x=0, pushes 42; otherwise pushes 99.
  ;;
  ;; Example with stack balancing (when): (when (= x 0) 42) → else-body is (progn)
  ;;   Then-branch pushes 1 value, else-branch pushes 0 → pad else with a dummy "0"
  ;;   so both branches have the same stack effect.
  ;;
  ;; Calc conditional structure:  a= Z[ <then> Z: <else> Z]
  ;;   a= = equality test (pushes 1 or 0), Z[ = start if (consumes test result),
  ;;   Z: = else separator, Z] = end if.
  ;; Both branches must leave the stack at the same height, so we pad the shorter
  ;; branch with dummy 0 pushes if needed.

  (when (null else-body)
    (setq else-body '(progn)))

  (let* ((initial-output (output-of output-and-stack))
         (initial-stack (stack-of output-and-stack))
         ;; Pre-compute both branches to extract their Calc instructions and stack effects.
         ;; Each branch is processed against the current state; we then strip the
         ;; pre-existing output to isolate only the branch's contribution.
         (then-result (process-atom-or-sexp output-and-stack then-body))
         (then-body-output-in-context (output-of then-result))
         (then-body-output (subseq then-body-output-in-context 0 (- (length then-body-output-in-context) (length initial-output))))
         (then-stack-delta (- (length (stack-of then-result)) (length initial-stack)))
         (else-result (process-atom-or-sexp output-and-stack else-body))
         (else-body-output-in-context (output-of else-result))
         (else-body-output (subseq else-body-output-in-context 0 (- (length else-body-output-in-context) (length initial-output))))
         (else-stack-delta (- (length (stack-of else-result)) (length initial-stack)))
         (branch-stack-delta (max then-stack-delta else-stack-delta))
         (final-output nil)
         (final-stack nil))

    ;; Balance branches: pad the shorter branch with dummy 0 pushes so both
    ;; branches produce the same net stack growth (required by Calc)
    (when (< else-stack-delta then-stack-delta)
      (dotimes (i (- then-stack-delta else-stack-delta))
        (push 0 else-body-output)))
    (when (< then-stack-delta else-stack-delta)
      (dotimes (i (- else-stack-delta then-stack-delta))
        (push 0 then-body-output)))

    ;; Emit: push term1, push term2, then a= tests equality
    (setq output-and-stack (process-atom-or-sexp output-and-stack term1))
    (setq output-and-stack (process-atom-or-sexp output-and-stack term2))
    (setq output-and-stack (add-to-output output-and-stack "a="))
    ;; a= consumes both operands and pushes the test result;
    ;; Z[ then consumes the test result — clean up our internal stack tracking
    (pop-and-check-from-stack-of output-and-stack "if-=")
    (pop-and-check-from-stack-of output-and-stack "if-=")
    ;; Emit the conditional body: Z[ <then> Z: <else> Z]
    (setq output-and-stack (add-to-output output-and-stack "Z["))
    (setq output-and-stack (append-to-output output-and-stack then-body-output))
    ;; Update final stack to account for the net stack growth of the branches
    (setq final-stack (stack-of output-and-stack))
    (dotimes (i branch-stack-delta)
      (push 'NIL final-stack))
    (setq output-and-stack (add-to-output output-and-stack "Z:"))
    (setq output-and-stack (append-to-output output-and-stack else-body-output))
    (setq output-and-stack (add-to-output output-and-stack "Z]"))
    (setq final-output (output-of output-and-stack))

    (build-output-and-stack-from final-output final-stack)))

(defun process-if (output-and-stack terms)
  "Convert a '(if (...) ...)' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let ((control-sexp (car terms))
        (then-body (cadr terms))
        (else-body (caddr terms)))
    (let ((condition-operator (car control-sexp)))
      (cond ((equal '= condition-operator)
             (unless (= 3 (length control-sexp))
               (error "(if) Malformed control form: ~a" control-sexp))
             (process-if-= output-and-stack (cadr control-sexp) (caddr control-sexp)
                           then-body else-body))
            ((equal 'or condition-operator)
             (process-if-or output-and-stack (cdr control-sexp) then-body else-body))
            (t (error "(if) Condition not recognized: ~a" condition-operator))))))

(defun process-when (output-and-stack terms)
  "Convert a '(when (...) ...)' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  ;; Desugared into if: (when (= x 0) body) → (if (= x 0) (progn body) (progn))
  ;; The empty else-branch (progn) gets padded with dummy values by process-if-=.
  (let ((control-sexp (car terms))
        (body (cdr terms)))
    (let ((condition-operator (car control-sexp)))
      (cond ((equal '= condition-operator)
             (unless (= 3 (length control-sexp))
               (error "(when) Malformed control form: ~a" control-sexp))
             (process-if-= output-and-stack (cadr control-sexp) (caddr control-sexp)
                           `(progn ,@body) `(progn)))
            ((equal 'or condition-operator)
             (process-if-or output-and-stack (cdr control-sexp)
                            `(progn ,@body) `(progn)))
            (t (error "(when) Condition not recognized: ~a" condition-operator))))))

(defun process-= (output-and-stack terms)
  "Convert (= term1 term2) as a standalone expression returning 0 or 1, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (unless (= 2 (length terms))
    (error "(=) requires exactly 2 arguments, got: ~a" terms))
  (let ((term1 (car terms))
        (term2 (cadr terms)))
    (setq output-and-stack (process-atom-or-sexp output-and-stack term1))
    (setq output-and-stack (process-atom-or-sexp output-and-stack term2))
    (setq output-and-stack (add-to-output output-and-stack "a="))
    ;; a= consumed both operands, pushes result (0 or 1)
    (pop-and-check-from-stack-of output-and-stack "=")
    (pop-and-check-from-stack-of output-and-stack "=")
    (build-output-and-stack-from (output-of output-and-stack)
                                 (cons 'NIL (stack-of output-and-stack)))))

(defun process-or (output-and-stack terms)
  "Convert (or (= a1 b1) (= a2 b2)) with exactly 2 equality-test arguments, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.
Returns 0 or 1 using nested Calc conditionals with short-circuit evaluation.

Pattern: <a1> <b1> a= Z[ 1 Z: <a2> <b2> a= Z]
The first a= drives Z[ directly — no extra 0 a= wrapper needed.
If first = is true (a==1), Z[ enters then-branch → push 1 (short-circuit).
If first = is false (a==0), Z[ enters else-branch → evaluate second =, whose a= gives the final 0/1."
  (unless (= 2 (length terms))
    (error "(or) requires exactly 2 arguments, got: ~a" terms))
  (let ((first-eq (car terms))
        (second-eq (cadr terms)))
    (unless (and (consp first-eq) (equal '= (car first-eq)) (= 3 (length first-eq)))
      (error "(or) Arguments must be (= a b) forms, got: ~a" first-eq))
    (unless (and (consp second-eq) (equal '= (car second-eq)) (= 3 (length second-eq)))
      (error "(or) Arguments must be (= a b) forms, got: ~a" second-eq))

    (let* ((initial-output (output-of output-and-stack))
           (initial-stack (stack-of output-and-stack))
           ;; Pre-compute else branch: second = test (evaluated when first is false)
           (else-result (process-= output-and-stack (cdr second-eq)))
           (else-output (subseq (output-of else-result) 0
                                (- (length (output-of else-result))
                                   (length initial-output))))
           ;; Pre-compute then branch: just push 1 (short-circuit when first is true)
           (then-result (process-atom-or-sexp output-and-stack 1))
           (then-output (subseq (output-of then-result) 0
                                (- (length (output-of then-result))
                                   (length initial-output)))))

      ;; Emit first = comparison: <a1> <b1> a=
      (setq output-and-stack (process-atom-or-sexp output-and-stack (cadr first-eq)))
      (setq output-and-stack (process-atom-or-sexp output-and-stack (caddr first-eq)))
      (setq output-and-stack (add-to-output output-and-stack "a="))
      ;; a= consumed both operands — clean up stack tracking
      (pop-and-check-from-stack-of output-and-stack "or")
      (pop-and-check-from-stack-of output-and-stack "or")
      ;; a= result drives Z[ directly:
      ;; Z[ then (a==1, first = was true) → push 1
      ;; Z[ else (a==0, first = was false) → evaluate second =
      (setq output-and-stack (add-to-output output-and-stack "Z["))
      (setq output-and-stack (append-to-output output-and-stack then-output))
      (setq output-and-stack (add-to-output output-and-stack "Z:"))
      (setq output-and-stack (append-to-output output-and-stack else-output))
      (setq output-and-stack (add-to-output output-and-stack "Z]"))

      ;; Final stack: initial + 1 anonymous result (the 0/1 from whichever branch ran)
      (build-output-and-stack-from (output-of output-and-stack)
                                   (cons 'NIL initial-stack)))))

(defun process-if-or (output-and-stack or-args then-body else-body)
  "Convert (if/when (or ...) then-body else-body) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.
Uses the or result (0/1) directly with Z[ — no extra 0 a= test needed.
Z[ enters then-branch when or=1 (truthy), else-branch when or=0 (falsy)."
  (when (null else-body)
    (setq else-body '(progn)))
  (let* ((initial-output (output-of output-and-stack))
         (initial-stack (stack-of output-and-stack))
         ;; Pre-compute both branches
         (then-result (process-atom-or-sexp output-and-stack then-body))
         (then-body-output (subseq (output-of then-result) 0
                                   (- (length (output-of then-result))
                                      (length initial-output))))
         (then-stack-delta (- (length (stack-of then-result)) (length initial-stack)))
         (else-result (process-atom-or-sexp output-and-stack else-body))
         (else-body-output (subseq (output-of else-result) 0
                                   (- (length (output-of else-result))
                                      (length initial-output))))
         (else-stack-delta (- (length (stack-of else-result)) (length initial-stack)))
         (branch-stack-delta (max then-stack-delta else-stack-delta)))

    ;; Balance branches
    (when (< else-stack-delta then-stack-delta)
      (dotimes (i (- then-stack-delta else-stack-delta))
        (push 0 else-body-output)))
    (when (< then-stack-delta else-stack-delta)
      (dotimes (i (- else-stack-delta then-stack-delta))
        (push 0 then-body-output)))

    ;; Process or expression — pushes 0 or 1
    (setq output-and-stack (process-or output-and-stack or-args))
    ;; Z[ consumes the or result — clean up stack tracking
    (pop-and-check-from-stack-of output-and-stack "if-or")
    ;; Z[ then (or=1, truthy) → then-body, Z[ else (or=0, falsy) → else-body
    (setq output-and-stack (add-to-output output-and-stack "Z["))
    (setq output-and-stack (append-to-output output-and-stack then-body-output))
    (let ((final-stack (stack-of output-and-stack)))
      (dotimes (i branch-stack-delta)
        (push 'NIL final-stack))
      (setq output-and-stack (add-to-output output-and-stack "Z:"))
      (setq output-and-stack (append-to-output output-and-stack else-body-output))
      (setq output-and-stack (add-to-output output-and-stack "Z]"))
      (build-output-and-stack-from (output-of output-and-stack) final-stack))))

(defun process-dotimes (output-and-stack terms)
  "Convert a '(dotimes (i 4) ...)' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  ;; Desugared into: (let ((i 0)) (while (< i max) body... (incf i)))
  ;; This reuses the existing let/while/incf machinery.
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

;;; === Arithmetic ===

(defun process-unary-operation (output-and-stack term calc-instructions-list operation-name)
  "Convert an unary operation (negate, inverse...) with only one TERM, taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack.
OPERATION-NAME contains the name of the operation (for error message).
CALC-INSTRUCTIONS-LIST contains the list of related calc instructions."

  ;; Example: (- 5) → push 5, then negate ("n")  → output: "5 n"
  ;; Example: (/ 3) → push 3, then inverse ("&") → output: "3 &"

  ;; (1) Add term to the stack:
  (setq output-and-stack
        (process-atom-or-sexp output-and-stack term))

  ;; (2) Apply the operation:
  (let ((output (output-of output-and-stack))
        (stack (stack-of output-and-stack)))
    (check-stack-length stack 1 operation-name)
    (pop-and-check-from-stack stack operation-name)

    (build-output-and-stack-from
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

  ;; Example: (min 3 7) → push 3, push 7, then "f n" (Calc's floor/min) → output: "3 SPC 7 f n"
  ;; Example: (max 3 7) → push 3, push 7, then "f x" (Calc's floor/max) → output: "3 SPC 7 f x"
  ;; Example: (mod 10 3) → push 10, push 3, then "%" → output: "10 SPC 3 %"

  (let ((nb-of-terms (length terms)))
    (unless (= nb-of-terms 2)
      (error "Not the right numbers of terms to apply '~a' in ~a" operation-name terms))

    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))

    ;; (2) Apply operation:
    (let ((output (output-of output-and-stack))
          (stack (stack-of output-and-stack)))
      (check-stack-length stack 2 operation-name)
      (pop-and-check-from-stack stack operation-name)
      (pop-and-check-from-stack stack operation-name)
      (setq stack (cons 'NIL stack))
      (setq output (append (reverse calc-instructions-list) output))

      (build-output-and-stack-from output stack))))

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

  ;; Example: (+ 1 2 3) → push 1, push 2, push 3, then "+" twice
  ;;   output: "1 SPC 2 SPC 3 + +"  (first + adds 2+3, second + adds 1+result)
  ;; Example: (* 2 3 4) → push 2, push 3, push 4, then "*" twice
  ;;   output: "2 SPC 3 SPC 4 * *"

  (let ((nb-of-terms (length terms)))
    (when (< nb-of-terms 2)
      (error "Not enough terms to apply '~a' in terms ~a" operation-name terms))

    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))

    ;; (2) Apply the operation once or more:
    (let ((output (output-of output-and-stack))
          (stack (stack-of output-and-stack)))
      (dotimes (i (- nb-of-terms 1))
        (check-stack-length stack 2 operation-name)
        (pop-and-check-from-stack stack operation-name)
        (pop-and-check-from-stack stack operation-name)
        (setq stack (cons 'NIL stack))
        (setq output (append calc-instructions-list output)))

      (build-output-and-stack-from output stack))))

(defun process-plus (output-and-stack terms)
  "Convert a '+' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-multiple-arguments-operation output-and-stack terms '("+") "+"))

(defun process-mult (output-and-stack terms)
  "Convert a '*' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (process-multiple-arguments-operation output-and-stack terms '("*") "*"))

(defun process-minus (output-and-stack terms)
  "Convert a '-' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."

  ;; CL semantics: (- a b c) = a - b - c = a - (b + c)
  ;; Strategy: push all terms, then apply "+" to combine all but the first (the subtrahends),
  ;; then apply "-" once between the first term and the combined subtrahend.
  ;; Example: (- 10 3 2) → push 10, push 3, push 2, then "+" (3+2=5), then "-" (10-5=5)
  ;;   output: "10 SPC 3 SPC 2 + -"
  ;; Example: (- 10 3) → push 10, push 3, then "-" → output: "10 SPC 3 -"

  (let ((nb-of-terms (length terms)))
    (when (< nb-of-terms 2)
      (error "Not enough terms to apply - in terms ~a" terms))

    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))

    (let ((output (output-of output-and-stack))
          (stack (stack-of output-and-stack)))

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

      (build-output-and-stack-from output stack))))

(defun process-divide (output-and-stack terms)
  "Convert a '/' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."

  ;; CL semantics: (/ a b c) = a / b / c = a / (b * c)
  ;; Strategy: push all terms, then apply "*" to combine all but the first (the divisors),
  ;; then apply "/" once between the first term and the combined divisor.
  ;; Example: (/ 60 3 4) → push 60, push 3, push 4, then "*" (3*4=12), then "/" (60/12=5)
  ;;   output: "60 SPC 3 SPC 4 * /"
  ;; Example: (/ 10 2) → push 10, push 2, then "/" → output: "10 SPC 2 /"

  (let ((nb-of-terms (length terms)))
    (when (< nb-of-terms 2)
      (error "Not enough terms to apply / in terms ~a" terms))

    ;; (1) Add terms to the stack:
    (dolist (term terms)
      (setq output-and-stack
            (process-atom-or-sexp output-and-stack term)))

    (let ((output (output-of output-and-stack))
          (stack (stack-of output-and-stack)))

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

      (build-output-and-stack-from output stack))))

;;; === Dispatch & atoms ===

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
          ((equal 'let operator)
           (process-let output-and-stack (cdr sexp)))
          ((equal 'let* operator)
           (process-let* output-and-stack (cdr sexp)))
          ((equal 'while operator) ; (string= "WHILE" (symbol-name operator))
           (process-while output-and-stack (cdr sexp)))
          ((equal 'when operator)
           (process-when output-and-stack (cdr sexp)))
          ((equal 'if operator)
           (process-if output-and-stack (cdr sexp)))
          ((equal 'or operator)
           (process-or output-and-stack (cdr sexp)))
          ((equal '= operator)
           (process-= output-and-stack (cdr sexp)))
          ((equal 'incf operator)
           (process-incf output-and-stack (cadr sexp) (or (caddr sexp) 1)))
          ((equal 'decf operator)
           (process-decf output-and-stack (cadr sexp) (or (caddr sexp) 1)))
          ((equal 'setq operator)
           (let ((args (cdr sexp)))
             (loop while args do
               (setq output-and-stack
                     (process-setq output-and-stack (car args) (cadr args)))
               (setq args (cddr args)))
             output-and-stack))
          ((equal 'dotimes operator)
           (process-dotimes output-and-stack (cdr sexp)))
          ((equal 'prime-factorization operator)
           (process-prime-factorization output-and-stack (cdr sexp)))
          ((equal 'last-element operator)
           (process-last-element output-and-stack (cdr sexp)))
          (t (error "Operator not recognized: ~a" operator)))))

(defun process-positive-number (output-and-stack number)
  "Convert a positive number NUMBER taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  ;; Simply emits the number literal and pushes an anonymous NIL entry onto the stack.
  ;; Example: 42 → output gets "42", stack gets a new NIL on top.
  (build-output-and-stack-from
   ;; new output:
   (cons number (output-of output-and-stack))
   ;; new stack:
   (cons 'NIL (stack-of output-and-stack))))

(defun process-number (output-and-stack number)
  "Convert a number NUMBER taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  ;; Positive/zero numbers are emitted directly. Negative numbers are desugared
  ;; into (- abs-value) to use the unary minus machinery.
  ;; Example: -5 → (- 5) → output: "5 n"
  (if (>= number 0)
      (process-positive-number output-and-stack number)
      (let ((number2 (- number)))
        (process-sexp output-and-stack `(- ,number2)))))

(defun process-variable (output-and-stack symbol)
  "Convert a variable SYMBOL (for intance I) taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  ;; Copies the variable's value from its position in the Calc stack to the top.
  ;; In Calc: RET = duplicate top (pos 1), C-j = copy 2nd element (pos 2),
  ;; C-u N C-j = copy N-th element.
  ;; Example: stack is [NIL, x, y] and we reference x (position 1, 1-indexed = 2):
  ;;   → emits "C-j" to copy the 2nd stack element to the top.
  (let* ((output (output-of output-and-stack))
         (stack (stack-of output-and-stack))
         (place-of-symbol-in-stack (position symbol stack :test #'equal)))

    (when (null place-of-symbol-in-stack)
      (error "Variable ~a not found in stack" symbol))

    (incf place-of-symbol-in-stack)

    (build-output-and-stack-from
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

  (let ((sexp-output nil))
    (dolist (elt output)
      (let ((last-elt (car sexp-output))
            (last-last-elt (cadr sexp-output)))
        (when
            (or
             (and (numberp elt) (numberp last-elt))
             (and (equal "DEL" elt) (numberp last-elt))
             (and (equal "RET" elt) (numberp last-elt))
             (and (equal "C-u" elt) (numberp last-elt))
             (and (numberp elt) (equal "n" last-elt) (not (equal "f" last-last-elt)))
             (and (equal "M-DEL" elt) (numberp last-elt) (not (equal "C-u" last-last-elt))))
          (push "SPC" sexp-output))     ; when
        (push elt sexp-output)))
    (let ((string-output (format nil "~a" (car sexp-output))))
      (loop for elt in (cdr sexp-output)
            do (setq string-output (format nil "~a ~a" elt string-output)))
      string-output)))

(defun convert (code)
  "Convert CODE."
  (let* ((initial-void-output-and-stack (build-output-and-stack-from nil nil))
         (final-output-and-stack (process-atom-or-sexp initial-void-output-and-stack code))
         (clean-output (add-spaces (reverse (car final-output-and-stack)))))
    (format t "~%")
    (format t "code = ~a~%" code)
    (format t "~%final stack (newest first) = ~a~%" (stack-of final-output-and-stack))
    (format t "~%output = ~a~%" clean-output)))

;;; ================
;;; === EXAMPLES ===
;;; ================

(defun main ()

  (format t "~%Project Euler 1:~%----------------~%")

  (convert
   '(let* ((n 1000)
           (sum 0))
     (dotimes (i n)
       (when (or (= 0 (mod i 3)) (= 0 (mod i 5)))
         (incf sum i)))
     sum))
  

  ;; 233168

  (format t "~%Project Euler 2:~%----------------~%")

  (convert
   '(let* ((n 4000000)
           (f1 0)
           (f2 1)
           (tmp 0)
           (sum 0))
     (while (<= f2 n)
       (when (= 0 (mod f2 2)) (incf sum f2))
       (setq tmp f1
             f1 f2
             f2 (+ tmp f2)))
     sum))

  ;; 4613732

  (format t "~%Project Euler 3:~%----------------~%")

  (convert
   '(last-element (prime-factorization 600851475143)))

  ;; 6857

  (format t "~%Project Euler 5:~%----------------~%")

  (convert
   '(let* ((n 20)
           (res 1))
     (dotimes (i n)
       (setq res (lcm res (+ i 1))))
     res))

  ;; 232792560

  (format t "~%Project Euler 6:~%----------------~%")

  (convert
   '(let* ((n 100) (res 0))
     (dotimes (i (+ n 1))
       (setq res (+ res i)))
     (setq res (* res res))
     (dotimes (i (+ n 1))
       (setq res (- res (* i i))))
     res))

  ;; 25164150

  (format t "~%Project Euler 9:~%----------------~%")

  (convert
   '(let* ((n 1000)
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

  ) ; end of main

;;; end
