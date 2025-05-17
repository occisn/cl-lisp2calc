(defpackage cl2calc
  (:use :cl ))

(in-package :cl2calc)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defun delete-nth (n lst)
  "Return a list which is the original LST without its N-th element. Not destructive.
(v1 as of 2025-05-17, from cl-utils)"
  (loop for elt in lst
        for i from 0
        unless (= i n) collect elt))

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

(defun process-progn (output-and-stack terms)
  "Convert a 'progn' with terms TERMS taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (dolist (term terms)
    (setq output-and-stack
          (process-atom-or-sexp output-and-stack term)))
  output-and-stack)

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
      (error "Not the right numbers of terms to apply mod in ~a" terms))

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
          (t (error "Operator not recognized: ~a" operator)))))

(defun process-number (output-and-stack number)
  "Convert a number NUMBER taking into account current OUTPUT-AND-STACK, and return an updated output-and-stack."
  (let* ((output (car output-and-stack))
         (stack (cdr output-and-stack))
         (output2 (cons number output))
         (stack2 (cons (cons number nil) stack)))
    (cons output2 stack2)))

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

(defun add-spaces (output)
  "Add necessary 'SPC' between numbers in OUTPUT, and return an updated output as a string.
Also do it between number and DEL, or number and RET.
For instance: (3 4) --> '3 SPC 4'
              (3 DEL) --> '3 SPC DEL'
              (3 RET) --> '3 RET'
"
  (let ((is-number nil)
        (output2 nil))
    (dolist (elt output)
      (if (or (numberp elt) (equal elt "DEL") (equal elt "RET"))
          (progn
            (when is-number
              (push "SPC" output2))
            (setq is-number (numberp elt)))
          (setq is-number nil))
      (push elt output2))
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
    (format t "code = ~a~%" code)
    (format t "final stack (newest first) = ~a~%" (cdr output-and-stack2))
    ;; (format t "stack = ~a~%" (map 'list #'car (cdr output-and-stack2)))
    ;; (format t "output3 = ~a~%" output3)
    (format t "output = ~a~%" output4)))

(convert '(progn 444
           (let ((i 10) (j (+ i 2))) (- (+ 4 (* (/ 0.5) (mod 111 j)) 5)))
           222) ) ; 

(convert '(progn 444
           (- 45 2 3)
           (/ 90 9 5)
           222) )

