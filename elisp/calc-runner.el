;;; calc-runner.el --- Run Calc keyboard macros in batch mode, output JSON  -*- lexical-binding: t; -*-

;; Usage: emacs --batch -l calc-runner.el -f calc-runner--main "MACRO_STRING"
;; Output: {"status":"ok","result":"6857","stack_depth":1,"duration_ms":42,"error":null}

(require 'calc)
(require 'edmacro)
(require 'json)

(defvar calc-runner--timeout 120
  "Timeout in seconds for macro execution.")

(defun calc-runner--run (macro-string)
  "Run MACRO-STRING as a Calc keyboard macro and return an alist with results."
  (let ((status "ok")
        (result nil)
        (stack-depth 0)
        (err nil)
        (start-time (float-time)))
    ;; Initialize Calc and ensure we are in the Calculator buffer
    (calc)
    (set-buffer "*Calculator*")
    ;; Parse and execute the macro
    (condition-case e
        (let ((keys (edmacro-parse-keys macro-string)))
          (with-timeout (calc-runner--timeout
                         (setq status "error"
                               err "Timeout exceeded"))
            (execute-kbd-macro keys)))
      (error
       (setq status "error"
             err (error-message-string e))))
    ;; Read results
    (when (string= status "ok")
      (setq stack-depth (- (length calc-stack) 1))
      (if (> stack-depth 0)
          (setq result (math-format-value (calc-top 1)))
        (setq status "error"
              err "Stack is empty after macro execution")))
    ;; Compute duration
    (let ((duration-ms (round (* 1000 (- (float-time) start-time)))))
      `((status . ,status)
        (result . ,result)
        (stack_depth . ,stack-depth)
        (duration_ms . ,duration-ms)
        (error . ,err)))))

(defun calc-runner--main ()
  "Entry point for batch execution.
Called via: emacs --batch -l calc-runner.el -f calc-runner--main MACRO_STRING"
  (let* ((macro-string (car command-line-args-left))
         (result (calc-runner--run macro-string)))
    (princ (json-encode result))
    (setq command-line-args-left (cdr command-line-args-left))))

;; Note: do NOT auto-run at load time.  Use -f calc-runner--main instead.

(provide 'calc-runner)
;;; calc-runner.el ends here
