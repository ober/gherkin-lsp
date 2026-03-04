#!chezscheme
;;; process.sls -- Process execution compatible with Gerbil's :std/misc/process
;;; Provides run-process for running external commands and capturing output.

(library (compat process)
  (export run-process)

  (import (chezscheme))

  ;; Run a command and return its output as a string.
  ;; Compatible with Gerbil's (run-process ...) from :std/misc/process.
  ;; Accepts either:
  ;;   (run-process cmd-list coprocess: proc) — call proc with (in out pid)
  ;;   (run-process cmd-list) — return stdout as string
  (define (run-process cmd-list . kwargs)
    (let ((coprocess (kw-get kwargs 'coprocess: #f))
          (cmd (if (pair? cmd-list)
                 (string-join-space cmd-list)
                 cmd-list)))
      (if coprocess
        ;; With coprocess: callback gets (input-port output-port pid)
        (let-values (((from-stdout to-stdin pid) (apply values (process cmd))))
          (coprocess from-stdout to-stdin pid))
        ;; Without: capture stdout as string
        (let-values (((from-stdout to-stdin pid) (apply values (process cmd))))
          (close-port to-stdin)
          (let ((output (get-string-all from-stdout)))
            (close-port from-stdout)
            (if (eof-object? output) "" output))))))

  ;; Join strings with spaces (for command construction)
  (define (string-join-space lst)
    (if (null? lst) ""
      (let ((p (open-output-string)))
        (display (car lst) p)
        (for-each (lambda (s) (display " " p) (display s p)) (cdr lst))
        (get-output-string p))))

  ;; Extract keyword argument from plist
  (define (kw-get kwargs key default)
    (let loop ((rest kwargs))
      (cond
        ((null? rest) default)
        ((and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest))
        (else (loop (cdr rest))))))

  ) ;; end library
