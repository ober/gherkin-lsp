#!chezscheme
;; Entry point for gherkin-lsp
;; Equivalent to gsh.ss in gherkin-shell
(import (chezscheme)
        (lsp main))

;; Get args from LSP_ARGC/LSP_ARGn env vars (set by lsp-main.c)
;; or fall back to (command-line) for interpreted mode.
(define (get-real-args)
  (let ((argc-str (getenv "LSP_ARGC")))
    (if argc-str
      ;; Binary mode: custom main saved args in env vars
      (let ((argc (string->number argc-str)))
        (let loop ((i 0) (acc '()))
          (if (>= i argc)
            (reverse acc)
            (let ((val (getenv (format "LSP_ARG~a" i))))
              (loop (+ i 1) (cons (or val "") acc))))))
      ;; Interpreted mode (--program): use (command-line)
      (let ((cmdline (command-line)))
        (if (pair? cmdline) (cdr cmdline) '())))))

(apply main (get-real-args))
