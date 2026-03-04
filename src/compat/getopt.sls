#!chezscheme
;;; getopt.sls -- Command-line argument parsing compatible with Gerbil's :std/cli/getopt
;;; Provides call-with-getopt, flag, option for the LSP server's CLI.

(library (compat getopt)
  (export call-with-getopt flag option)

  (import (except (chezscheme) make-hash-table hash-table?)
          (runtime hash))

  ;; Option spec: (type name flag-string help default)
  ;; type: 'flag or 'option
  (define-record-type opt-spec
    (fields type name flag-str help default))

  ;; Create a flag specification
  ;; (flag 'name "--flag" help: "description")
  ;; Returns an opt-spec record
  (define (flag name flag-str . kwargs)
    (let ((help (kw-get kwargs 'help: "")))
      (make-opt-spec 'flag name flag-str help #f)))

  ;; Create an option specification
  ;; (option 'name "--option" help: "description" default: "value")
  (define (option name flag-str . kwargs)
    (let ((help (kw-get kwargs 'help: ""))
          (default (kw-get kwargs 'default: #f)))
      (make-opt-spec 'option name flag-str help default)))

  ;; Extract keyword argument from plist
  (define (kw-get kwargs key default)
    (let loop ((rest kwargs))
      (cond
        ((null? rest) default)
        ((and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest))
        (else (loop (cdr rest))))))

  ;; Parse arguments against specs, build result hash table
  (define (parse-args args specs)
    (let ((result (make-hash-table)))
      ;; Set defaults
      (for-each (lambda (spec)
                  (when (opt-spec-default spec)
                    (hash-put! result (opt-spec-name spec)
                               (opt-spec-default spec))))
                specs)
      ;; Parse args
      (let loop ((rest args))
        (cond
          ((null? rest) result)
          (else
           (let ((arg (car rest)))
             (let ((spec (find-spec arg specs)))
               (cond
                 ((not spec)
                  ;; Unknown arg — skip
                  (loop (cdr rest)))
                 ((eq? (opt-spec-type spec) 'flag)
                  (hash-put! result (opt-spec-name spec) #t)
                  (loop (cdr rest)))
                 ((eq? (opt-spec-type spec) 'option)
                  (if (pair? (cdr rest))
                    (begin
                      (hash-put! result (opt-spec-name spec) (cadr rest))
                      (loop (cddr rest)))
                    (begin
                      (fprintf (current-error-port)
                        "Warning: option ~a requires a value~n" arg)
                      (loop (cdr rest)))))
                 (else (loop (cdr rest)))))))))
      result))

  ;; Find spec matching a flag string
  (define (find-spec arg specs)
    (let loop ((rest specs))
      (cond
        ((null? rest) #f)
        ((string=? arg (opt-spec-flag-str (car rest))) (car rest))
        (else (loop (cdr rest))))))

  ;; Main entry point: parse args and call proc with result hash
  ;; (call-with-getopt proc args
  ;;   program: "name" help: "description"
  ;;   (flag ...) (option ...) ...)
  (define (call-with-getopt proc args . spec-and-kwargs)
    (let ((specs '())
          (program "program")
          (help ""))
      ;; Separate specs from kwargs
      (let loop ((rest spec-and-kwargs))
        (cond
          ((null? rest) (void))
          ((eq? (car rest) 'program:)
           (when (pair? (cdr rest))
             (set! program (cadr rest))
             (loop (cddr rest))))
          ((eq? (car rest) 'help:)
           (when (pair? (cdr rest))
             (set! help (cadr rest))
             (loop (cddr rest))))
          ((opt-spec? (car rest))
           (set! specs (cons (car rest) specs))
           (loop (cdr rest)))
          (else (loop (cdr rest)))))
      ;; Check for --help
      (when (member "--help" args)
        (display-help program help (reverse specs))
        (exit 0))
      ;; Parse and call
      (let ((result (parse-args args (reverse specs))))
        (proc result))))

  ;; Display help text
  (define (display-help program help specs)
    (fprintf (current-error-port) "Usage: ~a [options]~n" program)
    (when (> (string-length help) 0)
      (fprintf (current-error-port) "~a~n" help))
    (fprintf (current-error-port) "~nOptions:~n")
    (for-each (lambda (spec)
                (fprintf (current-error-port) "  ~a~a~a~n"
                  (opt-spec-flag-str spec)
                  (if (eq? (opt-spec-type spec) 'option) " VALUE" "")
                  (if (> (string-length (opt-spec-help spec)) 0)
                    (string-append "  " (opt-spec-help spec))
                    "")))
              specs)
    (fprintf (current-error-port) "  --help  Show this help~n"))

  ) ;; end library
