;;; -*- Gerbil -*-
;;; Tests for lsp/analysis/parser
(import :std/test
        :lsp/lsp/analysis/parser
        :lsp/lsp/util/position)

(export parser-test-suite)

(def parser-test-suite
  (test-suite "lsp/analysis/parser"

    ;; --- parse-source ---
    (test-case "parse-source: single form"
      (let ((forms (parse-source "(def x 42)")))
        (check (length forms) => 1)
        (let ((lf (car forms)))
          (check-equal? (located-form-form lf) '(def x 42))
          (check (located-form-line lf) => 0))))

    (test-case "parse-source: multiple forms"
      (let ((forms (parse-source "(def x 1)\n(def y 2)")))
        (check (length forms) => 2)
        (check-equal? (located-form-form (car forms)) '(def x 1))
        (check-equal? (located-form-form (cadr forms)) '(def y 2))))

    (test-case "parse-source: empty string"
      (check (length (parse-source "")) => 0))

    (test-case "parse-source: nested parens"
      (let ((forms (parse-source "(if (> x 0) x (- x))")))
        (check (length forms) => 1)
        (check-equal? (located-form-form (car forms))
                       '(if (> x 0) x (- x)))))

    (test-case "parse-source: multiline form with line info"
      (let ((forms (parse-source "(def (foo x)\n  (+ x 1))")))
        (check (length forms) => 1)
        (let ((lf (car forms)))
          (check (located-form-line lf) => 0)
          ;; End line should be line 1 (second line)
          (check (located-form-end-line lf) => 1))))

    ;; --- parse-source-forms ---
    (test-case "parse-source-forms: valid forms"
      (let ((forms (parse-source-forms "(+ 1 2)\n(+ 3 4)")))
        (check (length forms) => 2)
        (check-equal? (car forms) '(+ 1 2))
        (check-equal? (cadr forms) '(+ 3 4))))

    (test-case "parse-source-forms: empty string"
      (check (length (parse-source-forms "")) => 0))

    (test-case "parse-source-forms: malformed gives partial results"
      (let ((forms (parse-source-forms "(+ 1 2)\n(unclosed")))
        ;; Should at least get the first valid form
        (check (>= (length forms) 1) => #t)
        (check-equal? (car forms) '(+ 1 2))))

    ;; --- form-at-line ---
    (test-case "form-at-line: match first"
      (let* ((forms (parse-source "(def x 1)\n(def y 2)\n(def z 3)"))
             (f (form-at-line forms 0)))
        (check (not (eq? f #f)) => #t)
        (check-equal? (located-form-form f) '(def x 1))))

    (test-case "form-at-line: match last"
      (let* ((forms (parse-source "(def x 1)\n(def y 2)\n(def z 3)"))
             (f (form-at-line forms 2)))
        (check (not (eq? f #f)) => #t)
        (check-equal? (located-form-form f) '(def z 3))))

    (test-case "form-at-line: no match"
      (let* ((forms (parse-source "(def x 1)"))
             (f (form-at-line forms 10)))
        (check f => #f)))

    (test-case "form-at-line: empty forms"
      (check (form-at-line '() 0) => #f))

    ;; --- definition-form? ---
    (test-case "definition-form?: def"
      (check (and (definition-form? '(def x 1)) #t) => #t))

    (test-case "definition-form?: define"
      (check (and (definition-form? '(define (foo x) x)) #t) => #t))

    (test-case "definition-form?: defstruct"
      (check (and (definition-form? '(defstruct point (x y))) #t) => #t))

    (test-case "definition-form?: defclass"
      (check (and (definition-form? '(defclass widget (name))) #t) => #t))

    (test-case "definition-form?: defmethod"
      (check (and (definition-form? '(defmethod (draw w) body)) #t) => #t))

    (test-case "definition-form?: defrule"
      (check (and (definition-form? '(defrule (my-macro stx) body)) #t) => #t))

    (test-case "definition-form?: defsyntax"
      (check (and (definition-form? '(defsyntax my-mac body)) #t) => #t))

    (test-case "definition-form?: defvalues"
      (check (and (definition-form? '(defvalues (a b) (values 1 2))) #t) => #t))

    (test-case "definition-form?: defconst"
      (check (and (definition-form? '(defconst pi 3.14)) #t) => #t))

    (test-case "definition-form?: deferror-class"
      (check (and (definition-form? '(deferror-class MyError () message: "err")) #t) => #t))

    (test-case "definition-form?: non-def form"
      (check (definition-form? '(if x y z)) => #f))

    (test-case "definition-form?: non-pair"
      (check (definition-form? 42) => #f))

    ;; --- parse-source-resilient ---
    (test-case "parse-source-resilient: valid source returns all forms"
      (let ((forms (parse-source-resilient "(def x 1)\n(def y 2)")))
        (check (length forms) => 2)
        (check-equal? (located-form-form (car forms)) '(def x 1))
        (check-equal? (located-form-form (cadr forms)) '(def y 2))))

    (test-case "parse-source-resilient: empty string"
      ;; Empty produces nothing from normal parse, recovery finds nothing
      (let ((forms (parse-source-resilient "")))
        (check (length forms) => 0)))

    (test-case "parse-source-resilient: malformed between valid forms"
      ;; Use #< which triggers an unreadable-object parse error
      (let ((forms (parse-source-resilient "(def x 1)\n#<bad>\n(def z 3)")))
        ;; Recovery should find at least 2 forms
        (check (>= (length forms) 2) => #t)
        ;; Filter to only successfully parsed forms (not #:parse-error)
        (let ((valid-forms (filter
                             (lambda (lf)
                               (let ((f (located-form-form lf)))
                                 (and (pair? f)
                                      (not (eq? (car f) '#:parse-error)))))
                             forms)))
          (check (>= (length valid-forms) 2) => #t))))

    (test-case "parse-source-resilient: completely broken source"
      ;; Even completely broken source should not crash
      (let ((forms (parse-source-resilient "(((")))
        (check (list? forms) => #t)))

    (test-case "parse-source-resilient: single valid form"
      (let ((forms (parse-source-resilient "(def x 42)")))
        (check (length forms) => 1)
        (check-equal? (located-form-form (car forms)) '(def x 42))))

    ;; --- offset->line-col ---
    (test-case "offset->line-col: start of text"
      (let-values (((l c) (offset->line-col "hello" 0)))
        (check l => 0)
        (check c => 0)))

    (test-case "offset->line-col: second line"
      (let-values (((l c) (offset->line-col "ab\ncd" 4)))
        (check l => 1)
        (check c => 1)))

    (test-case "offset->line-col: at newline"
      (let-values (((l c) (offset->line-col "ab\ncd" 2)))
        (check l => 0)
        (check c => 2)))
  ))

(def main
  (lambda ()
    (run-tests! parser-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
