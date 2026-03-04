;;; -*- Gerbil -*-
;;; Tests for lsp/handlers/on-type-formatting
(import :std/test
        :lsp/lsp/state
        :lsp/lsp/analysis/document
        :lsp/lsp/handlers/on-type-formatting)

(export on-type-formatting-test-suite)

(def on-type-formatting-test-suite
  (test-suite "lsp/handlers/on-type-formatting"

    ;; --- paren-depth-at ---
    (test-case "paren-depth-at: no parens"
      (check (paren-depth-at "hello" 5) => 0))

    (test-case "paren-depth-at: one open"
      (check (paren-depth-at "(hello" 6) => 1))

    (test-case "paren-depth-at: balanced"
      (check (paren-depth-at "(hello)" 7) => 0))

    (test-case "paren-depth-at: nested"
      (check (paren-depth-at "((hello)" 8) => 1))

    (test-case "paren-depth-at: ignores parens in strings"
      (check (paren-depth-at "\"(\"" 3) => 0))

    (test-case "paren-depth-at: ignores parens in comments"
      (check (paren-depth-at "; (\n" 4) => 0))

    (test-case "paren-depth-at: partial offset"
      ;; At offset 5, only the first ( at position 0 has been counted
      (check (paren-depth-at "(def (foo x) x)" 5) => 1))

    ;; --- calculate-indent ---
    (test-case "calculate-indent: line 0"
      (check (calculate-indent "(def x 1)" 0) => 0))

    (test-case "calculate-indent: after open paren"
      (check (calculate-indent "(def (foo x)\n  x)" 1) => 2))

    ;; --- form-aware indentation: defun rule ---
    (test-case "calculate-indent: def body after name+args"
      ;; (def (foo x)\n|cursor → arg-pos=1, defun → paren_col+2=2
      (check (calculate-indent "(def (foo x)\n" 1) => 2))

    (test-case "calculate-indent: def without args yet"
      ;; (def\n|cursor → arg-pos=0, defun → paren_col+4=4
      (check (calculate-indent "(def\n" 1) => 4))

    ;; --- form-aware indentation: indent-0 rule ---
    (test-case "calculate-indent: begin body"
      ;; (begin\n|cursor → arg-pos=0, indent-0 → 0>=0 → paren_col+2=2
      (check (calculate-indent "(begin\n" 1) => 2))

    (test-case "calculate-indent: begin after expr"
      ;; (begin expr1\n|cursor → arg-pos=1, indent-0 → paren_col+2=2
      (check (calculate-indent "(begin expr1\n" 1) => 2))

    ;; --- form-aware indentation: indent-1 rule ---
    (test-case "calculate-indent: when condition on same line"
      ;; (when condition\n|cursor → arg-pos=1, indent-1 → 1>=1 → paren_col+2=2
      (check (calculate-indent "(when condition\n" 1) => 2))

    (test-case "calculate-indent: when no condition yet"
      ;; (when\n|cursor → arg-pos=0, indent-1 → 0<1 → paren_col+4=4
      (check (calculate-indent "(when\n" 1) => 4))

    (test-case "calculate-indent: if body after condition"
      ;; (if cond\n|cursor → arg-pos=1, indent-1 → 1>=1 → paren_col+2=2
      (check (calculate-indent "(if cond\n" 1) => 2))

    ;; --- form-aware indentation: indent-2 rule ---
    (test-case "calculate-indent: syntax-case after first arg"
      ;; (syntax-case x ()\n|cursor → arg-pos=2, indent-2 → 2>=2 → paren_col+2=2
      (check (calculate-indent "(syntax-case x ()\n" 1) => 2))

    ;; --- form-aware indentation: nested forms ---
    (test-case "calculate-indent: nested def"
      ;; (def (outer)\n  (def (inner x)\n|cursor
      ;; Inner def is at col 2, arg-pos=1 → col+2=4
      (check (calculate-indent "(def (outer)\n  (def (inner x)\n" 2) => 4))

    ;; --- form-aware indentation: import/export ---
    (test-case "calculate-indent: import list"
      ;; (import\n|cursor → arg-pos=0, indent-0 → paren_col+2=2
      (check (calculate-indent "(import\n" 1) => 2))

    ;; --- form-aware indentation: unknown form ---
    (test-case "calculate-indent: unknown form defaults to +2"
      ;; (unknown-fn\n|cursor → no rule → paren_col+2=2
      (check (calculate-indent "(unknown-fn\n" 1) => 2))

    ;; --- count-leading-spaces ---
    (test-case "count-leading-spaces: no spaces"
      (check (count-leading-spaces "hello") => 0))

    (test-case "count-leading-spaces: 2 spaces"
      (check (count-leading-spaces "  hello") => 2))

    (test-case "count-leading-spaces: all spaces"
      (check (count-leading-spaces "   ") => 3))

    (test-case "count-leading-spaces: empty string"
      (check (count-leading-spaces "") => 0))

    ;; --- make-spaces ---
    (test-case "make-spaces: 0 spaces"
      (check-equal? (make-spaces 0) ""))

    (test-case "make-spaces: 4 spaces"
      (check-equal? (make-spaces 4) "    "))

    ;; --- handle-on-type-formatting: integration ---
    (test-case "handle-on-type-formatting: returns edits on newline"
      (let* ((uri "file:///test-otf.ss")
             (text "(def (foo x)\n  x)")
             (doc (make-document uri 1 text "gerbil")))
        (set-document! uri doc)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 1) ("character" 0)))
                             ("ch" "\n")
                             ("options" (hash ("tabSize" 2)
                                              ("insertSpaces" #t)))))
               (result (handle-on-type-formatting params)))
          (check (vector? result) => #t))
        (remove-document! uri)))

    (test-case "handle-on-type-formatting: empty for close paren"
      (let* ((uri "file:///test-otf2.ss")
             (text "(def x 1)")
             (doc (make-document uri 1 text "gerbil")))
        (set-document! uri doc)
        (let* ((params (hash ("textDocument" (hash ("uri" uri)))
                             ("position" (hash ("line" 0) ("character" 8)))
                             ("ch" ")")
                             ("options" (hash ("tabSize" 2)
                                              ("insertSpaces" #t)))))
               (result (handle-on-type-formatting params)))
          (check (vector? result) => #t)
          (check (vector-length result) => 0))
        (remove-document! uri)))

    (test-case "handle-on-type-formatting: empty for missing document"
      (let* ((params (hash ("textDocument" (hash ("uri" "file:///nonexistent.ss")))
                           ("position" (hash ("line" 0) ("character" 0)))
                           ("ch" "\n")
                           ("options" (hash))))
             (result (handle-on-type-formatting params)))
        (check (vector? result) => #t)
        (check (vector-length result) => 0)))
  ))

(def main
  (lambda ()
    (run-tests! on-type-formatting-test-suite)
    (test-report-summary!)
    (exit (if (eq? (test-result) 'OK) 0 1))))

(main)
