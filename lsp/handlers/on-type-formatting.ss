;;; -*- Gerbil -*-
;;; On-type formatting handler — textDocument/onTypeFormatting
;;; Triggered on ), ], and newline to auto-indent
(import ../util/log
        ../util/position
        ../util/string
        ../types
        ../state
        ../analysis/document)
(export #t)

;;; Form-specific indent rules (from gerbil-mode.el)
;;; Values: 0, 1, 2 (indent-N) or 'defun
(def *indent-rules*
  (let ((ht (make-hash-table)))
    ;; Indent 0: all subforms are body at paren-col+2
    (for-each (lambda (s) (hash-put! ht s 0))
      '("import" "export" "declare" "include" "or" "and" "case-lambda"
        "call/cc" "call/values" "begin-syntax" "begin-foreign" "cond-expand"
        "for-each" "map" "foldl" "foldr" "unwind-protect" "begin" "cond"))
    ;; Indent 1: first arg distinguished at paren-col+4, rest at paren-col+2
    (for-each (lambda (s) (hash-put! ht s 1))
      '("if" "when" "unless" "set!" "begin-annotation" "begin0"
        "with-syntax" "with-syntax*" "apply" "let-values" "letrec-values"
        "letrec*-values" "module" "syntax-parameterize" "rec" "alet" "alet*"
        "awhen" "parameterize" "parameterize*" "error" "catch" "guard"
        "match" "match*" "with" "with*" "let/cc" "let/esc" "lambda%"
        "chain" "letrec*" "while" "let-hash" "for" "for*" "for/collect"
        "begin-ffi" "test-suite" "test-case" "interface" "with-result"
        "using" "with-interface" "with-struct" "with-class" "with-contract"
        "lambda" "let" "let*" "letrec" "do" "receive" "try" "with-catch"
        "dynamic-wind"))
    ;; Indent 2: first 2 args distinguished at paren-col+4, rest at paren-col+2
    (for-each (lambda (s) (hash-put! ht s 2))
      '("syntax-case" "ast-case" "core-syntax-case" "core-ast-case"
        "do-while" "for/fold"))
    ;; Defun: name is first arg, body at paren-col+2
    (for-each (lambda (s) (hash-put! ht s 'defun))
      '("def" "defvalues" "extern" "defalias" "defsyntax" "defrule"
        "defrules" "defrules*" "defstruct" "defclass" "defgeneric"
        "defmethod" "defmessage" "deftype" "definline" "definline*"
        "define-values" "define-syntaxes" "defcall-actor" "define"
        "deftable" "definterface" "implement" "deferror-class" "defconst"))
    ht))

;;; Check if a character is a sexp delimiter
(def (is-delim? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline)
      (char=? c #\return) (char=? c #\() (char=? c #\))
      (char=? c #\[) (char=? c #\]) (char=? c #\{) (char=? c #\})
      (char=? c #\") (char=? c #\;)))

;;; Get the column number (0-based) of a given offset in text
(def (column-of-offset text offset)
  (let loop ((i (- offset 1)) (col 0))
    (cond
      ((< i 0) col)
      ((char=? (string-ref text i) #\newline) col)
      (else (loop (- i 1) (+ col 1))))))

;;; Scan forward past one complete sexp starting at position i.
;;; Returns position after the sexp (or i if at end).
(def (scan-forward-sexp text i len regions)
  (if (>= i len) i
    (let ((c (string-ref text i)))
      (cond
        ;; List or bracketed form
        ((or (char=? c #\() (char=? c #\[))
         (let loop ((j (+ i 1)) (depth 1))
           (cond
             ((>= j len) j)
             ((= depth 0) j)
             ;; Skip over string/comment characters
             ((> (u8vector-ref regions j) 0)
              (loop (+ j 1) depth))
             (else
              (let ((sc (string-ref text j)))
                (cond
                  ((or (char=? sc #\() (char=? sc #\[))
                   (loop (+ j 1) (+ depth 1)))
                  ((or (char=? sc #\)) (char=? sc #\]))
                   (loop (+ j 1) (- depth 1)))
                  (else (loop (+ j 1) depth))))))))
        ;; String literal
        ((char=? c #\")
         (let loop ((j (+ i 1)))
           (cond
             ((>= j len) j)
             ((char=? (string-ref text j) #\\) (loop (+ j 2)))
             ((char=? (string-ref text j) #\") (+ j 1))
             (else (loop (+ j 1))))))
        ;; Quote/backtick prefix — consume and scan the next sexp
        ((or (char=? c #\') (char=? c #\`))
         (let skip ((j (+ i 1)))
           (if (>= j len) j
             (if (or (char=? (string-ref text j) #\space)
                     (char=? (string-ref text j) #\tab))
               (skip (+ j 1))
               (scan-forward-sexp text j len regions)))))
        ;; Atom (symbol, number, #t, #f, #\char, etc.)
        (else
         (let loop ((j i))
           (if (or (>= j len) (is-delim? (string-ref text j)))
             j
             (loop (+ j 1)))))))))

;;; Count complete sexps in text[start..end), ignoring whitespace and comments.
(def (count-args-in-range text start end regions)
  (let loop ((i start) (count 0))
    (cond
      ((>= i end) count)
      (else
       (let ((c (string-ref text i)))
         (cond
           ;; Skip comment characters
           ((= (u8vector-ref regions i) 2)
            (loop (+ i 1) count))
           ;; Skip whitespace
           ((or (char=? c #\space) (char=? c #\tab) (char=? c #\newline)
                (char=? c #\return))
            (loop (+ i 1) count))
           ;; Hit closing delimiter — we've exited the containing form
           ((or (char=? c #\)) (char=? c #\]))
            count)
           ;; Count this sexp
           (else
            (let ((next (scan-forward-sexp text i (string-length text) regions)))
              (if (and (> next i) (<= next end))
                (loop next (+ count 1))
                count)))))))))

;;; Find the innermost containing form at offset.
;;; Returns (values paren-off paren-col head-sym arg-pos) or (-1 0 #f 0).
(def (find-containing-form text offset regions)
  (let ((len (string-length text)))
    (let loop ((i (- offset 1)) (depth 0))
      (cond
        ((< i 0)
         (values -1 0 #f 0))
        ;; Skip string/comment characters going backward
        ((> (u8vector-ref regions i) 0)
         (loop (- i 1) depth))
        (else
         (let ((c (string-ref text i)))
           (cond
             ;; Closing delimiter: going backward, increase nesting depth
             ((or (char=? c #\)) (char=? c #\]))
              (loop (- i 1) (+ depth 1)))
             ;; Opening delimiter
             ((or (char=? c #\() (char=? c #\[))
              (if (> depth 0)
                (loop (- i 1) (- depth 1))
                ;; Found our containing form
                (let ((paren-col (column-of-offset text i)))
                  ;; Skip whitespace and read head symbol
                  (let head-skip ((j (+ i 1)))
                    (if (>= j len)
                      (values i paren-col #f 0)
                      (let ((sc (string-ref text j)))
                        (cond
                          ((or (char=? sc #\space) (char=? sc #\tab)
                               (char=? sc #\newline) (char=? sc #\return))
                           (head-skip (+ j 1)))
                          ;; Head is an atom — read to delimiter
                          ((not (is-delim? sc))
                           (let head-end ((k (+ j 1)))
                             (if (or (>= k len) (is-delim? (string-ref text k)))
                               (let* ((head (substring text j k))
                                      (arg-pos (count-args-in-range text k offset regions)))
                                 (values i paren-col head arg-pos))
                               (head-end (+ k 1)))))
                          ;; Head is a delimiter (e.g. nested paren) — no symbol head
                          (else
                           (values i paren-col #f 0)))))))))
             (else
              (loop (- i 1) depth)))))))))

;;; Handle textDocument/onTypeFormatting
;;; Returns TextEdit[] to fix indentation
(def (handle-on-type-formatting params)
  (let* ((td (hash-ref params "textDocument" (hash)))
         (uri (hash-ref td "uri" ""))
         (pos (hash-ref params "position" (hash)))
         (line (hash-ref pos "line" 0))
         (col (hash-ref pos "character" 0))
         (ch (hash-ref params "ch" ""))
         (doc (get-document uri)))
    (if doc
      (let ((text (document-text doc)))
        (cond
          ;; On newline: calculate indent for new line
          ((string=? ch "\n")
           (let ((indent (calculate-indent text line)))
             (if (> indent 0)
               (let ((line-text (text-line-at text line)))
                 (let ((current-indent (count-leading-spaces line-text)))
                   (if (not (= current-indent indent))
                     (vector
                       (make-text-edit
                         (make-lsp-range line 0 line current-indent)
                         (make-spaces indent)))
                     (vector))))
               (vector))))
          ;; On ) or ]: no auto-edit needed, most editors handle this
          (else (vector))))
      (vector))))

;;; Calculate the desired indentation for a line using form-aware rules
(def (calculate-indent text line)
  (if (= line 0) 0
    (let* ((offset (line-col->offset text line 0))
           (regions (classify-text-regions text)))
      (let-values (((paren-off paren-col head-sym arg-pos)
                    (find-containing-form text offset regions)))
        (if (< paren-off 0)
          0
          (if head-sym
            (let ((rule (hash-ref *indent-rules* head-sym #f)))
              (cond
                ;; Indent-N: arg-pos < N → distinguished (+4), else body (+2)
                ((number? rule)
                 (if (< arg-pos rule)
                   (+ paren-col 4)
                   (+ paren-col 2)))
                ;; Defun: first arg (name) distinguished, rest is body at +2
                ((eq? rule 'defun)
                 (if (< arg-pos 1)
                   (+ paren-col 4)
                   (+ paren-col 2)))
                ;; No rule: default body indent
                (else
                 (+ paren-col 2))))
            ;; No head symbol: default indent
            (+ paren-col 2)))))))

;;; Count the paren depth at a given offset
;;; Returns the number of unclosed opening delimiters
(def (paren-depth-at text offset)
  (let ((limit (min offset (string-length text))))
    (let loop ((i 0) (depth 0) (in-string #f) (in-comment #f))
      (if (>= i limit) (max 0 depth)
        (let ((c (string-ref text i)))
          (cond
            ;; Handle string literals
            (in-string
             (if (char=? c #\")
               (loop (+ i 1) depth #f #f)
               ;; Skip escaped characters
               (if (and (char=? c #\\) (< (+ i 1) limit))
                 (loop (+ i 2) depth #t #f)
                 (loop (+ i 1) depth #t #f))))
            ;; Handle line comments
            (in-comment
             (if (char=? c #\newline)
               (loop (+ i 1) depth #f #f)
               (loop (+ i 1) depth #f #t)))
            ;; Start of string
            ((char=? c #\")
             (loop (+ i 1) depth #t #f))
            ;; Start of line comment
            ((char=? c #\;)
             (loop (+ i 1) depth #f #t))
            ;; Opening delimiters
            ((or (char=? c #\() (char=? c #\[))
             (loop (+ i 1) (+ depth 1) #f #f))
            ;; Closing delimiters
            ((or (char=? c #\)) (char=? c #\]))
             (loop (+ i 1) (- depth 1) #f #f))
            (else
             (loop (+ i 1) depth #f #f))))))))

;;; Count leading spaces on a line
(def (count-leading-spaces line)
  (let loop ((i 0))
    (if (or (>= i (string-length line))
            (not (char=? (string-ref line i) #\space)))
      i
      (loop (+ i 1)))))

;;; Create a string of N spaces
(def (make-spaces n)
  (make-string n #\space))
