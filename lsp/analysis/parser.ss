;;; -*- Gerbil -*-
;;; S-expression parser for Gerbil source
;;; Parses source text into a list of top-level forms with position info
(import ../compat/compat
        ../util/log
        ../util/position)
(export #t)

;;; A parsed form with source location
(defstruct located-form (form line col end-line end-col) transparent: #t)

;;; Parse source text into a list of top-level S-expressions.
;;; Returns list of located-form records.
;;; Handles parse errors gracefully — returns partial results.
(def (parse-source text)
  (let ((port (open-input-string text)))
    (let loop ((forms '()) (last-line 0))
      (let ((pos (get-port-position port)))
        (with-catch
          (lambda (e)
            (lsp-debug "parse error at ~a: ~a" pos e)
            ;; Skip past the error and try to continue
            (skip-to-next-form port)
            (loop forms (+ last-line 1)))
          (lambda ()
            (let ((form (read port)))
              (if (eof-object? form)
                (reverse forms)
                (let* ((end-pos (get-port-position port))
                       (start-line (position-line pos))
                       (start-col (position-col pos))
                       (end-line (position-line end-pos))
                       (end-col (position-col end-pos)))
                  (loop (cons (make-located-form form start-line start-col
                                                  end-line end-col)
                              forms)
                        end-line))))))))))

;;; Get position info from a port
;;; Gambit ports use 1-based line/column; LSP uses 0-based, so subtract 1
(def (get-port-position port)
  (let ((line (input-port-line port))
        (col (input-port-column port)))
    (cons (max 0 (- line 1)) (max 0 (- col 1)))))

(def (position-line pos) (car pos))
(def (position-col pos) (cdr pos))

;;; Skip characters until we find something that looks like the start
;;; of a new top-level form (opening paren at the start of a line)
(def (skip-to-next-form port)
  (let loop ()
    (let ((c (read-char port)))
      (cond
        ((eof-object? c) (void))
        ;; Found a newline — peek ahead for start of form
        ((char=? c #\newline)
         (let ((next (peek-char port)))
           (cond
             ((eof-object? next) (void))
             ((char=? next #\() (void))  ; let the reader take over
             ((char=? next #\;) (loop))  ; skip comment lines
             (else (loop)))))
        (else (loop))))))

;;; Parse source text into just the raw S-expressions (no position info)
(def (parse-source-forms text)
  (let ((port (open-input-string text)))
    (let loop ((forms '()))
      (with-catch
        (lambda (e)
          (reverse forms))
        (lambda ()
          (let ((form (read port)))
            (if (eof-object? form)
              (reverse forms)
              (loop (cons form forms)))))))))

;;; Resilient parser: tries normal parse first, falls back to per-form parsing.
;;; When a syntax error makes the entire file unanalyzable, this scans for
;;; top-level form boundaries (balanced parens at column 0) and tries each
;;; form individually. Valid forms are returned normally; failed forms get a
;;; #:parse-error marker with position info.
(def (parse-source-resilient text)
  ;; Try normal parse first — it's faster and more accurate
  (let ((normal-result
          (with-catch
            (lambda (e) #f)
            (lambda ()
              (let ((forms (parse-source text)))
                (and (pair? forms) forms))))))
    (if normal-result
      normal-result
      ;; Normal parse failed or returned nothing — try per-form recovery
      (recover-forms text))))

;;; Recover individual top-level forms from text with syntax errors.
;;; Scans for balanced-paren boundaries at column 0, then tries to read
;;; each chunk independently.
(def (recover-forms text)
  (let ((boundaries (find-top-level-boundaries text))
        (len (string-length text)))
    (let loop ((bounds boundaries) (forms '()))
      (if (null? bounds)
        (reverse forms)
        (let* ((bound (car bounds))
               (start (car bound))
               (end (cdr bound))
               (chunk (substring text start (min end len))))
          (let ((result (try-parse-chunk chunk text start)))
            (loop (cdr bounds)
                  (if result (cons result forms) forms))))))))

;;; Find the start/end byte offsets of each top-level form.
;;; A top-level form starts with ( at column 0 and ends when parens balance.
;;; Also handles non-paren top-level forms (bare symbols, etc.)
(def (find-top-level-boundaries text)
  (let ((len (string-length text))
        (boundaries '()))
    (let loop ((i 0) (col 0))
      (if (>= i len)
        (reverse boundaries)
        (let ((c (string-ref text i)))
          (cond
            ;; Newline — reset column
            ((char=? c #\newline)
             (loop (+ i 1) 0))
            ;; Opening paren at column 0 — start of top-level form
            ((and (= col 0) (char=? c #\())
             (let ((end (scan-balanced text i len)))
               (set! boundaries (cons (cons i end) boundaries))
               (loop end 0)))
            ;; Opening bracket at column 0
            ((and (= col 0) (char=? c #\[))
             (let ((end (scan-balanced text i len)))
               (set! boundaries (cons (cons i end) boundaries))
               (loop end 0)))
            ;; Skip whitespace at column 0
            ((and (= col 0) (char-whitespace? c))
             (loop (+ i 1) 0))
            ;; Comment at column 0 — skip to end of line
            ((and (= col 0) (char=? c #\;))
             (let skip ((j (+ i 1)))
               (if (or (>= j len) (char=? (string-ref text j) #\newline))
                 (loop (+ j 1) 0)
                 (skip (+ j 1)))))
            ;; Non-paren symbol at column 0 — scan to next whitespace/newline
            ((= col 0)
             (let skip-token ((j i))
               (if (or (>= j len)
                       (char-whitespace? (string-ref text j))
                       (char=? (string-ref text j) #\newline))
                 (begin
                   (set! boundaries (cons (cons i j) boundaries))
                   (loop j 0))
                 (skip-token (+ j 1)))))
            ;; Mid-line — advance
            (else
             (loop (+ i 1) (+ col 1)))))))))

;;; Scan from an opening delimiter to find its matching close.
;;; Handles nested parens, strings, character literals, and comments.
(def (scan-balanced text start len)
  (let loop ((i (+ start 1)) (depth 1))
    (cond
      ((>= i len) i)  ;; unterminated — return end of file
      ((<= depth 0) i)
      (else
       (let ((c (string-ref text i)))
         (cond
           ;; String — skip to closing quote
           ((char=? c #\")
            (let str-loop ((j (+ i 1)))
              (cond
                ((>= j len) (loop j depth))
                ((char=? (string-ref text j) #\\)
                 (str-loop (+ j 2)))  ;; skip escaped char
                ((char=? (string-ref text j) #\")
                 (loop (+ j 1) depth))
                (else (str-loop (+ j 1))))))
           ;; Line comment — skip to end of line
           ((char=? c #\;)
            (let skip ((j (+ i 1)))
              (if (or (>= j len) (char=? (string-ref text j) #\newline))
                (loop (+ j 1) depth)
                (skip (+ j 1)))))
           ;; Character literal #\x
           ((and (char=? c #\#)
                 (< (+ i 1) len)
                 (char=? (string-ref text (+ i 1)) #\\))
            (loop (+ i 3) depth))
           ;; Block comment #| ... |#
           ((and (char=? c #\#)
                 (< (+ i 1) len)
                 (char=? (string-ref text (+ i 1)) #\|))
            (let blk ((j (+ i 2)) (d 1))
              (cond
                ((>= j len) (loop j depth))
                ((and (char=? (string-ref text j) #\|)
                      (< (+ j 1) len)
                      (char=? (string-ref text (+ j 1)) #\#))
                 (if (= d 1)
                   (loop (+ j 2) depth)
                   (blk (+ j 2) (- d 1))))
                ((and (char=? (string-ref text j) #\#)
                      (< (+ j 1) len)
                      (char=? (string-ref text (+ j 1)) #\|))
                 (blk (+ j 2) (+ d 1)))
                (else (blk (+ j 1) d)))))
           ;; Open paren/bracket
           ((or (char=? c #\() (char=? c #\[))
            (loop (+ i 1) (+ depth 1)))
           ;; Close paren/bracket
           ((or (char=? c #\)) (char=? c #\]))
            (loop (+ i 1) (- depth 1)))
           ;; Anything else
           (else (loop (+ i 1) depth))))))))

;;; Try to parse a single chunk of text. Returns a located-form or #f.
(def (try-parse-chunk chunk original-text chunk-start)
  (with-catch
    (lambda (e)
      ;; Failed to parse — return error marker
      (let-values (((start-line start-col) (offset->line-col original-text chunk-start)))
        (make-located-form
          (list 'parse-error (format "~a" e))
          start-line start-col start-line (+ start-col 1))))
    (lambda ()
      (let* ((port (open-input-string chunk))
             (form (read port)))
        (if (eof-object? form)
          #f
          (let-values (((start-line start-col) (offset->line-col original-text chunk-start))
                       ((end-line end-col) (offset->line-col original-text
                                              (+ chunk-start (string-length chunk)))))
            (make-located-form form start-line start-col end-line end-col)))))))

;;; Find the top-level form at a given line number
(def (form-at-line forms line)
  (let loop ((fs forms))
    (if (null? fs) #f
      (let ((f (car fs)))
        (if (and (>= line (located-form-line f))
                 (<= line (located-form-end-line f)))
          f
          (loop (cdr fs)))))))

;;; Check if a form is a definition form
(def (definition-form? form)
  (and (pair? form)
       (symbol? (car form))
       (memq (car form)
             '(def define defn def*
               defstruct defclass
               defmethod defproto
               defrule defrules defsyntax defsyntax-call defsyntax-case
               defvalues defconst
               deferror-class
               deftable definterface implement))))
