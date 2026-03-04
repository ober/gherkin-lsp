;;; -*- Gerbil -*-
;;; Position and range utilities for LSP
;;; LSP uses 0-based line and column numbers
(export #t)

;; Convert a (0-based) offset in text to (0-based) line and column
(def (offset->line-col text offset)
  (let loop ((i 0) (line 0) (col 0))
    (cond
      ((>= i offset) (values line col))
      ((>= i (string-length text)) (values line col))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ line 1) 0))
      (else
       (loop (+ i 1) line (+ col 1))))))

;; Convert (0-based) line and column to offset in text
(def (line-col->offset text line col)
  (let loop ((i 0) (cur-line 0) (cur-col 0))
    (cond
      ((and (= cur-line line) (= cur-col col)) i)
      ((>= i (string-length text)) i)
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ cur-line 1) 0))
      (else
       (loop (+ i 1) cur-line (+ cur-col 1))))))

;; Get the text of a specific line (0-based)
(def (text-line-at text line-number)
  (let loop ((i 0) (cur-line 0) (start 0))
    (cond
      ((>= i (string-length text))
       (if (= cur-line line-number)
         (substring text start i)
         ""))
      ((char=? (string-ref text i) #\newline)
       (if (= cur-line line-number)
         (substring text start i)
         (loop (+ i 1) (+ cur-line 1) (+ i 1))))
      (else
       (loop (+ i 1) cur-line start)))))

;; Find the symbol at a given position in text
;; Returns (values symbol-string start-col end-col) or (values #f #f #f)
(def (symbol-at-position text line col)
  (let ((line-text (text-line-at text line)))
    (if (or (string=? line-text "") (>= col (string-length line-text)))
      (values #f #f #f)
      (let* ((start (let loop ((i col))
                      (if (or (< i 0) (not (symbol-char? (string-ref line-text i))))
                        (+ i 1)
                        (loop (- i 1)))))
             (end (let loop ((i col))
                    (if (or (>= i (string-length line-text))
                            (not (symbol-char? (string-ref line-text i))))
                      i
                      (loop (+ i 1))))))
        (if (< start end)
          (values (substring line-text start end) start end)
          (values #f #f #f))))))

;; Check if a character can be part of a Scheme symbol
(def (symbol-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\- #\_ #\! #\? #\* #\+ #\/ #\< #\> #\= #\. #\: #\# #\%
                #\& #\^ #\~))))

;; Build LSP Position object
(def (make-lsp-position line character)
  (hash ("line" line) ("character" character)))

;; Build LSP Range object
(def (make-lsp-range start-line start-col end-line end-col)
  (hash ("start" (make-lsp-position start-line start-col))
        ("end" (make-lsp-position end-line end-col))))

;; Build LSP Location object
(def (make-lsp-location uri range)
  (hash ("uri" uri) ("range" range)))
